# connection test module
defmodule BertGate.Modules.Bert do
   def ping, do: :pong

   def some_integer, do: 1234
   def some_float, do: 1.234
   def some_atom, do: :this_is_atom
   def some_tuple, do: {1,2,3,4}
   def some_bytelist, do: [1,2,3,4]
   def some_list, do: [1,2,[3,4]]
   def some_binary, do: "This is a binary"
   def some_map, do: %{a: 1, b: 2}

   def exception1, do: raise "Test exception"
   def exception2, do: raise ArgumentError
end

defmodule BertGate.Server do
   use GenServer

   def start_link(options\\[]), do: :gen_server.start_link({:local,__MODULE__}, __MODULE__, [options], [])

   def stop,   do: :gen_server.cast(__MODULE__,:stop)
   def reload, do: :gen_server.cast(__MODULE__,:reload)

   def init [options] do
      :ok = Application.ensure_started :ranch
      port = Keyword.get(options,:port,9484)
      acceptors_num = Keyword.get(options,:acceptors_num,20)
      BertGate.Logger.notice "BertGate server listening on port #{port} with #{acceptors_num} acceptors"
      :ranch.start_listener(:bert_gate_server, acceptors_num, :ranch_tcp,
          [{:port, port}], BertGate.Server.Proto, [])
   end
end

defmodule BertGate.Server.Proto.State do
   defstruct ref: nil, transport: nil, socket: nil, opts: [], client: {nil,nil}
end

defmodule BertGate.Server.Proto do
   alias BertGate.Server.Proto.State

   def start_link(ref, socket, transport, opts) do
      state = %State{transport: transport, socket: socket, ref: ref, opts: opts}
      pid = spawn_link(__MODULE__, :init, [state])
      {:ok,pid}
   end

   def init(state) do
      :ok = :ranch.accept_ack(state.ref)
      {:ok,{host,port}} = state.transport.peername(state.socket)
      BertGate.Logger.info "Client connected: #{inspect host}:#{port}"
      state = %State{state|client: {host,port}}
      loop(state)
   end

   defp loop(state) do
      case receive_packet(state.transport,state.socket) do
         {:cast, mod, fun, args} when is_atom(mod) and is_atom(fun) and is_list(args) ->
            BertGate.Logger.debug "CAST: #{mod}.#{fun} #{inspect args}"
            spawn(fn -> jailed_apply(mod,fun,args) end)
            send_packet(state.transport,state.socket,{:noreply})
            loop(state)
         {:call, mod, fun, args} when is_atom(mod) and is_atom(fun) and is_list(args) ->
            BertGate.Logger.debug "CALL: #{mod}.#{fun} #{inspect args}"
            reply = jailed_apply(mod,fun,args)
            send_packet(state.transport,state.socket,reply)
            loop(state)
         ## @TODO: info packets not implemented
         #{:info, :callback, [{:service, service}, {:mfa, mod, fun, args}]}
         #{:info, :cache, [{:access, access}, {:expiration, time}]}
         #{:info, :cache, [{:access, access}, {:validation, token}]}
         #{:info, :stream, []}
         {:info, command, options} when is_atom(command) and is_list(options) ->
            BertGate.Logger.debug "INFO: #{command} #{inspect options} [NOT IMPLEMENTED]"
            loop(state)
         x ->
            BertGate.Logger.error  "BERT: unexpected message: #{inspect(x)}"
            reply = {:error,{:protocol,400,"BERTError","Unexpected message. Closing connection.",[]}}
            send_packet(state.transport,state.socket,reply)
            state.transport.close state.socket
            exit(:normal)
      end
   end

   defp jailed_apply(mod,fun,args) do
      mod = Module.concat(BertGate.Modules,mod)
      try do
         result = apply(mod,fun,args)
         {:reply, result}
      rescue
         # NOTE: we send back exceptions also (intentional BERT-RPC specification violation) 
         err -> {:exception, err}

         ## correct implementation would look like this probably...
         # {error, {Type, Code, Class, Detail, Backtrace}}
         #err in UndefinedFunctionError ->
         #   {:error,{:protocol,404,"BERTError",inspect(err),[]}}
         #err ->
         #   IO.puts "BERT Error: #{inspect(err)}"
         #   {:error,{:server,501,"BERTError","Internal server error: #{inspect err}",[]}}
      end
   end

   defp receive_packet(transport,socket) do
      # this is a blocking entrypoint -> we will use infinity here
      <<payload_size::size(32)>> = recv(transport,socket,4,:infinity)
      data = recv(transport,socket,payload_size,5000)
      Bert.decode(data)
   end

   defp recv(transport,socket,bytes,timeout) do
      case transport.recv(socket,bytes,timeout) do
         {:ok,data} ->
            # @TODO: do we really need to check the size of returned data? Didn't transport already check it? 
            case bytes-byte_size(data) do
               0 -> data
               n when n<0 ->
                  BertGate.Logger.error "INTERNAL ERROR: #{bytes}B wanted, #{bytes-n}B received. This should not happen. PANIC"
                  exit(:bad_recv_data_len)
               n ->
                  # NOTE: we use the _same_ timeout here. No calculation of the remaining time is done!
                  data2 = recv(transport,socket,n,timeout)
                  <<data,data2>>
            end
         {:error,x} when x in [:closed,:timeout] ->
            transport.close socket
            exit(:normal)
         {:error,any} ->
            BertGate.Logger.error "BERT: error: #{inspect(any)}"
            exit(:bert_error)
      end
   end

   defp send_packet(transport,socket,data) do
      payload = Bert.encode(data)
      payload_size = byte_size(payload)
      case transport.send(socket,<<payload_size::size(32),payload::binary>>) do
         :ok -> :ok
         {:error,err} ->
            BertGate.Logger.error "send_packet error: #{err}"
            exit(:send_error)
      end
   end

end
