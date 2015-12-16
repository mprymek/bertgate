# connection test module
defmodule BertGate.Modules.Bert do
   def ping(_auth), do: :pong
   def auth_data(auth), do: auth

   def some_integer(_auth), do: 1234
   def some_float(_auth), do: 1.234
   def some_atom(_auth), do: :this_is_atom
   def some_tuple(_auth), do: {1,2,3,4}
   def some_bytelist(_auth), do: [1,2,3,4]
   def some_list(_auth), do: [1,2,[3,4]]
   def some_binary(_auth), do: "This is a binary"
   def some_map(_auth), do: %{a: 1, b: 2}

   def exception1(_auth), do: raise "Test exception"
   def exception2(_auth), do: raise ArgumentError
end

defmodule BertGate.Server do
   require Logger
   use GenServer

   def start_link(options\\%{}), do: :gen_server.start_link({:local,__MODULE__}, __MODULE__, [options], [])

   def stop,   do: :gen_server.cast(__MODULE__,:stop)
   def reload, do: :gen_server.cast(__MODULE__,:reload)

   def init([options]) do
      :ok = Application.ensure_started :ranch
      port = Dict.get(options,:port,9484)
      allowed = Dict.get(options,:public,[:'Bert'])
      authenticator = Dict.get(options,:authenticator,fn _,_,_ -> nil end)
      acceptors_num = Dict.get(options,:acceptors_num,20)
      Logger.info "BertGate server listening on port #{port} with #{acceptors_num} acceptors"
      Logger.info "Public modules: #{inspect allowed}"
      :ranch.start_listener(:bert_gate_server, acceptors_num, :ranch_tcp,
          [{:port, port}], BertGate.Server.Proto, %{allowed: allowed, authenticator: authenticator})
   end
end

defmodule BertGate.Server.Proto.State do
   defstruct ref: nil, transport: nil, socket: nil, opts: [], client: {nil,nil}, allowed: [], auth_data: nil
end

defmodule BertGate.Server.Proto do
   require Logger
   alias BertGate.Server.Proto.State

   def start_link(ref, socket, transport, opts) do
      state = %State{transport: transport, socket: socket, ref: ref, opts: opts, allowed: opts[:allowed]}
      pid = spawn_link(__MODULE__, :init, [state])
      {:ok,pid}
   end

   def init(state) do
      :ok = :ranch.accept_ack(state.ref)
      {:ok,{host,port}} = state.transport.peername(state.socket)
      Logger.info "Client connected: #{inspect host}:#{port}"
      state = %State{state|client: {host,port}}
      loop(state)
   end

   defp check_auth(state=%State{},type,mod) do
      if not mod in state.allowed do
         Logger.warn "Client #{inspect state.client} unauthorized for #{type}:#{mod}"
         close(state,401,"Unauthorized.")
      end
   end

   defp close(state,code,message) do
      Logger.warn "BERTError: client=#{inspect state.client} code=#{code} message: #{message}"
      reply = {:error,{:protocol,code,"BERTError",message<>" Closing connection.",[]}}
      send_packet(state.transport,state.socket,reply)
      state.transport.close state.socket
      exit(:normal)
   end

   defp loop(state=%State{}) do
      case receive_packet(state.transport,state.socket) do
         {:cast, mod, fun, args} when is_atom(mod) and is_atom(fun) and is_list(args) ->
            check_auth(state,:cast,mod)
            Logger.debug "CAST: #{mod}.#{fun} #{inspect args}"
            spawn(fn -> jailed_apply(mod,fun,state.auth_data,args) end)
            send_packet(state.transport,state.socket,{:noreply})
            loop(state)
         # built-in Auth module
         {:call, :'Auth', :auth, [token]} ->
            case state.opts.authenticator.(state.allowed,state.auth_data,token) do
               nil ->
                  close(state,401,"Authentication failed.")
               {new_allowed,auth_data} when is_list(new_allowed) ->
                  # remove duplicates
                  allowed = (state.allowed++new_allowed) |> Enum.uniq
                  Logger.info "Client #{inspect state.client} now authorized for #{inspect allowed}"
                  send_packet(state.transport,state.socket,{:reply,:ok})
                  loop(%State{state|allowed: allowed, auth_data: auth_data})
            end
         {:call, mod, fun, args} when is_atom(mod) and is_atom(fun) and is_list(args) ->
            check_auth(state,:call,mod)
            Logger.debug "CALL: #{mod}.#{fun} #{inspect args}"
            reply = jailed_apply(mod,fun,state.auth_data,args)
            send_packet(state.transport,state.socket,reply)
            loop(state)
         ## @TODO: info packets not implemented
         #{:info, :callback, [{:service, service}, {:mfa, mod, fun, args}]}
         #{:info, :cache, [{:access, access}, {:expiration, time}]}
         #{:info, :cache, [{:access, access}, {:validation, token}]}
         #{:info, :stream, []}
         {:info, command, options} when is_atom(command) and is_list(options) ->
            Logger.debug "INFO: #{command} #{inspect options} [NOT IMPLEMENTED]"
            reply = {:error,{:protocol,501,"BERTError","Info messages not implemented.",[]}}
            send_packet(state.transport,state.socket,reply)
            loop(state)
         x ->
            Logger.error  "BERT: unexpected message: #{inspect(x)}"
            close(state,400,"Unexpected message.")
      end
   end

   defp jailed_apply(mod,fun,auth_data,args) do
      mod = Module.concat(BertGate.Modules,mod)
      try do
         result = apply(mod,fun,[auth_data|args])
         {:reply, result}
      rescue
         # NOTE: we send back exceptions also (little intentional BERT-RPC specification violation)
         err ->
            #{:error,{:user,601,Map.get(err,:__struct__),err.message,err}}
            error_trace = System.stacktrace
            Logger.error "#{Map.get(err,:__struct__)}: #{Map.get(err,:__message__)}\n#{Exception.format_stacktrace(error_trace)}"
            {:error, {:user, 601, Map.get(err,:__struct__), err, error_trace |> Enum.map(&Exception.format_stacktrace_entry(&1))}}
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
                  Logger.error "INTERNAL ERROR: #{bytes}B wanted, #{bytes-n}B received. This should not happen. PANIC"
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
            Logger.error "BERT: error: #{inspect(any)}"
            exit(:bert_error)
      end
   end

   defp send_packet(transport,socket,data) do
      payload = Bert.encode(data)
      payload_size = byte_size(payload)
      case transport.send(socket,<<payload_size::size(32),payload::binary>>) do
         :ok -> :ok
         {:error,err} ->
            Logger.error "send_packet error: #{err}"
            exit(:send_error)
      end
   end

end
