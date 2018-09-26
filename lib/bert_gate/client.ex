defmodule BertGate.Client.State do
  defstruct host: nil, port: nil, socket: nil
end

############################################################################
# api

defmodule BertGate.Client do
   require Logger
   alias BertGate.Client.State

   def connect(host,options\\%{}) do
      port = options |> Map.get(:port,9484)
      Logger.info "Connecting to #{inspect host}:#{port}"
      case :gen_tcp.connect(String.to_charlist(host), port, [:binary,{:packet,4},{:active, false}]) do
         {:ok, socket} ->
           {:ok, pid} = GenServer.start_link(BertGate.Client.Impl, %State{host: host, port: port, socket: socket})
           pid
         {:error, err} -> raise NetworkError, error: err
      end
   end

   def call(pid,mod,fun,args\\[],timeout\\5000) do
      res = GenServer.call(pid, {:call,mod,fun,args,timeout})
      case res do
         {:result,r} -> r
         {:exception,e} -> raise e
      end
   end

   def cast(pid,mod,fun,args\\[]) do
      res = GenServer.call(pid, {:cast,mod,fun,args})
      case res do
         {:result,r} -> r
         {:exception,e} -> raise e
      end
   end

   def auth(pid,token), do:
      call(pid,:'Auth',:auth,[token])

   def close(pid), do:
      GenServer.call(pid, :close)

   # @TODO: info not implemented

end

############################################################################
# implementation

defmodule BertGate.Client.Impl do
   require Logger
   use GenServer
   alias BertGate.Client.State

   def init(args) do
     {:ok, args}
   end

   def handle_call({:call,mod,fun,args,timeout},_,s=%State{socket: socket}) do
      res = try do
        res = call_(socket,mod,fun,args,timeout)
        {:result,res}
      rescue
        e -> {:exception,e}
      end
      {:reply,res,s}
   end

   def handle_call({:cast,mod,fun,args},_,s=%State{socket: socket}) do
      res = try do
        cast_(socket,mod,fun,args)
        {:result, :ok}
      rescue
        e -> {:exception,e}
      end
      {:reply,res,s}
   end

   def handle_call(:close,_,s=%State{socket: socket}) do
      res = :gen_tcp.close socket
      {:reply,res,%State{s|socket: nil}}
   end

   defp call_(socket,mod,fun,args,timeout) do
      :ok = send_packet(socket,{:call,mod,fun,args})
      recv_packet(socket,timeout)
   end

   defp cast_(socket,mod,fun,args) do
      :ok = send_packet(socket,{:cast,mod,fun,args})
      :ok
   end

   # NOTE: packet length is automatically inserted by gen_tcp due to the {:packet,4} option
   defp send_packet(socket,data) do
      payload=Bert.encode(data)
      case :gen_tcp.send(socket,payload) do
         :ok -> :ok
         {:error,reason} -> raise NetworkError, error: reason
      end
   end

   # @TODO: receive packet!
   defp recv_packet(socket,timeout) do
      case :gen_tcp.recv(socket,0,timeout) do
         {:ok,data} ->
            case Bert.decode(data) do
               {:reply,reply} -> reply
               # exception raised by user function
               {:error,{:user,601,_,err,_}} ->
                  raise err
               {:error, {type, code, class, detail, backtrace}} ->
                  raise BERTError, type: type, code: code, class: class, detail: detail, backtrace: backtrace
               any ->
                  raise "Bad reply: #{inspect(any)}"
            end
         {:error,x} when x in [:closed,:timeout] ->
            :gen_tcp.close socket
            raise BERTClosed
         {:error,any} ->
            Logger.error "BERT: error: #{inspect(any)}"
            :gen_tcp.close socket
            raise "BERT recv error: #{inspect(any)}"
      end
   end
end
