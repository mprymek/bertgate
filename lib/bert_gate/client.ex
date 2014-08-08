defmodule BertGate.Client do
   require Logger

   def connect(host,options\\%{}) do
      port = Map.get(options,:port,9484)
      Logger.info "Connecting to #{inspect host}:#{port}"
      case :gen_tcp.connect(String.to_char_list(host), port, [:binary,{:packet,4},{:active, false}]) do
         {:ok, socket} -> socket
         {:error, err} -> raise NetworkError, error: err
      end
   end

   def call(socket,mod,fun,args\\[],timeout\\5000) do
      :ok = send_packet(socket,{:call,mod,fun,args})
      recv_packet(socket,timeout)
   end

   def cast(socket,mod,fun,args\\[]) do
      :ok = send_packet(socket,{:cast,mod,fun,args})
      :ok
   end

   def auth(socket,token), do:
      call(socket,:'Auth',:auth,[token])

   # @TODO: info not implemented

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
               {:error, {type, code, class, detail, backtrace}=err} ->
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
