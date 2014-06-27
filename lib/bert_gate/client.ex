defmodule BertGate.Client do
   def connect(host,port\\9484) do
      case :gen_tcp.connect(bitstring_to_list(host), port, [:binary,{:packet,4},{:active, false}]) do
         {:ok, socket} -> socket
         {:error, err} -> raise NetworkError, error: err
      end
   end

   def call(socket,mod,fun,args,timeout\\5000) do
      :ok = send_packet(socket,{:call,mod,fun,args})
      recv_packet(socket,timeout)
   end

   def cast(socket,mod,fun,args,timeout) do
      :ok = send_packet(socket,{:cast,mod,fun,args})
      :ok
   end

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
               {:exception, err} -> raise err
               {:error, {_type, _code, _class, _detail, _backtrace}=err} ->
                  raise BERTError, details: err
               any ->
                  raise "Bad reply: #{inspect(any)}"
            end
         {:error,x} when x in [:closed,:timeout] ->
            :gen_tcp.close socket
            raise BERTClosed
         {:error,any} ->
            BertGate.Logger.error "BERT: error: #{inspect(any)}"
            :gen_tcp.close socket
            raise "BERT recv error: #{inspect(any)}"
      end
   end
end
