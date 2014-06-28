defmodule NetworkError do
   defexception [error: nil]
   def message(exception), do: "Network connection error: #{inspect exception.error}"
end

defmodule RpcError do
   defexception [error: nil]
   def message(exception), do: "RPC error: #{inspect exception.error}"
end

defmodule Rpc.State do
  defstruct names: %{}
end

defmodule Rpc do
  alias Rpc.State

  def start_link do
    Agent.start_link(fn -> %State{} end, name: __MODULE__)
  end

  def add_bert_server(name,host,options\\%{}) do
    connect {:bert,nil,name,host,options}
    :ok
  end

  def add_rpc_node(name,node) do
    connect {:rpc,node,name}
    :ok
  end

  def call(name,mod,fun,args\\[],timeout\\5000) do
    case connection(name) do
      nil -> raise KeyError, [key: name, term: "servers list"]
      cdef={:rpc,node,_} ->
        case :rpc.call(node,mod,fun,args,timeout) do
           {:badrpc, reason} ->
              raise RpcError, error: reason
           ret -> ret
        end
      cdef={:bert,conn,_,_,_} ->
        try do
          BertGate.Client.call(conn,mod,fun,args,timeout)
        rescue
          [BERTClosed,NetworkError] ->
             conn = connect cdef
             BertGate.Client.call(conn,mod,fun,args,timeout)
        end
    end
  end

  def cast(name,mod,fun,args\\[]) do
    case connection(name) do
      nil -> raise KeyError, [key: name, term: "servers list"]
      cdef={:rpc,node,_} ->
        case :rpc.cast(node,mod,fun,args) do
           {:badrpc, reason} ->
              raise RpcError, error: reason
           ret -> ret
        end
      cdef={:bert,conn,_,_,_} ->
        try do
          BertGate.Client.cast(conn,mod,fun,args)
        rescue
          [BERTClosed,NetworkError] ->
             conn = connect cdef
             BertGate.Client.cast(conn,mod,fun,args)
        end
    end
  end

  defp connection(name), do:
    Agent.get(__MODULE__,fn %State{names: names} -> names[name] end)

  defp connect({:bert,_,name,host,options}) do
    conn = BertGate.Client.connect(host,options)
    case options[:auth] do
       nil -> :ok
       token ->
          :ok = BertGate.Client.auth(conn,token)
    end
    Agent.update(__MODULE__, fn state=%State{names: names} ->
      val = {:bert,conn,name,host,options}
      names = names
        |> Map.update(name,val,fn {:bert,oldconn,_name,_host,_options} ->
          :gen_tcp.close oldconn
          val
        end)
      %State{state|names: names}
    end)
    conn
  end

  defp connect({:rpc,node,name}) do
    BertGate.Logger.info "RPC: connecting to #{node}"
    ret = Node.connect(node)
    if ret != true do
      raise NetworkError, error: {:connect_return,ret}
    end
    Agent.update(__MODULE__, fn state=%State{names: names} ->
      names = names
        |> Map.put(name, {:rpc,node,name})
      %State{state|names: names}
    end)
    node
  end
end
