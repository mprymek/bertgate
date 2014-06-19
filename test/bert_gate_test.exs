defmodule BertGateTest do
  use ExUnit.Case

  alias BertGate.Client

  @port 9485

  setup_all do
     {:ok, server} = BertGate.Server.start_link [port: @port]
     {:ok, [server: server]}
  end

  setup do
     conn=Client.connect("localhost", @port)
     {:ok, [conn: conn]}
  end

  test "ping", meta do
    conn=meta[:conn]
    assert Client.call(conn,:'Bert',:ping,[]) == :pong
    assert Client.cast(conn,:'Bert',:ping,[]) == :ok
  end

  test "data types", meta do
    conn=meta[:conn]
    assert Client.call(conn,:'Bert',:some_integer,[]) == 1234
    assert Client.call(conn,:'Bert',:some_float,[]) == 1.234
    assert Client.call(conn,:'Bert',:some_atom,[]) == :this_is_atom
    assert Client.call(conn,:'Bert',:some_tuple,[]) == {1,2,3,4}
    assert Client.call(conn,:'Bert',:some_bytelist,[]) == [1,2,3,4]
    assert Client.call(conn,:'Bert',:some_list,[]) == [1,2,[3,4]]
    assert Client.call(conn,:'Bert',:some_binary,[]) == "This is a binary"
    assert Client.call(conn,:'Bert',:some_map,[]) == %{a: 1, b: 2}
    assert_raise RuntimeError, fn -> Client.call(conn,:'Bert',:exception1,[]) end
    assert_raise ArgumentError, fn -> Client.call(conn,:'Bert',:exception2,[]) end
  end

  test "argument passing", meta do
    conn=meta[:conn]
    assert Client.call(conn,:'Calc',:sum,[5,6]) == 11
  end
end

defmodule BertGate.Modules.Calc do
  def sum(x,y), do: x+y
end
