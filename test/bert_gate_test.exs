defmodule BertGateTest do
  use ExUnit.Case

  alias BertGate.Client

  @port 9485

  setup_all do
     Rpc.start_link
     authenticator = fn
        _,_,:calc_auth_token -> {[:'CalcPrivate'],:some_auth_data}
        _,_,_ -> nil
     end
     {:ok, server} = BertGate.Server.start_link(%{
        port: @port,
        authenticator: authenticator,
        public: [:'Bert',:'CalcPublic'],
     })
     {:ok, [server: server]}
  end

  setup do
     conn=Client.connect("localhost", %{port: @port})
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

  test "nonexistent module call", meta do
    conn=Client.connect("localhost", %{port: @port})
    assert_raise BERTError, "BERTError(401): Unauthorized. Closing connection.", fn ->
       Client.call(conn,:'Nonexistent',:somefun)
    end
  end

  test "nonexistent function call", meta do
    conn=Client.connect("localhost", %{port: @port})
    assert_raise UndefinedFunctionError, fn -> Client.call(conn,:'Bert',:nonexistent) end
  end

  test "invalid authentication", meta do
    conn=Client.connect("localhost", %{port: @port})
    assert_raise BERTError, "BERTError(401): Authentication failed. Closing connection.", fn -> Client.auth(conn,:invalid_token) end
  end

  test "public module, argument passing", meta do
    conn=meta[:conn]
    assert Client.call(conn,:'CalcPublic',:sum,[5,6]) == 11
  end

  test "access denied to private module when unauthenticated", meta do
    conn=Client.connect("localhost", %{port: @port})
    assert_raise BERTError, "BERTError(401): Unauthorized. Closing connection.", fn ->
       Client.call(conn,:'CalcPrivate',:sum,[5,6])
    end
  end

  # authentication does not remove already allowed modules
  test "authentication strictly adds allowed modules ", meta do
    conn=Client.connect("localhost", %{port: @port})
    assert Client.auth(conn,:calc_auth_token) == :ok
    assert Client.call(conn,:'CalcPublic',:sum,[5,6]) == 11
  end

  test "access allowed to private module when authenticated", meta do
    conn=meta[:conn]
    conn=Client.connect("localhost", %{port: @port})
    assert_raise BERTError, "BERTError(401): Unauthorized. Closing connection.", fn ->
       Client.call(conn,:'CalcPrivate',:sum,[5,6])
    end
    # connection is closed when unauthorized call is made
    assert_raise BERTClosed, "BERT-RPC connection closed", fn ->
       Client.call(conn,:'CalcPrivate',:sum,[5,6])
    end

    conn=Client.connect("localhost", %{port: @port})
    assert Client.auth(conn,:calc_auth_token) == :ok
    assert Client.call(conn,:'CalcPrivate',:sum,[5,6]) == 11
  end

  test "RPC manager" do
    Rpc.add_bert_server(:bert_public, "localhost", %{port: @port})
    assert Rpc.call(:bert_public, :'Bert', :ping) == :pong
    assert Rpc.call(:bert_public, :'CalcPublic', :sum, [5,6]) == 11
    Rpc.add_bert_server(:bert_private, "localhost", %{port: @port, auth: :calc_auth_token})
    assert Rpc.call(:bert_private, :'Bert', :ping) == :pong
    assert Rpc.call(:bert_private, :'CalcPrivate', :sum, [5,6]) == 11
  end
end

defmodule BertGate.Modules.CalcPublic do
  def sum(_,x,y), do: x+y
end

defmodule BertGate.Modules.CalcPrivate do
  def sum(_,x,y), do: x+y
end
