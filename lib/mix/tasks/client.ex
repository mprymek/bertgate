defmodule Mix.Tasks.Client do
  use Mix.Task

  @shortdoc "Run BERTGate test client"

  def run([]) do
     Rpc.start_link
  end
end

defmodule Mix.Tasks.ClientPing do
  use Mix.Task

  @shortdoc "Run BERTGate test client - calls Bert.ping()"

  def run([]) do
     conn = BertGate.Client.connect("localhost")
     reply = BertGate.Client.call(conn,:'Bert',:ping,[])
     IO.puts "Reply: #{inspect reply}"
  end
end
