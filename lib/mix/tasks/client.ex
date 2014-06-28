defmodule Mix.Tasks.Client do
  use Mix.Task

  def run([]) do
     Rpc.start_link
  end
end

defmodule Mix.Tasks.Ping do
  use Mix.Task

  def run([]) do
     conn = BertGate.Client.connect("localhost")
     reply = BertGate.Client.call(conn,:'Bert',:ping,[])
     IO.puts "Reply: #{inspect reply}"
  end
end
