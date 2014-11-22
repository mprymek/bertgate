defmodule Mix.Tasks.Server do
  use Mix.Task

  def run([]) do
     BertGate.Server.init [%{}]
     receive do
     end
  end
end

defmodule Mix.Tasks.ServerPing do
  use Mix.Task

  def run([]) do
     conn = BertGate.Client.connect("localhost")
     reply = BertGate.Client.call(conn,:'Bert',:ping,[])
     IO.puts "Reply: #{inspect reply}"
  end
end
