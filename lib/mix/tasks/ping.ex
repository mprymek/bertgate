defmodule Mix.Tasks.Ping do
  use Mix.Task

  @shortdoc "Call Bert.ping() using BERTRpc on the given host (or localhost)"

  def run(argv) do
     {server,opts} = case OptionParser.parse argv do
       {opts,[],[]} ->
         {"localhost",opts}
       {opts,[server],[]} ->
         {server,opts}
       _ ->
         raise "Usage: mix ping [host]"
     end
     opts = case opts[:port] do
       nil -> opts
       port -> opts |> Keyword.put(:port,String.to_integer(port))
     end
     conn = BertGate.Client.connect(server,opts)
     reply = BertGate.Client.call(conn,:'Bert',:ping,[])
     IO.puts "Reply: #{inspect reply}"
  end
end
