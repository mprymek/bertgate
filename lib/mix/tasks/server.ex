defmodule Mix.Tasks.Server do
  use Mix.Task

  @shortdoc "Run BERTGate server"

  def run(argv) do
     opts = case OptionParser.parse argv do
       {opts,[],[]} -> opts
       _ -> raise "Usage: mix server [--port PORT] [--public Module1,Module2]"
     end
     opts = case opts[:port] do
       nil -> opts
       port -> opts |> Keyword.put(:port,String.to_integer(port))
     end
     opts = case opts[:public] do
       nil -> opts
       pubspec ->
         public = pubspec
         |> String.split(",")
         |> Enum.map(fn mod -> mod |> String.to_atom end)
         opts |> Keyword.put(:public,public)
     end
     BertGate.Server.init [opts]
     receive do
     end
  end
end
