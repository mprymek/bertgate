defmodule Mix.Tasks.Server do
  use Mix.Task

  @shortdoc "Run BERTGate server"

  def run(argv) do
     opts = case OptionParser.parse argv do
       {opts,[],[]} ->
         public = Dict.get(opts,:public,"")
         |> String.split(",")
         |> Enum.map(fn mod -> mod |> String.to_atom end)
         opts |> Keyword.put(:public,public)
     end
     BertGate.Server.init [opts]
     receive do
     end
  end
end
