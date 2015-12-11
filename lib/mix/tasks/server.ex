defmodule Mix.Tasks.Server do
  use Mix.Task

  @shortdoc "Run BERTGate server"

  def run([]) do
     BertGate.Server.init [%{}]
     receive do
     end
  end
end
