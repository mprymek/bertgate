defmodule Mix.Tasks.Client do
  use Mix.Task

  @shortdoc "Run BERTGate test client"

  def run([]) do
     Rpc.start_link
  end
end
