defmodule Mix.Tasks.Server do
  use Mix.Task

  def run([]) do
     BertGate.Server.init [%{}]
     receive do
     end
  end
end
