defmodule Mix.Tasks.Client do
  use Mix.Task

  def run([]) do
     Rpc.start_link
  end
end
