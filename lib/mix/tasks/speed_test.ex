defmodule Timing do
   def test_avg(m, f, a, n) when n > 0 do
       l = test_loop(m, f, a, n, [])
       len = length(l)
       min = :lists.min(l)
       max = :lists.max(l)
       med = :lists.nth(round((len / 2)), :lists.sort(l))
       avg = round(:lists.foldl(fn(x, sum) -> x + sum end, 0, l) / len)
       IO.puts "Range: #{min} - #{max} us"
       IO.puts "Median: #{med} us"
       IO.puts "Average: #{avg} us"
       med
   end

   defp test_loop(_m, _f, _a, 0, list), do: list
   defp test_loop(m, f, a, n, list) do
       {t, _result} = :timer.tc(m, f, a)
       test_loop(m, f, a, n - 1, [t|list])
   end
end

defmodule Mix.Tasks.SpeedTest do
   use Mix.Task

   @shortdoc "Run BERTGate speed test"

   def run([server]) do
      IO.puts "**** BertGate"
      s=BertGate.Client.connect("localhost")
      Timing.test_avg BertGate.Client, :call, [s,:"Bert",:ping,[]], 10000

      IO.puts "\n**** :rpc"
      server = String.to_atom(server)
      true = Node.connect server
      Timing.test_avg :rpc, :call, [server, BertGate.Modules.Bert, :ping, []], 10000
   end
end
