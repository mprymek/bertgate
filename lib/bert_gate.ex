defmodule BertGate.Logger.Helper do
   defmacro logfun(fun,level,prefix) do
      quote bind_quoted: [fun: fun, prefix: prefix, level: level] do
         if @loglevel > level do
            def unquote(fun)(s) do IO.puts unquote(prefix)<>s end
         else
            def unquote(fun)(s), do: :ok
         end
      end
   end
end

# if you want, you can delegate these functions to ExLager etc.
defmodule BertGate.Logger do
   require BertGate.Logger.Helper
   alias BertGate.Logger.Helper

   @loglevel 3

   Helper.logfun(:debug,4,"[DEBUG] ")
   Helper.logfun(:info,3,"[INFO ] ")
   Helper.logfun(:notice,2,"[NOTIC] ")
   Helper.logfun(:warning,1,"[WARN ] ")
   Helper.logfun(:error,0,"[ERROR] ")
end

defmodule BERTError do
   defexception [message: "BERT-RPC error", details: nil]
end

defmodule BERTClosed do
   defexception [message: "BERT-RPC connection closed"]
end
