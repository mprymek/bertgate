defmodule BERTError do
   defexception [type: nil, code: nil, class: nil, detail: "", backtrace: nil]

   def message(exc) do
      if is_bitstring(exc.detail) do
         "BERTError(#{exc.code}): #{exc.detail}"
      else
         # we MUST use inspect(exc.detail) here because we transport user exceptions in this field
         "BERTError(#{exc.code}): #{inspect exc.detail}"
      end
   end
end

defmodule BERTClosed do
   defexception [message: "BERT-RPC connection closed"]
end
