## based on http://github.com/mojombo/bert.erl
## support for maps added

defmodule Bert do
   def encode(term), do: :erlang.term_to_binary(encode_term(term))
   def decode(term), do: decode_term(:erlang.binary_to_term(term))

   defp encode_term([]), do: {:bert,nil}
   defp encode_term(true), do: {:bert,true}
   defp encode_term(false), do: {:bert,false}
   defp encode_term(map) when is_map(map), do:
      {:bert, :dict, Map.to_list(map)}
   defp encode_term(list) when is_list(list) do
      list |> Enum.map(&encode_term/1)
   end
   defp encode_term(tuple) when is_tuple(tuple) do
      tuple |> :erlang.tuple_to_list
            |> Enum.map(&encode_term/1)
            |> :erlang.list_to_tuple
   end
   defp encode_term(term), do: term

   defp decode_term({:bert,nil}), do: []
   defp decode_term({:bert,true}), do: true
   defp decode_term({:bert,false}), do: false
   defp decode_term({:bert,:dict,list}) do
      list |> Enum.into(%{})
   end
   defp decode_term({:bert,other}), do: {:bert,other}
   defp decode_term(list) when is_list(list) do
      list |> Enum.map(&decode_term/1)
   end
   defp decode_term(tuple) when is_tuple(tuple) do
      tuple |> :erlang.tuple_to_list
            |> Enum.map(&decode_term/1)
            |> :erlang.list_to_tuple
   end
   defp decode_term(term), do: term
end
