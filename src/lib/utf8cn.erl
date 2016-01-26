-module(utf8cn).
-compile(export_all).


bin_to_list(B) ->
  {ok, Ret} = asn1rt:utf8_binary_to_list(B),
  Ret.

list_to_bin(B) ->
  {ok, Ret} = asn1rt:utf8_list_to_binary(B),
  Ret.
