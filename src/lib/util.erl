-module(util).
-compile(export_all).

%% @doc Convert erlang:now() to ISO8601 2015-09-22T13:22:20Z
%%
%% see [https://cwiki.apache.org/confluence/display/solr/Working+with+Dates]
%% @see iso_to_local_time/1
-spec now_iso() -> string().
now_iso() ->
  binary_to_list(iso8601:format(now())).

%% @doc Convert ISO8601 to local time {{2016,1,14},{15,54,11}}
%%
-spec iso_to_local_time(string()) -> tuple().
iso_to_local_time(Iso) ->
  calendar:universal_time_to_local_time(iso8601:parse(Iso)).

%% @doc Generate SHA hash from string
%%
sha(S) ->
    Sha_list = binary_to_list(crypto:hash(sha, S)),
    lists:flatten(list_to_hex(Sha_list)).

%% Convert Integer from the SHA to Hex
list_to_hex(L)->
       lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
       [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
       $0+N;
hex(N) when N >= 10, N < 16 ->
       $a + (N-10).


