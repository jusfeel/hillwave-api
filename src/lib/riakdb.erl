-module(riakdb).
-compile(export_all).

-define(DB_HOST, "127.0.0.1").
-define(DB_PORT, 8087).

-include("riakc.hrl").

ping() ->
  Pid = connect_to_db(),
  riakc_pb_socket:ping(Pid),
  disconnect_db(Pid).

get_server_info() ->
  Pid = connect_to_db(),
  R = riakc_pb_socket:get_server_info(Pid),
  disconnect_db(Pid),
  R.

get_client_id() ->
  Pid = connect_to_db(),
  R = riakc_pb_socket:get_client_id(Pid),
  disconnect_db(Pid),
  R.

is_connected() ->
  Pid = connect_to_db(),
  R = riakc_pb_socket:is_connected(Pid),
  disconnect_db(Pid),
  R.

list_keys(Bucket) ->
  Pid = connect_to_db(),
  {ok, Keys} = riakc_pb_socket:list_keys(Pid, Bucket),
  disconnect_db(Pid),
  Keys.

list_buckets() ->
  Pid = connect_to_db(),
  {ok,Buckets} = riakc_pb_socket:list_buckets(Pid),
  disconnect_db(Pid),
  Buckets.
list_buckets(Type) ->
  Pid = connect_to_db(),
  {ok,Buckets} = riakc_pb_socket:list_buckets(Pid, Type),
  disconnect_db(Pid),
  Buckets.

get_bucket(Bucket) ->
  Pid = connect_to_db(),
  {ok, R} = riakc_pb_socket:get_bucket(Pid, Bucket),
  disconnect_db(Pid),
  R.

set_bucket(Bucket, BucketProps) ->
  Pid = connect_to_db(),
  R = riakc_pb_socket:set_bucket(Pid, Bucket, BucketProps),
  disconnect_db(Pid),
  R.

get_bucket_type(BucketType) ->
  Pid = connect_to_db(),
  R = riakc_pb_socket:get_bucket_type(Pid, BucketType),
  disconnect_db(Pid),
  R.

list_search_indexes() ->
  Pid = connect_to_db(),
  R = riakc_pb_socket:list_search_indexes(Pid),
  disconnect_db(Pid),
  R.

get_search_index(Index) ->
  Pid = connect_to_db(),
  R = riakc_pb_socket:get_search_index(Pid, Index),
  disconnect_db(Pid),
  R.

create_search_index(Index, Schema) ->
  Pid = connect_to_db(),
  R = riakc_pb_socket:create_search_index(Pid, Index, Schema, []),
  disconnect_db(Pid),
  R.

delete_search_index(Index) ->
  Pid = connect_to_db(),
  R = riakc_pb_socket:delete_search_index(Pid, Index),
  disconnect_db(Pid),
  R.

update_counter(Bucket, Key, Increment) ->
  Counter = riakc_counter:new(),
  Counter1 = riakc_counter:increment(Increment, Counter),
  Update = riakc_counter:to_op(Counter1),
  Pid = connect_to_db(),
  R = riakc_pb_socket:update_type(Pid, Bucket, Key, Update),
  disconnect_db(Pid),
  R.

update_set(Bucket, Key, List) ->
  Set = set_add_elements(riakc_set:new(), List),
  Update = riakc_set:to_op(Set),
  io:format("update:~p~n", [Update]),
  Pid = connect_to_db(),
  R = riakc_pb_socket:update_type(Pid, Bucket, Key, Update),
  disconnect_db(Pid),
  R.

set_add_elements(Set, []) ->
  Set;
set_add_elements(Set, [H|T]) ->
  NewSet = riakc_set:add_element(H, Set),
  set_add_elements(NewSet, T).

%% add map
add_customer(Bucket, Key, Customer) ->
  Map = riakc_map:new(),
  Map1 = riakc_map:update({<<"first_name">>, register}, 
                          fun(R) -> riakc_register:set(proplists:get_value(first_name, Customer), R) end, Map),
  Map2 = riakc_map:update({<<"last_name">>, register}, 
                          fun(R) -> riakc_register:set(proplists:get_value(last_name, Customer), R) end, Map1),
  case proplists:get_value(enterprise_customer, Customer) of
    false -> Map3 = Map2;
    true -> Map3 = riakc_map:update({<<"enterprise_customer">>, flag},
                                                  fun(F) -> riakc_flag:enable(F) end, Map2)
  end,
  Map4 = riakc_map:update({<<"page_visits">>, counter},
                          fun(C) -> riakc_counter:increment(proplists:get_value(page_visits, Customer), C) end, Map3),
  Interests = proplists:get_value(interests, Customer),
  Map5 = add_interests(Map4, Interests),
  Update = riakc_map:to_op(Map5),
  io:format("update:~p~n", [Update]),
  Pid = connect_to_db(),
  R = riakc_pb_socket:update_type(Pid, Bucket, Key, Update),
  disconnect_db(Pid),
  R.

add_interests(Map, []) ->
  Map;
add_interests(Map, [H|T]) ->
  NewMap = riakc_map:update({<<"interests">>, set},fun(S) -> riakc_set:add_element(H, S) end, Map),
  add_interests(NewMap, T).

add_ego_name(Bucket, Key, Name) ->
  Pid = connect_to_db(),
  {ok, Customer} = riakc_pb_socket:fetch_type(Pid, Bucket, Key),
  NewCustomer = riakc_map:update({<<"alter_ego">>, map},
                           fun(M) -> riakc_map:update({<<"name">>, register}, 
                                                      fun(R) -> riakc_register:set(Name, R) end, M) 
                           end, 
                           Customer),
  Update = riakc_map:to_op(NewCustomer),
  R = riakc_pb_socket:update_type(Pid, Bucket, Key, Update),
  disconnect_db(Pid),
  R.

new_obj(Bucket, Key, Value) ->
  C = riakc_obj:new(Bucket, Key, Value, "application/json"),
  Pid = connect_to_db(),
  riakc_pb_socket:put(Pid, C),
  disconnect_db(Pid).

delete(Bucket, Key) ->
  Pid = connect_to_db(),
  R = riakc_pb_socket:delete(Pid, Bucket, Key),
  disconnect_db(Pid),
  R.

clear_bucket(Bucket) ->
  Pid = connect_to_db(),
  Keys = list_keys(Bucket),
  F = fun(Key) ->
        delete(Bucket, Key),
        ok
      end,
  Res = lists:map(F, Keys),
  disconnect_db(Pid),
  length(Res). 

get(Bucket, Key) ->
  Pid = connect_to_db(),
  {ok, Obj} = riakc_pb_socket:get(Pid, Bucket, Key),
  disconnect_db(Pid),
  Obj.

fetch_type(Bucket, Key) ->
  Pid = connect_to_db(),
  R = riakc_pb_socket:fetch_type(Pid, Bucket, Key),
  io:format("Fetch type:~p~n",[R]),
  disconnect_db(Pid),
  R.

get_index(Bucket, SecondaryIndex, Key) ->
  Pid = connect_to_db(),
  R = riakc_pb_socket:get_index(Pid, Bucket, SecondaryIndex, Key),
  disconnect_db(Pid),
  R.

connect_to_db() ->
  {ok, Pid} = riakc_pb_socket:start_link(?DB_HOST, ?DB_PORT),
  Pid.

disconnect_db(Pid) ->
  riakc_pb_socket:stop(Pid).
 
%% Dump bucket into a riakc_obj list and write to a file
export(Bucket, FileName) ->
  Pid = connect_to_db(),
  Keys = list_keys(Bucket),
  F = fun(Key) ->
        get(Bucket, Key)
      end,
  Res = lists:map(F, Keys),
  disconnect_db(Pid),
  if Res =:= [] ->
       error_empty_bucket;
     true ->
       write(Res, FileName),
       Res
  end.

%% restore a bucket in same bucket type from file
restore(File) ->
  {ok, Data} = file:read_file(File),
  Pid = connect_to_db(),
  KVList = binary_to_term(Data),
  F = fun(Elem) -> 
    riakc_pb_socket:put(Pid, Elem),
    ok
  end,
  Res = lists:map(F, KVList),
  disconnect_db(Pid),
  length(Res).

%% @doc import bucket into a new bucket type
import(File, NewBT) ->
  {ok, Data} = file:read_file(File),
  Pid = connect_to_db(),
  KVList = binary_to_term(Data),
  F = fun(Elem) ->
    Bucket  = {NewBT, riakc_obj:only_bucket(Elem)},    
    Key 		= riakc_obj:key(Elem),
    Value 	= riakc_obj:get_value(Elem),
    CT 			= riakc_obj:get_content_type(Elem),
    NewElem = riakc_obj:new(Bucket, Key, Value, CT),
    riakc_pb_socket:put(Pid, NewElem),
    ok
  end,
  Res = lists:map(F, KVList),
  disconnect_db(Pid),
  length(Res).

write(RiakObjList, FileName) when is_list(FileName) ->
    Bin = term_to_binary(RiakObjList),
    file:write_file(FileName, Bin).
