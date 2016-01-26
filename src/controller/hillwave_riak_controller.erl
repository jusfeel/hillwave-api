-module(hillwave_riak_controller,[Req, SessionId]).
-compile(export_all).

%-include("letsparty.hrl").
-include("riakc.hrl").

-define(ANIMALS, <<"animals">>).
-define(CATS, <<"cats">>).

-define(DB_HOST, "127.0.0.1").
-define(DB_PORT, 8087).

before_(_) ->
   ok.
%  {redirect, "/"}.

index('GET', []) ->
  io:format("ping: ~p~n",[riakdb:ping()]),
  case riakdb:ping() of
    ok -> 
      ConnectionStatus = true;
    _ -> 
      ConnectionStatus = false
  end,
  {ok, [{connection_status, ConnectionStatus}]}.

buckets('GET', []) ->
  {ok, []};
buckets('POST', []) ->
  Type = list_to_binary(Req:post_param("type")),
  {ok, [{type, Type},{buckets, riakdb:list_buckets(Type)}]}.

get_bucket('GET', []) ->
  {ok, []};
get_bucket('POST', []) ->
  Type = list_to_binary(Req:post_param("type")),
  Bucket = list_to_binary(Req:post_param("bucket")),
  {ok, [{type, Type},{bucket, Bucket},{props, riakdb:get_bucket({Type,Bucket})}]}.

set_bucket('GET', []) ->
  {ok, []};
set_bucket('POST', []) ->
  Type = list_to_binary(Req:post_param("type")),
  Bucket = list_to_binary(Req:post_param("bucket")),
  Prop = ic_pragma:list_to_term(Req:post_param("prop")),
  Result = riakdb:set_bucket({Type,Bucket}, Prop),
  {ok, [{type,Type},{bucket, Bucket},{result, Result}]}.

search_indexes('GET',[]) ->
  {ok, Indexes} = riakdb:list_search_indexes(),
  {ok, [{indexes, Indexes}]};
search_indexes('POST',[]) ->
  Index = list_to_binary(Req:post_param("index")),
  Schema = list_to_binary(Req:post_param("schema")),
  Result = riakdb:create_search_index(Index, Schema),
  {ok, [{result, Result}]}.

delete_search_index('GET',[]) ->
  Index = list_to_binary(Req:query_param("index")),
  case riakdb:delete_search_index(Index) of
    {error, R} -> throw(R);
    _ -> {redirect, "/riak"}
  end.

%% Add a cat into {animals,cats}
new_obj('POST',[]) ->
  Json = Req:request_body(),

  %% generate a key based on cat's name
  {struct, Decoded} = mochijson:decode(Json),
  io:format("Decoded json: ~p~n", [Decoded]),
  Name = proplists:get_value("name_s",Decoded),
  Key = list_to_binary(string:to_lower(util:strip_white_spaces(Name))),

  io:format("Key: ~p~n", [Key]),
  io:format("POST Req:request_body():~p~n", [Req:request_body()]),
  riakdb:new_obj({?ANIMALS, ?CATS}, Key, Json),
  {json, [{success,true}]}.

create_counter('GET',[]) ->
  {ok, []};
create_counter('POST',[]) ->
  Type = list_to_binary(Req:post_param("type")),
  Bucket = list_to_binary(Req:post_param("bucket")),
  Key = list_to_binary(Req:post_param("key")),
  Increment = list_to_integer(Req:post_param("increment")),
  Result = riakdb:update_counter({Type, Bucket},Key, Increment),
  {ok, [{result, Result}]}.

create_set('GET',[]) ->
  {ok, []};
create_set('POST',[]) ->
  Type = list_to_binary(Req:post_param("type")),
  Bucket = list_to_binary(Req:post_param("bucket")),
  Key = list_to_binary(Req:post_param("key")),
  Set = proplists:get_all_values("set", Req:post_params()),
  Result = riakdb:update_set({Type, Bucket},Key, [list_to_binary(I) || I <- Set]),
  {ok, [{result, Result}]}.

create_map('GET',[]) ->
  {ok, []};
create_map('POST',[]) ->
  Type = list_to_binary(Req:post_param("type")),
  Bucket = list_to_binary(Req:post_param("bucket")),
  Key = list_to_binary(Req:post_param("key")),
  FirstName = list_to_binary(Req:post_param("first_name")),
  LastName = list_to_binary(Req:post_param("last_name")),
  F = fun(E) ->
          case E of
            "on" -> true;
            _ -> false
          end
      end,
  EnterpriseCustomer = F(Req:post_param("enterprise_customer")),
  PageVisits = list_to_integer(Req:post_param("page_visits")),
  Interests = proplists:get_all_values("interests", Req:post_params()),
  Customer = [{first_name, FirstName},
             {last_name, LastName},
             {enterprise_customer, EnterpriseCustomer},
             {page_visits, PageVisits},
             {interests, [list_to_binary(I) || I <- Interests]}],
  Result = riakdb:add_customer({Type, Bucket}, Key, Customer),
  io:format("Result: ~p~n", [Result]),
  {ok, [{result, Result}]}.

embed_map('GET',[]) ->
  Keys = riakdb:list_keys({<<"maps">>,<<"customers">>}),
  {ok, [{keys, [binary_to_list(I) || I <- Keys]}]};
embed_map('POST',[]) ->
  Key = list_to_binary(Req:post_param("key")),
  Name = list_to_binary(Req:post_param("alter_ego")),
  Result = riakdb:add_ego_name({<<"maps">>,<<"customers">>}, Key, Name),
  {ok, [{result, Result}]}.

simple_query('GET', []) ->
  {ok, []};
simple_query('POST', []) ->
  Index = list_to_binary(Req:post_param("index")),
  Conditions = list_to_binary(Req:post_param("condition")),
  {ok, Pid} = riakc_pb_socket:start_link(?DB_HOST, ?DB_PORT),
  {ok, Results} = riakc_pb_socket:search(Pid, Index, Conditions),
  io:fwrite("~p~n", [Results]),
  Docs = Results#search_results.docs,
  MaxScore = Results#search_results.max_score,
  NumFound = Results#search_results.num_found,
  io:fwrite("~p~n", [Docs]),
  riakc_pb_socket:stop(Pid),
  {ok, [{index, Index},{conditions, Conditions},{search_results, Results},{search_results_max_score, MaxScore},{search_results_num_found, NumFound},{docs, Docs}]}.

pagination('GET', []) ->
  {ok, []};
pagination('POST', []) ->
  Conditions = list_to_binary(Req:post_param("condition")),
  Start = list_to_integer(Req:post_param("start")),
  Rows = list_to_integer(Req:post_param("rows")),
  Index = list_to_binary(Req:post_param("index")),
  {ok, Pid} = riakc_pb_socket:start_link(?DB_HOST, ?DB_PORT),
  {ok, Results} = riakc_pb_socket:search(Pid, Index, Conditions, [{sort,<<"timestamp_register desc">>},{start, Start},{rows, Rows}]),
  io:fwrite("~p~n", [Results]),
  Docs = Results#search_results.docs,
  MaxScore = Results#search_results.max_score,
  NumFound = Results#search_results.num_found,
  io:fwrite("~p~n", [Docs]),
  riakc_pb_socket:stop(Pid),
  {ok, [{conditions, Conditions},{start,Start},{rows,Rows},{search_results, Results},{search_results_max_score, MaxScore},{search_results_num_found, NumFound},{docs, Docs}]}.

delete('GET', []) ->
  {ok, []};
delete('POST', []) ->
  Type = list_to_binary(Req:post_param("type")),
  Bucket = list_to_binary(Req:post_param("bucket")),
  Key = list_to_binary(Req:post_param("key")),
  Result = riakdb:delete({Type, Bucket}, Key),
  {ok, [{result, Result}]}.




