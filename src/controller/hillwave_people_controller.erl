-module(hillwave_people_controller, [Req, SessionId]).
-compile(export_all).
-include("hillwave.hrl").

before_(_,Method,_) when Method =:= 'OPTIONS' -> {ok,ok};
before_(_,Method,_) when Method =:= 'GET' -> {ok,ok};
before_(_,_,_) -> account_lib:require_login(Req).

index('OPTIONS', _, _) -> {200, "", ?OPTA};
index(_, _, {error, Reason}) -> hwdb:err(Reason);
index('GET', [Id], _) -> hwdb:find(Id, [works,quotes,proses]);
index('GET', [], _) ->
  PerPage0 = Req:query_param("limit"),
  Page0 = Req:query_param("cp"),
  ?DEBUG("page is ~p", Page0),
  if PerPage0 =/= undefined ->
       PerPage = list_to_integer(PerPage0);
     true ->
       PerPage = 6
  end,
  if Page0 =/= undefined ->
       Page = list_to_integer(Page0),
       Opts = [{offset, PerPage * (Page - 1)},{limit, PerPage}, {order_by, created}, {descending, true}],
       hwdb:find(person, [], Opts);
     true -> hwdb:find(person, [], [{order_by, created}, {descending, true}])
  end;
index('POST', [], {ok,_}) -> hwdb:post(Req);
index('PATCH', [Id], {ok,_}) -> hwdb:patch(Req);
index('DELETE', [Id], {ok,_}) -> hwdb:delete(Id).
