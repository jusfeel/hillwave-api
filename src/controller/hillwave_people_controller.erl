-module(hillwave_people_controller, [Req, SessionId]).
-compile(export_all).
-include("hillwave.hrl").

before_(_,Method,_) when Method =:= 'OPTIONS' -> {ok,ok};
before_(_,Method,_) when Method =:= 'GET' -> {ok,ok};
before_(_,_,_) -> account_lib:require_login(Req).

index('OPTIONS', _, _) -> {200, "", ?OPTA};
index(_, _, {error, Reason}) -> hwdb:err(Reason);
index('GET', [Id], _) -> hwdb:find(Id, [works,quotes,proses]);
index('GET', [], _) -> hwdb:find(person, [], [{order_by, created}, {descending, true}]);
index('POST', [], {ok,_}) -> hwdb:post(Req);
index('PATCH', [Id], {ok,_}) -> hwdb:patch(Req);
index('DELETE', [Id], {ok,_}) -> hwdb:delete(Id).
