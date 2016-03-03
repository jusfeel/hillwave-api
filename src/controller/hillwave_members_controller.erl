-module(hillwave_members_controller, [Req, SessionId]).
-compile(export_all).
-include("hillwave.hrl").

before_(_,_,_) -> account_lib:require_login(Req).

index(_, _, {error, Reason}) -> hwdb:err(Reason);

index('OPTIONS', [Id], _) -> {json, [], ?OPTG};
index('OPTIONS', [], _) -> {json, [], ?OPTA};
index('GET', [], _) ->
  ?DEBUG("~p",[]),
  {json, []};

%% @doc registration
index('POST', [], _) ->
  ReqBody = jsx:decode(Req:request_body(), [{labels, atom}]),
  Keys = [email, username, password1, password2],
  [Email, Username, Password1, Password2] = jsonapi:attributes(Keys, ReqBody),
  case validation:validate_registration(Email, Username, Password1, Password2) of
    ok              -> Result = account_lib:register(Email, Username, Password1);
    {error, Errors} -> Result = Errors
  end,
  {json, Result, ?HEADERS}.



