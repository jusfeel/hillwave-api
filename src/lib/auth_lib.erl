-module(auth_lib).
-compile(export_all).
-include("hillwave.hrl").

get_access_token_via_password({Username, Password}, Scope, Ctx) ->
  case oauth2:authorize_password({Username,Password}, [Scope], Ctx) of
    {error, _Reason} -> 
      {error, "{\"error\":\"invalid_grant\"}"};
    {ok, {_AppCtx, Auth}} -> 
			{ok, {_, Token}} = oauth2:issue_token(Auth, []),
		  Response = oauth2_response:to_proplist(Token),
			?DEBUG("Json response:~p", Response),
      {ok, Response}
  end.

%% @TODO Refresh token
refresh_token(_RefreshToken) ->
  %% verify refresh token
  %% invalid refresh token or
  %% issue new token
  {error, "{\"error\":\"invalid_grant\"}"}.


