-module(hillwave_account_controller, [Req, SessionId]).
-compile(export_all).
-include("hillwave.hrl").

%% @doc login
%%
token('OPTIONS', []) -> {json, [], ?OPTP};
token('POST', []) ->
  GrantType = Req:post_param("grant_type", ""),
  Username  = Req:post_param("username", ""),
  Password  = Req:post_param("password", ""),
  Scope     = Req:post_param("scope", ""),

  InvalidRequest = "{\"error\":\"invalid_request\"}",
  case GrantType of
    "refresh_token" ->
       case Req:post_param("refresh_token") of
         undefined ->
           {400, InvalidRequest, ?HEADERS};
         RefreshToken ->
           case auth_lib:refresh_token(RefreshToken) of
             {error, ErrorJson} -> {400, ErrorJson, ?HEADERS};
             {ok, Response}     -> {json, Response, ?HEADERS}
           end
       end;
    "password" ->
       Uname = string:strip(Username),
       Pass  = string:strip(Password),
       if Uname =:= undefined; Pass =:= undefined ->
            {400, InvalidRequest, ?HEADERS};
          true ->
            case auth_lib:get_access_token_via_password({Uname, Pass}, Scope, []) of
              {error, ErrorJson} -> {400, ErrorJson, ?HEADERS};
              {ok, Response}     -> {json, Response, ?HEADERS}
            end
       end
  end.


