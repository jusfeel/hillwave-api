-module(security).
-compile(export_all).
-include("hillwave.hrl").

%% @doc check password to be true or false

-spec check_password(string(), string()) -> boolean().

check_password(Password, PasswordHash) ->
    bcrypt:hashpw(Password, PasswordHash) =:= {ok, PasswordHash}.

%% @doc generate password hash

-spec generate_password_hash(string()) -> string().

generate_password_hash(Password) ->
  {ok, Salt} = bcrypt:gen_salt(),
  {ok, Hash} = bcrypt:hashpw(Password, Salt),
  Hash.

is_logged_in(SessionId) ->
  case boss_session:get_session_data(SessionId, "SESSMEMID") of
        undefined -> false;
        _ -> true
     end.
 
