-module(member, [Id, 
                 Email::string(), 
                 PasswordHash::string(), 
                 Username::string(),
                 Active::boolean(), 
                 Blocked::boolean(), 
                 Created::string()]).
-compile(export_all).

-has({verifications, many}).
-has({works, many}).
-has({proses, many}).

%% @doc check password to be true or false
check_password(Password) ->
  security:check_password(Password, PasswordHash).
