-define(API_KEY, "3NC4vxfEiej5niCnHlKJf5WNnBHqqUCu").
-define(SECRET_WORD, "ILIKEEMBERJS").
-define(UPLOAD_ROOT, "/opt/www/hillwave/priv/static/upload").
-define(DOMAIN, "http://dev.hillwave.cn").
-define(IMAGE_DOMAIN, "http://dev.hillwave.cn/static/upload/").

-define(debug, ok).
-define(logging, ok).

-ifdef(debug).
-define(DEBUG(Format, Args), io:format("{~s,~w} DEBUG: ~n" ++ Format ++ "~n", [ ?MODULE, ?LINE, Args])).
-else.
-define(DEBUG(Format, Args), true).
-endif.

-ifdef(logging).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE,?LINE,X])).
-else.
-define(LOG(X), true).
-endif.


-define(ERR_INVALID_REQUEST, "698").
-define(ERR_SERVER_ERROR, "699").

-define(ERR_OPERATION_FAILURE, "700").
-define(ERR_INVALID_EMAIL_FORMAT, "701").
-define(ERR_PASSWORDS_NOT_SAME, "702").
-define(ERR_INCOMPLETE_INPUT, "703").
-define(ERR_PASSWORD_TOO_SHORT, "704").
-define(ERR_USERNAME_TOO_LONG, "705").
-define(ERR_ACCOUNT_EXISTS, "706").

-define(MIN_PASSWORD_LENGTH, 6).
-define(MAX_USERNAME_LENGTH, 22).

%% @doc Header Macro
%%
-define(CONTENT_TYPE, {"Content-Type", "application/vnd.api+json"}).
-define(ACAMG, {"Access-Control-Allow-Methods", "GET, OPTIONS"}).
-define(ACAMP, {"Access-Control-Allow-Methods", "POST, OPTIONS"}).
-define(ACAMA, {"Access-Control-Allow-Methods", "GET, POST, DELETE, PATCH, OPTIONS"}).
-define(ACAH, {"Access-Control-Allow-Headers", "authorization,content-type,api_key,secret_word"}).
-define(ACAO, {"Access-Control-Allow-Origin", "http://localhost:4200"}).

-define(ALLOW_G, [{"Access-Control-Allow-Methods", "GET, OPTIONS"}, {"Access-Control-Allow-Headers", "authorization"}]).
-define(ALLOW_P, [{"Access-Control-Allow-Methods", "POST, OPTIONS"}, {"Access-Control-Allow-Headers", "authorization"}]).
-define(ALLOW_A, [{"Access-Control-Allow-Methods", "GET, PATCH, POST, OPTIONS"}, {"Access-Control-Allow-Headers", "authorization"}]).

-define(OPTG, [?CONTENT_TYPE, ?ACAO, ?ACAH, ?ACAMG]).
-define(OPTP, [?CONTENT_TYPE, ?ACAO, ?ACAH, ?ACAMP]).
-define(OPTA, [?CONTENT_TYPE, ?ACAO, ?ACAH, ?ACAMA]).
-define(HEADERS, [?CONTENT_TYPE, ?ACAO]).

-define(INVALID_REQUEST, "{\"error\":\"invalid_request\"}").




