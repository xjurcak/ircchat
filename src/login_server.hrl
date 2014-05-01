-define(LOGIN_SERVER_GLOBAL, login_server).
-define(LOGIN_SERVER_SUP_GLOBAL, login_server_supervisor).

-define(SERVER, ?MODULE).
-define(LOGINS_TABLE, login).
-define(EXPIRATION, 30).

-record(login, {name, expiration, listener}).