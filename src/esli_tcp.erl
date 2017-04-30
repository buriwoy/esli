-module(esli_tcp).
-export([listen/3, accept/2, send/3, setopts/3]).

listen (plain, Port, Opts) -> gen_tcp:listen(Port, Opts);
listen (ssl, Port, Opts) -> ssl:listen(Port, Opts).

accept (plain, Socket) -> gen_tcp:accept(Socket);
accept (ssl, Socket) ->
  case ssl:transport_accept(Socket) of
    {ok, S} ->
      case ssl:ssl_accept(S) of
        ok ->
          {ok, S};
        {error, closed} ->
          {error, econnaborted};
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

send (plain, Socket, Content) -> gen_tcp:send(Socket, Content);
send (ssl, Socket, Content) -> ssl:send(Socket, Content).

setopts(plain, Socket, Opts) -> inet:setopts(Socket, Opts);
setopts(ssl, Socket, Opts) -> ssl:setopts(Socket, Opts).
