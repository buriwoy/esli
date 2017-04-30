-module(esli_acceptor).

-export([init/2]).

init (ListenSocket, Type) ->
  {ok, Socket} = esli_tcp:accept(Type, ListenSocket),
  spawn(fun() -> connect(ListenSocket, Type) end),
  loop(Socket, #{}, Type).

connect (Listen, Type) ->
  %file:write_file("/tmp/connect", "1\n", [append]),
  {ok, Socket} = esli_tcp:accept(Type, Listen),
  spawn(fun() -> connect(Listen, Type) end),
  loop(Socket, #{}, Type).

loop (Socket, Req, Type) ->
  %ok = inet:setopts(Socket, [{active, once}]),
  case esli_tcp:setopts(Type, Socket, [{active, once}]) of
    ok -> ok;
    {error, Error} ->
      file:write_file("/tmp/ineterrors", io_lib:fwrite("~p~n", [Error]), [append])
  end,
  receive
    {_, Socket, Data} ->
      %io:format("Data: ~p~n", [Data]),
      case Data of
        {http_request, Method, {abs_path, Uri}, Vsn} ->
          {Path, GetParams} = esli_req:get_params(Uri),
          NReq = #{method => Method, path => Path, version => Vsn, get_params => GetParams};
        {http_header, _, Hdr, _, Val} ->
          NReq = Req#{Hdr => Val};
        http_eoh ->
          NReq = Req,
          case maps:get(method, Req) of
            'GET' ->
              respond(Socket, NReq, Type);
            _ ->
              esli_tcp:setopts(Type, Socket, [{packet, 0}])
          end;
        RawBody ->
          %io:format("BinS: ~p~n", [Bin]),
          Body = re:replace(RawBody, <<"\r\n">>, <<>>, [{return, binary}]),
          NReq = Req#{body => esli_req:qs(Body)},
          respond(Socket, NReq, Type)
      end,
      loop(Socket, NReq, Type);
%    {tcp, Socket, Bin} ->
%      io:format("Bin: ~p~n", [Bin]),
%      Body = re:replace(Bin, <<"\r\n">>, <<>>, [{return, binary}]),
%      NReq = Req#{body => esli_req:qs(Body)},
%      respond(Socket, NReq, Type),
%      %inet:setopts(Socket, [{active, once}]),
%      loop(Socket, #{}, Type);
    {tcp_error, Socket, Reason} ->
      io:format("Error: ~p~n", [Reason]);
    {tcp_closed, Socket} ->
      io:format("Socket closed~n");
    {ssl_error, Socket, Reason} ->
      io:format("SSL Error: ~p~n", [Reason]);
    {ssl_closed, Socket} ->
      io:format("SSL Socket closed~n")
  end.


respond (Socket, Req, Type) ->
  Content = case gen_server:call(esli_router, {get, Req}) of
    not_found ->
      esli_http:build(code, 404, Req);
    {ok, Handler, Args} ->
      {ok, Response, Req2} = Handler:handle(Args, Req),
      esli_http:build(Response, Req2)
  end,
  esli_tcp:send(Type, Socket, Content),
  ok.
