-module(esli_req).
-export([parse/1, qs/1, get_params/1]).
-record(req, {
%%  method = <<"GET">> :: binary(),
%%  host = undefined :: binary(),
%%  path = <<"/">>,
%%  user_agent = undefined :: binary(),
%%  referer = undefined :: binary(),
%%  connection = undefined :: binary(),
%%  pragma = undefined :: binary(),
%%  cache_control = undefined :: binary(),
%%  upgrade_insecure_requests = undefine :: binary(),
%%  accept = undefined :: binary(),
%%  accept_encoding = undefined :: binary(),
%%  accept_language = undefined :: binary()
  method, host, path, user_agent, referer, connection,
  pragma, cache_control, upgrade_insecure_requests, qs,
  accept, accept_language, accept_encoding, get_params
}).

parse (Data) when is_binary(Data) ->
  [Header, Body] = binary:split(Data, <<"\r\n\r\n">>),
  %io:format("~p~n", [Header]),
  HeaderBits = binary:split(Header, <<"\r\n">>, [global]),
  Req = filter(HeaderBits),
  #{method := Method} = Req,
  case Method of
    <<"GET">> ->
      #{get_params := GetParams} = Req,
      QueryData = qs(GetParams);
    _ ->
      QueryData = qs(Body)
  end,
  Req2 = Req#{qs => QueryData},
  {ok, Req2}.
 



%%=====================================================================
%% query string or post data parsing
%%---------------------------------------------------------------------
qs (<<>>) -> <<>>;
qs (Body) ->
  List = binary:split(Body, <<"&">>, [global]),
  qs(List, []).
qs ([], QueryData) -> QueryData;
qs ([H | T], QueryData) ->
  [Key, Value] = binary:split(H, <<"=">>),
  qs(T, [{Key, Value} | QueryData]).


get_params (RawPath) ->
  Params = case binary:split(RawPath, <<"?">>) of
    [Path, GetParams] ->
      {Path, qs(GetParams)};
    [Path] ->
      {Path, <<>>}
  end,
  Params.

filter ([H | T]) ->
  [Method, RawPath, _] = binary:split(H, <<" ">>, [global]),
  case binary:split(RawPath, <<"?">>) of
    [Path, GetParams] -> ok;
    [Path] -> GetParams = <<>>
  end,
  Req = #{method => Method, path => Path, get_params => GetParams},
  filter(T, Req).

filter ([], Req) -> Req;

filter ([H | T], Req) ->
  [Name, Value] = binary:split(H, <<": ">>),
  case Name of
    <<"Host">> ->
      NewReq = Req#{host => Value};
    <<"User-Agent">> ->
      NewReq = Req#{user_agent => Value};
    <<"Accept">> ->
      NewReq = Req#{accept => Value};
    <<"Accept-Language">> ->
      NewReq = Req#{accept_language => Value};
    <<"Accept-Encoding">> ->
      NewReq = Req#{accept_encoding => Value};
    <<"Referer">> ->
      NewReq = Req#{referer => Value};
    <<"Connection">> ->
      NewReq = Req#{connection => Value};
    <<"Pragma">> ->
      NewReq = Req#{pragma => Value};
    <<"Cache-Control">> ->
      NewReq = Req#{cache_control => Value};
    <<"Upgrade-Insecure-Requests">> ->
      NewReq = Req#{upgrade_insecure_requests => Value};
    _ ->
      NewReq = Req
  end,
  filter(T, NewReq).
