-module(esli_gate).
-export([init/3]).

init (Port, Type, Certificates) ->
  PlainOpts = [binary, {packet, http_bin}, {reuseaddr, true}, {active, once}],
  Opts = case Type of
    plain ->
      PlainOpts;
    ssl ->
      ssl:start(),
      lists:flatten(PlainOpts, Certificates)
  end,
  {ok, Listen} = esli_tcp:listen(Type, Port, Opts),
  spawn(esli_acceptor, init, [Listen, Type]),
  {ok, self()}.
