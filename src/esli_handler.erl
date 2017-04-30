-module(esli_handler).

-callback handle(any(), map()) ->
  {'ok', any(), map()} | {'error', Reason :: string()}.

