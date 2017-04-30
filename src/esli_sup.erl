-module(esli_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Dynamic children callbacks
-export([start_router/1, start_http/1, start_https/2]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link () ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init (_Args) ->
  {ok, {{one_for_one, 5, 10}, []}}.

start_router (Routes) ->
  Child = {esli_router,
    {esli_router, start_link, [Routes]},
    permanent,
    5000,
    worker,
    [esli_router]
  },
  supervisor:start_child(?MODULE, Child).

start_http (Port) ->
  Child = {esli_gate,
    {esli_gate, init, [Port, plain, []]},
    permanent,
    5000,
    worker,
    [esli_gate]
  },
  supervisor:start_child(?MODULE, Child).

start_https (Port, Certificates) ->
  Child = {esli_gate,
    {esli_gate, init, [Port, ssl, Certificates]},
    permanent,
    5000,
    worker,
    [esli_gate]
  },
  supervisor:start_child(?MODULE, Child).

