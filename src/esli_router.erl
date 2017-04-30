-module(esli_router).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Routes) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Routes, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Routes) ->
  {ok, Routes}.

handle_call({get, Req}, _From, Routes) ->
  Path = maps:get(path, Req),
  Method = maps:get(method, Req),
  case find_handler(Path, Method, Routes) of
    not_found ->
      {reply, not_found, Routes};
    {Handler, Args} ->
      {reply, {ok, Handler, Args}, Routes}
  end;
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

find_handler (Path, Method, [{any, Routes}]) ->
  find_handler(Path, Method, Routes);
find_handler(_Path, _Method, []) -> not_found;
find_handler (Path, Method, [{Pattern, Handler} | T]) ->
  find_handler(Path, Method, [{<<"_">>, Pattern, Handler} | T]);
find_handler (Path, Method, [{RouterMethod, Pattern, Handler} | T]) ->
  case re:run(Path, Pattern, [{capture, all, binary}]) of
    {match, Match} ->
      case is_valid_method(Method, RouterMethod) of
        true ->
          case length(Match) of
            1 -> Args = [];
            _ -> [_| Args] = Match
          end,
          {Handler, Args};
        _ -> find_handler(Path, Method, T)
      end;
    _ -> find_handler(Path, Method, T)
  end.


is_valid_method (RequestMethod, RouterMethod) ->
  case RouterMethod of
    <<"_">> -> true;
    _ -> RouterMethod =:= RequestMethod
  end.
