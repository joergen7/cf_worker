-module( cf_worker ).
-behaviour( application ).


%%====================================================================
%% Exports
%%====================================================================

-export( [start/2, stop/1] ).
-export( [start/0] ).


%%====================================================================
%% API functions
%%====================================================================


start() ->
  application:start( cf_worker ).


%%====================================================================
%% Application callback functions
%%====================================================================

start( _StartType, _StartArgs ) ->

  % find out CRE process id
  {ok, CrePid} = cre:pid(),

  % find out number of cores in this computer
  NSlot = case erlang:system_info( logical_processors_available ) of
            unknown -> 1;
            N       -> N
          end,

  % start supervisor
  cf_worker_sup:start_link( cf_worker_sup, {CrePid, NSlot} ).


stop( _State ) ->
  ok.

