-module( cf_worker ).
-behaviour( application ).


%%====================================================================
%% Exports
%%====================================================================

-export( [start/2, stop/1] ).
-export( [start/0] ).
-export( [main/1] ).


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
  cf_worker_sup:start_link( CrePid, NSlot ).


stop( _State ) ->
  ok.


%%====================================================================
%% Escript main functions
%%====================================================================

main( [CreNode] )
when is_list( CreNode ) ->

  % connect to node
  pong = net_adm:ping( list_to_atom( CreNode ) ),

  % start worker application
  ok = start(),

  % wait indefinitely
  receive
    _ -> ok
  end.

