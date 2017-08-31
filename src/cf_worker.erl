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

  io:format( "application:     cf_worker~nnode name:       ~p~n", [node()] ),

  % connect to CRE node
  pong = net_adm:ping( list_to_atom( CreNode ) ),
  io:format( "connected nodes: ~p~n", [nodes()] ),

  % start worker application
  ok = start(),
  io:format( "state:           ok~n" ),

  % wait indefinitely
  receive
    _ -> ok
  end.

