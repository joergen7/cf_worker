-module( cf_worker_sup ).
-behaviour( supervisor ).


%%====================================================================
%% Exports
%%====================================================================

-export( [init/1] ).
-export( [start_link/2] ).


%%====================================================================
%% API functions
%%====================================================================

start_link( CrePid, NSlot )
when is_pid( CrePid ),
     is_integer( NSlot ), NSlot > 0 ->
  supervisor:start_link( ?MODULE, {CrePid, NSlot} ).

%%====================================================================
%% Supervisor callback functions
%%====================================================================

init( {CrePid, NSlot} )
when is_pid( CrePid ),
     is_integer( NSlot ), NSlot > 0 ->

  Id = fun( I ) ->
  	     S = lists:flatten( io_lib:format( "wrk-~p", [I] ) ),
         list_to_atom( S )
       end,

  SupFlags = #{
               strategy  => one_for_one,
               intensity => 1,
               period    => 5
              },

  WorkerNodeSpec = #{
                     start    => {cf_worker_process, start_link, [CrePid]},
                     restart  => permanent,
                     shutdown => 5000,
                     type     => worker,
                     modules  => [cf_worker_node]
                    },

  SpecLst = [WorkerNodeSpec#{ id => Id( I )} || I <- lists:seq( 1, NSlot )],

  {ok, {SupFlags, SpecLst}}.

