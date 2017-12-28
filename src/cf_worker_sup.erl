-module( cf_worker_sup ).
-behaviour( supervisor ).


%%====================================================================
%% Exports
%%====================================================================

-export( [init/1] ).
-export( [start_link/5] ).


%%====================================================================
%% API functions
%%====================================================================

start_link( CreNode, NWrk, WrkDir, RepoDir, DataDir ) ->
  supervisor:start_link( ?MODULE, {CreNode, NWrk, WrkDir, RepoDir, DataDir} ).

%%====================================================================
%% Supervisor callback functions
%%====================================================================

init( {CreNode, NWrk, WrkDir, RepoDir, DataDir} )
when is_atom( CreNode ),
     is_integer( NWrk ), NWrk > 0,
     is_list( WrkDir ),
     is_list( RepoDir ),
     is_list( DataDir ) ->

  F =
    fun() ->    
      cre:pid( CreNode )
    end,

  Id =
    fun( I ) ->
      S = lists:flatten( io_lib:format( "wrk-~p", [I] ) ),
      list_to_atom( S )
    end,

  % connect with CRE
  pong =
    case CreNode of
      'nonode@nohost' -> pong;
      _               -> net_adm:ping( CreNode )
    end,

  SupFlags = #{
               strategy  => one_for_one,
               intensity => 1,
               period    => 5
              },

  WorkerNodeSpec = #{
                     start    => {cf_worker_process,
                                  start_link,
                                  [F, WrkDir, RepoDir, DataDir]},
                     restart  => permanent,
                     shutdown => 5000,
                     type     => worker,
                     modules  => [cf_worker_process]
                    },

  SpecLst = [WorkerNodeSpec#{ id => Id( I ) } || I <- lists:seq( 1, NWrk )],

  {ok, {SupFlags, SpecLst}}.

