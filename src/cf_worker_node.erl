%% -*- erlang -*-
%%
%% A common runtime environment (CRE) for distributed workflow languages.
%%
%% Copyright 2015-2017 Jörgen Brandt
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @author Jörgen Brandt <joergen.brandt@onlinehome.de>
%% @version 0.1.0
%% @copyright 2015-2017 Jörgen Brandt
%%
%%
%%
%%
%%
%% @end
%% -------------------------------------------------------------------

-module( cf_worker_node ).
-behaviour( cre_worker ).


%%====================================================================
%% Exports
%%====================================================================

-export( [do_stagein/3, do_stageout/3, init/1, run/2, stagein_lst/1,
          stageout_lst/2] ).


%%====================================================================
%% CRE worker callback functions
%%====================================================================

-spec do_stagein( A :: _, F :: _, UsrInfo :: _ ) -> ok | {error, enoent}.

do_stagein( _A, _F, _UsrInfo ) -> ok.


-spec do_stageout( A :: _, F :: _, UsrInfo :: _ ) -> ok | {error, enoent}.

do_stageout( _A, _F, _UsrInfo ) -> ok.


-spec init( WrkArg :: _ ) -> UsrInfo :: _.

init( _WrkArg ) -> [].


-spec run( A :: _, UsrInfo :: _ ) -> {ok, R :: _} | {error, Reason :: _}.

run( A, _UsrInfo ) -> {ok, A}.


-spec stagein_lst( A :: _ ) -> [F :: _].

stagein_lst( _A ) -> [].


-spec stageout_lst( A :: _, R :: _ ) -> [F :: _].

stageout_lst( _A, _R ) -> [].