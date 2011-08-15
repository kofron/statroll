% statroll.erl
% written by jared kofron <jared.kofron@gmail.com>
%
% a data structure that tabulates rolling statistics - i.e. the stats
% are calculated on the fly without ever storing a list.
-module(statroll).

% integers and floats are numbers.
-type num() :: integer() | float().

% the record, not meant for external consumption...
-record(statroll,{nupdates = 0 :: integer(),
		  mean = 0.0   :: float(),
		  min  = 0     :: num(),
		  max  = 0     :: num()}).

% data structure constructors
-export([new/0,new/1]).

% update function
-export([update/2]).

% getters
-export([get_mean/1, get_min/1, get_max/1]).

%%%%%%%%%%%
%%% api %%%
%%%%%%%%%%%

% the "constructors".  either takes no argument or a seed list over which
% our stats are calculated.
-spec new() -> #statroll{}.
new() ->		 
    new([]).

-spec new([num()]) -> #statroll{}.
new([]) ->
    #statroll{};
new(SeedList) ->		 
    #statroll{mean = mean(SeedList),
	      max  = lists:max(SeedList),
	      min  = lists:min(SeedList)}.

% updates a statroll with a new value.  all statistics are
% recalculated on the fly.
-spec update(num(),#statroll{}) -> #statroll{}.
update(NewVal,#statroll{nupdates = 0}=S) ->
    S#statroll{mean = NewVal,
	       nupdates = 1,
	       max = NewVal,
	       min = NewVal};
update(NewVal,#statroll{nupdates = N,
			mean     = Mu,
		        max      = Sup}=S) when NewVal > Sup ->
    NewMean = rolling_mean(N,Mu,NewVal),
    S#statroll{mean = NewMean,
	       nupdates = N+1,
	       max = NewVal};
update(NewVal,#statroll{nupdates = N,
			mean     = Mu,
		        min      = Inf}=S) when NewVal < Inf ->
    NewMean = rolling_mean(N,Mu,NewVal),
    S#statroll{mean = NewMean,
	       nupdates = N+1,
	       min = NewVal};
update(NewVal,#statroll{nupdates = N,
			mean     = Mu}=S) ->
    NewMean = rolling_mean(N,Mu,NewVal),
    S#statroll{mean = NewMean,
	       nupdates = N+1}.

% gets the mean from a statroll
-spec get_mean(#statroll{}) -> float().
get_mean(#statroll{mean=Mu}) ->
    Mu.

-spec get_min(#statroll{}) -> float().
get_min(#statroll{min=Inf}) ->
    Inf.

-spec get_max(#statroll{}) -> float().
get_max(#statroll{max=Sup}) ->
    Sup.
		      
%%%%%%%%%%%%%%%%
%%% internal %%%
%%%%%%%%%%%%%%%%

% calculates the mean of a list.
-spec mean([float() | integer()]) -> float().
mean(L) when is_list(L) ->
    lists:sum(L) / erlang:length(L).

% calculates the new rolling mean given an existing mean
% and the number of elements which contributed to that mean.
% works b.c...
% x_bar(N) = 1/N*sum_i=0^N x_i
% => x_bar(N+1) = 1/(N+1)*(sum_i=0^(N+1) x_i)
%               = 1/(N+1)*(sum_i=0^(N) x_i + x_(N+1))
%               = 1/(N+1)*(N*x_bar(N) + x_(N+1))
% no list traversal!
-spec rolling_mean(float(), float(), num()) -> float().
rolling_mean(OldNum, OldMean, NewValue) ->
    (1/(OldNum + 1)) * (OldNum*OldMean + NewValue).			  

%%%%%%%%%%%%%
%%% eunit %%%
%%%%%%%%%%%%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% try it
construct_test() ->
    L = lists:seq(1,1000),
    S = ?MODULE:new(L),
    % the mean had better be equal to 500.5
    ?assert(abs(?MODULE:get_mean(S) - 500.5) < 1.0e-5),
    % obviously min and max should be right too
    ?assertEqual(?MODULE:get_min(S),1),
    ?assertEqual(?MODULE:get_max(S),1000).

update_min_test() ->
    L = lists:seq(1,1000),
    MinVal = 0.5,
    S = ?MODULE:new(L),
    S1 = ?MODULE:update(MinVal,S),
    ?assertEqual(?MODULE:get_min(S1),MinVal).

update_max_test() ->
    L = lists:seq(1,1000),
    MaxVal = 1000.5,
    S = ?MODULE:new(L),
    S1 = ?MODULE:update(MaxVal,S),
    ?assertEqual(?MODULE:get_max(S1),MaxVal).

rolling_mean_test() ->
    L = lists:seq(1,1000),
    S = lists:foldl(fun(X,S) -> ?MODULE:update(X,S) end, 
		    ?MODULE:new(),
		    L),
    ?assert(abs(?MODULE:get_mean(S) - 500.5) < 1.0e-5).

rolling_min_test() ->
    L = lists:seq(1,1000),
    S = lists:foldl(fun(X,S) -> ?MODULE:update(X,S) end, 
		    ?MODULE:new(),
		    L),
    ?assertEqual(?MODULE:get_min(S), 1).

rolling_max_test() ->
    L = lists:seq(1,1000),
    S = lists:foldl(fun(X,S) -> ?MODULE:update(X,S) end, 
		    ?MODULE:new(),
		    L),
    ?assertEqual(?MODULE:get_max(S), 1000).

-endif.
