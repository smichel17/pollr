-module(master).
-export([start/0, test/1]).
-define(FREQ, 10000). % Priority (user-generated) API call frequency
-define(RESERVED_API_CALLS, 2).

%% This is our MVP. Call master:test(Hashtag) and it returns the score.

test(Query) ->
    register(master, self()),
    scraper:scrape(Query),
    receive
        {result, _Query, Sentiment, _Hashtags} -> Sentiment
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Core Functions %%

start() ->
    register(scheduler, spawn(fun() -> scheduler() end)),
    register(background_scheduler, spawn(fun() -> background_scheduler() end)),
    register(master, spawn(fun() -> loop([]) end)),
    ok.

% Listen for and keep track of requests and results. Message as appropriate.
loop(Requests) ->
    io:format("Requests: ~p~n", [Requests]),
    receive
        {result, Hashtag, Score, HashtagList} -> 
            io:format("result: {Hashtag, Score}: {~p, ~p}~n", [Hashtag, Score]),
            spawn(fun() -> crawl(HashtagList) end),
            db:update(Hashtag, Score),
            loop(lookup(Hashtag, Requests));
        {lookup, Hashtag, Requestor} ->
            io:format("lookup: {Hashtag, Requestor}: {~p, ~p}~n", 
                      [Hashtag, Requestor]),
            Score = db:score_of(Hashtag),
            case Score of
                not_found -> 
                    whereis(scheduler) ! Hashtag,
                    loop([{Hashtag, Requestor}|Requests]);
                _ -> 
                    Requestor ! {Hashtag, Score},
                    loop(lookup(Hashtag, Requests))
            end
    end.

% Every FREQ, add a token. Making a request uses a token. If there are
% RESERVED_API_CALLS tokens already, spend the token on a background request
scheduler(N) ->
    case N of
        0 -> receive after ?FREQ -> scheduler(1) end;
        ?RESERVED_API_CALLS ->
            whereis(background_scheduler) ! next,
            scheduler(N-1);
        _ -> receive
                Hashtag ->
                    io:format("Foreground scrape: ~p~n", [Hashtag]),
                    analyze(Hashtag), scheduler(N-1)
             after ?FREQ -> scheduler(N+1)
             end
    end.

% Seed with initial value
scheduler() -> scheduler(0).

% crawl the next scheduled hashtag whenever prompted
background_scheduler() ->
    receive
        next -> 
            Hashtag = db:next_hashtag(),
            case Hashtag of
                not_found -> 
                    io:format("Background scrape: Nothing to scrape.~n"),
                    background_scheduler();
                _ -> 
                    io:format("Background scrape: ~p~n", [Hashtag]),
                    analyze(Hashtag),
                    db:remove(Hashtag),
                    background_scheduler()
            end
    end.

%% Helper Functions %%

% Put all hashtags in the list into the background queue.
crawl([]) -> ok;
crawl([H|T]) ->
    db:request(H),
    crawl(T).

% Look up if someone is waiting to hear the result of a hashtag
% and if so, send a query for them
lookup(_Query, []) -> [];
lookup(Query, [{Hashtag, Requestor}|T]) ->
    case Query of
        Hashtag -> 
            whereis(master) ! {lookup, Hashtag, Requestor},
            T;
        _ ->
            [{Hashtag, Requestor}|lookup(Query, T)]
    end.

analyze(Hashtag) -> spawn(scraper, scrape, [Hashtag]).

%% Old, unused code %%

% Remove a requestor from the list of requestors.
% Used when we got a result for that person and they've been notified already
%remove(_Requestor, [{null, _}]) -> [{null, {null, null}}];
%remove(Requestor, [{H, R}|T]) ->
%    case Requestor == R of
%        true -> remove(Requestor, T);
%        false -> [{H, R}|remove(Requestor, T)]
%    end.

% Return the lower of two ints.
%min_of(A, B) ->
%    case A > B of
%        true -> B;
%        false -> A
%    end.

% Look up the score of a hashtag
%score(Query, [{Hashtag, Score}|T]) ->
%    if
%        Hashtag == Query -> Score;
%        Hashtag == null -> not_found;
%        true -> score(Query, T)
%    end.

