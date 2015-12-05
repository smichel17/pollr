-module(master).
-export([start/0, test/1, loop/2]).
-define(BFREQ, 10000). % Background crawling API call frequency
-define(PFREQ, 10000). % Priority (user-generated) API call frequency
-define(NUM_PRIORITY_THREADS, 5).

%% This is our MVP. Call master:test(Hashtag) and it returns the score.

test(Query) ->
    register(master, self()),
    scraper:scrape(Query),
    receive
        {score, _HashTag, Score} -> Score
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Core Functions %%

start() ->
    HashList = [{null, null}],          %{Hashtag, Score}
    Requests = [{null, {null, null}}],  %{Hashtag, Requestor}
    register(priority_scheduler, spawn(priority_scheduler)),
    register(background_scheduler, spawn(background_scheduler)),
    register(master, spawn(master, loop, [HashList, Requests])),
    ok.


loop(HashList, Requests) ->
    receive
        {result, Hashtag, Score, HashtagList} -> 
            crawl(HashtagList),
            Requestor = requested(Hashtag, Requests),
            case Requestor == not_found of
                true -> loop([{Hashtag, Score}|HashList], Requests);
                false -> self() ! {lookup, Hashtag, Requestor},
                         loop([{Hashtag, Score}|HashList],
                                  remove(Requestor, Requests))
            end;
        {lookup, Hashtag, Requestor} ->
            Score = score(Hashtag, HashList),
            case Score == not_found of
                true -> whereis(priority_scheduler) ! Hashtag,
                        loop(HashList, [{Hashtag, Requestor}|Requests]);
                false -> Requestor ! {Hashtag, Score},
                         loop(HashList, Requests)
            end
    end.


background_scheduler() ->
    receive
        Hashtag -> scraper:scrape(Hashtag)
    end,
    receive
    after BFREQ -> background_scheduler()
    end.

priority_scheduler() -> priority_scheduler(NUM_PRIORITY_THREADS).

priority_scheduler(0) -> 
    receive
    after PFREQ -> priority_scheduler(1)
    end;

priority_scheduler(N) ->
    receive
        Hashtag -> scraper:scrape(Hashtag),
                   priority_scheduler(N-1)
    after PREQ -> priority_scheduler(min_of(N+1, NUM_PRIORITY_THREADS))
    end.

%% Helper Functions %%

crawl([]) -> ok;

crawl([H|T]) ->
    whereis(background_scheduler) ! H,
    crawl(T).

score(Query, [{Hashtag, Score}|T]) ->
    if
        Hashtag == Query -> Score;
        Hashtag == null -> not_found;
        true -> score(Query, T)
    end.

requested(Query, [{Hashtag, Requestor}|T]) ->
    if
        Hashtag == Query -> Requestor;
        Hashtag == null -> not_found;
        true -> score(Query, T)
    end.

remove(_Requestor, [{null, _}]) -> [{null, {null, null}}];
remove(Requestor, [{H, R}|T]) ->
    case Requestor == R of
        true -> remove(Requestor, T);
        false -> [{H, R}|remove(Requestor, T)]
    end.

min_of(A, B) ->
    case A > B of
        true -> B;
        false -> A
    end.
