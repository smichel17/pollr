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
        {result, _Query, Sentiment, _Hashtags} -> Sentiment
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Core Functions %%

start() ->
    HashList = [{null, null}],          %{Hashtag, Score}
    Requests = [{null, {null, null}}],  %{Hashtag, Requestor}
    register(priority_scheduler, spawn(fun() -> priority_scheduler() end)),
    register(background_scheduler, spawn(fun() -> background_scheduler() end)),
    register(master, spawn(fun() -> loop(HashList, Requests) end)),
    ok.

% Listen for and keep track of requests and results. Message as appropriate.
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

% Start crawling one hashtag per BFREQ seconds
background_scheduler() ->
    receive
        Hashtag -> spawn(scraper, scrape, [Hashtag])
    end,
    receive
    after ?BFREQ -> background_scheduler()
    end.

% Seed with initial value
priority_scheduler() -> priority_scheduler(?NUM_PRIORITY_THREADS).

% See comment on priority_scheduler(N).
% If you've already spawned N threads, enforce a hard rate limit.
priority_scheduler(0) -> 
    receive
    after ?PFREQ -> priority_scheduler(1)
    end;

% Schedule Priority (user-initiated) scrapes. These are also rate limited.
% However, you can basically "borrow" up to NUM_PRIORITY_THREADS worth of
% requests, so a user won't be stuck waiting for 10 seconds if they send a few
% requests in quick succession.
priority_scheduler(N) ->
    receive
        Hashtag -> scraper:scrape(Hashtag),
                   priority_scheduler(N-1)
    % Enforce rate limit
    after ?PFREQ -> priority_scheduler(min_of(N+1, ?NUM_PRIORITY_THREADS))
    end.

%% Helper Functions %%

% Put all hashtags in the list into the background queue.
crawl([]) -> ok;
crawl([H|T]) ->
    whereis(background_scheduler) ! H,
    crawl(T).

% Look up the score of a hashtag
score(Query, [{Hashtag, Score}|T]) ->
    if
        Hashtag == Query -> Score;
        Hashtag == null -> not_found;
        true -> score(Query, T)
    end.

% Look up if someone is waiting to hear the result of a hashtag
requested(Query, [{Hashtag, Requestor}|T]) ->
    if
        Hashtag == Query -> Requestor;
        Hashtag == null -> not_found;
        true -> score(Query, T)
    end.

% Remove a requestor from the list of requestors.
% Used when we got a result for that person and they've been notified already
remove(_Requestor, [{null, _}]) -> [{null, {null, null}}];
remove(Requestor, [{H, R}|T]) ->
    case Requestor == R of
        true -> remove(Requestor, T);
        false -> [{H, R}|remove(Requestor, T)]
    end.

% Return the lower of two ints.
min_of(A, B) ->
    case A > B of
        true -> B;
        false -> A
    end.
