-module(master).
-export([start/0, test/1, loop/2]).

%% This is our MVP. Call master:test(Hashtag) and it returns the score.

test(Query) ->
    register(master, self()),
    scraper:scrape(Query),
    receive
        {score, _HashTag, Score} -> Score
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    %register(scheduler, spawn(scheduler)),
    HashList = [{null, null}],          %{Hashtag, Score}
    Requests = [{null, {null, null}}],  %{Hashtag, Requestor}
    register(master, spawn(master, loop, [HashList, Requests])),
    ok.


loop(HashList, Requests) ->
    receive
        {result, Hashtag, Score, _HashtagList} -> %TODO:delete that underscore
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
                true -> scraper:scrape(Hashtag),
                        loop(HashList, [{Hashtag, Requestor}|Requests]);
                false -> Requestor ! {Hashtag, Score},
                         loop(HashList, Requests)
            end
    end.

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













scheduler() -> 
    receive
        Query -> scraper:scrape(Query)
    end,
    receive
    after 10000 -> scheduler()
    end.
