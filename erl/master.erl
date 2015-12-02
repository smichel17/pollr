-module(master).
-export([start/0, test/1]).

%% This is our MVP. Call master:test(Hashtag) and it returns the score.

test(Query) ->
    register(master, self()),
    scraper:scrape(Query),
    receive
        {score, _HashTag, Score} -> Score
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
    register(master, self()),
    HashList = [{null, null}],
    loop(HashList).


loop(HashList) ->
    receive
        {Query} -> scraper:scrape(Query);
        {result, Hashtag, Score} -> loop([{Hashtag, Score}|HashList]);
        {lookup, Hashtag, Pid} ->
            Score = score(Hashtag, HashList),
            case Score = not_found of
                true -> scraper:scrape(Hashtag);
                false -> whereis(interface) ! {Hashtag, Score}
            end
    end,
    loop(HashList).



score(Query, [{Hashtag, Score}|T]) ->
    if
        Hashtag == Query -> Score;
        Hashtag == null -> not_found;
        true -> score(Query, T)
    end.
    
