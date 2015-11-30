-module(master).
-export([start/0]).


start() ->
    register(master, self()),
    HashList = [{null, null}],
    loop(HashList).


loop(HashList) ->
    receive
        {Hashtag} -> scraper:scrape(Hashtag);
        {score, Hashtag, Score} -> loop([{Hashtag, Score}|HashList];
        {whatis, Hashtag} ->
            Score = score(Hashtag, Hashlist),
            case Score = null of
                true -> scraper:scrape(Hashtag);
                false -> whereis(interface) ! {Hashtag, Score}
            end
    end,
    loop(HashList).



score(Query, [{Hashtag, Score}|T]) ->
    if
        Hashtag = Query -> Score;
        Hashtag = null -> not_found;
        true -> score(Query, T)
    end.
    
