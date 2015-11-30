-module(scraper).
-export([scrape/1]). %scrape/1s

scrape(Query) ->
	{ok, P} = python:start([{python_path, "./python"},
                         	{python, "python3"}]),
    Tweetlist = python:call(P, scraper, search, [Query]),
    % THIS COMMENT IS DEPRECATED:
	% Send back the tuple of happy probability and sad probability
    whereis(master) ! {score, Query, analyzer:analyze_tweets(Tweetlist)}, 
	python:stop(P).


analyze(Tweetlist) -> 0.5. %stub
