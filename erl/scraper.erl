-module(scraper).
-export([scrape/1]). %scrape/1s

scrape(Query) ->
	{ok, P} = python:start([{python_path, "./python"},
                         	{python, "python3"}]),
	% Generate a list of tweets matching the query
    Tweetlist = python:call(P, scraper, search, [Query]),
    % Send to master the average sentiment of the resulting tweets
    whereis(master) ! {result, Query, analyzer:analyze_tweets(Tweetlist)}, 
	python:stop(P).
