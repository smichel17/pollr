-module(scraper).
-export([scrape/1]). %scrape/1s

scrape(Query) ->
	{ok, P} = python:start([{python_path, "./python"},
                         	{python, "python3"}]),
    Tweetlist = python:call(P, scraper, search, [Query]),
    Sentiment = analyzer:analyze_sentiment(Tweetlist),
    Hashtags  = analyzer:analyze_hashtags(Tweetlist),
    whereis(master) ! {result, Query, Sentiment, Hashtags}, 
	python:stop(P).
