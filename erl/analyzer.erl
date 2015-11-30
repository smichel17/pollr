-module(analyzer).
-export([analyze_tweets/1, analyze/1]).

analyze(Tweet) ->
	{ok, P} = python:start([{python_path, "./python"},
                         	{python, "python3"}]),
	% Send back the tuple of happy probability and sad probability
	Score = python:call(P, sentiment, analyze, [Tweet]),
	python:stop(P),
	Score.

analyze_tweets(Tweets) -> 
	Scores = concurrency:map(fun(T) -> analyze(T) end, Tweets),
	SumOfScores = lists:foldl(fun(X, Sum) -> X + Sum end, 0, Scores),
	SumOfScores / lists:flatlength(Scores).

