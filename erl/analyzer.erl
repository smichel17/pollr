-module(analyzer).
-export([analyze_sentiment/1, analyze_hashtags/1]).

% Given a list of tweets, computes the average sentiment of all tweets
analyze_sentiment(Tweets) -> 
	Scores = concurrency:map(fun(T) -> analyze(T) end, Tweets),
	case lists:foldl(fun(X, Sum) -> X + Sum end, 0, Scores) of
		0 -> -1;
		SumOfScores -> SumOfScores / length(Scores)
	end.

% Given a list of tweets, computes a list of hashtags found in the tweets
analyze_hashtags(Tweets) -> 
	HashtagLists = concurrency:map(fun(T) -> hashtags(T) end, Tweets),
	{ok, P} = python:start([{python_path, "./python"},
                         	{python, "python3"}]),
	% Send back the tuple of happy probability and sad probability
	Hashtags = python:call(P, sentiment, mergeHashTagLists, [HashtagLists]),
	python:stop(P),
	Hashtags.

% Given a tweet, analyzes the sentiment of each word and computes the overall
% probability of the tweet to be pleasant / happy.
analyze(Tweet) ->
	{ok, P} = python:start([{python_path, "./python"},
                         	{python, "python3"}]),
	% Send back the tuple of happy probability and sad probability
	{Pos, _Neg} = python:call(P, sentiment, analyze, [Tweet]),
	python:stop(P),
	Pos.

% Given a tweet, analyzes the sentiment of each word and computes the overall
% probability of the tweet to be pleasant / happy.
hashtags(Tweet) ->
	{ok, P} = python:start([{python_path, "./python"},
                         	{python, "python3"}]),
	% Send back the tuple of happy probability and sad probability
	Hashtags = python:call(P, sentiment, hashtags, [Tweet]),
	python:stop(P),
	Hashtags.



