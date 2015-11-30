-module(analyzer).
-export([analyze/2]).

analyze(Tweet, Hashtag) ->
	{ok, P} = python:start([{python_path, "./python"},
                         	{python, "python3"}]),
	% Send back the tuple of happy probability and sad probability
	Hashtag ! python:call(P, sentiment, analyze, [Tweet]),
	python:stop(P).