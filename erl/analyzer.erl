-module(analyzer).
-export([analyze/2]).

analyze(Tweet, _Hashtag) ->
	{ok, P} = python:start([{python_path, "./python"},
                         	{python, "python3"}]),
	python:call(P, sentiment, analyze, [Tweet]).
	