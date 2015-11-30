-module(scraper).
-export([scrape/2]). %scrape/1s



scrape(Query, From) ->
	{ok, P} = python:start([{python_path, "./python"},
                         	{python, "python3"}]),
	% Send back the tuple of happy probability and sad probability
	From ! python:call(P, scraper, search, [Query]),
	python:stop(P)
	.

