-module(scraper).
-export([scrape/1]). %scrape/1s

scrape(Query) ->
	{ok, P} = python:start([{python_path, "./python"},
                         	{python, "python3"}]),
	% Send back the tuple of happy probability and sad probability
	whereis(master) ! python:call(P, scraper, search, [Query]),
	python:stop(P).
