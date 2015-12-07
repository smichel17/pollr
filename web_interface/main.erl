#!/usr/bin/env escript
%%! -smp enable -sname ui
% -import(master, [test/1]).
% -import(analyzer, [analyze_sentiment/1, analyze_hashtags/1]).
% -import(scraper, [scrape/1]).
% -import(concurrency, [map/2]).

main([Query]) ->
	register(ui, self()),
	{master, 'server@Seths-MacBook-Pro'} ! {lookup, Query, {ui, node()}},
	
	receive
		{_Hashtag, Score} -> io:format("~p~n", [Score])
	end.
