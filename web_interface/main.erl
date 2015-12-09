#!/usr/bin/env escript
%%! -smp enable -name ui -setcookie twerlang  -kernel inet_dist_listen_min 9000  inet_dist_listen_max 9005

main([Query]) ->
	register(ui, self()),
	case file:consult("config.erl") of {ok, [{server_node, Name}]} ->
		{master, Name} ! {lookup, Query, {ui, node()}}
	end,
	receive
		{_Hashtag, Score} -> io:format("~p~n", [Score])
	end.
