-module(db).
-export([spawn_db/0, next_hashtag/0, request/1, remove/1, score_of/1, update/2]).

%%These all work on the DB for hashtags to be crawled %%

spawn_db() ->
	{ok, P} = python:start([{python_path, "./python"},
                         	{python, "python3"}]),
	python:call(P, db, build_db_scratch, []),
	python:stop(P),
	ok. 

% return the hashtag with the highest number of requests
next_hashtag() -> 
	{ok, P} = python:start([{python_path, "./python"},
                         	{python, "python3"}]),
	Hashtag = python:call(P, db, most_requested_hashtag_erl, []),
	python:stop(P),
	if 
		Hashtag == undefined -> not_found;
		true -> Hashtag
	end.

% If Hashtag is not in the DB, add it with counter = 1.
% If it is, increment the counter.
request(Hashtag) -> 
	{ok, P} = python:start([{python_path, "./python"},
                         	{python, "python3"}]),
	python:call(P, db, update_requested_hashtag_erl, [Hashtag]),
	python:stop(P),
	ok.  

remove(Hashtag) -> 
	{ok, P} = python:start([{python_path, "./python"},
                         	{python, "python3"}]),
	python:call(P, db, delete_requested_hashtag_erl, [Hashtag]),
	python:stop(P),
	ok. 


%% These work on the DB with Hashtags and Scores %%

% return the score associated with a hashtag
score_of(Hashtag) -> 
	{ok, P} = python:start([{python_path, "./python"},
                         	{python, "python3"}]),
	Score = python:call(P, db, get_hashtag_sentiment_erl, [Hashtag]),
	python:stop(P),
	if 
		Score == undefined -> not_found;
		true -> Score
	end.

% If Hashtag in DB, update score. Else add to DB.
update(Hashtag, Score) -> 
	{ok, P} = python:start([{python_path, "./python"},
                         	{python, "python3"}]),
	python:call(P, db, set_hashtag_sentiment_erl, [Hashtag, Score]),
	python:stop(P),
	ok. 