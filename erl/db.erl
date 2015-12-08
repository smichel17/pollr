-module(db).
-export([]).

%%These all work on the DB for hashtags to be crawled %%

% return the hashtag with the highest number of requests
next_hashtag() -> .

% If Hashtag is not in the DB, add it with counter = 1.
% If it is, increment the counter.
request(Hashtag) -> . 

remove(Hashtag) -> . % 


%% These work on the DB with Hashtags and Scores %%

% return the score associated with a hashtag
score_of(Hashtag) -> 
	{ok, P} = python:start([{python_path, "./python"},
                         	{python, "python3"}]),
	Score = python:call(P, db, get_hashtag_sentiment_erl, [Hashtag]),
	python:stop(P),
	Score. 

% If Hashtag in DB, update score. Else add to DB.
update(Hashtag, Score) -> . 
	{ok, P} = python:start([{python_path, "./python"},
                         	{python, "python3"}]),
	Score = python:call(P, db, set_hashtag_sentiment_erl, [Hashtag, Score]),
	python:stop(P),
	Score. 