-module(db).
-export([]).


%% These all work on the DB for hashtags to be crawled %%

next_hashtag() -> .% return the hashtag with the highest number of requests

request(Hashtag) -> . % If Hashtag is not in the DB, add it with counter = 1.
                      % If it is, increment the counter.

remove(Hashtag) -> . % 


%% These work on the DB with Hashtags and Scores %%

score_of(Hashtag) -> . % return the score associated with a hashtag

update(Hashtag, Score) -> . % If Hashtag in DB, update score. Else add to DB.
