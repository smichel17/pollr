import tweepy, config

def search (query):
	query = ''.join(chr(i) for i in query)
	auth = tweepy.OAuthHandler(config.consumer_key, config.consumer_secret)
	auth.set_access_token(config.access_token, config.access_token_secret)

	api = tweepy.API(auth)
	results = api.search(query, lang="en", locale="en", count=30, result_type="recent")

	return [tweet.text for tweet in results]