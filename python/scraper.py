import tweepy, config

def search (query):
	query = ''.join(chr(i) for i in query)
	auth = tweepy.OAuthHandler(config.consumer_key, config.consumer_secret)
	auth.set_access_token(config.access_token, config.access_token_secret)

	api = tweepy.API(auth)

	return [tweet.text for tweet in api.search(query)]