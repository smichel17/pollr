import numpy, sys, os, collections
import db

# Converts Erlang style strings into python style strings
def convert(sentence):
	return ''.join(chr(i) for i in sentence)

# Removes characters that will hinder a word from being potentially scored
def clean(tweet):
	bad_characters = '`~!@$%^&*()-_=+[{]}\|;:,<.>/?\n\t' + chr(8230) #...
	for char in bad_characters:
		tweet = tweet.replace(char, '')
	return tweet

# Naive Bayes Analysis
# Calculates the sentiment probabilities for each word in a sentence / tweet
def analyze(sentence):
	sentence = convert(sentence)
	sentence = clean(sentence)
	tokens = sentence.rstrip().split(" ")
	pos_log_prob = 0
	neg_log_prob = 0
	for token in tokens:
		if token.startswith("#"):
			result = db.get_hashtag_sentiment_log(token)
			if result != None:
				pos, neg = result
				pos_log_prob += pos
				neg_log_prob += neg
		else:
			result = db.get_word_sentiment(token)
			if result != None:
				pos, neg = result
				pos_log_prob += pos
				neg_log_prob += neg
	try:
		pos_prob = numpy.reciprocal(numpy.exp(neg_log_prob - pos_log_prob) + 1)
		neg_prob = 1 - pos_prob
	except:
		print("Calculation Failed... defaulting to 0.5")
		return float(0.5), float(0.5)
	else:
		return float(pos_prob), float(neg_prob)

# Returns a list of hashtags in a sentence
def hashtags(tweet):
	tweet = convert(tweet)
	tags = [clean(token) for token in tweet.split(' ') if token.startswith("#")]
	return tags