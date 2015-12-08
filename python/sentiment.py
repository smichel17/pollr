import numpy, sys, os, collections
import db

# Converts Erlang style strings into python style strings
def convert(sentence):
	# print(sentence)
	return ''.join(chr(i) for i in sentence)

# Removes characters that will hinder a word from being potentially scored
def clean(tweet):
	bad_characters = '`~!@$%^&*()-_=+[{]}\|;:,<.>/?'
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
	pos_prob = numpy.reciprocal(numpy.exp(neg_log_prob - pos_log_prob) + 1)
	neg_prob = 1 - pos_prob
	return float(pos_prob), float(neg_prob)

# Returns a list of hashtags in a sentence
def hashtags(tweet):
	tweet = convert(tweet)
	tags = [token for token in tweet.split(' ') if token.startswith("#")]
	return tags

# Takes a list of hashtag lists and returns the set of hashtags sorted by frequency
def mergeHashTagLists(hashtagLists):
	# flatten the list
	all_hashtags = [convert(item) for sublist in hashtagLists for item in sublist]
	# Calculate number of occurrences of each item
	counts = collections.Counter(all_hashtags)
	# Sort by occurrence and reduce the list into a set
	return sorted(list(set(all_hashtags)), key=lambda x: -counts[x])

# def main(argv):
# 	if len(argv) < 2:
# 		print("usage: python sentiment.py [wordlist_filename]")
# 		os._exit(1)
# 	print("Enter a sentence: ")
# 	sample = input("")
# 	# First argument is filename of word list
# 	happy_log_probs, sad_log_probs = readList(argv[1])
# 	happy_prob, sad_prob = analyze_sentiment(sample, happy_log_probs, sad_log_probs)
# 	print("P(happy) = " + str(happy_prob))
# 	print("P(sad)   = " + str(sad_prob))
# 	return 0

# if __name__ == '__main__':
# 	main(sys.argv)