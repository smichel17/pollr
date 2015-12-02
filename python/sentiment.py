import numpy, sys, os, collections

def readList(filename):
	f = open(filename, 'r')
	# Dictionary of words and happy probabilities
	happy_log_probs = {}
	# Dictionary of words and sad probabilities
	sad_log_probs = {}
	header = f.readline()

	for line in f:
		tokens = line.rstrip().split(",")
		# Set happy probability
		happy_log_probs[tokens[0]] = float(tokens[1])
		# Set sad probability
		sad_log_probs[tokens[0]] = float(tokens[2])
	return happy_log_probs, sad_log_probs

# Naive Bayes Analysis
def analyze_sentiment(sentence, happy, sad):
	useless_words = []
	tokens = sentence.rstrip().split(" ")
	happy_log_prob = numpy.sum([happy[token] for token in tokens if token in happy and token not in useless_words])
	sad_log_prob = numpy.sum([sad[token] for token in tokens if token in sad and token not in useless_words])
	happy_prob = numpy.reciprocal(numpy.exp(sad_log_prob - happy_log_prob) + 1)
	sad_prob = 1 - happy_prob
	return happy_prob, sad_prob

# Converts Erlang style strings into python style strings
def convert(sentence):
	return ''.join(chr(i) for i in sentence)

# Calculates the sentiment probabilities for each word in a sentence / tweet
def analyze(sentence):
	sentence = convert(sentence)
	tags = hashtags(sentence)
	sentence = clean(sentence)
	happy_log_probs, sad_log_probs = readList("./wordlists/top20000.txt")
	happy_prob, sad_prob = analyze_sentiment(sentence, happy_log_probs, sad_log_probs)
	return float(happy_prob), float(sad_prob)

# Removes characters that will hinder a word from being potentially scored
def clean(tweet):
	bad_characters = '`~!@$%^&*()-_=+[{]}\|;:,<.>/?'
	for char in bad_characters:
		tweet = tweet.replace(char, '')
	tweet = ' '.join([token for token in tweet.split(' ') if not token.startswith("#")])
	return tweet

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

def main(argv):
	if len(argv) < 2:
		print("usage: python sentiment.py [wordlist_filename]")
		os._exit(1)
	print("Enter a sentence: ")
	sample = input("")
	# First argument is filename of word list
	happy_log_probs, sad_log_probs = readList(argv[1])
	happy_prob, sad_prob = analyze_sentiment(sample, happy_log_probs, sad_log_probs)
	print("P(happy) = " + str(happy_prob))
	print("P(sad)   = " + str(sad_prob))
	return 0

if __name__ == '__main__':
	main(sys.argv)