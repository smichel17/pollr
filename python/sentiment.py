import numpy, sys, os

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

def analyze(sentence):
	sentence = ''.join(chr(i) for i in sentence)
	happy_log_probs, sad_log_probs = readList("./wordlists/top20000.txt")
	happy_prob, sad_prob = analyze_sentiment(sentence, happy_log_probs, sad_log_probs)
	# return (happy_prob, sad_prob)
	# return "P(happy) = " + str(happy_prob) + "\nP(sad)   = " + str(sad_prob)
	return float(happy_prob), float(sad_prob)

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