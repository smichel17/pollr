import indicoio, math, sys
indicoio.config.api_key = '668bd19d0ad525c638d0c1c7102d0e90'

def print_sentiments (words):
    for w in words:
        print_sentiment(w)

def print_sentiment (w):
    pos = indicoio.sentiment_hq(w)
    neg = 1 - pos 
    pos_e = math.log(pos)
    neg_e = math.log(neg)
    print(w + "," + str(pos_e) + "," + str(neg_e))

if __name__ == '__main__':
    line = sys.stdin.readline()
    while line != "":
        line = line.rstrip()
        print_sentiment(line)
        line = sys.stdin.readline()