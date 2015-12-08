import sys, math
import sqlite3 as lite

# Will build both dbs in their initial state, deleting any prior data
def build_db_scratch():
    build_word_db()
    build_hashtag_db()

def readList(filename):
    words = []
    f = open(filename, 'r')
    header = f.readline()

    for line in f:
        tokens = line.rstrip().split(",")
        words.append((tokens[0], tokens[1], tokens[2]))
    return tuple(words)

def build_word_db():
    con = lite.connect('./db/sentiment.db')
    with con:
        cur = con.cursor()    
        cur.execute("DROP TABLE IF EXISTS Words")
        cur.execute("CREATE TABLE Words(Word TEXT, Pos INT, Neg INT)")
        words = readList("./wordlists/top20000.txt")
        cur.executemany("INSERT INTO Words VALUES(?, ?, ?)", words)

def get_word_sentiment(word):
    con = lite.connect('./db/sentiment.db')
    with con:
        cur = con.cursor()    
        cur.execute("SELECT Pos, Neg FROM Words WHERE Word=:Word", {"Word": word})        
        con.commit()
        return cur.fetchone()

def build_hashtag_db():
    con = lite.connect('./db/sentiment.db')
    with con:
        cur = con.cursor()    
        cur.execute("DROP TABLE IF EXISTS Hashtags")
        cur.execute("CREATE TABLE Hashtags(Hashtag TEXT, Pos INT, Neg INT)")
        cur.execute("INSERT INTO Hashtags VALUES('#DonaldTrump', 0.75, 0.25)")

def get_hashtag_sentiment(hashtag):
    con = lite.connect('./db/sentiment.db')
    with con:
        cur = con.cursor()    
        cur.execute("SELECT Pos FROM Hashtags WHERE Hashtag=:Hashtag", {"Hashtag": hashtag})        
        con.commit()
        result = cur.fetchone()
        return result[0] if result != None else None

def get_hashtag_sentiment_log(hashtag):
    con = lite.connect('./db/sentiment.db')
    with con:
        cur = con.cursor()    
        cur.execute("SELECT Pos, Neg FROM Hashtags WHERE Hashtag=:Hashtag", {"Hashtag": hashtag})        
        con.commit()
        result = cur.fetchone()
        return math.log(result[0]), math.log(result[1]) if result != None else None