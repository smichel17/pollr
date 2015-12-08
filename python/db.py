import sys
import sqlite3 as lite

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

build_word_db()