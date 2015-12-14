from flask import Flask, render_template, request
from query import query
app = Flask(__name__)

@app.route('/')
def hello_world():
    return render_template('index.html')

@app.route('/sentiment')
def sentiment():
    print(request.args)
    hashtag1 = request.args.get('hashtag1', None)
    hashtag1 = clean(hashtag1)
    hashtag2 = request.args.get('hashtag2', None)
    hashtag2 = clean(hashtag2)

    positivity1 = query(hashtag1)
    positivity2 = query(hashtag2)

    best_tag = None
    bad_tag = None

    if positivity1 > positivity2:
        best_tag = hashtag1
        bad_tag = hashtag2
    else:
        best_tag = hashtag2
        bad_tag = hashtag1

    return render_template('results.html', best_tag=best_tag, bad_tag=bad_tag)

def clean(hashtag):
    hashtag = hashtag.replace("%23", "#")
    hashtag = hashtag.replace(" ", "")
    if hashtag[0] != "#":
        hashtag = "#" + hashtag
    unsafe_chars = "<>"
    for char in unsafe_chars:
        hashtag = hashtag.replace(char, "")
    return hashtag

if __name__ == '__main__':
    app.run()