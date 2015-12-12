<img align="right" height="260" src="web_interface/static/pollr_head.png">
# Pollr -- Twitter Crawler/Analyzer #
##### Fall 2015 Concurrency Final Project #####
##### [Stephen Michel](https://github.com/smichel17), [Seth Kahn](https://github.com/kahns729), [Gabe Terrell](https://github.com/gabe-terrell) #####

# Overview
Pollr is a sentiment analysis application and web crawler that determines how the general public feels about a particular subject. To do so, pollr examine data related to tweets that are tagged with particular hashtags.

# Dependencies
Pollr uses both Python and Erlang, and has required dependencies in both languages. For Python, these dependencies can be acquired using __Pip__. You must have [Python 3](https://www.python.org/download/releases/3.0/) installed. We recommend using a [virtual environment](https://virtualenv.readthedocs.org/en/latest/) to hold Python dependencies, though it is not necessary.

In your virtual env (or outside of it, if you choose not to use one), run
`pip3 install -r requirements.txt`
to install the necessary dependencies.

For Erlang, the only dependency is [Erlport](http://erlport.org/). We have included Erlport in the repository, but sometimes installation is not as easy as just having the `erlport` directory. If you are experiencing issues running the Erlang modules, try downloading Erlport [directly from their website](http://erlport.org/downloads/) instead. Be sure that the `erlport` directory is at the path `erl/erlport` in order for Pollr to run.

# Use
Once you have installed Erlport and the necessary Python dependencies, open an Erlang shell with the command `erl -env ERL_LIBS erl/erlport -name server -setcookie twerlang -kernel inet_dist_listen_min 9000 inet_dist_listen_max 9005`. By running this, we are ensuring that we have access to erlport, that our node's name is `server@host.com`, and that our cookies agree with the cookies used in the frontend web interface. We also ensure that we agree on the range of ports we will be communicating over.

To use Pollr's web interface, cd `web_interface` and create a file called `config.erl`. In it, you must specify the node name of the Erlang node, in the format `{server_node, 'server@somehost'}.` This identifies the Erlang node that is running in the background, which is the node our web interface will be communicating with.

run `python3 server.py`. Then, in a web browser, navigate to __http://127.0.0.1:5000/__. You should now be able to make queries.

# Overview of Files
### In the `python` directory:
* `config.py` -- Contains API keys and access tokens for using the Twitter API. This file should be created by each user, and is not included in the Github repo (but is included for the submission of this assignment).
		
* `db.py` -- Contains utility functions for interacting with a database that includes hashtags and their scores, Bayes’ classifiers, and a queue of hashtags waiting to be crawled.

* `scraper.py` -- Contains functions for retrieving tweets corresponding to a hashtag from Twitter.

* `sentiment.py` -- Contains functions for analyzing a tweet’s sentiment. Interacts with the wordlist in the database (Bayes’ classifiers), and extracts hashtag data from tweets, along with their sentiment values.

### In the `erl` directory:
* `analyzer.erl` -- Erlang module that exports an analyze function, which is spawned when a tweet requires analysis. This function returns a score for the tweet, along with the hashtags that were encountered inside of the tweet.

* `concurrency.erl` -- Erlang module containing just a function that performs a concurrent map operation over a list, where each function application happens in its own process.
		
* `db.erl` -- Erlang module that exports a number of wrapper functions to call the Python functions that are responsible for database interactions.

* `master.erl` -- Erlang module that exports a start() function, which spawns and registers persistent processes and contains many helper functions. Controls most application logic, including scheduling to conform to Twitter’s rate limit. Spawns scrapers and makes database calls.

* `scraper.erl` -- Erlang module that scrapes Twitter, using the scraper Python module, for tweets to pass onto analyzers. When analyzers return their results, this module averages the positivity ratings and also the hashtags encountered during the scrape and analysis.

### In the `web_interface` directory:
* `config.erl` -- Erlang file containing just one configuration variable which specifies the node name of the Erlang process that is running master. This could be local or remote, but should specify the full node name (retrieved by calling node() in the Erlang shell running master).
		
* `main.erl` -- Erlang script that simply sends a message to request a hashtag’s score (the hashtag is specified in the command line arguments) from the remote (or potentially local) master node. Pulls the node name of the master Erlang node from the config.erl file.

* `query.py` -- Python module that simply serves as a Python wrapper to call main.erl and retrieve the results for a given hashtag.
	
* `server.py` -- Very simple Flask web application that handles serving frontend content, as well as performing a backend lookup for hashtag scores.

* `templates` directory -- Folder containing html templates to be served by the Flask server.

* `static` directory -- Folder containing static files, specifically the Pollr logo and some style sheets to style the frontend interface.

###In the `wordlists` directory:
The wordlists directory contains a number of text files representing lists of words and their positivity ratings. Since the word list is now contained in a database, we only use these lists to populate new databases, and we have a few word lists to choose from (default is `top20000.txt`).


