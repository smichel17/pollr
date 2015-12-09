# Pollr -- Twitter Sentiment Analysis #
##### Fall 2015 Concurrency Final Project #####
##### Stephen Michel, Seth Kahn, Gabe Terrell #####

# Overview
Pollr is a sentiment analysis application and web crawler that determines how the general public feels about a particular subject. To do so, pollr examine data related to tweets that are tagged with particular hashtags.

# Use
To use pollr's web interface, you must have the appropriate Python dependencies installed. We recommend using a [virtual environment](https://virtualenv.readthedocs.org/en/latest/) to hold these dependencies, though it is not necessary. Additionally, you must have [Python 3](https://www.python.org/download/releases/3.0/) installed.

In your virtual env, run
`pip3 install -r requirements.txt`
to install the necessary dependencies.

`cd web_interface` and create a file called `config.erl`. In it, you must specify the node name of the Erlang node, in the format `{server_node, 'server@somehost'}`.

run `python3 server.py`. Then, in a web browser, navigate to __http://127.0.0.1:5000/__. You should now be able to make queries.