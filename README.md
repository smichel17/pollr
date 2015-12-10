<img align="right" height="260" src="web_interface/static/pollr_head.png">
# Pollr -- Twitter Crawler/Analyzer #
##### Fall 2015 Concurrency Final Project #####
##### [Stephen Michel](https://github.com/smichel17), [Seth Kahn](https://github.com/kahns729), [Gabe Terrell](https://github.com/gabe-terrell) #####

# Overview
Pollr is a sentiment analysis application and web crawler that determines how the general public feels about a particular subject. To do so, pollr examine data related to tweets that are tagged with particular hashtags.

# Dependencies
Pollr uses both Python and Erlang, and has required dependencies in both languages. For Python, these dependencies can be acquired using Pip. You must have [Python 3](https://www.python.org/download/releases/3.0/) installed. We recommend using a [virtual environment](https://virtualenv.readthedocs.org/en/latest/) to hold Python dependencies, though it is not necessary.

In your virtual env (or outside of it, if you choose not to use one), run
`pip3 install -r requirements.txt`
to install the necessary dependencies.

For Erlang, the only dependency is [Erlport](http://erlport.org/). We have included Erlport in the repository, but sometimes installation is not as easy as just having the `erlport` directory. If you are experiencing issues running the Erlang modules, try downloading Erlport [directly from their website](http://erlport.org/downloads/) instead. Be sure that the `erlport` directory is at the path `erl/erlport` in order for Pollr to run.

# Use
Once you have installed Erlport and the necessary Python dependencies, open an Erlang shell with the command `erl -env ERL_LIBS erl/erlport -name server -setcookie twerlang -kernel inet_dist_listen_min 9000 inet_dist_listen_max 9005`. By running this, we are ensuring that we have access to erlport, that our node's name is `server@host.com`, and that our cookies agree with the cookies used in the frontend web interface. We also ensure that we agree on the range of ports we will be communicating over.

To use Pollr's web interface, cd `web_interface` and create a file called `config.erl`. In it, you must specify the node name of the Erlang node, in the format `{server_node, 'server@somehost'}.` This identifies the Erlang node that is running in the background, which is the node our web interface will be communicating with.

run `python3 server.py`. Then, in a web browser, navigate to __http://127.0.0.1:5000/__. You should now be able to make queries.
