import numpy as np
import twitter
import pandas as pd
import TweetMiner
from sklearn.feature_extraction.text import TfidfVectorizer
from collections import Counter
from textacy.preprocess import preprocess_text

twitter_keys = {
    'consumer_key':        'L4sziHBqV4VUIfKezbos0JMVl',
    'consumer_secret':     'lJau6R7GIHFwoGR5wB3PlLQPXBChwzJFJ9WGXXtazcDSA1Vb1X',
    'access_token_key':    '941359629606539264-05XcmQfdwMXTbPNWS3r7cZThvbQBxCK',
    'access_token_secret': 'VdE3VJVk6oxbohQGcw7WYA5Tg4Sr8kW9duTO1wxmB6qXk'
}

api = twitter.Api(
    consumer_key         =   twitter_keys['consumer_key'],
    consumer_secret      =   twitter_keys['consumer_secret'],
    access_token_key     =   twitter_keys['access_token_key'],
    access_token_secret  =   twitter_keys['access_token_secret'],
    tweet_mode = 'extended'
)

miner = TweetMiner(api, result_limit=200)

personA = miner.mine_user_tweets(user=input("twitter username for first person"), max_pages=14)
personB = miner.mine_user_tweets(user=input("twitter username for second person"), max_pages=14)

personA_df = pd.DataFrame(personA)
personB_df = pd.DataFrame(personB)
combined = pd.concat([personA_df, personB_df], axis=0)

vect = TfidfVectorizer(ngram_range=(2, 5), stop_words='english')

clean_tweets = [preprocess_text(x, fix_unicode=True, lowercase=True, no_urls=True, no_emails=True, no_phone_numbers=True,
                              no_currency_symbols=True, no_punct=True, no_accents=True)
                for x in "".join(combined['text'])]

ngrams = vect.build_analyzer()("".join(combined['text']))
Counter(ngrams).most_common(20)
