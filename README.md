# Russian Troll Tweets
[Source](https://www.nbcnews.com/tech/social-media/now-available-more-200-000-deleted-russian-troll-tweets-n844731) | "Twitter deleted 200,000 Russian troll tweets. Read them here." by Ben Popken at NBC News. 

## Data

Per NBC News-

> NBC News is publishing its database of more than 200,000 tweets that Twitter has tied to "malicious activity" from Russia-linked accounts during the 2016 U.S. presidential election.

> These accounts, working in concert as part of large networks, pushed hundreds of thousands of inflammatory tweets, from fictitious tales of Democrats practicing witchcraft to hardline posts from users masquerading as Black Lives Matter activists. Investigators have traced the accounts to a Kremlin-linked propaganda outfit founded in 2013 known as the Internet Research Agency (IRA). The organization has been assessed by the U.S. Intelligence Community to be part of a Russian state-run effort to influence the outcome of the 2016 U.S. presidential race. And they're not done.

### Data Model

I cleaned up the data and put together a data model. The data model is in an S3 bucket. You can download the tables:

[Tweets](https://s3.amazonaws.com/russian-troll-tweets/data-model/tweets.csv)
[Users](https://s3.amazonaws.com/russian-troll-tweets/data-model/users.csv)
[Mentions](https://s3.amazonaws.com/russian-troll-tweets/data-model/mention_bridge.csv)
[Hashtags](https://s3.amazonaws.com/russian-troll-tweets/data-model/hashtag_bridge.csv)
