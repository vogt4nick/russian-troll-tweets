# Russian Troll Tweets
[Source](https://www.nbcnews.com/tech/social-media/now-available-more-200-000-deleted-russian-troll-tweets-n844731) | "Twitter deleted 200,000 Russian troll tweets. Read them here." by Ben Popken at NBC News. 

## Data

Per NBC News-

> NBC News is publishing its database of more than 200,000 tweets that Twitter has tied to "malicious activity" from Russia-linked accounts during the 2016 U.S. presidential election.

> These accounts, working in concert as part of large networks, pushed hundreds of thousands of inflammatory tweets, from fictitious tales of Democrats practicing witchcraft to hardline posts from users masquerading as Black Lives Matter activists. Investigators have traced the accounts to a Kremlin-linked propaganda outfit founded in 2013 known as the Internet Research Agency (IRA). The organization has been assessed by the U.S. Intelligence Community to be part of a Russian state-run effort to influence the outcome of the 2016 U.S. presidential race. And they're not done.

### Data Model

I cleaned up the data and put together a data model. The data model is in an S3 bucket. You can download the tables:

- [tweets.csv](http://russian-troll-tweets.s3.amazonaws.com/data-model/tweets.csv)  
- [users.csv](http://russian-troll-tweets.s3.amazonaws.com/data-model/users.csv)  
- [mention-bridge.csv](http://russian-troll-tweets.s3.amazonaws.com/data-model/mention-bridge.csv)
- [hashtag-bridge.csv](http://russian-troll-tweets.s3.amazonaws.com/data-model/hashtag-bridge.csv)


## How can I recreate the figures in the project paper?

1. Download and extract the repo on your local machine. 

1. Download the preprocessed data from my S3 bucket and overwrite the files in the `data-model/` directory.  

1. Open `R/gen-figures-and-model.R` and change the `ROOT` variable (on line 20) to the extracted repo's filepath. 

1. Run the script (`ctrl + shift + enter` if using RStudio). 

Note that there is a large, commented out section where we calculate model perplexity for some values of `k`. This section takes 2 hours or more to run, so leave it commented unless you're really curious. 
