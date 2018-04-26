#' Script generating models and figures.
#'
#' Nicholas Vogt
#' Lexical Analysis of Interfering Agents
#' Advisor Luis E. Ortiz
#' CIS 579: Artificial Intelligence, Winter 2018 Term
#' Univesrity of Michigan Dearborn


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(igraph)
library(ggraph)
library(tidytext)
library(topicmodels)

# Define globals and functions --------------------------------------------

ROOT <- 'C:/Users/NVOGT3/GitHub/russian-troll-tweets-master/'

#' Keep so many clusters
#' 
#' @param graph an igraph object
#' @param communities a communities object
#' @param max_clusters the max number of clusters to keep
keep_clusters <- function(graph, communities, max_clusters) {
  vertices <- as_data_frame(graph, "vertices")
  
  graph <- set_vertex_attr(
    graph,
    'group', 
    V(graph), 
    as.factor(membership(communities)[as.character(vertices$name)])
  )
  
  if (max_clusters > length(communities)) {
    max_clusters <- length(communities)
  }
  top_groups <- names(sort(sizes(communities), decreasing = TRUE)[1:max_clusters])
  
  bad_vertices <- V(graph)[!(vertex_attr(graph, 'group', V(graph)) %in% top_groups)]
  graph <- delete_vertices(graph, bad_vertices)  
  
  return(graph)
}

#' Create a named vector mapping user keys to their cluster's label
#' 
#' @param communities an igraph::communities object
keys_to_clusters <- function(communities) {
  comms <- as.list(communities)
  vec <- character(0L)
  
  for (group in names(comms)) {
    for (k in comms[[group]]) {
      v <- group
      names(v) <- k
      vec <- c(vec, v)
    }
  }
  return(sort(vec))
}

stem_words <- function(charvec) {
  stem_list <- hunspell::hunspell_stem(charvec)
  sapply(stem_list, function(x) if (length(x) > 0) x[[1]] else NA)
}


# Load Data ---------------------------------------------------------------

tweets <- readr::read_csv(
  file = paste0(ROOT, "data-model/tweets.csv"),
  col_types = cols(
    tweet_k = col_integer(),
    user_k = col_integer(),
    mention_bridge_k = col_integer(),
    hashtag_bridge_k = col_integer(),
    timestamp_s = col_datetime("%Y-%m-%d %H:%M:%S"),
    tweet_x = col_character(),
    favorites_t = col_integer(),
    retweets_t = col_integer(),
    retweeted_f = col_logical(),
    retweeted_user_k = col_integer(),
    tweet_c = col_character()
  )
)

# dtdf <- readr::read_csv(
#   file = paste0(ROOT, "staging-data/document-terms.csv"),
#   col_types = cols(
#     tweet_k = col_integer(),
#     term_x = col_character()
#   )
# )

users <- readr::read_csv(
  file = paste0(ROOT, "data-model/users.csv"),
  col_types = cols(
    user_k = col_integer(),
    handle_n = col_character(),
    alias_n = col_character(),
    bio_x = col_character(),
    followers_t = col_integer(),
    statuses_t = col_integer(),
    favorites_t = col_integer(),
    friends_t = col_integer(),
    listed_t = col_integer(),
    russian_troll_f = col_logical(),
    verified_f = col_logical(),
    politician_f = col_logical(),
    media_f = col_logical(),
    location_x = col_character(),
    language_n = col_character(),
    timezone_n = col_character(),
    created_y = col_date("%Y-%m-%d"),
    created_s = col_datetime("%Y-%m-%d %H:%M:%S")
  )
)

# Represent Users and Mentions as Graph -----------------------------------

weighted_mentions <- readr::read_csv(
  file = paste0(ROOT, "data-model/mention-bridge.csv")
) %>% 
  inner_join(users, by = c('user_k' = 'user_k')) %>% 
  select(mention_bridge_k, user_k, handle_n) %>% 
  rename(mentioned_user_k = user_k) %>% 
  inner_join(tweets, by = 'mention_bridge_k') %>% 
  select(user_k, mentioned_user_k) %>% 
  group_by(user_k, mentioned_user_k) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) 

edges <-  weighted_mentions %>% 
  rename(from = user_k,
         to = mentioned_user_k)  %>% 
  filter(n >= 5)

vertices <- users %>% 
  select(user_k, handle_n) %>% 
  filter(user_k %in% c(edges$from, edges$to)) %>% 
  rename(name=user_k)

GRAPH <- igraph::graph_from_data_frame(edges, vertices = vertices, directed = F) %>% 
  simplify(remove.loops = FALSE) 



# Wakita and Tsurumi Algorithm ----------------------------------------------------------------

cfg <- igraph::cluster_fast_greedy(GRAPH)
subgraph <- keep_clusters(GRAPH, cfg, max_clusters = 4)

which_cluster <- keys_to_clusters(communities(cfg))
cfg_users <- users %>% 
  filter(as.character(user_k) %in% names(V(subgraph))) %>% 
  mutate(cluster = which_cluster[as.character(user_k)]) %>% 
  select(user_k, cluster)

# Draw W&T Results --------------------------------------------------------

plot_clusters <- function(G) {
  set.seed(1)  # Get the same picture every time.
  ggraph(G, layout="nicely") +
    geom_edge_link() +
    geom_node_point(aes(colour = factor(group)), size = 2.5, show.legend = FALSE) +
    theme_minimal() +
    scale_x_continuous(labels = NULL) +
    scale_y_continuous(labels = NULL) +
    labs(
      x = NULL,
      y = NULL,
      subtitle = paste(
        "Showing the largest 6 communities found by the algorithm.",
        "Pictured are connections between users who mentioned each other at least 5 times.",
        sep = "\n"),
      colour = NULL
    )  
}

igraph::cluster_fast_greedy(GRAPH) %>% 
  keep_clusters(GRAPH, ., max_clusters=6) %>% 
  plot_clusters() + 
  labs(
    title = "Communities of Interfering Agents, Wakita's and Tsurumi's Algorithm",
    caption = "Nicholas Vogt | CIS 579: Artificial Intelligence | University of Michigan - Dearborn"
  )

igraph::cluster_edge_betweenness(GRAPH) %>% 
  keep_clusters(GRAPH, ., max_clusters=6) %>% 
  plot_clusters() + 
  labs(
    title = "Communities of Interfering Agents, Girvan-Newman Algorithm",
    caption = "Nicholas Vogt | CIS 579: Artificial Intelligence | University of Michigan - Dearborn"
  )

igraph::cluster_leading_eigen(GRAPH) %>% 
  keep_clusters(GRAPH, ., max_clusters=6) %>% 
  plot_clusters() + 
  labs(
    title = "Communities of Interfering Agents, Newman's Leading Eigenvector Algorithm",
    caption = "Nicholas Vogt | CIS 579: Artificial Intelligence | University of Michigan - Dearborn"
  )


# Create Terms ------------------------------------------------------------

load(paste0(ROOT, 'data-model/tweet-langs.Rdata'))

terms <- tweets %>% 
  filter(is.na(retweeted_user_k)) %>% 
  select(tweet_k, tweet_x) %>% 
  inner_join(tweet_langs, by='tweet_k') %>% 
  filter(lang_n %in% c('english', 'scots')) %>% 
  select(-lang_n) %>% 
  unnest_tokens(word, tweet_x) %>% 
  mutate(word = str_replace_all(word, "[[:punct:]]", "")) %>% 
  filter(str_detect(word, "[[:alpha:]]")) %>% 
  filter(word != "amp") %>% 
  anti_join(stop_words, by = 'word') %>% 
  mutate(
    stem = stem_words(word),
    clean_word = ifelse(is.na(stem), word, stem)
  ) %>% 
  select(tweet_k, clean_word) %>% 
  rename(word = clean_word) %>% 
  filter(word != "ha")

terms <- terms %>% 
  count(word) %>% 
  filter(n >= 50) %>% 
  inner_join(terms, by = 'word') %>% 
  select(tweet_k, word)

rm('tweet_langs')

tweets <- tweets %>% 
  filter(tweet_k %in% unique(terms$tweet_k))


# How many topics? --------------------------------------------------------
# NOTE: This section will take at least two hours to run on an a standard laptop. 

# k_tweets <- list()
# k_dtms <- list()
# k_ldas <- list()
# perplexities <- list()
# 
# k <- c(2 ** (1:8))
# 
# set.seed(1)
# training_tweets <- sample(tweets$tweet_k, 0.8 * nrow(tweets))
# this_cluster <- 1
# 
# for (i in 1:length(k)) {
#   message(paste("Running LDA with", k[i], "topics..."))
# 
#   k_tweets[[i]] <- tweets %>% 
#     mutate(train_f = tweet_k %in% training_tweets) %>% 
#     filter(as.character(user_k) %in% names(which_cluster[which_cluster == this_cluster])) %>% 
#     select(tweet_k, user_k, tweet_x, train_f)
#   
#   k_dtms[[i]] <- terms %>% 
#     inner_join(k_tweets[[i]], by = "tweet_k") %>% 
#     filter(train_f == TRUE) %>% 
#     count(tweet_k, word) %>% 
#     cast_dtm(tweet_k, word, n)
#   
#   k_ldas[[i]] <- topicmodels::LDA(
#     k_dtms[[i]], 
#     k = k[i], 
#     control = list(seed = 1)
#   )
# 
#   perplexities[[i]] <-  terms %>% 
#     inner_join(k_tweets[[i]], by = "tweet_k") %>% 
#     filter(train_f == FALSE) %>% 
#     count(tweet_k, word) %>% 
#     cast_dtm(tweet_k, word, n) %>% 
#     perplexity(k_ldas[[i]], newdata = ., control = list(seed = 1))
# }
# 
# perplexities <- unlist(perplexities)
# perplexities
# 
# df <- data.frame(k = k, perplexity = perplexities)
# df %>% 
#   ggplot(aes(x=k, y=perplexity)) +
#   geom_line() +
#   geom_point() +
#   geom_text(aes(label=paste("k =", k)), nudge_x = 10, nudge_y = -2, size=3) +
#   labs(
#     title = "Perplexity of Latent Dirichlet Allocation by Number of Topics",
#     x = "Number of Topics (k)",
#     y = "Perplexity",
#     caption = "Nicholas Vogt | CIS 579: Artificial Intelligence | University of Michigan - Dearborn"
#   ) +
#   theme_minimal()
# stop()


# LDA Communities ---------------------------------------------------------

clustered_tweets <- list()
clustered_dtms <- list()
clustered_ldas <- list()
clustered_lda_topics <- list()

for (i in 1:length(communities(cfg))) {
  clustered_tweets[[i]] <- tweets %>% 
    filter(as.character(user_k) %in% names(which_cluster[which_cluster == i])) %>% 
    select(tweet_k, user_k, tweet_x)
  
  clustered_dtms[[i]] <- terms %>% 
    inner_join(clustered_tweets[[i]], by = "tweet_k") %>% 
    count(tweet_k, word) %>% 
    cast_dtm(tweet_k, word, n)
  
  clustered_ldas[[i]] <- topicmodels::LDA(
    clustered_dtms[[i]], 
    k = 6, 
    control = list(seed = 42)
  )
  
  clustered_lda_topics[[i]] <- tidy(clustered_ldas[[i]], matrix = "beta")
}

clustered_dtms 

# Plot --------------------------------------------------------------------

plot_topics <- function(model) {
  topics <- tidy(model, matrix = "beta")
  
  topics %>%
    group_by(topic) %>%
    top_n(4, -beta) %>%
    ungroup() %>%
    arrange(topic, -beta) %>%
    # filter(topic <= 4) %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    theme_minimal() +
    scale_y_continuous(labels = NULL) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )
}

ggs <- list()
for (i in 1:4) {
  ggs[[i]] <- plot_topics(clustered_ldas[[i]]) + 
    labs(
      title=paste0('Cluster ', i, ': Top Terms of LDA Topics'),
      x = NULL,
      y = NULL,
      caption = "Nicholas Vogt | CIS 579: Artificial Intelligence | University of Michigan - Dearborn"
    )
  
}
ggs[[1]]
ggs[[2]]
ggs[[3]]
ggs[[4]]

