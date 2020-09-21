# How do Twitter users feel about Python and R?
**Catarina Silva**

**September 20, 2020**

<p>&nbsp;</p>

Python and R are popular programming languages for statistics. There is a vast amount of discussion around what's the difference between them, which language is the best for data science and how to choose which one to learn first.

I wanted to have an overview of the Twitter community opinion behind each language. So I used *"python AND language"* and *"rstats"* for a Twitter search query for Python and R, respectively.

An interesting point I can conclude from this little exercise is that people think that both R and Python are easy (although the word "difficult" was twitted twice as much as "easy" for Python tweets and it didn't pop up in the top words for R!).

**Top sentiments for Python**

![Sentiment Python](/figures/sentiment_python.png)

**Top sentiments for R**

![Sentiment R](/figures/sentiment_r.png)

| **Word cloud of top Tweets for Python** | **Word cloud of top Tweets for R** |
|-----------------------------------------|-----------------------------------|
|![Word Could Python](/figures/wordcloud_python.png) | ![Word Could R](/figures/wordcloud_r.png) |




<p>&nbsp;</p>

Here is how I conducted the analysis:

<p>&nbsp;</p>

**Load the packages**

```{r load packages, cache=FALSE, results='hide', message=FALSE, warning=FALSE}

library("rmarkdown")
library("rtweet")
library("dplyr")
library("tidyr")
library("tidytext")
library("textdata")
library("ggplot2")
library("wordcloud2") 
library("webshot")
library("htmlwidgets")

```

<p>&nbsp;</p>

**Load data and process each set of tweets into tidy text**

```{r  load data, results='hide'}

python_2020_09_19_clean = read.csv("./python_2020_09_19_clean.csv", header = T)
r_2020_09_20_clean = read.csv("./r_2020_09_20_clean.csv", header = T)

tweets_python_2020_09_19_clean = python_2020_09_19_clean %>% select(screen_name, text)
tweets_r_2020_09_20_clean = r_2020_09_20_clean %>% select(screen_name, text)

```

<p>&nbsp;</p>

**Use pre-processing text transformations to clean up the tweets**


1. Remove http elements manually

```{r  pre-processing 1, results='hide'}

tweets_python_2020_09_19_clean$stripped_text1 = gsub("http\\S+","",tweets_python_2020_09_19_clean$text)

tweets_r_2020_09_20_clean$stripped_text1 = gsub("http\\S+","",tweets_r_2020_09_20_clean$text)

```



2. Remove punctuation and add id to each tweet (note: unnest_tokens() converts to lower case)

```{r  pre-processing 2, results='hide'}

tweets_python_2020_09_19_clean_stem = tweets_python_2020_09_19_clean %>% 
  select(stripped_text1) %>%
  unnest_tokens(word, stripped_text1)

tweets_r_2020_09_20_clean_stem = tweets_r_2020_09_20_clean %>% 
  select(stripped_text1) %>%
  unnest_tokens(word, stripped_text1)

```



3. Remove stop words from your list of words (e.g. is, on...)

```{r  pre-processing 3, results='hide', message=FALSE,}

cleaned_tweets_python_2020_09_19_stem = tweets_python_2020_09_19_clean_stem %>%
  anti_join(stop_words)

cleaned_tweets_r_2020_09_20_stem = tweets_r_2020_09_20_clean_stem %>%
  anti_join(stop_words)

```


<p>&nbsp;</p>

**Have a look at the top 40 words**

```{r word cloud, message=FALSE}

top_words_tweets_python_2020_09_19 = cleaned_tweets_python_2020_09_19_stem %>% 
  count(word, sort=TRUE) %>%
  top_n(40) %>%
  mutate(word = reorder(word,n))

cloud_phyton = wordcloud2(top_words_tweets_python_2020_09_19, size = 2, color = "grey", shape = "circle")
cloud_phyton

top_words_tweets_r_2020_09_20 = cleaned_tweets_r_2020_09_20_stem %>% 
  count(word, sort=TRUE) %>%
  top_n(40) %>%
  mutate(word = reorder(word,n))

cloud_r = wordcloud2(top_words_tweets_r_2020_09_20, size = 1, color = "grey", shape = "circle")
cloud_r

```




<p>&nbsp;</p>

**Get sentiment lexicons**

```{r run sentiment analysis, results='hide', message=FALSE, warning=FALSE}

get_sentiments(lexicon = c("bing", "afinn", "loughran", "nrc")) %>% filter(sentiment=="positive")
get_sentiments(lexicon = c("bing", "afinn", "loughran", "nrc")) %>% filter(sentiment=="negative")

bing_python = cleaned_tweets_python_2020_09_19_stem %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()

bing_r = cleaned_tweets_r_2020_09_20_stem %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()

```

<p>&nbsp;</p>

**Plot sentiment analysis for top 5 words**

```{r plot sentiment analysis, message=FALSE}

bing_python %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title="Python tweets",
       y="",
       x=NULL) +
  coord_flip() + theme_bw()

bing_r %>%
  group_by(sentiment) %>%
  filter(word != "plot") %>% 
  filter(word != "cloud") %>% 
  filter(word != "shiny") %>% 
  top_n(5) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title="R tweets",
       y="",
       x=NULL) +
  coord_flip() + theme_bw()


```


