### Quantitative Text Analysis ###

# install packages --------------------------------------------------------

install.packages("quanteda")
install.packages("quanteda.textmodels")
install.packages("quanteda.textstats")
install.packages("quanteda.textplots")
install.packages("keyATM")

# library -----------------------------------------------------------------

library(quanteda)
library(quanteda.textmodels)
library(quanteda.textstats)
library(quanteda.textplots)
library(keyATM)
library(tidyverse)

# seed
set.seed(21732)

# load data ---------------------------------------------------------------

load("data/small_corpus.RData")

# quanteda basics ---------------------------------------------------------

## recommended resource:
## https://tutorials.quanteda.io

# create a corpus object
crp <- corpus(small_corpus,
              docid_field = "para_id",
              text_field = "text")

# corpus summary
summary(crp)

# segment a corpus into tokens
toks <- tokens(crp,
               what = "word",
               remove_numbers = TRUE,
               remove_punct = TRUE,
               remove_symbols = TRUE,
               remove_separators = TRUE,
               remove_url = TRUE)

# look at keywords in context
migrants <- kwic(toks,
                  pattern = "*migrant*",
                  window = 3)
head(migrants, 10)

# remove stopwords
print(stopwords("en"))
toks_nostop <- tokens_select(toks, pattern = stopwords("en"), selection = "remove")

# document-term matrix
dfmat <- dfm(
  toks_nostop,
  tolower = TRUE,
  verbose = TRUE,
)
  
# trim the matrix
dfmat <- dfmat |> 
  dfm_keep(min_nchar = 2,
           max_nchar = 15) |> 
  dfm_trim(min_termfreq = 2,
           max_docfreq = 0.5,
           docfreq_type = "prop")

# dfm summary
textstat_frequency(dfmat)

# exploring with quanteda -------------------------------------------------

# readability measures
textstat_readability(crp[1:10], measure = "Fucks")

# group frequencies by author type
dfmat_type <- dfmat |> 
  dfm_group(groups = author_type)

# calculate similarity
dfmat_type |> 
  textstat_simil(method = "cosine")

# group frequencies by author
dfmat_authors <- dfmat |> 
  dfm_group(groups = author)

# calculate similarity
cos_authors <- dfmat_authors |> 
  textstat_simil(method = "cosine")

cos_authors_long <- cos_authors |> 
  as.data.frame() |>
  as_tibble() |> 
  arrange(-cosine) |> 
  rename(author1 = document1,
         author2 = document2)

# are AGs more similar to each other or does that not matter?
authors <- small_corpus |> 
  distinct(author, author_type)

cos_authors_same <- cos_authors_long |> 
  left_join(authors |> rename(author_t1 = author_type), by = c("author1" = "author")) |> 
  left_join(authors |> rename(author_t2 = author_type), by = c("author2" = "author")) |> 
  mutate(same_type = author_t1 == author_t2)

lm(
  data = cos_authors_same,
  formula = cosine ~ same_type,
) |> 
  summary()

# keyATM ------------------------------------------------------------------

# smaller sample of paragraphs
dfmat_small <- dfmat[sample(1:nrow(dfmat), 5000),]

# define keywords
keywords <- list(
  taxation = c("tax","vat","company","taxation","taxable","turnover","amount"),
  market = c("market","trade","competition","aid","goods","services","internal","capital"),
  procedure = c("court","preliminary","infringement","failure","annulment","jurisdiction",
                "action","judicial","judgment","law","legal")
)

# prepare documents
key_docs <- keyATM_read(dfmat_small, progress_bar = TRUE)

# run model
key_model <- keyATM(
  docs = key_docs,
  keywords = keywords,
  no_keyword_topics = 3,
  model = "base",
  options = list(seed = 25082)
)

# analyse results:

# relative frequency of words for each topic
top_words(key_model)

# which documents are most representative for each topic
top_docs(key_model)

# retrieve one top paragraph
small_corpus |> 
  filter(para_id == rownames(dfmat_small[top_docs(key_model)[1,"3_procedure"],])) |> 
  select(text) |> 
  tibble::deframe()

# model fit
plot_modelfit(key_model)
