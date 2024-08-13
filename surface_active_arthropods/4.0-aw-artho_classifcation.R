# Loads the many libraries needed for the text mining and topic modelling. 
library(tm)
library(SnowballC)
library(ggplot2)
library(kernlab)
library(topicmodels)
library(lsa)
library(slam)
library(philentropy)
library(tidytext)
library(Matrix)
library(tidytext)
library(tidyverse)
library(ggdendro)
library(ldatuning)
library(reshape2)

# This sets a global theme for all my plots. 
theme_set(theme_bw() +
            theme(
              plot.background = element_blank()
              ,panel.grid.major = element_blank()
              ,panel.grid.minor = element_blank()
              ,panel.background = element_blank()
              ,panel.border = element_blank()
              ,axis.text.x  = element_text(angle=90, vjust=0.5, size=8)
              ,axis.ticks = element_blank()
            ))

bug_data <- read_csv("./data/processed/surface_active_arthropods_annual_mean_counts_by_species.csv", na=".")
bug_data

bug_select <- bug_data %>% filter(class != 9 & Year > 2013) %>% 
  select(`Site number`, Year, Name, `Annual mean counts`) %>% 
  unite(ID,`Site number`, Year)
bug_select

# This creates a tibble that will be converted into a document term matrix (dtm). 
species_book <- bug_select %>%
  rename(document =ID, term = Name, count = `Annual mean counts`) %>% 
  drop_na()
species_book
summary(species_book)
unique(species_book$document)

# This prepares the tibble data for the dtm. 
# species_book$document <- as.integer(species_book$document)
# species_book$count <- as.numeric(species_book$count)
# species_book$term <- as.character(species_book$term)
# unique(species_book$document)

species_book %>%
  print(n = Inf)

# Converts the tibble to the dtm. 
dtm <- species_book %>%
  cast_dtm(document, term, count)
dtm

# Remove sparse terms
# On sparse - Ken Benoit
# In the sense of the sparse argument to removeSparseTerms(), sparsity refers to the threshold
# of relative document frequency for a term, above which the term will be removed. Relative
# document frequency here means a proportion. As the help page for the command states (although
# not very clearly), sparsity is smaller as it approaches 1.0. (Note that sparsity cannot take
# values of 0 or 1.0, only values in between.)
# 
# For example, if you set sparse = 0.99 as the argument to removeSparseTerms(), then this will
# remove only terms that are more sparse than 0.99. The exact interpretation for sparse = 0.99
# is that for term $j$, you will retain all terms for which $df_j > N * (1 - 0.99)$, where $N$
# is the number of documents -- in this case probably all terms will be retained 
#(see example below).
# 
# Near the other extreme, if sparse = .01, then only terms that appear in (nearly) every document
# will be retained. (Of course this depends on the number of terms and the number of documents,
# and in natural language, common words like "the" are likely to occur in every document and hence
# never be "sparse".)
# 
# An example of the sparsity threshold of 0.99, where a term that occurs at most in (first example)
# less than 0.01 documents, and (second example) just over 0.01 documents:

dtm <- removeSparseTerms(dtm, .95) #Removes words that occur in only a few documents. 
dim(dtm) #This shows the dimensions of the matrix. The second number is the number of words.
dtm

# Looks at species that occur more then 100 times. 
freq_100 <- findFreqTerms(dtm, 100)
freq_100

## Frequency of species occurrence. Top n
dtm_topN <- head(sort(col_sums(dtm), decreasing = TRUE), n=20L)
dtm_topN

assoc <- findAssocs(dtm, "Eleodes longicollis", 0.20)
assoc <- as.data.frame(assoc)
assoc_df_el_long <- assoc %>% rownames_to_column() %>% as_tibble()
assoc_df_el_long

write.table(assoc_df_el_long, "data/processed/eleodes_longicollis_2014_2020_assoc.csv",
            sep=",", row.names = FALSE, quote = FALSE, na = ".")


assoc <- findAssocs(dtm, "Calathus opaculus", 0.20)
assoc <- as.data.frame(assoc)
assoc_df_ca_opa <- assoc %>% rownames_to_column() %>% as_tibble()
assoc_df_ca_opa

write.table(assoc_df_ca_opa, "data/processed/calathus_opaculus_2014_2020_assoc.csv",
           sep=",", row.names = FALSE, quote = FALSE, na = ".")

tidy_dtm <- dtm %>% tidy()
tidy_dtm

bugs_by_year <- tidy_dtm %>% 
  separate_wider_delim(document, "_", names = c("Site number", "Year")) %>% 
  add_count(Year, wt = count, name = "year_total")
bugs_by_year

bugs_by_year$Year <- as.numeric(bugs_by_year$Year)
bugs_by_year$Year

bugs_by_year

bugs_by_year %>%
  filter(term %in% c("Armadillidium vulgare","Calathus opaculus","Curculionidae",
                     "Porcellio laevis","Harpalus sp.","Eleodes suturalis")) %>%
  filter(`Site number`==27) %>% 
  ggplot(aes(Year, count / year_total)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~ term, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "% frequency of species in data set")

bugs_by_year



#####################################
#### CLUSTERING OF RELATED WORDS ####
#####################################

#This takes the term document matrix from the frequncy of word occurance
#and preforms a hierachal cluster analysis on the words
#I am making a choice here to cluster on the top40 occuring words

topN.df <- as.data.frame(dtm_topN) #Convert to a data frame 
head(topN.df)

topN.df.scale <- scale(topN.df) #Scales to normalize the data
topN_d <- dist(topN.df.scale, method = "euclidian")
topN_fit <- hclust(topN_d, method="ward.D")
head(topN_fit)

plot(topN_fit)

#################################################
#### HOW RELATED ARE THE TEXTS TO EACH OTHER ####
#################################################

dtm_w <- weightTfIdf(dtm, normalize = TRUE)
dtm_w
td.mat <- as.matrix(dtm_w)
td.mat

plot(td.mat)

text_dist <- distance(td.mat,method="cosine",test.na = TRUE,use.row.names = TRUE)
text_dist
library(spatstat)

plot(im(text_dist[nrow(text_dist):1,]), main="Correlation Matrix Map")

#dissimilarity <- 1 - text_dist
dissimilarity <- 1 - abs(text_dist)

dist <- as.dist(dissimilarity)

wsHClust2 <- hclust(dist, method = "average")
str(wsHClust2)
wsHClust2$height

#ggplot version of the dendrogram
dend <- ggdendrogram(wsHClust2, theme_dendro=FALSE, labels=TRUE)
dend

#############################
### TOPIC MODELING TUNING ###
#############################
result <- ldatuning::FindTopicsNumber(
  dtm,
  topics = seq(from = 6, to = 30, by = 1),
  metrics = c("CaoJuan2009","Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)

FindTopicsNumber_plot(result)


# Check for empty rows. If you have any, remove them before moving on. 
row_sums(dtm) > 0

######################
### TOPIC MODELING ###
######################

k <- 12
SEED <- 1984


# High alpha priors for topics result in an even distribution of topics within a document.
# Low alpha priors ensure that the inference process distributes the probability mass on a few
# topics for each document.

# To set the prior we need some domain knowledge. On one extreme is a the default prior 
# which is estimated and usually high. You can think of this as being somewhere between a 
# non-informative prior and very weakly informative. A high alpha is basically saying all
# topics (assembles of arthropods) are represented at all sites. We know that our sites should
# contain a mix of topics but usually dominated by 1 or 2. The lower alpha values help to reign
# in the model using our prior knowledge of arthropods and ecosystems. If all of our sites were
# closer (not spanning 360km of river and different habitats) we would choose a higher alpha. 

jss_TM <- LDA(dtm, k = k, method = "Gibbs", SEED=SEED,
              control = list(burnin = 1000, alpha=0.2,
                             thin = 500, iter = 4000))
str(jss_TM)

# Model check
jss_TM@alpha

# Wrangle model output
ladout_gibbs <- jss_TM
ladout_gibbs

Topic <- topics(ladout_gibbs, 8)
Terms <- terms(ladout_gibbs, 8)

dim(Topic)
Topic

dim(Terms)
Terms

#
tmResult <- posterior(jss_TM)

# for every document we have a probability distribution of its contained topics
theta <- tmResult$topics 
dim(theta) 


test_post <- posterior(ladout_gibbs)$topics
test_post
class(test_post)

write.table(test_post, file = "./models/lda/site_year_post_probabs_modelled.csv", sep = ",", col.names = NA)
write.table(Terms, file = "./models/lda/site_year_terms_modelled.csv", sep = ",", col.names = NA)
write.table(Topic, file = "./models/lda/site_year_site_topics_modelled.csv", sep = ",", col.names = NA)

# For plotting topics 

top4termsPerTopic <- terms(ladout_gibbs , 6)
top4termsPerTopic
topicNames <- apply(top4termsPerTopic, 2, paste, collapse=" ")
topicNames

# Pick out the site and years you want to display. 
exampleIds <- c("7_2014", "7_2015", "7_2016", "7_2017", "7_2018", "7_2019", "7_2020")
exampleIds <- c("31_2014", "31_2015", "31_2016", "31_2017", "31_2018", "31_2019", "31_2020")
exampleIds <- c("27_2016", "27_2017", "27_2018", "27_2019", "27_2020")

N <- length(exampleIds)
# get topic proportions form example documents
topicProportionExamples <- theta[exampleIds,]
colnames(topicProportionExamples) <- topicNames
colnames(topicProportionExamples)
vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = exampleIds), variable.name = "topic", id.vars = "document")  
colnames(vizDataFrame)
vizDataFrame$document

ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, ncol = N)



