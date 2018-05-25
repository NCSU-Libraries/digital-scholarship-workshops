
# Getting Altmetrics Data Using R
# by Markus Wust, Alison Blaine, and Erica Hayes

#### Section 1. Setting up Your R Session #####

# Step 1. Install the following packages if not already installed. Otherwise, skip this step. 

install.packages("rAltmetric")
install.packages("plyr")
install.packages("tidyverse")


# Step 2. Load the libraries into your R Session.

library("rAltmetric")
library("plyr")
library("tidyverse")


#### Section 2: Getting the data #### 

# Step 3. Try getting altmetrics from one example doi. 

altmetrics(doi="10.1002/prca.201400084")


# Step 4. Read in a CSV file into a data frame with a bunch of dois listed in one column. 

dois <- read_csv("doi_list.csv") 

dois  #look at the data


# Step 5. Convert the doi column in your dataset to a list. 

dois_list <- as.list(dois$doi)


# Step 6. Create a function to test whether the doi leads to an actual article. 
# If no data is returned, assign the value NA.

getArticleData <- function(x) {
  print(x)                                                #prints the list of dois
  articleData <- try(altmetrics(doi = x), silent = TRUE)  #looks up altmetrics for the doi
  if (class(articleData) == 'try-error') {                #if error is returned, give it the value NA
    return(NA)
  }
  articleData
}


# Step 7. Get the raw_metrics data from every doi in the dois_list dataset.

raw_metrics <- lapply(dois_list, function(x) getArticleData(x))  #apply getArticleFunction to dois_list data.

raw_metrics  # see the data


# Step 8. Notice that there are lots of dois with NA values. Now write two functions to identify 
# those dois that have NA values (because they have no data). 
# This code is adapted from this source on github : https://gist.github.com/rhochreiter/7029236

# Step 8a. This function identifies a doi with NA values.

identifyNa <- function(x) {
  all(is.na(x))
}


# Step 8b. This function sends every doi in raw_metrics to the function we defined in step 8b.
# If a doi does not have an NA value, it is returned and added to raw_metrics_non_na (see step 9).

noNaList <- function(x) {
  return(x[!sapply(x, function(y) identifyNa(y))])
}


# Step 9. You haven't actually removed the values yet. Now remove those NA values by passing the raw_metrics
# data into the function just created--noNaList()--and only copy those values into raw_metrics_non_na that don't have NA values. 

raw_metrics_non_na <- noNaList(raw_metrics)


# Step 10. Now use ldply() to apply the altmetric_data function to get the actual data and return results as a data frame.

metric_data <- ldply(raw_metrics_non_na, altmetric_data)


#### Section 3. Data Cleaning, Subsetting and Filtering ####

# Step 11. Specify a list of the columns you want for your analysis. 

columns_to_grab <- c("title", "doi", "url", "pmid", "journal", "cited_by_fbwalls_count", "cited_by_posts_count", "cited_by_policies_count", "cited_by_wikipedia_count", "cited_by_feeds_count", "cited_by_gplus_count", "cited_by_msm_count", "cited_by_tweeters_count", "cited_by_accounts_count")


# Step 12. Create a data subset only including columns specified in Step 9. 

subset_data <- select(metric_data, one_of(columns_to_grab))

head(subset_data) # examine the data subset


# Step 13. Clean up and rename social media categories. 

reshaped_data <- subset_data %>%
  gather(cited_by, times, cited_by_fbwalls_count:cited_by_accounts_count) %>% # collapses range of columns into two
  mutate(cited_by = gsub("_count", "", cited_by)) %>%
  mutate(cited_by = gsub("cited_by_", "", cited_by)) %>%
  mutate(cited_by = gsub("tweeters", "Twitter", cited_by)) %>%
  mutate(cited_by = gsub("fbwalls", "Facebook", cited_by)) %>%
  mutate(cited_by = gsub("gplus", "Google+", cited_by)) %>%
  mutate(cited_by = gsub("feeds", "Bloggers", cited_by)) %>%
  mutate(cited_by = gsub("msm", "News Outlets", cited_by)) %>%
  mutate(cited_by = gsub("posts", "Posts", cited_by)) %>%
  mutate(cited_by = gsub("accounts", "Total", cited_by)) %>%
  mutate(cited_by = gsub("policies", "Policies", cited_by)) %>%
  mutate(cited_by = gsub("wikipedia", "Wikipedia", cited_by)) %>%
  mutate(times = as.numeric(times))

reshaped_data


# Step 14. Filter the data by total number of times cited to be greater than or equal to 4

graph_data <- reshaped_data %>% 
  filter(times >= 4) %>%
  filter(cited_by != "Total")


#### Section 4. Graphing the Data #### 

# Step 15. Set a color-blind friendly color palette

cbPalette <- c("#333333", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# Step 16. Graph the data using ggplot2. It won't look great - we will fix it in later charts.

ggplot(graph_data) + geom_bar(aes(x=cited_by, y=times, fill=doi), stat="identity")+
  labs(x = "Media") + scale_fill_manual(values=cbPalette) + coord_flip() + theme_classic()


# Step 17. Create a column with title abbreviations to use as the title of the graphs (instead of the doi).

graph_data <- mutate(graph_data, title_abbr = paste(substr(graph_data$title, 1, 18), "...", sep="" ))


# Step 18. Regraph with the bars according to frequency. Set title to title_abbr variable.
# Inside facet_wrap(), use title_abbr

ggplot(graph_data) + geom_bar(aes(fct_infreq(factor(cited_by)), times, fill=title_abbr), stat="identity")+
  labs(x = "Media", fill="Abbreviated Title") + scale_fill_manual(values=cbPalette)  + theme_classic() + facet_wrap(~title_abbr)


#### Section 5. Saving the data and graphs to your computer #### 

# Step 19. Write the graph data to a CSV file.

write.csv(graph_data, file = 'sample_data_altmetrics.csv')


# Step 20. Save the graph to a file on your computer 

my_graph <- ggplot(graph_data) + geom_bar(aes(fct_infreq(factor(cited_by)), times, fill=title_abbr), stat="identity")+
  labs(x = "Media Types", y="Times Cited", title="Altmetrics for Requested Articles", fill="Abbreviated Title") + scale_fill_manual(values=cbPalette)  + theme_classic() + facet_wrap(~title_abbr)

ggsave("graph.png", plot=my_graph, height=5, width= 9)  #default for height and width is inches.

