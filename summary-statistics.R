##-----------------------------------------------##
##  METHODS IN ECOLOGY AND EVOLUTION REVIEW      ##
##  Quantifying and defining marine novelty      ##
##  Emer Cunningham - November 2025              ##
##-----------------------------------------------##

# load packages
library(readxl)
library(tidyverse)

# load Web of Science searches -- 7 November 2025
# query link below, downloaded top 1000 most relevant search results
# https://www.webofscience.com/wos/woscc/summary/204dbc23-69f9-4d29-9dd2-e3d19d1a2d4a-018716c2f8/0502142a-9a8e-40c6-bc27-5046ff867728-018716bbbd/relevance/1
# i.e., (((TS = novel ecosyste*) OR (TS = ecological novelty)) OR (TS = novel ecolog*))
wos_dat <- read_excel("data/savedrecs.xls")

# or for lots of novelties
# https://www.webofscience.com/wos/woscc/summary/91e1ec67-1fa8-4773-9eed-48d78cdb3cf2-0187179bf9/0feaa463-29fb-49c6-a76c-dac0b930afa8-0187179bc4/relevance/1
# ((((((((TS = novel ecosyste*) OR (TS = novel ecolog*)) OR (TS = ecological novelty)) 
# OR (TS = novel communit*)) OR (TS = novel compositio*)) OR (TS = novel stat*)) 
# OR (TS = novel species)) OR (TS = novel assemblag*))
#wos_dat <- read_excel("data/savedrecs (1).xls")

#

##### 1. explore and test #####

# check
head(wos_dat)
names(wos_dat)

# let's clean some things up -- select and rename columns
wos_dat <- wos_dat %>%
  rename_all(tolower) %>%
  select("document type", "publication year", "publication date", 
         "article title", "source title", "language",
         "authors", "author keywords", "abstract", 
         "times cited, all databases", "publisher") %>%
  rename_with(~ gsub(" ","_", .x), contains(" "))

# some summary tables:

# types of literature
summary_doc_type_dat <- wos_dat %>%
  group_by(document_type) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count))

summary_doc_type_dat

# publishing papers and getting citations over time
summary_year_dat <- wos_dat %>%
  group_by(publication_year) %>%
  summarise(papers = n(),
            citations = sum(`times_cited,_all_databases`, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(publication_year))

summary_year_dat

# visualise
plot_publications <- ggplot(data = summary_year_dat,
                            aes(x = publication_year, y = papers)) +
  labs(x = "Publication year", 
       y = "Number of papers published") +
  geom_vline(xintercept = 2006, lty = 2, col = "grey30") +
  geom_point() +
  geom_line() +
  theme_bw()

plot_citations <- ggplot(data = summary_year_dat,
                         aes(x = publication_year, y = citations)) +
  labs(x = "Publication year", 
       y = "Number of citations") +
  geom_vline(xintercept = 2006, lty = 2, col = "grey30") +
  geom_point() +
  geom_line() +
  theme_bw()

plot_publications
plot_citations

#

# # languages
# summary_language_dat <- wos_dat %>%
#   group_by(language) %>%
#   summarise(count = n()) %>%
#   ungroup() %>%
#   arrange(desc(count))
# 
# summary_language_dat
# # why doesn't language pop up as anything? it's all NA values?

#

# search for marine / ocean / coast / water / aquatic / salt

# simple table search
wos_dat %>%
  pull(article_title) %>%
  str_detect(pattern = "marine") %>%
  table()

# create a vector of marine terms to look for
marine_terms <- c("marine", "aquatic", "ocean", "coast", "water")

# we're happy to find any one of these terms -- use the "or" symbol
marine_terms_or <- paste(marine_terms, collapse = "|")

# detect these strings in the title and abstract of each article
wos_dat <- wos_dat %>%
  mutate(watery_title = case_when(str_detect(article_title,
                                             marine_terms_or) == TRUE  ~ "marine",
                                  .default = "non-marine")) %>%
  mutate(watery_abstract = case_when(str_detect(abstract,
                                                marine_terms_or) == TRUE ~ "marine",
                                     .default = "non-marine"))

# what are our proportions looking like?
table(wos_dat$watery_title)
table(wos_dat$watery_abstract)

#

# re-do the papers over time by marine and non-marine contributions
watery_abstract_dat <- wos_dat %>%
  group_by(publication_year, watery_abstract) %>%
  summarise(papers = n(),
            citations = sum(`times_cited,_all_databases`, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(publication_year)) %>%
  filter(publication_year != 2026)

watery_abstract_dat

# stacked bar plot
plot_marine_abstract <- ggplot(watery_abstract_dat,
                               aes(x = publication_year, y = papers,
                                   fill = watery_abstract)) + 
  geom_bar(position = position_stack(reverse = TRUE),
           stat = "identity") +
  scale_fill_manual(values = c("grey20", "grey60")) +
  labs(x = "Publication year", y = "Number of publications",
       fill = "Abstract\ncontent") +
  geom_vline(xintercept = 2006, lty = 2) +
  theme_bw()

plot_marine_abstract

#

# proportion?

# re-do the papers over time by marine and non-marine contributions
prop_marine_dat <- wos_dat %>%
  group_by(publication_year) %>%
  summarise(papers = n(),
            proportion = length(which(watery_abstract == "marine"))/n()) %>%
  ungroup() %>%
  arrange(desc(publication_year)) %>%
  filter(publication_year != 2026)

# summary on that!
boxplot(prop_marine_dat$proportion)
mean(prop_marine_dat$proportion) # on average, a proportion of 22.88% of studies
# even MENTION marine or aquatic (eco)systems

# what about the "recent" studies, after 2006 (Hobbs novel ecosystems)
prop_marine_dat %>%
  filter(publication_year > 2006) %>%
  pull(proportion) %>%
  mean() # 19.70% of studies post-Hobbs have even MENTIONED the marine world

#

# let's now filter for all the abstracts identified as "watery"
# what do they actually discuss? what scientific contribution? quantitative?
watery_wos_dat <- wos_dat %>%
  filter(watery_abstract == "marine")

table(watery_wos_dat$watery_title) # woah only 22.60% of their TITLES are watery
# marine    non-marine 
# 47        161 

# read some abstracts? firstly of the watery abstract ones
# we could export these, and put them into excel again after manually assigning
# them a Y/N relevance and a Y/N quantitative (or qual, quant, theory, review)
write.csv(watery_wos_dat, "output/watery-web-of-science-results.csv")

#

#

#

#