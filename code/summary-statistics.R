##-----------------------------------------------##
##  METHODS IN ECOLOGY AND EVOLUTION REVIEW      ##
##  Quantifying and defining marine novelty      ##
##-----------------------------------------------##

# load packages
library(ragg) # for saving plots
library(readxl) # for reading WoS search exports
library(tidyverse) # for many helpful functions

# load Web of Science searches -- 7 November 2025
# query link below, downloaded top 1000 most relevant search results
# https://www.webofscience.com/wos/woscc/summary/204dbc23-69f9-4d29-9dd2-e3d19d1a2d4a-018716c2f8/0502142a-9a8e-40c6-bc27-5046ff867728-018716bbbd/relevance/1
# i.e., (((TS = novel ecosyste*) OR (TS = ecological novelty)) OR (TS = novel ecolog*))
wos_dat <- read_excel("data/wos-novel-ecosystems.xls")

#

#### 1. process WoS search data #####

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

#### 2. identify marine-related works ####

# search for marine / aquatic / ocean / coast / benthic / pelagic / estuary

# simple table search -- is "marine" in the title?
wos_dat %>%
  pull(article_title) %>%
  str_detect(pattern = "marine") %>%
  table()

# create a vector of marine terms to look for
marine_terms <- c("marine", "aquatic", "ocean", "coast",
                  "benth", "pelagi", "estuar")

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
table(wos_dat$watery_title)    # 36 marine-related titles
table(wos_dat$watery_abstract) # 143 marine-related abstracts

#

# re-do the papers-over-time-plot by marine and non-marine contributions
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

# over time, what proportion of published studies have marine-related abstracts?

# re-do the papers over time by marine and non-marine contributions
prop_marine_dat <- wos_dat %>%
  group_by(publication_year) %>%
  summarise(papers = n(),
            proportion = length(which(watery_abstract == "marine"))/n()) %>%
  ungroup() %>%
  arrange(desc(publication_year)) %>%
  filter(publication_year != 2026)

# visualise the spread of annual marine study proportions
boxplot(prop_marine_dat$proportion,
        xlab = "Proportion of studies\nwith marine-related abstracts")

# on average, a proportion of 18.92% of studies
# even MENTION marine or aquatic (eco)systems
mean(prop_marine_dat$proportion) * 100

# what about the "recent" studies, after 2006 (Hobbs novel ecosystems)
prop_marine_dat %>%
  filter(publication_year > 2006) %>%
  pull(proportion) %>%
  mean() # 13.03% of studies post-Hobbs mention marine terms in their abstracts

prop_marine_dat %>%
  filter(publication_year > 2006) %>%
  pull(proportion) %>%
  sd() # standard deviation = 5.74% 

#

# let's now filter for all the abstracts identified as "watery"
# what do they actually discuss? what scientific contribution? quantitative?
watery_wos_dat <- wos_dat %>%
  filter(watery_abstract == "marine")

# additionally, 22.37% of their titles are watery
table(watery_wos_dat$watery_title) 
# marine    non-marine 
# 32        111 

# save these abstracts for further manual analysis of their content
#write.csv(watery_wos_dat, "output/watery-web-of-science-results.csv")

#

#### 3. analyse marine-related abstracts ####

# I manually read these abstracts and assigned them to a few categories:

# marine_content = assess whether the paper is about marine science
# dimension = broad content of the paper: do the authors study climate novelty,
#             ecological, evolutionary, or social novelty, or just use novelty
#             as a "technical" term?
# novelty_measured = do the authors quantify novelty directly? 
# novelty_study_type = note the context for novelty research: are we studying
#                      empirical ecological novelty or biotechnology innovation?
# study_system = note the paper's focal taxa, place, or field

# read in the manually-assigned abstracts
marine_dat <- read.csv("output/watery-web-of-science-results_EC.csv")

# let's firstly visualise everything
marine_stack_dat <- marine_dat %>%
  select(marine_content:novelty_measured) %>%
  mutate(marine_content = ifelse(marine_content == TRUE,
                                 "marine",
                                 "non-marine"),
         novelty_measured = ifelse(novelty_measured == TRUE,
                                   "measured",
                                   "non-measured"),
         novelty_measured = ifelse(dimension == "technical",
                                   NA,
                                   novelty_measured)) %>%
  pivot_longer(cols = (marine_content:novelty_measured),
               names_to = "bar",
               values_to = "class") %>%
  filter(!is.na(class)) %>%
  group_by(bar, class) %>%
  summarise(number_abstracts = length(class)) %>%
  ungroup()

# check
marine_stack_dat

# also calculate proportions
marine_stack_dat <- marine_stack_dat %>%
  group_by(bar) %>%
  mutate(prop_abstracts = number_abstracts / sum(number_abstracts))

# also make an annoying string like: "dimension (number_abstracts)"
marine_stack_dat <- marine_stack_dat %>%
  mutate(label = paste(class, " (", number_abstracts, ")",
                       sep = ""))

# set a fully grayscale version of colours:
class_colours_bw <- c("grey10", "grey30", "grey40", "grey20", "grey20",
                      "grey70", "grey70", "grey10", "grey70")
text_colours_bw <- c("white", "white", "white", "white", "black",
                     "white", "black", "white", "black")

#

# plot without novelty_study_type
plot_marine_stack <- ggplot(
  data = marine_stack_dat %>%
    filter(bar != "novelty_study_type"),
  aes(x = factor(bar, levels = c("marine_content",
                                 "dimension",
                                 "novelty_measured")), 
      y = prop_abstracts,
      fill = class)) +
  scale_fill_manual(values = class_colours_bw) +
  scale_x_discrete(labels = c("marine focus", "novelty focus",
                              "novelty quantification")) +
  labs(x = "Content of publications",
       y = "Proportion of publications") +
  geom_bar(position = "stack", stat = "identity",
           width = 0.75) +
  geom_text(aes(label = label), 
            size = 3, position = position_stack(vjust = 0.5),
            colour = text_colours_bw) +
  theme_classic() +
  theme(legend.position = "none")

# visualise
plot_marine_stack

#

### let's check the statistics ###

# how many marine-related papers actually had a marine focus?
marine_dat %>%
  pull(marine_content) %>%
  table()

(113/143) * 100 # papers = 79.02% marine

# how many of these marine-focused papers considered novel ecosystems?
marine_dat %>%
  filter(marine_content == TRUE) %>%
  pull(dimension) %>%
  table()

(37/113) * 100 # papers = 32.74% marine novelty

# how many of these marine novelty-focused papers directly measure novelty?
marine_dat %>%
  filter(marine_content == TRUE,
         dimension != "technical") %>%
  pull(novelty_measured) %>%
  table()

(12/37) * 100 # papers = 32.43% quantitative marine novelty

#

# and that means, that out of a total of 1000 search results:
113/1000 * 100 # 11.30% of papers are marine,
37/1000 * 100  #  3.70% of papers are about novel marine ecosystems, and
12/1000 * 100  #  1.20% of papers quantify novel marine ecosystems

#

# now: the marine + novelty + quantitative studies!

# let's save all of the true ecosystem attribute papers that measure novelty
# (our key focus of quantitative novelty in marine ecosystems)
focus_dat <- marine_dat %>%
  filter(marine_content == TRUE,
         dimension %in% c("ecological", "evolutionary", "climate", "social"),
         novelty_measured == TRUE) %>%
  select(-X) %>%
  as_tibble()

# check
focus_dat

# what is the subject matter of these papers?
focus_dat %>%
  select(article_title, dimension, novelty_study_type, system) %>%
  arrange(dimension, novelty_study_type)

#

#

#

#### 4. save figures ####

# overview (n = 1000)

agg_png("figures/marine-abstracts-over-time.png",
        width = 7, height = 3.5, units = "in",
        scaling = 1, res = 1000)

plot_marine_abstract

dev.off()

# marine abstracts (n = 143)

agg_png("figures/marine-abstracts-content.png",
        width = 5.5, height = 4.5, units = "in",
        scaling = 1, res = 1000)

plot_marine_stack

dev.off()

#