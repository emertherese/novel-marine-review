##-----------------------------------------------##
##  METHODS IN ECOLOGY AND EVOLUTION REVIEW      ##
##  Quantifying and defining marine novelty      ##
##  Emer Cunningham - January 2025              ##
##-----------------------------------------------##

# load packages
library(ragg) # for saving plots
library(readxl) # for reading WoS search exports
library(tidyverse) # for many helpful functions

# load Web of Science searches -- 20 January 2025
# query link below, downloaded top 1000 most relevant search results
# for two adjacent fields, to contextualise the proportion of marine:non-marine

# community ecology:
# https://www.webofscience.com/wos/woscc/summary/3a65bb68-d0ff-43f0-86a0-33891d469678-0199c18d03/relevance/1
wos_comm_dat <- read_excel("data/wos-community-ecology.xls")

# functional ecology:
# https://www.webofscience.com/wos/woscc/summary/7d1f54ab-d14e-43f1-b32d-f1220c6589cd-0199c183da/relevance/1
wos_func_dat <- read_excel("data/wos-functional-ecology.xls")

#

#### 1. community ecology #####

# check
head(wos_comm_dat)
names(wos_comm_dat)

# let's clean some things up -- select and rename columns
wos_comm_dat <- wos_comm_dat %>%
  rename_all(tolower) %>%
  select("document type", "publication year", "publication date", 
         "article title", "source title", "language",
         "authors", "author keywords", "abstract", 
         "times cited, all databases", "publisher") %>%
  rename_with(~ gsub(" ","_", .x), contains(" "))

# some summary tables:

# types of literature
summary_doc_type_dat <- wos_comm_dat %>%
  group_by(document_type) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count))

summary_doc_type_dat

# publishing papers and getting citations over time
summary_year_dat <- wos_comm_dat %>%
  group_by(publication_year) %>%
  summarise(papers = n(),
            citations = sum(`times_cited,_all_databases`, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(publication_year))

summary_year_dat

# visualise
plot_comm_publications <- ggplot(data = summary_year_dat,
                            aes(x = publication_year, y = papers)) +
  labs(x = "Publication year", 
       y = "Number of papers published") +
  geom_vline(xintercept = 2006, lty = 2, col = "grey30") +
  geom_point() +
  geom_line() +
  theme_bw()

plot_comm_citations <- ggplot(data = summary_year_dat,
                         aes(x = publication_year, y = citations)) +
  labs(x = "Publication year", 
       y = "Number of citations") +
  geom_vline(xintercept = 2006, lty = 2, col = "grey30") +
  geom_point() +
  geom_line() +
  theme_bw()

plot_comm_publications
plot_comm_citations

#

### identify marine-related works ###

# search for marine / aquatic / ocean / coast / benthic / pelagic / estuary

# create a vector of marine terms to look for
marine_terms <- c("marine", "aquatic", "ocean", "coast",
                  "benth", "pelagi", "estuar")

# we're happy to find any one of these terms -- use the "or" symbol
marine_terms_or <- paste(marine_terms, collapse = "|")

# detect these strings in the title and abstract of each article
wos_comm_dat <- wos_comm_dat %>%
  mutate(watery_title = case_when(str_detect(article_title,
                                             marine_terms_or) == TRUE  ~ "marine",
                                  .default = "non-marine")) %>%
  mutate(watery_abstract = case_when(str_detect(abstract,
                                                marine_terms_or) == TRUE ~ "marine",
                                     .default = "non-marine"))

# what are our proportions looking like?
table(wos_comm_dat$watery_title)    # 72 (vs. 36 novel) marine-related titles
table(wos_comm_dat$watery_abstract) # 215 (vs. 143 novel) marine-related abstracts

#

# re-do the papers-over-time-plot by marine and non-marine contributions
watery_abstract_dat <- wos_comm_dat %>%
  group_by(publication_year, watery_abstract) %>%
  summarise(papers = n(),
            citations = sum(`times_cited,_all_databases`, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(publication_year)) %>%
  filter(publication_year != 2026)

watery_abstract_dat

# stacked bar plot
plot_comm_abstract <- ggplot(watery_abstract_dat,
                               aes(x = publication_year, y = papers,
                                   fill = watery_abstract)) + 
  geom_bar(position = position_stack(reverse = TRUE),
           stat = "identity") +
  scale_fill_manual(values = c("grey20", "grey60")) +
  labs(x = "Publication year", y = "Number of publications",
       fill = "Abstract\ncontent") +
  geom_vline(xintercept = 2006, lty = 2) +
  theme_bw()

plot_comm_abstract

#

# over time, what proportion of published studies have marine-related abstracts?

# re-do the papers over time by marine and non-marine contributions
prop_comm_dat <- wos_comm_dat %>%
  group_by(publication_year) %>%
  summarise(papers = n(),
            proportion = length(which(watery_abstract == "marine"))/n()) %>%
  ungroup() %>%
  arrange(desc(publication_year)) %>%
  filter(publication_year != 2026)

# visualise the spread of annual marine study proportions
boxplot(prop_comm_dat$proportion,
        xlab = "Proportion of studies\nwith marine-related abstracts",
        ylim = c(0, 1))

# on average, a proportion of 18.06% of studies
# even MENTION marine or aquatic (eco)systems
mean(prop_comm_dat$proportion) * 100

# plot proportion of marine studies over time:
plot_comm_prop <- ggplot(data = prop_comm_dat,
                         aes(x = publication_year, y = proportion)) +
  labs(x = "Publication year", 
       y = "Proportion of studies with marine-related abstracts") +
  ylim(0, 1) +
  geom_vline(xintercept = 2006, lty = 2, col = "grey30") +
  geom_point() +
  geom_line() +
  theme_bw()

plot_comm_prop

#

# # what about the "recent" studies, after 2006 (Hobbs novel ecosystems)
# prop_marine_dat %>%
#   filter(publication_year > 2006) %>%
#   pull(proportion) %>%
#   mean() # 21.14% of studies post-Hobbs mention marine terms in their abstracts
# 
# prop_marine_dat %>%
#   filter(publication_year > 2006) %>%
#   pull(proportion) %>%
#   sd() # standard deviation = 5.74% 
# 
# #
# 
# # let's now filter for all the abstracts identified as "watery"
# # what do they actually discuss? what scientific contribution? quantitative?
# watery_wos_comm_dat <- wos_comm_dat %>%
#   filter(watery_abstract == "marine")
# 
# # additionally, 22.37% of their titles are watery
# table(watery_wos_comm_dat$watery_title) 
# # marine    non-marine 
# # 32        111 

#

#### 2. functional ecology ####

# check
head(wos_func_dat)
names(wos_func_dat)

# let's clean some things up -- select and rename columns
wos_func_dat <- wos_func_dat %>%
  rename_all(tolower) %>%
  select("document type", "publication year", "publication date", 
         "article title", "source title", "language",
         "authors", "author keywords", "abstract", 
         "times cited, all databases", "publisher") %>%
  rename_with(~ gsub(" ","_", .x), contains(" "))

# some summary tables:

# types of literature
summary_doc_type_dat <- wos_func_dat %>%
  group_by(document_type) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count))

summary_doc_type_dat

# publishing papers and getting citations over time
summary_year_dat <- wos_func_dat %>%
  group_by(publication_year) %>%
  summarise(papers = n(),
            citations = sum(`times_cited,_all_databases`, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(publication_year))

summary_year_dat

# visualise
plot_func_publications <- ggplot(data = summary_year_dat,
                            aes(x = publication_year, y = papers)) +
  labs(x = "Publication year", 
       y = "Number of papers published") +
  geom_vline(xintercept = 2006, lty = 2, col = "grey30") +
  geom_point() +
  geom_line() +
  theme_bw()

plot_func_citations <- ggplot(data = summary_year_dat,
                         aes(x = publication_year, y = citations)) +
  labs(x = "Publication year", 
       y = "Number of citations") +
  geom_vline(xintercept = 2006, lty = 2, col = "grey30") +
  geom_point() +
  geom_line() +
  theme_bw()

plot_func_publications
plot_func_citations

#

### identify marine-related works ###

# search for marine / aquatic / ocean / coast / benthic / pelagic / estuary

# create a vector of marine terms to look for
marine_terms <- c("marine", "aquatic", "ocean", "coast",
                  "benth", "pelagi", "estuar")

# we're happy to find any one of these terms -- use the "or" symbol
marine_terms_or <- paste(marine_terms, collapse = "|")

# detect these strings in the title and abstract of each article
wos_func_dat <- wos_func_dat %>%
  mutate(watery_title = case_when(str_detect(article_title,
                                             marine_terms_or) == TRUE  ~ "marine",
                                  .default = "non-marine")) %>%
  mutate(watery_abstract = case_when(str_detect(abstract,
                                                marine_terms_or) == TRUE ~ "marine",
                                     .default = "non-marine"))

# what are our proportions looking like?
table(wos_func_dat$watery_title)    # 89 (vs. 36 novel) marine-related titles
table(wos_func_dat$watery_abstract) # 226 (vs. 143 novel) marine-related abstracts

#

# re-do the papers-over-time-plot by marine and non-marine contributions
watery_abstract_dat <- wos_func_dat %>%
  group_by(publication_year, watery_abstract) %>%
  summarise(papers = n(),
            citations = sum(`times_cited,_all_databases`, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(publication_year)) %>%
  filter(publication_year != 2026)

watery_abstract_dat

# stacked bar plot
plot_func_abstract <- ggplot(watery_abstract_dat,
                               aes(x = publication_year, y = papers,
                                   fill = watery_abstract)) + 
  geom_bar(position = position_stack(reverse = TRUE),
           stat = "identity") +
  scale_fill_manual(values = c("grey20", "grey60")) +
  labs(x = "Publication year", y = "Number of publications",
       fill = "Abstract\ncontent") +
  geom_vline(xintercept = 2006, lty = 2) +
  theme_bw()

plot_func_abstract

#

# over time, what proportion of published studies have marine-related abstracts?

# re-do the papers over time by marine and non-marine contributions
prop_func_dat <- wos_func_dat %>%
  group_by(publication_year) %>%
  summarise(papers = n(),
            proportion = length(which(watery_abstract == "marine"))/n()) %>%
  ungroup() %>%
  arrange(desc(publication_year)) %>%
  filter(publication_year != 2026)

# visualise the spread of annual marine study proportions
boxplot(prop_func_dat$proportion,
        xlab = "Proportion of studies\nwith marine-related abstracts",
        ylim = c(0, 1))

# on average, a proportion of 22.44% of studies
# even MENTION marine or aquatic (eco)systems
mean(prop_marine_dat$proportion) * 100

# plot proportion of marine studies over time:
plot_func_prop <- ggplot(data = prop_func_dat,
                         aes(x = publication_year, y = proportion)) +
  labs(x = "Publication year", 
       y = "Proportion of studies with marine-related abstracts") +
  ylim(0, 1) +
  geom_vline(xintercept = 2006, lty = 2, col = "grey30") +
  geom_point() +
  geom_line() +
  theme_bw()

plot_func_prop

#

#### 3. proportions plot ####

# bring all data together:
prop_marine_dat$topic <- "novel"
prop_comm_dat$topic <- "community"
prop_func_dat$topic <- "functional"

prop_dat <- rbind(prop_marine_dat, prop_comm_dat, prop_func_dat)

# plot all proportions over time:

plot_marine_props <- ggplot(data = prop_dat,
                            aes(x = publication_year, y = proportion,
                                col = topic)) +
  labs(x = "Publication year", 
       y = "Proportion of marine-related abstracts", 
       col = "Research\ntopic") +
  ylim(0, 1) +
  geom_point() +
  geom_line() +
  scale_colour_manual(values = c("novel" = "black", 
                                 "community" = "grey60",
                                 "functional" = "grey30")) +
  geom_vline(xintercept = 2006, lty = 2, col = "grey30") +
  theme_bw()

plot_marine_props

# summarise the mean and standard deviations:

prop_summary_dat <- prop_dat %>%
  #filter(publication_year > 2006) %>%
  group_by(topic) %>%
  summarise(prop_mean = mean(proportion, na.rm = TRUE),
            prop_sd = sd(proportion, na.rm = TRUE)) %>%
  ungroup()

prop_summary_dat

#

#### 4. save figures ####

# overview (n = 1000)

agg_png("figures/marine-proportions-in-ecology.png",
        width = 7.2, height = 3.5, units = "in",
        scaling = 1, res = 1000)

plot_marine_props

dev.off()

#