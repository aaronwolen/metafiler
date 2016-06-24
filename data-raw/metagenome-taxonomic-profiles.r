# See http://huttenhower.sph.harvard.edu/metaphlan
# for explanation of data and file format

library(readr)
library(dplyr)
library(tidyr)
library(purrr)

url <- 'http://huttenhower.sph.harvard.edu/sites/default/files/HMP.PF.txt'

profiles <- read_delim(url, delim = '\t') %>%
  gather(sample, abundance, -sid)


# keep tip of each clade's taxonomic level
profiles <- profiles %>%
  mutate(sid = strsplit(sid, split = '|', fixed = TRUE)) %>%
  mutate(sid = map(sid, ~ tail(., 1))) %>%
  unnest(sid) %>%
  select(sid, sample, abundance)


# keep only species-level information
profiles <- profiles %>%
  separate(sid, c('level', 'taxa'), sep = '__') %>%
  filter(level == 's') %>%
  select(-level)

devtools::use_data(profiles, overwrite = TRUE)
