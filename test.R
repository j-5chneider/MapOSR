#' ---------------------------------------------------------------------------
#' 
#' Testing Review Programming Workflow
#' 
#' ---------------------------------------------------------------------------



# setup
# -----------
library(tidyverse)
library(jsonlite)

pf_test<-("tesing")
dir.create(pf_test)
# -----------





# 
# -----------

# see https://community.rstudio.com/t/how-to-handle-tags-list-in-one-column-of-a-csv-file/15493/7

file <- "Meta-Research on OS-related Surveys.csv" %>%
  file.path(pf_test, .)

lib <- read_csv(file)

lib0 <- lib %>%
  select(1,40)

lib0 %>%
  separate_rows() %>%
  rename(tags=2) %>%
  mutate(tag_dummy = 1) %>%
  spread(tags, tag_dummy, fill = 0)


# -----------





# 
# -----------
# see https://www.r-bloggers.com/2020/04/upset-plots/
# see https://github.com/const-ae/ggupset


# -----------