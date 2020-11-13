#' ---------------------------------------------------------------------------
#' 
#' Testing Review Programming Workflow
#' 
#' ---------------------------------------------------------------------------



# setup
# -----------
library(tidyverse)
library(lubridate)
library(cowplot)
library(ggthemes)

current_export <- "test_2020-10-29" %>%    #easier copy&paste without file ending
  paste0(., ".csv")
  
pf_01 <- ("01_zotero_export")
pf_02 <- ("02_data_tidy")
pf_03 <- ("03_graphs")
# -----------





# loading export
# -----------

# see https://community.rstudio.com/t/how-to-handle-tags-list-in-one-column-of-a-csv-file/15493/7

file <- current_export %>%
  file.path(pf_01, .)

lib <- read_csv(file)

lib0 <- lib %>%
  select(1,40)

# inspect tags to see whether they are in order
lib0 %>%
  rename(tags=2) %>%
  separate_rows(tags, sep="; ") %>%
  pull(tags)


# check that keys are unique
lib0 %>%
  group_by(Key) %>%
  summarize(n=n()) %>%
  filter(n>1)


# separate tags into columns
lib_long01 <- lib0 %>%
  rename(tags=2) %>%
  separate_rows(tags, sep="; ") 


# get a variable with n as a separate column
lib_n <- lib_long01 %>%
  filter(str_sub(tags, 1, 1)=="n") %>%
  mutate(n=str_replace(tags, "n=", "")) %>%
  mutate(n=as.numeric(str_replace(n, "XXX", NA_character_))) %>%
  select(Key, n)


# get a variable with year as separate column
lib_year <- lib_long01 %>%
  filter(str_sub(tags, 1, 1)=="y") %>%
  mutate(temp_coverage=str_replace(tags, "y=", "")) %>%
  select(Key, temp_coverage)


# add n and year as well as remove it from tags
lib_long02 <- lib_long01 %>%
  filter(str_sub(tags, 1, 1)!="n") %>%
  filter(str_sub(tags, 1, 1)!="y") %>%
  left_join(., lib_n, by=key) %>%
  left_join(., lib_year, by=key) 


# spread tags, one reference per row
lib1 <- lib_long02 %>%
  mutate(
    tags=str_replace(tags, "=", "_"),
    tag_dummy = 1) %>%
  arrange(tags) %>%
  pivot_wider(names_from=tags, values_from=tag_dummy, values_fill = list(tag_dummy=0))

# join with basic info
lib2 <- lib %>%
  select(1:6) %>%
  left_join(., lib1, by="Key")

# -----------




# export
# -----------
export_name <- "test_export_tagged" 

file_name <- export_name %>%
  paste0(., "__", today(), ".csv") %>%
  file.path(pf_02, .)

write_excel_csv2(lib2, file_name)  

# -----------




# get some example graphs
# -----------
# see https://www.r-bloggers.com/2020/04/upset-plots/
# see https://github.com/const-ae/ggupset

# bar chart of topics
use <- lib2 %>%
  select(starts_with("a_")) %>%
  select(!ends_with("XXX")) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarize("n"=sum(value)) %>%
  arrange(n) %>%
  mutate(order = factor(1:5))

ylabs <- use %>%
  pull(name) %>%
  str_replace(., "a_", "") %>%
  str_replace(., "open", "open ") %>%
  tools::toTitleCase(.) %>%
  str_wrap(25)
  
  
#check
lib2 %>% pull(a_opendata)

ggplot(data=use, aes(y=order, x=n)) + 
  geom_bar(
    stat="identity",
    fill=colorblind_pal()(8)[4] ,
    alpha=.6) +
  scale_y_discrete(
    name= "Action",
    label=ylabs) +
  scale_x_continuous(
    name="Count") +
  theme_minimal_vgrid(12) +
  theme(
    plot.title.position = "plot",
    legend.position = "none",
    axis.line.y = element_blank()
  ) +
  labs(title = str_wrap("Open Science Action", 60)) 

plotname <- "example_01.pdf" %>%
  file.path(pf_03, .)
ggsave(plotname)







# bivariate action by year
use <- lib2 %>%
  select()

# -----------