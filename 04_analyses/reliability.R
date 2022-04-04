library(tidyverse)
library(irr)

# IMPORT ########################################################################
rel <- rio::import("02_data_tidy/reliability.xlsx")

# WRANGLING #####################################################################
rel <- rel %>%
  mutate(across(2:11, ~ str_remove_all(., "(a=|m=|d=|t=|geo=)"))) %>% # alle präfixe löschen
  pivot_longer(2:11, names_to = "variable", values_to = "values") %>%
  separate_rows(values) %>%                  # mehrere Einträge in Zellen als mehrere Zeilen
  mutate(values = trimws(tolower(values)),   # whitespace löschen, Kleinbuchstaben
         values = case_when(
                      values == "librarians" ~ "librarian",   # Tippfehler recoding
                      values == "bibliometric" ~ "biblio",    # geo code zar gibt es nicht -> zaf?
                      values == "openpolicy" ~ "openpolicies",
                      values == "opentool" ~ "opentools",
                      values == "publishers" ~ "publisher",
                      values == "researchers" ~ "researcher",
                      values == "soscie" ~ "socscie",
                      str_detect(variable, "geo") & (values == "none" | values == "all") ~ "unspecific",
                      str_detect(variable, "discipline") & (values == "none" | values == "all") ~ "unspecific",
                      TRUE ~ as.character(values)),
         cells = 1) %>%                             # add new variable for coded values
  dplyr::filter(values != "" & !is.na(values)) %>%  # Leere Zellen entfernen
  pivot_wider(id_cols = ID, 
              names_from = c(variable, values),
              values_from = cells, values_fill = 0)

# KAPPA #######################################################################

kappa_variablen <- data.frame(variablen = str_remove(names(rel[-1]), "(c1_|c2_)"))

kappa_variablen <- kappa_variablen %>%
                      group_by(variablen) %>%
                      dplyr::summarise(count = n())

# Variablen OHNE Übereinstimmung
kappa_variablen %>%
  dplyr::filter(count != 2) %>%
  print(n=30)

# Variablen für die man Übereinstimmung berechnen kann
kappa_variablen <- kappa_variablen %>%
  dplyr::filter(count == 2)

# generate empty data frame for results
kappa_results <- data.frame(variable = as.character(),
                            kappa    = as.numeric(),
                            percent  = as.numeric())

#loop over all dichotomous variables
for(i in kappa_variablen$variablen) {
  
  tmp$kappa <- kappa2(data.frame(rel[paste0("c1_", i)],
                                 rel[paste0("c2_", i)]))
  
  tmp$perc <- agree(data.frame(rel[paste0("c1_", i)],
                               rel[paste0("c2_", i)]))
  
  kappa_results <- kappa_results %>%
    add_row(variable = i,
            kappa    = tmp$kappa$value,
            percent  = tmp$perc$value)
}

# plot results kappa
ggplot(kappa_results, aes(x=kappa)) +
  geom_density(size=2) +
  geom_dotplot() +
  scale_x_continuous(expand=c(0,0)) +
  geom_vline(xintercept = .7, color="red", alpha=.5, linetype="dashed", size=1) +
  theme_light()

# plot results percent
ggplot(kappa_results, aes(x=percent)) +
  geom_density(size=1) +
  geom_dotplot(binwidth = 1) +
  scale_x_continuous(expand=c(0,0), limits = c(0,100)) +
  geom_vline(xintercept = 80, color="red", alpha=.5, linetype="dashed", size=1) +
  theme_light()

# results table
# ordered by kappa
kappa_results[order(kappa_results$kappa),]

# ordered by percent
kappa_results[order(kappa_results$percent),]

# summary of results
summary(kappa_results)

