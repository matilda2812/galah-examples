library(ggplot2)
library(ggtext)
library(dplyr)
# Install {galah} from github
library(galah)
library(lubridate)

#TODO: Incorporate common names into chart

# Most commonly sighted birds in 2020
top_9 <- ala_counts(taxa = ala_taxa("Aves"),
                     filters = select_filters(year = 2020,
                                              data_resource_uid = "dr1411"),
                     group_by = "species",
                     limit = 9)

top_9_counts <- bind_rows(lapply(top_9$name, function(x) {
  counts_2020 <- ala_counts(taxa = ala_taxa(x),
                       filters = select_filters(year = 2020,
                                                data_resource_uid = "dr1411"),
                       group_by = 'month') %>%
    rowwise() %>%
    mutate(month = month(as.integer(name), label = TRUE),
           year = 2020,
           date = as.Date(ISOdate(month = name, year = 2020, day = 1)))
  counts_2019 <- ala_counts(taxa = ala_taxa(x),
                            filters = select_filters(year = 2019,
                                                     data_resource_uid = "dr1411"),
                            group_by = 'month') %>%
    mutate(month = month(as.integer(name), label = TRUE),
           year = 2019,
           date = as.Date(ISOdate(month = name, year = 2020, day = 1)))
  counts <- counts_2019 %>% bind_rows(counts_2020) %>% mutate(bird = x)
  #counts <- counts_2020 %>% mutate(bird = x)
  counts
}))


common_name <- function(n) {
  switch (n,
    "Anas superciliosa" = "Pacific black duck",
    "Cacatua galerita" = "Sulphur-crested cockatoo",
    "Gymnorhina tibicen" = "Australian Magpie",
    "Eolophus roseicapilla" = "Galah"
  )
}


ggplot(top_9_counts) + 
  geom_line(mapping = aes(x = date, y = count, colour = as.character(year))) +
  facet_wrap(~ bird) +
  scale_x_date(date_labels = "%b") + 
  scale_color_discrete(name = "Year") + 
  theme(
    plot.title = element_text(family = "Lato", face = "bold", size = 10, hjust = 0.5),
    axis.title = element_text(family = "Roboto"),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.background = element_blank(), strip.placement = "outside",
    strip.text.x = element_text(size = 7),
    
  ) + 
  ggtitle("Bird sightings per month in the ALA from iNaturalist") +
  xlab("Date") +
  ylab("Record count")


