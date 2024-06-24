#### Analysis for community involvement statement project
### Results reported in main manuscript
### Last updated on 14 Jun 2024


#### preamble ----
### library setups
library(readxl)
library(tidyverse)
library(crosstable)
library(stringr)
library(ggpattern)

### data import
fulldata <- read_excel("raw-data/comm-inv_data_20231129.xlsx", sheet = "FINALISED")
fulldata$yearPub <- as.factor(fulldata$yearPub)

### colour palettes for graphs

comminvPalette <- c("Yes" = "#FFD700",
                    "No" = "#C19AC4")

yrPalette <- c("2019" = "#6699CC", 
               "2022" = "#CC9966")



lopPalette <- c("No involvement" = "#C0C0C0", 
                "Insufficient information" = "#996699", 
                "Informing" = "#996633", 
                "Consulting" = "#779966" , 
                "Engaging" = "#508080", 
                "Co-producing" = "#C19AC4", 
                "Community led" = "#CC9966", 
                "Community controlled" = "#6699CC")

mapPalette <- c("< 10" = "#ff0000",
                "10-20" = "#ff8c00", 
                "21-30" = "#ffcc00", 
                "> 30" = "#0055ff")


#### main analysis ----
# organised according to results section

### frequency of reporting community involvement (Fig S1) ----

# frequency of including community involvement statement and community involvement itself, and percentages grouped by year of publication
ciscifreq <- fulldata %>%
  group_by(yearPub, statementPresence, commInvPresence, artType) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n))

# frequency of community involvement and percentages grouped by year of publication
cifreq <- fulldata %>%
  group_by(yearPub, commInvPresence) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n))

cifreq$yearPub <- as.factor(cifreq$yearPub)

## bar plot (Figure S1)

cifreqplot <- ggplot(cifreq, aes(x = fct_relevel(yearPub, c("2022", "2019")),
                                 y = pct,
                                 fill = fct_relevel(commInvPresence, c("Yes", "No")),
                                 pattern = fct_relevel(commInvPresence, c("Yes", "No")),
                                 label = scales::percent(pct))) +
  geom_col_pattern(width = 0.5,
                   position = "dodge",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values = c("Yes" = "stripe", "No" = "none")) +
  scale_fill_manual(values = comminvPalette,
                    limits = c("No", "Yes")) +
  labs(x = "Year",
       y = "Percentages of articles",
       fill = "Community involvement") +
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size = 16),
        legend.position = "bottom") +
  guides(fill = guide_legend(override.aes = 
                                  list(
                                    pattern = c("none", "stripe"))),
         pattern = "none") +
  coord_flip()

cifreqplot

# export file (Figure 2)
ggsave(filename = "graphs/figs1_freq.png",
       plot = cifreqplot,
       scale = 1.5,
       width = 12,
       height = 8,
       units = "cm",
       dpi = 300)

## bar plot (INSAR poster)

cifreqplot <- ggplot(cifreq, aes(x = fct_relevel(yearPub, c("2022", "2019")), 
                                 y = pct, 
                                 fill = fct_relevel(commInvPresence, c("Yes", "No")),
                                 label = scales::percent(pct))) +
  geom_col(position = "dodge") +
  labs(x = "Year",
       y = "Percentages of articles",
       fill = "Community\ninvolvement") +
  scale_fill_manual(values = comminvPalette,
                    limits = c("No", "Yes")) +
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size = 18),
        legend.position = "right") +
  coord_flip()

cifreqplot

# export file (INSAR Fig 1)
ggsave(filename = "graphs/insar-fig1.png",
       plot = cifreqplot,
       scale = 0.5,
       width = 39.87,
       height = 14.56,
       units = "cm",
       dpi = 500)


### community members involved (Table 1) ----

# table 1 values (in bold and italics; broad coding scheme)

cmbroaddata <- fulldata %>%
  filter(commInvPresence == "Yes") %>%
  select(PMID, yearPub, c("Autistic People": "Insufficient information (community)")) %>%
  pivot_longer(cols = c("Autistic People",
                        "Families of Autistic People",
                        "Professionals",
                        "Administration",
                        "Insufficient information (community)"),
               names_to = "Community members",
               values_to = "Level of participation")


cmbroadtable <- cmbroaddata %>%
  filter(`Level of participation` != "No involvement") %>%
  group_by(yearPub, `Community members`) %>%
  summarise(noOfArticles = n())

totalarticles <- cmbroaddata %>%
  group_by(yearPub) %>%
  summarise(totalByYr = n_distinct(PMID))

cmbroadtable <- cmbroadtable %>%
  left_join(., totalarticles) %>%
  mutate(pct = (noOfArticles/totalByYr))


# table 1 values (not in bold or italics; detailed coding scheme)

cmdetaileddata <- fulldata %>%
  select(c(yearPub,"Autistic adults":"Cannot be determined")) %>%
  pivot_longer(cols = c("Autistic adults",
                        "Autistic young people",
                        "Autistic researchers",
                        "Researchers and parents of autistic people",
                        "Researchers and professionals",
                        "Parents of autistic adults",
                        "Parents of autistic young people",
                        "Family members (unspecified connection)",
                        "Parents of autistic people (unspecified age)",
                        "Community members (unspecified connection)",
                        "Clinicians/Healthcare providers",
                        "Educators",
                        "Professionals (unspecified specialty)",
                        "Policymakers",
                        "Funders",
                        "Patient/Advocacy organisations",
                        "Cannot be determined"),
               names_to = "Community members",
               values_to = "Level of participation")

cmdetailedtable <- cmdetaileddata %>%
  filter(`Level of participation` != "No involvement") %>%
  group_by(yearPub, `Community members`) %>%
  summarise(noOfArticles = n()) %>%
  left_join(., totalarticles) %>%
  mutate(pct = (noOfArticles/totalByYr))


#### Level of participation (Fig 2) ----

# frequency grouped by level of participation and year of publication 

lopdata <- fulldata %>%
  filter(commInvPresence == "Yes") %>%
  select(yearPub, levelOfPart) %>%
  group_by(yearPub, levelOfPart) %>%
  summarise(noOfArticles = n()) %>%
  left_join(., totalarticles) %>%
  mutate(pct = (noOfArticles/totalByYr))
  
lopnulldata <- data.frame(yearPub = c(2019, 2019, 2019, 2019),
                       levelOfPart = c("Community led", "Community controlled", "Engaging", "Informing"),
                       noOfArticles = c(0, 0, 0, 0),
                       totalByYr = c(12, 12, 12, 12),
                       pct = c(0, 0, 0, 0))
lopnulldata$yearPub <- as.factor(lopnulldata$yearPub)

lopdata <- lopdata %>%
  bind_rows(., lopnulldata)

## bar plot (Figure 2)
lopdataplot <- ggplot(lopdata, aes(x = fct_relevel(levelOfPart, c("Insufficient information", "Informing", "Consulting", "Engaging", "Co-producing", "Community led", "Community controlled")), 
                                   y = pct, 
                                   fill = fct_relevel(yearPub, c("2022", "2019")),
                                   pattern = fct_relevel(yearPub, c("2022", "2019")),
                                   label = scales::percent(pct))) +
  geom_col_pattern(width = 0.8,
                   position = "dodge",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values = c("2019" = "none", "2022" = "stripe")) +
  scale_fill_manual(values = yrPalette,
                    limits = c("2019", "2022")) +  
  labs(x = "Level of participation",
       y = "Percentages of articles",
       fill = "Year of publication") +
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size = 16),
        legend.position = "bottom") +
  guides(fill = guide_legend(override.aes = 
                               list(
                                 pattern = c("none", "stripe"))),
         pattern = "none") +
  coord_flip()

lopdataplot

# export file
ggsave(filename = "graphs/fig2_levelofpart.png",
       plot = lopdataplot,
       scale = 1.5,
       width = 14,
       height = 8,
       units = "cm",
       dpi = 300)


## bar plot (INSAR poster)
lopdataplot <- ggplot(lopdata, aes(x = fct_relevel(levelOfPart, c("Insufficient information", "Informing", "Consulting", "Engaging", "Co-producing", "Community led", "Community controlled")), 
                                   y = pct, 
                                   fill = reorder(yearPub, -pct),
                                   label = scales::percent(pct))) +
  geom_col(position = "dodge") +
  #  geom_text(position = position_dodge(width = 0.9),
  #            vjust = 0.1,
  #            hjust = -0.1,
  #            size = 3) +
  labs(x = "Level of\nparticipation",
       y = "Percentages of articles",
       fill = "Year of\npublication") +
  scale_fill_manual(values = yrPalette,
                    limits = c("2019", "2022")) +
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size = 18),
        legend.position = "right") +
  coord_flip()

lopdataplot

# export file (INSAR fig)
ggsave(filename = "graphs/insar_levelofpart.png",
       plot = lopdataplot,
       scale = 0.5,
       width = 39.87,
       height = 16.68,
       units = "cm",
       dpi = 500)


### breadth of community involvement (Table 2) ----

# table 2 values (in bold and italics; broad coding scheme)

boibroaddata <- fulldata %>%
  filter(commInvPresence == "Yes") %>%
  select(yearPub, c("Conceptualisation":"Insufficient information (breadth)")) %>%
  rename("Insufficient information" = "Insufficient information (breadth)") %>%
  pivot_longer(cols = c("Conceptualisation",
                        "Design",
                        "Implementation",
                        "Analysis",
                        "Dissemination",
                        "Insufficient information"),
               names_to = "Research activities",
               values_to = "Level of participation")

boibroadtable <- boibroaddata %>%
  filter(`Level of participation` != "No involvement") %>%
  group_by(yearPub, `Research activities`) %>%
  summarise(noOfArticles = n()) %>%
  left_join(., totalarticles) %>%
  mutate(pct = (noOfArticles/totalByYr))


# table 2 values (not in bold or italics; detailed coding scheme)
boidetaileddata <- fulldata %>%
  filter(commInvPresence == "Yes") %>%
  select(yearPub, c("Developing community-based theories of the research":"Insufficient information (narrow)")) %>%
  pivot_longer(cols = c("Developing community-based theories of the research", 
                        "Grant proposal writing", 
                        "Background research", 
                        "Designing study", 
                        "Choosing research methods", 
                        "Developing sampling procedures", 
                        "Recruiting study participants", 
                        "Developing/Adapting intervention", 
                        "Implementing the intervention", 
                        "Designing/Modifying interview and/or survey questions",
                        "Providing inputs on public-facing materials", 
                        "Collecting primary data", 
                        "Analysing data", 
                        "Interpreting study findings", 
                        "Writing reports/journal articles", 
                        "Giving presentations at meetings/conferences",
                        "Disseminating findings (approach unspecified)",
                        "Providing endorsement on project", 
                        "All aspects of research", 
                        "Insufficient information (narrow)"),
               names_to = "Research activities",
               values_to = "Level of participation")

boidetailedtable <- boidetaileddata %>%
  filter(`Level of participation` != "No involvement") %>%
  group_by(yearPub, `Research activities`) %>%
  summarise(noOfArticles = n()) %>%
  left_join(., totalarticles) %>%
  mutate(pct = (noOfArticles/totalByYr))
  

### community members (broad) X level of participation (Table S8 & Fig 3) ----

## create crosstable to calculate number and percentage of articles for each group of community members broken down by level of participation and year of publication (Table S8)

cmXloptable <- cmbroaddata %>%
  group_by(yearPub, `Level of participation`, `Community members`) %>%
  summarise(noOfArticles = n()) %>%
  left_join(., totalarticles) %>%
  mutate(pct = (noOfArticles/totalByYr))


## create panel diagram (Figure 4)
# filter out insufficient information for coding community members and level of participation and no involvement

cmXloptable <- cmXloptable %>%
  filter(`Level of participation` != "No involvement",
         `Level of participation` != "Insufficient information", 
         `Community members` != "Insufficient information (community)") %>%
  rename("levelOfPart" = "Level of participation",
         "commMem" = "Community members")

# create null data table
cmXlopnulldata <- data.frame(yearPub = c(2019, 2019, 2019, 2019,2019, 2019, 2019, 2019,2019, 2019, 2019, 2019),
                          levelOfPart = c("Co-producing", "Co-producing", "Co-producing", "Community controlled", "Community led", "Community led", "Community led", "Engaging", "Engaging", "Engaging", "Engaging", "Informing"),
                          commMem = c("Families of Autistic People", "Professionals", "Administration", "Autistic People", "Autistic People", "Families of Autistic People", "Professionals", "Autistic People", "Families of Autistic People", "Professionals", "Administration", "Autistic People"),
                          noOfArticles = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                          totalByYr = c(12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12),
                          pct = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
cmXlopnulldata$yearPub <- as.factor(cmXlopnulldata$yearPub)

cmXloptable <- cmXloptable %>%
  bind_rows(., cmXlopnulldata)

cmXloppanel <- ggplot(cmXloptable, aes(fill = reorder(yearPub, -pct),
                                       pattern = reorder(yearPub, -pct),
                                       y = pct,
                                       x = fct_relevel(commMem,c("Administration", "Professionals", "Families of Autistic People", "Autistic People")),
                                       label = scales::percent(pct))) +
  geom_col_pattern(width = 0.8,
                   position = "dodge",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.05,
                   pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values = c("2019" = "none", "2022" = "stripe")) +
  scale_fill_manual(values = yrPalette,
                    limits = c("2019", "2022")) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "bottom", 
        text = element_text(size = 16)) +
  labs(x = "Community members",
       y = "Percentage of articles",
       fill = "Year of publication") +
  guides(fill = guide_legend(override.aes = 
                               list(
                                 pattern = c("none", "stripe"))),
         pattern = "none") +
  coord_flip() +
  facet_wrap(~ levelOfPart, ncol = 3)

cmXloppanel

# export file
ggsave(filename = "graphs/fig3_cmXloppanel.png",
       plot = cmXloppanel,
       scale = 2,
       width = 15,
       height = 8,
       units = "cm",
       dpi = 300)

### frequency of community involvement by country (Table S9 & Figure 4) ----

# create a crosstable to look at country distribution overall (regardless of community involvement)
allcountrydata <- fulldata %>%
  select(yearPub, country) %>%
  group_by(yearPub, country) %>%
  summarise(noOfArticles = n())
  
# create a crosstable to look at country distribution of papers reporting community involvement broken by year of publication (Table S9)
countrytableByYr <- fulldata %>%
  filter(commInvPresence == "Yes") %>%
  select(yearPub, country) %>%
  group_by(yearPub, country) %>%
  summarise(noOfArticles = n()) %>%
  left_join(., totalarticles) %>%
  mutate(pct = noOfArticles/totalByYr)

# join all country totals with country with comm inv

countrytableByYr <- countrytableByYr %>%
  inner_join(y = allcountrydata, by = c("yearPub", "country")) %>%
  rename(yesCommInv = noOfArticles.x,
         countryArticles = noOfArticles.y) %>%
  mutate(pctCountry = yesCommInv/countryArticles)

# create a map plot to visualise country data (Figure 5a and 5b)

countrytableByYr$count_group <- cut(countrytableByYr$yesCommInv, 
                                breaks = c(-Inf, 10, 20, 30, Inf), 
                                labels = c("< 10", "10-20", "21-30", "> 30"))

world_map <- map_data(map = "world")

#Figure 5a (2019)

countrytable2019 <- countrytableByYr %>%
  filter(yearPub == 2019)

countrytable2019$count_group <- cut(countrytable2019$yesCommInv, 
                                breaks = c(-Inf, 10, 20, 30, Inf), 
                                labels = c("< 10", "10-20", "21-30", "> 30"))

countryplot2019 <- ggplot(countrytable2019) +
  geom_map(aes(map_id = country, fill = fct_rev(count_group)), map = world_map) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'black', fill = NA) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  scale_fill_manual(values = mapPalette,
                    limits = c("< 10", "10-20", "21-30", "> 30")) +
  theme_void() +
  theme(panel.border = element_blank()) +
  coord_fixed() +
  theme(legend.position = "bottom") +
  labs(fill = "2019",
       title = "A")

countryplot2019

#Figure 5b (2022)

countrytable2022 <- countrytableByYr %>%
  filter(yearPub == 2022)

countrytable2022$count_group <- cut(countrytable2022$yesCommInv, 
                                    breaks = c(-Inf, 10, 20, 30, Inf), 
                                    labels = c("< 10", "10-20", "21-30", "> 30"))

countryplot2022 <- ggplot(countrytable2022) +
  geom_map(aes(map_id = country, fill = fct_rev(count_group)), map = world_map) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'black', fill = NA) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  scale_fill_manual(values = mapPalette,
                    limits = c("< 10", "10-20", "21-30", "> 30")) +
  theme_void() +
  theme(panel.border = element_blank()) +
  coord_fixed() +
  theme(legend.position = "bottom") +
  labs(fill = "2022",
       title = "B")

countryplot2022

countryplot <- ggpubr::ggarrange(countryplot2019, countryplot2022, ncol = 1, nrow = 2)
countryplot
# export file
ggsave(filename = "graphs/fig5_country.png",
       plot = countryplot,
       scale = 1.5,
       width = 8,
       height = 10,
       units = "cm",
       dpi = 300,
       bg = "white")

### frequency of community involvement by research areas (Table 3, Table S10 & Figure 6[relegated]) ----

# create crosstable to calculate number and percentage of articles for each research area by year of publication for all articles including those without community involvement (Table 3)

allradata <- fulldata %>%
  select(yearPub, researchArea) %>%
  group_by(yearPub, researchArea) %>%
  summarise(noOfArticles = n()) %>%
  mutate(pct = (noOfArticles/sum(noOfArticles)))

# create crosstable to calculate number and percentage of articles that included comm involvement broken down by the year of publication (Table 3)

radata <- fulldata %>%
  filter(commInvPresence == "Yes") %>%
  select(yearPub, researchArea) %>%
  group_by(yearPub, researchArea) %>%
  summarise(noOfArticles = n()) %>%
  left_join(., totalarticles) %>%
  mutate(pct = (noOfArticles/totalByYr))


# create crosstable to calculate number and percentage of articles for each research area broken down by level of participation and year of publication (Table S10)

raXlopdata <- fulldata %>%
  filter(commInvPresence == "Yes") %>%
  select(yearPub, researchArea, levelOfPart) %>%
  group_by(yearPub, levelOfPart, researchArea) %>%
  summarise(noOfArticles = n()) %>%
  left_join(., totalarticles) %>%
  mutate(pct = (noOfArticles/totalByYr))


### frequency of community involvement by research methods (Table 4) ----

# create crosstable to calculate number and percentage of articles for each research method by year of publication for all articles including those without community involvement (Table 4)


allrmdata <- fulldata %>%
  select(yearPub, methodType) %>%
  group_by(yearPub, methodType) %>%
  summarise(noOfArticles = n()) %>%
  mutate(pct = (noOfArticles/sum(noOfArticles)))

# create crosstable to calculate number and percentage of articles that included comm inv for each research method broken down by year of publication 

rmdata <- fulldata %>%
  filter(commInvPresence == "Yes") %>%
  select(yearPub, methodType) %>%
  group_by(yearPub, methodType) %>%
  summarise(noOfArticles = n()) %>%
  left_join(., totalarticles) %>%
  mutate(pct = (noOfArticles/totalByYr))


# # create crosstable to calculate number and percentage of articles for each research area broken down by level of participation and year of publication 
# 
# rmXlopdata <- fulldata %>%
#   filter(commInvPresence == "Yes") %>%
#   select(yearPub, methodType, levelOfPart) %>%
#   group_by(yearPub, levelOfPart, methodType) %>%
#   summarise(noOfArticles = n()) %>%
#   left_join(., totalarticles) %>%
#   mutate(pct = (noOfArticles/totalByYr))



### frequency of community involvement by funders (Table 5) ----

# create crosstable to calculate number and percentage of articles for each funder type by year of publication for all articles including those without community involvement (Table 5)

allfunddata <- fulldata %>%
  select(yearPub, c("Industry": "None")) %>%
  pivot_longer(cols = c("Industry",
                        "Government",
                        "University",
                        "Trust",
                        "Service providers",
                        "None"),
               names_to = "funderType",
               values_to = "levelOfPart") %>%
  filter(levelOfPart != "No involvement") %>%
  group_by(yearPub, funderType) %>%
  summarise(noOfArticles = n()) %>%
  mutate(pct = (noOfArticles/sum(noOfArticles)))


# create crosstable to calculate number and percentage of articles that included comm inv for each funder type broken down by year of publication (Table 5)

funddata <- fulldata %>%
  filter(commInvPresence == "Yes") %>%
  select(yearPub, c("Industry": "None")) %>%
  pivot_longer(cols = c("Industry",
                        "Government",
                        "University",
                        "Trust",
                        "Service providers",
                        "None"),
               names_to = "funderType",
               values_to = "levelOfPart") %>%
  filter(levelOfPart != "No involvement") %>%
  group_by(yearPub, funderType) %>%
  summarise(noOfArticles = n()) %>%
  left_join(., totalarticles) %>%
  mutate(pct = (noOfArticles/totalByYr))



# ## ******[RELEGATED] ----
# ## create panel diagram (Figure 6)
# # filter out insufficient information for coding level of participation
# 
# raXlopdata <- raXlopdata %>%
#   filter(levelOfPart != "Insufficient information") 
# 
# 
# # create null data table
# raXlopnulldata <- data.frame(yearPub = c(2022, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019),
#                              levelOfPart = c("Co-producing", "Co-producing", "Co-producing", "Co-producing", "Co-producing", "Community controlled", "Community controlled", "Community controlled", "Community led", "Community led", "Consulting", "Engaging", "Engaging", "Informing"),
#                               researchArea = c("Infrastructure and surveillance", "Biology", "Treatments and interventions", "Services", "Lifespan issues", "Treatments and interventions", "Lifespan issues", "Infrastructure and surveillance", "Services", "Lifespan issues", "Screening and diagnosis", "Treatments and interventions", "Services", "Lifespan issues"),
#                              noOfArticles = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
#                              totalByYr = c(12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12),
#                              pct = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
# raXlopnulldata$yearPub <- as.factor(raXlopnulldata$yearPub)
# 
# raXlopdata <- raXlopdata %>%
#   bind_rows(., raXlopnulldata)
# 
# raXloppanel <- ggplot(raXlopdata, aes(x = fct_relevel(researchArea, c("Infrastructure and surveillance", "Lifespan issues","Services", "Treatments and interventions", "Risk factors", "Biology", "Screening and diagnosis")), 
#                                    y = pct, 
#                                    fill = reorder(yearPub,pct),
#                                    lavel = scales::percent(pct))) +
#   geom_col(position = "dodge") +
#   labs(x = "Research areas",
#        y = "Percentage of articles",
#        fill = "Year of publication") +
#   scale_fill_manual(values = yrPalette,
#                     limits = c("2019", "2022")) +
#   scale_y_continuous(labels = scales::percent) +
#   theme(text = element_text(size = 16),
#         legend.position = "bottom") +
#   coord_flip() +
#   facet_wrap(~ levelOfPart, ncol = 3)
# 
# raXloppanel
# 
# 
# # export file
# ggsave(filename = "graphs/fig6_raXloppanel.png",
#        plot = raXloppanel,
#        scale = 2,
#        width = 15,
#        height = 8,
#        units = "cm",
#        dpi = 300)
