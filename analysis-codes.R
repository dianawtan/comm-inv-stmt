#### Analysis for community involvement statement project
### last updated on 29 Nov 2023

### library setups
library(readxl)
library(tidyverse)
library(crosstable)


### data import
fulldata <- read_excel("raw-data/comm-inv_data_20231129.xlsx", sheet = "FINALISED")

### colour blind friendly palette

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

mapPalette <- c("< 10" = "#00CED1",
                "10-20" = "#FFD700", 
                "21-30" = "#FF6F61", 
                "> 30" = "#228B22")

### main analyses

#### community involvement frequency before vs after ----

## frequency values
#cifreq <- fulldata %>%
#  select("yearPub", "commInvPresence") %>%
#  table() %>%
#  as.data.frame()

## frequency and percentages grouped by yearPub
cifreq <- fulldata %>%
  group_by(yearPub, commInvPresence) %>%
  summarise(n = n()) %>%
  mutate(pct = n/sum(n))

cifreq$yearPub <- as.factor(cifreq$yearPub)

## bar plot

cifreqplot <- ggplot(cifreq, aes(x = fct_relevel(yearPub, c("2022", "2019")), 
                                 y = pct, 
                                 fill = fct_relevel(commInvPresence, c("Yes", "No")),
                                 label = scales::percent(pct))) +
  geom_col(position = "dodge") +
  geom_text(position = position_dodge(width = 0.9),
            vjust = 0,
            hjust = -0.2,
            size = 3) +
  labs(x = "Year",
       y = "Percentages of articles",
       fill = "Community involvement") +
  scale_fill_manual(values = comminvPalette,
                    limits = c("No", "Yes")) +
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(size = 12),
        legend.position = "bottom") +
  coord_flip()

cifreqplot

# export file
ggsave(filename = "graphs/freq.png",
       plot = cifreqplot,
       scale = 2,
       width = 6,
       height = 4,
       units = "cm",
       dpi = 300)


#### level of participation ----

## frequency values

lopdata <- fulldata %>%
  select("levelOfPart", "yearPub") %>%
  filter(levelOfPart != "No community involvement") %>%
  table() %>%
  as.data.frame()

## bar plot
lopdataplot <- ggplot(lopdata, aes(x = fct_relevel(levelOfPart, c("Insufficient information", "Informing", "Consulting", "Engaging", "Co-producing", "Community led", "Community controlled")), 
                                 y = Freq, 
                                 fill = reorder(yearPub, -Freq))) +
  geom_col(position = "dodge") +
  labs(x = "Level of participation",
       y = "Number of articles",
       fill = "Year of publication") +
  scale_fill_manual(values = yrPalette,
                    limits = c("2019", "2022")) +
  theme(text = element_text(size = 12),
        legend.position = "bottom") +
  coord_flip()

lopdataplot

# export file
ggsave(filename = "graphs/levelofpart.png",
       plot = lopdataplot,
       scale = 2,
       width = 6,
       height = 4,
       units = "cm",
       dpi = 300)


#### breadth of participation (detailed coding scheme) X level of participation (stacked bar chart) ----

## create long table
bopdatalong <- fulldata %>%
  select(c("Developing community-based theories of the research":"Insufficient information (narrow)")) %>%
  rename("Insufficient information" = "Insufficient information (narrow)") %>%
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
                        "Insufficient information"),
               names_to = "Research activities",
               values_to = "Level of participation") %>%
  filter(`Level of participation` != "No involvement")


##create cross table to calculate number of articles for each breadth of involvement  broken down by level of participation
boptab <- crosstable(bopdatalong, "Research activities", by = "Level of participation",
                             percent_pattern = "{n}") 
boptab <- as.data.frame(boptab)

##create a long table
bopdatalong <-  boptab %>%
  pivot_longer(cols = c("Informing", "Consulting", "Engaging", "Co-producing", "Community led", "Community controlled", "Insufficient information"),
               names_to = "Level of participation",
               values_to = "Count") %>%
  select(c("variable", "Level of participation", "Count")) %>%
  rename("Research activities" = "variable")

bopdatalong$Count <- as.numeric(bopdatalong$Count)

## create stacked bar chart

bopplot <- ggplot(bopdatalong, aes(x = fct_relevel(`Research activities`, c("Insufficient information", "All aspects of research", "Providing endorsement on project", "Disseminating findings (approach unspecified)", "Giving presentations at meetings/conferences", "Writing reports/journal articles", "Interpreting study findings", "Analysing data", "Collecting primary data", "Providing inputs on public-facing materials", "Designing/Modifying interview and/or survey questions", "Implementing the intervention", "Developing/Adapting intervention", "Recruiting study participants", "Developing sampling procedures", "Choosing research methods", "Designing study", "Background research", "Grant proposal writing", "Developing community-based theories of the research")), y = Count, fill = fct_relevel(`Level of participation`, c("Insufficient information", "Informing", "Consulting", "Engaging", "Co-producing", "Community led", "Community controlled")))) +
  geom_col() +
  scale_fill_manual(values = lopPalette) +
  labs(x = "Research activities (detailed)",
       y = "Number of articles",
       fill = "Level of participation") +
  theme(text = element_text(size = 12),
        legend.position = "bottom") +
  coord_flip()

bopplot 

# export file
ggsave(filename = "graphs/resact_detailed.png",
       plot = bopplot,
       scale = 2,
       width = 15,
       height = 8,
       units = "cm",
       dpi = 300)

#### breadth of participation (broad coding scheme) X level of participation (panel diagram) ----

bopdatalong2 <- fulldata %>%
  select(yearPub, c("Conceptualisation":"Insufficient information (breadth)")) %>%
  rename("Insufficient information" = "Insufficient information (breadth)") %>%
  pivot_longer(cols = c("Conceptualisation",
                        "Design",
                        "Implementation",
                        "Analysis",
                        "Dissemination",
                        "Insufficient information"),
               names_to = "Research activities",
               values_to = "Level of participation") %>%
  filter(`Level of participation` != "No involvement")


#create cross table to calculate number of articles for each breadth of involvement  broken down by level of participation & year of pub
boptab2 <- crosstable(bopdatalong2, "Research activities", by = c("Level of participation", "yearPub"),
                              percent_pattern = "{n}") 

bopdatalong2 <-  boptab2 %>%
  pivot_longer(cols = c("Level of participation=Informing & yearPub=2019", 
                        "Level of participation=Consulting & yearPub=2019", 
                        "Level of participation=Engaging & yearPub=2019", 
                        "Level of participation=Co-producing & yearPub=2019", 
                        "Level of participation=Community led & yearPub=2019", 
                        "Level of participation=Community controlled & yearPub=2019", 
                        "Level of participation=Insufficient information & yearPub=2019",
                        "Level of participation=Informing & yearPub=2022", 
                        "Level of participation=Consulting & yearPub=2022", 
                        "Level of participation=Engaging & yearPub=2022", 
                        "Level of participation=Co-producing & yearPub=2022", 
                        "Level of participation=Community led & yearPub=2022", 
                        "Level of participation=Community controlled & yearPub=2022", 
                        "Level of participation=Insufficient information & yearPub=2022"),
               names_to = "Level of participation",
               values_to = "Count") %>%
  select(c("variable", "Level of participation", "Count")) %>%
  rename("Research activities" = "variable")

bopdatalong2$Count <- as.numeric(bopdatalong2$Count)

#delimiting column & cleaning cell values

bopdatalong2 <- separate_wider_delim(bopdatalong2, cols = `Level of participation`, delim = " & ", names = c("Level of participation", "Year of publication"))

bopdatalong2$`Level of participation` <- gsub("Level of participation=","",bopdatalong2$`Level of participation`)

bopdatalong2$`Year of publication` <- gsub("yearPub=","",bopdatalong2$`Year of publication`)

#bopdatalong2 <- mutate(bopdatalong2, `Level of participation` = factor(`Level of participation`, levels = c("Community controlled","Community led","Co-producing","Engaging","Consulting","Informing","Insufficient information")))

# create panel diagram

boppanel <- ggplot(bopdatalong2, aes(fill = reorder(`Year of publication`, -Count),
                                     y = Count, 
                                     x = fct_relevel(`Research activities`, c("Insufficient information", "Dissemination", "Analysis", "Implementation", "Design", "Conceptualisation")))) +
  geom_bar(stat = "identity") +
  theme(legend.position = "bottom", 
        text = element_text(size = 16)) +
  labs(x = "Research activities (broad)",
       y = "Number of articles",
       fill = "Year of publication") +
  scale_fill_manual(values = yrPalette,
                    limits = c("2019", "2022")) +
  coord_flip() +
  facet_wrap(~ `Level of participation`, ncol = 3)

boppanel

# export file
ggsave(filename = "graphs/resact-level_panel.png",
       plot = boppanel,
       scale = 2,
       width = 15,
       height = 8,
       units = "cm",
       dpi = 300)


#### community members (detailed coding scheme) X level of participation (stacked bar chart)----

# create a long table
cmdata <- fulldata %>%
  select(c("Autistic adults":"Cannot be determined")) %>%
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


##create cross table to calculate number of articles for each group of community member broken down by level of participation

cmtab <- crosstable(cmdata, "Community members", by = "Level of participation",
                          percent_pattern = "{n}") %>%
  select(-c(".id","label"))

cmdata <- cmtab %>%
  pivot_longer(cols = c("No involvement", "Informing", "Consulting", "Engaging", "Co-producing", "Community led", "Community controlled", "Insufficient information"),
               names_to = "Level of participation",
               values_to = "Count") %>%
  filter(`Level of participation` != "No involvement") %>%
  rename("Community members" = "variable")
cmdata$Count <- as.numeric(cmdata$Count)


#create stacked bar chart
cmplot <- ggplot(cmdata, aes(x = fct_relevel(`Community members`, c("Cannot be determined","Patient/Advocacy organisations","Funders","Policymakers","Professionals (unspecified specialty)","Educators","Clinicians/Healthcare providers","Community members (unspecified connection)","Parents of autistic people (unspecified age)","Family members (unspecified connection)","Parents of autistic young people","Parents of autistic adults","Researchers and professionals","Researchers and parents of autistic people","Autistic researchers","Autistic young people","Autistic adults")), 
                             y = Count, 
                             fill = fct_relevel(`Level of participation`, c("Insufficient information", "Informing", "Consulting", "Engaging", "Co-producing", "Community led", "Community controlled")))) +
  geom_col() +
  scale_fill_manual(values = lopPalette) +
  theme(legend.position = "bottom", 
        text = element_text(size = 12)) +
  labs(x = "Community members (detailed)",
       y = "Number of articles",
       fill = "Level of participation") +
  coord_flip()

cmplot

# export file
ggsave(filename = "graphs/commem_detailed.png",
       plot = cmplot,
       scale = 2,
       width = 15,
       height = 8,
       units = "cm",
       dpi = 300)


#### community members (broad coding scheme) X level of participation (panel diagram) ----

# create a long table

cmdata2 <- fulldata %>%
  select(yearPub, c("Autistic People":"Insufficient information (community)")) %>%
  rename("Insufficient information" = "Insufficient information (community)") %>%
  pivot_longer(cols = c("Autistic People",
                        "Families of Autistic People",
                        "Professionals",
                        "Administration",
                        "Insufficient information"),
               names_to = "Community members",
               values_to = "Level of participation") 


#create cross table to calculate number of articles for each group of community member broken down by level of participation and year of pub

cmtab2 <- crosstable(cmdata2, "Community members", by = c("Level of participation", "yearPub"),
                          percent_pattern = "{n}") %>%
  select(-c(".id", "label"))

# create a long table

cmdata2 <-  cmtab2 %>%
  pivot_longer(cols = c("Level of participation=Informing & yearPub=2019", 
                        "Level of participation=Consulting & yearPub=2019", 
                        "Level of participation=Engaging & yearPub=2019", 
                        "Level of participation=Co-producing & yearPub=2019", 
                        "Level of participation=Community led & yearPub=2019", 
                        "Level of participation=Community controlled & yearPub=2019", 
                        "Level of participation=Insufficient information & yearPub=2019",
                        "Level of participation=Informing & yearPub=2022", 
                        "Level of participation=Consulting & yearPub=2022", 
                        "Level of participation=Engaging & yearPub=2022", 
                        "Level of participation=Co-producing & yearPub=2022", 
                        "Level of participation=Community led & yearPub=2022", 
                        "Level of participation=Community controlled & yearPub=2022", 
                        "Level of participation=Insufficient information & yearPub=2022"),
               names_to = "Level of participation",
               values_to = "Count") %>%
  select(c("variable", "Level of participation", "Count")) %>%
  rename("Community members" = "variable")

cmdata2$Count <- as.numeric(cmdata2$Count)


#delimiting column and cleaning up cells

cmdata2 <- separate_wider_delim(cmdata2, cols = `Level of participation`, delim = " & ", names = c("Level of participation", "Year of publication"))
cmdata2$`Level of participation` <- gsub("Level of participation=","",cmdata2$`Level of participation`)
cmdata2$`Year of publication` <- gsub("yearPub=","",cmdata2$`Year of publication`)

# create panel diagram

cmpanel <- ggplot(cmdata2, aes(fill = reorder(`Year of publication`, -Count), 
                               y = Count, 
                               x = fct_relevel(`Community members`,c("Insufficient information", "Administration", "Professionals", "Families of Autistic People", "Autistic People")))) +
  geom_col() +
  theme(legend.position = "bottom", 
        text = element_text(size = 16)) +
  labs(x = "Community members (broad)",
       y = "Number of articles",
       fill = "Year of publication") +
  scale_fill_manual(values = yrPalette,
                    limits = c("2019", "2022")) +
  coord_flip() +
  facet_wrap(~ `Level of participation`, ncol = 3)

cmpanel

# export file
ggsave(filename = "graphs/commem-level_panel.png",
       plot = cmpanel,
       scale = 2,
       width = 15,
       height = 8,
       units = "cm",
       dpi = 300)


#### research areas X level of participation (panel diagram) ----

# create a long table

resdata <- fulldata %>%
  select(yearPub, researchArea, levelOfPart) %>%
  filter(levelOfPart != "No community involvement") 


#create cross table to calculate number of articles for each research area broken down by level of participation and year of pub

restab <- crosstable(resdata, "researchArea", by = c("levelOfPart", "yearPub"),
                     percent_pattern = "{n}") %>%
  select(-c(".id", "label"))

# create a long table

resdata <-  restab %>%
  pivot_longer(cols = c("levelOfPart=Informing & yearPub=2019", 
                        "levelOfPart=Consulting & yearPub=2019", 
                        "levelOfPart=Engaging & yearPub=2019", 
                        "levelOfPart=Co-producing & yearPub=2019", 
                        "levelOfPart=Community led & yearPub=2019", 
                        "levelOfPart=Community controlled & yearPub=2019", 
                        "levelOfPart=Insufficient information & yearPub=2019",
                        "levelOfPart=Informing & yearPub=2022", 
                        "levelOfPart=Consulting & yearPub=2022", 
                        "levelOfPart=Engaging & yearPub=2022", 
                        "levelOfPart=Co-producing & yearPub=2022", 
                        "levelOfPart=Community led & yearPub=2022", 
                        "levelOfPart=Community controlled & yearPub=2022", 
                        "levelOfPart=Insufficient information & yearPub=2022"),
               names_to = "Level of participation",
               values_to = "Count") %>%
  select(c("variable", "Level of participation", "Count")) %>%
  rename("Research areas" = "variable")


#delimiting column and cleaning up cells

resdata <- separate_wider_delim(resdata, cols = `Level of participation`, delim = " & ", names = c("Level of participation", "Year of publication"))
resdata$`Level of participation` <- gsub("levelOfPart=","",resdata$`Level of participation`)
resdata$`Year of publication` <- gsub("yearPub=","",resdata$`Year of publication`)
resdata$Count <- as.numeric(resdata$Count)

# create panel diagram

respanel <- ggplot(resdata, aes(x = fct_relevel(`Research areas`, c("Infrastructure and surveillance", "Lifespan issues","Services", "Treatments and interventions", "Risk factors", "Biology", "Screening and diagnosis")), 
                                y = Count, 
                                fill = reorder(`Year of publication`,-Count))) +
  geom_col() +
  labs(x = "Research areas",
       y = "Number of articles",
       fill = "Year of publication") +
  scale_fill_manual(values = yrPalette,
                    limits = c("2019", "2022")) +
  theme(text = element_text(size = 16),
        legend.position = "bottom") +
  coord_flip() +
  facet_wrap(~ `Level of participation`, ncol = 3)

respanel  

# export file
ggsave(filename = "graphs/res-level_panel.png",
       plot = respanel,
       scale = 2,
       width = 15,
       height = 8,
       units = "cm",
       dpi = 300)


#### methods X level of participation (panel diagram) ----

# create a long table

methdata <- fulldata %>%
  select(yearPub, methodType, levelOfPart) %>%
  filter(levelOfPart != "No community involvement") 


#create cross table to calculate number of articles for each research area broken down by level of participation and year of pub

methtab <- crosstable(methdata, "methodType", by = c("levelOfPart", "yearPub"),
                     percent_pattern = "{n}") %>%
  select(-c(".id", "label"))

# create a long table

methdata <-  methtab %>%
  pivot_longer(cols = c("levelOfPart=Informing & yearPub=2019", 
                        "levelOfPart=Consulting & yearPub=2019", 
                        "levelOfPart=Engaging & yearPub=2019", 
                        "levelOfPart=Co-producing & yearPub=2019", 
                        "levelOfPart=Community led & yearPub=2019", 
                        "levelOfPart=Community controlled & yearPub=2019", 
                        "levelOfPart=Insufficient information & yearPub=2019",
                        "levelOfPart=Informing & yearPub=2022", 
                        "levelOfPart=Consulting & yearPub=2022", 
                        "levelOfPart=Engaging & yearPub=2022", 
                        "levelOfPart=Co-producing & yearPub=2022", 
                        "levelOfPart=Community led & yearPub=2022", 
                        "levelOfPart=Community controlled & yearPub=2022", 
                        "levelOfPart=Insufficient information & yearPub=2022"),
               names_to = "Level of participation",
               values_to = "Count") %>%
  select(c("variable", "Level of participation", "Count")) %>%
  rename("Research methods" = "variable")


#delimiting column and cleaning up cells

methdata <- separate_wider_delim(methdata, cols = `Level of participation`, delim = " & ", names = c("Level of participation", "Year of publication"))
methdata$`Level of participation` <- gsub("levelOfPart=","",methdata$`Level of participation`)
methdata$`Year of publication` <- gsub("yearPub=","",methdata$`Year of publication`)
methdata$Count <- as.numeric(methdata$Count)

# create panel diagram

methpanel <- ggplot(methdata, aes(x = fct_relevel(`Research methods`, c("Review", "Mixed methods", "Quantitative", "Qualitative")), 
                                y = Count, 
                                fill = reorder(`Year of publication`,-Count))) +
  geom_col() +
  labs(x = "Research methods",
       y = "Number of articles",
       fill = "Year of publication") +
  scale_fill_manual(values = yrPalette,
                    limits = c("2019", "2022")) +
  theme(text = element_text(size = 16),
        legend.position = "bottom") +
  coord_flip() +
  facet_wrap(~ `Level of participation`, ncol = 3)

methpanel  

# export file
ggsave(filename = "graphs/meth-level_panel.png",
       plot = methpanel,
       scale = 2,
       width = 15,
       height = 8,
       units = "cm",
       dpi = 300)



#### funders X level of participation (panel diagram) ----

# create a long table

funddata <- fulldata %>%
  select(yearPub, c("Industry":"None")) %>%
  pivot_longer(cols = c("Industry",
                        "Government",
                        "University",
                        "Trust",
                        "Service providers",
                        "None"),
               names_to = "Type of funders",
               values_to = "Level of participation") %>%
  filter(`Level of participation` != "No involvement")
 

#create cross table to calculate number of articles for each type of funder broken down by level of participation and year of pub

fundtab <- crosstable(funddata, "Type of funders", by = c("Level of participation", "yearPub"),
                     percent_pattern = "{n}") %>%
  select(-c(".id", "label"))

# create a long table

funddata <-  fundtab %>%
  pivot_longer(cols = c("Level of participation=Informing & yearPub=2019", 
                        "Level of participation=Consulting & yearPub=2019", 
                        "Level of participation=Engaging & yearPub=2019", 
                        "Level of participation=Co-producing & yearPub=2019", 
                        "Level of participation=Community led & yearPub=2019", 
                        "Level of participation=Community controlled & yearPub=2019", 
                        "Level of participation=Insufficient information & yearPub=2019",
                        "Level of participation=Informing & yearPub=2022", 
                        "Level of participation=Consulting & yearPub=2022", 
                        "Level of participation=Engaging & yearPub=2022", 
                        "Level of participation=Co-producing & yearPub=2022", 
                        "Level of participation=Community led & yearPub=2022", 
                        "Level of participation=Community controlled & yearPub=2022", 
                        "Level of participation=Insufficient information & yearPub=2022"),
               names_to = "Level of participation",
               values_to = "Count") %>%
  select(c("variable", "Level of participation", "Count")) %>%
  rename("Type of funders" = "variable")
funddata$Count <- as.numeric(funddata$Count)


#delimiting column and cleaning up cells

funddata <- separate_wider_delim(funddata, cols = `Level of participation`, delim = " & ", names = c("Level of participation", "Year of publication"))
funddata$`Level of participation` <- gsub("Level of participation=","",funddata$`Level of participation`)
funddata$`Year of publication` <- gsub("yearPub=","",funddata$`Year of publication`)

# create panel diagram

fundplot <- ggplot(funddata, aes(fill = reorder(`Year of publication`, -Count), 
                               y = Count, 
                               x = fct_relevel(`Type of funders`,c("None", "Service providers", "Trust", "University", "Government", "Industry")))) +
  geom_col() +
  theme(legend.position = "bottom", 
        text = element_text(size = 16)) +
  labs(x = "Funders",
       y = "Number of articles",
       fill = "Year of publication") +
  scale_fill_manual(values = yrPalette,
                    limits = c("2019", "2022")) +
  coord_flip() +
  facet_wrap(~ `Level of participation`, ncol = 3)

fundplot

# export file
ggsave(filename = "graphs/fund-level_panel.png",
       plot = fundplot,
       scale = 2,
       width = 15,
       height = 8,
       units = "cm",
       dpi = 300)


#### frequency of community involvement by country ----
country <- fulldata %>%
  filter(commInvPresence == "Yes") %>%
  select(country)

countrytable <- crosstable(country, "country", percent_pattern = "{n}") 
countrytable$value <- as.numeric(countrytable$value)

countrytable$count_group <- cut(countrytable$value, 
                                breaks = c(-Inf, 10, 20, 30, Inf), 
                                labels = c("< 10", "10-20", "21-30", "> 30"))

world_map <- map_data(map = "world")

countryplot <- ggplot(countrytable) +
  geom_map(aes(map_id = variable, fill = fct_rev(count_group)), map = world_map) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'black', fill = NA) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  scale_fill_manual(values = mapPalette,
                    limits = c("< 10", "10-20", "21-30", "> 30")) +
  theme_void() +
  theme(plot.background = element_rect(fill = "lightgrey", size = 0),
        panel.border = element_blank()) +
  coord_fixed() +
  theme(legend.position = "bottom") +
  labs(fill = "Frequency")

countryplot

# export file
ggsave(filename = "graphs/country_panel.png",
       plot = countryplot,
       scale = 1.5,
       width = 15,
       height = 8,
       units = "cm",
       dpi = 300)
