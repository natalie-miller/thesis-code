# Natalie Miller
# Code for Thesis -- Analyzing data and creating graphic representations
# Speech Interval Code

# set working directory

setwd("~/Google Drive/UT - ALL/Thesis/Stats Stuff")

#### combine data ####
library(tidyverse)
aq <- read_csv("all_questions.csv")
as <- read_csv("all_statements.csv")

# combine questions and statements to one table
everything <- aq %>% 
  bind_rows(as)

# show how many items are in each context for each level
table(everything$context, everything$lev)


#### Aggregate data for ANOVA ####

# Checking the data, how many entries for each context, and each level
table(everything$context_f)
table(everything$lev_f)

# Pull from everything a table (lev_temp) that has all of the level 1 declarative sentences for subject #1
everything %>% 
   filter(subj == 1 & context_f == "d" & lev_f == 1) ->
   lev_temp

# Aggregate, combine data
# Takes the average value of 9 responses for each level by each participant
# Basically collapses 9 responses per stimulus to one avg value; 81 responses per participant down to 9 rows

agg <- everything %>% 
  group_by(subj, context, lev) %>% 
  summarise(resp = mean(resp, na.rm=TRUE),
            exp = first(exp)) 

# Confirm that variable 'agg' now correctly has 9 rows per participant
# Utilizing musician or non-musician coding within the columns

agg %>% 
  group_by(subj) %>% 
  select(exp, context) %>% 
  table()

## Run the RM ANOVA with the aggregated data
## Between variable = experience (musician/non-musician)
## Within variables = context (declarative vs interrogative) and level (stimuli 1-9)

names(agg)
library(afex)
agg_mod <- aov_ez (id = "subj",
                  dv = "resp",
                  between = c("exp"),
                  within = c("context", "lev"),
                  data = agg,)

## Show a summary of the ANOVA results
summary(agg_mod)

## Show all ANOVA results
agg_mod




##### GRAPHING #####

### Finding statistical information of the responses, by calling the library "emmeans"
### Need the information in order to create box plots for the graphics

library(emmeans)

##Find the statistics dependent on level, regardless of context
emmeans(agg_mod, "lev") 

##Find the statistics dependent on level, sort by context
emmeans(agg_mod, "lev", by="context")

## Goes through and compares each level to another one. Seems pointless b/c this was already looked at within the ANOVA (in a better way)
pairs(emmeans(agg_mod, "lev", by="context")) 


### Creating graphs ###

## Create variable 'gr_resp_data' that includes the statistics we ran earlier (dependent on the levels, sorting by context)
gr_resp_data <- summary(emmeans(agg_mod, "lev", by="context"))
  
## Pull the declarative responses from above variable
## Graph average naturalness ratings (resp) as a bar chart

gr_resp_data %>% 
  filter(context == "d") %>% ##Select jus the declarative responses 
  ggplot( aes(x=lev, y=emmean)) + ##x-axis is level, y is resp
  geom_bar(stat="identity", fill = "blue") + ##bar-chart, color blue
  labs(title="Declarative Sentences") + ##label - title
  theme_bw() ##black and white theme


## Pull the interrogative responses from the above variable
## Graph average naturalness ratings (resp) as a bar chart

gr_resp_data %>% 
  filter(context == "i") %>% ##select interrogative responses
  ggplot( aes(x=lev, y=emmean)) + ##x-axis is level, y is resp
  geom_bar(stat="identity", fill = "red") + ##bar-chart, color red
  labs(title="Interogative Sentences") + ##label - title
  theme_bw() ##black and white theme



## Graph all average naturalness ratings (resp) for both interrogative and declarative as a bar chart

ggplot(gr_resp_data, aes(x=lev, y=emmean, fill=context)) +
  geom_bar(stat="identity", fill = "gray") +
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), width=0.2) + #add error bars
  facet_grid(~context) + #separate by context
  theme_bw()


#### Non-summary data #####

##Plot both declarative and interrogative sentences as box plots
ggplot(agg, aes(x=lev, y=resp, group=lev, fill=factor(context))) +
  facet_grid(~context) + #sort by context
  geom_boxplot() + #use boxplots on graph
  scale_x_continuous(breaks=seq(1,9,1)) + #set the x axis to go from 1 to 9 with steps of 1
  theme_bw() + #black and white background theme
  xlab ("Stimulus Number") +
  ylab ("Average Response") +
  ggtitle ("Declarative vs. Interrogative") + #add title
  theme(legend.position = "none") #no legend
  




### Pulling different pattern types
 
# Creat (to look at) variable
to_look_at <- agg %>%
  # select(subj, lev, resp) %>%
  filter(lev == 1 & context == "i") %>%
  filter(resp < 3.5) %>%
  distinct(subj) %>%
  pull(subj)

# Create pattern 2 variable: Pattern two - in lev 9 interrogative, high naturalness ratings
pattern_two <- agg %>% 
  #select(subj, lev_f, resp, context_f) %>% 
  filter(lev == 9 & context== "i") %>% 
  filter(resp > 3.5) %>% 
  distinct(subj) %>% 
  pull(subj) 

# Create pattern 3 variable: Pattern three - lev 1 interrogative, low naturalness ratings
pattern_three <- agg %>% 
  # select(subj, lev, resp) %>% 
  filter(lev == 1 & context == "i") %>% 
  filter(resp < 3.5) %>% 
  distinct(subj) %>% 
  pull(subj)


### Graph patterns ###
# From agg select 'to look at' and graph the participant responses as lines on a graph
agg %>% 
  filter(subj %in% to_look_at ) %>% 
  filter(context == "i") %>% 
ggplot(aes(x=lev, y=resp)) +
  geom_line(aes(x=as.numeric(as.character(lev)), color=factor(subj)), stat="identity") + #line graph w/ each line representing a participant response, each a diff color
  scale_x_continuous(breaks=seq(1,9,1)) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab ("Stimulus Number") +
  ylab ("Average Response") 


#### Feb 28 ####
#level 1 and context i, for above 3.5 folks

# Create 'pattern_up35' variable: at lev 1, high naturalness ratings.
pattern_up35 <- agg %>% 
  #select(subj, lev, resp, context_f) %>% 
  filter(lev == 1 & context == "i") %>% 
  filter(resp > 3.5) %>% 
  distinct(subj) %>% 
  pull(subj)   +
  theme_bw() +
  theme(legend.position = "none") +
  xlab ("Stimulus Number") +
  ylab ("Average Response") 

# Graph line graph of just 'pattern_up35'
agg %>% 
  filter(subj %in% pattern_up35 ) %>% 
  filter(context == "i") %>% 
  ggplot(aes(x=lev, y=resp)) +
  geom_line(aes(x=as.numeric(as.character(lev)), color=factor(subj)), stat="identity") +
  scale_x_continuous(breaks=seq(1,9,1)) +
  theme_bw()

# level 1 and context i, for below 3.5 folks
# Create 'pattern_down35_at1' where at level 1, naturalness ratings are below 3.5
pattern_down35_at1 <- agg %>% 
  #select(subj, lev, resp, context) %>% 
  filter(lev == 1 & context == "i") %>% 
  filter(resp < 3.5) %>% 
  distinct(subj) %>% 
  pull(subj) 

# Graph 'pattern_down35_at1' group
agg %>% 
  filter(subj %in% pattern_down35_at1 ) %>% 
  filter(context == "i") %>% 
  ggplot(aes(x=lev, y=resp)) +
  geom_line(aes(x=as.numeric(as.character(lev)), color=factor(subj)), stat="identity") +
  scale_x_continuous(breaks=seq(1,9,1)) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab ("Stimulus Number") +
  ylab ("Average Response") 


## Split 'pattern_down35_at1' into two subgroups

# Create variable 'pattern_up35_at9' for group that start w/ low ratings that just go high
#level 1 and context i, for below 3.5 folks AND
#level 9 and context i, for above 3.5
pattern_up35_at9 <- agg %>% 
  #select(subj, lev, resp, context_f) %>% 
  filter(lev == 9 & context == "i") %>% 
  filter(resp > 3.5) %>% 
  distinct(subj) %>% 
  pull(subj) 


# Check work
# Print participants in 'pattern_down35_at1'
pattern_down35_at1

# Print participants in 'pattern_up35_at9' -- subset of previous group
pattern_up35_at9


## Create new variable 'comb_subj' that narrows down 'pattern_down35_at1' to just those that are less that 3.5 at lev 1
pattern_down35_at1 %in% pattern_up35_at9
comb_subj <- pattern_down35_at1[pattern_down35_at1 %in% pattern_up35_at9]
comb_subj


## Graph ratings in 'comb_subj'
## start low, go high
agg %>% 
  filter(subj %in% comb_subj) %>% 
  filter(context == "i") %>% 
  ggplot(aes(x=lev, y=resp)) +
  geom_line(aes(x=as.numeric(as.character(lev)), color=factor(subj)), stat="identity") +
  scale_x_continuous(breaks=seq(1,9,1)) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab ("Stimulus Number") +
  ylab ("Average Response") 


## Create variable 'non_comb_subj' that isolates lev 1 < 3.5 AND lev 9 < 3.5
#level 1 and context i, for below 3.5 folks BUT NOT
#level 9 and context i, for above 3.5
non_comb_subj <- pattern_down35_at1[!pattern_down35_at1 %in% pattern_up35_at9]
non_comb_subj

## Graph 'non_comb_subj'
agg %>% 
  filter(subj %in% non_comb_subj ) %>% 
  filter(context == "i") %>% 
  ggplot(aes(x=lev_f, y=resp)) +
  geom_line(aes(x=as.numeric(as.character(lev)), color=factor(subj)), stat="identity") +
  scale_x_continuous(breaks=seq(1,9,1)) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab ("Stimulus Number") +
  ylab ("Average Response") 



################################################################################  
### Expanding code at Princeton, with stats advisors
### New ways of performing ANOVA's and playing with potential regression models

## Running an ANOVA on agg_data.csv
test = read.csv("agg_data.csv", header = TRUE, stringsAsFactors = FALSE)

anova1 = aov(resp ~ factor(context)*factor(lev)*factor(exp), data = test)
  
summary(anova1)


#Now, with aggrgate data, run the RM ANOVA
#First, convert variables to factors with as.factor
test$lev = as.factor(test$lev)
test$context = as.factor(test$context)
test$exp = as.factor(test$exp)

# Call on library afex
library(afex)

#do second ANOVA
anova2 <- aov_ez (id = "subj",
                   dv = "resp",
                   between = c("exp"),
                   within = c("context", "lev"),
                   data = test)

summary(anova2)
agg_mod


## Create simple regression test

reg1 = lm(resp ~ factor(context)*factor(lev)*factor(exp), data = test)

summary(reg1)


#### Running a mixed model regression ####

# What are the names of the diff columns?
names(everything)


everything <- everything %>% 
  mutate(lev_f = factor(lev),
         context_f = factor(context),
         exp_f = factor(exp))

### post-hoc, running a few mixed model regressions

mod_first <- lmerTest::lmer(resp ~ context_f*lev_f*exp_f + (1 + lev_f |subj), data=everything)
anova(mod_first)


mod_first <- lme4::lmer(resp ~ context_f*lev_f*exp_f + (1|subj) + (1|subj:context_f) + (1|subj:lev_f), data=everything)

library(afex)  
mod_first <- mixed(resp ~ context_f*lev_f*exp_f + (1|subj) + (1|subj:context_f) + (1|subj:lev_f), data=everything)

aov_ez(id = "subj", dv = "resp",
       within=c("lev_f", "context_f"),
       between = c("exp_f"),
       data=everything)