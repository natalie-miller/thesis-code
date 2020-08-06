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


### Finding averages of the responses, by calling the library "emmeans"

library(emmeans)
emmeans(agg_mod, "lev") #regardless of context

emmeans(agg_mod, "lev", by="context")
pairs(emmeans(agg_mod, "lev", by="context")) #seems pointless


#### graph ####
gr_resp_data <- summary(emmeans(agg_mod, "lev", by="context"))

gr_resp_data %>% 
  filter(context == "d") %>% 
ggplot( aes(x=lev, y=emmean)) +
  geom_bar(stat="identity", fill = "blue") +
  labs(title="Declarative Sentences") +
  theme_bw()


gr_resp_data %>% 
  filter(context == "i") %>% 
  ggplot( aes(x=lev, y=emmean)) +
  geom_bar(stat="identity", fill = "red") +
  labs(title="Interogative Sentences") +
  theme_bw()


ggplot(gr_resp_data, aes(x=lev, y=emmean, fill=context)) +
  geom_bar(stat="identity", fill = "gray") +
  geom_errorbar(aes(ymax=emmean+SE, ymin=emmean-SE), width=0.2) +
  facet_grid(~context) +
  theme_bw()

#### Non-summary data #####
ggplot(agg, aes(x=lev, y=resp, group=lev, fill=factor(context))) +
  facet_grid(~context) +
  geom_boxplot() +
  scale_x_continuous(breaks=seq(1,9,1)) +
  theme_bw() +
  xlab ("Stimulus Number") +
  ylab ("Average Response") +
  ggtitle ("Declarative vs. Interrogative") +
  theme(legend.position = "none")
  
  
pattern_two <- agg %>% 
  #select(subj, lev_f, resp, context_f) %>% 
  filter(lev == 9 & context== "i") %>% 
  filter(resp > 3.5) %>% 
  distinct(subj) %>% 
  pull(subj) 
  
pattern_three <- agg %>% 
  select(subj, lev, resp) %>% 
  filter(lev == 1 & context == "i") %>% 
  filter(resp < 3.5) %>% 
  distinct(subj) %>% 
  pull(subj)

to_look_at <- agg %>%
  select(subj, lev, resp) %>%
  filter(lev == 1 & context == "i") %>%
  filter(resp < 3.5) %>%
  distinct(subj) %>%
  pull(subj)


agg %>% 
  filter(subj %in% to_look_at ) %>% 
  filter(context == "i") %>% 
ggplot(aes(x=lev, y=resp)) +
  geom_line(aes(x=as.numeric(as.character(lev)), color=factor(subj)), stat="identity") +
  scale_x_continuous(breaks=seq(1,9,1)) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab ("Stimulus Number") +
  ylab ("Average Response") 


#### Feb 28 ####
#level 1 and context i, for above 3.5 folks
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

agg %>% 
  filter(subj %in% pattern_up35 ) %>% 
  filter(context == "i") %>% 
  ggplot(aes(x=lev, y=resp)) +
  geom_line(aes(x=as.numeric(as.character(lev)), color=factor(subj)), stat="identity") +
  scale_x_continuous(breaks=seq(1,9,1)) +
  theme_bw()

#level 1 and context i, for below 3.5 folks
pattern_down35_at1 <- agg %>% 
  #select(subj, lev, resp, context) %>% 
  filter(lev == 1 & context == "i") %>% 
  filter(resp < 3.5) %>% 
  distinct(subj) %>% 
  pull(subj) 

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

#level 1 and context i, for below 3.5 folks AND
#level 9 and context i, for above 3.5
pattern_up35_at9 <- agg %>% 
  #select(subj, lev, resp, context_f) %>% 
  filter(lev == 9 & context == "i") %>% 
  filter(resp > 3.5) %>% 
  distinct(subj) %>% 
  pull(subj) 

pattern_down35_at1
pattern_up35_at9

pattern_down35_at1 %in% pattern_up35_at9
comb_subj <- pattern_down35_at1[pattern_down35_at1 %in% pattern_up35_at9]
comb_subj

agg %>% 
  filter(subj %in% comb_subj ) %>% 
  filter(context == "i") %>% 
  ggplot(aes(x=lev, y=resp)) +
  geom_line(aes(x=as.numeric(as.character(lev)), color=factor(subj)), stat="identity") +
  scale_x_continuous(breaks=seq(1,9,1)) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab ("Stimulus Number") +
  ylab ("Average Response") 

#level 1 and context i, for below 3.5 folks BUT NOT
#level 9 and context i, for above 3.5
non_comb_subj <- pattern_down35_at1[!pattern_down35_at1 %in% pattern_up35_at9]
non_comb_subj

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



geom_line(aes(x=lev, y=resp, color=factor(subj)), stat="identity") 
  
################################################################################  
###Princeton

setwd("~/Google Drive")

test = read.csv("combined.csv", header = TRUE, stringsAsFactors = FALSE)

anova1 = aov(resp ~ factor(context)*factor(lev)*factor(exp), data = test)
  
summary(anova1)


#Now, with aggrgate data, run the RM ANOVA

test$lev = as.factor(test$lev)
test$context = as.factor(test$context)
test$exp = as.factor(test$exp)

library(afex)
anova2 <- aov_ez (id = "subj",
                   dv = "resp",
                   between = c("exp"),
                   within = c("context", "lev"),
                   data = test)

summary(anova2)
agg_mod



##regression test
# 
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