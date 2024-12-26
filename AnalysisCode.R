############################################
# POL S 205 - Final Project Replication
# Does Party Support Affect Primary Outcomes?
# Andrew Liang
############################################

# This script includes all of the code required to replicate the tables and graphs
# in my final project.

# Set workspace, load in packages and data
rm(list = ls())
setwd("") # Blank so that the directory can be altered if needed.
library(tidyverse); library(readxl); library(stargazer)
library(stringr); library(rlang); library(patchwork); library(jtools)
options(scipen = 999999)


####################
# Initial processing of data for variables
####################

# Contributions in primaries, 1992-2018 - loads in as "primaries"
# Data was obtained from the Stanford Database on Ideology, Money in Politics,
# and Elections (https://data.stanford.edu/dime), which is made available
# under the ODC Attribution License. 
load("primarycontribs1980_2018.RData") 
primaries$state <- primaries$recipient.state
primaries$party <- case_when(primaries$recipient.party == 100 ~ "Democratic",
                             primaries$recipient.party == 200 ~ "Republican",
                             primaries$recipient.party == 328 ~ "Independent")

# Primary dates, 1992-2018
# Obtained from America Votes Handbooks Volumes 14 to 19. 
primarydates <- read_xlsx("Primary Dates 1980-2018.xlsx") 

# Merging datasets
primaries.with.dates <- merge(primaries, primarydates, by = c("cycle", "state"), 
                              all.x = T)

# Creating dataset of party contributions during primaries
primary.party.contributions <- primaries.with.dates %>%
  filter(date < primarydate) %>%
  filter(str_detect(contributor.name, regex(party, ignore_case = TRUE)))

# Filtering for key phrases used by non-party organizations that still contain
# the candidate's party's name, and aggregating by candidate-cycle
primary.party.contributions <- primary.party.contributions %>%
  filter(!grepl(pattern = "america|action|club|educ|voter|citizens|american|club|pac|rancho|for|workers|majority|trust|team|ideas|bringing| to | in |opportunity", 
                x = primary.party.contributions$contributor.name, 
                ignore.case = T) == T) %>%
  group_by(cycle, state, bonica.rid, party) %>%
  summarize(total.party.contributions = sum(amount))

# DIME candidates dataset - loads in as "cands"
load(url("https://www.dropbox.com/scl/fi/22se61x8uvo73ers02lno/dime_recipients_1979_2022.rdata?rlkey=oscfx8399ytbn3wvkgl3n8gmi&dl=1"))

# Standardizing candidate names with existing dataset
cands$name <- toupper(cands$name)

# Creating dataset of contributions from corporations to candidates during primaries
# Aggregating by candidate-cycle and filtering for only committee contributions
primary.corp.contributions <- filter(primaries.with.dates, date < primarydate,
                                     grepl(pattern = "Corp|Corp.|Corporation|Inc|Inc.|Incorporated|Company", 
                                           x = primaries.with.dates$contributor.name, 
                                           ignore.case = T) == T,
                                     contributor.type == "C") %>%
  group_by(cycle, state, bonica.rid, party) %>%
  summarize(total.corp.contributions = sum(amount))

# Analysis covers only primaries from 1980 to 2018
# Changing proportion variable to one that takes on values between 0 and 100
cands.primaries.only <- subset(cands, !is.na(cands$prim.vote.pct)) %>%
  filter(cycle >= 1980, 
         cycle <= 2018) %>%
  select(cycle, bonica.rid, prim.vote.pct)

# Merging for primary outcomes and for contested primary elections
primariesparty <- merge(primary.party.contributions, cands.primaries.only, 
                        by = c("cycle", "bonica.rid")) %>%
  mutate(total.party.contributions.thousands = total.party.contributions/1000,
         total.party.contributions.logged = log(total.party.contributions)) %>%
  mutate(prim.vote.pct = prim.vote.pct*100) %>%
  filter(prim.vote.pct < 100, 
         prim.vote.pct > 0, 
         total.party.contributions >= 0)

# Based on our filter above, any -Inf values result from the value being 0. Thus,
# observations where the logged value is not a number is converted back to 0
primariesparty$total.party.contributions.logged[primariesparty$total.party.contributions.logged == -Inf] <- 0

# Merging for corporate outcomes and for contested primary elections
primariescorp <- merge(primary.corp.contributions, 
                       cands.primaries.only, 
                       by = c("cycle", "bonica.rid")) %>%
  mutate(total.corp.contributions.thousands = total.corp.contributions/1000,
         total.corp.contributions.logged = log(total.corp.contributions)) %>%
  mutate(prim.vote.pct = prim.vote.pct*100) %>%
  filter(prim.vote.pct < 100, 
         prim.vote.pct > 0, 
         total.corp.contributions >= 0)

# Based on our filter above, any -Inf values result from the value being 0. Thus,
# observations where the logged value is not a number is converted back to 0
primariescorp$total.corp.contributions.logged[primariescorp$total.corp.contributions.logged == -Inf] <- 0

# Aggregate dataset
primariescomplete <- merge(primariesparty, primariescorp, 
                           by = c("bonica.rid", 
                                  "cycle",
                                  "state",
                                  "prim.vote.pct",
                                  "party"),
                           all.x = T)


####################
# Data Analysis
####################

# Summary Statistics and Graphs
## Table 1: Number of observations is relevant here since we don't have 
## corporate contribution data for all candidates.
stargazer(primariescomplete, title = "Summary Statistics of Key Variables", 
          omit = c("cycle",
                   "total.party.contributions.logged",
                   "total.corp.contributions.thousands",
                   "total.party.contributions.thousands",
                   "total.corp.contributions.logged"),
          covariate.labels = c("% Primary Vote Share", "Total Contributions, Party", "Total Contributions, Corporations"), 
          digits = 2, 
          column.sep.width = "0pt", 
          omit.summary.stat = NULL,
          mean.sd = T, 
          min.max = T, 
          median = T,
          notes = "Contribution amounts in U.S. Dollars.",
          caption = "Summary Statistics of Key Variables.",
          label = "tbl:t1",
          type = "latex",
          out = "table1.tex")

## Histograms for Figure 3
prim.histogram <- ggplot(primariescomplete, aes(x = prim.vote.pct)) +
  geom_histogram(col = "black", 
                 fill = "gray", 
                 binwidth = 2) +
  labs(x = "% Primary Vote Share", 
       y = "Frequency") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0.005, 0)) +
  theme_classic()

party.histogram <- ggplot(primariescomplete, aes(x = total.party.contributions)) +
  geom_histogram(col = "black", 
                 fill = "gray", 
                 binwidth = 75000) +
  labs(x = "Total Party Contribution, USD", 
       y = "Frequency") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0.005, 0)) +
  theme_classic()

corp.histogram <- ggplot(primariescomplete, aes(x = total.corp.contributions)) +
  geom_histogram(col = "black", 
                 fill = "gray", 
                 binwidth = 100000) +
  labs(x = "Total Corporation Contribution, USD",
       y = "") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0.005, 0)) +
  theme_classic()

png(file = "Fig3.png", height = 6, width = 9, units = "in", res = 300)
prim.histogram / (party.histogram + corp.histogram)  +
  plot_annotation(tag_levels = "a")
dev.off()

# Histograms of log-transformed variables for Figure 4
logged.party.histogram <- ggplot(primariescomplete, aes(x = total.party.contributions.logged)) +
  geom_histogram(col = "black", 
                 fill = "gray", 
                 binwidth = 0.5) +
  labs(x = "Log, Total Party Contribution", 
       y = "Frequency") +
  scale_y_continuous(expand = c(0, 0))  +
  theme_classic()

logged.corp.histogram <- ggplot(primariescomplete, aes(x = total.corp.contributions.logged)) +
  geom_histogram(col = "black", 
                 fill = "gray", 
                 binwidth = 0.5) +
  labs(x = "Log, Total Corporation Contribution",
       y = "") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()

png(file = "Fig4.png", height = 6, width = 9, units = "in", res = 300)
logged.party.histogram + logged.corp.histogram +
  plot_annotation(tag_levels = "a")
dev.off()

# Boxplots of log-transformed variables for Figure 5
party.boxplot <- ggplot(primariescomplete) +
  geom_boxplot(aes(x = total.party.contributions.logged), col = "black") +
  labs(x = "Log, Total Party Contribution",
       y = "") +
  scale_y_continuous(expand = c(0, 1)) +
  theme_classic() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

corp.boxplot <- ggplot(primariescomplete) +
  geom_boxplot(aes(x = total.corp.contributions.logged), col = "blue") +
  labs(x = "Log, Total Corporation Contribution",
       y = "") +
  scale_y_continuous(expand = c(0, 1)) +
  theme_classic() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

png(file = "fig5.png", height = 6, width = 9, units = "in", res = 300)
party.boxplot / corp.boxplot  +
  plot_annotation(tag_levels = "a")
dev.off()

# Regression
# Model 1 (Party Contributions Only)
m1 <- lm(prim.vote.pct ~ total.party.contributions.logged, data = primariescomplete,
         na.action = na.exclude)
summary(m1)

# Model 2 (Corporate Contributions Only)
m2 <- lm(prim.vote.pct ~ total.corp.contributions.logged, data = primariescomplete,
         na.action = na.exclude)
summary(m2)

# Model 3 (Full Model)
m3 <- lm(prim.vote.pct ~ total.party.contributions.logged + 
           total.corp.contributions.logged, 
         data = primariescomplete,
         na.action = na.exclude)
summary(m3)

## Table 2
stargazer(m1, m2, m3,
          covariate.labels = c("Log, Total Contributions: Party", "Log, Total Contributions: Corporations", "Intercept"),
          omit.stat = c("f", "rsq"),
          dep.var.labels = "Percentage of Votes Received in Primary",
          notes = c("OLS standard errors in parentheses.", "Significance levels reported from two-sided tests."),
          notes.align = "l",
          label = "tbl:t2",
          align = T,
          digits = 3,
          type = "latex",
          out = "table2.tex")

## Predictions

# Dollar amounts of one-thousand, ten-thousand, hundred thousand, one million, 
# and ten million USD

# When holding covariates constant at 0, an approximately 61.16 percent increase
# in the total amount of contributions from a candidate's party is associated with
# a one-percentage point increase in the percentage of votes they receive
predict <- data.frame(amount = c(1, 1.61, 10, 16.11, 100, 161.15, 1000, 1611.57, 10000, 16115.77, 100000, 161157.7, 1000000, 1611577))
predict$total.party.contributions.logged <- log(predict$amount)
predict$total.corp.contributions.logged <- 0
predict$estimated <- predict.lm(m3, newdata = predict)


# When holding covariates constant at 0, an approximately 51.61 percent increase
# in the total amount of contributions from corporations is associated with
# a one-percentage point increase in the percentage of votes they receive
predict2 <- data.frame(amount = c(1, 1.51, 10, 15.16, 100, 151.61, 1000, 1516.10, 10000, 15161.08, 100000, 151610.80, 1000000, 1516180))
predict2$total.party.contributions.logged <- 0
predict2$total.corp.contributions.logged <- log(predict$amount)
predict2$estimated <- predict.lm(m3, newdata = predict2)
