#load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(data.table)
#load data
county_facts <- read_csv("~/Downloads/county_facts.csv")
primary_results <- read_csv("~/Downloads/primary_results.csv.zip")
head(county_facts)
head(primary_results)

unique(primary_results$party)
unique(primary_results$candidate)

#subset republican candiates by each state and max vote (absolute & %)
#county winner
republican_votes <- primary_results %>%
  filter(party=="Republican") %>%
  group_by(state, fips) %>%
  summarize(winner = candidate[which.max(fraction_votes)],
            fraction_votes = max(fraction_votes),
            votes = max(votes))

head(republican_votes)
str(republican_votes)
unique(republican_votes$winner)

county_demo <-subset(county_facts,select=
                            c("fips","area_name","AGE775214","SEX255214","RHI125214","RHI225214","RHI325214",
                              "RHI425214","RHI725214","POP645213","POP815213","EDU685213","VET605213","HSG445213",
                              "INC110213","PVY020213","POP060210"))
setnames(county_demo, old=c("AGE775214","SEX255214","RHI125214","RHI225214","RHI325214","RHI425214",
                            "RHI725214","POP645213","POP815213","EDU685213","VET605213","HSG445213",
                            "INC110213","PVY020213","POP060210"), 
                      new=c("over_65 ","female","white","african_americs","indian_american","asians",
                            "hispanic","foreign_born","non_native_english","college","veterans","home_owned_rate",
                            "income","poverty_level","pop_density"))
repub_votes<-inner_join(republican_votes,county_demo, by="fips")

repub_votes %>%
  filter(winner=="Donald Trump" | winner=="Ted Cruz" | winner=="Ben Carson" | 
           winner=="Marco Rubio"| winner=="John Kasich")

nrow(repub_votes)
