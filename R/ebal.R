library(ebal)
library(tidyverse)

c_data <- read_csv("data/c_data.csv")

# log1p(party_age) + 
#     r_avg_vote + 
#     cabinet_party_last + 
#     enep_lag + name_exp + elec_shock + pedersen

vars <- c("Party Age, Logged", "Party Size", "Incumbent Government",
          "Electoral Pluralism",  "Brand Exposure", "Electoral Shock", "Party System Weakness")

c_data1 <- c_data %>% 
    group_by(country, party) %>%
    arrange(country, party, year) %>% 
    mutate(votes_gain_abs = votes - lag(votes),
           votes_gain_rel = votes/lag(votes)) %>% 
    ungroup() %>% 
    select(country, year, party, cparty, election,
           change, party_age, r_avg_vote, cabinet_party_last, 
           enep_lag, name_exp, elec_shock, pedersen, 
           votes, votes_gain_abs, votes_gain_rel) %>% 
    filter(complete.cases(.) & votes_gain_rel!=Inf) 

c_ebal <- ebalance(Treatment = c_data1$change, 
                   X = c_data1 %>% select(-change, -votes, -votes_gain_abs, -votes_gain_rel,
                                          -country, -year, -party, -cparty, -election))

c_data1$w <- 1
c_data1$w[c_data1$change==0] <- c_ebal$w

lm(data = c_data1, votes_gain_rel ~ change, weights = w) %>% summary()
lm(data = c_data1, votes_gain_abs ~ change, weights = w) %>% summary()

lme4::lmer(data = c_data1, votes_gain_abs ~ change +
         log1p(party_age) + 
         r_avg_vote + 
         cabinet_party_last + 
         enep_lag + name_exp + elec_shock + pedersen +
         (1|cparty) + (1|election) + (1|country),
     weights = w) %>% summary()
