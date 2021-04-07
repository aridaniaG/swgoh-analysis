library("tidyverse")
library("rstanarm")
library("lme4")
library("swgoh")

characters = swgoh::rewards %>%
  left_join(swgoh::reward_details, by = "reward") %>%
  filter(type == "Character") %>%
  mutate(manual = !simulated)

equipment = swgoh::rewards %>%
  left_join(swgoh::reward_details, by = "reward") %>%
  filter(type == "Component") %>%
  mutate(manual = !simulated)

###### Non-Bayesian fit
m_nonBayes_character = glmer(cbind(count, attempts - count) ~
                               I(date - as.Date("2020-09-15")) + 
                               I(attempts - 5) + 
                               manual + 
                               (1|reward) + 
                               (1|battle) +
                               (1|battle:reward), 
                             family = binomial,
                             data = characters)

m_nonBayes_equipment = glmer(cbind(count, attempts - count) ~
                               I(date - as.Date("2020-09-15")) + 
                               I(attempts - 5) + 
                               manual + 
                               (1|reward) + 
                               (1|battle) +
                               (1|battle:reward), 
                             family = binomial,
                             data = equipment)


###### Bayesian fits
m_Bayes_character = stan_glmer(cbind(count, attempts - count) ~
                                 I(date - as.Date("2020-09-15")) + 
                                 I(attempts - 5) + 
                                 manual + 
                                 (1|reward) + 
                                 (1|battle) +
                                 (1|battle:reward), 
                               family = binomial,
                               data = characters,
                               seed = 20200406)


m_Bayes_equipment = stan_glmer(cbind(count, attempts - count) ~
                                 I(date - as.Date("2020-09-15")) + 
                                 I(attempts - 5) + 
                                 manual + 
                                 (1|reward) + 
                                 (1|battle) +
                                 (1|battle:reward), 
                               family = binomial,
                               data = equipment,
                               seed = 20200406)



save.image(file = "swgoh_models.RData")
