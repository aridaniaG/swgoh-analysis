library("rstanarm")

load("swgoh_models.RData")

summary(m_Bayes_character)
summary(m_Bayes_equipment)




################################################################################
# Character analysis 




# Posterior distributions





# Credible intervals
plot(m_Bayes_character, "areas", prob = 0.95, 
     pars = c("date","attempts","simulatedTRUE"))


plot(m_Bayes_character, "areas",
     regex_pars = "^Sigma")


plot(m_Bayes_character, regex_pars = "reward:")



# Posterior predictive
nd = data.frame(date = as.Date("2020-09-15"),
                attempts = 5,
                count = 0, # this doesn't do anything
                simulated = TRUE,
                battle = "Dark Side 1-A (Hard)",
                reward = "Sabine Wren")
  
p = posterior_predict(m_Bayes_character,
                      newdata = nd,
                      draws = 500)

ggplot(data.frame(p = p), aes(x = p)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, color = "white") + 
  labs(title = paste(nd$battle, nd$reward, sep = ": "),
       x = "Number of shards",
       y = "Probability") + 
  theme_bw()
