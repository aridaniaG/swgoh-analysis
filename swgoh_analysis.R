library("tidyverse"); ggplot2::theme_set(th)
library("gridExtra")
library("rstanarm")

load("swgoh_models.RData")

# summary(m_Bayes_character)
# summary(m_Bayes_equipment)


density_plot = function(samples, name, lessthanzero = TRUE) {
  g = ggplot(data.frame(x = samples), aes(x = x)) +
    geom_density() 
  
  d = ggplot_build(g)$data[[1]] %>% filter(x < 0) %>% select(x,y)
  
  if (lessthanzero) {
    g = g + 
      geom_area(data = d, aes(x=x, y=y), fill = "#C8102E", color = NA) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_text(aes(x = Inf, y = Inf, hjust = 1.2, vjust = 1.2,
                    label = paste0("P < 0 = ", 
                                   formatC(mean(samples < 0), digits = 2, format="f"))))
    
    xlabel = paste(name, "coefficient")
  } else {
    xlabel = paste(name, "standard deviation")
  }
  
  g + geom_density(size = 2) +
    labs(title = name,
         x = xlabel,
         y = "Probability density function") 
}

################################################################################
# Character analysis 
params = names(m_Bayes_character$coefficients)
dc = as.data.frame(m_Bayes_character)


# Extract posterior samples
character_samples = data.frame(
  intercept = dc[[params[1]]],
  date      = dc[[params[2]]],
  attempts  = dc[[params[3]]],
  manual    = dc[[params[4]]],
  
  # Standard deviation of random effects
  reward_sigma        = sqrt(dc[[155]]),
  battle_sigma        = sqrt(dc[[154]]),
  battle_reward_sigma = sqrt(dc[[153]])
)




p1 = density_plot(character_samples$date, "Date")
p2 = density_plot(character_samples$attempts, "Number of attempts")
p3 = density_plot(character_samples$manual, "Manual indicator")

p4 = density_plot(character_samples$reward_sigma, "Reward random effect", FALSE)
p5 = density_plot(character_samples$battle_sigma, "Battle random effect", FALSE)
p6 = density_plot(character_samples$battle_reward_sigma, "Battle:reward random effect", FALSE)

pp = gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)

ggsave("character_posteriors.png", pp, width = 14, height = 12)





################################################################################
# Equipment analysis 
de = as.data.frame(m_Bayes_equipment)


# Extract posterior samples
equipment_samples = data.frame(
  intercept = de[[params[1]]],
  date      = de[[params[2]]],
  attempts  = de[[params[3]]],
  manual    = de[[params[4]]],
  
  # Standard deviation of random effects
  reward_sigma        = sqrt(de[[732]]),
  battle_sigma        = sqrt(de[[731]]),
  battle_reward_sigma = sqrt(de[[730]])
)




p1 = density_plot(equipment_samples$date, "Date")
p2 = density_plot(equipment_samples$attempts, "Number of attempts")
p3 = density_plot(equipment_samples$manual, "Manual indicator")

p4 = density_plot(equipment_samples$reward_sigma, "Reward random effect", FALSE)
p5 = density_plot(equipment_samples$battle_sigma, "Battle random effect", FALSE)
p6 = density_plot(equipment_samples$battle_reward_sigma, "Battle:reward random effect", FALSE)

pe = gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)

ggsave("equipment_posteriors.png", pe, width = 14, height = 12)



# Specific combinations
characters = c("Luke Skywalker (Farmboy)",
               "Veteran Smuggler Han Solo",
               "Rey (Scavenger)",
               "Obi-Wan Kenobi (Old Ben)",
               "Boba Fett",
               "Clone Wars Chewbacca",
               "Kylo Ren (Unmasked)",
               "Kylo Ren","Young Lando Calrissian",
               "Finn")

pc = rewards %>% 
  filter(reward %in% characters) %>%
  select(battle, reward) %>%
  unique() %>%
  arrange(reward, battle) %>%
  mutate(
    date = as.Date("2019-09-15"),
         attempts = 5,
         manual = FALSE)

lpc = posterior_epred(m_Bayes_character, 
                        newdata = pc) 

a = 0.05
pc = pc %>% 
  mutate(median = apply(lpc, 2, median),
         lower  = apply(lpc, 2, function(x) quantile(x,   a/2)),
         upper  = apply(lpc, 2, function(x) quantile(x, 1-a/2)),
         
         battle_reward = paste(battle,reward, sep=": "),
         battle_reward = fct_reorder(battle_reward, median)) 

pc_plot = ggplot(pc, aes(y = battle_reward, 
               x = median, xmin = lower, xmax = upper,
               color = reward)) +
  geom_pointrange() +
  labs(title = "Character shard drop probabilities",
       y = "Battle: reward",
       x = "Drop probabilities") + 
  xlim(0,1) + 
  theme(legend.position = "none")

ggsave(file = "character_shard_drop_probabilities.png",
       pc_plot, height = 10, width = 12)

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
