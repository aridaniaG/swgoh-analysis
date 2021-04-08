library("tidyverse"); ggplot2::theme_set(theme_bw())
library("gridExtra")
library("rstanarm")
library("swgoh")

source("ISU_palette.R")
source("density_plot.R")

scaling = 0.9

load("swgoh_models.RData")

# summary(m_Bayes_character)
# summary(m_Bayes_equipment)




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

ggsave("character_posteriors.png", pp, 
       width = 16*scaling, height = 9*scaling)




# Specific combinations
selected_characters = c(
  "Luke Skywalker (Farmboy)",
               "Veteran Smuggler Han Solo",
               "Rey (Scavenger)",
               "Obi-Wan Kenobi (Old Ben)",
               "Boba Fett",
               "Clone Wars Chewbacca",
               "Kylo Ren (Unmasked)",
               "Kylo Ren","Young Lando Calrissian",
               "Finn")

pc = rewards %>% 
  filter(reward %in% selected_characters) %>%
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
         
         pairs = ifelse (reward %in% c("Rey (Scavenger)", 
                                       "Clone Wars Chewbacca",
                                       "Boba Fett",
                                       "Luke Skywalker (Farmboy)",
                                       "Finn"),
                         reward, "zzz"),
         
         battle_reward = paste(battle,reward, sep=": "),
         battle_reward = fct_reorder(battle_reward, pairs),
         battle_reward = fct_rev(battle_reward))

pc_plot = ggplot(pc, aes(y = battle_reward, 
                         x = median, xmin = lower, xmax = upper,
                         color = pairs)) +
  geom_linerange(size = 2) +
  labs(title = "Character shard drop probabilities",
       y = "Battle: reward",
       x = "Drop probabilities") + 
  scale_color_manual(values = ISU_secondary_palette[c(1,12,5,8,4,15)]) +
  xlim(0,1) + 
  theme(legend.position = "none")

ggsave(file = "character_shard_drop_probabilities.png",
       pc_plot, width = 16*scaling, height = 9*scaling)




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

ggsave("equipment_posteriors.png", pe, 
       width = 16*scaling, height = 9*scaling)





# Specific combinations
selected_rewards = c(
  "Mk 5 A/KT Stun Gun Prototype Salvage",
            "Mk 1 BAW Armor Mod",
            "Mk 3 Carbanti Sensor Array Salvage",
            "Mk 7 BAW Armor Mod Prototype Salvage")

equipment_selection = swgoh::rewards %>%
  left_join(swgoh::reward_details, by = "reward") %>%
  filter(type == "Component") %>%
  select(battle, reward) %>%
  filter(reward %in% selected_rewards) %>%
  unique()


pe = equipment_selection %>% 
  mutate(
    date = as.Date("2019-09-15"),
    attempts = 5,
    manual = FALSE)

lpe = posterior_epred(m_Bayes_equipment, 
                      newdata = pe) 

a = 0.05
pe = pe %>% 
  mutate(median = apply(lpe, 2, median),
         lower  = apply(lpe, 2, function(x) quantile(x,   a/2)),
         upper  = apply(lpe, 2, function(x) quantile(x, 1-a/2)),
         
         battle_reward = paste(battle,reward, sep=": "),
         battle_reward = fct_reorder(battle_reward, median)) 

pe_plot = ggplot(pe, aes(y = battle, 
                         x = median, xmin = lower, xmax = upper,
                         color = reward)) +
  geom_linerange(size = 2) +
  facet_grid(.~reward) + 
  labs(title = "Equipment drop probabilities",
       y = "Battle",
       x = "Drop probabilities") + 
  scale_color_manual(values = ISU_primary_palette) +
  xlim(0,1) + 
  theme(legend.position = "none")

ggsave(file = "equipment_shard_drop_probabilities.png",
       pe_plot, width = 16*scaling, height = 9*scaling)


