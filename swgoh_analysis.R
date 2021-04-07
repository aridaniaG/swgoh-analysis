library("tidyverse"); ggplot2::theme_set(th)
library("gridExtra")
library("rstanarm")

load("swgoh_models.RData")

# summary(m_Bayes_character)
# summary(m_Bayes_equipment)




################################################################################
# Character analysis 
params = names(m_Bayes_character$coefficients)
dc = as.data.frame(m_Bayes_character)


# Extract posterior samples
posterior_samples = data.frame(
  intercept = dc[[params[1]]],
  date      = dc[[params[2]]],
  attempts  = dc[[params[3]]],
  manual    = dc[[params[4]]],
  
  # Standard deviation of random effects
  reward_sigma        = sqrt(dc[[155]]),
  battle_sigma        = sqrt(dc[[154]]),
  battle_reward_sigma = sqrt(dc[[153]])
)


density_plot = function(samples, name, lessthanzero = TRUE) {
  g = ggplot(data.frame(x = samples), aes(x = x)) +
    geom_density() 
  
  d = ggplot_build(g)$data[[1]] %>% filter(x < 0) %>% select(x,y)
  
  if (lessthanzero) {
    g = g + 
      geom_area(data = d, aes(x=x, y=y), fill = "magenta", color = NA) +
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

p1 = density_plot(posterior_samples$date, "Date")
p2 = density_plot(posterior_samples$attempts, "Number of attempts")
p3 = density_plot(posterior_samples$manual, "Manual indicator")

p4 = density_plot(posterior_samples$reward_sigma, "Reward random effect", FALSE)
p5 = density_plot(posterior_samples$battle_sigma, "Battle random effect", FALSE)
p6 = density_plot(posterior_samples$battle_reward_sigma, "Battle:reward random effect", FALSE)

pp = gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)

ggsave("character_posteriors.png", pp, width = 14, height = 12)

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
