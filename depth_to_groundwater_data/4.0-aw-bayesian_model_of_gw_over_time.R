library(brms)
library(tidybayes)
library(cmdstanr)

annual_groundwater

model_data <- annual_groundwater %>% filter(`Site name`=="Alameda" |
                                              `Site name` == "Rio Grande Nature Center" |
                                              `Site name` == "Reynolds Forest" |
                                              `Site name` == "Los Lunas" |
                                              `Site name` == "Montano") %>% 
  select(`Site number`, `Site name`, Year, `Annual mean depth to groundwater cm`) %>% 
  rename(site_name = `Site name`,site_number = `Site number`,
         year = Year, annual_mean_depth = `Annual mean depth to groundwater cm`)
model_data

write_csv(model_data, "./models/five_site_groundwater_data_for_model.csv")

### Model 1 -- full model ###

# Fit model
brms.mod.1 <- bf(annual_mean_depth ~ year + (1|year) + (1|site_name ))
brms.mod.1

# See what the default priors are for model 1
get_prior(brms.mod.1, data = model_data)

# Set our own priors for model 1
prior1 <- c(
  set_prior("student_t(3,0,1)", coef = "year"),
  set_prior("student_t(7, 0, 2.5)", class = "sd", group = "year"),
  set_prior("student_t(7, 0, 2.5)", class = "sd", group = "site_name"))
prior1

# Run the model
k_fit_brms.1 <- brm(brms.mod.1, 
                    data = model_data,
                    family = "gaussian", prior = prior1,
                    chains = 4, cores = 8, iter = 2000, backend = "cmdstanr",
                    control = list(adapt_delta = 0.85, max_treedepth = 12))

# Model summary at the 50% uncertainty interval. Group summary and population estimates for traits.
# 
summary(k_fit_brms.1, prob = 0.5)
summary(k_fit_brms.1, prob = 0.95)

plot(k_fit_brms.1, ask = F)

### Model checking plots. Posterior predict. 
pp_check(k_fit_brms.1, resp = "species_CV", ndraws = 100)

# Full group level estimates for random effects
ranef(k_fit_brms.1, groups = "year", probs = 0.5)
ranef(k_fit_brms.1, groups = "site_name", probs = 0.5)


# Conditional plots
k_fit_brms.1.cond <- plot(conditional_effects(k_fit_brms.1), points = TRUE, ask = F,
                          probs=0.5)

k_fit_brms.1.pred.year<- model_data %>%
  add_predicted_draws(k_fit_brms.1, allow_new_levels = TRUE) %>%  # adding the posterior distribution
  ggplot(aes(y = annual_mean_depth, x = year)) +  
  stat_lineribbon(aes(y = .prediction), .width = c(.50),  # regression line and CI
                  alpha = 0.5, colour = "black") +
  geom_point(data = model_data, colour = "darkseagreen4", size = 3) +   # raw data
  scale_fill_brewer(palette = "Greys") + ylim(-450, 10) +
  xlab("Year") + ylab("Mean annual depth to groundwater (cm)")
k_fit_brms.1.pred.year
