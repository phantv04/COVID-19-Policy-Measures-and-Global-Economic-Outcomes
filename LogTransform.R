library(ggplot2)
library(patchwork)
library(car)

# Non-transformed data
p1 <- ggplot(master, aes(x = avg_workplace_closure, y = GDP_growth)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "GDP Growth vs Workplace Closure") +
  theme_minimal()

p2 <- ggplot(master, aes(x = annual_vaccinations_per_million, y = GDP_growth)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "GDP Growth vs Vaccinations") +
  theme_minimal()

p3 <- ggplot(master, aes(x = annual_tests_per_thousand, y = GDP_growth)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "GDP Growth vs Tests per Thousand") +
  theme_minimal()

p4 <- ggplot(master, aes(x = covid_deaths_per_million, y = GDP_growth)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "GDP Growth vs Deaths per Million") +
  theme_minimal()

(p1 | p2) / (p3 | p4)


# Log-transformed data
lp1 <- ggplot(master, aes(x = avg_workplace_closure, y = GDP_growth)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "RAW: Workplace Closure vs GDP Growth") +
  theme_minimal()

lp2 <- ggplot(master, aes(x = log(annual_vaccinations_per_million + 1), y = GDP_growth)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "LOG: Vaccinations vs GDP Growth") +
  theme_minimal()

lp3 <- ggplot(master, aes(x = log(annual_tests_per_thousand + 1), y = GDP_growth)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "LOG: Tests vs GDP Growth") +
  theme_minimal()

lp4 <- ggplot(master, aes(x = log(covid_deaths_per_million + 1), y = GDP_growth)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "LOG: Deaths vs GDP Growth") +
  theme_minimal()

# Show all 4 models together with log transforms
(lp1 | lp2) / (lp3 | lp4)


# Regression model (GDP) log transformation
model_1D_log <- lm(
  GDP_growth ~ 
    avg_workplace_closure +
    log(annual_vaccinations_per_million + 1) +
    log(annual_tests_per_thousand + 1) +
    log(covid_deaths_per_million + 1) +
    human_development_index,
  data = master
)
summary(model_1D_log)
vif(model_1D_log)


# Regression model (CPI) log transformation
model_1D_CPI_log <- lm(
  CPI_growth ~ 
    avg_workplace_closure +
    log(annual_vaccinations_per_million + 1) +
    log(annual_tests_per_thousand + 1) +
    log(covid_deaths_per_million + 1) +
    human_development_index,
  data = master
)


summary(model_1D_CPI_log)
vif(model_1D_CPI_log)
