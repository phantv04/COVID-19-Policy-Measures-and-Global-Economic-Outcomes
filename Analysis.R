library(dplyr)  
library(lmtest) 
library(lme4)
library(sandwich)
library(ggplot2)

df <- COVIDYears

# Correlation Matrix
cor_matrix <- df %>%
  select_if(is.numeric) %>%
  cor(use = "pairwise.complete.obs")
print(cor_matrix)

library(reshape2)
melted <- melt(cor_matrix)
ggplot(data = melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Regression: GDP
model_gdp <- lm(GDP_growth ~ annual_vaccinations_per_million +
                  annual_tests_per_thousand +
                  avg_workplace_closure +
                  covid_deaths_per_million,
                data = df)
coeftest(model_gdp, vcov = vcovHC(model_gdp, type = "HC1"))

# Regression: CPI
model_cpi <- lm(CPI_growth ~ annual_vaccinations_per_million +
                  annual_tests_per_thousand +
                  avg_workplace_closure +
                  covid_deaths_per_million,
                data = df)
coeftest(model_cpi, vcov = vcovHC(model_cpi, type = "HC1"))


# Regression: CPI (Africa)
selected_countries <- c("Zimbabwe", "Angola", "Botswana", "Ethiopia", "Guinea", "Kenya")

df_selected <- df %>%
  filter(
    Country %in% selected_countries,
    Year >= 2020 & Year <= 2022
  )

model_cpi_countries <- lm(
  CPI_growth ~ annual_vaccinations_per_million +
    annual_tests_per_thousand +
    avg_workplace_closure +
    covid_deaths_per_million,
  data = df_selected
)
coeftest(model_cpi_countries, vcov = vcovHC(model_cpi_countries, type = "HC1"))

# Regression: CPI (Asia)
selected_countries <- c("China", "India", "Mongolia", "Philippines", "Thailand", "Viet Nam")

df_selected <- df %>%
  filter(
    Country %in% selected_countries,
    Year >= 2020 & Year <= 2022
  )

model_cpi_countries <- lm(
  CPI_growth ~ annual_vaccinations_per_million +
    annual_tests_per_thousand +
    avg_workplace_closure +
    covid_deaths_per_million,
  data = df_selected
)
coeftest(model_cpi_countries, vcov = vcovHC(model_cpi_countries, type = "HC1"))

# Random intercepts by country: GDP
model_gdp_re <- lmer(GDP_growth ~ annual_vaccinations_per_million +
                       annual_tests_per_thousand +
                       avg_workplace_closure +
                       covid_deaths_per_million +
                       (1 | Country),
                     data = df)
summary(model_gdp_re)

# Random intercepts by country: CPI
model_cpi_re <- lmer(CPI_growth ~ annual_vaccinations_per_million +
                       annual_tests_per_thousand +
                       avg_workplace_closure +
                       covid_deaths_per_million +
                       (1 | Country),
                     data = df)
summary(model_cpi_re)
