library(tidyverse)

# _____________________________ GDP ________________________________________ #

# 1. Reshape from wide to long
GDP_long <- GDP %>%
  pivot_longer(
    cols = X2005:X2024,
    names_to = "Year",
    values_to = "GDP"
  )

# 2. Convert Year and GDP to numeric
GDP_long <- GDP_long %>%
  mutate(
    Year = as.numeric(str_replace(Year, "X", "")),  # remove X if present
    GDP = as.numeric(GDP)
  )

# 3. Filter for countries you want to plot & plot GDP
countries_to_plot <- c("Arab World", "Italy", "India", "Canada")
GDP_long_subset <- GDP_long %>%
  filter(`Country.Name` %in% countries_to_plot)
ggplot(GDP_long_subset, aes(x = Year, y = GDP, color = `Country.Name`)) +
  geom_line(size = 1) +
  labs(title = "GDP Over Time", y = "GDP (current US$)") +
  theme_minimal()

countries_to_plot2 <- c("Kuwait", "Uruguay", "Zambia")
GDP_long_subset <- GDP_long %>%
  filter(`Country.Name` %in% countries_to_plot2)
ggplot(GDP_long_subset, aes(x = Year, y = GDP, color = `Country.Name`)) +
  geom_line(size = 1) +
  labs(title = "GDP Over Time", y = "GDP (current US$)") +
  theme_minimal()

# _____________________________ CPI ________________________________________ #

# 1. Reshape from wide to long
CPI_long <- CPI %>%
  pivot_longer(
    cols = X2005:X2024,
    names_to = "Year",
    values_to = "CPI"
  )

# 2. Convert Year and CPI to numeric
CPI_long <- CPI_long %>%
  mutate(
    Year = as.numeric(str_replace(Year, "X", "")),  # remove X if present
    CPI = as.numeric(CPI)
  )

# 3. Filter for countries you want to plot & plot CPI
countries_to_plot <- c("Arab World", "Italy", "India", "Canada")
CPI_long_subset <- CPI_long %>%
  filter(`Country.Name` %in% countries_to_plot)
ggplot(CPI_long_subset, aes(x = Year, y = CPI, color = `Country.Name`)) +
  geom_line(size = 1) +
  labs(title = "CPI Over Time", y = "CPI (annual % in consumer prices)") +
  theme_minimal()

countries_to_plot2 <- c("Kuwait", "Uruguay", "Zambia")
CPI_long_subset <- CPI_long %>%
  filter(`Country.Name` %in% countries_to_plot2)
ggplot(CPI_long_subset, aes(x = Year, y = CPI, color = `Country.Name`)) +
  geom_line(size = 1) +
  labs(title = "CPI Over Time", y = "CPI (annual % in consumer prices)") +
  theme_minimal()
