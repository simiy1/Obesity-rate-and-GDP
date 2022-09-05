library("dplyr")
library("tidyr")
library("ggplot2")

# Obesity Data

# Import datasets
Obesity_countries <- read.csv("data/obesity-cleaned.csv")

Obesity_GDP_USA <- read.csv("data/Obesity_GDP_USA.csv")

# Drop the unuseful columns
Obesity_countries <- select(Obesity_countries, Country:Sex)

sample_obesity <- head(Obesity_countries, 5)
str(Obesity_countries)
Obesity_countries$Obesity.... <- sub('\\s*\\[.*$', '', Obesity_countries$Obesity....)
Obesity_countries$Country <- as.character(Obesity_countries$Country)
Obesity_countries$Sex <- as.character(Obesity_countries$Sex)
sample_obesity <- head(Obesity_countries, 5)
num_countries <- Obesity_countries %>% summarise(total = n_distinct(Country)) %>% pull(total)
year_range <- range(Obesity_countries$Year)
sex_list <- c(unique(Obesity_countries$Sex))
as.num <- function(x, na.strings = "NA") {
  stopifnot(is.character(x))
  na = x %in% na.strings
  x[na] = 0
  x = as.numeric(x)
  x[na] = NA_real_
  x
}
Obesity_countries$Obesity.... <- as.num(Obesity_countries$Obesity...., na.strings="No data")
mean_obesity <- Obesity_countries %>%
  filter(Year == max(Year)) %>%
  replace(is.na(.), 0) %>%
  summarise(mean = mean(Obesity....))%>%
  pull(mean)

max_obesity <- Obesity_countries %>%
  filter(Year == max(Year)) %>%
  replace(is.na(.), 0) %>%
  summarise(max = max(Obesity....))%>%
  pull(max)

country_max_obesity  <- Obesity_countries %>%
  replace(is.na(.), 0) %>%
  filter(Year == max(Year)) %>%
  filter(Obesity.... == max_obesity) %>%
  pull(Country)

both_sexes_df <- Obesity_countries %>%
  replace(is.na(.), 0) %>%
  filter(Year == max(Year)) %>%
  filter(Sex == "Both sexes") %>%
  group_by(Country)
all_plot <- hist(both_sexes_df$Obesity....)

male_df <- Obesity_countries %>%
  replace(is.na(.), 0) %>%
  filter(Year == max(Year)) %>%
  filter(Sex == "Male") %>%
  group_by(Country)
male_plot <- hist(male_df$Obesity....)

female_df <- Obesity_countries %>%
  replace(is.na(.), 0) %>%
  filter(Year == max(Year)) %>%
  filter(Sex == "Female") %>%
  group_by(Country)
female_plot <- hist(female_df$Obesity....)

obesity_sex_plot <- Obesity_countries %>%
  replace(is.na(.), 0) %>%
  mutate(sex=factor(Sex, levels=c("Both sexes", "Male", "Female"))) %>%
  ggplot(mapping = aes(x = Year, y = Obesity...., color = sex)) + 
  geom_point() + geom_line() + 
  labs(title = "Obesity rate by sex over time for all countries in the world",   
       x = "Year", 
       y = "Obesity",
       color = "Sex" ) +
  scale_color_brewer(palette = "Dark2")


sample_USA <- Obesity_GDP_USA %>% head(5) %>%
  select(State, Year, Adult.Obesity, Poverty.Rate, Real.GDP, Real.GDP.Growth, Real.Personal.Income, Real.GDP.Per.Capita)
str(Obesity_GDP_USA)
year_range_usa <- range(Obesity_GDP_USA$Year)
range_obesity_usa <- range(Obesity_GDP_USA$Adult.Obesity)
range_GDP_usa <- range(Obesity_GDP_USA$Real.GDP)
range_poverty_usa <- range(Obesity_GDP_USA$Poverty.Rate )
range_GDP_growth_usa <- range(Obesity_GDP_USA$Real.GDP.Growth)

mean_recent <- Obesity_GDP_USA %>%
  filter(Year == max(Year)) %>%
  select(Year, Adult.Obesity, Real.GDP, Poverty.Rate, Real.GDP.Growth) %>%
  replace(is.na(.), 0) %>%
  summarise_at(c("Adult.Obesity", "Real.GDP", "Poverty.Rate", "Real.GDP.Growth"), mean)

recent_df <- Obesity_GDP_USA %>%
  filter(Year == max(Year)) 

obesity_plot <- hist(recent_df$Adult.Obesity)
GDP_plot <- hist(recent_df$Real.GDP)
poverty_plot <- hist(recent_df$Poverty.Rate)
GDP_growth_plot <- hist(recent_df$Real.GDP.Growth)

# Question 1
# The relationship between GDP and Obesity Rate in 2017
obesity_2017 <- Obesity_GDP_USA %>%
  filter(Year == 2017) %>%
  select(Region, Adult.Obesity.100, Real.GDP) %>%
  group_by(Region) %>%
  summarize(
    Obesity_rate_by_percent = mean(Adult.Obesity.100),
    GDP_by_10_thousand = mean(Real.GDP/10000)
  ) %>%
  pivot_longer(
    cols = c(Obesity_rate_by_percent, GDP_by_10_thousand),
    names_to = "Types"
  ) %>%
  ggplot(mapping = aes(x = value, y = Region, fill = Types)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  labs(
    title = "Relationship between Obesity Rate and GDP in 2017",
    x = "Value",
    y = "Regions",
    fill = "Types"
  ) + scale_fill_brewer(palette = "Set1", labels = c("GDP (by 10000)", "Obesity Rate (by %)"))

# The relationship between GDP and Obesity Rate in 2016
obesity_2016 <- Obesity_GDP_USA %>%
  filter(Year == 2016) %>%
  select(Region, Adult.Obesity.100, Real.GDP) %>%
  group_by(Region) %>%
  summarize(
    Obesity_rate_by_percent = mean(Adult.Obesity.100),
    GDP_by_10_thousand = mean(Real.GDP/10000)
  ) %>%
  pivot_longer(
    cols = c(Obesity_rate_by_percent, GDP_by_10_thousand),
    names_to = "Types"
  ) %>%
  ggplot(mapping = aes(x = value, y = Region, fill = Types)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  labs(
    title = "Relationship between Obesity Rate and GDP in 2016",
    x = "Value",
    y = "Regions",
    fill = "Types"
  ) + scale_fill_brewer(palette = "Set1", labels = c("GDP (by 10000)", "Obesity Rate (by %)"))

# The relationship between GDP and Obesity Rate in 2015
obesity_2015 <- Obesity_GDP_USA %>%
  filter(Year == 2015) %>%
  select(Region, Adult.Obesity.100, Real.GDP) %>%
  group_by(Region) %>%
  summarize(
    Obesity_rate_by_percent = mean(Adult.Obesity.100),
    GDP_by_10_thousand = mean(Real.GDP/10000)
  ) %>%
  pivot_longer(
    cols = c(Obesity_rate_by_percent, GDP_by_10_thousand),
    names_to = "Types"
  ) %>%
  ggplot(mapping = aes(x = value, y = Region, fill = Types)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  labs(
    title = "Relationship between Obesity Rate and GDP in 2015",
    x = "Value",
    y = "Regions",
    fill = "Types"
  ) + scale_fill_brewer(palette = "Set1", labels = c("GDP (by 10000)", "Obesity Rate (by %)"))

# The relationship between GDP and Obesity Rate in 2014
obesity_2014 <- Obesity_GDP_USA %>%
  filter(Year == 2014) %>%
  select(Region, Adult.Obesity.100, Real.GDP) %>%
  group_by(Region) %>%
  summarize(
    Obesity_rate_by_percent = mean(Adult.Obesity.100),
    GDP_by_10_thousand = mean(Real.GDP/10000)
  ) %>%
  pivot_longer(
    cols = c(Obesity_rate_by_percent, GDP_by_10_thousand),
    names_to = "Types"
  ) %>%
  ggplot(mapping = aes(x = value, y = Region, fill = Types)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  labs(
    title = "Relationship between Obesity Rate and GDP in 2014",
    x = "Value",
    y = "Regions",
    fill = "Types"
  ) + scale_fill_brewer(palette = "Set1", labels = c("GDP (by 10000)", "Obesity Rate (by %)"))

# Question 2

Obesity_countries_40yrs <- Obesity_countries %>%
  group_by(Year, Sex) %>%
  summarize(
    Obesity = mean(Obesity...., na.rm = T)
  ) %>%
  ggplot(mapping = aes(x = Year, y = Obesity, color = Sex)) + 
  geom_line(size = 5)+
  labs(
    title = "The Changes of Obesity Rate from 1975 - 2016",
    x = "Year",
    y = "Obesity Rate, %",
    fill = "Sex"
  )

# Question 3
obesity_rate_Total <- filter(Obesity_countries, Year <= "2016") %>%
  filter(Year >= "2014") %>%
  replace(is.na(.), 0) %>%
  filter(Sex == "Both sexes")

obesity_rate_Total <- group_by(obesity_rate_Total, Year) %>%
  summarise(total_obesity_rate = mean(Obesity....))

obesity_rate_US <- filter(Obesity_GDP_USA, Year <= "2016") %>%
  filter(Year >= "2014") %>%
  replace(is.na(.), 0) 

obesity_rate_US <- group_by(obesity_rate_US, Year) %>%
  summarise(us_obesity_rate = mean(Adult.Obesity.100))

Total_rate <- left_join(obesity_rate_Total, obesity_rate_US)

Total_rate <- Total_rate %>%
  pivot_longer(cols = c(total_obesity_rate, us_obesity_rate), names_to = "Rates")
total_vs_us_plot <- ggplot(data = Total_rate) +
  geom_col(mapping = aes(x = Year, y = value, fill = Rates), position = position_dodge()) +
  scale_fill_manual(values = c("black", "yellow"), 
                    label = c(total_obesity_rate = "Total",us_obesity_rate = "US")) +
  labs(
    title = "Total obesity rate versus us obersity rate from 2014 to 2016",
    x = "year",
    y = "mean obesity rate"
  )

# Question 4
# the prediction of the average GDP in the world in 2015 using us ratio
obesity_rate_in_us_2014 <- mean(filter(Obesity_GDP_USA, Year == "2014") %>%
                                  pull(Adult.Obesity.100))
GDP_in_us_2014 <- mean(filter(Obesity_GDP_USA,Year == "2014") %>%
                         pull(Real.GDP))
GDP_versus_obesity_2014 <- GDP_in_us_2014 / obesity_rate_in_us_2014
obesity_in_countries_2014 <- mean(filter(Obesity_countries, Year == "2014") %>%
                                    replace(is.na(.), 0) %>%
                                    filter(Sex == "Both sexes") %>%
                                    pull(Obesity....))

predicted_GDP_2014 <- GDP_versus_obesity_2014 * obesity_in_countries_2014

obesity_rate_in_us_2015 <- mean(filter(Obesity_GDP_USA, Year == "2015") %>%
                                  pull(Adult.Obesity.100))
GDP_in_us_2015 <- mean(filter(Obesity_GDP_USA,Year == "2015") %>%
                         pull(Real.GDP))
GDP_versus_obesity_2015 <- GDP_in_us_2015 / obesity_rate_in_us_2015
obesity_in_countries_2015 <- mean(filter(Obesity_countries, Year == "2015") %>%
                                    replace(is.na(.), 0) %>%
                                    filter(Sex == "Both sexes") %>%
                                    pull(Obesity....))

predicted_GDP_2015 <- GDP_versus_obesity_2015 * obesity_in_countries_2015

obesity_rate_in_us_2016 <- mean(filter(Obesity_GDP_USA, Year == "2016") %>%
                                  pull(Adult.Obesity.100))
GDP_in_us_2016 <- mean(filter(Obesity_GDP_USA,Year == "2016") %>%
                         pull(Real.GDP))
GDP_versus_obesity_2016 <- GDP_in_us_2016 / obesity_rate_in_us_2016
obesity_in_countries_2016 <- mean(filter(Obesity_countries, Year == "2016") %>%
                                    replace(is.na(.), 0) %>%
                                    filter(Sex == "Both sexes") %>%
                                    pull(Obesity....))

predicted_GDP_2016 <- GDP_versus_obesity_2016 * obesity_in_countries_2016

predict_GDP_df <- data_frame(Year = c("2014", "2015", "2016"),
                             predicted_GDP = c(predicted_GDP_2014, predicted_GDP_2015, predicted_GDP_2016)) 

predict_GDP_plot <- ggplot(data = predict_GDP_df) +
  geom_point(mapping = aes(x = Year, y = predicted_GDP, color = Year)) + 
  labs(
    title = "predicted average GDP Over Time",
    x = "Year",
    y = "predicted GDP"
  )