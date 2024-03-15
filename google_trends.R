# Google Trends Data with Foreign Policy Examples
# William Christiansen, Ph.D.
# 3/14/24

# install and load packages
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("corrplot")
library(corrplot)


# load data
geo_dat <- read.csv("geoMap.csv")
time_dat <- read.csv("multiTimeline.csv")

# inspect structure of each data frame
str(geo_dat)
str(time_dat)

# reshape and clean geo dat
# Assuming geo_dat is your geographic data frame
geo_dat_long <- geo_dat %>%
  pivot_longer(cols = -Country, names_to = "Keyword", values_to = "Interest") %>%
  mutate(Interest = as.numeric(gsub("%", "", Interest))) %>%
  filter(!is.na(Interest)) %>%
  mutate(Keyword = gsub("\\.\\.\\.\\d+\\.\\d+\\.\\d+\\.\\.\\d+\\.\\d+\\.\\d+\\.", "", Keyword))

# Assuming geo_dat_long is your reshaped and cleaned geographic data
top_geo_dat <- geo_dat_long %>%
  group_by(Keyword) %>%
  top_n(10, Interest) %>%
  ungroup() %>%
  arrange(Keyword, desc(Interest))


# plot geo dat

ggplot(top_geo_dat, aes(x = Country, y = Interest, fill = Keyword)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 10 Countries by Interest and Keyword", x = "Country", y = "Interest") +
  scale_fill_brewer(palette = "Set3") +
  facet_wrap(~Keyword, scales = "free")


# reshape and prepare timeline data
time_dat_long <- time_dat %>%
  pivot_longer(cols = -Day, names_to = "Keyword", values_to = "Interest") %>%
  mutate(Day = as.Date(Day)) %>%
  mutate(Keyword = gsub("\\.\\.\\.Worldwide\\.", "", Keyword))


# plot
ggplot(time_dat_long, aes(x = Day, y = Interest, color = Keyword)) +
  geom_line() +
  theme_light() +
  labs(title = "Interest Over Time", x = "Date", y = "Interest") +
  scale_color_brewer(palette = "Set1")




# Other cool plots

# top_geo_dat contains the top countries and their interests
ggplot(top_geo_dat, aes(x = Keyword, y = reorder(Country, Interest), fill = Interest)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Search Interest Heatmap by Country and Keyword", x = "Keyword", y = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# reshaping for correlation analysis
time_dat_wide <- spread(time_dat_long, Keyword, Interest)

# calculating correlation
corr_matrix <- cor(time_dat_wide[, -1], use = "complete.obs")

# plotting
corrplot(corr_matrix, method = "circle")




