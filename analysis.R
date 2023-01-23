
# Setup ------------------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(maps)
library(usdata)
library(viridis)


## Load Data -------------------------------------------------------------------

county_level <- read.csv("https://github.com/vera-institute/incarceration_trends/blob/master/incarceration_trends.csv?raw=true")

## Data Filtering & Setup ------------------------------------------------------

# Selecting Relevant Columns and Filter data between 1990 - 2016
county_level_filt <- county_level %>%
  select(
    year, state, county_name, total_pop, total_pop_15to64, aapi_pop_15to64,
    black_pop_15to64, latinx_pop_15to64, native_pop_15to64,
    white_pop_15to64, total_prison_pop, aapi_prison_pop, black_prison_pop,
    latinx_prison_pop, native_prison_pop, other_race_prison_pop,
    white_prison_pop
  ) %>%
  filter(year %in% (1990:2016))

# Summarize Data by State
data_as_state <- county_level_filt %>%
  select(-county_name) %>%
  group_by(state) %>%
  group_by(year, .add = TRUE) %>%
  summarise_all(list(sum), na.rm = TRUE, )

# Summarize Data Nationally by Year
data_as_national <- county_level_filt %>%
  select(-county_name, -state) %>%
  group_by(year) %>%
  summarise_all(list(sum), na.rm = TRUE, ) %>%
  mutate(
    percent_of_pop = (total_prison_pop / total_pop_15to64) * 100,
    perc_aapi = (aapi_prison_pop / total_prison_pop) * 100,
    perc_black = (black_prison_pop / total_prison_pop) * 100,
    perc_latinx = (latinx_prison_pop / total_prison_pop) * 100,
    perc_native = (native_prison_pop / total_prison_pop) * 100,
    perc_white = (white_prison_pop / total_prison_pop) * 100
  )


# Section: 1 Summary Statistics ------------------------------------------------

# State With Highest Percent In Prison

## Total
### Name of State
state_highest_prison_prop <- data_as_state %>%
  ungroup() %>%
  filter(year == 2016) %>%
  select(state, total_pop_15to64, total_prison_pop) %>%
  mutate(prop_prison = (total_prison_pop / total_pop_15to64) * 100) %>%
  filter(prop_prison == max(prop_prison)) %>%
  pull(state) %>%
  abbr2state()

### Percentage
state_num_highest_prison_prop <- data_as_state %>%
  ungroup() %>%
  filter(year == 2016) %>%
  select(state, total_pop_15to64, total_prison_pop) %>%
  mutate(prop_prison = (total_prison_pop / total_pop_15to64) * 100) %>%
  filter(prop_prison == max(prop_prison, na.rm = T)) %>%
  pull(prop_prison) %>%
  round(digits = 2) %>%
  paste0("%")

## Black
### Name of State
state_high_prison_black <- data_as_state %>%
  ungroup() %>%
  filter(year == 2016) %>%
  select(state, black_pop_15to64, black_prison_pop) %>%
  mutate(prop_prison = (black_prison_pop / black_pop_15to64) * 100) %>%
  filter(prop_prison == max(prop_prison)) %>%
  pull(state) %>%
  abbr2state()

### Percentage
state_num_high_prison_black <- data_as_state %>%
  ungroup() %>%
  filter(year == 2016) %>%
  select(state, black_pop_15to64, black_prison_pop) %>%
  mutate(prop_prison = (black_prison_pop / black_pop_15to64) * 100) %>%
  filter(prop_prison == max(prop_prison, na.rm = T)) %>%
  pull(prop_prison) %>%
  round(digits = 2) %>%
  paste0("%")

# County With Highest Percent In Prison
## Name of County
county_highest_prison_prop <- county_level_filt %>%
  filter(year == 2016) %>%
  select(state, county_name, total_pop_15to64, total_prison_pop) %>%
  mutate(
    prop_prison = (total_prison_pop / total_pop_15to64) * 100,
    location = paste0(county_name, ", ", state)
  ) %>%
  filter(prop_prison == max(prop_prison, na.rm = T)) %>%
  pull(location)

## Percentage
county_num_highest_prison_prop <- county_level_filt %>%
  filter(year == 2016) %>%
  select(state, county_name, total_pop_15to64, total_prison_pop) %>%
  mutate(
    prop_prison = (total_prison_pop / total_pop_15to64) * 100,
    location = paste0(county_name, ", ", state)
  ) %>%
  filter(prop_prison == max(prop_prison, na.rm = T)) %>%
  pull(prop_prison) %>%
  round(digits = 2) %>%
  paste0("%")

# Year Prison Population Peaked
max_prison_pop_df <- data_as_national %>%
  filter(total_prison_pop == max(total_prison_pop, na.rm = T))

year_max_prison <- max_prison_pop_df %>% pull(year)

total_max_year <- max_prison_pop_df %>%
  pull(total_prison_pop) %>%
  prettyNum(big.mark = ",", scientific = FALSE)

black_pop_max_year <- max_prison_pop_df %>%
  pull(black_prison_pop) %>%
  prettyNum(big.mark = ",", scientific = FALSE)

white_pop_max_year <- max_prison_pop_df %>%
  pull(white_prison_pop) %>%
  prettyNum(big.mark = ",", scientific = FALSE)

# Prison Population Statistics for 2016
total_pop <- data_as_national %>%
  filter(year == 2016) %>%
  pull(total_prison_pop) %>%
  prettyNum(big.mark = ",", scientific = FALSE)

total_black_pop <- data_as_national %>%
  filter(year == 2016) %>%
  pull(black_prison_pop) %>%
  prettyNum(big.mark = ",", scientific = FALSE)

total_white_pop <- data_as_national %>%
  filter(year == 2016) %>%
  pull(white_prison_pop) %>%
  prettyNum(big.mark = ",", scientific = FALSE)

# Section: 2 Trends over time chart --------------------------------------------

## Calculate Variables --------------------------------------------------------
prison_demographic_df <- data_as_national %>%
  select(year, perc_aapi, perc_black, perc_latinx, perc_native, perc_white) %>%
  gather(key = "Race", value = "percent", -year)

## Plot -----------------------------------------------------------------------
# Plot showing Change in Prison Demographics overtime
prison_demographic_plot <- ggplot(prison_demographic_df, aes(
  x = year,
  y = percent,
  color = Race
)) +
  geom_line(size = 1.5, alpha = .6) +
  scale_color_discrete(
    name = "Race",
    labels = c(
      "Asian American & Pacific Islander",
      "African American", "Latinx",
      "Native American", "White"
    )
  ) +
  ggtitle("Change In Prison Demographics Over Time",
    subtitle = "Between 1990 - 2016"
  ) +
  labs(x = "Year", y = "Percentage (%)") +
  theme_minimal() +
  theme(
    legend.position = "bottom", plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))


# Section: 3 Variable comparison chart -----------------------------------------

## Select Relevant Variables ---------------------------------------------------
percent_popvblack_df <- data_as_national %>% select(
  year,
  black_prison_pop,
  percent_of_pop,
  white_prison_pop
)

## Plots -----------------------------------------------------------------------

# Black Prison Population Plot
black_pop_plot <- ggplot(percent_popvblack_df, aes(
  x = percent_of_pop,
  y = black_prison_pop,
  color = year
)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = lm) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(
    x = "Percent of Total Population in Prison (%)",
    y = "Black Prison Population"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

# White Prison Population Plot
white_pop_plot <- ggplot(percent_popvblack_df, aes(
  x = percent_of_pop,
  y = white_prison_pop,
  color = year
)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = lm, color = "red") +
  scale_color_gradient(low = "blue", high = "red") +
  labs(
    x = "Percent of Total Population in Prison (%)",
    y = "White Prison Population", color = "Year"
  ) +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))


# Combined Black and White Prison Population Plot
combined_plot_blackvwhite <- black_pop_plot + white_pop_plot +
  plot_annotation(title = paste0("White vs Black Prison Population against ",
  "Percent of Total Population In Prison"))

# Section: 4 Map ---------------------------------------------------------------

## Calculate Variables ---------------------------------------------------------

# Calc Percentages
black_prison_to_pop_dem_df <- data_as_state %>%
  select(
    year, state, total_pop_15to64, black_pop_15to64, total_prison_pop,
    black_prison_pop
  ) %>%
  filter(year == 2016) %>%
  mutate(
    black_pop_percent = (black_pop_15to64 / total_pop_15to64) * 100,
    black_prison_percent = (black_prison_pop / total_prison_pop) * 100,
    ratio_prison_to_dem = black_prison_percent / black_pop_percent
  )

# pullout state and ratio_prison_to_dem Variables and sort descending order
ratio_black_prison_state_pop <- black_prison_to_pop_dem_df %>%
  ungroup() %>%
  select(state, ratio_prison_to_dem) %>%
  arrange(desc(ratio_prison_to_dem))

# Insight and Statistics

most_disprop_state <- ratio_black_prison_state_pop %>%
  filter(ratio_prison_to_dem == max(ratio_prison_to_dem, na.rm = TRUE)) %>%
  pull(state) %>%
  abbr2state()

disprop_state_ratio <- ratio_black_prison_state_pop %>%
  filter(ratio_prison_to_dem == max(ratio_prison_to_dem, na.rm = TRUE)) %>%
  pull(ratio_prison_to_dem) %>%
  round(digits = 0)

second_most_disprop_state <- ratio_black_prison_state_pop %>%
  filter(row_number() == 2) %>%
  pull(state) %>%
  abbr2state()

third_most_disprop_state <- ratio_black_prison_state_pop %>%
  filter(row_number() == 3) %>%
  pull(state) %>%
  abbr2state()

## Setup Map -------------------------------------------------------------------

# Custom Blank Theme

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(), # remove axis lines
    axis.text = element_blank(), # remove axis labels
    axis.ticks = element_blank(), # remove axis ticks
    axis.title = element_blank(), # remove axis titles
    plot.background = element_blank(), # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank(), # remove border around plot
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 22),
    plot.subtitle = element_text(hjust = 0.5)
  )

# State Shapes and Join to Data
state_shape <- map_data("state") %>%
  rename(state = region) %>%
  mutate(state = state2abbr(state)) %>%
  left_join(ratio_black_prison_state_pop, by = "state")

# Plot Map

black_ration_map <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = ratio_prison_to_dem),
    color = "grey",
    size = 0.2
  ) +
  coord_map() +
  scale_fill_viridis(
    option = "mako", direction = -1,
    guide = guide_colourbar(
      direction = "horizontal",
      barheight = 0.7,
      barwidth = 20,
      draw.ulim = F,
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = 0.5
    )
  ) +
  ggtitle("Overrepresentation of African Americans in Prison",
    subtitle = paste0("Ratio of Black Prison Population Percent ",
                      "/ Black Population Percent in 2016 By State")
  ) +
  labs(fill = "Ratio") +
  blank_theme
