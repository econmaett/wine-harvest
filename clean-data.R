# clan-data.R ----
## load packages ----
library(tidyverse)

## load wide data ----
# Source: https://doi.org/10.5194/cp-8-1403-2012
# GHD-RCS : dates in number of days after August 31. 
wineharvest_wide <- readxl::read_excel(
  path = "Supplement.xlsx", 
  sheet = "S2. GHD-RCS", 
  skip = 2
  ) |> 
  rename_with(tolower, .cols = everything()) |> 
  rename(year = `abb.`)

write_csv(x = wineharvest_wide, file = "wineharvest_wide.csv")

## abbreviations----
locations <- readxl::read_excel(
  path = "Supplement.xlsx", 
  sheet = "S2. GHD-RCS", 
  skip = 1
  ) |> 
  select(-c("...1")) |> 
  mutate(across(.cols = everything(), .fns = tolower)) |> 
  slice_head(n = 1) |> 
  pivot_longer(cols = Alsace:`Vendée - Poitou Charente`, names_to = "name", values_to = "abbreviation") |> 
  relocate(abbreviation, name)

write_csv(x = locations, file = "locations.csv")

## create long data ----
wineharvest_long <- wineharvest_wide |> 
  pivot_longer(cols = -c(year), names_to = "abbreviation", values_to = "value")

wineharvest_long <- left_join(x = wineharvest_long, y = locations, by = join_by(abbreviation)) |> 
  relocate(abbreviation, name, year, value)

write_csv(x = wineharvest_long, file = "wineharvest_long.csv")

## plot ----
wineharvest_caption <- "Daux, V., Garcia de Cortazar-Atauri, I., Yiou, P., Chuine, I., Garnier, E., Le Roy Ladurie, E., Mestre, O., and Tardaguila, J.: An open-access database of grape harvest dates for climate research: data description and quality assessment, Clim. Past, 8, 1403–1418, https://doi.org/10.5194/cp-8-1403-2012, 2012."

p <- ggplot(data = wineharvest_wide, mapping = aes(x = year, y = swi)) +
  geom_hline(yintercept = 0, linewidth = 0.5, colour = "darkgray") +
  geom_line(linewidth = 0.5, colour = "gray", alpha = 0.25) +
  geom_point(size = 1) +
  geom_smooth(method = "loess", formula = "y ~ x", se = TRUE, colour = "#800020", fill = "#800020", span = 0.1, alpha = 0.2) +
  scale_x_continuous(breaks = seq(1450, 2050, 50), limits = c(1450, NA)) +
  # scale_x_continuous(breaks = seq(1350, 2050, 50)) +
  scale_y_continuous(breaks = seq(-10, 70, 20)) +
  labs(
    title = "Annual wine harvest in Switzerland (Leman Lake",
    subtitle = "Number of days after August 31",
    x = NULL, y = NULL,
    caption = str_wrap(wineharvest_caption)
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

print(p)

ggsave(filename = "switzerland.png", plot = p, path = ".", width = 24, height = 12, units = "cm", bg = "white")

variable_selection <- c("bur", "jur", "srv", "swi")

p <- ggplot(data = wineharvest_long |> filter(abbreviation %in% variable_selection), mapping = aes(x = year, y = value)) +
  # geom_hline(yintercept = 0, linewidth = 0.5, colour = "darkgray") +
  geom_line(linewidth = 0.5, colour = "gray", alpha = 0.25) +
  geom_point(size = 1) +
  geom_smooth(method = "loess", formula = "y ~ x", se = TRUE, colour = "#800020", fill = "#800020", span = 0.1, alpha = 0.2) +
  facet_wrap(facets = vars(name), ncol = 1, scale = "free_y") +
  scale_x_continuous(breaks = seq(1350, 2050, 50)) +
  # scale_y_continuous(breaks = seq(-10, 70, 20)) +
  labs(
    title = "Annual wine harvest, number of days after August 31",
    subtitle = NULL,
    x = NULL, y = NULL,
    caption = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank())

print(p)

ggsave(filename = "wineharvest.png", plot = p, path = ".", width = 20, height = 24, units = "cm", bg = "white")
