# 02-beaune.R ----
# Euro-Climhist Database: https://www.euroclimhist.unibe.ch/project/index_eng.html

## load packages ----
library(tidyverse)

## load data ----
beaune <- readr::read_csv(file = "raw_echexport.csv") |> 
  rename_with(.fn = tolower, .cols = everything()) |> 
  separate(col = data, into = c("description", "doy", "assessment"), sep = ": ") |> 
  separate(col = assessment, into = c("assessment", "location"), sep = "/ ") |> 
  mutate(doy = as.numeric(str_sub(string = doy, start = 1L, end = 3L))) |> 
  mutate(assessment = str_squish(assessment))

write_csv(x = beaune, file = "clean_beaune.csv")

## plot ----
beaune_caption <- "Labbé, T., Pfister, C., Brönnimann, S., Rousseau, D., Franke, J., and Bois, B.: The longest homogeneous series of grape harvest dates, Beaune 1354–2018, and its significance for the understanding of past and present climate, Clim. Past, 15, 1485–1501, https://doi.org/10.5194/cp-15-1485-2019, 2019."

p3 <- ggplot(data = beaune, mapping = aes(x = year, y = doy)) +
  geom_line(linewidth = 0.5, colour = "gray", alpha = 0.25) +
  geom_point(size = 1) +
  geom_smooth(method = "loess", formula = "y ~ x", se = TRUE, colour = "#800020", fill = "#800020", span = 0.1, alpha = 0.2) +
  scale_x_continuous(breaks = seq(1350, 2050, 50), limits = c(1350, 2025)) +
  labs(
    title = "Begin of grape harvest (day of year) in Beaune, France, 1354-2018",
    subtitle = NULL,
    x = NULL, y = NULL,
    caption = str_wrap(beaune_caption)
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

print(p3)

ggsave(filename = "fig_beaune.png", plot = p3, path = ".", width = 25, height = 12, units = "cm", bg = "white")
