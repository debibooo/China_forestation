
library(readxl)
library(dplyr)
library(purrr)
library(ggplot2)
library(viridis)
library(patchwork)


folder <- "fooddemand/total"  

files <- list.files(folder, pattern = "\\.xlsx$", full.names = TRUE)

data_all <- map_dfr(files, function(f) {
  read_xlsx(f) %>%
    filter(input %in% c("FoodDemand_NonStaples", "FoodDemand_Staples")) %>%
    select(Year, input, value)
}) %>%
  filter(Year >= 2015)

stats <- data_all %>%
  group_by(input, Year) %>%
  summarise(
    ymin   = min(value),
    ymax   = max(value),
    median = median(value),
    .groups = "drop"
  )

p <- ggplot(stats, aes(x = Year)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax),
              fill = "grey70", alpha = 0.4) +
  geom_line(aes(y = median),
            color = "black", size = 1) +
  facet_wrap(~ input,
             nrow = 1,
             scales = "fixed",
             labeller = labeller(input = c(
               FoodDemand_NonStaples = "Non-Staples",
               FoodDemand_Staples    = "Staples"
             ))) +
  labs(
    x = "Year",
    y = "Pcal/yr"
  ) +
  theme_minimal(base_family = "Arial") +
  theme(
    strip.text    = element_text(size = 12, face = "bold"),
    plot.title    = element_text(hjust = 0.5),
    panel.grid.minor     = element_blank(),
    panel.border  = element_rect(color = "black", fill = NA, size = 0.5),
    axis.title    = element_text(family = "Arial"),
    axis.text     = element_text(family = "Arial")
  )

print(p)

ggsave(
  filename = "/S4_Fooddemand.png")



