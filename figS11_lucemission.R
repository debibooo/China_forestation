library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

scenarios <- c("REF", "NetZero", "Afforest", "DietShift", "YieldUp", "SusCrop")


data_all <- lapply(scenarios, function(sheet) {
  df <- read_excel("compare.xlsx", sheet = sheet)
  df <- df %>%
    pivot_longer(-1, names_to = "Year", values_to = "delta") %>%
    rename(category = 1) %>%
    mutate(
      Year = as.numeric(Year),
      file = sheet
    )
  return(df)
}) %>% bind_rows()

data_all$file <- factor(data_all$file, levels = c("REF", "NetZero", "Afforest", "DietShift", "YieldUp", "SusCrop"))


desired_order <- c(
  "StapleCrops", "CashCrops", "FruitVeg", "OtherCrops", "Pasture", "Grassland",
  "ManagedForest", "UnmanagedForest", "Legumes", "ShrubTundra", "Urban", "Biomass"
)
data_all$category <- factor(data_all$category, levels = desired_order)


net_data <- data_all %>%
  group_by(file, Year) %>%
  summarise(net_delta = sum(delta, na.rm = TRUE), .groups = "drop")


min_year <- min(data_all$Year, na.rm = TRUE)
max_year <- max(data_all$Year, na.rm = TRUE)
xbreaks1 <- seq(min_year, max_year, by = 40)

custom_titles <- c(
  REF = "REF",
  NetZero = "NetZero",
  Afforest = "Afforest",
  DietShift = "DietShift",
  YieldUp = "YieldUp",
  SusCrop = "SustCrop"
)


p_area_leg <- ggplot(data_all, aes(x = Year, y = delta, fill = category)) +
  geom_area(alpha = 0.8, color = NA) +
  geom_hline(yintercept = 0, color = "grey", size = 0.8, alpha = 0.8) +
  geom_line(
    data = net_data,
    aes(x = Year, y = net_delta),
    color = "black",
    linetype = "dashed",
    size = 1.2,
    inherit.aes = FALSE
  ) +
  facet_wrap(
    ~ file,
    nrow = 1,
    ncol = 6,
    scales =  "fixed",
    labeller = as_labeller(custom_titles)
  ) +
  scale_x_continuous(limits = c(min_year, max_year), breaks = xbreaks1) +
  scale_y_continuous(limits = c(-500, 500), breaks = seq(-500, 500, 200))+
  scale_fill_viridis_d(option = "viridis", begin = 0.1, end = 0.9, drop = FALSE) +
  labs(
    x = "Year",
    y = "LUC emission (Mt C/yr)",
    fill = "Land_category"
  ) +
  theme_minimal(base_family = "Arial") +
  theme(
    text = element_text(family = "Arial", size = 8),
    panel.grid      = element_blank(),
    panel.border    = element_rect(color = "black", fill = NA),
    strip.text      = element_text(face = "bold", size = 8),
    axis.title      = element_text(size = 8),
    axis.text       = element_text(size = 7,angle = 30, hjust = 1),
    axis.text.x     = element_text(size = 7),
    legend.position = "bottom"
  )+ guides(
    fill = guide_legend(title.position = "top",nrow = 2)
  )

print(p_area_leg)


ggsave("LUC_emissions_area_plot.png", 
       p_area_leg, 
       device   = "png",           
       dpi      = 600 )                  
