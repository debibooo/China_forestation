
library(readxl)
library(dplyr)
library(purrr)
library(ggplot2)
library(viridis)
library(ggpubr)
library(cowplot)
library(ggsci)
library(viridisLite)
library(colorspace)


my_colors <- c(
  "#f8d1b4",  
  "#f7bf9e",  
  "#f69b72",  
  "#e7bc91",  
  "#d4a276",  
  "#b08968",  
  "#ffdda1",  
  "#ffd151",  
  "#d5a021",  
  "#cfe1b9",  
  "#b5c99a", 
  "#97a97c",  
  "#c5dbf0",  
  "#96b8db",  
  "#6083c5",   
  "#a8dadc",  
  "#457b9d", 
  "#314A5A",  
  "#dec9e9",  
  "#c19ee0", 
  "#76618F" 
)


folder  <- "/foodconsumption"
files4 <- file.path(folder, c(
  "NetZero.xlsx",
  "Afforest.xlsx",
  "DietShift.xlsx",
  "YieldUp.xlsx",
  "SusCrop.xlsx"
))

last_f <- file.path(folder, "reference.xlsx")
last_name <- tools::file_path_sans_ext(basename(last_f))

custom_titles <- setNames(
  c(
    "NetZero",
    "Afforest",
    "DietShift",
    "YieldUp",
    "SusCrop",
    "REF"),
  c(  "NetZero",
    "Afforest",
     "DietShift",
      "YieldUp",
      "SusCrop",
    last_name)
)


data4 <- map_dfr(files4, function(f) {
  read_xlsx(f) %>%
    select(Year, technology, delta) %>%
    mutate(file = tools::file_path_sans_ext(basename(f)))
}) %>%
  filter(Year >= 2015)

file_levels <- c(tools::file_path_sans_ext(basename(files4)), last_name)
data4$file    <- factor(data4$file, levels = file_levels)

net_data <- data4 %>%
  group_by(file, Year) %>%
  summarise(net_delta = sum(delta), .groups = "drop")

val_data <- read_xlsx(last_f) %>%
  select(Year, technology, value) %>%
  filter(Year >= 2015) %>%
  mutate(file = last_name)

min_year <- 2015
max_year <- 2100

xbreaks1 <- seq(2020, max_year, by = 40)
xbreaks2 <- seq(2020, max_year, by = 20)

p_area_leg <- ggplot(data4, aes(Year, delta, fill = technology)) +
  geom_area(alpha = .8, color = NA) +
  geom_hline(yintercept=0, alpha = .8, color = "grey",size=0.8) +
  geom_line(
    data = net_data,
    aes(Year, net_delta),
    color       = "black",
    linetype    = "dashed",
    size        = 1.2,
    inherit.aes = FALSE
  ) +
  facet_wrap(
    ~ file,
    nrow = 1,
    ncol =5,
    scales   = "free_y",
    labeller = as_labeller(custom_titles, default = label_parsed)
  ) +
  scale_x_continuous(limits = c(min_year, max_year),
                     breaks = xbreaks1) +
  scale_fill_viridis_d(option = "turbo",  begin = .1, end = .9,drop = FALSE) +
  labs(x = "Year", y = "Pcal", fill = "Food_category") +
  theme_minimal(base_family = "Arial") +
  theme(
    panel.grid      = element_blank(),
    panel.border    = element_rect(color = "black", fill = NA),
    strip.text      = element_text(face = "bold", size = 8),
    axis.title      = element_text(family = "Arial",size=8),
    axis.text       = element_text(family = "Arial",size=8),
    axis.text.x     = element_text( margin = margin(t = 8)),
    axis.title.y    = element_blank(),
    legend.position = "none"
  )

p_value <- ggplot(val_data, aes(Year, value, fill = technology)) +
  geom_area(alpha = .8, color = NA) +
  facet_wrap(
    ~ file,
    nrow     = 1,
    scales   = "fixed",
    labeller = as_labeller(custom_titles, default = label_parsed)
  ) +
  scale_x_continuous(limits = c(min_year, max_year),
                     breaks = xbreaks1) +
  scale_fill_viridis_d(option = "turbo", begin = .1, end = .9) +
  labs(x = "Year", y = "Pcal", fill = "Food_category") +
  guides(fill = guide_legend(nrow =3 , byrow = FALSE)) +
  theme_minimal(base_family = "Arial") +
  theme(
    panel.grid      = element_blank(),
    panel.border    = element_rect(color = "black", fill = NA),
    strip.text      = element_text(face = "bold", size = 8),
    axis.title      = element_text(family = "Arial",size=8),
    axis.text       = element_text(family = "Arial",size=8),
    axis.text.x     = element_text(margin = margin(t = 7)),
    legend.direction = "vertical",
    legend.key.width = unit(0.6, "cm"),
    legend.text     = element_text(family = "Arial", size = 8),
    legend.title    = element_text(family = "Arial", size = 8)
  )


combined <- ggarrange(
  p_value, p_area_leg,
  ncol          = 2,
  widths        = c(0.3, 0.9),
  common.legend = TRUE,
  legend        = "bottom"
)

print(combined)

ggsave(
  filename = "S5_Foodconsumption.png"
)




