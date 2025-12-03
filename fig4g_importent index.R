library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(purrr)


path   <- "sustainability_3otherindex.xlsx"
sheets <- c("M1_forest", "M2_lulucf", "Es3_Cuptake", "F2_hunger")

scenario_colors <- c(
  REF       = "#0077b6",  
  NetZero   = "#f08080",  
  Afforest  = "#7CBB6C",  
  DietShift = "#a8dadc",  
  YieldUp   = "#b39cd0",  
  SusCrop   = "#f9e79f"   
)
y_titles_expr <- list(
  M1_forest   = expression("Forest carbon emission (Pg " * C * ")"),
  M2_lulucf   = expression("LULUCF excl. forest emission (Pg " * C * ")"),
  Es3_Cuptake = expression("Net C uptake (Pg " * C * ")"),
  F2_hunger   = expression("Hunger population (Million)")
)


read_and_pivot_periods <- function(path, sheet_name) {
  df_wide <- read_xlsx(path, sheet = sheet_name)
  df2 <- df_wide %>%
    rename(
      Scenarios = 1,
      v1        = `2020-2060`,
      v2        = `2060-2100`
    )
  df_long <- df2 %>%
    pivot_longer(
      cols      = c(v1, v2),
      names_to  = "Period",
      values_to = "Value"
    ) %>%
    mutate(
      Period    = factor(Period, levels = c("v1", "v2")),
      Scenarios = factor(Scenarios, levels = names(scenario_colors))
    )
  return(df_long)
}

list_of_dflong <- map(sheets, ~ read_and_pivot_periods(path, .x))


make_stacked_bar <- function(df_long, title_text, y_label_expr) {

  net_values <- df_long %>%
    group_by(Scenarios) %>%
    summarise(net = sum(Value), .groups = "drop")
  

  ggplot(df_long, aes(x = Scenarios, y = Value, fill = Scenarios)) +
    geom_col(
      aes(
        linetype = Period,  
        alpha    = Period   
      ),
      position = "stack",
      color    = NA,
      width    = 0.7
    ) +
    scale_fill_manual(values = scenario_colors) +
    scale_linetype_manual(values = c(v1 = "solid", v2 = "dashed")) +
    scale_alpha_manual(values = c(v1 = 1.0, v2 = 0.5), guide = FALSE) +
    geom_point(
      data  = net_values,
      aes(x = Scenarios, y = net),
      shape = 23, size = 2.5, fill = "black"
    ) +
    labs(
      title = title_text,
      x     = NULL,
      y     = y_label_expr     
    ) +
    theme_minimal(base_family = "Arial", base_size = 7) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.border       = element_rect(color = "darkgray", fill = NA),
      plot.background    = element_blank(),
      axis.text.x        = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y        = element_text(size = 7),
      plot.title         = element_text(face = "bold", size = 9),
      legend.position    = "none"  
    )
}


plots_list <- pmap(
  list(
    df_long       = list_of_dflong,
    title_text    = sheets,
    y_label_expr  = y_titles_expr[sheets]
  ),
  ~ make_stacked_bar(..1, ..2, ..3)
)


panels_only <- ggarrange(
  plotlist      = plots_list,
  ncol          = 4,
  nrow          = 1,
  common.legend = FALSE,
  legend        = "none"
)


print(panels_only)


ggsave(
  filename = "important_index2.png",  
  plot     = panels_only,                   
  device   = "png",
  width    = 180,                  
  height   = 55,                   
  units    = "mm",                
  dpi      = 600                   
)
