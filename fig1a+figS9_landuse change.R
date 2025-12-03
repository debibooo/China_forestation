
library(tidyverse)
library(readxl)
library(ggpubr)

sheets <- c("delta2030_2", "delta2060_2", "delta2100_2")
year_labels <- c("2030", "2060", "2100")


landuse_columns <- read_excel(
  "relative_luc_plot.xlsx",
  sheet = sheets[1],
  n_max = 0
) %>%
  names() %>%
  setdiff("scenario")


landuse_long <- map2_dfr(sheets, year_labels, ~ {
  read_excel(
    "relative_luc_plot.xlsx",
    sheet = .x,
    n_max = 3
  ) %>%
    mutate(scenario = factor(scenario, levels = unique(scenario))) %>%
    pivot_longer(-scenario, names_to = "variable", values_to = "value") %>%
    mutate(
      variable = factor(variable, levels = landuse_columns),
      year = .y
    )
})

shared_theme <- theme_minimal(base_family = "Arial", base_size = 7) +  
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 7),
    legend.text = element_text(size = 7)
  )


plots <- landuse_long %>%
  split(.$year) %>%
  map2(c(FALSE, FALSE, FALSE), ~ {
    ggplot(.x, aes(x = scenario, y = value, fill = variable)) +
        geom_bar(stat = "identity", position = "stack") +
        coord_flip()  +
      scale_y_continuous(
        limits = c(-200,200),
        expand = c(0,0)
      ) +
      labs(
          x = NULL,
          y = if (.y) NULL else "Land use change (Mha)" 
        ) +
        scale_fill_brewer(palette = "RdYlGn",name = NULL ) +
        shared_theme +
        theme(
          legend.position = "none",
          legend.text = element_text(size = 7),
          axis.text.x = if (.y) element_blank() else element_text(size = 7),
          axis.title.x = element_blank() ,
          plot.margin = margin(2, 8, 2, 4)  
        )
      })


legend_plot <- ggplot(landuse_long, aes(x = scenario, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "RdYlGn")  + theme_minimal(base_family = "Arial", base_size = 7) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 7)
  )

legend <- get_legend(legend_plot)

final_plot <- ggarrange(plotlist = plots,
                        ncol = 1,
                        heights = c(1, 1, 1),
                        common.legend = TRUE,
                        legend = "bottom",
                        labels = c("2030", "2060", "2100"),  
                        font.label = list(size = 8, face = "bold"),
                        hjust = -3,                       
                        vjust = 3                          )
                    


print(final_plot)

plot_path <- "XXX"
ggsave(file.path(plot_path, "XXX.png"), final_plot, dpi = 600)

