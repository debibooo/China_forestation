
library(tidyverse)
library(readxl)
library(ggpubr)

# 定义 sheet 名及其标签（如 delta2030 → 2030 年）
sheets <- c("delta2030_2", "delta2060_2", "delta2100_2")
year_labels <- c("2030", "2060", "2100")

# 读取第一个 sheet 的列名，并排除 scenario
landuse_columns <- read_excel(
  "/Volumes/UCL/论文工作/forestation/my_output_folder/forestation/landuse/relative_luc_plot.xlsx",
  sheet = sheets[1],
  n_max = 0
) %>%
  names() %>%
  setdiff("scenario")

# 一次性读取所有 sheet 并设置 variable 为 factor，保持列顺序
landuse_long <- map2_dfr(sheets, year_labels, ~ {
  read_excel(
    "/Volumes/UCL/论文工作/forestation/my_output_folder/forestation/landuse/relative_luc_plot.xlsx",
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
# 定义共享主题
shared_theme <- theme_minimal(base_family = "Arial", base_size = 7) +  # 图内文字 7–8 pt
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    axis.title = element_text(size = 8),
    axis.text = element_text(size = 7),
    legend.text = element_text(size = 7)
  )



# 按年拆分为三个 ggplot 对象
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
          y = if (.y) NULL else "Land use change (Mha)"  # 只在最下方显示 y 轴标题
        ) +
        scale_fill_brewer(palette = "RdYlGn",name = NULL ) +
        shared_theme +
        theme(
          legend.position = "none",
          legend.text = element_text(size = 7),
          axis.text.x = if (.y) element_blank() else element_text(size = 7),
          axis.title.x = element_blank() ,
          plot.margin = margin(2, 8, 2, 4)  # 缩小上下间距
        )
      })


# 单独拿出 legend（从任何一个图中提取即可）
legend_plot <- ggplot(landuse_long, aes(x = scenario, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "RdYlGn")  + theme_minimal(base_family = "Arial", base_size = 7) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 7)
  )

legend <- get_legend(legend_plot)

# 使用 ggarrange 合并三图，并共用 legend
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

plot_path <- "/Volumes/UCL/论文工作/forestation/my_output_folder/plots/S8"
ggsave(file.path(plot_path, "figure_nature_fullpage.png"), final_plot,
       width = 182, height = 80, units = "mm", dpi = 600)

