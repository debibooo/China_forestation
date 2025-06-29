library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(purrr)

# —— 1. 定义 Excel 路径和要读取的 sheet 列表 —— #
path   <- "/Volumes/UCL/论文工作/forestation/my_output_folder/forestation/sustainability_3otherindex.xlsx"
sheets <- c("M1_forest", "M2_lulucf", "Es3_Cuptake", "F2_hunger")

# —— 2. 定义“每个情景”的配色 —— #
scenario_colors <- c(
  REF       = "#0077b6",  # 蓝
  NetZero   = "#f08080",  # 橙
  Afforest  = "#7CBB6C",  # 绿
  DietShift = "#a8dadc",  # 红
  YieldUp   = "#b39cd0",  # 紫
  SusCrop   = "#f9e79f"   # 棕
)
y_titles_expr <- list(
  M1_forest   = expression("Forest carbon emission (Pg " * C * ")"),
  M2_lulucf   = expression("LULUCF excl. forest emission (Pg " * C * ")"),
  Es3_Cuptake = expression("Net C uptake (Pg " * C * ")"),
  F2_hunger   = expression("Hunger population (Million)")
)


# —— 4. 定义一个函数：读取单个 sheet，并转换成长格式 —— #
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

# —— 5. 批量读 4 张表并转成长格式 —— #
list_of_dflong <- map(sheets, ~ read_and_pivot_periods(path, .x))

# —— 6. 写一个函数：根据 df_long、标题、y 轴表达式 绘制堆叠柱状图（无图例） —— #
make_stacked_bar <- function(df_long, title_text, y_label_expr) {
  # 6.1 先计算 net = v1 + v2
  net_values <- df_long %>%
    group_by(Scenarios) %>%
    summarise(net = sum(Value), .groups = "drop")
  
  # 6.2 正式绘图
  ggplot(df_long, aes(x = Scenarios, y = Value, fill = Scenarios)) +
    geom_col(
      aes(
        linetype = Period,  # v1 用实线，v2 用虚线
        alpha    = Period   # v1 不透明，v2 半透明
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
      y     = y_label_expr      # 直接传入 expression
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
      legend.position    = "none"  # 隐藏所有图例
    )
}

# —— 7. 批量生成 4 张 ggplot 对象（均无图例） —— #
plots_list <- pmap(
  list(
    df_long       = list_of_dflong,
    title_text    = sheets,
    y_label_expr  = y_titles_expr[sheets]
  ),
  ~ make_stacked_bar(..1, ..2, ..3)
)

# —— 8. 将 4 张子图排成 1×4 —— #
panels_only <- ggarrange(
  plotlist      = plots_list,
  ncol          = 4,
  nrow          = 1,
  common.legend = FALSE,
  legend        = "none"
)

# —— 9. 显示并保存 —— #
print(panels_only)


ggsave(
  filename = "/Volumes/UCL/论文工作/forestation/my_output_folder/plots/fig4/important_index2.png",  # 输出文件名
  plot     = panels_only,                    # ggplot 对象
  device   = "png",
  width    = 180,                  # 宽度 183 mm（双栏）
  height   = 55,                   # 高度 45 mm
  units    = "mm",                 # 单位 mm
  dpi      = 600                   # 分辨率 300 dpi
)