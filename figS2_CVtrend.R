library(readxl)
library(ggplot2)
library(ggpmisc)              # 每次用前都要加载

# —— 1. 读取 Excel 文件 —— #
excel_path <- "/Volumes/UCL/论文工作/forestation/my_output_folder/cv_results_10.xlsx"
df <- read_excel(excel_path, sheet = "Sheet1")

# 如果不确定列名，可以先查看：
# print(colnames(df))

# 提取向量
year_vec  <- df$year    # 年份
cv_vec    <- df$cv_x      # CV 数值
index_vec <- df$Price   # Index 数值




# —— 3. 子图 1：CV 随年份的折线图 —— #
p1 <- ggplot(df, aes(x = year, y = Price)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 2) +
  labs(
    x     = "Year",
    y     = "Weighted food price (2020USD per Kcal)"
  ) +
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title       = element_text(size = 10, face = "bold"),
    axis.title       = element_text(size = 10),
    axis.text        = element_text(size =8),
    panel.grid.major = element_line(color = "grey80", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )


# —— 4. 子图 2：Index vs CV 的散点 + 二次拟合曲线 —— #
p2 <- ggplot(df, aes(x = Price, y = cv_x)) +
  geom_point(
    color    = "steelblue",
    shape    = 21,
    size     = 2.5,
    fill     = "white",
    stroke   = 0.8
  ) +
  geom_smooth(method = "lm", se = FALSE,   # 线性拟合曲线
              colour = "black", linetype = "dashed", size = 1) +
  
  stat_poly_eq(                       # ⭐ 自动生成公式 / R² / p
    aes(label = paste(..eq.label..,   # 方程
                      ..rr.label..,  # R²
                      ..p.value.label.., sep = "*\", \"*")),
    formula   = y ~ x,
    parse     = TRUE,
    label.x   = "right",  # or 0.05  数 0–1 也行
    label.y   = 0.95,     # 顶部
    size      = 3,
    rr.digits = 3,
    p.digits  = 3
  ) +
  labs(
    x     = "Weighted food price (2020USD per Kcal)",
    y     = "CV"
  ) +
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title       = element_text(size = 10, face = "bold"),
    axis.title       = element_text(size = 10),
    axis.text        = element_text(size = 8),
    panel.grid.major = element_line(color = "grey80", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  ) 

# —— 5. 将两个子图合并为 1 行 2 列 —— #
combined_plot <- grid.arrange(
  p1, p2,
  ncol   = 2,
  widths = c(1, 1)
)
print(combined_plot)

ggsave("/Volumes/UCL/论文工作/forestation/my_output_folder/plots/S2/CV_trend2.jpg", 
       plot     = combined_plot,                    # ggplot 对象
       device   = "png",
       width    = 180,                  # 宽度 183 mm（双栏）
       height   = 90,                   # 高度 45 mm
       units    = "mm",                 # 单位 mm
       dpi      = 600                   # 分辨率 300 dpi
       )

