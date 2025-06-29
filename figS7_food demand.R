
# 加载包
library(readxl)
library(dplyr)
library(purrr)
library(ggplot2)
library(viridis)
library(patchwork)

# 1. 设置数据文件夹路径
folder <- "/Volumes/UCL/论文工作/forestation/my_output_folder/forestation/fooddemand/total"  

# 2. 列出所有 Excel 文件
files <- list.files(folder, pattern = "\\.xlsx$", full.names = TRUE)

# 3. 批量读取、筛选并合并：得到 Year, input, value
data_all <- map_dfr(files, function(f) {
  read_xlsx(f) %>%
    filter(input %in% c("FoodDemand_NonStaples", "FoodDemand_Staples")) %>%
    select(Year, input, value)
}) %>%
  filter(Year >= 2015)

# 4. 计算每年每种 input 的 min/max/median
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
# 6. 显示
print(p)
# 假设 p 已经包含了你的 ggplot 对象
ggsave(
  filename = "/Volumes/UCL/论文工作/forestation/my_output_folder/plots/S4-5/S4_Fooddemand.png",  # 输出文件名
  plot     = p,                    # ggplot 对象
  device   = "png",
  width    = 180,                  # 宽度 183 mm（双栏）
  height   = 90,                   # 高度 90 mm
  units    = "mm",                 # 单位 mm
  dpi      = 600                   # 分辨率 300 dpi
)



