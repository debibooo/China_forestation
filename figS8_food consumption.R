
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
  "#f8d1b4",  # 靛蓝
  "#f7bf9e",  # 浅橙
  "#f69b72",  # 暖橙
  "#e7bc91",  # 优雅紫（偏蓝）
  "#d4a276",  # 柔和绿（偏青）
  "#b08968",  # 柔粉（次要高亮）
  "#ffdda1",  # 黄绿
  "#ffd151",  # 松绿
  "#d5a021",  # 浅天蓝    
  "#cfe1b9",  # 淡黄绿（生机感）
  "#b5c99a",  # 柔橙（友好提示）
  "#97a97c",   # 亮黄（强调用）  
  "#c5dbf0",  # 明黄
  "#96b8db",  # 紫红
  "#6083c5",  # 暗灰（中性色，平衡用）  
  "#a8dadc",  # 灰绿
  "#457b9d",  # 灰绿
  "#314A5A",  # 深灰蓝
  "#dec9e9",  # 深红紫
  "#c19ee0",  # 深红紫  
  "#76618F"  # 紫

)


folder  <- "/Volumes/UCL/论文工作/forestation/my_output_folder/forestation/foodconsumption"
files4 <- file.path(folder, c(
  "NetZero.xlsx",
  "Afforest.xlsx",
  "DietShift.xlsx",
  "YieldUp.xlsx",
  "SusCrop.xlsx"
))

last_f <- file.path(folder, "reference.xlsx")

# 2. 取出最后一个文件的 bare name，用作 facet mapping 的 key
last_name <- tools::file_path_sans_ext(basename(last_f))

# 3. 自定义 strip 小标题（plotmath），Affor 的 high 做下标斜体
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

# 3. 读前三个文件，提取 Year/technology/delta，并加上 file 列
data4 <- map_dfr(files4, function(f) {
  read_xlsx(f) %>%
    select(Year, technology, delta) %>%
    mutate(file = tools::file_path_sans_ext(basename(f)))
}) %>%
  filter(Year >= 2015)

file_levels <- c(tools::file_path_sans_ext(basename(files4)), last_name)
data4$file    <- factor(data4$file, levels = file_levels)



# 4. 计算每个 scenario 每年 net_delta
net_data <- data4 %>%
  group_by(file, Year) %>%
  summarise(net_delta = sum(delta), .groups = "drop")

# 5. 读最后一个文件，提取 Year/technology/value，并加上 file 列
val_data <- read_xlsx(last_f) %>%
  select(Year, technology, value) %>%
  filter(Year >= 2015) %>%
  mutate(file = last_name)

# 6. 统一横轴范围和刻度（2015 开始，10 年一跳）
min_year <- 2015
max_year <- 2100

xbreaks1 <- seq(2020, max_year, by = 40)
xbreaks2 <- seq(2020, max_year, by = 20)

# 7. 绘制带 legend 的 delta 堆积面积图（p_area_leg）
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

print(p_area_leg)

# 8. 绘制不带 legend 的 value 堆积面积图（p_value）
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

# 9. 并排合成：左 value, 右 delta，共用底部 legend
combined <- ggarrange(
  p_value, p_area_leg,
  ncol          = 2,
  widths        = c(0.3, 0.9),
  common.legend = TRUE,
  legend        = "bottom"
)


# 10. 展示 & 保存
print(combined)

ggsave(
  filename = "/Volumes/UCL/论文工作/forestation/my_output_folder/plots/S4-5/S5_Foodconsumption.png",  # 输出文件名
  plot     = combined,                    # ggplot 对象
  device   = "png",
  width    = 180,                  # 宽度 183 mm（双栏）
  height   = 100,                   # 高度 90 mm
  units    = "mm",                 # 单位 mm
  dpi      = 600                   # 分辨率 300 dpi
)




