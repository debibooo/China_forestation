library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(ggpubr)
library(cowplot)  
library(purrr) 

path="/Volumes/UCL/论文工作/forestation/my_output_folder/forestation/sustainability_3otherindex.xlsx"

common_scales <- list(
  scale_x_continuous(limits = c(2020,2100), breaks = seq(2020,2100,20)),
  scale_y_continuous(limits = c(-2,5), breaks = seq(-2,5,2))  # 视需要微调
)



# 1. 先读取 netzero sheet，拿到 Sector 的原始顺序
sector_order <- read_xlsx(path, sheet = "netzero") %>%
  pull(Sector) %>%
  unique()

# 1. 先把两个 sheet 各自读成长表，并加上 file 标记
read_and_pivot <- function(path, sheet_name){
  read_xlsx(path, sheet = sheet_name) %>%
    pivot_longer(
      cols      = -Sector,
      names_to  = "Year",
      values_to = "delta"
    ) %>%
    mutate(
      Year = as.integer(Year),
      file = sheet_name               # 先把 sheet_name 赋给 file
    ) %>%
    mutate(
      Sector = factor(Sector, levels = sector_order),
      
      file = factor(file,             # 再把 file 转成 factor，指定顺序
                    levels = c("netzero", "afforest"))
    )
}


sheets <- c("netzero", "afforest")

df_long <- map_dfr(sheets, ~ read_and_pivot(path, .x))
df_long %>%
  filter(file == "ref", Year >= 2020, Year <= 2060) %>%
  arrange(Year) %>%
  select(Sector, Year, delta) %>%
  print(n = Inf)

# 2. 计算 net_delta
net_data <- df_long %>%
  group_by(file, Year) %>%
  summarise(net_delta = sum(delta), .groups="drop")

#计算lulucf的net—delta

net_data2 <- df_long %>%
  filter(Sector %in% c("forest", "lulucf")) %>%
  group_by(Sector, Year) %>%
  summarise(
    net_delta = sum(delta),
    .groups = "drop"
  )

# 3. 给分面取中文/plotmath 名称（可选）
custom_titles <- c(
netzero  = "NetZero",
  afforest = "Afforest",  
  dietshift  = "DietShift",
  yieldup = "YieldUp",
  suscrop = "SusCrop"
  
)

# 4. 按 file 拆出每个子图
plots <- df_long %>%
  split(.$file) %>%
  imap(~ {
    p <- ggplot(.x, aes(x = Year, y = delta, fill = Sector)) +
      geom_area(alpha = 0.8, color = NA) +
      geom_hline(yintercept = 0, color = "grey60", size = 0.8) +
      geom_line(
        data        = net_data %>% filter(file == .y),
        aes(x = Year, y = net_delta),
        color       = "black",
        linetype    = "dashed",
        size        = 1,
        inherit.aes = FALSE
      ) +
      common_scales +
      scale_fill_viridis_d(
     
        begin  = 0.1,
        end    = 0.9,
        drop   = FALSE
      ) +
      labs(
        title = custom_titles[.y],
        y     = expression(Pg~C),
        fill  = "Sector"
      ) +
      theme_minimal(base_family = "Arial") +
      theme(
        panel.grid      = element_blank(),
        panel.border    = element_rect(color="black", fill=NA),
        plot.title      = element_text(face="bold", size=12),
        axis.text       = element_text(size=7),
        axis.title      = element_text(size=7),
        legend.position = "right",           # 把 legend 放到右侧
        legend.direction = "vertical" ,       # 竖直排列
        legend.title     = element_text(size = 8),  # 图例标题文字大小
        legend.text      = element_text(size = 7)   # 图例标签文字大小
 
      )       
    # 针对两个子图分别添加不同注记
        if (.y == "netzero") {
          p <- p +
            annotate(
              "text",
              x        = 2100,
              y        = 5,
              label    = bquote("Lulucf excl. forest"~2.2~Pg~C),
              hjust    = 1, vjust = 1,
              size     = 4,
              fontface = "bold.italic",
              colour   = "black"
            )+
            annotate(
              "text",
              x        = 2100,
              y        =4.5,
              label    = bquote(Forest~.( -2.5 )~Pg~C),
              hjust    = 1, vjust = 1,
              size     = 4,
              fontface = "bold.italic",
              colour   = "black"
            )
        } else if (.y == "afforest") {
          p <- p +
            annotate(
              "text",
              x        = 2100,
              y        = 5,
              label    = bquote("Lulucf excl. forest"~12.5~Pg~C),
              hjust    = 1, vjust = 1,
              size     = 4,
              fontface = "bold.italic",
              colour   = "black"
            )+
            annotate(
              "text",
              x        = 2100,
              y        = 4.5,
              label    = bquote(Forest~.( -18.1 )~Pg~C),
              hjust    = 1, vjust = 1,
              size     = 4,
              fontface = "bold.italic",
              colour   = "black"
            )
        }else if (.y == "dietshift") {
          p <- p +
            annotate(
              "text",
              x        = 2100,
              y        = 5,
              label    = bquote("Lulucf excl. forest"~12.5~Pg~C),
              hjust    = 1, vjust = 1,
              size     = 4,
              fontface = "bold.italic",
              colour   = "black"
            )+
            annotate(
              "text",
              x        = 2100,
              y        = 4.5,
              label    = bquote(Forest~.( -18.9 )~Pg~C),
              hjust    = 1, vjust = 1,
              size     = 4,
              fontface = "bold.italic",
              colour   = "black"
            )
        }else if (.y == "yieldup") {
          p <- p +
            annotate(
              "text",
              x        = 2100,
              y        = 5,
              label    = bquote("Lulucf excl. forest"~12.6~Pg~C),
              hjust    = 1, vjust = 1,
              size     = 4,
              fontface = "bold.italic",
              colour   = "black"
            )+
            annotate(
              "text",
              x        = 2100,
              y        = 4.5,
              label    = bquote(Forest~.( -18.2 )~Pg~C),
              hjust    = 1, vjust = 1,
              size     = 4,
              fontface = "bold.italic",
              colour   = "black"
            )
        }else if (.y == "suscrop") {
          p <- p +
            annotate(
              "text",
              x        = 2100,
              y        = 5,
              label    = bquote("Lulucf excl. forest"~12.3~Pg~C),
              hjust    = 1, vjust = 1,
              size     = 4,
              fontface = "bold.italic",
              colour   = "black"
            )+
            annotate(
              "text",
              x        = 2100,
              y        = 4.4,
              label    = bquote(Forest~.( -10.1 )~Pg~C),
              hjust    = 1, vjust = 1,
              size     = 4,
              fontface = "bold.italic",
              colour   = "black"
            )
        }else if (.y == "ref") {
          p <- p +
            annotate(
              "text", 
              x        = 2100,
              y        = 5,
              label    = bquote("Lulucf excl. forest"~0.34~Pg~C),
              hjust    = 1, vjust = 1,
              size     = 4,
              fontface = "bold.italic",
              colour   = "black"
            )+
            annotate(
              "text",
              x        = 2100,
              y        = 4.5,
              label    = bquote(Forest~.( -0.37 )~Pg~C),
              hjust    = 1, vjust = 1,
              size     = 4,
              fontface = "bold.italic",
              colour   = "black"
            )
        }
        
        p
  })     
        
        

legend_only <- get_legend(
  plots[[1]] +
    theme(
      legend.position  = "right",
      legend.direction = "vertical"
    )
)


# 3. 再把实际的子图（不带 legend）并排
panels_only <- ggarrange(
  plotlist      = plots,
  ncol          = 2,    # 每行 3 张
  nrow          = 1,    # 总共 2 行
  common.legend = FALSE,   # 取消公用 legend
  legend        = "none"
)

# 4. 最后用 cowplot::plot_grid 按比例拼回去
#    rel_widths = c(0.8, 0.2) 表示：绘图区占 80%，图例占 20%
final_plot <- plot_grid(
  panels_only,   # 左边是三幅 panel
  legend_only,   # 右边是 legend
  ncol         = 2,
  rel_widths   = c(0.9, 0.3)
)

print(final_plot)

# 6. 保存

ggsave(
  filename = "/Volumes/UCL/论文工作/forestation/my_output_folder/plots/fig3/Emission_by_sector2.png",  # 输出文件名
  plot     = final_plot,                    # ggplot 对象
  device   = "png",
  width    = 180,                  # 宽度 183 mm（双栏）
  height   = 90,                   # 高度 90 mm
  units    = "mm",                 # 单位 mm
  dpi      = 600                   # 分辨率 300 dpi
)












##### net c uptake

# 1. 读入并转成长表
df_long3 <- read_xlsx(path, sheet = "netcuptake") %>%
  pivot_longer(
    cols      = -1,                     # 除了第 1 列以外
    names_to  = "Year", 
    values_to = "Value"
  ) %>%
  rename(Scenario = 1) %>%              # 把第一列重命名为 Scenario
  mutate(
    Year = as.integer(Year)
  )

# 2. 再 pivot_wider 回变成一列年份＋两条线各自的值
df_wide <- df_long3 %>%
  pivot_wider(
    names_from  = Scenario,
    values_from = Value
  )


# 3. 用 ggplot 画图：两条线＋中间 ribbon 填色＋垂直虚线 x=2060
p3 <- ggplot(df_wide, aes(x = Year)) +
  # 填充两条线之间的区域（假设 NetZero 在下面，Afforest 在上面；若顺序相反请调换 ymin/ymax）
  geom_ribbon(aes(ymin = NetZero, ymax = Afforest),
              fill = "#0077b6", alpha = 0.3) +
  # 两条线
  geom_line(aes(y = NetZero),   color = "#b39cd0",  size = 1 ,linetype = "dashed",) +
  geom_line(aes(y = Afforest),  color = "#0077b6", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  common_scales +
  labs(
    x = "Year",
    y = expression(Pg~C),
    title = "Net C uptake"
  ) +
  theme_minimal(base_family = "Arial")+
  theme(
    panel.grid      = element_blank(),
    panel.border    = element_rect(color="black", fill=NA),
    plot.title      = element_text(face="bold", size=12),
    axis.text       = element_text(size=7),
    axis.title      = element_text(size=7),
  )+
  annotate(
    "text",
    x       = 2100,            # 右边界
    y       = 4.5,              # 上边界
    label   =bquote(NetZero~.(60.9)~Pg~C),
    hjust   = 1,               # 文字右对齐
    vjust   = 1,               # 文字顶对齐
    size    = 4,               # 字号（可调）

    fontface= "bold.italic",         # 字体风格（可选）
    color="#b39cd0"
  )+
  annotate(
    "text",
    x       = 2100,            # 右边界
    y       = 5,              # 上边界
    label   =bquote(Afforest~.(68.2)~Pg~C),
    hjust   = 1,               # 文字右对齐
    vjust   = 1,               # 文字顶对齐
    size    = 4,               # 字号（可调）

    fontface= "bold.italic",         # 字体风格（可选）
    color="#0077b6"
  )


print(p3)
ggsave(
  filename = "/Volumes/UCL/论文工作/forestation/my_output_folder/plots/fig3/netcuptake2.png",  # 输出文件名
  plot     = p3,                    # ggplot 对象
  device   = "png",
  width    = 67.5,                  # 宽度 183 mm（双栏）
  height   = 90,                   # 高度 90 mm
  units    = "mm",                 # 单位 mm
  dpi      = 600                   # 分辨率 300 dpi
)



