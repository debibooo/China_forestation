
library(readxl)
library(ggplot2)
library(tidyr)
library(patchwork)  
library(ggrepel)
library(dplyr)


path2 <- "invest/biodiversity.xlsx"

data3 <- read_excel(path2, sheet = "Sheet1")  


p3 <- ggplot(data3, aes(x = x, y = y)) +

  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0, fill = "blue", alpha = 0.1) +  # Third quadrant
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = 0, fill = "red", alpha = 0.1) +   # Fourth quadrant
  annotate("rect", xmin = -Inf, xmax = 0, ymin = 0, ymax = Inf, fill = "green", alpha = 0.1) +  # Second quadrant
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = Inf, fill = "yellow", alpha = 0.1) +  # First quadrant

  geom_point(size = 1.2) +
  geom_text_repel(
    aes(label = province),
    family = "Arial", 
    size = 3,          
    box.padding = 0.2 ,
    point.padding = 0.2, 
  ) +

  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey", size = 0.5) +
  labs(
    x = "NetZero/REF",
    y = "Affores/REF") +

  theme_minimal() +
  theme(text = element_text(family = "Arial", size = 6),
        axis.text.y = element_text(size=8,color="black"),
        axis.text.x = element_text(size=8,color="black"),
        axis.title = element_text(size=8),
        panel.grid.major = element_blank(), 
        plot.title.position = "plot" ,
        panel.grid.minor = element_blank(), 
        plot.margin = unit(c(5,5,1,1), "mm"),         
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )


print(p3)

ggsave("fig2_biodiversity.jpg", p3,  dpi = 600)

