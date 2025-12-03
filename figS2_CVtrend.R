library(readxl)
library(ggplot2)
library(ggpmisc)             


excel_path <- "cv_results_10.xlsx"
df <- read_excel(excel_path, sheet = "Sheet1")

year_vec  <- df$year 
cv_vec    <- df$cv_x     
index_vec <- df$Price   

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


p2 <- ggplot(df, aes(x = Price, y = cv_x)) +
  geom_point(
    color    = "steelblue",
    shape    = 21,
    size     = 2.5,
    fill     = "white",
    stroke   = 0.8
  ) +
  geom_smooth(method = "lm", se = FALSE,   
              colour = "black", linetype = "dashed", size = 1) +
  
  stat_poly_eq(                      
    aes(label = paste(..eq.label..,  
                      ..rr.label..,  
                      ..p.value.label.., sep = "*\", \"*")),
    formula   = y ~ x,
    parse     = TRUE,
    label.x   = "right",  
    label.y   = 0.95,    
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

combined_plot <- grid.arrange(
  p1, p2,
  ncol   = 2,
  widths = c(1, 1)
)
print(combined_plot)

ggsave("/CV_trend2.jpg", 
       plot     = combined_plot,                   
       device   = "png"
       )

