library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(ggpubr)
library(cowplot)  
library(purrr) 

path="/sustainability_3otherindex.xlsx"

common_scales <- list(
  scale_x_continuous(limits = c(2020,2100), breaks = seq(2020,2100,20)),
  scale_y_continuous(limits = c(-2,5), breaks = seq(-2,5,2))  
)


sector_order <- read_xlsx(path, sheet = "netzero") %>%
  pull(Sector) %>%
  unique()

read_and_pivot <- function(path, sheet_name){
  read_xlsx(path, sheet = sheet_name) %>%
    pivot_longer(
      cols      = -Sector,
      names_to  = "Year",
      values_to = "delta"
    ) %>%
    mutate(
      Year = as.integer(Year),
      file = sheet_name              
    ) %>%
    mutate(
      Sector = factor(Sector, levels = sector_order),
      
      file = factor(file,            
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


net_data <- df_long %>%
  group_by(file, Year) %>%
  summarise(net_delta = sum(delta), .groups="drop")



net_data2 <- df_long %>%
  filter(Sector %in% c("forest", "lulucf")) %>%
  group_by(Sector, Year) %>%
  summarise(
    net_delta = sum(delta),
    .groups = "drop"
  )


custom_titles <- c(
netzero  = "NetZero",
  afforest = "Afforest",  
  dietshift  = "DietShift",
  yieldup = "YieldUp",
  suscrop = "SusCrop"
  
)


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
        legend.position = "right",           
        legend.direction = "vertical" ,       
        legend.title     = element_text(size = 8),  
        legend.text      = element_text(size = 7)   
 
      )       

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

panels_only <- ggarrange(
  plotlist      = plots,
  ncol          = 2,    
  nrow          = 1,    
  common.legend = FALSE,  
  legend        = "none"
)


final_plot <- plot_grid(
  panels_only,   
  legend_only,   
  ncol         = 2,
  rel_widths   = c(0.9, 0.3)
)

print(final_plot)

ggsave(
  filename = "/Emission_by_sector2.png", 
  plot     = final_plot,                   
  device   = "png",
  width    = 180,                  
  height   = 90,                   
  units    = "mm",                 
  dpi      = 600                   
)



df_long3 <- read_xlsx(path, sheet = "netcuptake") %>%
  pivot_longer(
    cols      = -1,                    
    names_to  = "Year", 
    values_to = "Value"
  ) %>%
  rename(Scenario = 1) %>%              
  mutate(
    Year = as.integer(Year)
  )

df_wide <- df_long3 %>%
  pivot_wider(
    names_from  = Scenario,
    values_from = Value
  )



p3 <- ggplot(df_wide, aes(x = Year)) +

  geom_ribbon(aes(ymin = NetZero, ymax = Afforest),
              fill = "#0077b6", alpha = 0.3) +
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
    x       = 2100,            
    y       = 4.5,              
    label   =bquote(NetZero~.(60.9)~Pg~C),
    hjust   = 1,               
    vjust   = 1,               
    size    = 4,               

    fontface= "bold.italic",         
    color="#b39cd0"
  )+
  annotate(
    "text",
    x       = 2100,            
    y       = 5,            
    label   =bquote(Afforest~.(68.2)~Pg~C),
    hjust   = 1,               
    vjust   = 1,               
    size    = 4,               

    fontface= "bold.italic",        
    color="#0077b6"
  )


print(p3)
ggsave(
  filename = "/netcuptake.png", 
  plot     = p3,                  
  device   = "png",
  width    = 67.5,                  
  height   = 90,                   
  units    = "mm",                
  dpi      = 600                  
)



