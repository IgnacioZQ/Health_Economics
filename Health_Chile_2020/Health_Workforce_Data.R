### Library ----

library(tidyverse)
library(readr)
library(ggrepel)

#### Theme ----

theme_elegante <- function(base_size = 10,
                           base_family = "Raleway"
)
{
  color.background = "#FFFFFF" # Chart Background
  color.grid.major = "#D9D9D9" # Chart Gridlines
  color.axis.text = "#666666" # 
  color.axis.title = "#666666" # 
  color.title = "#666666"
  color.subtitle = "#666666"
  strip.background.color = '#9999CC'
  
  ret <-
    theme_bw(base_size=base_size) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.55, linetype="dotted")) +
    theme(panel.grid.minor=element_line(color=color.grid.major,size=.55, linetype="dotted")) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=base_size-3,color=color.axis.title, family = base_family)) +
    
    theme(strip.text.x = element_text(size=base_size,color=color.background, family = base_family)) +
    theme(strip.text.y = element_text(size=base_size,color=color.background, family = base_family)) +
    #theme(strip.background = element_rect(fill=strip.background.color, linetype="blank")) +
    theme(strip.background = element_rect(fill = "grey70", colour = NA)) +
    # theme(panel.border= element_rect(fill = NA, colour = "grey70", size = rel(1)))+
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, 
                                  size=20, 
                                  vjust=1.25, 
                                  family=base_family, 
                                  hjust = 0.5
    )) +
    
    theme(plot.subtitle=element_text(color=color.subtitle, size=base_size+2, family = base_family,  hjust = 0.5))  +
    
    theme(axis.text.x=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(axis.text.y=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(text=element_text(size=base_size, color=color.axis.text, family = base_family)) +
    
    theme(axis.title.x=element_text(size=base_size+2,color=color.axis.title, vjust=0, family = base_family)) +
    theme(axis.title.y=element_text(size=base_size+2,color=color.axis.title, vjust=1.25, family = base_family)) +
    theme(plot.caption=element_text(size=base_size-2,color=color.axis.title, vjust=1.25, family = base_family)) +
    
    # Legend  
    theme(legend.text=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(legend.title=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(legend.key=element_rect(colour = color.background, fill = color.background)) +
    theme(legend.position="bottom", 
          legend.box = "horizontal", 
          legend.title = element_blank(),
          legend.key.width = unit(.75, "cm"),
          legend.key.height = unit(.75, "cm"),
          legend.spacing.x = unit(.25, 'cm'),
          legend.spacing.y = unit(.25, 'cm'),
          legend.margin = margin(t=0, r=0, b=0, l=0, unit="cm")) +
    
    # Plot margins
    theme(plot.margin = unit(c(.5, .5, .5, .5), "cm"))
  
  ret
}

### Upload and handling Data (Dentists) ----

Dentists <- read.csv("https://raw.githubusercontent.com/IgnacioZQ/Health_Economics/main/Health_Chile_2020/Health_Workforce/Dentists.xls")

Dentists_Chile <- Dentists %>%
  select(ï..Location, Period, First.Tooltip) %>%
  filter(ï..Location == "Chile")

# Graphic

Dentists_Chile %>%
  ggplot(mapping = aes(x = Period, y = First.Tooltip, color = ï..Location)) +
  geom_line(size = 1.2) +
  geom_point(size = 4) +
  geom_point(size = 2, color = "white") +
  labs(title = paste("Dentists available per 10,000 population"), 
       subtitle = paste0("Chile") , 
       caption = "Fuente: OMS", 
       y = "Dentists per 10,000 population", 
       x = "Year") +
  theme_elegante() +
  geom_text(aes(label = First.Tooltip, digits = 1, big.mark = ','), vjust = -1, hjust = 0, alpha = 0.8, size = 2.5, angle = 75, color = "black") +
  scale_x_continuous(breaks = c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018)) +
  scale_y_continuous(breaks = c(2, 4, 6, 8, 10, 12, 14))
