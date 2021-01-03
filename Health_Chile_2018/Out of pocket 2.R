#### Library ----
library(readxl)
library(dplyr)
library(tidyverse)

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

#### General Data ----

Health <- read_excel("Health_2.xlsx")


# Out of pocket ----

# expenditure (% of current health expenditure)
# Long definition
# Share of out of pocket
# payments of total current health expenditures. Out of pocket
# payments are spending on health directly out of pocket
# by households.
# Source
# World Health Organization's Global Health Workforce Statistics, OECD, supplemented by country data.

# Out of pocket Data

Health_Out <- Health %>%
  select(Country, `Out of pocket%ofcurrent2018`) %>%
  filter(`Out of pocket%ofcurrent2018`!= "..", Country %in%
           c("Cuba", "Greece", "Austria", "Uruguay", "Israel",
             "Switzerland", "Germany", "Argentina", "Italy", "Denmark", "Russian Federation",
             "Spain", "Finland", "Australia", "Europe & Central Asia", "France", "Costa Rica", "United Kingdom", "Canada",
             "Chile", "United States", "North America", "Japan", "Mexico",
             "Latin America & Caribbean", "Brazil", "China", "East Asia & Pacific",
             "Bolivia", "Panama", "World", "Peru")) %>%
  arrange(`Out of pocket%ofcurrent2018`) # Reorder the data

Health_Out$`Out of pocket%ofcurrent2018` <- as.numeric(as.character(Health_Out$`Out of pocket%ofcurrent2018`)) #Transform column character to numeric

# Out of pocket Graphic

Health_Out %>%
  mutate(Country = fct_reorder(Country, `Out of pocket%ofcurrent2018`)) %>%
  ggplot(aes(x = Country, y = `Out of pocket%ofcurrent2018`)) +
  geom_segment(aes(x = Country, xend = Country, y = 0, yend = `Out of pocket%ofcurrent2018`), color = ifelse(Health_Out$Country %in% c("Chile"), "springgreen4", "gray58"), size = ifelse(Health_Out$Country %in% c("Chile"), 1.1, 0.8)) +
  geom_point(color = ifelse(Health_Out$Country %in% c("Chile"), "springgreen4", "gray58"), size = ifelse(Health_Out$Country %in% c("Chile"), 3.4, 2)) +
  coord_flip() +
  theme_elegante() +
  ylab("% of current health expenditure") +
  labs(title = paste("Percentage of Household Income Invested in Health"), 
       caption = "World Health Organization's Global Health Workforce Statistics, OECD, supplemented by country data.",
       subtitle = paste0("Situation of Chile and the world\n", "2018" )) +
  geom_text(aes(label = Health_Out$`Out of pocket%ofcurrent2018`), hjust = -.5, nudge_x = 0, color = "gray46", size = 3.1) +
  theme(panel.grid.major.y = element_blank(), panel.border = element_blank(), axis.ticks.y = element_blank()) +
  theme(axis.title.y=element_blank(),
        legend.position = "none") +
  scale_y_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70))
