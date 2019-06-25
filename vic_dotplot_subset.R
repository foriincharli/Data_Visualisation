setwd("C:/Users/path/to/df")

library(tidyverse)
library(ggplot2)

#### Read in the data frame ####

vic <- read.csv("weather_extra.csv") %>% filter(State == "VIC")


#### Generate some summary statistics for the dotplot ####

df2 <- vic %>% 
  group_by(Year, Location) %>% 
  summarise_each(funs(mean(., na.rm = T), 
                      n = sum(!is.na(.)), 
                      se = sd(., na.rm = T)/sqrt(sum(!is.na(.)))), 
                 MinTemp:Rainfall) %>% 
  mutate_if(is.numeric, round, 2)


#### Make a graph ####
pd <- position_dodge(width = 0.3)

ggplot(data = df2, 
       aes(x = Year, 
           y = Rainfall_mean, 
           colour = Location)) +
  
  geom_hline(aes(yintercept = 0), colour = "#333333", linetype = "dashed") +
  
  geom_point(size = 3.5,
             position = pd) +
  
  geom_errorbar(aes(ymax = Rainfall_mean + Rainfall_se, 
                    ymin = Rainfall_mean - Rainfall_se), 
                size  = .3, 
                width = .2, 
                linetype = "solid", 
                position = pd) +
  
  theme(text = element_text(size = 14),
        panel.grid = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "#333333"),                   
        panel.border = element_rect(colour = "#333333", 
                                    fill = NA, 
                                    size = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom", 
        legend.key = element_blank()) +
  
  scale_x_continuous(limits = c(2007.5, 2018.5),  
                        breaks= c(2008, 2010, 2012, 2014, 2016, 2018)) +
  
  xlab("Year") + ylab("Mean Rainfall (mm)") +
  
  facet_wrap(~ Location, nrow = 2) +
  theme(strip.text.x = element_text(size = 16),
        strip.background = element_rect(colour = "#333333", fill = "#E8E8E8"))

# save the graph
ggsave("vic_subset_dotplot.png", height = 7, width = 12)
