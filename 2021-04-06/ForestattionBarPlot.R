library(tidyverse)
library(gganimate)
library(countrycode)
library(ggimage)
library(extrafont)
extrafont::loadfonts(device = "win")

forest <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest.csv')
neighbours<-c("India","China","Myanmar","Bhutan","Bangladesh","Nepal")
indiaforestdata<-forest %>% filter(entity%in% neighbours) 
indiaforestdata <- indiaforestdata %>%
  select(entity,Year=year,NetForestConversion=net_forest_conversion) %>% 
  mutate(
    NetForestConversion=NetForestConversion/1000
    ,iso2=countrycode(entity,"country.name","iso2c")
  )


animated.plot<-  ggplot(data = indiaforestdata,mapping = aes(x=iso2)) +
  geom_bar(mapping = aes(x=iso2,y=NetForestConversion),fill="#3B7A57",stat="identity") +
  ggtitle(label ="Forestation in India's Neighbourhood",subtitle =  "Year : {closest_state}" )+
  ylab("Forestation / Deforestation in 1000's Hectares")+
  labs(caption = "Source: Our World in Data. Graphic:2Rabbits2")+
  theme_modern_rc()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  geom_flag(mapping = aes(image=iso2),y=-700) +
  expand_limits(y = -800) + 
  coord_flip()+
  theme(line = element_line(colour = "white"))+
  transition_states(states = Year) 

anim_save("./Forestation_barplot_1.gif", animated.plot, width = 1000, height = 1000)

