library(maps)
library(tidyverse)
library(gganimate)
forest <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest.csv')
neighbours<-c("India","China","Myanmar","Bhutan","Bangladesh","Nepal")
indiamapdata<-map_data("world") %>% 
              filter(region %in% neighbours & is.na(subregion)) %>% 
              union(map_data("world") %>% filter(region=="Indonesia"))

indiaforestdata<-forest %>% filter(entity%in% neighbours) 
indiaforestdata <- indiaforestdata %>%
  select(entity,Year=year,NetForestConversion=net_forest_conversion) %>% 
  mutate(
    NetForestColor=
      case_when(
                NetForestConversion < -400000 ~ "< -400",
                between(NetForestConversion,-400000,-200000) ~ "< -200",
                between(NetForestConversion ,-200001,-100000) ~"< -100",
                between(NetForestConversion ,-100001,0) ~"0",
                between(NetForestConversion ,1,100000) ~"<100",
                between(NetForestConversion ,100001,200000) ~"<200",
                between(NetForestConversion ,200001,400000)~"<400",
                NetForestConversion > 400000 ~ ">400",
                TRUE~NA_character_
                      )
  )

  joineddata<-indiaforestdata %>% 
  inner_join(indiamapdata,by = c("entity"="region"),keep = TRUE) %>% 
  select(Year,NetForestConversion,NetForestColor,Longitude=long
         ,Latitude=lat,MapGroup=group,MapOrder=order,Region=region)
  joineddata$Year<-as.factor(joineddata$Year)
  clevels<- c("< -400" ,"< -200","< -100", "0","<100","<200","<400",">400")
  scalecolors <- c(
    "< -400" = "#d73027",
    "< -200" = "#f46d43",
    "< -100" = "#fdae61",
    "0" = "#fee08b",
    "<100" = "#d9ef8b" ,
    "<200" = "#a6d96a",
    "<400" = "#66bd63",
    ">400" ="#1a9850"   
  )
animated.plot<-  ggplot() +
geom_polygon(data=joineddata,
             mapping = aes(x=Longitude,y=Latitude,group=MapGroup,fill=factor(NetForestColor,levels = clevels))
             ,color="white") +
scale_fill_manual(  values=scalecolors  )+
  theme_classic()+
  theme(axis.line = element_blank()
        ,axis.title = element_blank()
        ,axis.text = element_blank()
        ,axis.ticks = element_blank()
        )+
  theme(legend.position = "bottom"
        ,legend.direction = "horizontal"
        ,legend.background = element_rect(fill = "azure")
        ,legend.spacing.x = unit(0,'cm')
        ,legend.key.width = unit(2,"cm")
        ,legend.title.align = 0.5  
        )+
  theme(plot.background = element_rect(fill = "azure"))+
  theme(panel.background = element_rect(fill = "azure"))+
  theme(text = element_text(colour = "navy"))+
  guides(fill=guide_legend("Forestation in 1000s of hectares"
                           ,label = TRUE
                           ,label.position = "top"
                           # ,direction = "horizontal"
                           ,nrow = 1
                           ,title.position = "top"
                           
                           
  ))+
  ggtitle(label ="Forestation in India's Neighbourhood",subtitle =  "Year : {closest_state}" )+
  transition_states(states = Year) 
anim_save("./Forestation.gif", animated.plot, width = 1000, height = 1000)

