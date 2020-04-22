
library(tidyverse)

Bom_data <- read_csv("data/Bom_data.csv")
view(Bom_data)

Bom_stations <- read.csv("data/Bom_stations.csv")

view(Bom_stations)

#Challenge 1:
stations_data <- Bom_data %>%
  separate(Temp_min_max,into=c("min_temp","max_temp"),sep = "/") %>% 
  filter(min_temp != "-", max_temp != "-", Rainfall != "-") %>%
  group_by(Station_number) %>%  
  summarise()
stations_data

write_csv(stations_data,"results/stations_data.csv")


# Answer:20 stations have those data recorded.

#Challenge 2:
lowest_ave_temp <- Bom_data %>% 
  separate(Temp_min_max,into=c("min_temp","max_temp"),sep="/")%>%
  filter(min_temp!="-",max_temp!="-")%>%
  mutate(min_temp=as.numeric(min_temp))%>%
  mutate(max_temp=as.numeric(max_temp)) %>%
  mutate(temp_diff=max_temp-min_temp) %>% 
  group_by(Month) %>% 
  summarise(average=mean(temp_diff)) %>% 
  arrange(average) %>% 
  slice(1)
lowest_ave_temp

write_csv(lowest_ave_temp,"results/lowest_ave_temp.csv")

Bom_stations <- read_csv("data/Bom_stations.csv")
Bom_stations

#Challenge 3
Tidy_bom_station <- Bom_stations %>% 
  gather(key = Station_number, value = amount,-info) %>% 
  spread(key = info, value = amount) %>%
  #mutate(Station_number=str_replace_all(string = Station_number,
                                        #pattern = "X",
                                        #replacement = "")) %>% 
  mutate(Station_number=as.numeric(Station_number))
Tidy_bom_station

state_ave_temp <- Bom_data %>% 
  separate(Temp_min_max,into=c("min_temp","max_temp"),sep = "/") %>%  
  mutate(min_temp=as.numeric(min_temp))%>%
  mutate(max_temp=as.numeric(max_temp)) %>%
  mutate(temp_diff=max_temp-min_temp)
state_ave_temp

Station_df <- Bom_stations %>% 
  gather(key = Station_number, value = amount,-info) %>%
  spread(key = info, value = amount)
Station_df

stations_meteo_merge <- full_join(state_ave_temp,Tidy_bom_station, by=c("Station_number"="Station_number"))
stations_meteo_merge

write_csv(stations_meteo_merge,"results/stations_mateo_merge.csv")

#Which state saw the lowest average daily temperature difference?  
answer3 <- stations_meteo_merge %>% 
  mutate(min_temp=as.numeric(min_temp))%>%
  mutate(max_temp=as.numeric(max_temp)) %>%
  #mutate(temp_diff=max_temp-min_temp) %>% 
  group_by(state) %>% 
  summarise(average=mean(temp_diff,na.rm = TRUE)) %>% 
  arrange(average) %>% 
  slice(1) #Answer for this challenge came to QLD 7.15
  answer3
  write_csv(answer3,"results/answer3.csv")

#Does the westmost (lowest longitude) or eastmost (highest longitude)
#weather station in our dataset have a higher average solar exposure?
Question4 <- stations_meteo_merge %>%
  mutate(Solar_exposure = as.numeric(Solar_exposure)) %>%
  mutate(lon=as.numeric(lon)) %>%
  group_by(Station_number,lon) %>% 
  summarise(average = mean(Solar_exposure, na.rm = TRUE)) %>%
  ungroup() %>% 
  filter(lon==min(lon)|lon==max(lon))

Question4

write_csv(Question4,"results/Question4.csv")

#Second part-----------------------------------of the challenge
library(tidyverse)
read_csv("data/Bom_data.csv")  
BOM_data_new
BOM_data_temp
BOM_data_temp_solar
#question1
perth <- filter(BOM_data_temp_solar, Station_number==9225)
perth

perths <- filter(perth, Solar_exposure!="-")
perths
plot1 <-ggplot(data = perths)+
  geom_point(aes(as.numeric(max), as.numeric(min), alpha=0.3), colour="red")+
  xlab("Max Temp")+
  ylab("Min Temp")+
  theme(legend.position = "none")
plot1

#plot2
perths$min<-as.numeric(perths$min)
perths$max<-as.numeric(perths$max)
perths$Rainfall<-as.numeric(perths$Rainfall)
plot2<- ggplot(data = perths, aes(x = max, y = Rainfall) )+
  geom_point(alpha =0.3, colour="blue")
plot2
perths 

plot3 <-ggplot(data = perths, aes(x = max, y = Solar_exposure))+
  geom_point(alpha = 0.3, colour ="green")
plot3

#question 2
plot4 <-ggplot(data = perths, aes(x = max, y = min))+
  geom_point(aes(size = Rainfall, colour=Solar_exposure))+
  labs(x="Max Temp",
       y="Min Temp",
       size="Solar exposure",
       colour="Rainfall")+
  theme(legend.text = element_text(size = 8), legend.title = element_text(size = 8))
plot4
#question 3
install.packages("cowplot")
library(cowplot)
theme_set(theme_cowplot())
combined_plot<- plot_grid(plot1, plot2, plot3, plot4, labels = "AUTO")
combined_plot
ggsave(filename = "combined plot.png", plot=combined_plot)

#question 4
BOM_data
BOM_data_new4 <- select(BOM_data, Station_number, Month, Day, Rainfall) %>% 
  filter(Rainfall!='-')
BOM_data_new4$Rainfall <- as.numeric(BOM_data_new4$Rainfall)
BOM_data_new4
Station_mean_rainfall <- group_by(BOM_data_new4, Station_number, Month) %>%
  summarise(Station_mean_rainfall=mean(Rainfall))
Station_mean_rainfall
BOM_Stations_wide$Station_number <- as.numeric(BOM_Stations_wide$Station_number)
BOM_combined2 <- left_join(Station_mean_rainfall, BOM_Stations_wide, by="Station_number")
View(BOM_combined2)

plot5 <-ggplot(data = BOM_combined2, aes(as.factor(Month), Station_mean_rainfall, colour=state, group=Station_number))+
  geom_line(size=1)
plot5

plot6 <-BOM_combined2 %>%
  ggplot(aes(as.factor(Month), Station_mean_rainfall, group=as.factor(Station_number), colour=state))+
  geom_line(size=1)+
  facet_wrap(~state)+
  theme(legend.text = element_text(size = 8), legend.title = element_text(size = 8))+
  labs(x="Month", y="Average rainfall",
       colour="State")

plot6
ggsave(filename = "Average rainfall per month for each state.png", plot=plot6)