#### Preparation ####
list_packages <- c("ggplot2","ggrepel","dplyr","tidyr","treemap","NMF",
                   "tidyverse","tidyquant")

library(tidyverse)
library(dplyr)
library(ggrepel)
library(treemap)
library(NMF)
library(tidyquant)
library(tidyr)
library(lubridate)
library(maps)
library(mapproj)
library(tigris)
library(ggplot2)
library(nycmaps)
# library(ggmap)

#load data
raw_data <-read_csv("NYPD_15-18.csv")
#data manipulation
myvars <- c("DATE", "BOROUGH", "ON STREET NAME","CROSS STREET NAME","OFF STREET NAME", 
            "NUMBER OF PERSONS INJURED", "NUMBER OF PERSONS KILLED", "NUMBER OF PEDESTRIANS INJURED",
            "NUMBER OF PEDESTRIANS KILLED","NUMBER OF CYCLIST INJURED",
            "NUMBER OF CYCLIST KILLED","NUMBER OF MOTORIST INJURED", "NUMBER OF MOTORIST KILLED",
            "CONTRIBUTING FACTOR VEHICLE 1")
data_update <- raw_data[myvars]
colnames(data_update) <- c("Date","Borough","On_Street","Cross_street","Off_Street","Person_Injured","Person_Death",
                           "Pedestrians_Injured","Pedestrians_Death","Cyclist_Injured","Cyclist_Death",
                           "Motorist_Injured","Motorist_Death","Factor")

data_update$Date <- as.Date(data_update$Date,"%m/%d/%y")
data_update$year <-year(data_update$Date)
data_update$Year_month <-format(as.Date(data_update$Date), "%Y-%m")
data_update$Accident <- 1
# str(data_update)
# view(raw_data)
remove(test_greater5)
############################# 1. Annual collision ################################
myvars <-names(data_update) %in% c("On_Street","Cross_street","Off_Street","Factor")

data_update_subset <-data_update[!myvars]
# view(data_update_subset)                                  

# 1.1 annual collision by year
annual_collision <- data_update %>%
  group_by(year) %>%
  summarize('Total_Collision' =sum(Accident, na.rm = TRUE),
            'Injured' =sum(Person_Injured,Pedestrians_Injured,Cyclist_Injured,Cyclist_Injured,Motorist_Injured, na.rm = TRUE),
            'Killed' =sum(Person_Death, Pedestrians_Death,Cyclist_Death,Motorist_Death,na.rm = TRUE))
# # 1.2 annual collision by year and month
# annual_collision_bymonth <- data_update %>%
#   group_by(Year_month) %>%
#   summarize('Collision' =sum(Accident, na.rm = TRUE),
#             'Injured' =sum(Person_Injured,Pedestrians_Injured,Cyclist_Injured,Motorist_Injured, na.rm = TRUE),
#             'Killed' =sum(Person_Death, Pedestrians_Death,Cyclist_Death,Motorist_Death,na.rm = TRUE))
# view(annual_collision_bymonth)

# 1.1 annual collision by accident type
annual_collision_group <- data_update %>%
  group_by(year) %>%
  summarize('Total_Collision' =sum(Accident, na.rm = TRUE),
            'Person_Injured'= sum(Person_Injured, na.rm = TRUE),
            'Person_Death'= sum(Person_Death, na.rm = TRUE),
            'Pedestrians_Injured'= sum(Pedestrians_Injured, na.rm = TRUE),
            'Pedestrians_Death'= sum(Pedestrians_Death, na.rm = TRUE),
            'Cyclist_Injured'= sum(Cyclist_Injured, na.rm = TRUE),
            'Cyclist_Death'= sum(Cyclist_Death, na.rm = TRUE),
            'Motorist_Injured'= sum(Motorist_Injured,na.rm = TRUE),
            'Motorist_Death'= sum(Motorist_Death,na.rm = TRUE))


#plot histogram
#plot barchart 1.1Annual collision
annual_collision$Unspecified <-annual_collision$Total_Collision-annual_collision$Injured-annual_collision$Killed

# temp1 <- subset(annual_plot, select=c("year","Total_Collision"))
# temp1$group <-"Total_Collision"
# colnames(temp1) <- c("year","count","group")
temp2 <- subset(annual_collision, select=c("year", "Injured"))
temp2$group <-"Injured"
colnames(temp2) <- c("year","count","group")

temp3 <- subset(annual_collision, select=c("year", "Killed"))
temp3$group <-"Killed"
colnames(temp3) <- c("year","count","group")

temp4 <- subset(annual_collision, select=c("year", "Unspecified"))
temp4$group <-"Unspecified"
colnames(temp4) <- c("year","count","group")

annual_plot <-rbind(temp4,temp2,temp3)
# view(annual_plot)
remove(temp1,temp2,temp3,temp4)

png(filename="1.1Annual collision.png",width = 800, height = 600) 
ggplot(data = annual_plot,aes(x = year,y = count, fill = group)) +
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="RdBu")+
  labs(title = 'NYC Motor Vehicle Collisions Frequency 2015−2018',
       x = 'Year',
       y = 'count',
       caption = 'Data Source: NYC OpenDate provided by the Police Department (NYPD). ' ) +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size=12, face="bold"),
        legend.text = element_text(size=10),
      axis.text.x = element_text(size =12),axis.text.y = element_text(size =10),
      panel.background = element_blank())
dev.off()

### use geom_col to plot
# png(filename="1.1 Annual collision.png",width = 800, height = 600) 
# ggplot(data = annual_collision,aes(x = year)) +
#   geom_col(aes(y = Total_Collision),
#            alpha = .85, 
#            fill = "pink", 
#            color = "pink") +
#   geom_text(aes(label=Total_Collision,y = Total_Collision), vjust=2, size=5)+
#   geom_col(aes(y = Injured),
#            alpha = .85, 
#            fill = "cornflowerblue", 
#            color = "cornflowerblue") +
#   geom_text(aes(label=Injured,y = Injured), vjust=2, size=5)+
#   geom_col(aes(y = Killed),
#            alpha = .85, 
#            fill = "blue", 
#            color = "blue") +
#   geom_text(aes(label=Killed,y = Killed), vjust=-0.3, size=5)+
#   labs(title = 'NYC Motor Vehicle Collisions Frequency 2015−2018',
#        x = 'Year',
#        y = 'count',
#        caption = 'Data Source: NYC OpenDate provided by the Police Department (NYPD). ',
#        cex = 10 ) +
#   theme_light() + 
#   theme(plot.title = element_text(hjust = 0.5),
#         axis.text.x = element_text(size =12),axis.text.y = element_text(size =10),
#         panel.background = element_blank())
# dev.off()
###



############################# 2. Injures type heapmap #############################
#plot heapmat 1.2heapmap
mat_plot <- data.matrix(annual_collision_group[,-1])
rownames(mat_plot) <- annual_collision$year
mat_plot_t<- t(mat_plot)
rownames(mat_plot_t) <-c("Total Collisions","Person Injured","Person Death",
               "Pedestrians Injured","Pedestrians Death","Cyclist Injured","Cyclist Death",
               "Motorist Injured","Motorist Death")
# view(mat_plot_t)

png(filename="2.1heapmap.png",width = 600, height = 400) 
aheatmap(mat_plot_t, 
         color="RdBu:50", # color with custom features
         Rowv=FALSE, Colv=FALSE, # disable dendrogram
         # Rowv=NA, Colv=NA, # disable dendrogram
         scale = "row",breaks = NA, # key label range
         cexCol=1.2, cexRow=1.2,# column/row height
         main="NYC Collisions Injures Types")
dev.off()
###plot borough accident point chart
# png(filename="2.2.png",width = 600, height = 400)
# ggplot(data=data_update)+
#       geom_point(aes(x=Pedestrians_Injured, y=Person_Injured, color = Borough,size = Cyclist_Injured))
# dev.off()
###




############################# 3. Borough map #############################
borough_map <- data_update %>%
  group_by(Borough) %>%
  summarize('count' =sum(Accident, na.rm = TRUE))
borough_map_plot <-borough_map[1:5,]

#plot map
nyc_map <- map_data("nyc")
nyc_map$region[grepl('Brooklyn', nyc_map$region)] <- 'Brooklyn'
nyc_map$region[grepl('Brookyln', nyc_map$region)] <- 'Brooklyn'
nyc_map$region[grepl('Staten Island', nyc_map$region)] <- 'Staten Island'
nyc_map$region[grepl('Manhattan', nyc_map$region)] <- 'Manhattan'

borough_map_plot$Borough<-c("Bronx","Brooklyn","Manhattan","Queens","Staten Island")
merge_map <- merge(nyc_map,borough_map_plot,by.x="region", by.y = "Borough")
#clean background
theme_clean <- function(base_size = 12){
  require(grid)
  theme_grey(base_size) %+replace%theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks.length = unit(0, "cm"),
    axis.text.x = element_text(margin=unit(0, "cm")),
    axis.text.y = element_text(margin=unit(0, "cm")),
    panel.spacing = unit(0, "lines"),
    plot.margin = unit(c(0, 0, 0, 0), "lines"),
    plot.title = element_text(size=18,hjust = 0.5),
    complete = TRUE
  )}
# ### Attampt to merge by lat and long, do not work!!
# library(geosphere)
# test_greaterthan20 <-test%>% filter(sum >= 20)
# test_greaterthan20$long <-test_greaterthan20$Longtitude
# test_greaterthan20$lat <-test_greaterthan20$Latitude
# m1 <- merge(test_greaterthan20,nyc_map, by= c("long","lat"),
#             all.x=T)
# view(m1)
# ggplot(m1,aes(map_id = nyc,fill = sum ))+
#   geom_map(map = nyc,colour = "black")+
#   scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B",
#                        midpoint=median(m1$sum)) +
#   expand_limits(x = nyc_map$long, y = nyc_map$lat)+
#   coord_map("polyconic")
# ###

png(filename="3.1map.png",width = 800, height = 400)
ggplot(borough_map_plot,aes(map_id =Borough, fill =count/5))+
  geom_map(map = nyc_map, colour = "black")+
  scale_fill_distiller(palette = "Spectral",trans ="log1p")+
  expand_limits(x = nyc_map$long, y = nyc_map$lat)+
  labs(title = "NYC Collision Map by Borough 2015-2018",
       fill = "Collision count \nper year")+
  theme_clean()+
  coord_fixed()
dev.off()

# ###Attempt to add google map, do not work!
# nyc_google_map <- get_map(location="new york city", zoom=8, maptype="terrain")
# ggmap(nyc_google_map)
# test_greater5 <-test%>% filter(sum >= 5)
# png(filename="3.4injured_map.png",width = 800, height = 600)
# ggmap(nyc_google_map)+
#   geom_point(test_greater5, mapping = aes(x=Longtitude, y=Latitude),color = "purple",
#              size = sum, alpha =0.5)
# dev.off()
# ####

#Injury map by people group
injury_plot <-subset(raw_data, select =(1:19))
colnames(injury_plot) <- c("Date","Time","Borough","Zip_code","Latitude","Longtitude","Location","On_Street","Cross_street","Off_Street","Person_Injured","Person_Death",
                           "Pedestrians_Injured","Pedestrians_Death","Cyclist_Injured","Cyclist_Death",
                           "Motorist_Injured","Motorist_Death","Factor")

injury_plot$sum <- apply(injury_plot[,c(11:18)], 1, sum)
# view(injury_plot)

#plot point chart
png(filename="3.2injured_map.png",width = 800, height = 600)
ggplot(injury_plot%>% filter(sum >= 5), aes(x=Longtitude, y=Latitude,fill = sum),alpha =0.03) +
  geom_point(aes(size = sum),pch = 21, show.legend = FALSE)+
  scale_fill_gradient(low="white", high='red')+
  # scale_color_gradient(low="grey", high='red', trans="log")+
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat))+
  labs(title = "NYC Collision Injury Map",
       fill = "Injured People count,\ncount"
  )+
  theme_clean()+
  coord_equal()
dev.off()

############################# 4. reasons of collision #############################
type_coln <- data_update %>%
  group_by(Factor) %>%
  summarise(acdt_count=n())
type_coln <-arrange(type_coln,desc(acdt_count))
top_type_sub <- type_coln[-(1:1),]
top_type_sub <- mutate(top_type_sub,prct=round(acdt_count/sum(acdt_count),2)*100)

#plot bar chart
top_type_sub_plot <-top_type_sub[1:16,]

png(filename="4reasons.png",width = 800, height = 400)
ggplot(data = top_type_sub_plot) +
  aes(x = fct_reorder(Factor,desc(-prct)), y = prct, fill = log10(acdt_count),label = Factor) +
  # aes(x = fct_reorder(Factor,desc(-prct)), y = prct, label = Factor) +
  geom_col() +
  # geom_text(aes(label=ifelse(prct>5,as.character(Factor),'')), position = position_stack(vjust = 0.5),size=4,colour = 'White')+
  geom_text(aes(label=ifelse(prct > 5,as.character(Factor),'')), position = position_stack(vjust = 0.5),size=4,colour = 'white')+
  geom_text(aes(label=ifelse(between(prct, 3, 5),as.character(Factor),'')), position = position_stack(vjust = 2.0),size=4,colour = 'black')+
  geom_text(aes(label=ifelse(between(prct, 1.1, 2.9),as.character(Factor),'')), position = position_stack(vjust = 4),size=4,colour = 'black')+
  geom_text(aes(label=ifelse(prct <= 1,as.character(Factor),'')), position = position_stack(vjust = 7),size=4,colour = 'black')+
  coord_flip(ylim = c(1,31)) +
  labs(title = 'NYC Motor Vehicle Collisions Factors',
       x = 'Collision Factor',
       y = 'Percentage') +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size=12, face="bold"),
        legend.text = element_text(size=10),
        axis.text.x = element_text(size =12),axis.text.y = element_blank())
dev.off()

############################# 5. Top 5 street collison #############################
street_coln <- summarise(group_by(data_update,On_Street,Cross_street),count =n())
street_coln <- arrange(street_coln,desc(count))
# Top 10 dangerous streets 
top_danger_street <- street_coln[2:11,]
# view(top_danger_street)

# Most dangerous cross spots
cross_coln <- subset(street_coln,(!is.na(street_coln[,2])) & (!is.na(street_coln[,3])))
cross_coln <- arrange(cross_coln,desc(count))
cross_coln$cross <- with(cross_coln, paste0(On_Street,"  and  ",Cross_street))
# view(cross_coln)

# plot 
png(filename="5.1top_danger_st.png",width = 800, height = 400)
ggplot(data = top_danger_street) +
  aes(x = fct_reorder(On_Street,desc(-count)), y = count, fill = count,label = On_Street,yaxt='n') +
  geom_col() +
  geom_text(aes(label=On_Street), position = position_stack(vjust = 0.5),size=4,colour = 'White')+
  coord_flip() +
  labs(title = 'Most 10 Dangerous Streets',
       x = 'Street',
       y = 'Collision count') +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size=12, face="bold"),
        legend.text = element_text(size=10),
        axis.text.x = element_text(size =12),axis.text.y = element_blank())
dev.off()

cross_coln_plot <-cross_coln[1:20,]

png(filename="5.2top_danger_cross.png",width = 800, height = 400)
ggplot(data = cross_coln_plot) +
  aes(x = fct_reorder(cross,desc(-count)), y = count, fill = count,label = On_Street,yaxt='n') +
  geom_col() +
  geom_text(aes(label=cross), position = position_stack(vjust = 0.5),size=4,colour = 'White')+
  coord_flip() +
  labs(title = 'Most 20 Dangerous Street Cross',
       x = 'Cross Name',
       y = 'Collision count') +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size=12, face="bold"),
        legend.text = element_text(size=10),
        axis.text.x = element_text(size =12),axis.text.y = element_blank())
dev.off()

save.image(file = "final_collision.rdata")
