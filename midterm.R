#### Preparation and Submission ####
list_packages <- c("ggplot2","ggrepel","dplyr","tidyr","treemap","NMF",
                  "tidyverse","tidyquant","httr","jsonlite")

library(tidyverse)
library(dplyr)
library(ggrepel)
library(treemap)
library(NMF)
library(tidyquant)
library(httr)
library(jsonlite)
library(tidyr)
library(lubridate)
library(maps)
library(mapproj)

###############  1.Bureau of Economic Analysis (BEA) API ###############
#supply BEA API 
key_string <- 
  paste("https://apps.bea.gov/api/data/?&UserID=","79699785-FCE5-49F3-BF3F-71958279B254",
        "&method=GetData&DataSetName=NIPA&TableName=T10101&Frequency=Q&Year=ALL&ResultFormat=JSON", sep = "")
#pass URL and get JSON file
bea_gdp_api <- 
  fromJSON(key_string,
           simplifyDataFrame = FALSE, 
           simplifyMatrix = FALSE) %>% 
  pluck("BEAAPI","Results","Data") %>% 
  map_df(magrittr::extract, c("LineDescription", "TimePeriod", 
                              "SeriesCode", "DataValue")) 
# view(bea_gdp_api)
# summary(bea_gdp_api)
# write.csv(bea_gdp_value_api,file= "bea.csv")
#clean up using rename()
bea_gdp_perc_api <- 
  bea_gdp_api %>% 
  group_by(LineDescription) %>% 
  rename(account = LineDescription, 
         quarter = TimePeriod, 
         percent_change = DataValue) %>% 
  mutate(percent_change = as.numeric(percent_change),
         quarter = yq(quarter))
# view(bea_gdp_perc_api)

#modify account/group column names and "Series Code" names using case_when()
bea_gdp_value_wrangled <- 
  bea_gdp_perc_api %>% 
  ungroup() %>%  
  mutate(account = case_when(SeriesCode == "A253RL" ~ "Export Goods",
                             SeriesCode == "A255RL" ~ "Import Goods",
                             SeriesCode == "DGDSRL" ~ "Goods",
                             SeriesCode == "DSERRL" ~ "Services",
                             SeriesCode == "A656RL" ~ "Import Services",
                             SeriesCode == "A646RL" ~ "Export Services",
                             SeriesCode == "A822RL" ~ "Govt",
                             SeriesCode == "A006RL" ~ "Investment",
                             SeriesCode == "DPCERL" ~ "PCE",
                             TRUE ~ .$account)) %>% 
  group_by(account) %>% 
  select(-SeriesCode)
#visualization of GDP trend on a quarterly basis 2008-2018
png(filename="1.1GDP Growth perc.png",width = 800, height = 600) 
bea_gdp_perc_wrangled %>%
  filter(quarter > "2008-01-01") %>% 
  filter(account == "Investment" |
           account == "Exports" |
           account == "Imports" |
           account == "Govt") %>% 
  mutate(col_blue = 
           if_else(percent_change > 0, 
                   percent_change, as.numeric(NA)),
         col_red = 
           if_else(percent_change < 0, 
                   percent_change, as.numeric(NA))) %>%
  ggplot(aes(x = quarter)) +
  geom_col(aes(y = col_red),
           alpha = .85, 
           fill = "pink", 
           color = "pink") +
  geom_col(aes(y = col_blue),
           alpha = .85, 
           fill = "cornflowerblue", 
           color = "cornflowerblue") +
  ylab("Quarterly Change (percent)") +
  scale_x_date(breaks = scales::pretty_breaks(n = 5)) +
  labs(title = "Quarterly GDP Growth",
       subtitle = " (since 2008)",
       x = "",
       caption = "more here: www.bea.gov/newsreleases/national/gdp/gdpnewsrelease.htm") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption=element_text(hjust=0)) +
  facet_wrap(~account)
dev.off()

### Change to T10105Q to reproduce a GDP value chart ###
#supply BEA API (change TableName to T10105)
key_string_new <- 
  paste("https://apps.bea.gov/api/data/?&UserID=","79699785-FCE5-49F3-BF3F-71958279B254",
        "&method=GetData&DataSetName=NIPA&TableName=T10105&Frequency=Q&Year=ALL&ResultFormat=JSON", sep = "")
#pass URL and get JSON file
bea_gdp_api_new <- 
  fromJSON(key_string_new,
           simplifyDataFrame = FALSE, 
           simplifyMatrix = FALSE) %>% 
  pluck("BEAAPI","Results","Data") %>% 
  map_df(magrittr::extract, c("LineDescription", "TimePeriod", 
                              "SeriesCode", "DataValue")) 
# view(bea_gdp_api_new)

#modify percent_change to reflect the GDP value 
bea_gdp_value_api <- 
  bea_gdp_api_new %>% 
  group_by(LineDescription) %>% 
  rename(account = LineDescription, 
         quarter = TimePeriod, 
         value = DataValue) %>% 
  # change from percent to actual value
  mutate(value = as.numeric(gsub(',','',value)),#remove "," in value
         quarter = yq(quarter))
# view(bea_gdp_value_api)
# summary(bea_gdp_value_api)
#modify the series numbers of T10105Q in the mutate
bea_gdp_value_wrangled <- 
  bea_gdp_value_api %>% 
  ungroup() %>%  
  mutate(account = case_when(SeriesCode == "A253RC" ~ "Export Goods",
                             SeriesCode == "A255RC" ~ "Import Goods",
                             SeriesCode == "DGDSRC" ~ "Goods",
                             SeriesCode == "DSERRC" ~ "Services",
                             SeriesCode == "A656RC" ~ "Import Services",
                             SeriesCode == "A646RC" ~ "Export Services",
                             SeriesCode == "A822RC" ~ "Govt",
                             SeriesCode == "A006RC" ~ "Investment",
                             SeriesCode == "DPCERC" ~ "PCE",
                             TRUE ~ .$account)) %>% 
  group_by(account) %>% 
  select(-SeriesCode)
# view(bea_gdp_value_wrangled)

#visualization of GDP value basis 2008-2018
png(filename="1.2GDP Growth value.png",width = 800, height = 600) 
bea_gdp_value_wrangled %>%
  filter(quarter > "2008-01-01") %>% 
  filter(account == "Investment" |
           account == "Exports" |
           account == "Imports" |
           account == "Govt") %>% 
  ggplot(aes(x = quarter,
             y = value/1000,
             color = account)) +
  geom_line(size = 1.5) +
  xlab("")+
  ylab("GDP (Billions of dollors)") +
  scale_color_brewer(palette = "Accent") +
  labs(title = "Quarterly GDP Growth",
       subtitle = " (since 2008)",
       color = "Account",
       caption = "More infomation please visit: www.bea.gov/newsreleases/national/gdp/gdpnewsrelease.htm \n 
              Govt:Govenment consumption expenditures and gross investment") +
  theme_minimal() 
dev.off()





###################### 2.Outlay Treemap ########################
### Obama 2017 Budget  ###
#load the data file
# getwd()
# setwd("/Users/sunlei/Desktop/EM622/hw/mid/")
outlays <- read_csv("outlays.csv")

#modify treemap data
outlays_treemap= outlays %>% filter(X2017>10000000) %>% select("Agency.Name", "Bureau.Name","BEA.Category", "X2017") %>%
  group_by(Agency.Name,Bureau.Name) %>% summarise(Outlay=sum(X2017))

# view(outlays_treemap)

#plot treemap for Obama 2017 Budget  
library(treemap)
png(filename="2.1 2017outlays.png",width = 800, height = 600) 
mytree<-treemap(outlays_treemap,index=c("Agency.Name", "Bureau.Name"),
                vSize="Outlay",palette = "Set2",
                fontsize.labels=c(15, 8),
                align.labels=list(c("center", "center"), c("left", "top")),
                lowerbound.cex.labels=.2,#if they don't fit, reduce to a minimum of .2*12 label size
                force.print.labels=TRUE, # draw all labels at fixed fontsize,
                title="Obama 2017 Budget Outlays")
dev.off()

### Trump 2019 Budget ###
outlays2019 <- read_csv("outlays-fy2019.csv")

#modify column names 
names(outlays2019) <- gsub(" ",".",names(outlays2019))
# head(outlays2019)

#modify treemap data
# head(outlays2019)
outlays2019_treemap= outlays2019 %>% filter(`2019`>10000000) %>% select("Agency.Name", "Bureau.Name","BEA.Category", "2019") %>%
  group_by(Agency.Name,Bureau.Name) %>% summarise(Outlay=sum(`2019`))
# view(outlays2019_treemap_try)
# summary(outlays2019)

#plot treemap
png(filename="2.2 2019outlays.png",width = 800, height = 600) 
mytree_2019<-treemap(outlays2019_treemap,index=c("Agency.Name","Bureau.Name"),
                vSize="Outlay",palette = "Set2",
                fontsize.labels=c(15, 8),
                align.labels=list(c("center", "center"), c("left", "top")),
                lowerbound.cex.labels=.2,#if they don't fit, reduce to a minimum of .2*12 label size
                force.print.labels=TRUE, # draw all labels at fixed fontsize,
                title="Trump 2019 Budget Outlays") 
dev.off()

### Comparison Summary: ####
# Comparing the 2017 and 2019 treemap, the majority of both budget outlays are similar: Department 
# of Health and Human Services and Social Security Administration take up to 50% of the outlays, 
# and in both 2017 and 2019 budget outlays the 3rd and 4th are also Department of the Treasury 
# and Department of Defense-Military Programs. This shows that those bureaus are constantly 
# essential for the govenment, which makes sence that citizens' medical services and employments 
# as well as nation's economy and military are key factors.
# 
# However, the rest parts are quite different.According to 2017 Obama Budget Outlays plot, 
# Obama's govenment took more concerns and actions on defense civil and infrastructions as well as
# veterans and agriculture. While in 2019 Trump Budget Outlays plot it seems the govenment concerns
# veterans a little more and then defense civil programs. An obvious different is that the U.S.
# govenment switch the focus from transportation and education to international assistance.  





############################# 3.Heatmap ###############################
library("imputeTS")
energy <- read_csv("energy_data.csv")
# view(energy_new_update)

# modify data
energy_new_update <- data.matrix(energy_new[,-1])
rownames(energy_new_update) <-energy$Country
energy_new_update = subset(energy_new_update, select = -Total)
#plotting
png(filename="3. energy heapmap.png",width = 600, height = 400) 
aheatmap(energy_new_update, 
         color="RdBu:50", # color with custom features
         Rowv=FALSE, Colv=FALSE, # disable dendrogram
         scale = "row",breaks = NA, # key label range
         cexCol=0.8, cexRow=2,# column/row height
         main="Primary Energy Production \n (Petajoules and Gigajoules per capita)")
dev.off()

############################ 4.Geographic Map ##################################
energy_map <- read_csv("map_df.csv")
world_map <- map_data("world")
#modify country names
energy_map$Country_Area[energy_map$Country_Area=="Russian Federation"] <-"Russian"
energy_map$Country_Area[energy_map$Country_Area=="United States"] <-"USA"
# view(energy_map)

merge_map <-merge(world_map,energy_map,by.x="region",by.y="Country_Area")
merge_map <-arrange(merge_map,group,order)
# view(merge_map)

###plotting
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

#plott map
png(filename="4. map.png",width = 800, height = 400)
ggplot(energy_map,aes(map_id =Country_Area, fill =Energy_Supply_per_capita ))+
  geom_map(map = world_map, colour = "black")+
  scale_fill_distiller(palette = "Spectral",trans ="log1p",breaks = c(10, 30, 100, 300))+
  expand_limits(x = world_map$long, y = world_map$lat)+
  labs(title = "Energy Supply World Map",
       fill = "Energy Supply \nper capita(log)"
       )+
  theme_clean()
dev.off()

#################### 5.Your Own Plot ##########################
#Multivariate Plot 
summary(crimes,3)
summary(energy_p5_update,3)
#data manipulation
energy_p5 <-energy_new_update

#merge data into matrix
energy_p5_update <- reshape2::melt(energy_p5,id =1)
energy_p5_update <-
  energy_p5_update %>% 
  rename(
    Country_Area = Var1,
    Energy_type = Var2,
    Value = value)
# summary(energy_p5)
#modify country names
energy_p5_update$Country_Area <- as.character(energy_p5_update$Country_Area)
energy_p5_update$Country_Area[energy_p5_update$Country_Area=="Russian Federation"] <-"Russian"
energy_p5_update$Country_Area[energy_p5_update$Country_Area=="United States"] <-"USA"
energy_plot <-data.frame(energy_p5_update)
#add a new column
energy_plot <- energy_plot %>%
  mutate(Value_sqrt=sqrt(Value))
#plot submaps
png(filename="5.energy_multi_maps.png",width = 800, height = 600) 
if (require(maps)){
     world_map <- map_data("world")
     last_plot() + coord_map()
     ggplot(energy_plot, aes(map_id = Country_Area)) +
         geom_map(aes(fill = Value_sqrt), map = world_map) +
       # scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B",
       #                      midpoint=median(energy_plot$Value_sqrt))+
       scale_fill_distiller(palette = "Spectral",trans ="log1p",breaks = c(10, 30, 100))+
         expand_limits(x = world_map$long, y = world_map$lat) +
       labs(title = "Primary Energy Production \n (Petajoules and Gigajoules per capita)",
            fill = "Energy Value Sqrt")+
         facet_wrap( ~ Energy_type)+
       theme_light() %+replace%theme(
         axis.title = element_blank(),
         axis.text = element_blank(),
         panel.spacing = unit(0, "lines"),
         plot.margin = unit(c(0, 0, 0, 0), "lines"),
         plot.title = element_text(size=12,hjust = 0.5),
         complete = TRUE
       )
}
dev.off()

### Comparison summary ###
# In order to see each energy distribution among counties in "Energy" category, I choose 
# geographic map to plot each category,and get 5 sub world map plots:Solids, Liquidz,Gases, 
# Electricity,and Heat. One of the Pros of the visulazation if that it is clear to compare 
# each category; and the range of energy level is pretty straight forward. One of Cons is that
# it is difficult to tell the total energy of particular country. And since it is static plot,
# it's hard to tell the exact value. 
# 
# On the other hand, the sample chord diagram is interactive and each value is clear. Besides, 
# it tells the distribution of energy types of each country.So I would say that chord diagram 
# is in general better visulazation. However, geographic map is a good option particularly for 
# region/states distribution analysis. 

# save file in RData format
save.image(file = "midterm_lei.rdata")

