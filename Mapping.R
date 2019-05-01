# install.packages("tidyverse")
# install.packages("ggmap")
# install.packages("maps")
# install.packages("mapproj")
# install.packages("maptools")
# install.packages("rgdal")
# install.packages("rgeos")
# install.packages("gganimate")
# install.packages("gifski")
library(tidyverse)
library(ggmap)
library(rgdal)
library(rgeos)
library(maptools)
library(gganimate)
library(ggplot2)
library(RColorBrewer)


RacialGroup = "WhF"

### Read in Data
states <- map_data("state")
ilmap <- states %>%
  filter(region=="illinois")
colleges <- readOGR("Colleges_and_Universities/Colleges_and_Universities.shp")
ILColleges <- colleges[colleges@data$STATE=="IL",]
myData <- read_csv("CleanedData.csv")

myData <- subset(myData, myData$InstrLevel=="Total")
locData <- as.data.frame(ILColleges@data)


##### Merging the two data frames (Fuzzy Merge)
## Taken from https://www.princeton.edu/~otorres/FuzzyMergeR101.pdf

# Separating the string variable from each dataset
myData.name = data.frame(myData$Institute)
names(myData.name)[names(myData.name)=="myData.Institute"] = "name.myData"
myData.name$name.myData = as.character(myData.name$name.myData)
myData.name = unique(myData.name)
head(myData.name)
locData.name = data.frame(locData$NAME)
names(locData.name)[names(locData.name)=="locData.NAME"] = "name.locData"
locData.name$name.locData = as.character(locData.name$name.locData)
locData.name = unique(locData.name)
head(locData.name)

# Matching string variables from myData to locData

myData.name$name.locData <- "" # Creating an empty column
for (i in 1:dim(myData.name)[1]) {
  x <- agrep(myData.name$name.myData[i], locData.name$name.locData,
             ignore.case=TRUE, value=TRUE,
             max.distance=0.05, useBytes=TRUE)
  x <- paste0(x,"")
  myData.name$name.locData[i] <- x
}

# Merging the key file myData.name to the original dataset myData
myData = merge(myData, myData.name, by.x=c("Institute"), by.y=c("name.myData"), all=TRUE)

# Merging the two original data files (keeping all data from both)
schools = merge(myData, locData, by.x=c("name.locData"), by.y=c("NAME"))[,c("name.locData","LATITUDE","LONGITUDE","Year","InstrLevel","AfAmF","AfAmM","AIANF","AIANM","AsianF","AsianM","HispF","HispM","NHOPIF","NHOPIM","WhF","WhM","MultF","MultM","NRAF","NRAM","UnkF","UnkM","AllF","AllM","GenderNoneTotal","AllTotal")]

schools <- subset(schools, LONGITUDE <= -84.6875 & LONGITUDE >= -93.3125 & LATITUDE >= 37 & LATITUDE <=42.75)
### Sort by Year
schools <- schools[order(schools$name.locData,schools$Year),]

### Calculate Differences
years <- sort(unique(schools$Year))
schoolNames <- unique(schools$name.locData)
diffData <- setNames(data.frame(matrix(ncol=3,nrow=0)),c("name.locData","Diffs","Year"))
for (i in 1:length(schoolNames)){
  this <- subset(schools,schools$name.locData == schoolNames[i])
  thisDiff <- diff(this[[RacialGroup]])
  for (j in 1:length(thisDiff)){
    diffData[nrow(diffData)+1,] = list(schoolNames[i], as.numeric(thisDiff[j]), as.numeric(years[j]))
  }
}
fullDiffData <- merge(schools,diffData, by.x=c("name.locData","Year"), by.y=c("name.locData","Year"))
fullDiffData <- subset(fullDiffData, fullDiffData$Year!=2016)

########################################################################################################################################
##################################  Creates Animation of Selected Group for Year Difference  ##########################################
########################################################################################################################################
cols <- brewer.pal(n = 5, name = "RdBu")

diffAnimation <- fullDiffData %>%
  group_by(Year) %>%
  ggplot() +  
  geom_polygon(data=ilmap,mapping=aes(x=long,y=lat),color="white",fill="black") +
  coord_map() +
  geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE,colour=Diffs,size=ifelse(WhF==0,NA, WhF)),alpha=0.5) +
  scale_size_area(max_size=10,name="White Female Enrollment",limits=c(0,30000)) +
  ggtitle(" White Female Enrollment in {1995+{frame}}") +
  xlab("Longitude") +
  ylab("Lattitude") +
  scale_colour_gradientn(colours = cols, 
                          values = scales::rescale(c(-1000, -500, 0, 500, 1000)),
                          guide = "colorbar", limits=c(-1000, 1000), name = "Year to Year \n Difference") +
  theme_void() +

  theme(plot.title=element_text(hjust=0.5)) +
  transition_manual(Year,cumulative = FALSE) +
  enter_fade() +
  exit_fade()

# animate(diffAnimation)
# animate(diffAnimation, fps=20, duration=57, end_pause=10)

anim_save("WhF_EnrollmentDifferences.gif", diffAnimation, fps=20, duration=57, end_pause=10)
########################################################################################################################################
########################################################################################################################################

names(fullDiffData)


# ########################################################################################################################################
# ##################################  Creates Animation of Selected Group for Total Population  ##########################################
# ########################################################################################################################################
# 
# animation <- schools %>%
#   group_by(Year) %>%
#   ggplot() +  
#   geom_polygon(data=ilmap,mapping=aes(x=long,y=lat),color="grey",fill="beige") +
#   coord_map() +
#   geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE,size=AllTotal),col="blue",alpha=0.3) +
#   scale_size_area(limits=c(1,30000),name="Total Enrollment") + #max_size=10,
#   xlab("Longitude") +
#   ylab("Lattitude") +
#   # ggtitle("Total Enrollment in {closest_state}") +
#   ggtitle("Total Enrollment in {1995+{frame}}") +
#   theme(plot.title=element_text(hjust=0.5)) +
#   transition_manual(Year,cumulative = FALSE) + #+
#   # transition_components(Year)
#   # ease_aes('linear') +
#   enter_fade() +
#   exit_fade()
#   # exit_disappear() +
#   # enter_appear()
# animate(animation)
# animate(animation, fps=20, duration=63, end_pause=10)
# 
# ########################################################################################################################################
# ########################################################################################################################################
# ########################################################################################################################################
# 
# 
# 
# 
# head(animation$data,50)
# ###
# unique(schools$name.locData)
# nrow(unique(schools[,c('name.locData','Year')]))
# nrow(unique(schools[,c('name.locData','LATITUDE','LONGITUDE')]))
# length(unique(schools$Year))
# 
# ABC <- schools %>%
#   filter(!is.na(Year)) %>%
#   filter(!is.na(LATITUDE)) %>%
#   filter(!is.na(Institute)) %>%
#   group_by(Institute) %>%
#   ggplot() +
#   geom_polygon(data=ilmap,mapping=aes(x=long,y=lat),color="grey",fill="beige") +
#   coord_map() +
#   geom_point(data=subset(schools),mapping=aes(x=LONGITUDE, y=LATITUDE,size=RacialGroup),col="blue",alpha=0.5) +
#   xlim(-93.3125,-84.6875) +
#   ylim(37,42.75) +
#   scale_size_area(limits=c(0,30000),na.value="-1",name="Total Enrollment") +
#   xlab("Longitude") +
#   ylab("Lattitude") +
#   ggtitle("Total Enrollment in Year") +
#   theme(plot.title=element_text(hjust=0.5)) +
#   transition_time(Year,range=c(1999,2018)) +
#   ease_aes('linear') +
#   enter_fade() +
#   exit_fade()
# 
# ABC
# # ABC  + transition_time(Year,range=c(1999,2019))
# 
# # na.omit(ABC$data$Year)
# # min(schools$Year)
# # 
# # 
# # is.na(schools$AllTotal)
# # 
# # schools[schools=='NA'] <- NA
# # test <- subset(schools,Year==1999)
# # test %>% group_by(Institute)
# # test2 <- na.omit(schools)
# # test2
# write.csv(schools, file="RData.csv")
# length(unique(schools$name.locData))
# length(schools$name.locData)
# 
# 
# fulldata <- subset(schools, aggregate(schools$Year, by=list(Category=schools$name.locData), FUN=sum)==40113 )
# 
# aggregate(fulldata$Year, by=list(Category=fulldata$name.locData), FUN=sum)
# 
# thisdata <- subset(myData,myData$Institute == "Bradley University")
# thisdata <- subset(thisdata,thisdata$InstrLevel == "Total")
# summarize(thisdata)
# thisdata$Year



## Scatter Plot Testing

# schools %>%
#   group_by(Year) %>%
#   ggplot() +
#   geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE),stat="identity") +
#   ggtitle("{closest_state}") +
#   transition_states(Year,transition_length=1,state_length=1)


