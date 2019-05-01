install.packages("tidyverse")
install.packages("ggmap")
install.packages("maps")
install.packages("mapproj")
install.packages("maptools")
install.packages("rgdal")
install.packages("rgeos")
install.packages("gganimate")
install.packages("gifski")
install.packages("magick")
library(tidyverse)
library(ggmap)
library(rgdal)
library(rgeos)
library(maptools)
library(gganimate)
library(ggplot2)
library(RColorBrewer)
library(magick)

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
diffData <- setNames(data.frame(matrix(ncol=24,nrow=0)),c("name.locData","AllDiff","AllFDiff","AllMDiff","AfAmFDiff","AfAmMDiff","AIANFDiff",
                                                         "AIANMDiff","AsianFDiff","AsianMDiff","HispFDiff","HispMDiff","NHOPIFDiff",
                                                         "NHOPIMDiff","WhFDiff","WhMDiff","MultFDiff","MultMDiff","NRAFDiff","NRAMDiff",
                                                         "UnkFDiff","UnkMDiff","GenderNoneTotalDiff","Year"))

for (i in 1:length(schoolNames)){
  this <- subset(schools,schools$name.locData == schoolNames[i])
  AllDiff <- diff(this$AllTotal)
  AllFDiff <- diff(this$AllF)
  AllMDiff <- diff(this$AllM)
  AfAmFDiff <- diff(this$AfAmF)
  AfAmMDiff <- diff(this$AfAmM)
  AIANFDiff <- diff(this$AIANF)
  AIANMDiff <- diff(this$AIANM)
  AsianFDiff <- diff(this$AsianF)
  AsianMDiff <- diff(this$AsianM)
  HispFDiff <- diff(this$HispF)
  HispMDiff <- diff(this$HispM)
  NHOPIFDiff <- diff(this$NHOPIF)
  NHOPIMDiff <- diff(this$NHOPIM)
  WhFDiff <- diff(this$WhF)
  WhMDiff <- diff(this$WhM)
  MultFDiff <- diff(this$MultF)
  MultMDiff <- diff(this$MultM)
  NRAFDiff <- diff(this$NRAF)
  NRAMDiff <- diff(this$NRAM)
  UnkFDiff <- diff(this$UnkF)
  UnkMDiff <- diff(this$UnkM)
  GenderNoneTotalDiff <- diff(this$GenderNoneTotal)

  for (j in 1:length(AllDiff)){
    diffData[nrow(diffData)+1,] = list(schoolNames[i], as.numeric(AllDiff[j]), as.numeric(AllFDiff[j]),as.numeric(AllMDiff[j]),
                                      as.numeric(AfAmFDiff[j]), as.numeric(AfAmMDiff[j]), as.numeric(AIANFDiff[j]),
                                      as.numeric(AIANMDiff[j]), as.numeric(AsianFDiff[j]), as.numeric(AsianMDiff[j]), as.numeric(HispFDiff[j]),
                                      as.numeric(HispMDiff[j]), as.numeric(NHOPIFDiff[j]), as.numeric(NHOPIMDiff[j]), as.numeric(WhFDiff[j]),
                                      as.numeric(WhMDiff[j]), as.numeric(MultFDiff[j]), as.numeric(MultMDiff[j]), as.numeric(NRAFDiff[j]),
                                      as.numeric(NRAMDiff[j]), as.numeric(UnkFDiff[j]), as.numeric(UnkMDiff[j]), as.numeric(GenderNoneTotalDiff[j]),
                                      as.numeric(years[j]))
  }
}

fullDiffData <- merge(schools,diffData, by.x=c("name.locData","Year"), by.y=c("name.locData","Year"))
fullDiffData <- subset(fullDiffData, fullDiffData$Year!=2016)
cols <- brewer.pal(n = 5, name = "RdBu")

########################################################################################################################################
##################################  Creates Animation of Selected Group for Year Difference  ##########################################
########################################################################################################################################

diffAnimation_AllTotal <- fullDiffData %>%
  group_by(Year) %>%
  ggplot() +  
  geom_polygon(data=ilmap,mapping=aes(x=long,y=lat),color="white",fill="black") +
  coord_map() +
  geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE,colour=AllTotal,size=ifelse(AllTotal==0,NA, AllTotal)),alpha=0.5) +
  scale_size_area(max_size=10,name="Total Enrollment",limits=c(0,30000)) +
  ggtitle(" Total Enrollment in {1995+{frame}}") +
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

diffAnimation_AllTotal <- animate(diffAnimation_AllTotal, fps=20, duration=57, end_pause=10)
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
##################################  Creates Animation of Selected Group for Year Difference  ##########################################
########################################################################################################################################

diffAnimation_AllF <- fullDiffData %>%
  group_by(Year) %>%
  ggplot() +  
  geom_polygon(data=ilmap,mapping=aes(x=long,y=lat),color="white",fill="black") +
  coord_map() +
  geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE,colour=AllF,size=ifelse(AllF==0,NA, AllF)),alpha=0.5) +
  scale_size_area(max_size=10,name="Total Female Enrollment",limits=c(0,30000)) +
  ggtitle(" Total Female Enrollment in {1995+{frame}}") +
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

diffAnimation_AllF <- animate(diffAnimation_AllF, fps=20, duration=57, end_pause=10)
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
##################################  Creates Animation of Selected Group for Year Difference  ##########################################
########################################################################################################################################

diffAnimation_AllM <- fullDiffData %>%
  group_by(Year) %>%
  ggplot() +  
  geom_polygon(data=ilmap,mapping=aes(x=long,y=lat),color="white",fill="black") +
  coord_map() +
  geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE,colour=AllM,size=ifelse(AllM==0,NA, AllM)),alpha=0.5) +
  scale_size_area(max_size=10,name="Total Male Enrollment",limits=c(0,30000)) +
  ggtitle(" Total Male Enrollment in {1995+{frame}}") +
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

diffAnimation_AllM <- animate(diffAnimation_AllM, fps=20, duration=57, end_pause=10)
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
##################################  Creates Animation of Selected Group for Year Difference  ##########################################
########################################################################################################################################

diffAnimation_AfAmFDiff <- fullDiffData %>%
  group_by(Year) %>%
  ggplot() +  
  geom_polygon(data=ilmap,mapping=aes(x=long,y=lat),color="white",fill="black") +
  coord_map() +
  geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE,colour=AfAmFDiff,size=ifelse(AfAmF==0,NA, AfAmF)),alpha=0.5) +
  scale_size_area(max_size=10,name="African American \n Female Enrollment",limits=c(0,30000)) +
  ggtitle(" African American \n Female Enrollment in {1995+{frame}}") +
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

diffAnimation_AfAmFDiff <- animate(diffAnimation_AfAmFDiff, fps=20, duration=57, end_pause=10)
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
##################################  Creates Animation of Selected Group for Year Difference  ##########################################
########################################################################################################################################

diffAnimation_AfAmMDiff <- fullDiffData %>%
  group_by(Year) %>%
  ggplot() +  
  geom_polygon(data=ilmap,mapping=aes(x=long,y=lat),color="white",fill="black") +
  coord_map() +
  geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE,colour=AfAmMDiff,size=ifelse(AfAmM==0,NA, AfAmM)),alpha=0.5) +
  scale_size_area(max_size=10,name="African American \n Male Enrollment",limits=c(0,30000)) +
  ggtitle(" African American \n Male Enrollment in {1995+{frame}}") +
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

diffAnimation_AfAmMDiff <- animate(diffAnimation_AfAmMDiff, fps=20, duration=57, end_pause=10)
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
##################################  Creates Animation of Selected Group for Year Difference  ##########################################
########################################################################################################################################

diffAnimation_AIANFDiff <- fullDiffData %>%
  group_by(Year) %>%
  ggplot() +  
  geom_polygon(data=ilmap,mapping=aes(x=long,y=lat),color="white",fill="black") +
  coord_map() +
  geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE,colour=AIANFDiff,size=ifelse(AIANF==0,NA, AIANF)),alpha=0.5) +
  scale_size_area(max_size=10,name="American Idian \n Female Enrollment",limits=c(0,30000)) +
  ggtitle(" American Indian \n Female Enrollment in {1995+{frame}}") +
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

diffAnimation_AIANFDiff <- animate(diffAnimation_AIANFDiff, fps=20, duration=57, end_pause=10)
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
##################################  Creates Animation of Selected Group for Year Difference  ##########################################
########################################################################################################################################

diffAnimation_AIANMDiff <- fullDiffData %>%
  group_by(Year) %>%
  ggplot() +  
  geom_polygon(data=ilmap,mapping=aes(x=long,y=lat),color="white",fill="black") +
  coord_map() +
  geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE,colour=AIANMDiff,size=ifelse(AIANM==0,NA, AIANM)),alpha=0.5) +
  scale_size_area(max_size=10,name="American Idian \n Male Enrollment",limits=c(0,30000)) +
  ggtitle(" American Indian \n Male Enrollment in {1995+{frame}}") +
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

diffAnimation_AIANMDiff <- animate(diffAnimation_AIANMDiff, fps=20, duration=57, end_pause=10)
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
##################################  Creates Animation of Selected Group for Year Difference  ##########################################
########################################################################################################################################

diffAnimation_AsianFDiff <- fullDiffData %>%
  group_by(Year) %>%
  ggplot() +  
  geom_polygon(data=ilmap,mapping=aes(x=long,y=lat),color="white",fill="black") +
  coord_map() +
  geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE,colour=AsianFDiff,size=ifelse(AsianF==0,NA, AsianF)),alpha=0.5) +
  scale_size_area(max_size=10,name="Asian Female Enrollment",limits=c(0,30000)) +
  ggtitle(" Asian Female Enrollment in {1995+{frame}}") +
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

diffAnimation_AsianFDiff <- animate(diffAnimation_AsianFDiff, fps=20, duration=57, end_pause=10)
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
##################################  Creates Animation of Selected Group for Year Difference  ##########################################
########################################################################################################################################

diffAnimation_AsianMDiff <- fullDiffData %>%
  group_by(Year) %>%
  ggplot() +  
  geom_polygon(data=ilmap,mapping=aes(x=long,y=lat),color="white",fill="black") +
  coord_map() +
  geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE,colour=AsianMDiff,size=ifelse(AsianM==0,NA, AsianM)),alpha=0.5) +
  scale_size_area(max_size=10,name="Asian Male Enrollment",limits=c(0,30000)) +
  ggtitle(" Asian Male Enrollment in {1995+{frame}}") +
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

diffAnimation_AsianMDiff <- animate(diffAnimation_AsianMDiff, fps=20, duration=57, end_pause=10)
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
##################################  Creates Animation of Selected Group for Year Difference  ##########################################
########################################################################################################################################

diffAnimation_HispFDiff <- fullDiffData %>%
  group_by(Year) %>%
  ggplot() +  
  geom_polygon(data=ilmap,mapping=aes(x=long,y=lat),color="white",fill="black") +
  coord_map() +
  geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE,colour=HispFDiff,size=ifelse(HispF==0,NA, HispF)),alpha=0.5) +
  scale_size_area(max_size=10,name="Hispanic Female Enrollment",limits=c(0,30000)) +
  ggtitle(" Hispanic Female Enrollment in {1995+{frame}}") +
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

diffAnimation_HispFDiff <- animate(diffAnimation_HispFDiff, fps=20, duration=57, end_pause=10)
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
##################################  Creates Animation of Selected Group for Year Difference  ##########################################
########################################################################################################################################

diffAnimation_HispMDiff <- fullDiffData %>%
  group_by(Year) %>%
  ggplot() +  
  geom_polygon(data=ilmap,mapping=aes(x=long,y=lat),color="white",fill="black") +
  coord_map() +
  geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE,colour=HispFDiff,size=ifelse(HispM==0,NA, HispM)),alpha=0.5) +
  scale_size_area(max_size=10,name="Hispanic Male Enrollment",limits=c(0,30000)) +
  ggtitle(" Hispanic Male Enrollment in {1995+{frame}}") +
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

diffAnimation_HispMDiff <- animate(diffAnimation_HispMDiff, fps=20, duration=57, end_pause=10)
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
##################################  Creates Animation of Selected Group for Year Difference  ##########################################
########################################################################################################################################

diffAnimation_NHOPIFDiff <- fullDiffData %>%
  group_by(Year) %>%
  ggplot() +  
  geom_polygon(data=ilmap,mapping=aes(x=long,y=lat),color="white",fill="black") +
  coord_map() +
  geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE,colour=NHOPIFDiff,size=ifelse(NHOPIF==0,NA, NHOPIF)),alpha=0.5) +
  scale_size_area(max_size=10,name="Pacific Islander \n Female Enrollment",limits=c(0,30000)) +
  ggtitle(" Pacific Islander \n Female Enrollment in {1995+{frame}}") +
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

diffAnimation_NHOPIFDiff <- animate(diffAnimation_NHOPIFDiff, fps=20, duration=57, end_pause=10)
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
##################################  Creates Animation of Selected Group for Year Difference  ##########################################
########################################################################################################################################

diffAnimation_NHOPIMDiff <- fullDiffData %>%
  group_by(Year) %>%
  ggplot() +  
  geom_polygon(data=ilmap,mapping=aes(x=long,y=lat),color="white",fill="black") +
  coord_map() +
  geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE,colour=NHOPIFDiff,size=ifelse(NHOPIM==0,NA, NHOPIM)),alpha=0.5) +
  scale_size_area(max_size=10,name="Pacific Islander \n Male Enrollment",limits=c(0,30000)) +
  ggtitle(" Pacific Islander \n Male Enrollment in {1995+{frame}}") +
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

diffAnimation_NHOPIMDiff <- animate(diffAnimation_NHOPIMDiff, fps=20, duration=57, end_pause=10)
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
##################################  Creates Animation of Selected Group for Year Difference  ##########################################
########################################################################################################################################

diffAnimation_WhFDiff <- fullDiffData %>%
  group_by(Year) %>%
  ggplot() +  
  geom_polygon(data=ilmap,mapping=aes(x=long,y=lat),color="white",fill="black") +
  coord_map() +
  geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE,colour=WhFDiff,size=ifelse(WhF==0,NA, WhF)),alpha=0.5) +
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

diffAnimation_WhFDiff <- animate(diffAnimation_WhFDiff, fps=20, duration=57, end_pause=10)
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
##################################  Creates Animation of Selected Group for Year Difference  ##########################################
########################################################################################################################################

diffAnimation_WhMDiff <- fullDiffData %>%
  group_by(Year) %>%
  ggplot() +  
  geom_polygon(data=ilmap,mapping=aes(x=long,y=lat),color="white",fill="black") +
  coord_map() +
  geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE,colour=WhMDiff,size=ifelse(WhM==0,NA, WhM)),alpha=0.5) +
  scale_size_area(max_size=10,name="White Male Enrollment",limits=c(0,30000)) +
  ggtitle(" White Male Enrollment in {1995+{frame}}") +
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

diffAnimation_WhMDiff <- animate(diffAnimation_WhMDiff, fps=20, duration=57, end_pause=10)
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
##################################  Creates Animation of Selected Group for Year Difference  ##########################################
########################################################################################################################################

diffAnimation_MultFDiff<- fullDiffData %>%
  group_by(Year) %>%
  ggplot() +  
  geom_polygon(data=ilmap,mapping=aes(x=long,y=lat),color="white",fill="black") +
  coord_map() +
  geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE,colour=MultFDiff,size=ifelse(MultF==0,NA, MultF)),alpha=0.5) +
  scale_size_area(max_size=10,name="Multi Racial Female Enrollment",limits=c(0,30000)) +
  ggtitle(" Multi Racial Female Enrollment in {1995+{frame}}") +
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

diffAnimation_MultFDiff <- animate(diffAnimation_MultFDiff, fps=20, duration=57, end_pause=10)
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
##################################  Creates Animation of Selected Group for Year Difference  ##########################################
########################################################################################################################################

diffAnimation_MultMDiff<- fullDiffData %>%
  group_by(Year) %>%
  ggplot() +  
  geom_polygon(data=ilmap,mapping=aes(x=long,y=lat),color="white",fill="black") +
  coord_map() +
  geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE,colour=MultMDiff,size=ifelse(MultM==0,NA, MultM)),alpha=0.5) +
  scale_size_area(max_size=10,name="Multi Racial Male Enrollment",limits=c(0,30000)) +
  ggtitle(" Multi Racial Male Enrollment in {1995+{frame}}") +
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

diffAnimation_MultMDiff <- animate(diffAnimation_MultMDiff, fps=20, duration=57, end_pause=10)
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
##################################  Creates Animation of Selected Group for Year Difference  ##########################################
########################################################################################################################################

diffAnimation_NRAFDiff<- fullDiffData %>%
  group_by(Year) %>%
  ggplot() +  
  geom_polygon(data=ilmap,mapping=aes(x=long,y=lat),color="white",fill="black") +
  coord_map() +
  geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE,colour=NRAFDiff,size=ifelse(NRAF==0,NA, NRAF)),alpha=0.5) +
  scale_size_area(max_size=10,name="Alien Female Enrollment",limits=c(0,30000)) +
  ggtitle(" Alien Female Enrollment in {1995+{frame}}") +
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

diffAnimation_NRAFDiff <- animate(diffAnimation_NRAFDiff, fps=20, duration=57, end_pause=10)
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
##################################  Creates Animation of Selected Group for Year Difference  ##########################################
########################################################################################################################################

diffAnimation_NRAMDiff<- fullDiffData %>%
  group_by(Year) %>%
  ggplot() +  
  geom_polygon(data=ilmap,mapping=aes(x=long,y=lat),color="white",fill="black") +
  coord_map() +
  geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE,colour=NRAMDiff,size=ifelse(NRAM==0,NA, NRAM)),alpha=0.5) +
  scale_size_area(max_size=10,name="Alien Male Enrollment",limits=c(0,30000)) +
  ggtitle(" Alien Male Enrollment in {1995+{frame}}") +
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

diffAnimation_NRAMDiff <- animate(diffAnimation_NRAMDiff, fps=20, duration=57, end_pause=10)
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
##################################  Creates Animation of Selected Group for Year Difference  ##########################################
########################################################################################################################################

diffAnimation_UnkFDiff<- fullDiffData %>%
  group_by(Year) %>%
  ggplot() +  
  geom_polygon(data=ilmap,mapping=aes(x=long,y=lat),color="white",fill="black") +
  coord_map() +
  geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE,colour=UnkFDiff,size=ifelse(UnkF==0,NA, UnkF)),alpha=0.5) +
  scale_size_area(max_size=10,name="Unknown Female Enrollment",limits=c(0,30000)) +
  ggtitle(" Unknown Female Enrollment in {1995+{frame}}") +
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

diffAnimation_UnkFDiff <- animate(diffAnimation_UnkFDiff, fps=20, duration=57, end_pause=10)
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
##################################  Creates Animation of Selected Group for Year Difference  ##########################################
########################################################################################################################################

diffAnimation_UnkMDiff<- fullDiffData %>%
  group_by(Year) %>%
  ggplot() +  
  geom_polygon(data=ilmap,mapping=aes(x=long,y=lat),color="white",fill="black") +
  coord_map() +
  geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE,colour=UnkMDiff,size=ifelse(UnkM==0,NA, UnkM)),alpha=0.5) +
  scale_size_area(max_size=10,name="Unknown Male Enrollment",limits=c(0,30000)) +
  ggtitle(" Unknown Male Enrollment in {1995+{frame}}") +
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

diffAnimation_UnkMDiff <- animate(diffAnimation_UnkMDiff, fps=20, duration=57, end_pause=10)
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
##################################  Creates Animation of Selected Group for Year Difference  ##########################################
########################################################################################################################################

diffAnimation_GenderNoneTotalDiff<- fullDiffData %>%
  group_by(Year) %>%
  ggplot() +  
  geom_polygon(data=ilmap,mapping=aes(x=long,y=lat),color="white",fill="black") +
  coord_map() +
  geom_point(mapping=aes(x=LONGITUDE,y=LATITUDE,colour=GenderNoneTotalDiff,size=ifelse(GenderNoneTotal==0,NA, GenderNoneTotal)),alpha=0.5) +
  scale_size_area(max_size=10,name="Unknown Male Enrollment",limits=c(0,30000)) +
  ggtitle(" Unknown Male Enrollment in {1995+{frame}}") +
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

diffAnimation_GenderNoneTotalDiff <- animate(diffAnimation_GenderNoneTotalDiff, fps=20, duration=57, end_pause=10)
########################################################################################################################################
########################################################################################################################################

# names(fullDiffData)

# grid.arrange(diffAnimation,diffAnimation)

diffAnimation_AllTotalgif <- image_read(diffAnimation_AllTotal)
diffAnimation_AllFgif <- image_read(diffAnimation_AllF)
diffAnimation_AllMgif <- image_read(diffAnimation_AllM)
diffAnimation_AfAmMgif <- image_read(diffAnimation_AfAmMDiff)
diffAnimation_AfAmFgif <- image_read(diffAnimation_AfAmFDiff)
diffAnimation_AsianFgif <- image_read(diffAnimation_AsianFDiff)
diffAnimation_AsianMgif <- image_read(diffAnimation_AsianMDiff)
diffAnimation_HispMgif <- image_read(diffAnimation_HispMDiff)
diffAnimation_HispFgif <- image_read(diffAnimation_HispFDiff)
diffAnimation_WhFgif <- image_read(diffAnimation_WhFDiff)
diffAnimation_WhMgif <- image_read(diffAnimation_WhMDiff)


# new_gif <- image_append(c(diffAnimation_AllTotalgif[1], diffAnimation_AllFgif[1], 
#                           diffAnimation_WhFgif[1], diffAnimation_AfAmFgif[1],
#                           diffAnimation_AsianFgif[1]))
# 
# new_gifM <- image_append(c(diffAnimation_AllTotalgif[1],diffAnimation_AllMgif[1], 
#                           diffAnimation_WhMgif[1], diffAnimation_AfAmMgif[1],
#                           diffAnimation_AsianMgif[1]))
# 
# new_gifFinal <- image_append(c(new_gif,newgifM))

for (i in 2:length(diffAnimation_AllFgif)){
  combinedF <- image_append(c(diffAnimation_AllTotalgif[i], diffAnimation_AllFgif[i], 
                             diffAnimation_WhFgif[i], diffAnimation_AfAmFgif[i],
                             diffAnimation_AsianFgif[i]))
  new_gifF <- c(new_gifF, combinedF)
  
  combinedM <- image_append(c(diffAnimation_AllTotalgif[i], diffAnimation_AllMgif[i], 
                              diffAnimation_WhMgif[i], diffAnimation_AfAmMgif[i],
                              diffAnimation_AsianMgif[i]))
  new_gifM <- c(new_giM, combinedM)
}



anim_save("Females.gif",new_gifF)
anim_save("Males.gif",newgifM)


