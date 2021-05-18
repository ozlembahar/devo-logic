require(igraph)
require(BoolNet)
require(readxl)
require(readr)
setwd("~/Documents/Git/DevelopmentalSignaling")
Genelist <- read_xlsx("Egfr-Hh-Wg-Gene-List-FlybaseIDs.xlsx")

#Get the dataset
DataMatSym <- read.csv(file ="DatamatSym.csv")

# name the column 
require(dplyr)
colnames(DataMatSym)[1] = "Gene_symbol"

PathwayData <- merge(DataMatSym, Genelist, by="Gene_symbol")

install.packages("textshape")
require(textshape)
PathwayData<-column_to_rownames(PathwayData, loc = 1)
PathwayData <- PathwayData[-c(49, 50)]

# perform binarization with k-means
binarizedData <- binarizeTimeSeries(PathwayData, method = "kmeans")
head(binarizedData)

# save binarized datasets as xls
write.xlsx(as.data.frame(binarizedData), file="Kmeans-BinarizedTimeSeries.xlsx") 
write.xlsx(as.data.frame(binarizedData), file="EdgeDetector-BinarizedTimeSeries.xlsx") 
write.xlsx(as.data.frame(binarizedData), file="ScanStatistic-BinarizedTimeSeries.xlsx") 




network <- reconstructNetwork(binarizedData$binarizedMeasurements,	method="reveal")

# Visually check sanity of loaded model (and return iGraph object)
graph = plotNetworkWiring(network, layout_with_fr, grid = "grid", vertex.size = 10)
layout = layout_as_tree(graph, circular = FALSE)
plot(graph, layout = layout)


# upload binarized datasets 
kmeans<- read_xlsx("Kmeans-BinarizedTimeSeries.xlsx")
kmeans<-column_to_rownames(kmeans, loc = 1)
edgeDetect<- read_xlsx("EdgeDetector-BinarizedTimeSeries.xlsx")
edgeDetect<-column_to_rownames(edgeDetect, loc = 1)
scanSta<- read_xlsx("ScanStatistic-BinarizedTimeSeries.xlsx")
scanSta<-column_to_rownames(scanSta, loc = 1)

# Plot the binary time-series
# specify the colors of the time-series
binarizedData <- as.data.frame(binarizedData)
require(ggplot2)
binarizedData  <-binarizedData[ ,grepl("pos", colnames(binarizedData))]
binarizedData <- binarizedData[, -c(1:8)]

ggplot(binarizedData$binarizedMeasurements.vnd_pos1_4.6, aes(x=rownames(binarizedData), y=c("st8", "st10", "st12")))
