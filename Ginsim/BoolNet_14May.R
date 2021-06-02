require(igraph)
require(BoolNet)
require(readxl)
require(readr)
setwd("~/Documents/Git/DevelopmentalSignaling")
Genelist <- read_xlsx("Egfr-Hh-Wg-Gene-List-FlybaseIDs.xlsx")
Genelist <- Genelist[c(1,4,6,9:13,18,19,21:23),]
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


##get mean of related columns
PathwayData = data.frame(
  IC_4.6h=rowMeans(PathwayData[, c(37, 39)]), 
  IC_6.8h=rowMeans(PathwayData[, c(38, 40)]),                                                     
  VC_4.6h =rowMeans(PathwayData[, c(45, 47)]),                 
  VC_6.8h =rowMeans(PathwayData[, c(46, 48)]), 
  NB_4.6h =rowMeans(PathwayData[, c(17, 19)]), #pros
  NB_6.8h=rowMeans(PathwayData[, c(18, 20)]), 
  Neuron.6.8h=rowMeans(PathwayData[, c(8, 11)]), #elav
  Neuron_8.10h  =rowMeans(PathwayData[, c(9, 12)]),                            
  Neuron_18.22h =rowMeans(PathwayData[, c(10, 7)]),                            
  Glia_6.8h =rowMeans(PathwayData[, c(28, 31)]),   #repo                         
  Glia_8.10h=rowMeans(PathwayData[, c(29, 32)]),                       
  Glia_18.22h=rowMeans(PathwayData[, c(27, 30)]))





# perform binarization with k-means
binarizedData <- binarizeTimeSeries(PathwayData, method = "kmeans")
head(binarizedData)

# save binarized datasets as xls
require(xlsx)
write.xlsx(as.data.frame(binarizedData), file="Kmeans-BinarizedTimeSeries_13Genes.xlsx") 
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
binarizedData <- binarizedData[, -c(1:13)]

ggplot(binarizedData$binarizedMeasurements.vnd_pos1_4.6, aes(x=rownames(binarizedData), y=c("st8", "st10", "st12")))

#Check expression graph of pnt
require(dplyr)

pnt_exp <- PathwayData %>%  filter(Gene_symbol == "pnt")
pnt_exp <- column_to_rownames(pnt_exp, loc = 1)
pnt_exp <- as.data.frame(t(PathwayData))
pnt_exp$time <- rownames(pnt_exp)
require(reshape2)
pnt_exp <- melt(pnt_exp, id.vars=c("time"))


require(ggplot2)
library(hrbrthemes)
pnt <- filter(pnt_exp, variable == "pnt")
pnt %>% 
  ggplot(aes(x=time, y=value, group = 1))+ 
  labs(x="Developmental time", y= "Gene expression", title="Pnt expression")+
  geom_point(shape=21, color="black", fill="#000000", size=3)+ 
  geom_line(color="grey")+
  theme_ipsum()+
  scale_x_discrete(limits = c("IC_4.6h", 
                              "IC_6.8h",                                                     
                              "VC_4.6h",                 
                              "VC_6.8h", 
                              "NB_4.6h",
                              "NB_6.8h", 
                              "Neuron.6.8h", 
                             "Neuron_8.10h",                            
                              "Neuron_18.22h",                            
                              "Glia_6.8h",                      
                              "Glia_8.10h",                       
                              "Glia_18.22h"))+
theme(axis.text.x = element_text(angle = 60), plot.title = element_text(hjust = 0.5))# Rotate axis labels
 
 
# Plot all of the genes at the same time
gene_exp<- as.data.frame(t(PathwayData))
gene_exp$time <- rownames(gene_exp)
require(reshape2)
gene_exp<- melt(gene_exp, id.vars=c("time"))

  
  
gene_exp %>% 
  ggplot(aes(x=time, y=value, col = variable, group = variable))+ 
  geom_line()+
  geom_point()+
  labs(x="Neurogenic populations", y= "Count")+
  scale_y_log10()+
  theme_ipsum()+
  scale_x_discrete(limits = c("IC_4.6h", 
                              "VC_4.6h",
                              "IC_6.8h",
                              "VC_6.8h"))+
  theme(axis.text.x = element_text(angle = 60), plot.title = element_text(hjust = 0.5))# Rotate axis labels


