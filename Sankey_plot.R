rm(list=ls())

library(terra)
library(dplyr)
library(networkD3)

# Function to calculate number of pixel for each land cover from land cover image
create_data = function(fileInfo){
  # define node info
  nodeInfo = data.frame(         
                     nodeName="BAR", nodeID=0,  mapClass=1, nodeCol=1, nodeGroup='a') %>%
    rbind(data.frame(nodeName="BUP", nodeID=1,  mapClass=2, nodeCol=1, nodeGroup='b')) %>%
    rbind(data.frame(nodeName="FOR", nodeID=2,  mapClass=3, nodeCol=1, nodeGroup='c')) %>%
    rbind(data.frame(nodeName="AGR", nodeID=3,  mapClass=4, nodeCol=1, nodeGroup='d')) %>%
    rbind(data.frame(nodeName="WAT", nodeID=4,  mapClass=5, nodeCol=1, nodeGroup='e')) %>%
    
    rbind(data.frame(nodeName="BAR", nodeID=5,  mapClass=1, nodeCol=2, nodeGroup='a')) %>%
    rbind(data.frame(nodeName="BUP", nodeID=6,  mapClass=2, nodeCol=2, nodeGroup='b')) %>%
    rbind(data.frame(nodeName="FOR", nodeID=7,  mapClass=3, nodeCol=2, nodeGroup='c')) %>%
    rbind(data.frame(nodeName="AGR", nodeID=8,  mapClass=4, nodeCol=2, nodeGroup='d')) %>%
    rbind(data.frame(nodeName="WAT", nodeID=9,  mapClass=5, nodeCol=2, nodeGroup='e')) %>%
    
    rbind(data.frame(nodeName="BAR", nodeID=10,  mapClass=1, nodeCol=3, nodeGroup='a')) %>%
    rbind(data.frame(nodeName="BUP", nodeID=11,  mapClass=2, nodeCol=3, nodeGroup='b')) %>%
    rbind(data.frame(nodeName="FOR", nodeID=12,  mapClass=3, nodeCol=3, nodeGroup='c')) %>%
    rbind(data.frame(nodeName="AGR", nodeID=13,  mapClass=4, nodeCol=3, nodeGroup='d')) %>%
    rbind(data.frame(nodeName="WAT", nodeID=14,  mapClass=5, nodeCol=3, nodeGroup='e')) %>%
    
    rbind(data.frame(nodeName="BAR", nodeID=15, mapClass=1, nodeCol=4, nodeGroup='a')) %>%
    rbind(data.frame(nodeName="BUP", nodeID=16, mapClass=2, nodeCol=4, nodeGroup='b')) %>%
    rbind(data.frame(nodeName="FOR", nodeID=17, mapClass=3, nodeCol=4, nodeGroup='c')) %>%
    rbind(data.frame(nodeName="AGR", nodeID=18, mapClass=4, nodeCol=4, nodeGroup='d')) %>%
    rbind(data.frame(nodeName="WAT", nodeID=19, mapClass=5, nodeCol=4, nodeGroup='e')) 
  
  # define group color - note that the colors correspond to the nodeGroups, one for each unique group, we have used (a, b, c, d, e, f) - color is applied in order
  groupColor = c('#F5F5F5', '#f37f6b', '#4c7300', '#ffffbf', '#74b3ff')
  
  #################################################################################################################################################
  # collapse groupColor to a string
  groupColor = paste0('"',paste(groupColor, collapse = '", "'),'"')
  
  # join fileInfo to nodeInfo
  nodeInfo = dplyr::left_join(nodeInfo, fileInfo, by='nodeCol')
  
  # convert factors to characters
  nodeInfo$nodeName = as.character(nodeInfo$nodeName)
  nodeInfo$rasterFile = as.character(nodeInfo$rasterFile)
  
  # define the the links
  NodeCols = sort(unique(nodeInfo$nodeCol))
  linkInfo = data.frame()
  
  for(i in 1:(length(NodeCols)-1)){
    fromCol = dplyr::filter(nodeInfo, nodeCol==NodeCols[i])
    toCol = dplyr::filter(nodeInfo, nodeCol==NodeCols[i+1])
    fromR = values(rast(fromCol$rasterFile[1], fromCol$rasterBand[1]))
    toR = values(rast(toCol$rasterFile[1], toCol$rasterBand[1]))
    for(f in 1:nrow(fromCol)){
      for(t in 1:nrow(toCol)){
        nFromTo = length(which(fromR == fromCol$mapClass[f] & toR == toCol$mapClass[t]))
        linkInfo = rbind(linkInfo, data.frame(source=fromCol$nodeID[f], target=toCol$nodeID[t], value=nFromTo))
      }
    }
  }
  output = list(linkInfo=linkInfo, 
                groupColor=groupColor,
                nodeInfo=nodeInfo)
  return(output)
}


############################################################################

fontSize = 30
fontFamily = "TimeNew Roman"
nodeWidth = 30

##################################
# define file info

fileInfo = data.frame(         
                   nodeCol=1, rasterFile="./data/img_lcs_1990.tif", rasterBand=1) %>%
  rbind(data.frame(nodeCol=2, rasterFile="./data/img_lcs_2000.tif", rasterBand=1)) %>%
  rbind(data.frame(nodeCol=3, rasterFile="./data/img_lcs_2010.tif", rasterBand=1)) %>%
  rbind(data.frame(nodeCol=4, rasterFile="./data/img_lcs_2020.tif", rasterBand=1))

# Data extraction
data = create_data(fileInfo)

# Calculate Area
data$linkInfo$Area_km = data$linkInfo$value * 900 / 1000000

# Plot 
p= sankeyNetwork(Links = data$linkInfo, Nodes = data$nodeInfo,
              Source = "source",
              Target = "target",
              Value = "Area_km",
              NodeID = "nodeName",
              NodeGroup = "nodeGroup",
              fontSize = fontSize,
              fontFamily = fontFamily,
              nodeWidth = nodeWidth,
              height = 500, width = 800,
              nodePadding = 20,
              units = 'Km2',
              colourScale = paste0('d3.scaleOrdinal().range([',data$groupColor,'])')
              )
p

saveNetwork(p, "./sankey_plot.html")


