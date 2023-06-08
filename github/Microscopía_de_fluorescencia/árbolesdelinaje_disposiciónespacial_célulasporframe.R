library(ViSCAR)
library(jsonlite)
library(tidyverse)
library(igraph)
library(ggplot2)
library(viridis)

#seleccionar la ubicación del archivo 'bacteria.csv' de la microcolonia deseada#
bact <- read.csv("/home/ada/ymartinez/Experiments/results/C1/bacteriaC1P12.csv",header=F)
bact <- bact[, -c(length(bact)-1,length(bact))]
names <- bact[1,]
bact <- bact[-1, ]
colnames(bact)<-names
sb <- bact[,c("NAME","TRAJECTORY","TRAJECTORY.lineage.name","TRAJECTORY.frame.start",
              "TRAJECTORY.lineage.index","SHAPE.length","INTENSITY.ch2.mean","LOCATION.x", "LOCATION.y","SHAPE.area")]#,"SHAPE.roundness")]
sb$TRAJECTORY.frame.start <- as.integer(sb$TRAJECTORY.frame.start)
sb$TRAJECTORY.frame.start <- sb$TRAJECTORY.frame.start + 1 # it has to be non zero and fiji index from 0
sb$TRAJECTORY.lineage.index <- unname(
  sapply(sb$TRAJECTORY.lineage.index,
         function(x) as.numeric(unlist(strsplit(x, split = "Lineage"))[2])
  )
)
cells <- list()
for(row in 1:nrow(sb)){
  cell <- sb[row,]
  cellName <- paste0(cell$TRAJECTORY,"_f",cell$TRAJECTORY.frame.start)
  frame <- cell$TRAJECTORY.frame.start
  # no ocurre división
  candidates <- sb[which(sb$TRAJECTORY == cell$TRAJECTORY &
                           sb$NAME!=cell$NAME &
                           sb$TRAJECTORY.frame.start > cell$TRAJECTORY.frame.start),]
  daughter <- candidates[which(candidates$TRAJECTORY.frame.start==min(candidates$TRAJECTORY.frame.start)),]
  daughterIds <- c(paste0(daughter$TRAJECTORY,"_f",daughter$TRAJECTORY.frame.start))
  # hay división
  if(nrow(daughter)==0){
    d1 <- sb[which(sb$TRAJECTORY.lineage.name==paste0(cell$TRAJECTORY.lineage.name,".1") &
                     sb$TRAJECTORY.lineage.index == cell$TRAJECTORY.lineage.index),]
    d1 <- d1[which(d1$TRAJECTORY.frame.start==min(d1$TRAJECTORY.frame.start)),]
    d2 <- sb[which(sb$TRAJECTORY.lineage.name==paste0(cell$TRAJECTORY.lineage.name,".2") &
                     sb$TRAJECTORY.lineage.index == cell$TRAJECTORY.lineage.index),]
    d2 <- d2[which(d2$TRAJECTORY.frame.start==min(d2$TRAJECTORY.frame.start)),]
    if(nrow(d1)==1 & nrow(d2)==1){
      daughterIds <- c(paste0(d1$TRAJECTORY,"_f",d1$TRAJECTORY.frame.start),
                       paste0(d2$TRAJECTORY,"_f",d2$TRAJECTORY.frame.start))
    } else{
      daughterIds <-c()
    }
  }
  cells[[row]] <- list(
    cellName=cellName,
    frame=frame,
    colony = 1,
    daughterIds=daughterIds,
    area=as.numeric(cell$SHAPE.area),
    fluo=as.numeric(cell$INTENSITY.ch2.mean),
    fluo_norm=(as.numeric(cell$INTENSITY.ch2.mean)*as.numeric(cell$SHAPE.area)),
    coord.x=as.numeric(cell$LOCATION.x),
    coord.y=as.numeric(cell$LOCATION.y)
  )
}
cells <- toJSON(cells)
write(cells, "cells.json")
imported_data <- import_json("cells.json", file_cols = NULL)
myFLT <- createFLT(cell_list = imported_data$cell_list, Ncols = imported_data$Ncols)
myFDT <- createFDT(LTmain = myFLT$LTmain, minLife = 5, frameR = 1)
FLT <- myFDT$LTmain
FDT <- myFDT$DTmain

#generar el gráfico número de células por frame#
cells_C1P12 <- plot_Ncells(tree = FLT, treeT = "LT", grouped = "frame", groups = -1, Ngroups = 180,save = FALSE)
##cells_C1P12 <- get_cells(tree = FLT, treeT = 'LT', type = 'all')
write.csv(cells_C1P12, 'C1P12_cells.csv')

#generar árbol de linaje (generación)
plot_tree(tree = FLT, treeT = "LT", attrC = "generation", NC = 11, attrS = "",
          cellsC = "", colorCol = FALSE, showLabels = FALSE, circular = FALSE,
          showLegends = TRUE, sizeV = 2, sizeE = 1)#, save = TRUE,
#savePars = list(w = 6000, h = 6000, res = 300, path = getwd(), name = "Fig 7a"))

#generar árbol de linaje (fluorescencia)
plot_tree(tree = FLT, treeT = "LT", attrC = "fluo",
          NC = 12, attrS = "", cellsC = "", colorCol = FALSE,
          showLabels = FALSE, circular = FALSE, showLegends = TRUE,
          sizeV = 2, sizeE = 1)#,
#save = TRUE, savePars = list(w = 6000, h = 6000, res = 300, path = getwd(), name = "Fig 7a"))

#generar una matriz de datos con las coordenadas del frame seleccionado#
x <- as.numeric(bact$LOCATION.x[bact$POSITION.frame==100])
y <- as.numeric(bact$LOCATION.y[bact$POSITION.frame==100])
fluo <- as.numeric(bact$INTENSITY.ch2.mean[bact$POSITION.frame==100])
area <- as.numeric(bact$SHAPE.area[bact$POSITION.frame==100])
space <- data.frame(x=x, y=y, fluo=fluo, area=area)
space$fluo_norm <- space$fluo/space$area


#generar el gráfico de disposición espacial de las células, y distintas opciones de representación#
ggplot(data = space, aes(x=x, y=y, color=fluo_norm))+
  geom_point()

ggplot(data = space, aes(x=x, y=y, color=fluo_norm))+
  geom_point(aes(size=area))+
  scale_color_viridis_c()


p <- ggplot(data = space, aes(x=x, y=y, size=area, color=fluo))+
  geom_point()+
  scale_x_continuous(breaks = seq(20,50,5))+
  scale_y_continuous(breaks = seq(0,50,5))+
  coord_fixed()+
  scale_size(name="Area μm²")+
  scale_color_viridis_c(name="Fluorescence a.u.")+
  ggtitle("Space distribution of cells - C1 Position 12")+
  xlab("x-coordinate (px)")+
  ylab("y-coordinate (px)")+
  scale_y_reverse()

p+theme(plot.title = element_text(size=14, face="bold", hjust=0.5))



p <- ggplot(data = space, aes(x=x, y=y, color=fluo_norm, size=fluo_norm))+
  geom_point(alpha=0.5)+
  scale_radius(range = c(20))+
  scale_x_continuous(breaks = seq(20,50,5))+
  scale_y_continuous(breaks = seq(0,50,5))+
  coord_fixed()+
  scale_color_viridis_c(name="Fluorescence a.u.")+
  ggtitle("Space distribution of cells - C1 Position 12")+
  xlab("x-coordinate (px)")+
  ylab("y-coordinate (px)")+
  scale_y_reverse()

p+theme(plot.title = element_text(size=14, face="bold", hjust=0.5))

