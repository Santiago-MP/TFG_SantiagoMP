library("tidyr")
setwd('/home/ada/ymartinez/Plate_Reader/growth_study/20230302_HB101_pGLO_OD600_GFP_induction_10h_C1')
path_to_functions <- 'generic_functions.R'
source(path_to_functions)

dataraw <-read.csv("20230302_pGLO_OD600_GFP_10h_C1.csv", sep = ";")
layout_path <- "230302_layout.csv"

#extraer valores
data_index <- which(regexpr("Cycle", dataraw$Application..Tecan.i.control)>0)
label_index <- data_index - 1
time_index <- data_index + 1
data_index <- data_index + 3


well <- rep((dataraw[data_index[1]:(data_index[1]+95),1]),ncol(dataraw)-1)

ind=2
canal = 1
cols = list()
for (canal in 1:length(data_index)) {
  new_cols = c()
  new_time = c()
  for (ind in 2:ncol(dataraw)){
    coltmp <- as.numeric(dataraw[data_index[canal]:(data_index[canal]+95),ind])
    new_cols <- c(new_cols, coltmp)
    timetmp <- rep(dataraw[time_index[canal], ind], 96)
    new_time <- c(new_time, timetmp)
  }
  cols[[canal]] <- new_cols
}

ndf <- data.frame(well = well, time = new_time, OD600 = cols[[1]], GFP = cols[[2]])


#unir datos con archivo layout#
data <- combine_info_file(ndf,
                               .info_file = layout_path,
                               .common_column = 'well')

#añadir columnas de 'fila' (row) y 'columna' (column)#
data$row <-substr(data$well,1,1)
data$column <- substr(data$well, 2,3)

#guardar datos ya procesados#
write.csv(data,"20230302_pGLO_OD600_GFP_10h_C1_parsed.csv", row.names = FALSE, na = '')
