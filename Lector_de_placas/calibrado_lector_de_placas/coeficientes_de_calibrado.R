library(flopr)
library("tidyr")
setwd('/home/ada/ymartinez/Plate_Reader/calibration/20230301/definitiva')

#cargar algunas de las funciones necesarias para el procesado por medio del archivo adjunto (generic_functions.R)#
path_to_functions <- 'generic_functions.R'
source(path_to_functions)

caldata <-read.csv("Calibration_2023_03_01_film.csv")
callayout_path <- "calibration_plate_layout_updated.csv"


#extraer valores
label_index <- which(regexpr("Label:", caldata$Application..Tecan.i.control)>0)
data_index <- which(regexpr("<>", caldata$Application..Tecan.i.control)>0)

ind = 1
cal = data.frame(row.names = caldata[(data_index[ind]+1):(data_index[ind]+96),1])
for (ind in (1:length(data_index))) {
  new_col = caldata[(data_index[ind]+1):(data_index[ind]+96),2]
  col_name <- strsplit(caldata$Application..Tecan.i.control[label_index[ind]], ": ")[[1]][2]
  cal[,col_name] = new_col
  ind = ind + 1
}

new_col_names <- strsplit(colnames(cal), split = "(?<=[a-zA-Z])\\s*(?=[0-9])", perl = TRUE)

pasted_col_names <- c("OD600")
for (l in new_col_names) {
  print(l)
    if (l[1] != "OD" ) {
      pasted_col_names <- c(pasted_col_names, paste(l[1], l[2]))
    }
}

colnames(cal) <- pasted_col_names

#sustituir valores 'OVER' por 'NA'
cal[cal == "OVER"] <- NA


#añadir columna 'pocillo' (well)#
well_names <- do.call(paste, c(dplyr::arrange(expand.grid(c("A","B","C","D","E","F","G","H"),
          (1:12)), Var1),list(sep="")))

cal$well <- well_names


#unir datos con archivo layout#
data_full <- combine_info_file(cal,
                               .info_file = callayout_path,
                               .common_column = 'well')

#ordenar columnas
data <- data_full[, c(12, 13, 14, 15, 16, 11,1,2,3,4,5,6,7,8,9,10)]

#añadir columnas de 'fila' (row) y 'columna' (column)#
data$row <-substr(data$well,1,1)
data$column <- substr(data$well, 2,3)

#guardar datos ya procesados#
write.csv(data,"calibration_membrane_parsed.csv", row.names = FALSE, na = '')

#generación de coeficientes de calibrado#
flopr::generate_cfs(calibration_csv = "calibration_membrane_parsed.csv")
