library(readr)
library(tidyverse)
library(tidyr)
library(broom)
library(scales)
library(rgl)
library(cowplot)
library(RColorBrewer)
library(patchwork)
library(MetBrewer)
library(ggplot2)

#unir todos los archivos de valores .csv en uno solo#
#este paso solo necesita realizarse una vez#

setwd('/home/ada/ymartinez/Plate_Reader/growth_study/results')

file1 <- 'C1_results.csv'
file2 <- 'C2_results.csv'
file3 <- 'C3_results.csv'
file4 <- 'C4_results.csv'

#mediante la función mutate() añadimos una columna que indique a qué condición corresponde cada grupo de datos
data1 <- read_csv(file1)
data1 <- data1 %>% mutate(Condition = 'C1')
data2 <- read_csv(file2)
data2 <- data2 %>% mutate(Condition = 'C2')
data3 <- read_csv(file3)
data3 <- data3 %>% mutate(Condition = 'C3')
data4 <- read_csv(file4)
data4 <- data4 %>% mutate(Condition = 'C4')

data_full = rbind(data1,data2,data3,data4)

write.csv(data_full, 'plate_reader_results.csv', row.names = FALSE, na = '')

#los pasos anteriores solo necesitan realizarse una vez#
#para usos futuros, empezar aquí#

setwd('/home/ada/ymartinez/Plate_Reader/growth_study/results')

file_path <- 'plate_reader_results.csv'
data_full <- read.csv('plate_reader_results.csv')

#separar los datos por condición y eliminar los pocillos correspondientes a controles experimentales#
data_C1 = data_full[(data_full$Condition == 'C1'), ]
data_C1 = data_C1[!(data_C1$strain == 'blank'), ]
data_C1 = data_C1[!(data_C1$strain == 'fluorescence_neg_control'), ]

data_C2 = data_full[(data_full$Condition == 'C2'), ]
data_C2 = data_C2[!(data_C2$strain == 'blank'), ]
data_C2 = data_C2[!(data_C2$strain == 'fluorescence_neg_control'), ]

data_C3 = data_full[(data_full$Condition == 'C3'), ]
data_C3 = data_C3[!(data_C3$strain == 'blank'), ]
data_C3 = data_C3[!(data_C3$strain == 'fluorescence_neg_control'), ]

data_C4 = data_full[(data_full$Condition == 'C4'), ]
data_C4 = data_C4[!(data_C4$strain == 'blank'), ]
data_C4 = data_C4[!(data_C4$strain == 'fluorescence_neg_control'), ]

#agrupar los datos por condición con las variables de interés y generar los archivos .csv con dicha información#
resultsC1 <- data_C1 %>% 
  group_by(time = time/60) %>%
  summarise(avg_fc_C1 = mean(fc, na.rm = T), sd_fc_C1 = sd(fc, na.rm = T), avg_nOD = mean(normalised_OD, na.rm = T), 
            sd_nOD = sd(normalised_OD, na.rm = T), avg_nGFP = mean(normalised_GFP, na.rm = T), 
            sd_nGFP = sd(normalised_GFP, na.rm = T), avg_cOD = mean(calibrated_OD, na.rm = T),
            sd_cOD = sd(calibrated_OD, na.rm = T), avg_cGFP = mean(calibrated_GFP, na.rm = T),
            sd_cGFP = sd(calibrated_GFP, na.rm = T), Condition)
write.csv(resultsC1, 'resultsC1_pr.csv', row.names = FALSE, na = '')

resultsC2 <- data_C2 %>% 
  group_by(time = time/60) %>%
  summarise(avg_fc_C2 = mean(fc, na.rm = T), sd_fc_C2 = sd(fc, na.rm = T), avg_nOD = mean(normalised_OD, na.rm = T), 
            sd_nOD = sd(normalised_OD, na.rm = T), avg_nGFP = mean(normalised_GFP, na.rm = T), 
            sd_nGFP = sd(normalised_GFP, na.rm = T), avg_cOD = mean(calibrated_OD, na.rm = T),
            sd_cOD = sd(calibrated_OD, na.rm = T), avg_cGFP = mean(calibrated_GFP, na.rm = T),
            sd_cGFP = sd(calibrated_GFP, na.rm = T), Condition)
write.csv(resultsC2, 'resultsC2_pr.csv', row.names = FALSE, na = '')

resultsC3 <- data_C3 %>% 
  group_by(time = time/60) %>%
  summarise(avg_fc_C3 = mean(fc, na.rm = T), sd_fc_C3 = sd(fc, na.rm = T), avg_nOD = mean(normalised_OD, na.rm = T), 
            sd_nOD = sd(normalised_OD, na.rm = T), avg_nGFP = mean(normalised_GFP, na.rm = T), 
            sd_nGFP = sd(normalised_GFP, na.rm = T), avg_cOD = mean(calibrated_OD, na.rm = T),
            sd_cOD = sd(calibrated_OD, na.rm = T), avg_cGFP = mean(calibrated_GFP, na.rm = T),
            sd_cGFP = sd(calibrated_GFP, na.rm = T), Condition)
write.csv(resultsC3, 'resultsC3_pr.csv', row.names = FALSE, na = '')

resultsC4 <- data_C4 %>% 
  group_by(time = time/60) %>%
  summarise(avg_fc_C4 = mean(fc, na.rm = T), sd_fc_C4 = sd(fc, na.rm = T), avg_nOD = mean(normalised_OD, na.rm = T), 
            sd_nOD = sd(normalised_OD, na.rm = T), avg_nGFP = mean(normalised_GFP, na.rm = T), 
            sd_nGFP = sd(normalised_GFP, na.rm = T), avg_cOD = mean(calibrated_OD, na.rm = T),
            sd_cOD = sd(calibrated_OD, na.rm = T), avg_cGFP = mean(calibrated_GFP, na.rm = T),
            sd_cGFP = sd(calibrated_GFP, na.rm = T), Condition)
write.csv(resultsC4, 'resultsC4_pr.csv', row.names = FALSE, na = '')

#todos los datos en un gráfico#
#el tiempo se divide por los valores de tiempo de replicación (tau) específicos para cada condición#
#para obtener los valores de tau, usar la pendiente de la curva de crecimiento celular (tau=1/pendiente) de cada condición#
ggplot() +
  geom_ribbon(data = resultsC1, aes(x = ((time)/35.33), y = avg_MEFL, ymin=avg_MEFL-sd_fc, ymax=avg_MEFL+sd_fc),
              position=position_dodge(0.05),fill="lightpink")+
  geom_line(data = resultsC1, aes(x = ((time)/35.33), y = avg_MEFL), color = 'red', size=1) +
  geom_ribbon(data = resultsC2, aes(x = ((time)/39.36), y = avg_MEFL, ymin=avg_MEFL-sd_fc, ymax=avg_MEFL+sd_fc),
              position=position_dodge(0.05),fill="#90548BFF")+
  geom_line(data = resultsC2, aes(x = ((time)/39.36), y = avg_MEFL), color = 'orange', size=1) +
  geom_ribbon(data = resultsC3, aes(x = ((time)/38.25), y = avg_MEFL, ymin=avg_MEFL-sd_fc, ymax=avg_MEFL+sd_fc),
              position=position_dodge(0.05),fill="#F9B641FF", alpha = 0.8)+
  geom_line(data = resultsC3, aes(x = ((time)/38.25), y = avg_MEFL), color = '#619CFF', size=1, alpha = 0.8) +
  geom_ribbon(data = resultsC4, aes(x = ((time)/43.64), y = avg_MEFL, ymin=avg_MEFL-sd_fc, ymax=avg_MEFL+sd_fc),
              position=position_dodge(0.05),fill="black", alpha = 0.7)+
  geom_line(data = resultsC4, aes(x = ((time)/43.64), y = avg_MEFL), color = 'white', size=1, alpha = 0.7) +
  #scale_color_manual(values=met.brewer("OKeeffe1", 4))+
  #theme(legend.position = 'right') +
  ylab('fluorescence/cell') +
  xlab('replication cycles')+
  labs(title='plate reader data')+
  xlim(3.5,27) +
  ylim(-100,4700)


#solo C" y C3 en un gráfico#
ggplot() +
  geom_ribbon(data = resultsC2, aes(x = ((time)/39.36), y = avg_fc_C2, ymin=avg_fc_C2-sd_fc_C2, ymax=avg_fc_C2+sd_fc_C2),
              position=position_dodge(0.05),fill="#90548BFF")+
  geom_line(data = resultsC2, aes(x = ((time)/39.36), y = avg_fc_C2, color=Condition), size=1) +
  geom_ribbon(data = resultsC3, aes(x = ((time)/38.25), y = avg_fc_C3, ymin=avg_fc_C3-sd_fc_C3, ymax=avg_fc_C3+sd_fc_C3),
              position=position_dodge(0.05),fill="#F9B641FF", alpha = 0.75)+
  geom_line(data = resultsC3, aes(x = ((time)/38.25), y = avg_fc_C3, color=Condition), size=1, alpha = 0.75) +
  scale_color_manual(values=met.brewer("OKeeffe1", 2))+
  theme(legend.position = 'right') +
  ylim(0,1200)+
  xlim(4.35,8.05)+
  labs(title= 'Plate Reader curves', x= "replication cycles", y="fluorescence/cell")

