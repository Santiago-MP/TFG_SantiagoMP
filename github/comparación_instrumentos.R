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

#cargar los datos de microscopía#
setwd("/home/ada/ymartinez/Experiments/results/final_results")

file_micro <- 'microscope_results.csv'
data_full_micro <- read_csv('microscope_results.csv')

#separar los datos por condición y crear la columna de ciclos de replicación con el valor de tau específico para la condición#
data_C1_micro = data_full_micro[(data_full_micro$Condition == 'C1'), ]
data_C1_micro = data_C1_micro %>% mutate(time = POSITION.frame*3, rep_cycles = ((POSITION.frame*3)/31.5), UA = INTENSITY.ch2.mean)
data_C2_micro = data_full_micro[(data_full_micro$Condition == 'C2'), ]
data_C2_micro = data_C2_micro %>% mutate(time = POSITION.frame*3, rep_cycles = ((POSITION.frame*3)/39.2), UA = INTENSITY.ch2.mean)
data_C3_micro = data_full_micro[(data_full_micro$Condition == 'C3'), ]
data_C3_micro = data_C3_micro %>% mutate(time = POSITION.frame*3, rep_cycles = ((POSITION.frame*3)/29.3), UA = INTENSITY.ch2.mean)
data_C4_micro = data_full_micro[(data_full_micro$Condition == 'C4'), ]
data_C4_micro = data_C4_micro %>% mutate(time = POSITION.frame*3, rep_cycles = ((POSITION.frame*3)/46.6), UA = INTENSITY.ch2.mean)

#obtener la media de los valores de fluorescencia de las réplicas de cada condición#
data_avg_C1_micro <- data_C1_micro %>%
  group_by(rep_cycles) %>%
  summarise(time, avg_UA = mean(UA, na.rm = T),
            sd_UA = sd(UA, na.rm = T), Condition)
data_avg_C2_micro <- data_C2_micro %>%
  group_by(rep_cycles) %>%
  summarise(time, avg_UA = mean(UA, na.rm = T),
            sd_UA = sd(UA, na.rm = T), Condition)
data_avg_C3_micro <- data_C3_micro %>%
  group_by(rep_cycles) %>%
  summarise(time, avg_UA = mean(UA, na.rm = T),
            sd_UA = sd(UA, na.rm = T), Condition)
data_avg_C4_micro <- data_C4_micro %>%
  group_by(rep_cycles) %>%
  summarise(time, avg_UA = mean(UA, na.rm = T),
            sd_UA = sd(UA, na.rm = T), Condition)

#crear el archivo.csv con los datos de microscopía para la comparación#
data_avg_full_micro = rbind(data_avg_C1_micro, data_avg_C2_micro, data_avg_C3_micro, data_avg_C4_micro)
data_avg_full_micro <- distinct(data_avg_full_micro)
data_final_micro <- data_avg_full_micro %>% group_by(Condition) %>% summarise(time, rep_cycles, avg_UA, sd_UA)
write.csv(data_final_micro, 'UA_final_data.csv')

#una vez hechos los pasos de arriba, podemos cargar el nuevo archivo en lugar de repetirlos cada vez#
file_micro <- 'UA_final_data_.csv'
data_final_micro <- read.csv(file_pr)

data_final_micro_C1 <- subset(data_final_micro, data_final_micro$Condition == 'C1')
data_final_micro_C2 <- subset(data_final_micro, data_final_micro$Condition == 'C2')
data_final_micro_C3 <- subset(data_final_micro, data_final_micro$Condition == 'C3')
data_final_micro_C4 <- subset(data_final_micro, data_final_micro$Condition == 'C4')

#representación de los plots de microscopía, todos juntos o separados por condición#
micro_plot <- ggplot(data = data_final_micro) +
  geom_ribbon(aes(x = rep_cycles, y = avg_UA, ymin=avg_UA-sd_UA, ymax=avg_UA+sd_UA, fill = Condition))+
  geom_line(aes(x = rep_cycles, y = avg_UA, color = Condition))+
  scale_color_manual(values=met.brewer("OKeeffe1", 4))+
  scale_fill_manual(values=met.brewer("Renoir", 4))+
  xlim(1,8)+
  ylim(0,6700)+
  labs(title='microscope data')

micro_plotC1 <- ggplot(data = data_final_micro_C1) +
  geom_ribbon(aes(x = rep_cycles, y = avg_UA, ymin=avg_UA-sd_UA, ymax=avg_UA+sd_UA, fill = Condition))+
  geom_line(aes(x = rep_cycles, y = avg_UA, color = Condition))+
  scale_color_manual(values=met.brewer("OKeeffe1", 4))+
  scale_fill_manual(values=met.brewer("Renoir", 4))+
  xlim(1,8)+
  ylim(0,6700)+
  labs(title='microscope data')

micro_plotC2 <- ggplot(data = data_final_micro_C2) +
  geom_ribbon(aes(x = rep_cycles, y = avg_UA, ymin=avg_UA-sd_UA, ymax=avg_UA+sd_UA, fill = Condition))+
  geom_line(aes(x = rep_cycles, y = avg_UA, color = Condition))+
  scale_color_manual(values=met.brewer("OKeeffe1", 4))+
  scale_fill_manual(values=met.brewer("Renoir", 4))+
  xlim(1,8)+
  ylim(0,6700)+
  labs(title='microscope data')

micro_plotC3 <- ggplot(data = data_final_micro_C3) +
  geom_ribbon(aes(x = rep_cycles, y = avg_UA, ymin=avg_UA-sd_UA, ymax=avg_UA+sd_UA, fill = Condition))+
  geom_line(aes(x = rep_cycles, y = avg_UA, color = Condition))+
  scale_color_manual(values=met.brewer("OKeeffe1", 4))+
  scale_fill_manual(values=met.brewer("Renoir", 4))+
  xlim(1,8)+
  ylim(0,6700)+
  labs(title='microscope data')

micro_plotC4 <- ggplot(data = data_final_micro_C4) +
  geom_ribbon(aes(x = rep_cycles, y = avg_UA, ymin=avg_UA-sd_UA, ymax=avg_UA+sd_UA, fill = Condition))+
  geom_line(aes(x = rep_cycles, y = avg_UA, color = Condition))+
  scale_color_manual(values=met.brewer("OKeeffe1", 4))+
  scale_fill_manual(values=met.brewer("Renoir", 4))+
  xlim(2,8.5)+
  ylim(0,6700)+
  labs(title='microscope data C4')

#cargar los datos del lector de placas#
setwd('/home/ada/ymartinez/Plate_Reader/growth_study/results')

file1_pr <- 'resultsC1_pr.csv'
file2_pr <- 'resultsC2_pr.csv'
file3_pr <- 'resultsC3_pr.csv'
file4_pr <- 'resultsC4_pr.csv'

resultsC1_pr <- read.csv(file1_pr)
resultsC2_pr <- read.csv(file2_pr)
resultsC3_pr <- read.csv(file3_pr)
resultsC4_pr <- read.csv(file4_pr)

data_avg_full_pr = rbind(resultsC1_pr,resultsC2_pr,resultsC3_pr,resultsC4_pr)
data_avg_full_pr <- distinct(data_avg_full_pr) #con este paso eliminamos todos los valores duplicados que se generan al mediar
data_final_pr <- data_avg_full_pr %>% group_by(Condition) %>% summarise(time, rep_cycles, avg_MEFL, sd_MEFL = sd_fc)
write.csv(data_final_pr, 'MEFL_final_data_nocontrols.csv') #en este caso, a los archivos cargados ya se les ha quitado los controles experimentales

#una vez hechos los pasos de arriba, podemos cargar el nuevo archivo en lugar de repetirlos cada vez#
file_pr <- 'MEFL_final_data_nocontrols.csv'
data_final_pr <- read.csv(file_pr)

#separar los datos por condición#
data_final_pr_C1 <- subset(data_final_pr, data_final_pr$Condition == 'C1')
data_final_pr_C2 <- subset(data_final_pr, data_final_pr$Condition == 'C2')
data_final_pr_C3 <- subset(data_final_pr, data_final_pr$Condition == 'C3')
data_final_pr_C4 <- subset(data_final_pr, data_final_pr$Condition == 'C4')

#representación de los plots del lector de placas, todos juntos o separados por condición#
pr_plot <- ggplot(data = data_final_pr) +
  geom_ribbon(aes(x = rep_cycles, y = avg_MEFL, ymin=avg_MEFL-sd_MEFL, ymax=avg_MEFL+sd_MEFL, fill = Condition))+
  geom_line(aes(x = rep_cycles, y = avg_MEFL, color = Condition))+
  scale_color_manual(values=met.brewer("OKeeffe1", 4))+
  scale_fill_manual(values=met.brewer("Renoir", 4))+
  xlim(3,8)+
  ylim(-100,2700)+
  labs(title='plate reader data')

pr_plotC1 <- ggplot(data = data_final_pr_C1) +
  geom_ribbon(aes(x = rep_cycles, y = avg_MEFL, ymin=avg_MEFL-sd_MEFL, ymax=avg_MEFL+sd_MEFL, fill = Condition))+
  geom_line(aes(x = rep_cycles, y = avg_MEFL, color = Condition))+
  scale_color_manual(values=met.brewer("OKeeffe1", 4))+
  scale_fill_manual(values=met.brewer("Renoir", 4))+
  xlim(3,8)+
  ylim(-100,2700)+
  labs(title='plate reader data')

pr_plotC2 <- ggplot(data = data_final_pr_C2) +
  geom_ribbon(aes(x = rep_cycles, y = avg_MEFL, ymin=avg_MEFL-sd_MEFL, ymax=avg_MEFL+sd_MEFL, fill = Condition))+
  geom_line(aes(x = rep_cycles, y = avg_MEFL, color = Condition))+
  scale_color_manual(values=met.brewer("OKeeffe1", 4))+
  scale_fill_manual(values=met.brewer("Renoir", 4))+
  xlim(3,8)+
  ylim(-100,2700)+
  labs(title='plate reader data')

pr_plotC3 <- ggplot(data = data_final_pr_C3) +
  geom_ribbon(aes(x = rep_cycles, y = avg_MEFL, ymin=avg_MEFL-sd_MEFL, ymax=avg_MEFL+sd_MEFL, fill = Condition))+
  geom_line(aes(x = rep_cycles, y = avg_MEFL, color = Condition))+
  scale_color_manual(values=met.brewer("OKeeffe1", 4))+
  scale_fill_manual(values=met.brewer("Renoir", 4))+
  xlim(3,8)+
  ylim(-100,2700)+
  labs(title='plate reader data')

pr_plotC4 <- ggplot(data = data_final_pr_C4) +
  geom_ribbon(aes(x = rep_cycles, y = avg_MEFL, ymin=avg_MEFL-sd_MEFL, ymax=avg_MEFL+sd_MEFL, fill = Condition))+
  geom_line(aes(x = rep_cycles, y = avg_MEFL, color = Condition))+
  scale_color_manual(values=met.brewer("OKeeffe1", 1))+
  scale_fill_manual(values=met.brewer("Renoir", 1))+
  xlim(2,8.5)+
  ylim(-100,3000)+
  labs(title='plate reader data C4')



#de cara a establecer la comparación, acotar los rangos de ciclos de replicación comparables y crear un .csv a partir del cual analizar posibles correspondencias#
data_final_micro_C1 <- subset(data_final_micro_C1, data_final_micro_C1$rep_cycles > 1)
data_final_micro_C1 <- subset(data_final_micro_C1, data_final_micro_C1$rep_cycles < 8.5)
data_final_pr_C1 <- subset(data_final_pr_C1, data_final_pr_C1$rep_cycles > 2)
data_final_pr_C1 <- subset(data_final_pr_C1, data_final_pr_C1$rep_cycles < 8.5)

data_final_C1 <- rbind(data_final_micro_C1, data_final_pr_C1)
write.csv(data_final_C1, 'comparación_unidadesfluo_C1.csv')

data_final_micro_C2 <- subset(data_final_micro_C2, data_final_micro_C2$rep_cycles > 1)
data_final_micro_C2 <- subset(data_final_micro_C2, data_final_micro_C2$rep_cycles < 8.5)
data_final_pr_C2 <- subset(data_final_pr_C2, data_final_pr_C2$rep_cycles > 2)
data_final_pr_C2 <- subset(data_final_pr_C2, data_final_pr_C2$rep_cycles < 8.5)

data_final_C2 <- rbind(data_final_micro_C2, data_final_pr_C2)
write.csv(data_final_C2, 'comparación_unidadesfluo_C2.csv')

data_final_micro_C3 <- subset(data_final_micro_C3, data_final_micro_C3$rep_cycles > 1)
data_final_micro_C3 <- subset(data_final_micro_C3, data_final_micro_C3$rep_cycles < 8.5)
data_final_pr_C3 <- subset(data_final_pr_C3, data_final_pr_C3$rep_cycles > 2)
data_final_pr_C3 <- subset(data_final_pr_C3, data_final_pr_C3$rep_cycles < 8.5)

data_final_C3 <- rbind(data_final_micro_C3, data_final_pr_C3)
write.csv(data_final_C3, 'comparación_unidadesfluo_C3.csv')


data_final_micro_C4 <- subset(data_final_micro_C4, data_final_micro_C4$rep_cycles > 1)
data_final_micro_C4 <- subset(data_final_micro_C4, data_final_micro_C4$rep_cycles < 8.5)
data_final_pr_C4 <- subset(data_final_pr_C4, data_final_pr_C4$rep_cycles > 2)
data_final_pr_C4 <- subset(data_final_pr_C4, data_final_pr_C4$rep_cycles < 8.5)

data_final_C4 <- rbind(data_final_micro_C4, data_final_pr_C4)
write.csv(data_final_C4, 'comparación_unidadesfluo_C4.csv')
