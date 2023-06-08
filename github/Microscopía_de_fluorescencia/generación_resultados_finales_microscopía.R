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

#unir todos los archivos .csv de resultados en uno solo#
#este paso solo será necesario realizarlo una vez

setwd("/home/ada/ymartinez/Experiments/results/final_results")

file1 <- 'C1_results.csv'
file2 <- 'C2_results.csv'
file3 <- 'C3_results.csv'
file4 <- 'C4_results.csv'

data1 <- read_csv(file1)
data2 <- read_csv(file2)
data3 <- read_csv(file3)
data4 <- read_csv(file4)

data_full = rbind(data1,data2,data3,data4)
write.csv(data_full, 'microscope_results.csv', row.names = FALSE, na = '')

#los pasos mostrados arriba solo necesitarán realizarse una vez#
#para futuros usos, empezar a partir de aquí#

setwd("/home/ada/ymartinez/Experiments/results/final_results")

data_full <- read_csv('microscope_results.csv')


data_C1 = data_full[(data_full$Condition == 'C1'), ]
data_C2 = data_full[(data_full$Condition == 'C2'), ]
data_C3 = data_full[(data_full$Condition == 'C3'), ]
data_C4 = data_full[(data_full$Condition == 'C4'), ]

data_avg_C1 <- data_C1 %>%
  group_by(POSITION.frame) %>%
  summarise(INTENSITY.ch2.mean, avg_fluo_C1 = mean(INTENSITY.ch2.mean, na.rm = T),
            sd_fluo_C1 = sd(INTENSITY.ch2.mean, na.rm = T), Condition, Experiment)
data_avg_C2 <- data_C2 %>%
  group_by(POSITION.frame) %>%
  summarise(INTENSITY.ch2.mean, avg_fluo_C2 = mean(INTENSITY.ch2.mean, na.rm = T),
            sd_fluo_C2 = sd(INTENSITY.ch2.mean, na.rm = T), Condition, Experiment)
data_avg_C3 <- data_C3 %>%
  group_by(POSITION.frame) %>%
  summarise(INTENSITY.ch2.mean, avg_fluo_C3 = mean(INTENSITY.ch2.mean, na.rm = T),
            sd_fluo_C3 = sd(INTENSITY.ch2.mean, na.rm = T), Condition, Experiment)
data_avg_C4 <- data_C4 %>%
  group_by(POSITION.frame) %>%
  summarise(INTENSITY.ch2.mean, avg_fluo_C4 = mean(INTENSITY.ch2.mean, na.rm = T),
            sd_fluo_C4 = sd(INTENSITY.ch2.mean, na.rm = T), Condition, Experiment)

data_avg_full = rbind(data_avg_C1, data_avg_C2, data_avg_C3, data_avg_C4)

#todos los datos en un gráfico#
ggplot() +
  geom_ribbon(data = data_avg_C1, aes(x = (((POSITION.frame*3)/1)/33.35), y = avg_fluo_C1, ymin=avg_fluo_C1-sd_fluo_C1, ymax=avg_fluo_C1+sd_fluo_C1),
              position=position_dodge(0.05),fill="lightpink")+
  geom_line(data = data_avg_C1, aes(x = (((POSITION.frame*3)/1)/33.35), y = avg_fluo_C1), color='red', size=1) +
  geom_ribbon(data = data_avg_C2, aes(x = (((POSITION.frame*3)/1)/40.47), y = avg_fluo_C2, ymin=avg_fluo_C2-sd_fluo_C2, ymax=avg_fluo_C2+sd_fluo_C2),
              position=position_dodge(0.05),fill="#90548BFF")+
  geom_line(data = data_avg_C2, aes(x = (((POSITION.frame*3)/1)/40.47), y = avg_fluo_C2), color='orange', size=1) +
  geom_ribbon(data = data_avg_C3, aes(x = (((POSITION.frame*3)/1)/30.46), y = avg_fluo_C3, ymin=avg_fluo_C3-sd_fluo_C3, ymax=avg_fluo_C3+sd_fluo_C3),
              position=position_dodge(0.05),fill="#F9B641FF", alpha = 0.75)+
  geom_line(data = data_avg_C3, aes(x = (((POSITION.frame*3)/1)/30.46), y = avg_fluo_C3), color='#619CFF', size=1, alpha = 0.75) +
  geom_ribbon(data = data_avg_C4, aes(x = (((POSITION.frame*3)/1)/46.99), y = avg_fluo_C4, ymin=avg_fluo_C4-sd_fluo_C4, ymax=avg_fluo_C4+sd_fluo_C4),
              position=position_dodge(0.05),fill="black")+
  geom_line(data = data_avg_C4, aes(x = (((POSITION.frame*3)/1)/46.99), y = avg_fluo_C4), color='white', size=1) +
  #scale_color_manual(values=met.brewer("OKeeffe1", 4))+
  #theme(legend.position = 'right') +
  ylab('fluorescence/cell') +
  xlab('replication cycles')+
  labs(title='microscopy data')+
  xlim(0,8) +
  ylim(0,6500)

#C2 and C3 en un sólo gráfico#
ggplot() +
  geom_ribbon(data = data_avg_C2, aes(x = (((POSITION.frame*3)/1)/40.47), y = avg_fluo_C2, ymin=avg_fluo_C2-sd_fluo_C2, ymax=avg_fluo_C2+sd_fluo_C2),
             position=position_dodge(0.05),fill="#90548BFF")+
  geom_line(data = data_avg_C2, aes(x = (((POSITION.frame*3)/1)/40.47), y = avg_fluo_C2), color="white", size=1) +
  geom_ribbon(data = data_avg_C3, aes(x = (((POSITION.frame*3)/1)/30.46), y = avg_fluo_C3, ymin=avg_fluo_C3-sd_fluo_C3, ymax=avg_fluo_C3+sd_fluo_C3),
              position=position_dodge(0.05),fill="#F9B641FF", alpha = 0.75)+
  geom_line(data = data_avg_C3, aes(x = (((POSITION.frame*3)/1)/30.46), y = avg_fluo_C3), color="black", size=1, alpha = 0.75) +
  theme(legend.position = 'none') +
  ylab('fluorescence/cell') +
  xlab('replication cycles')+
  labs(title='microscopy data')+
  xlim(0,8.5)+
  ylim(0,2200)

#para evaluar las pendientes de ambas condiciones, acotar a los ciclos de replicación donde se observe un comportamiento lineal#
data_avg_C2 <- subset(data_avg_C2, data_avg_C2$POSITION.frame*3/40.47 < 4.5)
data_avg_C2 <- subset(data_avg_C2, data_avg_C2$POSITION.frame*3/40.47 > 1)
data_avg_C3 <- subset(data_avg_C3, data_avg_C3$POSITION.frame*3/30.46 < 4.5)
data_avg_C3 <- subset(data_avg_C3, data_avg_C3$POSITION.frame*3/30.46 > 1)

ggplot(data) +
  geom_ribbon(data = data_avg_C2, aes(x = (((POSITION.frame*3)/1)/40.47), y = avg_fluo_C2, ymin=avg_fluo_C2-sd_fluo_C2, 
                                      ymax=avg_fluo_C2+sd_fluo_C2),
              position=position_dodge(0.05),fill="#90548BFF")+
  geom_line(data = data_avg_C2, aes(x = (((POSITION.frame*3)/1)/40.47), y = avg_fluo_C2, color = Condition), size=1) +
  geom_ribbon(data = data_avg_C3, aes(x = (((POSITION.frame*3)/1)/30.46), y = avg_fluo_C3, ymin=avg_fluo_C3-sd_fluo_C3, ymax=avg_fluo_C3+sd_fluo_C3),
              position=position_dodge(0.05),fill="#F9B641FF", alpha = 0.75)+
  geom_line(data = data_avg_C3, aes(x = (((POSITION.frame*3)/1)/30.46), y = avg_fluo_C3, color = Condition), size=1, alpha = 0.75) +
  theme(legend.position = 'right') +
  ylab('fluorescence/cell') +
  xlab('replication cycles')+
  labs(title='C2/C3 microscope slope comparison')+
  scale_color_manual(values=met.brewer("OKeeffe1", 2))+
  xlim(1,8.5)+
  ylim(0,6500)

data_avg_C2 <- mutate(data_avg_C2,rep_cycle=POSITION.frame*3/40.47)
data_avg_C3 <- mutate(data_avg_C3,rep_cycle=POSITION.frame*3/30.46)

ggplot()+
  geom_point(data=data_avg_C2, aes(x=rep_cycle, y=avg_fluo_C2), color='black')+
  geom_point(data=data_avg_C3, aes(x=rep_cycle, y=avg_fluo_C3),color='violet')+
  theme(legend.position = 'none') +
  ylab('fluorescence/cell') +
  xlab('replication cycles')+
  labs(title='C2/C3 microscope slope comparison')
  
#mediante la función lm (linear model), obtenemos las ecuaciones de ambas rectas de regresión#
#este paso también puede realizarse en Microsoft Excel#
lm(formula = avg_fluo_C2~rep_cycle, data=data_avg_C2)
lm(formula = avg_fluo_C3~rep_cycle, data=data_avg_C3)

