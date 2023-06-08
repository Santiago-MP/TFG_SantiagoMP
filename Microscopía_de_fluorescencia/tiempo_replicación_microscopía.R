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

#cargar los archivos 'trajectory.csv' para cada una de las réplicas de cada condición, usando la función mutate() para crear la columna 'Dif', en la que se obtiene el tiempo de vida de cada célula individual hasta su replicación, y columnas con identificadores de réplica y condición#
setwd('/home/ada/ymartinez/Experiments/results/trajectories/')

file1 <- 'trajectoryC1P4.csv'
file2 <- 'trajectoryC1P9.csv'
file3 <- 'trajectoryC1P10.csv'
file4 <- 'trajectoryC1P11.csv'
file5 <- 'trajectoryC1P12.csv'

data1 <- read_csv(file1)
data1 <- data1 %>% mutate(Dif = (data1$FRAME.end-data1$FRAME.start)*3, Condition = 'C1', Experiment = 'C1P4')
data2 <- read_csv(file2)
data2 <- data2 %>% mutate(Dif = (data2$FRAME.end-data2$FRAME.start)*3,Condition = 'C1', Experiment = 'C1P8')
data3 <- read_csv(file3)
data3 <- data3 %>% mutate(Dif = (data3$FRAME.end-data3$FRAME.start)*3,Condition = 'C1', Experiment = 'C1P10')
data4 <- read_csv(file4)
data4 <- data4 %>% mutate(Dif = (data4$FRAME.end-data4$FRAME.start)*3,Condition = 'C1', Experiment = 'C1P11')
data5 <- read_csv(file5)
data5 <- data5 %>% mutate(Dif = (data5$FRAME.end-data5$FRAME.start)*3,Condition = 'C1', Experiment = 'C1P12')

dataC1 = bind_rows(data1,data2,data3,data4,data5)

file1 <- 'trajectoryC2P1.csv'
file2 <- 'trajectoryC2P3.csv'
file3 <- 'trajectoryC2P5.csv'
file4 <- 'trajectoryC2P7.csv'
file5 <- 'trajectoryC2P9.csv'

data1 <- read_csv(file1)
data1 <- data1 %>% mutate(Dif = (data1$FRAME.end-data1$FRAME.start)*3, Condition = 'C2', Experiment = 'C2P1')
data2 <- read_csv(file2)
data2 <- data2 %>% mutate(Dif = (data2$FRAME.end-data2$FRAME.start)*3,Condition = 'C2', Experiment = 'C2P3')
data3 <- read_csv(file3)
data3 <- data3 %>% mutate(Dif = (data3$FRAME.end-data3$FRAME.start)*3,Condition = 'C2', Experiment = 'C2P5')
data4 <- read_csv(file4)
data4 <- data4 %>% mutate(Dif = (data4$FRAME.end-data4$FRAME.start)*3,Condition = 'C2', Experiment = 'C2P7')
data5 <- read_csv(file5)
data5 <- data5 %>% mutate(Dif = (data5$FRAME.end-data5$FRAME.start)*3,Condition = 'C2', Experiment = 'C2P9')

dataC2 = rbind(data1,data2,data3,data4,data5)

file1 <- 'trajectoryC3P27.csv'
file2 <- 'trajectoryC3P35.csv'
file3 <- 'trajectoryC3P39.csv'
file4 <- 'trajectoryC3P41.csv'
file5 <- 'trajectoryC3P43.csv'

data1 <- read_csv(file1)
data1 <- data1 %>% mutate(Dif = (data1$FRAME.end-data1$FRAME.start)*3, Condition = 'C3', Experiment = 'C3P27')
data2 <- read_csv(file2)
data2 <- data2 %>% mutate(Dif = (data2$FRAME.end-data2$FRAME.start)*3,Condition = 'C3', Experiment = 'C3P35')
data3 <- read_csv(file3)
data3 <- data3 %>% mutate(Dif = (data3$FRAME.end-data3$FRAME.start)*3,Condition = 'C3', Experiment = 'C3P39')
data4 <- read_csv(file4)
data4 <- data4 %>% mutate(Dif = (data4$FRAME.end-data4$FRAME.start)*3,Condition = 'C3', Experiment = 'C3P41')
data5 <- read_csv(file5)
data5 <- data5 %>% mutate(Dif = (data5$FRAME.end-data5$FRAME.start)*3,Condition = 'C3', Experiment = 'C3P43')

dataC3 = rbind(data1,data2,data3,data4,data5)

file1 <- 'trajectoryC4P3.csv'
file2 <- 'trajectoryC4P4.csv'
file3 <- 'trajectoryC4P6.csv'
file4 <- 'trajectoryC4P7.csv'
file5 <- 'trajectoryC4P8.csv'

data1 <- read_csv(file1)
data1 <- data1 %>% mutate(Dif = (data1$FRAME.end-data1$FRAME.start)*3, Condition = 'C4', Experiment = 'C4P3')
data2 <- read_csv(file2)
data2 <- data2 %>% mutate(Dif = (data2$FRAME.end-data2$FRAME.start)*3,Condition = 'C4', Experiment = 'C4P4')
data3 <- read_csv(file3)
data3 <- data3 %>% mutate(Dif = (data3$FRAME.end-data3$FRAME.start)*3,Condition = 'C4', Experiment = 'C4P6')
data4 <- read_csv(file4)
data4 <- data4 %>% mutate(Dif = (data4$FRAME.end-data4$FRAME.start)*3,Condition = 'C4', Experiment = 'C4P7')
data5 <- read_csv(file5)
data5 <- data5 %>% mutate(Dif = (data5$FRAME.end-data5$FRAME.start)*3,Condition = 'C4', Experiment = 'C4P8')

dataC4 = rbind(data1,data2,data3,data4,data5)

#unir todos los datos en una sola matriz#
duplication_time_data = bind_rows(dataC1,dataC2,dataC3,dataC4)
write.csv(duplication_time_data, 'microscopy_duplication_time_data.csv')

duplication_data_file <- 'microscopy_duplication_time_data.csv'
duplication_time_data <- read.csv(duplication_data_file)

data_C1 = duplication_time_data[(duplication_time_data$Condition == 'C1'), ]
data_C2 = duplication_time_data[(duplication_time_data$Condition == 'C2'), ]
data_C3 = duplication_time_data[(duplication_time_data$Condition == 'C3'), ]
data_C4 = duplication_time_data[(duplication_time_data$Condition == 'C4'), ]

#obtener el tiempo de replicación medio para cada condición#
replication_times <- duplication_time_data %>%
  summarise(C1 = mean(data_C1$Dif), C2 = mean(data_C2$Dif), C3 = mean(data_C3$Dif), C4 = mean(data_C4$Dif))

#diagramas de violín#
#para cada condición en un sólo gráfico#
ggplot()+
  geom_violin(data = duplication_time_data, aes(x = Condition, y = Dif, fill = Condition))+
  labs(title = '', x = '', y = 'Replication time (minutes)')+
  scale_fill_manual(values=met.brewer("Tam", 4))+
  scale_y_continuous(n.breaks = 10)

#en la misma figura:
#todas las réplicas de cada condición
g <- ggplot()+
  geom_violin(data = data_C1, aes(x = Experiment, y = Dif, fill = Condition),
              draw_quantiles = c(0.25, 0.5, 0.75))+
  labs(title = '', x = '', y = 'Replication time (minutes)')+
  scale_fill_manual(values=met.brewer("Manet", 1))+
  #scale_y_continuous(n.breaks = 10)+
  stat_summary(data = data_C1, aes(x = Experiment, y = Dif),fun = "mean",
               geom = "point",
               color = "black")+ ylim(0,325)

g <- g + ggplot()+
  geom_violin(data = data_C2, aes(x = Experiment, y = Dif, fill = Condition),
              draw_quantiles = c(0.25, 0.5, 0.75))+
  labs(title = '', x = '', y = 'Replication time (minutes)')+
  scale_fill_manual(values=met.brewer("Renoir", 1))+
  #scale_y_continuous(n.breaks = 10)+
  stat_summary(data = data_C2, aes(x = Experiment, y = Dif),fun = "mean",
               geom = "point",
               color = "black")+ ylim(0,325)

g <- g + ggplot()+
  geom_violin(data = data_C3, aes(x = Experiment, y = Dif, fill = Condition), 
              draw_quantiles = c(0.25, 0.5, 0.75))+
  labs(title = '', x = '', y = 'Replication time (minutes)')+
  scale_fill_manual(values=met.brewer("Morgenstern", 5))+
  #scale_y_continuous(n.breaks = 10)+
  stat_summary(data = data_C3, aes(x = Experiment, y = Dif),fun = "mean",
               geom = "point",
               color = "black")+ ylim(0,325)

g <- g + ggplot()+
  geom_violin(data = data_C4, aes(x = Experiment, y = Dif, fill = Condition), 
              draw_quantiles = c(0.25, 0.5, 0.75))+
  labs(title = '', x = '', y = 'Replication time (minutes)')+
  scale_fill_manual(values=met.brewer("VanGogh1", 1))+
  #scale_y_continuous(n.breaks = 10)+
  stat_summary(data = data_C4, aes(x = Experiment, y = Dif),fun = "mean",
               geom = "point",
               color = "black")+ ylim(0,325)

g 


#media de cada condición
p <- ggplot()+
  geom_violin(data = data_C1, aes(x = Condition, y = Dif, fill = Condition),
              draw_quantiles = c(0.25, 0.5, 0.75))+
  labs(caption = 'meanC1 = 31.5 mins', x = '', y = 'Replication time (minutes)')+
  scale_fill_manual(values=met.brewer("Manet", 1))+
  #scale_y_continuous(n.breaks = 40)+
  stat_summary(data = data_C1, aes(x = Condition, y = Dif),fun = "mean",
               geom = "point",
               color = "black")+ ylim(0,325)

p <- p + ggplot()+
  geom_violin(data = data_C2, aes(x = Condition, y = Dif, fill = Condition),
              draw_quantiles = c(0.25, 0.5, 0.75))+
  labs(caption = 'meanC2 = 39.2 mins', x = '', y = 'Replication time (minutes)')+
  scale_fill_manual(values=met.brewer("Renoir", 1))+
  #scale_y_continuous(n.breaks = 40)+
  stat_summary(data = data_C2, aes(x = Condition, y = Dif),fun = "mean",
               geom = "point",
               color = "black")+ ylim(0,325)

p <- p + ggplot()+
  geom_violin(data = data_C3, aes(x = Condition, y = Dif, fill = Condition), 
              draw_quantiles = c(0.25, 0.5, 0.75))+
  labs(caption = 'meanC3 = 29.4 mins', x = '', y = 'Replication time (minutes)')+
  scale_fill_manual(values=met.brewer("Morgenstern", 5))+
  #scale_y_continuous(n.breaks = 40)+
  stat_summary(data = data_C3, aes(x = Condition, y = Dif),fun = "mean",
               geom = "point",
               color = "black")+ ylim(0,325)

p <- p + ggplot()+
  geom_violin(data = data_C4, aes(x = Condition, y = Dif, fill = Condition), 
              draw_quantiles = c(0.25, 0.5, 0.75), trim = F)+
  labs(caption = 'meanC4 = 46.6 mins', x = '', y = 'Replication time (minutes)')+
  scale_fill_manual(values=met.brewer("VanGogh1", 1))+
  #scale_y_continuous(n.breaks = 40)+
  stat_summary(data = data_C4, aes(x = Condition, y = Dif),fun = "mean",
               geom = "point",
               color = "black")+ ylim(0,325)

p

