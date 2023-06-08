library(readr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(broom)
library(scales)
library(rgl)
library(cowplot)
library(RColorBrewer)
library(patchwork)
library(MetBrewer)
library(viridis)

#los archivos cargados a continuación se obtuvieron por medio de Microsoft Excel a partir de la función plot_Ncells() del paquete VisCAR, en el archivo 'árbolesdelinaje_disposiciónespacial_célulasporframe.R'#
setwd('/home/ada/ymartinez/Experiments/results/final_results/')

file_growth1 <- 'growth_rate_C1.csv'
file_growth2 <- 'growth_rate_C2.csv'
file_growth3 <- 'growth_rate_C3.csv'
file_growth4 <- 'growth_rate_C4.csv'

data_growth1 <- read_csv(file_growth1)
data_growth2 <- read_csv(file_growth2)
data_growth3 <- read_csv(file_growth3)
data_growth4 <- read_csv(file_growth4)

#representar los gráficos para cada condición, el tiempo se multiplica por 60 (está en horas) y se divide por el tiempo de replicación que corresponda a cada condición#
#se representa mediante una línea gruesa la media de todos los datos, y mediante líneas finas cada una de las réplicas de la condición#
C1 <- ggplot() +
  geom_line(data = data_growth1, aes(x = ((time_hours*60)/33.35), y= logNcells_mean), color="#6A00A8FF", size = 2) +
  geom_line(data = data_growth1, aes(x = ((time_hours*60)/33.35), y= logNcells1), color="#6A00A8FF", size = 0.25) +
  geom_line(data = data_growth1, aes(x = ((time_hours*60)/33.35), y= logNcells2), color="#6A00A8FF", size = 0.25) +
  geom_line(data = data_growth1, aes(x = ((time_hours*60)/33.35), y= logNcells3), color="#6A00A8FF", size = 0.25) +
  geom_line(data = data_growth1, aes(x = ((time_hours*60)/33.35), y= logNcells4), color="#6A00A8FF", size = 0.25) +
  geom_line(data = data_growth1, aes(x = ((time_hours*60)/33.35), y= logNcells5), color="#6A00A8FF", size = 0.25) +
  theme(legend.position = 'none') +
  ylab('Log2(N/No)') +
  xlab('replication cycles')+
  labs(title='curva de crecimiento C1')+
  xlim(0,7.75)+
  ylim(0,7)

C2 <- ggplot() +
  geom_line(data = data_growth2, aes(x = ((time_hours*60)/40.47), y= logNcells_mean), color="#6A00A8FF", size = 2) +
  geom_line(data = data_growth2, aes(x = ((time_hours*60)/40.47), y= logNcells1), color="#6A00A8FF", size = 0.25) +
  geom_line(data = data_growth2, aes(x = ((time_hours*60)/40.47), y= logNcells2), color="#6A00A8FF", size = 0.25) +
  geom_line(data = data_growth2, aes(x = ((time_hours*60)/40.47), y= logNcells3), color="#6A00A8FF", size = 0.25) +
  geom_line(data = data_growth2, aes(x = ((time_hours*60)/40.47), y= logNcells4), color="#6A00A8FF", size = 0.25) +
  geom_line(data = data_growth2, aes(x = ((time_hours*60)/40.47), y= logNcells5), color="#6A00A8FF", size = 0.25) +
  theme(legend.position = 'none') +
  ylab('Log2(N/No)') +
  xlab('replication cycles')+
  labs(title='curva de crecimiento C2')+
  xlim(0,7.75)+
  ylim(0,7)

C3 <- ggplot() +
  geom_line(data = data_growth3, aes(x = ((time_hours*60)/30.46), y= logNcells_mean), color="#6A00A8FF", size = 2) +
  geom_line(data = data_growth3, aes(x = ((time_hours*60)/30.46), y= logNcells1), color="#6A00A8FF", size = 0.25) +
  geom_line(data = data_growth3, aes(x = ((time_hours*60)/30.46), y= logNcells2), color="#6A00A8FF", size = 0.25) +
  geom_line(data = data_growth3, aes(x = ((time_hours*60)/30.46), y= logNcells3), color="#6A00A8FF", size = 0.25) +
  geom_line(data = data_growth3, aes(x = ((time_hours*60)/30.46), y= logNcells4), color="#6A00A8FF", size = 0.25) +
  geom_line(data = data_growth3, aes(x = ((time_hours*60)/30.46), y= logNcells5), color="#6A00A8FF", size = 0.25) +
  theme(legend.position = 'none') +
  ylab('Log2(N/No)') +
  xlab('replication cycles')+
  labs(title='curva de crecimiento C3')+
  xlim(0,7.75)+
  ylim(0,7)

C4 <- ggplot() +
  geom_line(data = data_growth4, aes(x = ((time_hours*60)/46.99), y= logNcells_mean), color="#6A00A8FF", size = 2) +
  geom_line(data = data_growth4, aes(x = ((time_hours*60)/46.99), y= logNcells1), color="#6A00A8FF", size = 0.25) +
  geom_line(data = data_growth4, aes(x = ((time_hours*60)/46.99), y= logNcells2), color="#6A00A8FF", size = 0.25) +
  geom_line(data = data_growth4, aes(x = ((time_hours*60)/46.99), y= logNcells3), color="#6A00A8FF", size = 0.25) +
  geom_line(data = data_growth4, aes(x = ((time_hours*60)/46.99), y= logNcells4), color="#6A00A8FF", size = 0.25) +
  geom_line(data = data_growth4, aes(x = ((time_hours*60)/46.99), y= logNcells5), color="#6A00A8FF", size = 0.25) +
  theme(legend.position = 'none') +
  ylab('Log2(N/No)') +
  xlab('replication cycles')+
  labs(title='curva de crecimiento C4')+
  xlim(0,7.75)+
  ylim(0,7)

C1 + C2 + C3 + C4
