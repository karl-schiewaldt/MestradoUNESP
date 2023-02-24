library(tidyverse)
library(R.matlab)

data_uhf <- readMat('uhf_data_r.mat')
data_acust <- readMat('acustico_data_r.mat')

df_uhf <- data.frame(data_uhf$data.UHF)
df_acust <- data.frame(data_acust$data.Acustico)

Fs_uhf <- data_uhf$Fs[1]
Fs_acust <- data_acust$Fs[1]
