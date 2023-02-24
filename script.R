library(tidyverse)

data_uhf <- R.matlab::readMat('uhf_data_r.mat')
data_acust <- R.matlab::readMat('acustico_data_r.mat')

df_uhf <- data.frame(data_uhf$data.UHF)
df_acust <- data.frame(data_acust$data.Acustico)

Fs_uhf <- data_uhf$Fs[1]
Fs_acust <- data_acust$Fs[1]
