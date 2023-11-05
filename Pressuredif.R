library(tidyverse)
library(readxl)
library(here)
library(ggplot2)
library(ggh4x)
library(ggdist)
library(blandr)

dat <- readxl::read_xlsx(path = here::here("pressure_data.xlsx"))


prepped_dat <- dat %>% 
  dplyr::mutate(valve = if_else(`ValveType 1;N 0;S` == 0, "Sapien", "Navitor"), 
                catheter = if_else(catheter == 55555, NA_real_, catheter), 
                diff = catheter - THVmeanPG_Post) 

long_dat <- tidyr::pivot_longer(prepped_dat, 
                      cols = c(catheter, THVmeanPG_Post), 
                      names_to = "measure_device", 
                      values_to = "pressure")

fig <- ggpubr::ggboxplot(long_dat, x = "valve", 
                  y = "pressure", fill = "valve",
                  facet.by = "measure_device") + 
  scale_y_continuous(limits = c(0, 35)) +
  ggpubr::stat_compare_means(label.y = 32)

fig2 <- ggpubr::ggboxplot(long_dat, x = "measure_device", 
                         y = "pressure", 
                         fill = "measure_device", facet.by = "valve") + 
  scale_y_continuous(limits = c(0, 35)) +
  ggpubr::stat_compare_means(label.y = 32)

ggpubr::ggpaired(long_dat, x = "measure_device", 
                  y = "pressure", 
                  fill = "measure_device", facet.by = "valve") + 
  scale_y_continuous(limits = c(0, 35)) +
  ggpubr::stat_compare_means(label.y = 32)




