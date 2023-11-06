library(tidyverse)
library(readxl)
library(here)
library(ggplot2)
library(ggpubr)
library(lme4)
library(parameters)

dat <- readxl::read_xlsx(path = here::here("pressure_data.xlsx"))


prepped_dat <- dat %>% 
  dplyr::mutate(valve = if_else(`ValveType 1;N 0;S` == 0, "Sapien", "Navitor"), 
                catheter = if_else(catheter == 55555, NA_real_, catheter), 
                diff = catheter - THVmeanPG_Post, 
                echo = THVmeanPG_Post) %>% 
  tidyr::drop_na()

long_dat <- tidyr::pivot_longer(prepped_dat, 
                      cols = c(catheter, echo), 
                      names_to = "measure_device", 
                      values_to = "pressure") %>% 
  dplyr::mutate(valve_device = paste0(valve, "_", measure_device), 
                valve_device_fct = forcats::fct_relevel(valve_device, 
                                                        c("Sapien_catheter", "Sapien_echo", "Navitor_catheter", "Navitor_echo")))


# LMM model ---------------------------------------------------------------

fit <- lmer(pressure ~ valve + measure_device + valve*measure_device + (1|PatientIdOld), data = long_dat)

valve_pval <- as.data.frame(parameters::model_parameters(fit)) |> 
  base::subset(subset = Parameter == "valveSapien", select = "p") |> 
  as.numeric()

# make figure -------------------------------------------------------------

fig <- ggpubr::ggboxplot(long_dat, x = "valve_device_fct", 
                  y = "pressure", fill = "valve_device_fct") + 
  scale_y_continuous(limits = c(0, 35)) +
  ggpubr::stat_compare_means(
    aes(label = paste0("p = ", after_stat(p.format))), 
    comparisons = list(c("Sapien_catheter", "Sapien_echo"),
                       c("Navitor_catheter", "Navitor_echo")),
    method = "t.test", paired = TRUE, label.y = 30)



# fig2 <- ggpubr::ggboxplot(long_dat, x = "measure_device", 
#                          y = "pressure", 
#                          fill = "measure_device", facet.by = "valve") + 
#   scale_y_continuous(limits = c(0, 35)) +
#   ggpubr::stat_compare_means(label.y = 32)
# 
# ggpubr::ggpaired(long_dat, x = "measure_device", 
#                   y = "pressure", 
#                   fill = "measure_device", facet.by = "valve") + 
#   scale_y_continuous(limits = c(0, 35)) +
#   ggpubr::stat_compare_means(label.y = 32)




