
library(readxl)
library(dplyr)
library(ggplot2)
library(broom)


evan_2021 <- read.csv("~/Desktop/THESIS/evan 2021- root mass.csv") %>%
  select(fertilizer_cont, sla, tla, vcmax25, shade_cont) %>%
  mutate(study = "evan2021") %>%
  mutate(vcmax = vcmax25,
         n.trt = fertilizer_cont,
         add.trt = case_when(
           shade_cont == "0"  ~ "0% shade",
           shade_cont == "30" ~ "30% shade",
           shade_cont == "50" ~ "50% shade",
           shade_cont == "80" ~ "80% shade"
         ))


#NITROGEN TO LEAF AREA
evan_2024 <- read.csv("~/Desktop/THESIS/evan 2024 - symbiotic.csv") %>%
  select(n.trt, total.leaf.area) %>%
  mutate(study = "evan2024",
         sla = NA,
         vcmax25 = NA)%>%
  mutate(vcmax = vcmax25,
         tla = total.leaf.area,
         add.trt = NA)


evan_2025 <- read.csv("~/Desktop/THESIS/evan csv = data_sheets_NxCO2xI_compiled_datasheet.csv") %>%
  filter(inoc == "no.inoc")%>%
  select(n.trt, tla, vcmax25, focal.area, focal.biomass, co2,anet) %>%
  mutate(study = "evan2025",
         sla = focal.area/focal.biomass,
         add.trt = case_when(
           co2 == "elv" ~ "elevated CO2",
           co2 == "amb" ~ "ambient CO2"
         )) %>%
  mutate(vcmax = vcmax25)


wang_2024 <- read.csv("~/Desktop/THESIS/wang.csv") %>%
  select(nitrogen.ppm, CO2, Vcmax, SLAcm2.g, leaf.area.cm2.) %>%
  mutate(study = "wang2024",
         sla = SLAcm2.g,
         n.trt = nitrogen.ppm,
         tla = leaf.area.cm2. ,
         vcmax = Vcmax,
         add.trt = case_when(
           CO2 == "elevated" ~ "elevated CO2",
           CO2 == "ambient" ~ "ambient CO2")) 


marzu_2016 <- read.csv("~/Desktop/THESIS/marzu_2016.csv") %>%
  select(NITROGEN, OZONE, vcmax) %>%
  mutate(study = "marzuoli2016",
         sla = NA,
         n.trt = case_when(
           NITROGEN == "+N" ~ "70 kg N/ha*year",
           NITROGEN == "-N" ~ "tap water"),
         tla = NA ,
         vcmax = vcmax,
         add.trt = case_when(
           OZONE == "CF" ~ "-40% O3",
           OZONE == "NF" ~ "-5% O3",
           OZONE == "NF+" ~ "+30% O3",
           OZONE == "NF++" ~ "+75% O3")) 

sturchio.fert <- read.csv("~/Desktop/THESIS/sturchio - wetfert 3.csv") %>%
  filter(Fert == "post") %>%
  filter(treatment %in% c("c", "n")) %>% 
  select(Fert, treatment, vcmax.25, Asat) %>%
  mutate(
    study = "sturchio2022",
    sla = NA,
    n.trt = case_when(
      treatment == "C" ~ "no fertilization",
      treatment == "N" ~ "300g NH4"
    ),
    tla = NA,
    vcmax = vcmax.25,
    add.trt = NA
  )


## datos MALVADOS 

jungchoi <- read.csv("~/Desktop/THESIS/jungchoi.csv") %>%
  select(NO3_percent, SLA_mean, Vcmax_mean, Leaf_area_mean) %>%
  mutate(study = "jungchoi2024",
         sla = SLA_mean,
         n.trt = NO3_percent,
         tla = Leaf_area_mean ,
         vcmax = Vcmax_mean,
         add.trt = NA) 


zheng <- read.csv("~/Desktop/THESIS/zheng.csv")%>%
  select(N_Level, SLA_mean_cm2_g, Vcmax_mean_umol_m2_s, Leaf_Area_mean_cm2) %>%
  mutate(study = "zheng2025",
         sla = SLA_mean_cm2_g,
         n.trt = N_Level,
         tla = Leaf_Area_mean_cm2 ,
         vcmax = Vcmax_mean_umol_m2_s,
         add.trt = NA) 


master_file <- bind_rows(
  evan_2021 %>% mutate(n.trt = as.numeric(n.trt)),
  evan_2024 %>% mutate(n.trt = as.numeric(n.trt)),
  evan_2025 %>% mutate(n.trt = as.numeric(n.trt)),
  wang_2024 %>% mutate(n.trt = as.numeric(n.trt)),
  marzu_2016 %>% mutate(n.trt = as.numeric(n.trt)),
  sturchio.fert %>% mutate(n.trt = as.numeric(n.trt)),
  jungchoi %>% mutate(n.trt = as.numeric(n.trt)),
  zheng %>% mutate(n.trt = as.numeric(n.trt))
) %>%
  select(study, n.trt, vcmax, sla, tla, add.trt)



master_file %>%
  ggplot(aes(x = n.trt, y=vcmax))+
  geom_point(aes(color = add.trt))+
  geom_smooth(aes(color = add.trt), method = "lm")


# GRAPH THIS SHIT ########################
# REDDIT: Michaelis Menten

library(minpack.lm)

# Maximum dependent by datafile, km as the MEDIAN 
initial_estimates <- list(Vmax = max(evan_2021$vcmax, na.rm = TRUE), 
                          Km = median(evan_2021$n.trt))

# Fit model
nls_fit <- nlsLM(vcmax ~ Vmax * n.trt / (Km + n.trt),
                 data = evan_2021,
                 start = initial_estimates)

summary(nls_fit)

# Plot data + smooth fitted curve
plot(evan_2021$n.trt, evan_2021$vcmax, pch = 16,
     main = "Michaelis–Menten Fit", xlab = "n.trt", ylab = "Vcmax")

new_data <- data.frame(n.trt = seq(min(evan_2021$n.trt), max(evan_2021$n.trt), length.out = 200))
lines(new_data$n.trt, predict(nls_fit, newdata = new_data), col = "blue", lwd = 2)

## lets compare graphs ....
library(minpack.lm)
library(mgcv)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork) # for side-by-side plots


## industrializing this 

plz_work <- master_file %>%
  group_by(study)%>%
  mean(vcmax)
   nlsLM(vcmax ~ a * (1 - exp(-b * n.trt)),
                         start = list(a = max(vcmax, na.rm = TRUE), b = 0.05))

   
##NO PIPELIN --> separate curves  ############
   
df <- evan_2021

# vcmax --------------
#NOTE! = intercept (i)  tied to min vmax per 'df' set at the start
# linear
nls_lin_vcmax <- nlsLM(vcmax ~ i + b * n.trt,
                    data = df,
                    start = list(i= min(df$vcmax, na.rm = TRUE), b= 0.1))
   
   
# Exponential rise to max 
nls_exp_vcmax <- nlsLM(vcmax ~ a * (1 - exp(-b * n.trt)) + i,
                       data = subset(df, shade_cont == 0 ),
                       start = list(a = max(df$vcmax, na.rm = TRUE), b = 0.05, i =  min(df$vcmax, na.rm = TRUE)))

# Michaelis–Menten
nls_mm_vcmax <- nlsLM(vcmax ~ Vmax * n.trt / (Km + n.trt)+ i,
                      data = df,
                      start = list(Vmax = max(df$vcmax, na.rm = TRUE), Km = mean(df$n.trt, na.rm = TRUE), i =  min(df$vcmax, na.rm = TRUE)))

# GAM with limited flexibility (k = 4)  --> CHANGE PER TREATMENT 
gam_vcmax <- gam(vcmax ~ s(n.trt, k = 4), data = df)

# TLA  -------------------
#NOTE! = intercept (i)  tied to min vmax per 'df' set at the start

# Exponential growth 
nls_expg_tla <- nlsLM(tla ~ n.trt^(b)+i,
                     data = subset(df, shade_cont == 0),
                     start = list(i =  min(df$tla, na.rm = TRUE), b = 0.05))

# GAM with limited flexibility (k = 4)  --> CHANGE PER TREATMENT 
gam_tla <- gam(tla ~ s(n.trt, k = 4), data = df)


#----- smooth prediction data for plotting all at once 
new_data <- data.frame(n.trt = seq(min(df$n.trt), max(df$n.trt), length.out = 200))

# VCMAX
pred_vcmax <- data.frame(
  n.trt = new_data$n.trt,
  lin = predict(nls_lin_vcmax, new_data),
  exp = predict(nls_exp_vcmax, new_data),
  mm = predict(nls_mm_vcmax, new_data),
  gam = predict(gam_vcmax, new_data)
) %>%
  pivot_longer(-n.trt, names_to = "model", values_to = "fit")


#  TLA
pred_tla <- data.frame(
  n.trt = new_data$n.trt,
  expg = predict(nls_expg_tla, new_data),
  gam = predict(gam_tla, new_data)
) %>%
  pivot_longer(-n.trt, names_to = "model", values_to = "fit")

###


plot_vcmax <- ggplot(df, aes(x = n.trt, y = vcmax)) +
  geom_point(alpha = 0.6) +
  geom_line(data = pred_vcmax, aes(y = fit, color = model), size = 1) +
  theme_bw() +
  labs(title = "Vcmax Response to Fertilizer",
       x = "Fertilizer treatment ",
       y = "Vcmax") +
  scale_color_manual(values = c("exp" = "blue", "mm" = "red", "gam" = "green", "lin" = "orange"))

plot_tla <- ggplot(df, aes(x = n.trt, y = tla)) +
  geom_point(alpha = 0.6) +
  geom_line(data = pred_tla, aes(y = fit, color = model), size = 1) +
  theme_bw() +
  labs(title = "TLA Response to Fertilizer",
       x = "Fertilizer treatment ",
       y = "Total Leaf Area (TLA)") +
  scale_color_manual(values = c("expg" = "purple", "gam" = "green"))

#side-by-side
plot_vcmax + plot_tla




