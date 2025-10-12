
setwd("~/Desktop/Data Org")
library(readxl)
library(dplyr)
library(ggplot2)
library(broom)


evan_2021 <- read.csv("~/Desktop/Data Org/evan 2021- root mass.csv") %>%
  select(fertilizer_cont, sla, tla, vcmax25, shade_cont) %>%
  mutate(study = "evan2021") %>%
  mutate(vcmax = vcmax25,
         n.trt = fertilizer_cont,
         asat = NA,
         anet= NA,
         add.trt = case_when(
           shade_cont == "0"  ~ "0% shade",
           shade_cont == "30" ~ "30% shade",
           shade_cont == "50" ~ "50% shade",
           shade_cont == "80" ~ "80% shade"
         ))


#NITROGEN TO LEAF AREA
evan_2024 <- read.csv("~/Desktop/Data Org/evan 2024 - symbiotic.csv") %>%
  select(n.trt, total.leaf.area) %>%
  mutate(study = "evan2024",
         sla = NA,
         asat = NA,
         anet = NA,
         vcmax25 = NA)%>%
  mutate(vcmax = vcmax25,
         tla = total.leaf.area,
         add.trt = NA)


evan_2025 <- read.csv("~/Desktop/Data Org/evan csv = data_sheets_NxCO2xI_compiled_datasheet.csv") %>%
  filter(inoc == "no.inoc")%>%
  select(n.trt, tla, vcmax25, focal.area, focal.biomass, co2,anet) %>%
  mutate(study = "evan2025",
         sla = focal.area/focal.biomass,
         asat = NA,
         anet = anet,
         add.trt = case_when(
           co2 == "elv" ~ "elevated CO2",
           co2 == "amb" ~ "ambient CO2"
         )) %>%
  mutate(vcmax = vcmax25)


wang_2024 <- read.csv("~/Desktop/Data Org/wang.csv") %>%
  select(nitrogen.ppm, CO2, Vcmax, SLAcm2.g, leaf.area.cm2.) %>%
  mutate(study = "wang2024",
         sla = SLAcm2.g,
         n.trt = nitrogen.ppm,
         asat = NA,
         anet = NA,
         tla = leaf.area.cm2. ,
         vcmax = Vcmax,
         add.trt = case_when(
           CO2 == "elevated" ~ "elevated CO2",
           CO2 == "ambient" ~ "ambient CO2")) 


marzu_2016 <- read.csv("~/Desktop/Data Org/marzu_2016.csv") %>%
  select(NITROGEN, OZONE, vcmax) %>%
  mutate(study = "marzuoli2016",
         sla = NA,
         n.trt = case_when(
           NITROGEN == "+N" ~ "70 kg N/ha*year",
           NITROGEN == "-N" ~ "tap water"),
         tla = NA ,
         asat = NA,
         anet = NA,
         vcmax = vcmax,
         add.trt = case_when(
           OZONE == "CF" ~ "-40% O3",
           OZONE == "NF" ~ "-5% O3",
           OZONE == "NF+" ~ "+30% O3",
           OZONE == "NF++" ~ "+75% O3")) 

#doesnt fukcing work
sturchio.fert <- read.csv("~/Desktop/Data Org/sturchio - wetfert 3.csv") %>%
  filter(Fert == "post") %>%
  filter(treatment %in% c("c", "n")) %>% 
  select(Fert, treatment, vcmax.25, Asat) %>%
  mutate(
    study = "sturchio2022",
    sla = NA,
    asat = Asat,  
    anet = NA,
    n.trt = case_when(
      treatment == "C" ~ "no fertilization",
      treatment == "N" ~ "300g NH4"
    ),
    tla = NA,
    vcmax = vcmax.25,
    add.trt = NA
  )


## datos MALVADOS 

jungchoi <- read.csv("~/Desktop/Data Org/jungchoi.csv") %>%
  select(NO3_percent, SLA_mean, Vcmax_mean, Leaf_area_mean) %>%
  mutate(study = "jungchoi2024",
         sla = SLA_mean,
         n.trt = NO3_percent,
         asat = NA,
         anet = NA,
         tla = Leaf_area_mean ,
         vcmax = Vcmax_mean,
         add.trt = NA) 


zheng <- read.csv("~/Desktop/Data Org/zheng.csv")%>%
  select(N_Level, SLA_mean_cm2_g, Vcmax_mean_umol_m2_s, Leaf_Area_mean_cm2) %>%
  mutate(study = "zheng2025",
         sla = SLA_mean_cm2_g,
         n.trt = N_Level,
         asat = NA,
         anet = NA,
         tla = Leaf_Area_mean_cm2 ,
         vcmax = Vcmax_mean_umol_m2_s,
         add.trt = NA) 

  ## mean over
  
waring2023 <- read.csv("~/Desktop/Data Org/waring2023.csv") %>%
  select(tla, sla, Vcmax, N, LightLevel) %>%
  mutate(study = "waring2023",
         sla = sla,
         n.trt = N ,
         tla = tla ,
         asat = NA ,
         anet = NA ,
         vcmax = Vcmax,
         add.trt = case_when(
           LightLevel == "zero" ~ "0% Light",
           LightLevel == "thirty" ~ "30% Light",
           LightLevel == "fifty+" ~ "50% Light",
           LightLevel == "eighty++" ~ "80% Light")) 

master_file <- rbind(
  evan_2021 %>% select(study, n.trt, vcmax, asat, anet, sla, tla, add.trt),
  evan_2024 %>% select(study, n.trt, vcmax, asat, anet, sla, tla, add.trt),
  evan_2025 %>% select(study, n.trt, vcmax, asat, anet, sla, tla, add.trt),
  wang_2024 %>% select(study, n.trt, vcmax, asat, anet, sla, tla, add.trt),
  marzu_2016 %>% select(study, n.trt, vcmax, asat, anet, sla, tla, add.trt),
  sturchio.fert %>% select(study, n.trt, vcmax, asat, anet, sla, tla, add.trt),
  jungchoi %>% select(study, n.trt, vcmax, asat, anet, sla, tla, add.trt),
  zheng %>% select(study, n.trt, vcmax, asat, anet, sla, tla, add.trt),
  waring2023 %>% select(study, n.trt, vcmax, asat, anet, sla, tla, add.trt)
)


master_file %>%
  ggplot(aes(x = n.trt, y=vcmax))+
  geom_point(aes(color = add.trt))+
  geom_smooth(aes(color = add.trt), method = "lm")


##reddit saturating curve 
m <- glm(vcmax ~ n.trt, family = gaussian, data = master_file)

b <- augment(m, type.predict = "response")
ggplot(b, aes(x=n.trt, y=.fitted))+
  geom_line(color = 'blue')
