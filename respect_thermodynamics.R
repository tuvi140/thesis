library(readxl)
library(dplyr)
library(ggplot2)
library(broom)
library(purrr)
library(tidyr)
library(readr)
library(tibble)
library(minpack.lm)
library(writexl)
library(patchwork)
library(stringr)

evan_2021 <- read.csv("THESIS/evan 2021- root mass.csv") %>%
  select(fertilizer_cont, sla, tla, vcmax25, shade_cont, spp, narea) %>%
  mutate(
    study   = "evan2021",
    species = spp,
    vcmax   = vcmax25,
    pot     = 3,
    freq    = 2,
    vol     = 150,
    time    = 6,
    n.trt   = as.numeric(fertilizer_cont),
    shade.trt = case_when(
      shade_cont == "0"  ~ "0% shade",
      shade_cont == "30" ~ "30% shade",
      shade_cont == "50" ~ "50% shade",
      shade_cont == "80" ~ "80% shade",
      TRUE               ~ "none"
    ),
    add.trt = paste(shade.trt, spp)
  ) %>%
  select(-shade.trt) %>%
  mutate(
    N_1 = n.trt * (vol / 1000),
    N_2 = N_1 * freq * time,
    N_3 = N_2 / (pot * time)
  )

evan_2025 <- read.csv("THESIS/evan csv = data_sheets_NxCO2xI_compiled_datasheet.csv") %>%
  filter(inoc == "no.inoc") %>%
  select(n.trt, tla, vcmax25, co2) %>%
  mutate(
    study   = "evan2025",
    species = "Soybean",
    vcmax   = vcmax25,
    pot     = 6,
    freq    = 2,
    vol     = 150,
    time    = 7,
    n.trt   = as.numeric(n.trt),
    add.trt = case_when(
      co2 == "elv" ~ "elevated CO2",
      co2 == "amb" ~ "ambient CO2",
      TRUE         ~ "none"
    )
  ) %>%
  mutate(
    N_1 = n.trt * (vol / 1000),
    N_2 = N_1 * freq * time,
    N_3 = N_2 / (pot * time)
  )

## wang 2024
# wang_ppm <- c(
#   `10`  = 0.14,
#   `80`  = 1.12,
#   `150` = 2.10,
#   `220` = 3.08,
#   `290` = 4.06
# )

wang_2024 <- read.csv("THESIS/wang.csv") %>%
  select(
    nitrogen.ppm,
    CO2,
    species,
    Vcmax,
    leaf.area.cm2.
  ) %>%
  mutate(
    study  = "wang2024",
    n.trt = as.numeric(wang_ppm[as.character(nitrogen.ppm)]),
    # n.trt = as.numeric(nitrogen.ppm),
    tla    = leaf.area.cm2.,
    pot    = 3.5,
    time   = 8,
    vol    = 500,
    freq   = 2,
    vcmax  = Vcmax,
    co2    = case_when(
      CO2 == "elevated" ~ "elevated CO2",
      CO2 == "ambient"  ~ "ambient CO2",
      TRUE              ~ "none"
    ),
    add.trt = paste(co2, species)
  ) %>%
  select(-co2) %>%
  filter(vcmax <= 500) %>%
  mutate(
    n.trt = wang_ppm[as.character(n.trt)]
  ) %>%
  mutate(
    N_1 = n.trt * (vol / 1000),
    N_2 = N_1 * freq * time,
    N_3 = N_2 / (pot * time)
  )

##group 1: divide for plots

vcmax_group <- bind_rows(
  evan_2021,
  evan_2025,
  wang_2024,
  jungchoi
) %>%
  mutate(
    add.trt = as.character(add.trt),
    n.trt   = as.numeric(n.trt)
  ) %>%
  select(study, n.trt, N_1, N_2, N_3, vcmax, tla, add.trt, species)

tla_group <- bind_rows(
  evan_2021,
  evan_2024,
  evan_2025,
  wang_2024,
  jungchoi,
  zheng
) %>%
  mutate(
    add.trt = as.character(add.trt),
    n.trt   = as.numeric(n.trt)
  ) %>%
  select(study, n.trt, N_1, N_2, N_3, tla, add.trt, species)



treatment_levels <- c(
  "0% shade",
  "30% shade",
  "50% shade",
  "80% shade",
  "ambient CO2",
  "elevated CO2"
)

species_levels <- c(
  "Cotton",
  "Soybean",
  "White birch",
  "Yellow birch",
  "Lettuce"
)

standardize_groups <- function(df) {
  df %>%
    mutate(
      treatment = case_when(
        str_detect(add.trt, "0% shade")  ~ "0% shade",
        str_detect(add.trt, "30% shade") ~ "30% shade",
        str_detect(add.trt, "50% shade") ~ "50% shade",
        str_detect(add.trt, "80% shade") ~ "80% shade",
        str_detect(add.trt, regex("ambient",  ignore_case = TRUE))  ~ "ambient CO2",
        str_detect(add.trt, regex("elevated", ignore_case = TRUE)) ~ "elevated CO2",
        TRUE ~ "none"
      ),
      species = case_when(
        str_detect(add.trt, regex("cotton",       ignore_case = TRUE)) ~ "Cotton",
        str_detect(add.trt, regex("soy",          ignore_case = TRUE)) ~ "Soybean",
        str_detect(add.trt, regex("white birch",  ignore_case = TRUE)) ~ "White birch",
        str_detect(add.trt, regex("yellow birch", ignore_case = TRUE)) ~ "Yellow birch",
        str_detect(add.trt, regex("lettuce",      ignore_case = TRUE)) ~ "Lettuce",
        TRUE ~ as.character(species)
      ),
      treatment = factor(treatment, levels = treatment_levels),
      species   = factor(species,   levels = species_levels)
    )
}


### group 2: normalize and bind key parts 

vcmax_group <- standardize_groups(vcmax_group)
tla_group   <- standardize_groups(tla_group)

vcmax_group <- vcmax_group %>%
  group_by(study, species) %>%
  mutate(vcnorm = vcmax / mean(vcmax, na.rm = TRUE)) %>%
  ungroup()

tla_group <- tla_group %>%
  group_by(study, species) %>%
  mutate(tlanorm = tla / mean(tla, na.rm = TRUE)) %>%
  ungroup()

## exponential fits with intercept I 

I_start <- 0.5

vcm_ppm <- nlsLM(
  vcnorm ~ I + A * (1 - exp(-k * n.trt)),
  data   = vcmax_group,
  start  = list(I = I_start, A = max(vcmax_group$vcnorm, na.rm = TRUE) - I_start, k = 0.01),
  control = nls.lm.control(maxiter = 200)
)

vcm_n1 <- nlsLM(
  vcnorm ~ I + A * (1 - exp(-k * N_1)),
  data   = vcmax_group,
  start  = list(I = I_start, A = max(vcmax_group$vcnorm, na.rm = TRUE) - I_start, k = 0.01),
  control = nls.lm.control(maxiter = 200)
)

vcm_n2 <- nlsLM(
  vcnorm ~ I + A * (1 - exp(-k * N_2)),
  data   = vcmax_group,
  start  = list(I = I_start, A = max(vcmax_group$vcnorm, na.rm = TRUE) - I_start, k = 0.01),
  control = nls.lm.control(maxiter = 200)
)

vcm_n3 <- nlsLM(
  vcnorm ~ I + A * (1 - exp(-k * N_3)),
  data   = vcmax_group,
  start  = list(I = I_start, A = max(vcmax_group$vcnorm, na.rm = TRUE) - I_start, k = 0.01),
  control = nls.lm.control(maxiter = 200)
)

tla_ppm <- nlsLM(
  tlanorm ~ I + A * (1 - exp(-k * n.trt)),
  data   = tla_group,
  start  = list(I = I_start, A = max(tla_group$tlanorm, na.rm = TRUE) - I_start, k = 0.01),
  control = nls.lm.control(maxiter = 200)
)

tla_n1 <- nlsLM(
  tlanorm ~ I + A * (1 - exp(-k * N_1)),
  data   = tla_group,
  start  = list(I = I_start, A = max(tla_group$tlanorm, na.rm = TRUE) - I_start, k = 0.01),
  control = nls.lm.control(maxiter = 200)
)

tla_n2 <- nlsLM(
  tlanorm ~ I + A * (1 - exp(-k * N_2)),
  data   = tla_group,
  start  = list(I = I_start, A = max(tla_group$tlanorm, na.rm = TRUE) - I_start, k = 0.01),
  control = nls.lm.control(maxiter = 200)
)

tla_n3 <- nlsLM(
  tlanorm ~ I + A * (1 - exp(-k * N_3)),
  data   = tla_group,
  start  = list(I = I_start, A = max(tla_group$tlanorm, na.rm = TRUE) - I_start, k = 0.01),
  control = nls.lm.control(maxiter = 200)
)

## parameters per n and plot 
pars_vcm_ppm <- coef(vcm_ppm); I_vcm_ppm <- pars_vcm_ppm["I"]; A_vcm_ppm <- pars_vcm_ppm["A"]; k_vcm_ppm <- pars_vcm_ppm["k"]
pars_vcm_n1  <- coef(vcm_n1);  I_vcm_n1  <- pars_vcm_n1["I"];  A_vcm_n1  <- pars_vcm_n1["A"];  k_vcm_n1  <- pars_vcm_n1["k"]
pars_vcm_n2  <- coef(vcm_n2);  I_vcm_n2  <- pars_vcm_n2["I"];  A_vcm_n2  <- pars_vcm_n2["A"];  k_vcm_n2  <- pars_vcm_n2["k"]
pars_vcm_n3  <- coef(vcm_n3);  I_vcm_n3  <- pars_vcm_n3["I"];  A_vcm_n3  <- pars_vcm_n3["A"];  k_vcm_n3  <- pars_vcm_n3["k"]

pars_tla_ppm <- coef(tla_ppm); I_tla_ppm <- pars_tla_ppm["I"]; A_tla_ppm <- pars_tla_ppm["A"]; k_tla_ppm <- pars_tla_ppm["k"]
pars_tla_n1  <- coef(tla_n1);  I_tla_n1  <- pars_tla_n1["I"];  A_tla_n1  <- pars_tla_n1["A"];  k_tla_n1  <- pars_tla_n1["k"]
pars_tla_n2  <- coef(tla_n2);  I_tla_n2  <- pars_tla_n2["I"];  A_tla_n2  <- pars_tla_n2["A"];  k_tla_n2  <- pars_tla_n2["k"]
pars_tla_n3  <- coef(tla_n3);  I_tla_n3  <- pars_tla_n3["I"];  A_tla_n3  <- pars_tla_n3["A"];  k_tla_n3  <- pars_tla_n3["k"]

r2_nls <- function(model, data, y_col) {
  y <- data[[y_col]]
  yhat <- predict(model, newdata = data)
  rss <- sum((y - yhat)^2, na.rm = TRUE)
  tss <- sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
  1 - rss / tss
}

## table of 8 exponential fits: A, I, k, r^2

exp_fits_table <- tibble(
  response = c(rep("Vcmax", 4), rep("TLA", 4)),
  N_metric = rep(c("ppm", "N1", "N2", "N3"), 2),
  A  = c(A_vcm_ppm, A_vcm_n1, A_vcm_n2, A_vcm_n3, A_tla_ppm, A_tla_n1, A_tla_n2, A_tla_n3),
  I  = c(I_vcm_ppm, I_vcm_n1, I_vcm_n2, I_vcm_n3, I_tla_ppm, I_tla_n1, I_tla_n2, I_tla_n3),
  k  = c(k_vcm_ppm, k_vcm_n1, k_vcm_n2, k_vcm_n3, k_tla_ppm, k_tla_n1, k_tla_n2, k_tla_n3),
  r2 = c(
    r2_nls(vcm_ppm, vcmax_group, "vcnorm"),
    r2_nls(vcm_n1,  vcmax_group, "vcnorm"),
    r2_nls(vcm_n2,  vcmax_group, "vcnorm"),
    r2_nls(vcm_n3,  vcmax_group, "vcnorm"),
    r2_nls(tla_ppm, tla_group,   "tlanorm"),
    r2_nls(tla_n1,  tla_group,   "tlanorm"),
    r2_nls(tla_n2,  tla_group,   "tlanorm"),
    r2_nls(tla_n3,  tla_group,   "tlanorm")
  )
)

print(exp_fits_table)

## 95% asymptote point 
N_flat_from_k <- function(k) {
  -log(0.05) / k
}

diminishing_table <- exp_fits_table %>%
  mutate(
    N_flat        = N_flat_from_k(k),
    y_at_flat     = I + A * (1 - exp(-k * N_flat)),
    slope_at_flat = A * k * exp(-k * N_flat)
  )

print(diminishing_table)
write_xlsx(exp_fits_table, "AIKRtable.xlsx")
write_xlsx(diminishing_table, "ASYMPTOTE.xlsx")

## grids ##############################
grid_vcm_ppm <- tibble(
  n.trt = seq(min(vcmax_group$n.trt, na.rm = TRUE),
              max(vcmax_group$n.trt, na.rm = TRUE),
              length.out = 200)
) %>%
  mutate(vcnorm_hat = I_vcm_ppm + A_vcm_ppm * (1 - exp(-k_vcm_ppm * n.trt)))

grid_vcm_N1 <- tibble(
  N_1 = seq(min(vcmax_group$N_1, na.rm = TRUE),
            max(vcmax_group$N_1, na.rm = TRUE),
            length.out = 200)
) %>%
  mutate(vcnorm_hat = I_vcm_n1 + A_vcm_n1 * (1 - exp(-k_vcm_n1 * N_1)))

grid_vcm_N2 <- tibble(
  N_2 = seq(min(vcmax_group$N_2, na.rm = TRUE),
            max(vcmax_group$N_2, na.rm = TRUE),
            length.out = 200)
) %>%
  mutate(vcnorm_hat = I_vcm_n2 + A_vcm_n2 * (1 - exp(-k_vcm_n2 * N_2)))

grid_vcm_N3 <- tibble(
  N_3 = seq(min(vcmax_group$N_3, na.rm = TRUE),
            max(vcmax_group$N_3, na.rm = TRUE),
            length.out = 200)
) %>%
  mutate(vcnorm_hat = I_vcm_n3 + A_vcm_n3 * (1 - exp(-k_vcm_n3 * N_3)))

grid_tla_ppm <- tibble(
  n.trt = seq(min(tla_group$n.trt, na.rm = TRUE),
              max(tla_group$n.trt, na.rm = TRUE),
              length.out = 200)
) %>%
  mutate(tlanorm_hat = I_tla_ppm + A_tla_ppm * (1 - exp(-k_tla_ppm * n.trt)))

grid_tla_N1 <- tibble(
  N_1 = seq(min(tla_group$N_1, na.rm = TRUE),
            max(tla_group$N_1, na.rm = TRUE),
            length.out = 200)
) %>%
  mutate(tlanorm_hat = I_tla_n1 + A_tla_n1 * (1 - exp(-k_tla_n1 * N_1)))

grid_tla_N2 <- tibble(
  N_2 = seq(min(tla_group$N_2, na.rm = TRUE),
            max(tla_group$N_2, na.rm = TRUE),
            length.out = 200)
) %>%
  mutate(tlanorm_hat = I_tla_n2 + A_tla_n2 * (1 - exp(-k_tla_n2 * N_2)))

grid_tla_N3 <- tibble(
  N_3 = seq(min(tla_group$N_3, na.rm = TRUE),
            max(tla_group$N_3, na.rm = TRUE),
            length.out = 200)
) %>%
  mutate(tlanorm_hat = I_tla_n3 + A_tla_n3 * (1 - exp(-k_tla_n3 * N_3)))

## plots (exponential)
p_vcm_ppm <- ggplot(vcmax_group, aes(x = n.trt, y = vcnorm)) +
  geom_point(aes(color = treatment, shape = species), size = 2, alpha = 0.7) +
  geom_line(data = grid_vcm_ppm, aes(x = n.trt, y = vcnorm_hat), linewidth = 1, inherit.aes = FALSE) +
  scale_color_discrete(drop = FALSE) +
  labs(title = "Vcmax per Nitrogen (ppm)", x = "Nitrogen (ppm)", y = "Normalized Vcmax") +
  theme_classic()

p_vcm_N1 <- ggplot(vcmax_group, aes(x = N_1, y = vcnorm)) +
  geom_point(aes(color = treatment, shape = species), size = 2, alpha = 0.7) +
  geom_line(data = grid_vcm_N1, aes(x = N_1, y = vcnorm_hat), linewidth = 1, inherit.aes = FALSE) +
  scale_color_discrete(drop = FALSE) +
  labs(title = "Vcmax per Instantaneous mg N", x = "Instantaneous mg N (N1)", y = "Normalized Vcmax") +
  theme_classic()

p_vcm_N2 <- ggplot(vcmax_group, aes(x = N_2, y = vcnorm)) +
  geom_point(aes(color = treatment, shape = species), size = 2, alpha = 0.7) +
  geom_line(data = grid_vcm_N2, aes(x = N_2, y = vcnorm_hat), linewidth = 1, inherit.aes = FALSE) +
  scale_color_discrete(drop = FALSE) +
  labs(title = "Vcmax per Cumulative mg N", x = "Cumulative mg N (N2)", y = "Normalized Vcmax") +
  theme_classic()

p_vcm_N3 <- ggplot(vcmax_group, aes(x = N_3, y = vcnorm)) +
  geom_point(aes(color = treatment, shape = species), size = 2, alpha = 0.7) +
  geom_line(data = grid_vcm_N3, aes(x = N_3, y = vcnorm_hat), linewidth = 1, inherit.aes = FALSE) +
  scale_color_discrete(drop = FALSE) +
  labs(title = "Vcmax per mg N / L * Week", x = "mg N / L * Week (N3)", y = "Normalized Vcmax") +
  theme_classic()

p_tla_ppm <- ggplot(tla_group, aes(x = n.trt, y = tlanorm)) +
  geom_point(aes(color = treatment, shape = species), size = 2, alpha = 0.7) +
  geom_line(data = grid_tla_ppm, aes(x = n.trt, y = tlanorm_hat), linewidth = 1, inherit.aes = FALSE) +
  scale_color_discrete(drop = FALSE) +
  labs(title = "TLA per Nitrogen (ppm)", x = "Nitrogen (ppm)", y = "Normalized TLA") +
  theme_classic()

p_tla_N1 <- ggplot(tla_group, aes(x = N_1, y = tlanorm)) +
  geom_point(aes(color = treatment, shape = species), size = 2, alpha = 0.7) +
  geom_line(data = grid_tla_N1, aes(x = N_1, y = tlanorm_hat), linewidth = 1, inherit.aes = FALSE) +
  scale_color_discrete(drop = FALSE) +
  labs(title = "TLA per Instantaneous mg N", x = "Instantaneous mg N (N1)", y = "Normalized TLA") +
  theme_classic()

p_tla_N2 <- ggplot(tla_group, aes(x = N_2, y = tlanorm)) +
  geom_point(aes(color = treatment, shape = species), size = 2, alpha = 0.7) +
  geom_line(data = grid_tla_N2, aes(x = N_2, y = tlanorm_hat), linewidth = 1, inherit.aes = FALSE) +
  scale_color_discrete(drop = FALSE) +
  labs(title = "TLA per Cumulative mg N", x = "Cumulative mg N (N2)", y = "Normalized TLA") +
  theme_classic()

p_tla_N3 <- ggplot(tla_group, aes(x = N_3, y = tlanorm)) +
  geom_point(aes(color = treatment, shape = species), size = 2, alpha = 0.7) +
  geom_line(data = grid_tla_N3, aes(x = N_3, y = tlanorm_hat), linewidth = 1, inherit.aes = FALSE) +
  scale_color_discrete(drop = FALSE) +
  labs(title = "TLA per mg N / L * Week", x = "mg N / L * Week (N3)", y = "Normalized TLA") +
  theme_classic()

exp_panel <- (p_vcm_ppm | p_vcm_N1 | p_vcm_N2 | p_vcm_N3) /
  (p_tla_ppm | p_tla_N1 | p_tla_N2 | p_tla_N3) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

exp_panel

## linear regression
lm_vcm_ppm <- lm(vcnorm ~ n.trt, data = vcmax_group)
lm_vcm_N1  <- lm(vcnorm ~ N_1,   data = vcmax_group)
lm_vcm_N2  <- lm(vcnorm ~ N_2,   data = vcmax_group)
lm_vcm_N3  <- lm(vcnorm ~ N_3,   data = vcmax_group)

lm_tla_ppm <- lm(tlanorm ~ n.trt, data = tla_group)
lm_tla_N1  <- lm(tlanorm ~ N_1,   data = tla_group)
lm_tla_N2  <- lm(tlanorm ~ N_2,   data = tla_group)
lm_tla_N3  <- lm(tlanorm ~ N_3,   data = tla_group)

slopes_table <- bind_rows(
  tidy(lm_vcm_ppm) %>% filter(term == "n.trt") %>% mutate(response = "Vcmax", N_metric = "ppm"),
  tidy(lm_vcm_N1 ) %>% filter(term == "N_1")   %>% mutate(response = "Vcmax", N_metric = "N1"),
  tidy(lm_vcm_N2 ) %>% filter(term == "N_2")   %>% mutate(response = "Vcmax", N_metric = "N2"),
  tidy(lm_vcm_N3 ) %>% filter(term == "N_3")   %>% mutate(response = "Vcmax", N_metric = "N3"),
  tidy(lm_tla_ppm) %>% filter(term == "n.trt") %>% mutate(response = "TLA",   N_metric = "ppm"),
  tidy(lm_tla_N1 ) %>% filter(term == "N_1")   %>% mutate(response = "TLA",   N_metric = "N1"),
  tidy(lm_tla_N2 ) %>% filter(term == "N_2")   %>% mutate(response = "TLA",   N_metric = "N2"),
  tidy(lm_tla_N3 ) %>% filter(term == "N_3")   %>% mutate(response = "TLA",   N_metric = "N3")
)

print(slopes_table)
# write_xlsx(slopes_table, "linear_slopes.xlsx")

eq_label <- function(model, x_name = "N") {
  cf <- coef(model)
  a  <- cf[1]
  b  <- cf[2]
  paste0("y = ", round(a, 2), " + ", round(b, 4), " * ", x_name)
}

p_vcm_ppm_lin <- ggplot(vcmax_group, aes(x = n.trt, y = vcnorm)) +
  geom_point(aes(color = treatment, shape = species), size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_color_discrete(drop = FALSE) +
  annotate("text", x = Inf, y = Inf, label = eq_label(lm_vcm_ppm, "ppm"),
           hjust = 1.1, vjust = 1.5, size = 3) +
  labs(title = "Vcmax per Nitrogen (ppm)", x = "Nitrogen (ppm)", y = "Normalized Vcmax") +
  theme_classic()

p_vcm_N1_lin <- ggplot(vcmax_group, aes(x = N_1, y = vcnorm)) +
  geom_point(aes(color = treatment, shape = species), size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_color_discrete(drop = FALSE) +
  annotate("text", x = Inf, y = Inf, label = eq_label(lm_vcm_N1, "N1"),
           hjust = 1.1, vjust = 1.5, size = 3) +
  labs(title = "Vcmax per Instantaneous mg N", x = "Instantaneous mg N (N1)", y = "Normalized Vcmax") +
  theme_classic()

p_vcm_N2_lin <- ggplot(vcmax_group, aes(x = N_2, y = vcnorm)) +
  geom_point(aes(color = treatment, shape = species), size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_color_discrete(drop = FALSE) +
  annotate("text", x = Inf, y = Inf, label = eq_label(lm_vcm_N2, "N2"),
           hjust = 1.1, vjust = 1.5, size = 3) +
  labs(title = "Vcmax per Cumulative mg N", x = "Cumulative mg N (N2)", y = "Normalized Vcmax") +
  theme_classic()

p_vcm_N3_lin <- ggplot(vcmax_group, aes(x = N_3, y = vcnorm)) +
  geom_point(aes(color = treatment, shape = species), size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_color_discrete(drop = FALSE) +
  annotate("text", x = Inf, y = Inf, label = eq_label(lm_vcm_N3, "N3"),
           hjust = 1.1, vjust = 1.5, size = 3) +
  labs(title = "Vcmax per mg N / L * Week", x = "mg N / L * Week (N3)", y = "Normalized Vcmax") +
  theme_classic()

p_tla_ppm_lin <- ggplot(tla_group, aes(x = n.trt, y = tlanorm)) +
  geom_point(aes(color = treatment, shape = species), size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_color_discrete(drop = FALSE) +
  annotate("text", x = Inf, y = Inf, label = eq_label(lm_tla_ppm, "ppm"),
           hjust = 1.1, vjust = 1.5, size = 3) +
  labs(title = "TLA per Nitrogen (ppm)", x = "Nitrogen (ppm)", y = "Normalized TLA") +
  theme_classic()

p_tla_N1_lin <- ggplot(tla_group, aes(x = N_1, y = tlanorm)) +
  geom_point(aes(color = treatment, shape = species), size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_color_discrete(drop = FALSE) +
  annotate("text", x = Inf, y = Inf, label = eq_label(lm_tla_N1, "N1"),
           hjust = 1.1, vjust = 1.5, size = 3) +
  labs(title = "TLA per Instantaneous mg N", x = "Instantaneous mg N (N1)", y = "Normalized TLA") +
  theme_classic()

p_tla_N2_lin <- ggplot(tla_group, aes(x = N_2, y = tlanorm)) +
  geom_point(aes(color = treatment, shape = species), size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_color_discrete(drop = FALSE) +
  annotate("text", x = Inf, y = Inf, label = eq_label(lm_tla_N2, "N2"),
           hjust = 1.1, vjust = 1.5, size = 3) +
  labs(title = "TLA per Cumulative mg N", x = "Cumulative mg N (N2)", y = "Normalized TLA") +
  theme_classic()

p_tla_N3_lin <- ggplot(tla_group, aes(x = N_3, y = tlanorm)) +
  geom_point(aes(color = treatment, shape = species), size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_color_discrete(drop = FALSE) +
  annotate("text", x = Inf, y = Inf, label = eq_label(lm_tla_N3, "N3"),
           hjust = 1.1, vjust = 1.5, size = 3) +
  labs(title = "TLA per mg N / L * Week", x = "mg N / L * Week (N3)", y = "Normalized TLA") +
  theme_classic()

lin_panel <- (p_vcm_ppm_lin | p_vcm_N1_lin | p_vcm_N2_lin | p_vcm_N3_lin) /
  (p_tla_ppm_lin | p_tla_N1_lin | p_tla_N2_lin | p_tla_N3_lin) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

lin_panel
