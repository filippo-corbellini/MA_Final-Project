# Multivariate Analysis Final Assignment

# Check if we have the needed packages installed
# Load the packages

want = c("tidyverse", "rio", "corrplot", 
         "psych", "lavaan", "semTools", 
         "texreg")
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }

junk <- lapply(want, library, character.only = TRUE)
rm(have, want, junk)

rm(list = ls())
options(scipen = 99)

# Set the working directory

setwd("C:/Users/Filippo/OneDrive - Università degli Studi di Milano/Desktop/MA_Final Project")

# Functions to check missingness in scales and identify respondents with too many missing answers.

tot_comp <- function(x){
  sum(!is.na(x))
}
sh_comp <- function(x){
  round(sum(!is.na(x))/length(x), 2)
}


# DATA MANAGEMENT

data <- import("./DATA/FV_LL_consp_IPSR.csv") %>% 
  filter(pilot == 0) %>% 
  mutate_all(~ifelse(. == -999, NA, .)) %>% 
  mutate(
    nonwhite = ifelse(race %in% 5, 0, 1),
    age_or = (2014 - year),
    age = (age_or - mean(age_or, na.rm = T)) / sd(age_or, na.rm = T),
    female = gender - 1,
    edu = ifelse(hsdipl %in% c(1, 2) | degree == 8, -1,
                 ifelse(degree == 1, 0, 1)),
    lib = ifelse(ideo %in% 1:3, 1, 0),
    con = ifelse(ideo %in% 5:7, 1, 0),
    strideo = abs(ideo - 4),
    ideoc = ideo - 4,
    race = ifelse(race %in% c(9:345), 9, race),
    race = recode(
      race,
      `1` = "Black",
      `2` = "Asian",
      `3` = "Native American",
      `4` = "Hispanic",
      `5` = "White",
      `9` = "Other",
      .default = NA_character_
    )
  ) %>% 
  mutate(   # Check share of missing items in the batteries
    sm_rad = apply(dplyr::select(., starts_with("radact")), 1, sh_comp),
    sm_vio = apply(dplyr::select(., starts_with("endvio")), 1, sh_comp),
    sm_con = apply(dplyr::select(., starts_with("cons")), 1, sh_comp),
    sm_agg = apply(dplyr::select(., starts_with("agg")), 1, sh_comp),
    sm_rwa = apply(dplyr::select(., starts_with("rwa")), 1, sh_comp),
    sm_sdo = apply(dplyr::select(., starts_with("sdo")), 1, sh_comp),
    sm_eff = apply(dplyr::select(., starts_with("intself")), 1, sh_comp),
    sm_tru = apply(dplyr::select(., starts_with("trust")), 1, sh_comp)
  ) %>% 
  mutate(   # Check if somebody has all missings in one battery
    all_mis_any = apply(dplyr::select(., starts_with("sm_")), 1, function(x) sum(x == 0))
  ) %>% 
  filter(   # Drop those who have all missings in at least one battery
    all_mis_any == 0
  ) %>% 
  dplyr::select(starts_with("radact"), starts_with("endvio"), cons1:cons15,
                starts_with("agg"), starts_with("rwa"), starts_with("sdo"), 
                starts_with("intself"), starts_with("trust"), 
                ideoc, lib, con, strideo, age_or, age, income, edu, eduyrs, female, nonwhite, race) %>% 
  mutate_at(      # Reverse negative items - RWA
    vars(paste0("rwa", c(4, 6, 8, 9, 11, 13, 15, 18))),
    ~(8 - .)
  ) %>% 
  mutate_at(      # Reverse negative items - SDO
    vars(paste0("sdo", c(9:16))),
    ~(8 - .)
  ) %>% 
  mutate_at(      # Reverse negative items - Aggression
    vars(paste0("agg", c(7, 18))),
    ~(8 - .)
  ) %>% 
  mutate(         # Additive indexes
    ad_radac = rowMeans(dplyr::select(., paste0("radact", c(1:3, 5, 6))), na.rm = T),
    ad_vio = rowMeans(dplyr::select(., paste0("endvio", c(1:4))), na.rm = T),
    ad_con = rowMeans(dplyr::select(., paste0("cons", c(1:15))), na.rm = T),
    ad_aggr = rowMeans(dplyr::select(., paste0("agg", c(1:9, 11:29))), na.rm = T),
    ad_tru = rowMeans(dplyr::select(., paste0("trust", c(1:10))), na.rm = T),
    ad_rwa = rowMeans(dplyr::select(., paste0("rwa", c(1:19))), na.rm = T),
    ad_sdo = rowMeans(dplyr::select(., paste0("sdo", c(1:16))), na.rm = T),
    ad_intef = rowMeans(dplyr::select(., paste0("intself", c(1:8))), na.rm = T)
  ) %>% 
  mutate_at(
    vars(starts_with("ad_")),
    ~(. - mean(., na.rm = T)) / sd(., na.rm = T)
  )

# Reversed items 
# RWA: 4, 6, 8, 9, 11, 13, 15, 18
# SDO: 9:16
# Aggression: 7, 18

# DESCRIPTIVES (section "Data and variables")

# Percentage of women
round(mean(data$female, na.rm = T)*100, 1)

# Age (min, max, mean, median)
summary(data$age_or)

# Education (1 = Less than 12 years, 2 = 13 years or more)
round(prop.table(table(data$eduyrs))*100, 0)

# Percentage of liberals
round(mean(data$lib, na.rm = T)*100, 1)

# Percentage of conservatives
round(mean(data$con, na.rm = T)*100, 1)

# Percentages of race groups
round(prop.table(table(data$race))*100, 1)


# Number of items shown in scales with planned missing (Footnote 9)
data %>% 
  dplyr::select(starts_with("radact"),starts_with("agg"), 
                starts_with("rwa"), starts_with("sdo"), 
                starts_with("intself")) %>% 
  mutate(
    nc_radact = apply(dplyr::select(., starts_with("radact")), 1, tot_comp),
    nc_agg = apply(dplyr::select(., starts_with("agg")), 1, tot_comp),
    nc_rwa = apply(dplyr::select(., starts_with("rwa")), 1, tot_comp),
    nc_sdo = apply(dplyr::select(., starts_with("sdo")), 1, tot_comp),
    nc_intself = apply(dplyr::select(., starts_with("intself")), 1, tot_comp)
  ) %>% 
  dplyr::select(starts_with("nc")) %>% 
  summarize_all(
    ~median(.)
  )


# Results for "Table 1: Reliability score of 8 multi-item batteries"
# Check "raw_alpha" or "std.alpha"

corFiml(dplyr::select(data, paste0("radact", c(1:3, 5:6)))) %>% 
  psych::alpha() %>% .$total

corFiml(dplyr::select(data, paste0("endvio", c(1:4)))) %>% 
  psych::alpha() %>% .$total

corFiml(dplyr::select(data, paste0("cons", c(1:15)))) %>% 
  psych::alpha() %>% .$total

corFiml(dplyr::select(data, paste0("agg", c(1:9, 11:29)))) %>% 
  psych::alpha() %>% .$total

corFiml(dplyr::select(data, paste0("trust", c(1:10)))) %>% 
  psych::alpha() %>% .$total

corFiml(dplyr::select(data, paste0("rwa", c(1:19)))) %>% 
  psych::alpha() %>% .$total

corFiml(dplyr::select(data, paste0("sdo", c(1:16)))) %>% 
  psych::alpha() %>% .$total

corFiml(dplyr::select(data, paste0("intself", c(1:8)))) %>% 
  psych::alpha() %>% .$total


# Latent factors loadings
# I suggest to load m_latents.rds (OUTPUT folder) with the final output, since it takes several hours to run.

latents <- '
    latradac =~ radact1 + radact2 + radact3 + radact5 + radact6
    latvio =~ endvio1 + endvio2 + endvio3 + endvio4
    latcon =~ cons1 + cons2 + cons3 + cons4 + cons5 + cons6 + cons7 + cons8 + 
              cons9 + cons10 + cons11 + cons12 + cons13 + cons14 + cons15
    lataggr =~ agg1 + agg2 + agg3 + agg4 + agg5 + agg6 + agg7 + agg8 + agg9 + 
               agg11 + agg12 + agg13 + agg14 + agg15 + agg16 + agg17 + agg18 + agg19 + 
               agg20 + agg21 + agg22 + agg23 + agg24 + agg25 + agg26 + agg27 + agg28 + agg29
    lattru =~ trust1 + trust2 + trust3 + trust4 + trust5 + trust6 + trust7 + trust8 + trust9 + trust10
    latrwa =~ rwa1 + rwa2 + rwa3 + rwa4 + rwa5 + rwa6 + rwa7 + rwa8 + rwa9 + rwa10 + rwa11 + rwa12 +
              rwa13 + rwa14 + rwa15 + rwa16 + rwa17 + rwa18 + rwa19
    latsdo =~ sdo1 + sdo2 + sdo3 + sdo4 + sdo5 + sdo6 + sdo7 + sdo8 + sdo9 + sdo10 + sdo11 + sdo12 +
              sdo13 + sdo14 + sdo15 + sdo16
    latintef =~ intself1 + intself2 + intself3 + intself4 + intself5 + intself6 + intself7 + intself8
'
# Dropping radact4 & agg10

m_latents <- lavaan::cfa(latents, data = data, missing = "ML")

# Saving and loading the results, so it does not need to be run again

saveRDS(m_latents, file = "m_latents.rds")

m_latents <- readRDS("m_latents.rds")

# Extract factor scores

fscores <- lavPredict(m_latents, method = "regression")

data_lt <- cbind(
  data, data.frame(fscores)
)

cor_full <- data_lt %>% 
  dplyr::select(starts_with("lat")) %>% 
  psych::corr.test()
cor_lat <- cor_full$r
p_lat <- cor_full$p

rownames(cor_lat) <- c("LRPA", "JPV", "Conspiracy belief", "Aggression", 
                       "Trust in Institutions", "RWA", "SDO", "Internal Efficacy")
colnames(cor_lat) <- c("LRPA", "JPV", "Conspiracy belief", "Aggression", 
                       "Trust in Institutions", "RWA", "SDO", "Internal Efficacy")


# CORRELATIONS (Figure 2a)

cor_full <- data_lt %>% 
  dplyr::select(starts_with("lat")) %>% 
  psych::corr.test()
cor_lat <- cor_full$r
p_lat <- cor_full$p

rownames(cor_lat) <- c("LRPA", "JPV", "Conspiracy belief", "Aggression", 
                       "Trust in Institutions", "RWA", "SDO", "Internal Efficacy")
colnames(cor_lat) <- c("LRPA", "JPV", "Conspiracy belief", "Aggression", 
                       "Trust in Institutions", "RWA", "SDO", "Internal Efficacy")

# Make correlation plot

mycol <- ifelse(c(p_lat < 0.05), "black", "white")
pdf(file = "./FIGS/corplot_latents.pdf",
    width = 6, height = 6)
corrplot::corrplot(
  cor_lat, method = "color",
  type = "lower", 
  addCoef.col = matrix(mycol, nrow = 8)[lower.tri(matrix(mycol, nrow = 8), diag = F)], 
  number.cex = 0.8,
  tl.col = "black", tl.srt = 45, 
  diag = F, cl.pos = "n", outline = T,
  p.mat = p_lat, sig.level = 0.05, insig = "blank",
  mar = c(0, 0, 0, 0)
)
dev.off()


# 1ST EXTENSION 
# Latent factors modeled separately with individual CFAs (Figure 2b)

# Helper function to estimate and extract factor scores safely

get_scores <- function(model_string, factor_name, data) {
  fit <- lavaan::cfa(model_string, data = data, missing = "ML")
  scores <- lavPredict(fit, method = "regression")
  scores[, factor_name]
}

# Run separate CFA models

fscores_latradac <- get_scores("latradac =~ radact1 + radact2 + radact3 + radact5 + radact6", "latradac", data)
fscores_latvio   <- get_scores("latvio =~ endvio1 + endvio2 + endvio3 + endvio4", "latvio", data)
fscores_latcon   <- get_scores("latcon =~ cons1 + cons2 + cons3 + cons4 + cons5 + cons6 + cons7 + cons8 +
                                cons9 + cons10 + cons11 + cons12 + cons13 + cons14 + cons15", "latcon", data)
fscores_lataggr  <- get_scores("lataggr =~ agg1 + agg2 + agg3 + agg4 + agg5 + agg6 + agg7 + agg8 + agg9 + 
                                agg11 + agg12 + agg13 + agg14 + agg15 + agg16 + agg17 + agg18 + agg19 + 
                                agg20 + agg21 + agg22 + agg23 + agg24 + agg25 + agg26 + agg27 + agg28 + agg29",
                               "lataggr", data)
fscores_lattru   <- get_scores("lattru =~ trust1 + trust2 + trust3 + trust4 + trust5 + trust6 + trust7 + trust8 + trust9 + trust10", 
                               "lattru", data)
fscores_latrwa   <- get_scores("latrwa =~ rwa1 + rwa2 + rwa3 + rwa4 + rwa5 + rwa6 + rwa7 + rwa8 + rwa9 + rwa10 + rwa11 + rwa12 +
                                rwa13 + rwa14 + rwa15 + rwa16 + rwa17 + rwa18 + rwa19", "latrwa", data)
fscores_latsdo   <- get_scores("latsdo =~ sdo1 + sdo2 + sdo3 + sdo4 + sdo5 + sdo6 + sdo7 + sdo8 + sdo9 + sdo10 + sdo11 + sdo12 +
                                sdo13 + sdo14 + sdo15 + sdo16", "latsdo", data)
fscores_latintef <- get_scores("latintef =~ intself1 + intself2 + intself3 + intself4 + intself5 + intself6 + intself7 + intself8", 
                               "latintef", data)


data_lt <- cbind(data,
                 latradac = fscores_latradac,
                 latvio = fscores_latvio,
                 latcon = fscores_latcon,
                 lataggr = fscores_lataggr,
                 lattru = fscores_lattru,
                 latrwa = fscores_latrwa,
                 latsdo = fscores_latsdo,
                 latintef = fscores_latintef)


cor_full <- data_lt %>% 
  dplyr::select(starts_with("lat")) %>% 
  psych::corr.test()
cor_lat <- cor_full$r
p_lat <- cor_full$p

rownames(cor_lat) <- c("LRPA", "JPV", "Conspiracy belief", "Aggression", 
                       "Trust in Institutions", "RWA", "SDO", "Internal Efficacy")
colnames(cor_lat) <- c("LRPA", "JPV", "Conspiracy belief", "Aggression", 
                       "Trust in Institutions", "RWA", "SDO", "Internal Efficacy")

# Make correlation plot

mycol <- ifelse(c(p_lat < 0.05), "black", "white")
pdf(file = "./FIGS/extension_corplot_latents.pdf",
    width = 6, height = 6)
corrplot::corrplot(
  cor_lat, method = "color",
  type = "lower", 
  addCoef.col = matrix(mycol, nrow = 8)[lower.tri(matrix(mycol, nrow = 8), diag = F)], 
  number.cex = 0.8,
  tl.col = "black", tl.srt = 45, 
  diag = F, cl.pos = "n", outline = T,
  p.mat = p_lat, sig.level = 0.05, insig = "blank",
  mar = c(0, 0, 0, 0)
)
dev.off()


# REGRESSION MODELS

# LEGITIMATE RADICAL ACTION

m.rad.1 <- lm(latradac ~ 
                latcon + lataggr + lattru +
                latrwa + latsdo + latintef + 
                ideoc + age + income + edu + female + nonwhite,
              data = data_lt)
summary(m.rad.1)

stargazer::stargazer(m.rad.1, type = 'text')

# ENDORSEMENT OF POLITICAL VIOLENCE

m.vio.1 <- lm(latvio ~ 
                latcon + lataggr + lattru +
                latrwa + latsdo + latintef + 
                ideoc + age + income + edu + female + nonwhite,
              data = data_lt)
summary(m.vio.1)

stargazer::stargazer(m.vio.1, type = 'text')


# With additive indexes

# LEGITIMATE RADICAL ACTION

m.rad.1b <- lm(ad_radac ~ 
                 ad_con + ad_aggr + ad_tru +
                 ad_rwa + ad_sdo + ad_intef + 
                 ideoc + age + income + edu + female + nonwhite,
               data = data_lt)
summary(m.rad.1b)

stargazer::stargazer(m.rad.1b, type = 'text')

# ENDORSEMENT OF POLITICAL VIOLENCE

m.vio.1b <- lm(ad_vio ~ 
                 ad_con + ad_aggr + ad_tru +
                 ad_rwa + ad_sdo + ad_intef + 
                 ideoc + age + income + edu + female + nonwhite,
               data = data_lt)
summary(m.vio.1b)

stargazer::stargazer(m.vio.1b, type = 'text')

# Coefficient plots (Figure 3)

coefs <- c("Intercept", "Conspiracy belief", "Aggression",
           "Trust in Institutions", "RWA", "SDO", "Internal Efficacy",
           "Ideology (Lib-Cons)", "Age", "Income", "Education", "Gender (F)", "Non-white")

(data.frame(summary(m.rad.1)$coefficients[, 1:2]) %>% 
    mutate(
      mod = "LRPA"
    ) %>% 
    bind_rows(
      data.frame(summary(m.vio.1)$coefficients[, 1:2]) %>% 
        mutate(
          mod = "JPV"
        )
    ) %>% 
    mutate(
      var = rep(unlist(coefs), 2),
      var = factor(var, levels = rev(unlist(coefs))),
      mod = factor(mod, levels = c("LRPA", "JPV"))
    ) %>% 
    rename(
      b = Estimate,
      se = `Std..Error`
    ) %>% 
    filter(
      var != "Intercept"
    ) %>% 
    ggplot(aes(x = var, y = b)) +
    geom_hline(aes(yintercept = 0), 
               linetype = 3) +
    geom_pointrange(
      aes(ymin = b - 1.96*se, ymax = b+1.96*se),
      shape = 22, fill = "white"
    ) +
    facet_wrap(~mod, ncol = 2) +
    coord_flip() +
    ylab("Coefficient (95% CI)") + xlab("") +
    theme_bw()) %>% 
  ggsave(., filename = "./FIGS/coefplot_lat.pdf",
         device = "pdf", dpi = 300,
         width = 7, height = 4)


# 2ND Extension
# Whether the relationship between conspiracy belief and support for political violence is conditioned by ideological extremism. 

# Define strict ideological extremism

data$extreme_strict <- ifelse(data$ideo %in% c(1, 7), 1, 0)

# Create interaction term

data$cons_x_extreme_strict <- data$ad_con * data$extreme_strict

# LRPA model

model_lrpa_ext <- lm(ad_radac ~ ad_con + extreme_strict + cons_x_extreme_strict +
                       ad_aggr + ad_tru + ad_rwa + ad_sdo + ad_intef +
                       ideoc + age + income + edu + female + nonwhite,
                     data = data)
summary(model_lrpa_ext)

# JPV model

model_jpv_ext <- lm(ad_vio ~ ad_con + extreme_strict + cons_x_extreme_strict +
                      ad_aggr + ad_tru + ad_rwa + ad_sdo + ad_intef +
                      ideoc + age + income + edu + female + nonwhite,
                    data = data)
summary(model_jpv_ext)

# Regression table (Table 2)

stargazer::stargazer(model_lrpa_ext, model_jpv_ext, type = 'text')
