#======================================================================================================
# ADMIN
#======================================================================================================
# Check that needed packages are installed:
want = c("tidyverse", "rio", "corrplot", 
         "psych", "lavaan", "semTools", 
         "texreg")
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
# load packages
junk <- lapply(want, library, character.only = TRUE)
rm(have, want, junk)

rm(list = ls())
options(scipen = 99)
setwd("~/Dropbox/conspiracyFX/IPSR_submission_2020")

#======================================================================================================
# FUNCTIONS
#======================================================================================================
tot_comp <- function(x){
  sum(!is.na(x))
}
sh_comp <- function(x){
  round(sum(!is.na(x))/length(x), 2)
}

#======================================================================================================
# DATA MANAGEMENT
#======================================================================================================
data <- import("./data/FV_LL_consp_IPSR.csv") %>% 
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

#======================================================================================================
# DESCRIPTIVES (section "Data and variables")
#======================================================================================================
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


#-------------------------------------------------------------------------------
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


#-------------------------------------------------------------------------------
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



#======================================================================================================
# LATENTS
#======================================================================================================
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

m_latents <- cfa(latents, data = data, missing = "ML")


#-------------------------------------------------------------------------------
# Extract factor scores
fscores <- lavPredict(m_latents, method = "regression")

data_lt <- cbind(
  data, data.frame(fscores)
)


#-------------------------------------------------------------------------------
# CORRELATIONS (Figure 2)


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
pdf(file = "./results/corplot_latents.pdf",
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


#======================================================================================================
# REGRESSION MODELS
#======================================================================================================

#-------------------------------------------------------------------------------
# With latent scores

# LEGITIMATE RADICAL ACTION
m.rad.1 <- lm(latradac ~ 
                latcon + lataggr + lattru +
                latrwa + latsdo + latintef + 
                ideoc + age + income + edu + female + nonwhite,
              data = data_lt)
summary(m.rad.1)

# ENDORSEMENT OF POLITICAL VIOLENCE
m.vio.1 <- lm(latvio ~ 
                latcon + lataggr + lattru +
                latrwa + latsdo + latintef + 
                ideoc + age + income + edu + female + nonwhite,
              data = data_lt)
summary(m.vio.1)

#-------------------------------------------------------------------------------
# With additive indexes

# LEGITIMATE RADICAL ACTION
m.rad.1b <- lm(ad_radac ~ 
                 ad_con + ad_aggr + ad_tru +
                 ad_rwa + ad_sdo + ad_intef + 
                 ideoc + age + income + edu + female + nonwhite,
               data = data_lt)
summary(m.rad.1b)

# ENDORSEMENT OF POLITICAL VIOLENCE
m.vio.1b <- lm(ad_vio ~ 
                 ad_con + ad_aggr + ad_tru +
                 ad_rwa + ad_sdo + ad_intef + 
                 ideoc + age + income + edu + female + nonwhite,
               data = data_lt)
summary(m.vio.1b)


#======================================================================================================
# TABLES A1, A2, A3 in the appendix
#======================================================================================================

#-------------------------------------------------------------------------------
# Loadings (for Table A2 in the appendix)
standardizedSolution(m_latents) %>% 
  filter(op == "=~") %>% 
  rename(
    Latent = lhs,
    Manifest = rhs,
    Coef = est.std,
    SE = se
  ) %>% 
  mutate_at(
    vars(Coef, SE, pvalue),
    ~sprintf("%.3f", round(., 3))
  ) %>% 
  mutate(
    Latent = recode(Latent,
                    "latradac" = "LRPA",
                    "latvio" = "JPV",
                    "latcon" = "Conspiracy belief",
                    "lataggr" = "Aggression",
                    "latrwa" = "RWA",
                    "latsdo" = "SDO",
                    "latintef" = "Internal Efficacy",
                    "lattru" = "Institutional Trust")
  ) %>% 
  dplyr::select(-op, -z) %>% 
  export("./results/loadings_1_all.xlsx")

# To get fit measures for Table A2 in the appendix
summary(m_latents, fit.measures = T)



#-------------------------------------------------------------------------------
# Regressions (for tables A1 and A3 in the appendix)

# Coefficient names for Table A1 in the appendix
coefs <- list(
  `(Intercept)` = "Intercept",
  latcon = "Conspiracy belief",
  lataggr = "Aggression",
  lattru = "Trust in Institutions",
  latrwa = "RWA",
  latsdo = "SDO",
  latintef = "Internal Efficacy",
  ideoc = "Ideology",
  age = "Age",
  income = "Income",
  edu = "Education",
  female = "Gender (F)",
  nonwhite = "Non-white"
)

# Produces Table A1 in the appendix
htmlreg(list(m.rad.1, m.vio.1), file = "./results/regressions_lat.doc",
        single.row = T, 
        custom.model.names = c("LRPA", "JPV"),
        custom.coef.map = coefs,
        caption = "Table 1: Regression models for LRPA and JPV",
        caption.above = T,
        inline.css = FALSE, doctype = TRUE, html.tag = TRUE, head.tag = TRUE, body.tag = TRUE)



# Coefficient names for Table A3 in the appendix
coefs <- list(
  `(Intercept)` = "Intercept",
  ad_con = "Conspiracy belief",
  ad_aggr = "Aggression",
  ad_tru = "Trust in Institutions",
  ad_rwa = "RWA",
  ad_sdo = "SDO",
  ad_intef = "Internal Efficacy",
  ideoc = "Ideology",
  age = "Age",
  income = "Income",
  edu = "Education",
  female = "Gender (F)",
  nonwhite = "Non-white"
)

# Produces Table A3 in the appendix
htmlreg(list(m.rad.1b, m.vio.1b), file = "./results/regressions_ad.doc",
        single.row = T, 
        custom.model.names = c("LRPA", "JPV"),
        custom.coef.map = coefs,
        caption = "Table 1: Regression models for LRPA and JPV",
        caption.above = T,
        inline.css = FALSE, doctype = TRUE, html.tag = TRUE, head.tag = TRUE, body.tag = TRUE)

#======================================================================================================
# Coefficient plots (Figure 3)
#======================================================================================================

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
  ggsave(., filename = "./results/coefplot_lat.pdf",
         device = "pdf", dpi = 300,
         width = 7, height = 4)


#======================================================================================================
# SEM (for Table A4 in the appendix)
#======================================================================================================

#-------------------------------------------------------------------------------
# LEGITIMATE RADICAL ACTION
reg_rad <- '
  # measurement model
    latradac =~ radact1 + radact2 + radact3 + radact5 + radact6
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
  # regressions
    latradac ~ latcon + lataggr + lattru + latrwa + latsdo + latintef + ideoc + age + income + edu + female + nonwhite
  # residual correlations
    latcon ~~ ideoc
    ideoc ~~ age
    ideoc ~~ income
    ideoc ~~ edu
    ideoc ~~ female
    income ~~ age
    income ~~ edu
    income ~~ female
    edu ~~ age
'
m.rad.2 <- sem(reg_rad, data = data, missing = "ML", fixed.x = F)



#-------------------------------------------------------------------------------
# ENDORSEMENT OF POLITICAL VIOLENCE
reg_vio <- '
  # measurement model
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
  # regressions
    latvio ~ latcon + lataggr + lattru + latrwa + latsdo + latintef + ideoc + age + income + edu + female + nonwhite
  # residual correlations
    latcon ~~ ideoc
    ideoc ~~ age
    ideoc ~~ income
    ideoc ~~ edu
    ideoc ~~ female
    income ~~ age
    income ~~ edu
    income ~~ female
    edu ~~ age
'
m.vio.2 <- sem(reg_vio, data = data, missing = "ML", fixed.x = F)



# Function to produce stars
stars <- function(x){
  ifelse(x < 0.001, "***", 
         ifelse(x < 0.01, "**",
                ifelse(x < 0.05, "*", "")))
}

# Table A4 in the appendix (coefficients)
standardizedSolution(m.rad.2) %>% 
  filter(op == "~") %>% 
  mutate(
    res = paste0(round(est.std, 2), 
                 " (", round(se, 2), ")",
                 stars(pvalue))
  ) %>% 
  dplyr::select(rhs, res) %>% 
  bind_cols(
    standardizedSolution(m.vio.2) %>% 
      filter(op == "~") %>% 
      mutate(
        res2 = paste0(round(est.std, 2), 
                      " (", round(se, 2), ")",
                      stars(pvalue))
      ) %>% 
      dplyr::select(res2)
  ) %>% 
  rename(
    Variable = rhs,
    LRPA = res,
    JPV = res2
  ) %>% 
  mutate(
    Variable = recode(Variable,
                      "latcon" = "Conspiracy belief",
                      "lataggr" = "Aggression",
                      "latrwa" = "RWA",
                      "latsdo" = "SDO",
                      "latintef" = "Internal Efficacy",
                      "lattru" = "Institutional Trust",
                      "ideoc" = "Ideology",
                      "age" = "Age",
                      "income" = "Income",
                      "edu" = "Education",
                      "female" = "Gender (F)",
                      "nonwhite" = "Non-white")
  ) %>% 
  export("./results/sem_results.xlsx")

# Run this to get fit measures (last 5 rows of Table A4)
summary(m.rad.2, fit.measures = T)
summary(m.vio.2, fit.measures = T)
