# Pckages----
install.packages("caret")

library(ggpubr)
library(cowplot)
library(readxl)
library(tidyverse)
library(gghighlight)
library(ggrepel)
library(ggfortify)
library(gridExtra)
library(kableExtra)
library(patchwork)
library(ggridges)
library(ggExtra)

# Data import and wrangling----

df_sloss <- read_excel("FieldCompass/AquaRiskIndex/Systematic review 1/Conservation practices/Data/Calibset_new.xlsx",
                     sheet = "All") %>% 
  as_tibble() %>% 
  rename(sloss = Soil_loss_kg_ha_yr, hsg = Hydrologic_Soil_Group, loc = Location_ID, constill = Conservation_tillage) %>% 
  select(loc, hsg, constill, sloss, sloss_pred, sloss_pred2) %>%
  mutate(sloss = as.numeric(sloss)) %>%
  filter(constill == "NO") %>%
  slice(1:80)


sloss_test <- read_excel("FieldCompass/AquaRiskIndex/Systematic review 1/Conservation practices/Data/Calibset_new.xlsx",
                         sheet = "sloss_test") %>% 
  as_tibble() %>% 
  select(sloss, sloss_pred, model) %>%
  view()

sloss_train <- read_excel("FieldCompass/AquaRiskIndex/Systematic review 1/Conservation practices/Data/Calibset_new.xlsx",
                          sheet = "sloss_train") %>% 
  as_tibble() %>% 
  glimpse()

srun_train <- read_excel("FieldCompass/AquaRiskIndex/Systematic review 1/Conservation practices/Data/Calibset_new.xlsx",
                         sheet = "srun_train") %>% 
  as_tibble() %>%
  as.numeric(srun_pred)
  glimpse()

srun_test <- read_excel("FieldCompass/AquaRiskIndex/Systematic review 1/Conservation practices/Data/Calibset_new.xlsx",
                         sheet = "Literture data") %>% 
  as_tibble() %>%
  mutate_at("srun_pred2", as.numeric) %>%
  select(Precipitation_mm_yr, srun_pred2, Location_ID) %>%
  view()

nload_test <- read_excel("~/FieldCompass/AquaRiskIndex/Processes and models/SWI model description/Calibset_new.xlsx",
                         sheet = "nload_test")

  
df_ero <- read_excel("FieldCompass/AquaRiskIndex/Systematic review 1/Conservation practices/Data/Erosivity.xlsx",
                       na = "") %>% 
  as_tibble() %>% 
  na.omit() %>% 
  mutate(Rainfall.in = Rainfall.in * 25.4, Runoff = Runoff * 25.4) %>% 
  rename(Rainfall.mm = Rainfall.in, Runoff.mm = Runoff) %>% 
  glimpse()

df_usle <- read_csv("USLE_params.csv", na = "") %>%
  as_tibble()
 
df_runoff <- read_excel("FieldCompass/AquaRiskIndex/Systematic review 1/Conservation practices/Data/Runoff_data_STEPL.xlsx") %>% 
  as_tibble()

df_weather <- read_excel("Rain_data_STEPL.xlsx") %>% 
  as_tibble() %>% 
  glimpse()


df_srun_test <- srun_test %>% 
  select(Precipitation_mm_yr,srun_pred2, Location_ID) %>%
  group_by(Location_ID) %>%
  summarise(Precipitation_mm_yr = mean(Precipitation_mm_yr),
            srun_pred2 = mean(srun_pred2)) %>% 
  column_to_rownames(var = "Location_ID")


## Scenarios- precipitation----
df.prec.rc1 <-read_excel("scenarios.xlsx") %>% 
  filter(Land_use == "Row_crop", Scenario == 1)

df.prec.sg1 <-read_excel("scenarios.xlsx") %>% 
  filter(Land_use == "Small_grain", Scenario == 1)

df.prec.rc2 <-read_excel("scenarios.xlsx") %>% 
  filter(Land_use == "Row_crop", Scenario == 2)

df.prec.sg2 <-read_excel("scenarios.xlsx") %>% 
  filter(Land_use == "Small_grain", Scenario == 2)

## Scenarios- napp----

df.napp.rc1 <-read_excel("scenarios.xlsx") %>% 
  filter(Land_use == "Row_crop" & N_applied_kg_N_ha_yr %in% c("75",
                                                              "150",
                                                              "225",
                                                              "300"),
         Scenario == 1)

df.napp.sg1 <-read_excel("scenarios.xlsx") %>% 
  filter(Land_use == "Small_grain" & N_applied_kg_N_ha_yr %in% c("75",
                                                              "150",
                                                              "225",
                                                              "300"),
         Scenario == 1)

df.napp.rc2 <-read_excel("scenarios.xlsx") %>% 
  filter(Land_use == "Row_crop" & N_applied_kg_N_ha_yr %in% c("75",
                                                              "150",
                                                              "225",
                                                              "300"),
         Scenario == 2)

df.napp.sg2 <-read_excel("scenarios.xlsx") %>% 
  filter(Land_use == "Small_grain" & N_applied_kg_N_ha_yr %in% c("75",
                                                                 "150",
                                                                 "225",
                                                                 "300"),
         Scenario == 2)

# Pedo climatic Scenarios----

sc_pedo_all <- read_excel("~/FieldCompass/AquaRiskIndex/Processes and models/SWI model description/scenarios_pedoclima.xlsx",
                          sheet = "SC_gwf") %>%  
  mutate(Scenario = as.factor(Scenario)) %>%
  mutate(Scenario = recode(Scenario,
                           "1" = "High runoff",
                           "2" = "Low runoff")) %>%
  rename(Pedo_climatic_scenario = Scenario, ngwf = Grey_Water_Footprint_m3_ha_yr) %>% 
  view()

unique(sc_pedo_all$Treatment)


# "Fallow"  "Row_crop"  "Small_grain"
# 
# "None"  "Straight_row"  "No_Till" "Straight_row_and_Crop_residue"

## SC0----

sc_pedo0 <- sc_pedo_all %>%
  filter(Land_use == "Row_crop",
         Treatment == "Straight_row_and_Crop_residue")

plt0.1 <- ggplot(sc_pedo0,
                 aes(x = ngwf,
                     y = ..density..,
                     fill = Pedo_climatic_scenario,
                     colour = Pedo_climatic_scenario)) +
  geom_density(alpha = 1) +
  expand_limits(x = c(0, 100),
                y = c(0, 0.0005)) +
  scale_fill_manual(values = c("#ff9999", "#99ccff")) +
  # expand_limits(x = c(0, 100)) +
  # scale_x_continuous(limits = c(0, 100)) +
  labs(x = expression(atop("nGWF"~(m^3/(ha%*%yr)))),
       title = "Row crop &\nStraight rows with cover crop") +
  # geom_vline(xintercept = nlims,
  #            linetype = "dashed",
  #            colour = c("green4", "green3", "greenyellow", "orange", "red3"),
  #            size = 1.5) +
  theme_cowplot(font_size = 25) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5, size = 24))

plt0.1


plt0.2 <- ggplot(sc_pedo0,
                 aes(x = Soil_eros_kg_ha_yr,
                     y = ..density..,
                     fill = Pedo_climatic_scenario,
                     colour = Pedo_climatic_scenario)) +
  geom_density(alpha = 0.2) +
  # expand_limits(y = c(0, 0.003)) +
  # scale_y_continuous(breaks = seq(0, 0.003, by = 0.001)) +
  labs(y = "",
       x = expression(atop("Soil erosion rate"~(kg/(ha%*%yr))))) +
  theme_cowplot(font_size = 15)

plt0.3 <- ggplot(sc_pedo0,
                 aes(x = Surface_runoff_mm_yr,
                     y = ..density..,
                     fill = Pedo_climatic_scenario,
                     colour = Pedo_climatic_scenario)) +
  geom_density(alpha = 0.2) +
  # expand_limits(y = c(0, 0.003)) +
  # scale_y_continuous(breaks = seq(0, 0.003, by = 0.001)) +
  labs(y = "",
       x = expression(atop("Surface runoff"~(mm/yr)))) +
  theme_cowplot(font_size = 15)

plt0 <- (plt0.1 + plt0.2 + plt0.3)

plt0 + plot_annotation(tag_levels = "A",
                       title = "Fallow") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

## SC1----

sc_pedo1 <- sc_pedo_all %>% 
  filter(Land_use == "Row_crop",
         Treatment == "Straight_row")

plt1.1 <- ggplot(sc_pedo1,
               aes(x = N_load_kg_ha_yr,
                   y = ..density..,
                   fill = Pedo_climatic_scenario,
                   colour = Pedo_climatic_scenario)) +
  geom_density(alpha = 1) +
  scale_fill_manual(values = c("#ff9999", "#99ccff")) +
  geom_vline(xintercept = nlims,
             linetype = "dashed",
             colour = c("green4", "green3", "greenyellow", "orange", "red3"),
             size = 1.5) +
  expand_limits(x = c(0, 100)) +
  # scale_y_continuous(breaks = seq(0, 0.003, by = 0.001)) +
  labs(x = expression(atop("Nitrogen load"~(kg/(ha%*%yr)))),
       title = "Row crop &\nconventional tillage") +
  # annotate("text",
  #          x = 60,
  #          y = 0.075,
  #          label = "'C-factor' %~% italic(TriangDist(a == 0.5, c == 0.565, b == 0.62))",
  #          parse = TRUE) +
  theme_cowplot(font_size = 25) +
  theme(legend.position = "none")


plt1.1

plt1.2 <- ggplot(sc_pedo1,
               aes(x = Soil_eros_kg_ha_yr,
                   y = ..density..,
                   fill = Pedo_climatic_scenario,
                   colour = Pedo_climatic_scenario)) +
  geom_density(alpha = 0.2) +
  # expand_limits(y = c(0, 0.003)) +
  # scale_y_continuous(breaks = seq(0, 0.003, by = 0.001)) +
  labs(y = "",
       x = expression(atop("Soil erosion rate"~(kg/(ha%*%yr))))) +
  theme_cowplot(font_size = 15)

plt1.3 <- ggplot(sc_pedo1,
                 aes(x = Surface_runoff_mm_yr,
                     y = ..density..,
                     fill = Pedo_climatic_scenario,
                     colour = Pedo_climatic_scenario)) +
  geom_density(alpha = 0.2) +
  # expand_limits(y = c(0, 0.003)) +
  # scale_y_continuous(breaks = seq(0, 0.003, by = 0.001)) +
  labs(y = "",
       x = expression(atop("Surface runoff"~(mm/yr)))) +
  theme_cowplot(font_size = 15)

plt1 <- (plt1.1 + plt1.2 + plt1.3)

plt1 + plot_annotation(tag_levels = "A",
                       title = "Row crop & Straight rows") +
                      plot_layout(guides = "collect") &
                      theme(legend.position = "bottom",
                            plot.title = element_text(face = "bold")) 
## SC2----

sc_pedo2 <- sc_pedo_all %>% 
  filter(Land_use == "Row_crop",
         Treatment == "No_Till")

plt2.1 <- ggplot(sc_pedo2,
                 aes(x = N_load_kg_ha_yr,
                     y = ..density..,
                     fill = Pedo_climatic_scenario,
                     colour = Pedo_climatic_scenario)) +
  geom_density(alpha = 1) +
  scale_fill_manual(values = c("#ff9999", "#99ccff")) +
  geom_vline(xintercept = nlims,
             linetype = "dashed",
             colour = c("green4", "green3", "greenyellow", "orange", "red3"),
             size = 1.5) +
  expand_limits(x = c(0, 100)) +
  # scale_y_continuous(breaks = seq(0, 0.003, by = 0.001)) +
  labs(x = expression(atop("Nitrogen load"~(kg/(ha%*%yr)))),
       title = "Row crop &\nNo-till") +
  # annotate("text",
  #          x = 60,
  #          y = 0.125,
  #          label = "'C-factor' %~% italic(TriangDist(a == 0.17, c == 0.18, b == 0.28))",
  #          parse = TRUE) +
  theme_cowplot(font_size = 25) +
  theme(legend.position = "none")

plt2.1

plt2.2 <- ggplot(sc_pedo2,
                 aes(x = Soil_eros_kg_ha_yr,
                     y = ..density..,
                     fill = Pedo_climatic_scenario,
                     colour = Pedo_climatic_scenario)) +
  geom_density(alpha = 0.2) +
  # expand_limits(y = c(0, 0.003)) +
  # scale_y_continuous(breaks = seq(0, 0.003, by = 0.001)) +
  labs(y = "",
       x = expression(atop("Soil erosion rate"~(kg/(ha%*%yr))))) +
  theme_cowplot(font_size = 15)

plt2.3 <- ggplot(sc_pedo2,
                 aes(x = Surface_runoff_mm_yr,
                     y = ..density..,
                     fill = Pedo_climatic_scenario,
                     colour = Pedo_climatic_scenario)) +
  geom_density(alpha = 0.2) +
  # expand_limits(y = c(0, 0.003)) +
  # scale_y_continuous(breaks = seq(0, 0.003, by = 0.001)) +
  labs(y = "",
       x = expression(atop("Surface runoff"~(mm/yr)))) +
  theme_cowplot(font_size = 15)

plt2 <- (plt2.1 + plt2.2 + plt2.3)

plt2 + plot_annotation(tag_levels = "A",
                       title = "Row crop & No till") +
       plot_layout(guides = "collect") &
       theme(legend.position = "bottom",
             plot.title = element_text(face = "bold"))
## SC3----

sc_pedo3 <- sc_pedo_all %>% 
  filter(Land_use == "Small_grain",
         Treatment == "Straight_row")

plt3.1 <- ggplot(sc_pedo3,
                 aes(x = N_load_kg_ha_yr,
                     y = ..density..,
                     fill = Pedo_climatic_scenario,
                     colour = Pedo_climatic_scenario)) +
  geom_density(alpha = 1) +
  scale_fill_manual(values = c("#ff9999", "#99ccff")) +
  geom_vline(xintercept = nlims,
             linetype = "dashed",
             colour = c("green4", "green3", "greenyellow", "orange", "red3"),
             size = 1.5) +
  expand_limits(x = c(0, 100)) +
  # scale_y_continuous(breaks = seq(0, 0.003, by = 0.001)) +
  labs(x = expression(atop("Nitrogen load"~(kg/(ha%*%yr)))),
       title = "Small grain &\nConventional tillage") +
  # annotate("text",
  #          x = 60,
  #          y = 0.075,
  #          label = "'C-factor' %~% italic(TriangDist(a == 0.13, c == 0.21, b == 0.27))",
  #          parse = TRUE) +
  theme_cowplot(font_size = 25) +
  theme(legend.position = "none")

plt3.1

plt3.2 <- ggplot(sc_pedo3,
                 aes(x = Soil_eros_kg_ha_yr,
                     y = ..density..,
                     fill = Pedo_climatic_scenario,
                     colour = Pedo_climatic_scenario)) +
  geom_density(alpha = 0.2) +
  # expand_limits(y = c(0, 0.003)) +
  # scale_y_continuous(breaks = seq(0, 0.003, by = 0.001)) +
  labs(y = "",
       x = expression(atop("Soil erosion rate"~(kg/(ha%*%yr))))) +
  theme_cowplot(font_size = 15)

plt3.3 <- ggplot(sc_pedo3,
                 aes(x = Surface_runoff_mm_yr,
                     y = ..density..,
                     fill = Pedo_climatic_scenario,
                     colour = Pedo_climatic_scenario)) +
  geom_density(alpha = 0.2) +
  # expand_limits(y = c(0, 0.003)) +
  # scale_y_continuous(breaks = seq(0, 0.003, by = 0.001)) +
  labs(y = "",
       x = expression(atop("Surface runoff"~(mm/yr)))) +
  theme_cowplot(font_size = 15)

plt3 <- (plt3.1 + plt3.2 + plt3.3)

plt3 + plot_annotation(tag_levels = "A",
                       title = "Small grain & Straight rows") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

## SC4----

sc_pedo4 <- sc_pedo_all %>% 
  filter(Land_use == "Small_grain",
         Treatment == "No_Till")

plt4.1 <- ggplot(sc_pedo4,
                 aes(x = N_load_kg_ha_yr,
                     y = ..density..,
                     fill = Pedo_climatic_scenario,
                     colour = Pedo_climatic_scenario)) +
  geom_density(alpha = 1) +
  scale_fill_manual(values = c("#ff9999", "#99ccff")) +
  geom_vline(xintercept = nlims,
             linetype = "dashed",
             colour = c("green4", "green3", "greenyellow", "orange", "red3"),
             size = 1.5) +
  expand_limits(x = c(0, 100)) +
  # scale_y_continuous(breaks = seq(0, 0.003, by = 0.001)) +
  labs(x = expression(atop("Nitrogen load"~(kg/(ha%*%yr)))),
       title = "Small grain &\nNo-till") +
  # annotate("text",
  #          x = 60,
  #          y = 0.1,
  #          label = "'C-factor' %~% italic(TriangDist(a == 0.01, c == 0.03, b == 0.07))",
  #          parse = TRUE) +
  theme_cowplot(font_size = 25) +
  theme(legend.position = "none")

plt4.1

plt4.2 <- ggplot(sc_pedo4,
                 aes(x = Soil_eros_kg_ha_yr,
                     y = ..density..,
                     fill = Pedo_climatic_scenario,
                     colour = Pedo_climatic_scenario)) +
  geom_density(alpha = 0.2) +
  # expand_limits(y = c(0, 0.003)) +
  # scale_y_continuous(breaks = seq(0, 0.003, by = 0.001)) +
  labs(y = "",
       x = expression(atop("Soil erosion rate"~(kg/(ha%*%yr))))) +
  theme_cowplot(font_size = 15)

plt4.3 <- ggplot(sc_pedo4,
                 aes(x = Surface_runoff_mm_yr,
                     y = ..density..,
                     fill = Pedo_climatic_scenario,
                     colour = Pedo_climatic_scenario)) +
  geom_density(alpha = 0.2) +
  # expand_limits(y = c(0, 0.003)) +
  # scale_y_continuous(breaks = seq(0, 0.003, by = 0.001)) +
  labs(y = "",
       x = expression(atop("Surface runoff"~(mm/yr)))) +
  theme_cowplot(font_size = 15)

plt4 <- (plt4.1 + plt4.2 + plt4.3)

plt4 + plot_annotation(tag_levels = "A",
                       title = "Small grain & No till") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

plt_nitro <- (plt1.1 + plt2.1 + plt3.1 + plt4.1)

# Probabity calculations and graphics----


dat.scs <-  sc_pedo_all %>%
  unite("Land_use2", c("Land_use", "Treatment"), sep = " & ")
  # filter(Pedo_climatic_scenario == "High runoff potential") 

stat.scs <-  sc_pedo_all %>%
  unite("Land_use2", c("Land_use", "Treatment"), sep = " & ") %>% 
  filter(Pedo_climatic_scenario == "High runoff potential") %>% 
  group_by(Land_use2) %>% 
  select(Grey_Water_Footprint_m3_ha_yr, Land_use2) %>% 
  summarise(mean = mean(Grey_Water_Footprint_m3_ha_yr),
            med = median(Grey_Water_Footprint_m3_ha_yr),
            min = min(Grey_Water_Footprint_m3_ha_yr),
            max = max(Grey_Water_Footprint_m3_ha_yr),
            sd = sd(Grey_Water_Footprint_m3_ha_yr),
            n = n(),
            se = mean_se(Grey_Water_Footprint_m3_ha_yr),
            uci = se * qt(0.975, n-1))
view(stat.scs)

write.csv(stat.scs, "~/FieldCompass/AquaRiskIndex/Processes and models/SWI model description/stat_export.csv")

# Probability of N loads at various N critical thresholds

unique(dat.scs$Land_use2)

hist.nload <- sc_pedo_all %>%
   unite("Land_use2",
         c("Land_use", "Treatment"),
         sep = " & ") %>% 
   filter(Pedo_climatic_scenario == "High runoff potential" &
          Land_use2 == "Fallow & None") %>% 
   select(N_load_kg_ha_yr)


dens.nload_2.5 <- density(hist.nload$N_load_kg_ha_yr,
                          from = 0,
                          to = 2.5,
                          adjust = 1)

dens.nload_5 <- density(hist.nload$N_load_kg_ha_yr,
                          from = 2.5,
                          to = 5,
                          adjust = 1)

dens.nload_10 <- density(hist.nload$N_load_kg_ha_yr,
                        from = 5,
                        to = 10,
                        adjust = 1)

dens.nload_15 <- density(hist.nload$N_load_kg_ha_yr,
                         from = 10,
                         to = 15,
                         adjust = 1)

dens.nload_20 <- density(hist.nload$N_load_kg_ha_yr,
                         from = 15,
                         to = 20,
                         adjust = 1)
dens.nload_80 <- density(hist.nload$N_load_kg_ha_yr,
                            from = 20,
                            to = 85,
                            adjust = 1)


prob.nload_2.5 <- integrate(approxfun(dens.nload_2.5),
          lower = 0,
          upper = 2.5)
prob.nload_5 <- integrate(approxfun(dens.nload_5),
          lower = 2.5,
          upper = 5)
prob.nload_10 <- integrate(approxfun(dens.nload_10),
          lower = 5,
          upper = 10)
prob.nload_15 <-integrate(approxfun(dens.nload_15),
          lower = 10,
          upper = 15)
prob.nload_20 <-integrate(approxfun(dens.nload_20),
          lower = 15,
          upper = 20)
prob.nload_80 <- integrate(approxfun(dens.nload_80),
          lower = 20,
          upper = 85)


Ncrits = c("2.5", "5", "10", "15", "20", ">20")
probs = c(prob.nload_2.5$value, prob.nload_5$value, prob.nload_10$value,
          prob.nload_15$value, prob.nload_20$value, prob.nload_80$value)

# Probability tables, each for the land use and runoff potential, 14 altogether   
prob.tbl2.1 <- data.frame(Ncrits, probs)

prob.tbl1.1
prob.tbl2.1

## Density ridges graph with N critical thresholds----

nlims <- c(2.5, 5, 10, 15, 20)
  
plt.scs <-  ggplot(dat.scs, aes(x = Grey_Water_Footprint_m3_ha_yr,
                                y = Land_use2,
                                colour = Pedo_climatic_scenario,
                                fill = Pedo_climatic_scenario)) +
  geom_density_ridges(scale = 0.95,
                      rel_min_height = 0.0005) +
  scale_x_continuous(limits = c(0, 25000),
                     breaks = seq(0, 25000, by = 5000)) +
  scale_y_discrete(labels = c("Small_grain & Straight_row" = "Small grain & Straight row",
                              "Small_grain & No_Till" = "Small grain & No Till",
                              "Small_grain & Straight_row_and_Crop_residue" = "Small grain & Straight row and Crop residue",
                              "Row_crop & Straight_row" = "Row crop & Straight row",
                              "Row_crop & No_Till" = "Row crop & No Till",
                              "Row_crop & Straight_row_and_Crop_residue" = "Row crop & Straight row and Crop residue"),
                   expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("#D55E0050", "#0072B250"),
                    labels = c("High runoff", "Low runoff"),
                    name = "Pedo-climatic scenario") +
  scale_color_manual(values = c("#D55E0050", "#0072B250"),
                    labels = c("High runoff", "Low runoff"),
                     name = "Pedo-climatic scenario") +
  labs(title = "",
       x = expression(atop("nGWF"~(m^3/(ha%*%yr)))),
       y = "") +
  # geom_vline(xintercept = nlims,
  #            linetype = "dashed",
  #            colour = ncols) +
  # annotate("text",
  #          x = 73,
  #          y = 5,
  #          size = 2.2,
  #          parse = TRUE,
  #          label = expression(atop("Critical N surface runoff ", ~kg/(ha %*% yr)))) +
  # annotate("segment",
  #          x = 68:68,
  #          xend = 71:71,
  #          y = c(4.6, 4.45, 4.3, 4.15, 4.0),
  #          yend = c(4.6, 4.45, 4.3, 4.15, 4.0),
  #          colour = c("green4", "green3", "greenyellow", "orange", "red3"),
  #          linetype = "dashed") +
  # annotate("text",
  #          x = 75,
  #          y = c(4.6, 4.45, 4.3, 4.15, 4.0),
  #          size = 2.2,
  #          label = c("2.5", "5", "10", "15", ">20" )) +
  theme_cowplot(font_size = 8) +
  theme(legend.position = "right")

plt.scs

ggsave(plot = plt.scs, "pedo_scenarios_gwf.png", width = 15, height = 15, units = c("cm"), dpi = 300)


blank <-  ggplot() +
  geom_blank() +
annotate("text",
         x = 2.5,
         y = 6.5,
         size = 8,
         parse = TRUE,
         label = expression(atop("Critical N surface runoff ", ~kg/(ha %*% yr)))) +
annotate("segment",
         x = 1:1,
         xend = 3.5:3.5,
         y = c(1, 2, 3, 4, 5),
         yend = c(1, 2, 3, 4, 5),
         colour = c("green4", "green3", "greenyellow", "orange", "red3"),
         linetype = "dashed",
         size = 4) +
annotate("text",
         x = 4,
         y = c(1, 2, 3, 4, 5),
         size = 8,
         label = c("2.5", "5", "10", "15", ">20" )) +
  labs(x = "",
       y = "") +
  expand_limits(x = c(0, 10),
                y = c(0, 12)) +
  theme_cowplot(font_size = 15)


blank

### nload----

nload_plot <- ggplot(nload_test, aes(nload_stepl, nload_bn)) +
  geom_point(size = 2.5,
             alpha = 0.75,
             colour = "#f5f5f5",
             shape = 21,
             fill = "#0091b0") +
  # geom_pointrange(aes(ymin = srun_pred - srun_pred_sd, ymax = srun_pred + srun_pred_sd), width = 0.25) +
  # geom_text_repel(size = 2, max.overlaps = Inf) +
  # geom_smooth(method = lm) +
  # stat_cor(aes(label = paste(..rr.label..)), label.x.npc = 0.4, label.y.npc = 0.98, size = 4) +
  # geom_abline(intercept = 0, slope = 1, size = 0.7) +
  # geom_abline(slope = 1, intercept = 27, linetype = "dashed") +
  # geom_abline(slope = 1, intercept = -27, linetype = "dashed") +
  geom_hline(yintercept = 20, linetype = "dashed", colour = "magenta", size = 0.75) +
  geom_vline(xintercept = 20, linetype = "dashed", colour = "magenta", size = 0.75) +
  geom_hline(yintercept = 100, linetype = "dashed", colour = "gold3", size = 0.75) +
  geom_vline(xintercept = 100, linetype = "dashed", colour = "gold3", size = 0.75) +
  labs(x = expression("Simulated (STEPL) nitrogen load"~(kg/(ha%*%yr))),
       y = expression("Simulated (BN) nitrogen load"~(kg/(ha%*%yr)))) +
  xlim(0, 200) +
  ylim(0, 200) +
  # annotate("text", x = 15, y = 150, label = "N = 3043", size = 3) +
  # annotate("text", x = 150, y = 165, label = "1:1 line", size = 3) +
  annotate("text",
           x = 60, y = 195,
           label = expression(atop("Cutoff: 20"~kg/(ha%*%yr))),
           size = 3.5,
           colour = "magenta") +
  annotate("text",
           x = 140, y = 195,
           label = expression(atop("Cutoff: 100"~kg/(ha%*%yr))),
           size = 3.5,
           colour = "gold3") +
  # annotate("text", x = 150, y = 165, label = "1:1 line", size = 3) +
  # expand_limits(x = c(0, 200), y = c(0, 200)) +
  theme_pubr(base_size = 15)

nload_plot

ggsave("nload.svg",
       plot = nload_plot,
       dpi = 600,
       units = "cm",
       scale = 2)

nload_plot_marg <- ggMarginal(nload_plot,
                          type = "histogram",
                          fill = "#0091b0",
                          xparams = list(bins = 50),
                          yparams = list(bins = 50))


### Model performance----

nload.ref100 <- nload_test %>% 
  select(nload_stepl) %>% 
  mutate(nload.ref.n = factor(ifelse(nload_stepl >= 100, 1,0)))

nload.pred100 <- nload_test %>% 
  select(nload_bn) %>% 
  mutate(nload.pred.n = factor(ifelse(nload_bn >= 100, 1,0)))

cm20 <- confusionMatrix(data = nload.pred20$nload.pred.n, reference = nload.ref20$nload.ref.n)
cm100 <- confusionMatrix(data = nload.pred100$nload.pred.n, reference = nload.ref100$nload.ref.n)

cm100

dt.cm20 <- data.frame(cm20$table)
dt.cm100 <- data.frame(cm100$table)

dt.cm100 %>% 
  kbl()


tru.p <- 7/3060*100
tru.n <- 651/3060*100

fal.p <- 65/3060*100
fal.n <- 33/3060*100


(7+2952)/3060

## srun----
srun_plot <- ggplot(df_srun_test, aes(Precipitation_mm_yr, srun_pred2, label = rownames(df_srun_test))) +
  geom_point(size = 3) +
  # geom_pointrange(aes(ymin = srun_pred - srun_pred_sd, ymax = srun_pred + srun_pred_sd), width = 0.25) +
  geom_text_repel(size = 2, max.overlaps = Inf) +
  # geom_smooth(method = lm) +
  # stat_cor(aes(label = paste(..rr.label..)), label.x.npc = 0.4, label.y.npc = 0.98, size = 4) +
  # geom_abline(intercept = 0, slope = 1, size = 1) +
  # geom_abline(slope = 1, intercept = 98, linetype = "dashed") +
  # geom_abline(slope = 1, intercept = -98, linetype = "dashed") +
  labs(x = "Observed rainfall (mm/yr)",
       y ="Simulated surface runoff (mm/yr)") +
  # annotate("text", x = 40, y = 15, label = "N = 25", size = 4) +
  # annotate("text", x = 40, y = 45, label = "1:1 line", size = 4) +
  expand_limits(x = c(0, 600), y = c(0, 600)) +
  theme_cowplot(font_size = 15) +
  labs_pubr(base_size = 15)

srun_plot

### Variable precipitation sc1----

gwf.rc.plt1 <- ggplot(df.prec.rc1, aes(Precipitation_mm_yr, ngwf, shape = Treatment)) +
  geom_point(size = 2) +
  scale_shape_manual(labels = c("No till",
                                "Straight row",
                                "Straight row and\ncrop residue"),
                     values = c(2, 0, 4)) +
  labs(x = bquote(Precipitation~(mm/yr)),
       y = bquote(nGWF~(m^3/yr)),
       title = "Row crops - Scenario 1") +
  expand_limits(x = c(0, 3000),
                y = c(0, 8000)) +
  theme_pubr(base_size = 10)


gwf.sg.plt1 <- ggplot(df.prec.sg1, aes(Precipitation_mm_yr, ngwf, shape = Treatment)) +
  geom_point(size = 2) +
  scale_shape_manual(labels = c("No till",
                                "Straight row",
                                "Straight row and\ncrop residue"),
                     values = c(2, 0, 4)) +
  labs(x = bquote(Precipitation~(mm/yr)),
       y = "",
       title = "Small grain crops - Scenario 1") +
  expand_limits(x = c(0, 3000),
                y = c(0, 8000)) +
  theme_pubr(base_size = 10)

nload.rc.plt1 <- ggplot(df.prec.rc1, aes(Precipitation_mm_yr, nload, shape = Treatment)) +
  geom_point(size = 2) +
  scale_shape_manual(labels = c("No till",
                                "Straight row",
                                "Straight row and\ncrop residue"),
                     values = c(2, 0, 4)) +
  labs(x = bquote(Precipitation~(mm/yr)),
       y = expression(atop("Nitrogen load", (kg/(ha%*%yr))))) +
  expand_limits(x = c(0, 3000),
                y = c(0, 100)) +
  theme_pubr(base_size = 10)


nload.sg.plt1 <- ggplot(df.prec.sg1, aes(Precipitation_mm_yr, nload, shape = Treatment)) +
  geom_point(size = 2) +
  scale_shape_manual(labels = c("No till",
                                "Straight row",
                                "Straight row and\ncrop residue"),
                     values = c(2, 0, 4)) +
  labs(x = bquote(Precipitation~(mm/yr)),
       y = "") +
  expand_limits(x = c(0, 3000),
                y = c(0, 100)) +
  theme_pubr(base_size = 10)


gwf.rc.plt1 + gwf.sg.plt1 + nload.rc.plt1 + nload.sg.plt1 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect") & theme(legend.position = "bottom")


srun.rc.plt1 <- ggplot(df.prec.rc1, aes(Precipitation_mm_yr, srun, shape = Treatment)) +
  geom_point(size = 2) +
  scale_shape_manual(labels = c("No till",
                                "Straight row",
                                "Straight row and\ncrop residue"),
                     values = c(2, 0, 4)) +
  labs(x = bquote(Precipitation~(mm/yr)),
       y = bquote(atop("Surface runoff", (mm/yr)))) +
  expand_limits(x = c(0, 3000),
                y = c(0, 2000)) +
  theme_pubr(base_size = 10)


srun.sg.plt1 <- ggplot(df.prec.sg1, aes(Precipitation_mm_yr, srun, shape = Treatment)) +
  geom_point(size = 2) +
  scale_shape_manual(labels = c("No till",
                                "Straight row",
                                "Straight row and\ncrop residue"),
                     values = c(2, 0, 4)) +
  labs(x = bquote(Precipitation~(mm/yr)),
       y = "") +
  expand_limits(x = c(0, 3000),
                y = c(0, 2000)) +
  theme_pubr(base_size = 10)

sloss.rc.plt1 <- ggplot(df.prec.rc1, aes(Precipitation_mm_yr, sloss, shape = Treatment)) +
  geom_point(size = 2) +
  scale_shape_manual(labels = c("No till",
                                "Straight row",
                                "Straight row and\ncrop residue"),
                     values = c(2, 0, 4)) +
  labs(x = bquote(Precipitation~(mm/yr)),
       y = bquote(atop("Soil erosion rate", (kg/(ha%*%yr)))),
       title = "Row crops - Scenario 1") +
  expand_limits(x = c(0, 3000),
                y = c(0, 2500)) +
  theme_pubr(base_size = 10)


sloss.sg.plt1 <- ggplot(df.prec.sg1, aes(Precipitation_mm_yr, sloss, shape = Treatment)) +
  geom_point(size = 2) +
  scale_shape_manual(labels = c("No till",
                                "Straight row",
                                "Straight row and\ncrop residue"),
                     values = c(2, 0, 4)) +
  labs(x = bquote(Precipitation~(mm/yr)),
       y = "",
       title = "Small grain crops - Scenario 1") +
  expand_limits(x = c(0, 3000),
                y = c(0, 2500)) +
  theme_pubr(base_size = 10)

sloss.rc.plt1 + sloss.sg.plt1 + srun.rc.plt1 + srun.sg.plt1 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect") & theme(legend.position = "bottom")

### Variable precipitation sc2----

gwf.rc.plt2 <- ggplot(df.prec.rc2, aes(Precipitation_mm_yr, ngwf, shape = Treatment)) +
  geom_point(size = 2) +
  scale_shape_manual(labels = c("No till",
                                "Straight row",
                                "Straight row and\ncrop residue"),
                     values = c(2, 0, 4)) +
  labs(x = bquote(Precipitation~(mm/yr)),
       y = bquote(nGWF~(m^3/yr)),
       title = "Row crops - Scenario 2") +
  expand_limits(x = c(0, 3000),
                y = c(0, 8000)) +
  theme_pubr(base_size = 10)


gwf.sg.plt2 <- ggplot(df.prec.sg2, aes(Precipitation_mm_yr, ngwf, shape = Treatment)) +
  geom_point(size = 2) +
  scale_shape_manual(labels = c("No till",
                                "Straight row",
                                "Straight row and\ncrop residue"),
                     values = c(2, 0, 4)) +
  labs(x = bquote(Precipitation~(mm/yr)),
       y = "",
       title = "Small grain crops - Scenario 2") +
  expand_limits(x = c(0, 3000),
                y = c(0, 8000)) +
  theme_pubr(base_size = 10)

nload.rc.plt2 <- ggplot(df.prec.rc2, aes(Precipitation_mm_yr, nload, shape = Treatment)) +
  geom_point(size = 2) +
  scale_shape_manual(labels = c("No till",
                                "Straight row",
                                "Straight row and\ncrop residue"),
                     values = c(2, 0, 4)) +
  labs(x = bquote(Precipitation~(mm/yr)),
       y = expression(atop("Nitrogen load", (kg/(ha%*%yr))))) +
  expand_limits(x = c(0, 3000),
                y = c(0, 100)) +
  theme_pubr(base_size = 10)


nload.sg.plt2 <- ggplot(df.prec.sg2, aes(Precipitation_mm_yr, nload, shape = Treatment)) +
  geom_point(size = 2) +
  scale_shape_manual(labels = c("No till",
                                "Straight row",
                                "Straight row and\ncrop residue"),
                     values = c(2, 0, 4)) +
  labs(x = bquote(Precipitation~(mm/yr)),
       y = "") +
  expand_limits(x = c(0, 3000),
                y = c(0, 100)) +
  theme_pubr(base_size = 10)


gwf.rc.plt2 + gwf.sg.plt2 + nload.rc.plt2 + nload.sg.plt2 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect") & theme(legend.position = "bottom")


srun.rc.plt2 <- ggplot(df.prec.rc2, aes(Precipitation_mm_yr, srun, shape = Treatment)) +
  geom_point(size = 2) +
  scale_shape_manual(labels = c("No till",
                                "Straight row",
                                "Straight row and\ncrop residue"),
                     values = c(2, 0, 4)) +
  labs(x = bquote(Precipitation~(mm/yr)),
       y = bquote(atop("Surface runoff", (mm/yr)))) +
  expand_limits(x = c(0, 3000),
                y = c(0, 2000)) +
  theme_pubr(base_size = 10)


srun.sg.plt2 <- ggplot(df.prec.sg2, aes(Precipitation_mm_yr, srun, shape = Treatment)) +
  geom_point(size = 2) +
  scale_shape_manual(labels = c("No till",
                                "Straight row",
                                "Straight row and\ncrop residue"),
                     values = c(2, 0, 4)) +
  labs(x = bquote(Precipitation~(mm/yr)),
       y = "") +
  expand_limits(x = c(0, 3000),
                y = c(0, 2000)) +
  theme_pubr(base_size = 10)

sloss.rc.plt2 <- ggplot(df.prec.rc2, aes(Precipitation_mm_yr, sloss, shape = Treatment)) +
  geom_point(size = 2) +
  scale_shape_manual(labels = c("No till",
                                "Straight row",
                                "Straight row and\ncrop residue"),
                     values = c(2, 0, 4)) +
  labs(x = bquote(Precipitation~(mm/yr)),
       y = bquote(atop("Soil erosion rate", (kg/(ha%*%yr)))),
       title = "Row crops - Scenario 2") +
  expand_limits(x = c(0, 3000),
                y = c(0, 2500)) +
  theme_pubr(base_size = 10)


sloss.sg.plt2 <- ggplot(df.prec.sg2, aes(Precipitation_mm_yr, sloss, shape = Treatment)) +
  geom_point(size = 2) +
  scale_shape_manual(labels = c("No till",
                                "Straight row",
                                "Straight row and\ncrop residue"),
                     values = c(2, 0, 4)) +
  labs(x = bquote(Precipitation~(mm/yr)),
       y = "",
       title = "Small grain crops - Scenario 2") +
  expand_limits(x = c(0, 3000),
                y = c(0, 2500)) +
  theme_pubr(base_size = 10)

sloss.rc.plt2 + sloss.sg.plt2 + srun.rc.plt2 + srun.sg.plt2 + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect") & theme(legend.position = "bottom")

### Variable napp sc1----

ngwf.rc.barplt <- ggplot(df.napp.rc1, aes(x = N_applied_kg_N_ha_yr,
                                        y = ngwf,
                                        fill = Treatment)) +
  scale_fill_discrete(name = "Treatment: ",
                      labels = c("No till",
                               "Straight row",
                               "Straight row and\ncrop residue")) +
  scale_x_discrete(limits = c(75, 150, 225, 300)) +
  geom_bar(stat = "identity",
           position = "dodge",
           colour = "black") +
  geom_errorbar(aes(x = N_applied_kg_N_ha_yr,
                    ymax = ngwf + ngwf.sd,
                    ymin = ngwf), stat = "identity",
                position = "dodge",
                size = 0.5,
                alpha = 0.8) +
  expand_limits(y = c(0, 15000)) +
  labs(title = "Row crops - Scenario 1",
       y = bquote(nGWF~(m^3/yr)),
       x = expression(atop("Nitrogen application rate", (kg/(ha%*%yr))))) +
  theme(legend.key.size = unit(1, 'cm')) +
  theme_pubr(base_size = 8)

ngwf.sg.barplt <- ggplot(df.napp.sg1, aes(x = N_applied_kg_N_ha_yr,
                                        y = ngwf,
                                        fill = Treatment)) +
  scale_fill_discrete(name = "Treatment: ",
                      labels = c("No till",
                                 "Straight row",
                                 "Straight row and\ncrop residue")) +
  scale_x_discrete(limits = c(75, 150, 225, 300)) +
  geom_bar(stat = "identity",
           position = "dodge",
           colour = "black") +
  geom_errorbar(aes(x = N_applied_kg_N_ha_yr,
                    ymax = ngwf + ngwf.sd,
                    ymin = ngwf), stat = "identity",
                position = "dodge",
                size = 1.5,
                alpha= 0.8) +
  expand_limits(y = c(0, 15000)) +
  labs(title = "Small grain crops - Scenario 1",
       y = bquote(nGWF~(m^3/yr)),
       x = expression(atop("Nitrogen application rate", (kg/(ha%*%yr))))) +
  theme(legend.key.size = unit(1, 'cm')) +
  theme_pubr(base_size = 8)

ngwf.rc.barplt + ngwf.sg.barplt + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect") & theme(legend.position = "bottom")

### Variable napp sc2----

# Freedman-Diaconis rule to calculated number of bins----

bins_fd <- function(vec) {
  
  diff(range(vec)) / (2 * IQR(vec) / length(vec)^(1/3))

}


# Histograms of variables grouped between calibration and validation datasets----

## Soil loss----

sload <- na.omit(dfmain$`Avg_Soil_Loss_(kg/ha)`)

his.sload <- ggplot(dfmain, aes(log(`Avg_Soil_Loss_(kg/ha)`), fill = Dataset, na.rm = T)) +
  geom_histogram(alpha = 0.7,
                 position = "identity",
                 bins = bins_fd(sload),
                 binwidth = 0.5, 
                 color = "black") + 
  labs(fill = "",
       x = "log soil loss (kg/(ha*yr))",
       title = "Soil loss") +
  theme_half_open()

## Surface runoff ----

srun <- na.omit(dfmain$`Avg_Runoff_(mm)`)


his.srun <- ggplot(dfmain, aes(log(`Avg_Runoff_(mm)`), fill = Dataset, na.rm = T)) +
  geom_histogram(alpha = 0.7,
                 position = "identity",
                 bins = bins_fd(srun),
                 binwidth = 1,
                 color = "black") + 
  labs(fill = "",
       x = "log surface runoff (mm/yr)",
       title = "Surface runoff") +
  theme_half_open()

## Nitrogen load----

nload <- na.omit(dfmain$`Avg_Total_N_(kg/ha)`)


his.nload <- ggplot(dfmain, aes(log(`Avg_Total_N_(kg/ha)`), fill = Dataset, na.rm = T)) +
  geom_histogram(alpha = 0.5,
                 position = "identity",
                 bins = bins_fd(nload),
                 binwidth = 0.5,
                 color = "black") + 
  labs(fill = "",
       x = "log nitrogen load (kg/(ha*yr))",
       title = "Total nitrogen load") +
  theme_half_open()

## HSGs ----

hgs <- na.omit(dfmain$Hydrologic_Soil_Group)

his.hgs <- ggplot(dfmain, aes(Hydrologic_Soil_Group, fill = Dataset, na.rm = T)) +
  geom_histogram(alpha = 0.5,
                 position = "identity",
                 color = "black",
                 stat = "count") + 
  labs(fill = "",
       x = "Hydrologic Soil Groups",
       title = "Soil runoff potential") +
  theme_half_open()

## Precipitation----

rain <- na.omit(dfmain$`Avg_Precipitation_(mm)`)

his.rain <- ggplot(dfmain, aes(`Avg_Precipitation_(mm)`, fill = Dataset, na.rm = T)) +
  geom_histogram(bins = 20,
                 alpha = 0.5,
                 colour = "black") + 
  labs(fill = "",
       x = "Precipitation (mm/yr)",
       title = "Precipitation") +
  theme_half_open()

## Slope----

slope <- na.omit(dfmain$`Max_Land_Slope_(%)`)

his.slope <- ggplot(dfmain, aes(`Max_Land_Slope_(%)`, fill = Dataset, na.rm = T)) +
  geom_histogram(alpha = 0.5,
                 position = "identity",
                 bins = bins_fd(slope),
                 binwidth = 0.5,
                 color = "black") + 
  labs(fill = "",
       x = "Terrain slope (%)",
       title = "Terrain slope") +
  theme_half_open()

## Nitrogen application rate----

napp <- na.omit(dfmain$`Avg_N_Applied_(kg/ha)`)

his.napp <- ggplot(dfmain, aes(log(`Avg_N_Applied_(kg/ha)`), fill = Dataset, na.rm = T)) +
  geom_histogram(alpha = 0.5,
                 position = "identity",
                 bins = bins_fd(napp),
                 binwidth = 0.5,
                 color = "black") + 
  labs(fill = "",
       x = "log Nitrogen application rate (kg/(ha*yr)",
       title = "Nitrogen application rate") +
  theme_half_open()

# Probability density estimation----
# extra dataset for estimation of the cfactor
cfac.dat <- read_excel("~/FieldCompass/AquaRiskIndex/Processes and models/USLE/cfac.xlsx",
                       col_names = TRUE)

cfac.sr <- c(0.2, 0.19, 0.17, 0.15, 0.13, 0.27, 0.25, 0.24, 0.23, 0.21,
             0.25, 0.23, 0.21, 0.18, 0.17)
cfac.srcr <- c(0.14, 0.12, 0.11, 0.09, 0.08, 0.18, 0.16, 0.15, 0.12, 0.11,
               0.19, 0.17, 0.15, 0.13, 0.11)
cfac.srg <- c(0.32, 0.23, 0.22, 0.20, 0.19, 0.17, 0.37, 0.29, 0.27, 0.26, 0.24, 0.22,
              0.37, 0.27, 0.25, 0.23, 0.22, 0.2) 

summary(cfac.dat$cfac)
summary(cfac.srcr)
summary(cfac.srg)

view(df_usle)

ggplot(df_cf, aes(Rain_days_CF)) +
  geom_histogram()


ebeta(df_cf$Rain_days_CF, method = "mle")

epdfPlot(rbeta(448, shape1 = 6.522106, shape2 = 9.070190))

pdfPlot(ebeta, add = TRUE)

ggplot(df_runoff, aes(Rain_days_fraction_d_yr)) +
  geom_histogram()

enorm(df_runoff$Rain_days_fraction_d_yr, ci = TRUE)


prep_raind_plot <- ggplot(df_weather, aes(Precipitation_mm_yr, Rain_days_d_yr)) +
  geom_point(size = 3) +
  geom_smooth(se=TRUE, 
              method = "lm", 
              formula = y~poly(x,2), 
              size = 1.5) +
  labs(title = "Rainfall vs Rain days",
       y = "Rain days (d/yr)",
       x = "Rainfall (mm/yr)") +
  theme_cowplot(font_size = 20) +
  labs_pubr(base_size = 20)
prep_raind_plot

ravg <- ggplot(df_usle, aes(x = Ravg)) +
  geom_histogram(aes (y = ..density..),
                 fill = "cornflowerblue",
                 color = "white") +
  geom_density() +
  # annotate("text",
  #          x = 400,
  #          y = 200,
  #          label = "n = 3104",
  #          size = 25) +
  labs(x = "Rainfall erosivity factor (R)") +
  theme_pubr(base_size = 10)

kavg <- ggplot(df_usle, aes(x = Kavg)) +
  geom_histogram(aes (y = ..density..),
                 fill = "cornflowerblue",
                 color = "white") +
  geom_density() +
  # annotate("text",
  #          x = 0.15,
  #          y = 300,
  #          label = "n = 3104",
  #          size = 25) +
  labs(x = "Soil erodibility factor (K)") +
  theme_pubr(base_size = 10)

lsavg <- ggplot(df_usle, aes(x = LSavg)) +
  geom_histogram(aes (y = ..density..),
                 fill = "cornflowerblue",
                 color = "white",
                 binwidth = 1,
                 bins = 25) +
  # geom_density() +
  # annotate("text",
  #          x = 20,
  #          y = 500,
  #          label = "n = 3104",
  #          size = 25) +
  labs(x = "Topographic factor (LS)") +
  expand_limits(x = c(0, 10)) +
  theme_pubr(base_size = 10)

lsavg


pavg <- ggplot(df_usle, aes(Pavg)) +
  geom_histogram(aes (y = ..density..),
                 fill = "cornflowerblue",
                 color = "white") +
  geom_density() +
  # annotate("text",
  #          x = 0.55,
  #          y = 500,
  #          label = "n = 3104",
  #          size = 5) +
  # labs(y = "Data counts",
      labs(x = "Cropping management factor (P)") +
  theme_pubr(base_size = 10)


cavg <- ggplot(df_usle, aes(x = Cavg)) +
  geom_histogram(aes (y = ..density..),
                 fill = "cornflowerblue",
                 color = "white") +
  geom_density() +
  # annotate("text",
  #          x = 0.1,
  #          y = 300,
  #          label = "n = 3104",
  #          size = 5) +
  labs(x = "Erosion control factor (C)") +
  theme_pubr(base_size = 10)


rain.hist <- ggplot(df_weather, aes(Precipitation_mm_yr)) +
  geom_histogram(fill = "cornflowerblue",
                 color = "black",
                 bins = 25) +
  # geom_density() +
  scale_x_continuous(breaks = seq(0, 3000, 500),
                     limits = c(0, 3000)) +
  # annotate("text",
  #          x = 0.1,
  #          y = 300,
  #          label = "n = 3104",
  #          size = 5) +
  labs(x = "Precipitation (mm/yr)") +
  theme_pubr(base_size = 10)

rain.hist

rdays.hist <- ggplot(df_weather, aes(Rain_days_d_yr)) +
  geom_histogram(aes (y = ..density..),
                 fill = "cornflowerblue",
                 color = "white") +
  geom_density() +
  # annotate("text",
  #          x = 0.1,
  #          y = 300,
  #          label = "n = 3104",
  #          size = 5) +
  labs(x = "Rain days (d/yr)") +
  theme_pubr(base_size = 10)

rdays.hist

hists <- ggarrange(rain.hist, rdays.hist, ravg, kavg, lsavg, pavg, cavg, ncol = 3, nrow = 3)
annotate_figure(hists, left = text_grob("Density", rot = 90))

ebeta(df_usle$Pavg, method = "mle")


epdfPlot(rbeta(3000, shape1 = 4.067819 , shape2 = 0.249393))



pavg.qq <- ggplot(df_usle, aes(sample = Pavg))+
  stat_qq() +
  stat_qq_line()


# Weather----

rain.qq <- ggplot(df_ero, aes(sample = Rainfall.mm))+
  stat_qq() +
  stat_qq_line()



run.hist <- ggplot(df_ero, aes(Runoff.mm)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density()

rmean.hist <- ggplot(df_ero, aes(Rmean)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density()

rain.hist
rdays.hist


length(df_ero$RainDays)

t.test(df_ero$Rainfall.mm)
t.test(df_ero$RainDays)
t.test(df_ero$Runoff.mm)
t.test(df_ero$Rmean)

## Linear model----

rmean.lm <- lm(Rmean ~ Rainfall.mm + RainDays, data = df_ero)
summary(rmean.lm)
plot(rmean.lm, which = 1) 
visreg(rmean.lm, "Rainfall.mm", gg = TRUE)

### Power transformation----
bc<-boxcox(rmean.lm)
bc
which.max(bc$y)
lambda <- bc$x[which.max(bc$y)]
lambda

rmean.lambda <- lm(Rmean ~ I(Rainfall.mm^lambda) + RainDays, data = df_ero)
summary(rmean.lambda)
plot(rmean.lambda, which = 2)
visreg(rmean.lambda, "Rainfall.mm", gg = TRUE)
coef(rmean.lambda)
confint(rmean.lambda)

dwtest(rmean.lambda)
acf(rmean.lambda)

# Buffer strip trapping efficiency----


buffeff.fun <- function(width){
  
  a*(1-exp(-b*width))
  
}

width <- seq(from = 1, to = 100, by = 1)
ab <- list("a" = c(22.9239, 41.8543, 95.0098, 95.5259, 97.3184, 97.0041, 100),
           "b" = c(0.0124, 0.0362, 0.0250, 0.0511, 0.1103, 0.3133, 100))

df.buff.eff <- data.frame(width,
                      E1 = 0, E2 = 0, E3 = 0, E4 = 0, E5 = 0, E6= 0, E7 = 0,
                      a_b = ab) %>% 
  # pivot_longer(cols = c(2:8),
  #              names_to = "Buff",
  #              values_to = "Eff") %>% 
  map(.x = width,
      ~buffeff.fun(width = .x))



site.data <- data.frame(Reference_line = c("E1", "E2", "E3", "E4", "E5", "E6", "E7"),
                        Field_length = c(200, 200, 200, 400, 400, 200, 400),
                        Slope = c("2%", "2%", "2%", "2%", "2%", "10%", "2%"),
                        Soil_texture = c("Fine sandy loam", "Silty clay loam", "Silty clay loam", "Silty clay loam", "Fine sandy loam", "Silty clay loam", "Silty clay loam"),
                        Material = c("Sediment", "Sediment", "Sediment", "Sediment", "Water", "Sediment", "Water"))


table.theme <- ttheme_minimal(base_size = 1, padding = unit(c(2,3), "mm"),
                              cols = c("Reference line", "Field length", "Slope", "Soil Texture", "Material"),)

buff.eff.plt <- ggplot(df.buff.eff) +
  geom_line(aes(x = buff.width, y = E1, colour = "E1"), size= 1.5) +
  geom_line(aes(x = buff.width, y = E2, colour = "E2"), size= 1.5) +
  geom_line(aes(x = buff.width, y = E3, colour = "E3"), size= 1.5) +
  geom_line(aes(x = buff.width, y = E4, colour = "E4"), size= 1.5) +
  geom_line(aes(x = buff.width, y = E5, colour = "E5"), size= 1.5) +
  geom_line(aes(x = buff.width, y = E6, colour = "E6"), size= 1.5) +
  geom_line(aes(x = buff.width, y = E7, colour = "E7"), size= 1.55) +
  labs(x = "Buffer width (m)",
       y = "Trapping efficiency (%)",
       colour = "Reference lines") +
  theme_pubr(base_size = 25) +
  theme(legend.position = "bottom") +
  annotation_custom(tableGrob(site.data,
                              rows = NULL,
                              theme = table.theme),
                    xmin = 70, xmax = 70, ymin = 50, ymax = 60)

buff.eff.plt

# Tables----

## Search string

Component <-  c("Compartment (Population)", "Practices (Intervention)", "Comparator (Alternatives)", "Outcomes")
Definition <- c("Surface waters and in general freshwaters in edge-of-field ",
                "Agriculture practices, weed control techniques, tillage",
                "Various tillage alternatives, and tillage intensities, conservation agriculture practices",
                "Measures in changes in nutrients input from cropping system")
String <- c("'surface water' OR freshwater OR waterbody OR water OR watercourse",
            "agricult* OR farm* OR crop* AND till* OR no$till OR zero$till* OR 'minimum till' OR 'direct drill*' OR 'strip till*' OR 'conservation till*'",
            "'conservation agricult*' AND/OR 'conservation till*'",
            "nutrient$ OR fertili* AND nitr* AND loss OR runoff OR drain* OR leach* AND 'water quality' OR 'surface water  quality' OR 'fresh$water quality'")
tb <- data.frame(Component, Definition, String)

kable(tb,
      caption = "Components of the question and appropriate search terms.",
      booktabs = T,
      col.names = c("Components of the question", "Definition", "Search string"),
      linesep = "\\addlinespace") %>%
  kable_styling(latex_options = c("hold_position","striped"),
                font_size = 10) %>% 
  column_spec(1, italic = T) %>% 
  column_spec(2, width = "15em") %>%
  column_spec(3, width = "15em") %>%
  row_spec(0, bold = T)

## Inclusion criteria

Component <-  c("Compartment (Population)", "Practices (Intervention)", "Comparator (Alternatives)", "Outcomes", "Study design")
Criteria <- c("Water bodies at a farm scale receiving all forms of N (dissolved or particulate), soil", "All tillage practices such as no tillage, reduced, minimum tillage,       grazing are grouped into conservation tillage and plough, pasture, hoeing, harrowing as conventional tillage. Supporting conservation practices are also considered", "Any variation of tillage, and comparison with control that is no tillage (conservation) or any conservation practice", "Change in N and soil concentration/load to water bodies before and after application of conservation practice", "Field studies comparing N and solids concentration/load between inlet and outlet water. Where possible both BA                   (Before-After) and CI (Control-Impact) studies will be considered. Consideration of other confounding factors (e.g., vulnerability to soil erosion, seasonal variation or land use) will be taken into account for article inclusion.")
tb <- data.frame(Component, Criteria)

kable(tb,
      caption = "Eligibility criteria for evidence inclusion",
      booktabs = T,
      escape = F,
      col.names = c("Components of the question", "Eligibility criteria"),
      linesep = "\\addlinespace") %>%
  kable_styling(latex_options = c("hold_position","striped"),
                font_size = 10) %>% 
  column_spec(1, italic = T) %>% 
  column_spec(2, width = "30em") %>%
  row_spec(0, bold = T)

### Buffer efficiencies
Reference.line <- c("E1", "E2", "E3", "E4", "E5", "E6", "E7")
Field.length <- c(200, 200, 200, 400, 400, 200, 400)
Slope <- c("2", "2", "2", "2", "2", "10", "2")
Soil.texture <- c("Fine sandy loam", "Silty clay loam", "Silty clay loam", "Silty clay loam", "Fine sandy loam", "Silty clay loam", "Silty clay loam")
Material <- c("Sediment", "Sediment", "Sediment", "Sediment", "Water", "Sediment", "Water")

tb <- data.frame(Reference.line, Field.length, Slope, Soil.texture, Material)

kable(tb, caption = "Reference site-specific conditions",
      booktabs = T,
      escape = F,
      col.names = c("Reference line", "Field length", "Slope (%)", "Soil texture", "Material"),
      linesep = "\\addlinespace") %>%
  kable_styling(latex_options = c("hold_position","striped"),
                font_size = 10,
                row_label_position = "c") %>% 
  row_spec(0, bold = T) %>% 
  column_spec(1, width = "5em")

site.ch <- c("Nitrogen application rate\n(kg/(ha*yr))",
             "Slope (%)",
             "Field length (m)",
             "Soil texture",
             "Organic matter content (%)",
             "Soil Hydrologic Group",
             "Hydrologic condition") 
sc1 <- c("100", "2", "75", "Silt loam", "Medium", "B", "Poor")


tb <- data.frame(site.ch, sc1)

kable(tb, 
      caption = "Scenario characteristics",
      booktabs = T,
      linesep = "\\addlinespace",
      col.names = c("Characteristics",
                    "Scenario 1"),
      align = "c") %>%
  kable_styling(latex_options = c("hold_position", "striped"),
                font_size = 8) %>% 
  row_spec(0, bold = T) %>% 
  column_spec(1, italic = T)


test <-  c("Error rate (%)", "Logarithmic loss", "Quadratic loss", "Spherical payoff")
cl <-  c("44.42", "1.248", "0.6333", "0.6086")
em <-  c("35.03", "1.933", "0.5252", "0.7002")


tb_error <- data.frame(test, cl, em)
tb_error$cl <- cell_spec(tb_error$cl, align = "c")

# tb_error <- tb_error[c("test", "cl", "em")]

kable(tb_error, 
      caption = "Quality assessment of nitrogen load prediction in the data-trained Bayesian Network",
      booktabs = T,
      linesep = "\\addlinespace",
      col.names = c("Test", "Counting-Learning", "Expectation-maximization")) %>%
  kable_styling(latex_options = c("hold_position", "striped"),
                font_size = 10) %>% 
  row_spec(0, bold = T) %>% 
  column_spec(1, italic = T)


# Saving graphics----
  
ggsave(filename = "nload_marg.png",
       plot = nload_plot_marg,
       device = "png",
       dpi = 300,
       width = 17,
       height = 17,
       units = "cm",
       bg = "white")

# Image import----

srun.prec.cl <- image_read("images/srun_prec_cl.jpg") 
srun.prec.em <- image_read("images/srun_prec_em.jpg") 
srun.prec.CN <- image_read("images/srun_prec_CN.jpg") 

ggdraw() +
  draw_image(srun.prec.cl, width = 0.5)+
  draw_image(srun.prec.em, width = 0.5, x = 0.5) +
  draw_plot_label("A", size = 10, x = 0, y = 0.85) +
  draw_plot_label("B", size = 10, x = 0.5, y = 0.85)












