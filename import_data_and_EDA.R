#######################################################################
# CO19 Project 
# 24 May 2020
# Gabrielmaria Scozzarro
#    This project born during the Christmas holidays to asses which was the most effective
#    gov measures during the pandemic and its effects on the new cases, deaths and recovered.
#######################################################################

library(tidyverse)
library(timetk)
library(modeltime)
library(modeltime.ensemble)
library(ggpubr)
library(corrplot)
library(reshape2)
library(dplyr)
library(data.table)
library(reshape2)


#1.0 Import Data  ----
JU_data_death<- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
JU_data_newcases<- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
JU_data_recovered<- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv") 

Gov_measures_data<- readxl::read_xlsx("D:/Datascience_proj_cov19/COV19/Data/acaps_covid19_government_measures_dataset_0.xlsx", sheet = 2)


#1.1 Prepare data ----
col_to_remove_gov<- c(1,2,5,6)
Gov_measures_data<- Gov_measures_data[,-col_to_remove_gov]
Gov_measures_data$ENTRY_DATE<- as.Date(Gov_measures_data$ENTRY_DATE)

col_to_remove_ju_death<- c(3,4)
JU_data_death<- JU_data_death[,-col_to_remove_ju_death]

datalen<- ncol(JU_data_death)
today<- Sys.Date()-1
T_JU_death<- reshape(JU_data_death, 
                     direction = "long", 
                     varying = list(names(JU_data_death)[3:datalen]),
                     v.names = "Deaths",
                     idvar = c("Province.State", "Country.Region"),
                     timevar = "Date",
                     times = seq(as.Date('2020-01-22'),as.Date(today),'day'))

row.names(T_JU_death) <- NULL

JU_data_newcases<- JU_data_newcases[,-col_to_remove_ju_death]
T_JU_newcases<- reshape(JU_data_newcases,
                        direction = "long",
                        varying = list(names(JU_data_newcases)[3:datalen]),
                        v.names = "New_Cases",
                        idvar = c("Province.State", "Country.Region"),
                        timevar = "Date",
                        times = seq(as.Date('2020-01-22'),as.Date(today),'day'))

row.names(T_JU_newcases)<- NULL

JU_data_recovered<- JU_data_recovered[,-col_to_remove_ju_death]
T_ju_recovered<- reshape(JU_data_recovered,
                         direction = "long",
                         varying = list(names(JU_data_recovered)[3:datalen]),
                         v.names = "Recevered",
                         idvar = c("Province.State", "Country.Region"),
                         timevar = "Date",
                         times = seq(as.Date('2020-01-22'),as.Date(today),'day'))

row.names(T_ju_recovered)<- NULL


T_JU_death_it<- T_JU_death %>% filter(Country.Region == "Italy")
T_JU_newcases_it<- T_JU_newcases %>% filter(Country.Region == "Italy")
T_ju_recovered_it<- T_ju_recovered %>% filter(Country.Region == "Italy")

T_JU_death_rec_it<- merge(T_JU_death_it,T_ju_recovered_it, by = "Date")
T_JU_death_rec_it<- T_JU_death_rec_it[,-c(2,5,6)]

T_JU_full_it<- merge(T_JU_death_rec_it, T_JU_newcases_it, by = "Date")
T_JU_full_it<- T_JU_full_it[,-c(5,6)]

Gov_measures_data_it<- Gov_measures_data %>% filter(COUNTRY == "Italy") %>% select(LOG_TYPE, CATEGORY, MEASURE, COMMENTS, ENTRY_DATE)
Gov_measures_data_it<- Gov_measures_data_it[order(Gov_measures_data_it$ENTRY_DATE),]

col_to_add<- c(unique(Gov_measures_data_it$MEASURE))

Ita_gov_health<- T_JU_full_it
Ita_gov_health[col_to_add]<- 0

for (i in 1:nrow(Gov_measures_data_it)) {
  date = Gov_measures_data_it$ENTRY_DATE[i]
  r = which(Ita_gov_health$Date == date)
  col = which(colnames(Ita_gov_health) == Gov_measures_data_it$MEASURE[i])
  if (Gov_measures_data_it$LOG_TYPE[i] == "Introduction / extension of measures") {
    Ita_gov_health[r:nrow(Ita_gov_health),col]<-  Ita_gov_health[r:nrow(Ita_gov_health),col] + 1
  } else {
    Ita_gov_health[r:nrow(Ita_gov_health),col]<- Ita_gov_health[r:nrow(Ita_gov_health),col] -1
  
  }
}

Ita_gov_health[Ita_gov_health<0]<- 0

#2.0 EDA ----

death_plot<- Ita_gov_health %>% plot_time_series(Date, 
                                    Deaths, 
                                    .color_var = month(Date), 
                                    .color_lab = "Month", 
                                    .smooth = FALSE, 
                                    .title = "Death trend over time (2020)", .interactive = FALSE)

newcase_plot<- Ita_gov_health %>% plot_time_series(Date, 
                                                   New_Cases,
                                                   .color_var = month(Date), 
                                                   .color_lab = "Month", 
                                                   .smooth = FALSE, 
                                                   .title = "New cases trend over time (2020)", .interactive = FALSE)

recovered_plot<- Ita_gov_health %>% plot_time_series(Date, 
                                                     Recevered,
                                                     .color_var = month(Date), 
                                                     .color_lab = "Month", 
                                                     .smooth = FALSE, 
                                                     .title = "New cases trend over time (2020)", .interactive = FALSE)

ggarrange(death_plot, newcase_plot, recovered_plot, labels = c("A", "B", "C"), ncol = 1, nrow = 3)

test_policy_plot<- Ita_gov_health %>% ggplot(aes(Date, `Testing policy`, fill = `Testing policy`)) + 
                                      geom_bar(stat = "identity") +
                                      labs(fill = "intensity") +
                                      theme(legend.position = "bottom")
emer_strc_plot<- Ita_gov_health %>% ggplot(aes(Date, `Emergency administrative structures activated or established`, fill = `Emergency administrative structures activated or established`)) +
                                    geom_bar(stat = "identity") +
                                    labs(fill = "intensity") +
                                    theme(legend.position = "bottom")
hlt_screening_plot<- Ita_gov_health %>% ggplot(aes(Date, `Health screenings in airports and border crossings`, fill = `Health screenings in airports and border crossings`)) +
                                        geom_bar(stat = "identity") +
                                        labs(fill = "intensity") +
                                        theme(legend.position = "bottom")
awr_camp_plot<- Ita_gov_health %>% ggplot(aes(Date, `Awareness campaigns`, fill = `Awareness campaigns`)) +
                                   geom_bar(stat = "identity") +
                                   labs(fill = "intensity") +
                                   theme(legend.position = "bottom")
emerg_stt_plot<- Ita_gov_health %>% ggplot(aes(Date, `State of emergency declared`, fill = `State of emergency declared`)) +
                                    geom_bar(stat = "identity")+
                                    labs(fill = "intensity") +
                                    theme(legend.position = "bottom")
flgh_sosp_plot<- Ita_gov_health %>% ggplot(aes(Date, `International flights suspension`, fill = `International flights suspension`)) +
                                    geom_bar(stat = "identity") +
                                    labs(fill = "intensity") +
                                    theme(legend.position = "bottom")
schl_clos_plot<- Ita_gov_health %>% ggplot(aes(Date, `Schools closure`, fill = `Schools closure`)) +
                                    geom_bar(stat = "identity")+
                                    labs(fill = "intensity") +
                                    theme(legend.position = "bottom")
isol_qrt_plot<- Ita_gov_health %>% ggplot(aes(Date, `Isolation and quarantine policies`, fill = `Isolation and quarantine policies`)) +
                                   geom_bar(stat = "identity")+
                                   labs(fill = "intensity") +
                                   theme(legend.position = "bottom")
clos_bsn_plot<- Ita_gov_health %>% ggplot(aes(Date, `Closure of businesses and public services`, fill = `Closure of businesses and public services`)) +
                                   geom_bar(stat = "identity")+
                                   labs(fill = "intensity") +
                                   theme(legend.position = "bottom")
chk_pnt_plot<- Ita_gov_health %>% ggplot(aes(Date, `Checkpoints within the country`, fill = `Checkpoints within the country`)) +
                                  geom_bar(stat = "identity")+
                                  labs(fill = "intensity") +
                                  theme(legend.position = "bottom")
mass_test_plot<- Ita_gov_health %>% ggplot(aes(Date, `Mass population testing`, fill = `Mass population testing`)) +
                                    geom_bar(stat = "identity")+
                                    labs(fill = "intensity") +
                                    theme(legend.position = "bottom")
par_lckdwn_plot<- Ita_gov_health %>% ggplot(aes(Date, `Partial lockdown`, fill = `Partial lockdown`)) +
                                     geom_bar(stat = "identity")+
                                     labs(fill = "intensity") +
                                     theme(legend.position = "bottom")
lmt_pbl_gat_plot<- Ita_gov_health %>% ggplot(aes(Date, `Limit public gatherings`, fill = `Limit public gatherings`)) +
                                      geom_bar(stat = "identity")+
                                      labs(fill = "intensity") +
                                      theme(legend.position = "bottom")

brd_clo_plot<- Ita_gov_health %>% ggplot(aes(Date, `Border closure`, fill = `Border closure`)) +
                                  geom_bar(stat = "identity")+
                                  labs(fill = "intensity") +
                                  theme(legend.position = "bottom")
doc_arr_plot<- Ita_gov_health %>% ggplot(aes(Date, `Additional health/documents requirements upon arrival`, fill = `Additional health/documents requirements upon arrival`)) +
                                  geom_bar(stat = "identity")+
                                  labs(fill = "intensity") +
                                  theme(legend.position = "bottom")
lmt_prod_plot<- Ita_gov_health %>% ggplot(aes(Date, `Limit product imports/exports`, fill = `Limit product imports/exports`)) +
                                    geom_bar(stat = "identity")+
                                    labs(fill = "intensity") +
                                    theme(legend.position = "bottom")
stren_hltsis_plot<- Ita_gov_health %>% ggplot(aes(Date, `Strengthening the public health system`, fill = `Strengthening the public health system`)) +
                                        geom_bar(stat = "identity")+
                                        labs(fill = "intensity") +
                                        theme(legend.position = "bottom")
pris_plot<- Ita_gov_health %>% ggplot(aes(Date, `Changes in prison-related policies`, fill = `Changes in prison-related policies`)) +
                                geom_bar(stat = "identity")+
                                labs(fill = "intensity") +
                                theme(legend.position = "bottom")
ecn_mes_plot<- Ita_gov_health %>% ggplot(aes(Date, `Economic measures`, fill = `Economic measures`)) +
                                  geom_bar(stat = "identity")+
                                  labs(fill = "intensity") +
                                  theme(legend.position = "bottom")
mil_depl_plot<- Ita_gov_health %>% ggplot(aes(Date, `Military deployment`, fill = `Military deployment`)) +
                                    geom_bar(stat = "identity")+
                                    labs(fill = "intensity") +
                                    theme(legend.position = "bottom")
fll_lckdwn_plot<- Ita_gov_health %>% ggplot(aes(Date, `Full lockdown`, fill = `Full lockdown`)) +
                                      geom_bar(stat = "identity")+
                                      labs(fill = "intensity") +
                                      theme(legend.position = "bottom")
dom_trvl_plot<- Ita_gov_health %>% ggplot(aes(Date, `Domestic travel restrictions`, fill = `Domestic travel restrictions`)) +
                                    geom_bar(stat = "identity")+
                                    labs(fill = "intensity") +
                                    theme(legend.position = "bottom")
oth_hlth_plot<- Ita_gov_health %>% ggplot(aes(Date, `Other public health measures enforced`, fill = `Other public health measures enforced`)) +
                                    geom_bar(stat = "identity")+
                                    labs(fill = "intensity") +
                                    theme(legend.position = "bottom")
psy_ass_plot<- Ita_gov_health %>% ggplot(aes(Date, `Psychological assistance and medical social work`, fill = `Psychological assistance and medical social work`)) +
                                  geom_bar(stat = "identity")+
                                  labs(fill = "intensity") +
                                  theme(legend.position = "bottom")
gnr_rec_plot<- Ita_gov_health %>% ggplot(aes(Date, `General recommendations`, fill = `General recommendations`)) +
                                  geom_bar(stat = "identity")+
                                  labs(fill = "intensity") +
                                  theme(legend.position = "bottom")
brd_chk_plot<- Ita_gov_health %>% ggplot(aes(Date, `Border checks`, fill = `Border checks`)) +
                                  geom_bar(stat = "identity")+
                                  labs(fill = "intensity") +
                                  theme(legend.position = "bottom")
surv_mon_plot<- Ita_gov_health %>% ggplot(aes(Date, `Surveillance and monitoring`, fill = `Surveillance and monitoring`)) +
                                    geom_bar(stat = "identity")+
                                    labs(fill = "intensity") +
                                    theme(legend.position = "bottom")
mask_req_plotz<- Ita_gov_health %>% ggplot(aes(Date, `Requirement to wear protective gear in public`, fill = `Requirement to wear protective gear in public`)) +
                                    geom_bar(stat = "identity")+
                                    labs(fill = "intensity") +
                                    theme(legend.position = "bottom")
funeral_amd_plot<- Ita_gov_health %>% ggplot(aes(Date, `Amendments to funeral and burial regulations`, fill = `Amendments to funeral and burial regulations`)) +
                                      geom_bar(stat = "identity")+
                                      labs(fill = "intensity") +
                                      theme(legend.position = "bottom")
visa_res_plot<- Ita_gov_health %>% ggplot(aes(Date, `Visa restrictions`, fill = `Visa restrictions`)) +
                                    geom_bar(stat = "identity")+
                                    labs(fill = "intensity") +
                                    theme(legend.position = "bottom")

ggarrange(test_policy_plot, 
          emer_strc_plot, 
          hlt_screening_plot, 
          awr_camp_plot, 
          emer_strc_plot, 
          flgh_sosp_plot,
          schl_clos_plot,
          isol_qrt_plot,
          clos_bsn_plot,
          chk_pnt_plot,
          mass_test_plot,
          par_lckdwn_plot,
          lmt_pbl_gat_plot,
          brd_clo_plot,
          doc_arr_plot,
          lmt_prod_plot,
          stren_hltsis_plot,
          pris_plot,
          ecn_mes_plot,
          mil_depl_plot,
          fll_lckdwn_plot,
          dom_trvl_plot,
          oth_hlth_plot,
          psy_ass_plot,
          gnr_rec_plot,
          brd_chk_plot,
          surv_mon_plot,
          mask_req_plotz,
          funeral_amd_plot,
          visa_res_plot,
          ncol = 3, nrow = 10)  

# 2.1 Correlation analysis ----
corrplot(cor(Ita_gov_health[3:ncol(Ita_gov_health)]), method = "circle", tl.cex = 0.5)
