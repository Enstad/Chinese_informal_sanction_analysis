library(readxl)
library(rvest)
library(httr)
library(dplyr)
library(tidyverse)
library(zoo)
library(stringr)
library(haven)
library(devtools)
library(lubridate)
library(ggplot2)
library(httr)
library(jsonlite)
library(PxWebApiData)
library(readr)
library(countrycode)
library(imf.data)
library(gravity)
library(plm)
library(penppml)
library(fixest)
library(modelsummary)
library(stargazer)




cblocsan1  <- cblocsan %>%
  filter(!is.na(export_dots), !is.na(import_cif_dots))



democarcy <- read_excel ("C:/Users/serin/OneDrive/PECOS/Master thesis/MAdata/ddrevisited_data_v1.xls" )

democarcy<-democarcy %>% select(imf_code, democracy,year)

China_dd_o <- cblocsan1 %>%
  left_join(democarcy , by = c("year", "ifs_num" = "imf_code"))

China_dd_o <-China_dd_o%>%
  mutate(dd_o = democracy)


China_dd_o <- China_dd_o %>%
  left_join(democarcy , by = c("year", "p_ifs_num" = "imf_code"))

China_dd_o<-China_dd_o%>%
  mutate(dd_p = democracy.y)







####creating the wto varaible 


China_dd_o  <- China_dd_o %>%
  mutate(wto = case_when(
    wto_o == 0 & wto_d == 0 ~ 0,
    wto_o == 1 & wto_d == 1 ~ 1,
    wto_o == 1 & wto_d == 0 ~ 0,
    wto_o == 0 & wto_d == 1 ~ 0,
    TRUE ~ NA     
  ))

######################dropping country pair where ther has been no sanctions the last 10 years 
#this is what was doen in the rpelications, the way of implenting this is taken form that script

China_dd_o$imposesanct <- as.numeric(China_dd_o$imposesanct)
China_dd_o$threatsanct <- as.numeric(China_dd_o$threatsanct)



China_dd_o <- China_dd_o %>%
  arrange(ifs_pairid, year) %>% 
  group_by(ifs_pairid) %>%        
  
  # generating a flag telling me 10 years befor a sanctions 
  mutate(bef_imposit = ifelse(imposesanct == 0 & 
                                (dplyr::lead(imposesanct, n = 1, default = 0) == 1 |
                                   dplyr::lead(imposesanct, n = 2, default = 0) == 1 |
                                   dplyr::lead(imposesanct, n = 3, default = 0) == 1 |
                                   dplyr::lead(imposesanct, n = 4, default = 0) == 1 |
                                   dplyr::lead(imposesanct, n = 5, default = 0) == 1 |
                                   dplyr::lead(imposesanct, n = 6, default = 0) == 1 |
                                   dplyr::lead(imposesanct, n = 7, default = 0) == 1 |
                                   dplyr::lead(imposesanct, n = 8, default = 0) == 1 |
                                   dplyr::lead(imposesanct, n = 9, default = 0) == 1 |
                                   dplyr::lead(imposesanct, n = 10, default = 0) == 1),
                              1, 0)) %>% 

# generating a flag telling me 10 years after a sanctions 
mutate(aft_imposit = ifelse(imposesanct == 0 & 
                              (dplyr::lag(imposesanct, n = 1, default = 0) == 1 |
                                 dplyr::lag(imposesanct, n = 2, default = 0) == 1 |
                                 dplyr::lag(imposesanct, n = 3, default = 0) == 1 |
                                 dplyr::lag(imposesanct, n = 4, default = 0) == 1 |
                                 dplyr::lag(imposesanct, n = 5, default = 0) == 1 |
                                 dplyr::lag(imposesanct, n = 6, default = 0) == 1 |
                                 dplyr::lag(imposesanct, n = 7, default = 0) == 1 |
                                 dplyr::lag(imposesanct, n = 8, default = 0) == 1 |
                                 dplyr::lag(imposesanct, n = 9, default = 0) == 1 |
                                 dplyr::lag(imposesanct, n = 10, default = 0) == 1),
                            1, 0)) %>% 

# generating a flag telling me 10 years befor a threath
mutate(bef_threat = ifelse(threatsanct == 0 & 
                             (dplyr::lead(threatsanct, n = 1, default = 0) == 1 |
                                dplyr::lead(threatsanct, n = 2, default = 0) == 1 |
                                dplyr::lead(threatsanct, n = 3, default = 0) == 1 |
                                dplyr::lead(threatsanct, n = 4, default = 0) == 1 |
                                dplyr::lead(threatsanct, n = 5, default = 0) == 1 |
                                dplyr::lead(threatsanct, n = 6, default = 0) == 1 |
                                dplyr::lead(threatsanct, n = 7, default = 0) == 1 |
                                dplyr::lead(threatsanct, n = 8, default = 0) == 1 |
                                dplyr::lead(threatsanct, n = 9, default = 0) == 1 |
                                dplyr::lead(threatsanct, n = 10, default = 0) == 1),
                           1, 0)) %>% 

# generating a flag telling me 10 years after a threath
mutate(aft_threat = ifelse(threatsanct == 0 & 
                             (dplyr::lag(threatsanct, n = 1, default = 0) == 1 |
                                dplyr::lag(threatsanct, n = 2, default = 0) == 1 |
                                dplyr::lag(threatsanct, n = 3, default = 0) == 1 |
                                dplyr::lag(threatsanct, n = 4, default = 0) == 1 |
                                dplyr::lag(threatsanct, n = 5, default = 0) == 1 |
                                dplyr::lag(threatsanct, n = 6, default = 0) == 1 |
                                dplyr::lag(threatsanct, n = 7, default = 0) == 1 |
                                dplyr::lag(threatsanct, n = 8, default = 0) == 1 |
                                dplyr::lag(threatsanct, n = 9, default = 0) == 1 |
                                dplyr::lag(threatsanct, n = 10, default = 0) == 1),
                           1, 0)) %>%
  
  # Keep only the relevant observations
  filter(imposesanct == 1 | 
           threatsanct == 1 | 
           bef_imposit == 1 | 
           aft_imposit == 1 | 
           bef_threat == 1 | 
           aft_threat == 1)




#####################creating the MRT variables for the ols regression 
###################using his code as the blueprint 

China_dd_o   <- China_dd_o   %>% rename(comcur = custrict11 ) 



China_dd_o <- China_dd_o %>%
  group_by(year) %>%
  mutate(
    avdist_all = mean(log(distw), na.rm = TRUE),
    avlang_all = mean(comlang_off, na.rm = TRUE),
    avcol45_all = mean(col45, na.rm = TRUE),
    avcomcur_all = mean(comcur, na.rm = TRUE),
    avrta_all = mean(rta, na.rm = TRUE),
    avwto_all = mean(wto, na.rm = TRUE),
    avborder_all = mean(contig, na.rm = TRUE)
  )


China_dd_o <- China_dd_o %>%
  group_by(alpha_3, year) %>%
  mutate(
    avdist_exp = mean(log(distw), na.rm = TRUE),
    avlang_exp = mean(comlang_off, na.rm = TRUE),
    avcol45_exp = mean(col45, na.rm = TRUE),
    avcomcur_exp = mean(comcur, na.rm = TRUE),
    avrta_exp = mean(rta, na.rm = TRUE),
    avwto_exp = mean(wto, na.rm = TRUE),
    avborder_exp = mean(contig, na.rm = TRUE)
  )


China_dd_o <- China_dd_o %>%
  group_by(p_alpha_3, year) %>%
  mutate(
    avdist_imp = mean(log(distw), na.rm = TRUE),
    avlang_imp = mean(comlang_off, na.rm = TRUE),
    avcol45_imp = mean(col45, na.rm = TRUE),
    avcomcur_imp = mean(comcur, na.rm = TRUE),
    avrta_imp = mean(rta, na.rm = TRUE),
    avwto_imp = mean(wto, na.rm = TRUE),
    avborder_imp = mean(contig, na.rm = TRUE)
  )


China_dd_o <- China_dd_o %>%
  mutate(
    MRT_dist = log(distw) - avdist_exp - avdist_imp + avdist_all,
    MRT_lang = comlang_off - avlang_exp - avlang_imp + avlang_all,
    MRT_col45 = col45 - avcol45_exp - avcol45_imp + avcol45_all,
    MRT_comcur = comcur- avcomcur_exp - avcomcur_imp + avcomcur_all,
    MRT_rta = rta - avrta_exp - avrta_imp + avrta_all,
    MRT_wto = wto - avwto_exp - avwto_imp + avwto_all,
    MRT_border = avborder_exp - avborder_imp + avborder_all
  )


China_dd_o <- China_dd_o %>%
  mutate(
    log_export_dots = log(export_dots),
    lngdp_o = log(gdp_o),
    lngdp_d = log(gdp_d),
    lnpop_o = log(pop_o),
    lnpop_d = log(pop_d),
    log_import_cif_dots= log(import_cif_dots)
  )



China_dd_o<- pdata.frame(China_dd_o, index = c("ifs_pairid", "year"), drop.index=TRUE, row.names=TRUE)

China_dd_o  <- China_dd_o  %>% rename(year = year_str ) %>% rename(threat = threatsanct )


############################theoretically should I add in a varible here for the NA 

OLS_china_exports <- plm(log_export_dots ~ threat + informalsanctions + fs + 
                           lngdp_o + lngdp_d + lnpop_o + lnpop_d +
                           dd_o + dd_p + rta + comcur + wto + MRT_dist + 
                           MRT_lang + MRT_col45 + MRT_comcur +
                           MRT_border + MRT_rta + MRT_wto + factor(year ), 
                         data =China_dd_o, model = "within", 
                         vcovHC = vcovHC(China_dd_o, method = "arellano"))


OLS_china_imports<- plm(log_import_cif_dots ~ threat + informalsanctions + fs + 
                          lngdp_o + lngdp_d + lnpop_o + lnpop_d +
                          dd_o + dd_p + rta + comcur + wto + MRT_dist + 
                          MRT_lang + MRT_col45 + MRT_comcur +
                          MRT_border + MRT_rta + MRT_wto + factor(year), 
                        data = China_dd_o, model = "within", 
                        vcovHC = vcovHC(China_dd_o, method = "arellano"))


summary(OLS_china_exports )
summary(OLS_china_imports)


###########################replication article 
TSdataset10 <- read_dta("threates and sanctions replication/trade flow_10.dta")


data10 <- pdata.frame(TSdataset10, index = c("countrypairs_id", "year"))


model1 <- plm(lnexports ~ threat + sanction2 + sanction3 + lngdp_o + lngdp_d + 
                lnpop_o + lnpop_d + dd_o + dd_d + rta + comcur + wto + 
                MRT_dist + MRT_lang + MRT_col45 + MRT_comcur +
                MRT_border + MRT_rta + MRT_wto + factor(year), 
              data = data10, model = "within", 
              vcovHC = vcovHC(data, method = "arellano"))


data10 <- pdata.frame(TSdataset10, index = c("countrypairs_id", "year"))

data10 $l1.threat <- plm:::lagt.pseries(data10 $threat, 1)
data10 $l2.threat <- plm:::lagt.pseries(data10 $threat, 2)

data10 $l1.sanction2 <- plm:::lagt.pseries(data10 $sanction2, 1)
data10 $l2.sanction2 <- plm:::lagt.pseries(data10 $sanction2, 2)

data10 $l1.sanction3 <- plm:::lagt.pseries(data10 $sanction3, 1)
data10 $l2.sanction3 <- plm:::lagt.pseries(data10 $sanction3, 2)


model2 <- plm(lnexports ~ threat + l1.threat + l2.threat + sanction2 + 
                l1.sanction2 + l2.sanction2 + sanction3 + l1.sanction3 + 
                l2.sanction3 + lngdp_o + lngdp_d + lnpop_o + lnpop_d +
                dd_o + dd_d + rta + comcur + wto + MRT_dist + MRT_lang + 
                MRT_col45 + MRT_comcur + MRT_border + MRT_rta + MRT_wto + 
                factor(year), data = data10, model = "within",
              vcovHC = vcovHC(data10, method = "arellano"))

#importer regerssion with and without lags 


model3 <- plm(lnimports ~ threat + sanction2 + sanction3 + lngdp_o + 
                lngdp_d +  lnpop_o + lnpop_d + dd_o + dd_d + rta + comcur + wto + 
                MRT_dist + MRT_lang +
                MRT_col45 + MRT_comcur + MRT_border + MRT_rta + MRT_wto + 
                factor(year), 
              data = data10, model = "within", 
              vcovHC = vcovHC(data10, method = "arellano"))



model4 <- plm(lnimports ~ threat + l1.threat + l2.threat + sanction2 + 
                l1.sanction2 + l2.sanction2 +
                sanction3 + l1.sanction3 + l2.sanction3 + lngdp_o + lngdp_d + 
                lnpop_o + lnpop_d +
                dd_o + dd_d + rta + comcur + wto + MRT_dist + MRT_lang + 
                MRT_col45 + MRT_comcur +
                MRT_border + MRT_rta + MRT_wto + factor(year),
              data = data10, model = "within",
              vcovHC = vcovHC(data10, method = "arellano"))




#lager ppml modeller med relikasjons data \## imports

#PPML model resplicatons model, without MLR and with import and export time fixed effects




ppml_model_imports <- fepois(
  imports ~ distw + threat + sanction2 + sanction3 +
    lngdp_o + lngdp_d + lnpop_o + lnpop_d +
    dd_o + dd_d + rta + comcur + wto |exporter_year + importer_year,
  cluster = ~ countrypairs_id,
  data = data10 )

ppml_3_imports <- fepois(
  imports ~ threat + sanction2 + sanction3 |countrypairs_id + exporter_year + 
    importer_year,
  cluster = ~ countrypairs_id,
  data = data10 )


replicationppmlimports <- list(
  "OLS Model 1" = model3,
  "PPML Model imports 1" = ppml_model_imports,
  "PPML Model imports 3 way fixed effexts" = ppml_3_imports
)


modelsummary(replicationppmlimports, 
             output = "kableExtra",
             stars = TRUE,
             title = "Replications imports")


#lager ppml modeller med relikasjons data \## exports





ppml_3_exports <- fepois(
  exports ~ threat + sanction2 + sanction3 |countrypairs_id + exporter_year +
    importer_year,
  cluster = ~ countrypairs_id,
  data = data10 )


ppml_model_exports <- fepois(
  exports ~ distw + threat + sanction2 + sanction3 +
    lngdp_o + lngdp_d + lnpop_o + lnpop_d +
    dd_o + dd_d + rta + comcur + wto |exporter_year + importer_year,
  cluster = ~ countrypairs_id,
  data = data10 )


replicationppmlexports <- list(
  "OLS Model 1" = model1,
  "PPML Model Exports 1" = ppml_model_exports,
  "PPML Model Exports 3 way fixed effexts" = ppml_3_exports
)


modelsummary(replicationppmlexports,
             output = "kableExtra",
             stars = TRUE,
             title = "Replications exports")




replication<- data10 %>% rename(threatsanct = threat) %>% rename(fs = imposition)

ppml_imports_sanctions <- fepois(
  imports ~ threatsanct + fs|countrypairs_id + exporter_year + importer_year,
  data = replication )


ppml_exports_sanctions <- fepois(
  exports  ~ threatsanct + fs|countrypairs_id + exporter_year + importer_year,
  data = replication )



replication3ppml <- list(
  "M1 import" = model1,
  "M2 exports" = model3,
  "M3 imports" = ppml_3_imports,
  "M4 exports" = ppml_3_exports
)


modelsummary(replication3ppml, 
             output = "kableExtra",
             stars = TRUE,
             title = "OLS and PPML model")



panel_data_data10 <- panel(data10, panel.id = ~ countrypairs_id + year)

ppml_imports_sanctions_lags <- fepois(
  imports ~ threat + l(threat, 1)+ l(threat, 2)+ sanction2 +  l( sanction2, 1)+ l( sanction2, 2) + sanction +  l( sanction3, 2) + l( sanction3, 1) |countrypairs_id  + exporter_year + 
    importer_year,
  cluster = ~ countrypairs_id,
  data = panel_data_data10 )


ppml_exports_sanctions_lags <- fepois(
  exports ~ threat + l(threat, 1)+ l(threat, 2)+ sanction2 +  l( sanction2, 1)+ l( sanction2, 2)+sanction3 + l( sanction3, 2)+ l( sanction3, 1) |countrypairs_id  + exporter_year + 
    importer_year,
  cluster = ~ countrypairs_id,
  data = panel_data_data10 )


replication3ppml <- list(
  "M1 exports" = model1,
  "M2 imports" = model3,
  "M3 ex.lags" = model2,
  "M4 imp. lags" = model4,
  "M5 imports" = ppml_3_imports,
  "M6 imports" = ppml_imports_sanctions_lags,
  "M7 exports" = ppml_3_exports,
  "M8 exports" = ppml_exports_sanctions_lags
)


modelsummary(replication3ppml, 
             output = "kableExtra",
             stars = TRUE,
             title = "OLS and PPML model")



replication3ppmlLags <- list(
  "M1 exports" = model1,
  "M2 imports" = model3,
  "M3 ex.lags" = model2,
  "M4 imp. lags" = model4,
  "M5 imports" = ppml_3_imports,
  "M6 exports" = ppml_3_exports
)


modelsummary(replication3ppmlLags, 
             output = "kableExtra",
             stars = TRUE,
             title = "OLS and PPML model")





OLS_china_ties<-stargazer(model1, OLS_china_exports , model3, OLS_china_imports,type = "text",
                          title = "OLS china ties",
                          out = "table.txt")


replicationtable<-stargazer(model1, model2, model3, model4,type = "text",
                            title = "Replications table",
                            omit = c("factor\\(year\\)[0-9]{4}","^MRT_","^lnpop_","^lngdp_","^dd_","rta","comcur","wto"),
                            omit.stat = c("f", "ser", "rsq"),
                            out = "table.txt")

