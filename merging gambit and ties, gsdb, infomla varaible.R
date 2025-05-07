####33 creating a blocks dataset with from china  with all the sanctions variables 
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
library(fixest)
library(modelsummary)
library(stargazer)
library(plm)
library(dotwhisker)


setwd("C:/Users/serin/OneDrive/PECOS/Master thesis/MAdata")


###############making the datasets similar before merging 
###################this is the gambit dataset- all the sanctions that does not appere in china ties 

gambitds <- read_excel("C:/Users/serin/OneDrive/PECOS/Master thesis/MAdata/Only china gambit sanctions.xlsx")

gambitds <- gambitds %>%
  mutate(sender1 = 710) %>% 
  mutate(GAMBIT= 1)%>%
  mutate(startyear = as.numeric(startyear)) %>% 
  mutate(ID = as.character(ID)) %>% 
  select(-"infomal sanctions")

gambitds$ccode_target <- countrycode(gambitds$state2, "country.name", "cown")


#creating caseid for merging other dataset 
####have to keep the ID as character to avoid them becoming e numbers 


###########################adding in ties_2 
###########and making it sumilar to gambit 


ties_2<-read.csv("C:/Users/serin/OneDrive/PECOS/Master thesis/MAdata/china ties/CESv1 Sender CMPS(2).csv")

ties_2 <- ties_2 %>% 
  filter(Narrative.Link != 'N/A') %>%
  filter(institution != 1) %>% 
  rename(ID = Case.ID) %>%
  mutate(startyear = as.numeric(startyear)) %>% 
  mutate(ID = as.character(ID)) 

ties_2$primarysender<- 710




############falty coding in the daaset - renaming the variable of germany - as federal republic of regman is coded as west germany
###AND removing the EU as it is an instution and not a country 

ties_2 <- ties_2 %>%
  mutate(
    state2 = if_else(state2 == "German Federal Republic", "Germany", state2), # chagin the name to germany 
    ccode_target = if_else(
      state2 == "Germany" & ccode_target == 260, 255, ccode_target) # chaging the country code so that it is correct 
  )%>% filter(ccode_target != 1653) # removing EU


##################merging ties- china and gambit dataset

Chines_sanctions_ds <- bind_rows(ties_2, gambitds)


Chines_sanctions_ds<- Chines_sanctions_ds %>% select("ID", "threatsanct", 
                                                     "ccode_target", "startyear"
                                                     , "endyear", "ongoingasofyear"
                                                     , "imposesanct", "sancimpositionstartyear", 
                                                     "GAMBIT",  "verified", "institution", "primarysender", 
                                                     "sender1", "state2", "Narrative.Link", "reason",everything() )


#######################gambit and ties are fully joined !!!!
############################################################################################
#####################################################################

### add in global sanctions dataset variables 

GSDB4<- read.csv ("C:/Users/serin/OneDrive/PECOS/Master thesis/MAdata/GSDB_4/GSDB_V4.csv")

#filter so it only has CHina
GSDB4 <- GSDB4 %>% 
  filter(sanctioning_state == 'China') %>% 
  filter(sender_mult == '0')

GSDB4$sanctioned_state <- as.character(GSDB4$sanctioned_state)

GSDB4$ccode_target <- countrycode(GSDB4$sanctioned_state, "country.name", "cown")

GSDB4 <- GSDB4 %>%
  rename(
    startyear = begin,
    endyear = end,
    state2 = sanctioned_state,
    ID = case_id,
    state2 = sanctioned_state) %>% 
  filter(ID != 1239) %>% # EU
  filter(sender_mult == '0')

#adding in the country codes in cow so its the same for all 

#adding in varaibles to ease the merger of the datasets 
GSDB4$GSDB <- 1
GSDB4$imposesanct <- 1
GSDB4$sender1 <- 710


# now I will check witch GSDB sanctions allready have case ID from ties 
##creating a TIES variable in the dataset so that I have the whole dataset but I am only merging the once that are uniqe 


GSDB4_not_ties <- GSDB4 %>%
  mutate(TIES = ifelse(ID %in% c("222", "481","870","700", "127", "231", "440", "871","866", "1267", "1294", "1303", "1423","232"), 1, 0)) %>%
  mutate(ID = as.character(ID)) %>% 
  mutate(startyear = as.numeric(startyear)) %>%
  filter(TIES != 1) %>%  
  mutate( state2 = if_else(state2 == "South Vietnam", "Vietnam, Democratic Republic of", state2), #shouth vietnam is recoded becouse its a coding error 
  ccode_target = if_else( state2 == "Vietnam, Democratic Republic of" & ccode_target == 817, 816, ccode_target)) %>% 
  rename(successGSDB = success) 


##### joining the GSDB to the full dataset only the once that are  uniqe 

Chines_sanctions_ds <- bind_rows(
  Chines_sanctions_ds, 
  GSDB4_not_ties)

Chines_sanctions_ds <- Chines_sanctions_ds %>%
  mutate(verified = if_else(GAMBIT == 1 | GSDB == 1, 1, verified))
#####################complete dataset created ###################################
##############################################################################
######################################################################################
###################################################################################

################adding in the informal sanctions from its own dataset 
##############joining in on the ID varaible the informal variable 


insc2<- read_excel( "C:/Users/serin/OneDrive/PECOS/Master thesis/MAdata/informalsanctionsvaraible _ 2.xlsx")
insc2$ID <- as.character(insc2$ID) ###tired to join in infomal variable with another dataset  


####merging in the infmal sanctions varaibel 
Chines_sanctions_ds <- Chines_sanctions_ds %>%
  left_join(insc2 %>% select(ID, informalsanctions), by = "ID")  


Chines_sanctions_ds<- Chines_sanctions_ds %>% select("ID", "informalsanctions","imposesanct", "threatsanct",  "ccode_target", "startyear", "endyear", "ongoingasofyear", "sancimpositionstartyear", "GAMBIT",  "verified", "institution", "primarysender", "sender1", "state2", "Narrative.Link", "reason",everything() )


#the points that did not match are threathes not sanctions, hence I will make the NA 0 on infomal sanctions 

Chines_sanctions_ds <- Chines_sanctions_ds %>%
  mutate(informalsanctions = ifelse(is.na(informalsanctions) & imposesanct== 0, 0, informalsanctions))

############################################################################################################
########################################################################
########################################################################
#makiong a formal sanctions variable creating useing th infomal sanctions and imposed sanctions dummies 

Chines_sanctions_ds <- Chines_sanctions_ds %>%
  mutate(fs = case_when(
    informalsanctions == 0 & imposesanct == 1 ~ 1,  
    informalsanctions == 1 & imposesanct == 0 ~ 0,
    informalsanctions == 0 & imposesanct == 0 ~ 0,
    informalsanctions == 1 & imposesanct == 1 ~ 0,
    is.na(informalsanctions) & imposesanct == 1 ~ 1,
    TRUE ~ NA     
  ))

Chines_sanctions_ds <- Chines_sanctions_ds %>%
  mutate(informalsanctions = ifelse(is.na(informalsanctions), 0, informalsanctions))

#############No NAs in the dataset for fs

Chines_sanctions_ds<- Chines_sanctions_ds %>% select("ID", "fs", "informalsanctions","imposesanct", "threatsanct",  "ccode_target", "startyear", "endyear", "ongoingasofyear", "sancimpositionstartyear", "GAMBIT",  "verified", "institution", "primarysender", "sender1", "state2", "Narrative.Link", "reason",everything() )





####making the dataset yearly instead of insidence based 
## firs step is to make sure alll the sanctions have a start and an end point 
#reordering so I can read the database better

##########had a lot of missing endpoints so I filterd them out and made my own dataset where i fuiled out
##the endyear or ongoing variable and added them back in 
#### as the dataset has both endyear and ongoing varibales, I will use both of them to create and endvar character 
######that I will use as my endpoint of the analysis.



endyear <- read_excel("C:/Users/serin/OneDrive/PECOS/Master thesis/MAdata/datates for endatae dataset.xlsx")
endyear$ID <- as.character(endyear$ID)

#mering on ID
Chines_sanctions_ds <- Chines_sanctions_ds%>%
  left_join(endyear %>% select(ID,endvar), by = "ID")

#creating a common endvar variable with the 3 variables endvar, endyear, ongoingasofyear
Chines_sanctions_ds <- Chines_sanctions_ds %>%
  mutate(endvar = coalesce(endvar, endyear, ongoingasofyear))

#strucutturing dataset so its easier to visaully inspect 
Chines_sanctions_ds <- Chines_sanctions_ds %>%
  select(ID, endvar, verified, endyear,ongoingasofyear, sancimpositionstartyear, startyear, threatsanct, imposesanct, informalsanctions, state2, primarysender, everything())




#chacking for NA in the endvar variable its 0 witch is what we want 
sum(is.na(Chines_sanctions_ds$endvar))




###########creating a dayadic yearly iinsident dataset with all the sanctions and threates 

#fixing issue with tiwan and usa nameing issue
Chines_sanctions_ds <- Chines_sanctions_ds %>%
  mutate(state2 = ifelse(state2 == "taiwan", "Taiwan", state2))%>%
  mutate(state2 = ifelse(state2 == "USA","United States of America", state2))%>%
  mutate(state2 = ifelse(state2 == "UK","United Kingdom", state2))

chsc_yearly <- Chines_sanctions_ds %>%
  rowwise() %>%
  do({
    # Create a sequence of years for each row
    years <- seq(.$startyear, .$endvar, by = 1)
    # Determine if imposesanct should be 1 if it is NA
    impose_sanct_flag <- ifelse(is.na(.$imposesanct) & (.$GAMBIT == 1 | .$GSDB == 1), 1, .$imposesanct)
    # Repeat the row for each year
    tibble(
      year = years,
      ID = .$ID,
      primarysender = .$primarysender,
      state2 = .$state2,
      ccode_target = .$ccode_target,
      threatsanct = .$threatsanct,
      imposesanct = ifelse(years >= .$sancimpositionstartyear & !is.na(.$sancimpositionstartyear) & impose_sanct_flag == 1, 1, .$imposesanct),
      informalsanctions = .$informalsanctions,
      fs= .$fs,
      Narrative.Link = .$Narrative.Link,
      TIES_code = .$TIES_code,
      GSDB = .$GSDB,
      sender1 = .$sender1,
      ongoingasofyear = .$ongoingasofyear,
      sancimpositionstartyear = .$sancimpositionstartyear,
      threatsanct_type = .$threatsanct_type,
      PRC_leader = .$PRC_leader,
      endvar = .$endvar,
      PRCleader_1 = .$PRCleader_1,
      verified = .$verified,
      GAMBIT = .$GAMBIT,
      TIES = .$TIES,
      startyear = .$startyear,
      caseid_name = .$caseid_name,
    )
  }) %>%
  ungroup()

# Check the modified dataset
chsc_yearly$sender1 <- "China"
chsc_yearly$primarysender <- 710



# Convert COW codes to IMF codes using the countrycode function as blocks uses IMF 
chsc_yearly$imf_code_target <- countrycode(
  sourcevar = chsc_yearly$ccode_target,
  origin = "cown",  
  destination = "imf"  
)


#################north korea is missnamed accoridng th the IMF convetion so I am adding inn the right IMF code for it 
chsc_yearly <- chsc_yearly %>%
  mutate(
    imf_code_target = if_else(
      str_detect(ccode_target, "731"),
      if_else(is.na(imf_code_target), 954, imf_code_target + 954),  
      imf_code_target 
    )
  )

#reruning the code so that north korea is korect 
chsc_yearly$imf_code_sender <- countrycode(
  sourcevar = chsc_yearly$primarysender,
  origin = "cown",  
  destination = "imf"  
)



#################adding in the continet of the target 

chsc_yearly$continent_taget <- countrycode(
  sourcevar = chsc_yearly$imf_code_target,     
  origin = "imf",                           
  destination = "continent",                 
  warn = TRUE                                
)


chsc_yearly <- chsc_yearly %>%
  mutate(
    continent_taget = if_else(
      str_detect(imf_code_target, "954") & is.na(continent_taget),
      "Asia", 
      continent_taget
    )
  )




##################################################
######################Delaing with the issue fo potential multaples 
############becouse there are some years where the taget gets sanctions with diffrent types of sanctions 
###########in the same year. this is an issue as when I merge I get several non uniqe observations in the dataset
##hence I aggregate the sanctions in the sanctions dataset and merge that on.

chsc_yearly <- chsc_yearly %>%
  mutate(MID = paste0(ID, imf_code_target,imf_code_sender, year ))



###########creating the pair ids twice to have it as o and p country 
chsc_version1 <- chsc_yearly %>%
  mutate(ifs_pairid = paste0(imf_code_target, imf_code_sender)) %>%  mutate(id_ifs = paste0(ifs_pairid,year))

chsc_version2 <- chsc_yearly %>%
  mutate(ifs_pairid = paste0(imf_code_sender, imf_code_target))  %>%  mutate(id_ifs = paste0(ifs_pairid,year))


chsc_expanded <- bind_rows(chsc_version1, chsc_version2)


###########in this dataset there are mulitples fo the pair however the MID should never be the same 

#######add to blocs dataset 
#######################################################

Blocs<-read.csv("C:/Users/serin/OneDrive/PECOS/Master thesis/MAdata/blocs/BLOCS 3.2(2).csv")

###########leaving some years in the dataset before I have data on sanctions 
####################attempt this with other numbers as well have used 1981 for my last model

Blocs <- Blocs %>%
  filter(year >= 1955 & year <= 2022)


#####################merging the datasets 
############creating dummies for sanctions and threathes of sanctions 




###########merging the sanctions dataset with the blocks dataset.
#########agreagting to avoid duplicated 
china_sac<-chsc_expanded %>% 
  group_by(id_ifs) %>% 
  summarise(continent_taget = first(continent_taget),
            threatsanct= ifelse(any(threatsanct==1),1,0), 
            imposesanct= ifelse(any(imposesanct==1),1,0),
            informalsanctions= ifelse(any(informalsanctions==1),1,0),
            fs= ifelse(any(fs==1),1,0))


china_sac$id_ifs<- as.character(china_sac$id_ifs)
Blocs$id_ifs <- as.character(Blocs$id_ifs)



cblocsan <- Blocs %>%
  left_join(china_sac, by = c("id_ifs"))


########################
#becouse there is only threaths data inthe china ties datase t am I not making thos 0
#if they are na, however all the other china pairs should be 0.
#so this code checks if the pair includes china and changes the sanctiosn variables if they are.

cblocsan <- cblocsan %>%
  mutate(
    imposesanct = ifelse(is.na(imposesanct), 0, imposesanct),
    fs = ifelse(is.na(fs), 0, fs),
    informalsanctions = ifelse(is.na(informalsanctions), 0, informalsanctions)
  )




#####reducing the dataset to do the analysis on to minimise the dataset 

reduced_cblocsan <- cblocsan %>%
  select(o_country, p_country,   export_dots, threatsanct, informalsanctions, fs,  imposesanct, ifs_pairid, p_ifs_num_str, ifs_num_str,un_num_str, year,import_cif_dots, id_ifs)

reduced_cblocsan<-reduced_cblocsan%>%
  filter(year >= 1976 & year <= 2022)

         
##### PPML models for the analysis        
         
ppml_3_exports_china <- fepois(
  export_dots ~  informalsanctions + fs  | ifs_pairid + ifs_num_str*year + p_ifs_num_str*year,
  cluster = ~ ifs_pairid,
  data = reduced_cblocsan )


ppml_3_import_china<- fepois(  import_cif_dots ~  informalsanctions +fs |ifs_pairid + un_num_str*year + p_ifs_num_str*year,
                               cluster = ~ ifs_pairid,
                               data = reduced_cblocsan )



Chinamodel <- list(
  "PPML China_exports" = ppml_3_exports_china,
  "PPML China_imports" = ppml_3_import_china)


modelsummary(Chinamodel, 
             output = "kableExtra",
             stars = TRUE,
             title = "China modle, imports and exports")



#####interatiction model 
ppml_3_exports_china_fs_in <- fepois(
  export_dots ~  informalsanctions + fs + i(fs, informalsanctions)| ifs_pairid + ifs_num_str*year + p_ifs_num_str*year,
  cluster = ~ ifs_pairid,
  data = reduced_cblocsan )


ppml_3_import_china_fs_in<- fepois(
  import_cif_dots ~  informalsanctions + fs+ i(fs, informalsanctions) |ifs_pairid + ifs_num_str*year + p_ifs_num_str*year,
  cluster = ~ ifs_pairid,
  data = reduced_cblocsan )

Chinamodel_interaction <- list(
  "PPML China_exports" = ppml_3_exports_china,
  "PPML China formal and infomal ex" =ppml_3_exports_china_fs_in,
  "PPML China_imports" = ppml_3_import_china,
  "PPML China formal and infomal im"=ppml_3_import_china_fs_in)

modelsummary(Chinamodel_interaction , 
             output = "kableExtra",
             stars = TRUE,
             title = "China modle, imports and exports")



dwplot(list (ppml_3_exports_china,ppml_3_import_china))



##########model with the lead variable 

reduced_cblocsan$informalsanctions <- as.numeric(reduced_cblocsan$informalsanctions)
reduced_cblocsan$fs <- as.numeric(reduced_cblocsan$fs)



panel_data_china <- panel(reduced_cblocsan, panel.id = ~ ifs_pairid + year)

ppml_3_exports_china_lead <- fepois(
  export_dots ~  informalsanctions + fs + f(informalsanctions, 1)+ f(informalsanctions, 2)+ f(fs, 1)+ f(fs, 2)|ifs_pairid + p_ifs_num_str*year+ ifs_num_str*year,
  cluster =   ~ ifs_pairid,
  data = panel_data_china  )


ppml_3_import_china_lead<- fepois(
  import_cif_dots ~  informalsanctions +fs +f(informalsanctions, 1) + f(informalsanctions, 2)+ f(fs, 1)+ f(fs, 2)|ifs_pairid + p_ifs_num_str*year + ifs_num_str*year,
  cluster = ~ ifs_pairid,
  data = panel_data_china  )


###model with lagged variables 

reduced_cblocsan<-reduced_cblocsan%>%
  filter(year >= 1976 & year <= 2022)

panel_data_china <- panel(reduced_cblocsan, panel.id = ~ ifs_pairid + year)

# Model for imports using lagged values
ppml_3_import_china_lags <- fepois(
  import_cif_dots ~ informalsanctions + fs + 
    l(informalsanctions, 1) + l(informalsanctions, 2) +
    l(informalsanctions, 3) + l(informalsanctions, 4) +
    l(fs, 1) + l(fs, 2) + l(fs, 3) + l(fs, 4) |
    ifs_pairid + p_ifs_num_str * year + ifs_num_str * year,
  cluster = ~ ifs_pairid,
  data = panel_data_china
)



# Model for imports using lagged values
ppml_3_exports_china_lags<- fepois(
  export_dots ~ informalsanctions + fs + 
    l(informalsanctions, 1) + l(informalsanctions, 2) +
    l(informalsanctions, 3) + l(informalsanctions, 4) +
    l(fs, 1) + l(fs, 2) + l(fs, 3) + l(fs, 4) |
    ifs_pairid + p_ifs_num_str * year + ifs_num_str * year,
  cluster = ~ ifs_pairid,
  data = panel_data_china
)


Chinamodel_import_lag_lead <- list(
  "M1" = ppml_3_import_china,
  "M2" = ppml_3_import_china_lead,
  "M3" = ppml_3_import_china_lags)


modelsummary(Chinamodel_import_lag_lead, 
             output = "kableExtra",
             stars = TRUE,
             title = "China modle, importslags and leads")


Chinamodel_lags_lead_exports <- list(
  "M1" = ppml_3_exports_china,
  "M2" = ppml_3_exports_china_lead,
  "M3" = ppml_3_exports_china_lags)


modelsummary(Chinamodel_lags_lead_exports, 
             output = "kableExtra",
             stars = TRUE,
             title = "China modle, exports lags and leads")



Chinamodel<- list(
  "M1 imports" = ppml_3_import_china,
  "M2 exports" = ppml_3_exports_china,
  "M3 import" = ppml_3_import_china_lead,
  "M4 export" = ppml_3_exports_china_lead,
  "M5 import" = ppml_3_import_china_lags,
  "M6 export" = ppml_3_exports_china_lags)


modelsummary(Chinamodel, 
             output = "kableExtra",
             stars = TRUE,
             title = "China model, with lags and leads")


dwplot(list (ppml_3_exports_china,ppml_3_import_china,ppml_3_import_china_lead,ppml_3_exports_china_lead, ppml_3_import_china_lags,ppml_3_exports_china_lags))