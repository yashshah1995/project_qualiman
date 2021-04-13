library(ggplot2)
source("help_functions.r")

# Farben f√ºr die Kohorten
MDKE_Color     <- "#9fb059" #gr√ºn   
DE_Color      <- "#edae52" #orange   DigiEng
Frame_Color   <- "#060703"

# Bar graphes, histograms
barfill_blue  <- "#4271AE"
barlines_blue <- "#1F3552"

FIN_Color <- "#0068b4"


str_crit <- list(criterion1 = "K1 = Anz. gesammelter CP / \nAnz. Studiensemester * 30CP", criterion2 = "K2 = Anz. bestandener Pr√ºfungen / \nAnz. abgelegter Pr√ºfungen",  
                 criterion3 = "K3 = Durchschnittsnote aller\nbestandenen LV",
                 criterion4 = "K4 = CP pro Semester",
                 criterion5 = "K5 = Durchschnittsnote im Semester", 
                 criterion6 = "K6 = Anz. bestandener Pr√ºfungen im Semester / \nAnz. abgelegter Pr√ºfungen im Semester")  

#-----------------------------------------------------------------------------------


ua_help <- dplyr::select(ua, Studiengang, Studienbeginn, HZB_Note, Geschlecht, Sprachniveau, Studienbeginn_Alter)
nrow(dplyr::filter(ua_help, is.na(HZB_Note)))
mean(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2016 SoSe")[, "HZB_Note"])

ua_help$HZB_Note <- as.numeric(ua_help$HZB_Note)

DE_stat_table <- function(){
  ##### MDigiEng
  stats_DE <- matrix(c(nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2015 SoSe")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2015/2016 WiSe")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2016 SoSe")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2016/2017 WiSe")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2017 SoSe")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2017/2018 WiSe")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2018 SoSe")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2018/2019 WiSe")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2019 SoSe")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2019/2020 WiSe")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng")),
                       
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2015 SoSe" & Geschlecht == "1")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2015/2016 WiSe" & Geschlecht == "1")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2016 SoSe" & Geschlecht == "1")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2016/2017 WiSe" & Geschlecht == "1")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2017 SoSe" & Geschlecht == "1")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2017/2018 WiSe" & Geschlecht == "1")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2018 SoSe" & Geschlecht == "1")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2018/2019 WiSe" & Geschlecht == "1")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2019 SoSe" & Geschlecht == "1")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2019/2020 WiSe" & Geschlecht == "1")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Geschlecht == "1")),
                       
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2015 SoSe" & Geschlecht == "2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2015/2016 WiSe" & Geschlecht == "2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2016 SoSe" & Geschlecht == "2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2016/2017 WiSe" & Geschlecht == "2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2017 SoSe" & Geschlecht == "2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2017/2018 WiSe" & Geschlecht == "2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2018 SoSe" & Geschlecht == "2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2018/2019 WiSe" & Geschlecht == "2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2019 SoSe" & Geschlecht == "2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2019/2020 WiSe" & Geschlecht == "2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Geschlecht == "2")),
                       
                       mean(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2015 SoSe"& !is.na(HZB_Note))[, "HZB_Note"]),
                       mean(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2015/2016 WiSe"& !is.na(HZB_Note))[, "HZB_Note"]),
                       mean(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2016 SoSe"& !is.na(HZB_Note))[, "HZB_Note"]),
                       mean(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2016/2017 WiSe"& !is.na(HZB_Note))[, "HZB_Note"]),
                       mean(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2017 SoSe"& !is.na(HZB_Note))[, "HZB_Note"]),
                       mean(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2017/2018 WiSe"& !is.na(HZB_Note))[, "HZB_Note"]),
                       mean(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2018 SoSe"& !is.na(HZB_Note))[, "HZB_Note"]),
                       mean(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2018/2019 WiSe"& !is.na(HZB_Note))[, "HZB_Note"]),
                       mean(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2019 SoSe"& !is.na(HZB_Note))[, "HZB_Note"]),
                       mean(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2019/2020 WiSe"& !is.na(HZB_Note))[, "HZB_Note"]),
                       mean(dplyr::filter(ua_help, Studiengang=="MDigiEng")[, "HZB_Note"]),
                       
                       sd(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2015 SoSe" & !is.na(HZB_Note))[, "HZB_Note"]),
                       sd(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2015/2016 WiSe" & !is.na(HZB_Note))[, "HZB_Note"]),
                       sd(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2016 SoSe" & !is.na(HZB_Note))[, "HZB_Note"]),
                       sd(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2016/2017 WiSe" & !is.na(HZB_Note))[, "HZB_Note"]),
                       sd(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2017 SoSe" & !is.na(HZB_Note))[, "HZB_Note"]),
                       sd(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2017/2018 WiSe" & !is.na(HZB_Note))[, "HZB_Note"]),
                       sd(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2018 SoSe" & !is.na(HZB_Note))[, "HZB_Note"]),
                       sd(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2018/2019 WiSe" & !is.na(HZB_Note))[, "HZB_Note"]),
                       sd(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2019 SoSe" & !is.na(HZB_Note))[, "HZB_Note"]),
                       sd(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2019/2020 WiSe" & !is.na(HZB_Note))[, "HZB_Note"]),
                       sd(dplyr::filter(ua_help, Studiengang=="MDigiEng" & !is.na(HZB_Note))[, "HZB_Note"]),
                       
                       
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2015 SoSe" & !is.na(HZB_Note))),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2015/2016 WiSe" & !is.na(HZB_Note))),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2016 SoSe" & !is.na(HZB_Note))),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2016/2017 WiSe" & !is.na(HZB_Note))),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2017 SoSe" & !is.na(HZB_Note))),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2017/2018 WiSe" & !is.na(HZB_Note))),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2018 SoSe" & !is.na(HZB_Note))),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2018/2019 WiSe" & !is.na(HZB_Note))),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2019 SoSe" & !is.na(HZB_Note))),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2019/2020 WiSe" & !is.na(HZB_Note))),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & !is.na(HZB_Note))),
                       
                       mean(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2015 SoSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                       mean(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2015/2016 WiSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                       mean(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2016 SoSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                       mean(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2016/2017 WiSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                       mean(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2017 SoSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                       mean(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2017/2018 WiSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                       mean(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2018 SoSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                       mean(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2018/2019 WiSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                       mean(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2019 SoSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                       mean(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2019/2020 WiSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                       mean(dplyr::filter(ua_help, Studiengang=="MDigiEng"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                       
                       sd(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2015 SoSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                       sd(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2015/2016 WiSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                       sd(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2016 SoSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                       sd(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2016/2017 WiSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                       sd(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2017 SoSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                       sd(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2017/2018 WiSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                       sd(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2018 SoSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                       sd(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2018/2019 WiSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                       sd(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2019 SoSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                       sd(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2019/2020 WiSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                       sd(dplyr::filter(ua_help, Studiengang=="MDigiEng"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                       
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2015 SoSe" & Sprachniveau=="B2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2015/2016 WiSe" & Sprachniveau=="B2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2016 SoSe" & Sprachniveau=="B2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2016/2017 WiSe" & Sprachniveau=="B2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2017 SoSe" & Sprachniveau=="B2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2017/2018 WiSe" & Sprachniveau=="B2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2018 SoSe" & Sprachniveau=="B2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2018/2019 WiSe" & Sprachniveau=="B2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2019 SoSe" & Sprachniveau=="B2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2019/2020 WiSe" & Sprachniveau=="B2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Sprachniveau=="B2")),
                       
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2015 SoSe" & Sprachniveau=="C1")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2015/2016 WiSe" & Sprachniveau=="C1")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2016 SoSe" & Sprachniveau=="C1")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2016/2017 WiSe" & Sprachniveau=="C1")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2017 SoSe" & Sprachniveau=="C1")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2017/2018 WiSe" & Sprachniveau=="C1")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2018 SoSe" & Sprachniveau=="C1")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2018/2019 WiSe" & Sprachniveau=="C1")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2019 SoSe" & Sprachniveau=="C1")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2019/2020 WiSe" & Sprachniveau=="C1")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Sprachniveau=="C1")),
                       
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2015 SoSe" & Sprachniveau=="C2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2015/2016 WiSe" & Sprachniveau=="C2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2016 SoSe" & Sprachniveau=="C2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2016/2017 WiSe" & Sprachniveau=="C2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2017 SoSe" & Sprachniveau=="C2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2017/2018 WiSe" & Sprachniveau=="C2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2018 SoSe" & Sprachniveau=="C2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2018/2019 WiSe" & Sprachniveau=="C2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2019 SoSe" & Sprachniveau=="C2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2019/2020 WiSe" & Sprachniveau=="C2")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Sprachniveau=="C2")),
                       
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2015 SoSe" & Sprachniveau=="")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2015/2016 WiSe" & Sprachniveau=="")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2016 SoSe" & Sprachniveau=="")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2016/2017 WiSe" & Sprachniveau=="")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2017 SoSe" & Sprachniveau=="")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2017/2018 WiSe" & Sprachniveau=="")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2018 SoSe" & Sprachniveau=="")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2018/2019 WiSe" & Sprachniveau=="")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2019 SoSe" & Sprachniveau=="")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Studienbeginn =="2019/2020 WiSe" & Sprachniveau=="")),
                       nrow(dplyr::filter(ua_help, Studiengang=="MDigiEng" & Sprachniveau==""))),nrow=11, byrow=FALSE)
  
  rownames(stats_DE) <- c("2015 SoSe", "2015/2016 WiSe", "2016 SoSe", "2016/2017 WiSe", "2017 SoSe", "2017/2018 WiSe", "2018 SoSe", "2018/2019 WiSe", "2019 SoSe", "2019/2020 WiSe", "Insgesamt")
  colnames(stats_DE) <- c("Anzahl", "Geschlecht M", "Geschlecht W", "HZB-Note", "SD-Abweichung von Mean", "Menge, die Note hat", "Alter", "SD Alter", "B2", "C1", "C2", "Sprache nicht leer")
  
  stats_DE <- as.table(stats_DE)
  return(stats_DE)
  
}


##### DKE
DKE_stat_table <- function(){

stats_DKE <- matrix(c(nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2015 SoSe")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2015/2016 WiSe")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2016 SoSe")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2016/2017 WiSe")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2017 SoSe")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2017/2018 WiSe")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2018 SoSe")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2018/2019 WiSe")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2019 SoSe")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2019/2020 WiSe")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE")),
                      
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2015 SoSe" & Geschlecht == "1")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2015/2016 WiSe" & Geschlecht == "1")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2016 SoSe" & Geschlecht == "1")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2016/2017 WiSe" & Geschlecht == "1")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2017 SoSe" & Geschlecht == "1")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2017/2018 WiSe" & Geschlecht == "1")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2018 SoSe" & Geschlecht == "1")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2018/2019 WiSe" & Geschlecht == "1")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2019 SoSe" & Geschlecht == "1")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2019/2020 WiSe" & Geschlecht == "1")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Geschlecht == "1")),
                      
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2015 SoSe" & Geschlecht == "2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2015/2016 WiSe" & Geschlecht == "2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2016 SoSe" & Geschlecht == "2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2016/2017 WiSe" & Geschlecht == "2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2017 SoSe" & Geschlecht == "2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2017/2018 WiSe" & Geschlecht == "2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2018 SoSe" & Geschlecht == "2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2018/2019 WiSe" & Geschlecht == "2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2019 SoSe" & Geschlecht == "2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2019/2020 WiSe" & Geschlecht == "2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Geschlecht == "2")),
                      
                      mean(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2015 SoSe"&      !is.na(HZB_Note))[, "HZB_Note"]),
                      mean(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2015/2016 WiSe"& !is.na(HZB_Note))[, "HZB_Note"]),
                      mean(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2016 SoSe"&      !is.na(HZB_Note))[, "HZB_Note"]),
                      mean(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2016/2017 WiSe"& !is.na(HZB_Note))[, "HZB_Note"]),
                      mean(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2017 SoSe"&      !is.na(HZB_Note))[, "HZB_Note"]),
                      mean(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2017/2018 WiSe"& !is.na(HZB_Note))[, "HZB_Note"]),
                      mean(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2018 SoSe"&      !is.na(HZB_Note))[, "HZB_Note"]),
                      mean(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2018/2019 WiSe"& !is.na(HZB_Note))[, "HZB_Note"]),
                      mean(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2019 SoSe"&      !is.na(HZB_Note))[, "HZB_Note"]),
                      mean(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2019/2020 WiSe"& !is.na(HZB_Note))[, "HZB_Note"]),
                      mean(dplyr::filter(ua_help, Studiengang=="MDKE"& !is.na(HZB_Note))[, "HZB_Note"]),
                      
                      sd(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2015 SoSe" &       !is.na(HZB_Note))[, "HZB_Note"]),
                      sd(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2015/2016 WiSe" &  !is.na(HZB_Note))[, "HZB_Note"]),
                      sd(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2016 SoSe" &       !is.na(HZB_Note))[, "HZB_Note"]),
                      sd(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2016/2017 WiSe" &  !is.na(HZB_Note))[, "HZB_Note"]),
                      sd(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2017 SoSe" &       !is.na(HZB_Note))[, "HZB_Note"]),
                      sd(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2017/2018 WiSe" &  !is.na(HZB_Note))[, "HZB_Note"]),
                      sd(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2018 SoSe" &       !is.na(HZB_Note))[, "HZB_Note"]),
                      sd(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2018/2019 WiSe" &  !is.na(HZB_Note))[, "HZB_Note"]),
                      sd(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2019 SoSe" &       !is.na(HZB_Note))[, "HZB_Note"]),
                      sd(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2019/2020 WiSe" &  !is.na(HZB_Note))[, "HZB_Note"]),
                      sd(dplyr::filter(ua_help, Studiengang=="MDKE" & !is.na(HZB_Note))[, "HZB_Note"]),
                      
                      
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2015 SoSe" & !is.na(HZB_Note))),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2015/2016 WiSe" & !is.na(HZB_Note))),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2016 SoSe" & !is.na(HZB_Note))),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2016/2017 WiSe" & !is.na(HZB_Note))),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2017 SoSe" & !is.na(HZB_Note))),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2017/2018 WiSe" & !is.na(HZB_Note))),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2018 SoSe" & !is.na(HZB_Note))),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2018/2019 WiSe" & !is.na(HZB_Note))),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2019 SoSe" & !is.na(HZB_Note))),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2019/2020 WiSe" & !is.na(HZB_Note))),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & !is.na(HZB_Note))),
                      
                      mean(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2015 SoSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                      mean(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2015/2016 WiSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                      mean(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2016 SoSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                      mean(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2016/2017 WiSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                      mean(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2017 SoSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                      mean(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2017/2018 WiSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                      mean(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2018 SoSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                      mean(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2018/2019 WiSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                      mean(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2019 SoSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                      mean(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2019/2020 WiSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                      mean(dplyr::filter(ua_help, Studiengang=="MDKE" & !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                      
                      sd(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2015 SoSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                      sd(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2015/2016 WiSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                      sd(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2016 SoSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                      sd(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2016/2017 WiSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                      sd(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2017 SoSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                      sd(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2017/2018 WiSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                      sd(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2018 SoSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                      sd(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2018/2019 WiSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                      sd(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2019 SoSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                      sd(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2019/2020 WiSe"& !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                      sd(dplyr::filter(ua_help, Studiengang=="MDKE" & !is.na(Studienbeginn_Alter))[, "Studienbeginn_Alter"]),
                      
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2015 SoSe" & Sprachniveau=="B2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2015/2016 WiSe" & Sprachniveau=="B2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2016 SoSe" & Sprachniveau=="B2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2016/2017 WiSe" & Sprachniveau=="B2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2017 SoSe" & Sprachniveau=="B2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2017/2018 WiSe" & Sprachniveau=="B2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2018 SoSe" & Sprachniveau=="B2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2018/2019 WiSe" & Sprachniveau=="B2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2019 SoSe" & Sprachniveau=="B2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2019/2020 WiSe" & Sprachniveau=="B2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Sprachniveau=="B2")),
                      
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2015 SoSe" & Sprachniveau=="C1")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2015/2016 WiSe" & Sprachniveau=="C1")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2016 SoSe" & Sprachniveau=="C1")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2016/2017 WiSe" & Sprachniveau=="C1")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2017 SoSe" & Sprachniveau=="C1")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2017/2018 WiSe" & Sprachniveau=="C1")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2018 SoSe" & Sprachniveau=="C1")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2018/2019 WiSe" & Sprachniveau=="C1")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2019 SoSe" & Sprachniveau=="C1")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2019/2020 WiSe" & Sprachniveau=="C1")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Sprachniveau=="C1")),
                      
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2015 SoSe" & Sprachniveau=="C2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2015/2016 WiSe" & Sprachniveau=="C2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2016 SoSe" & Sprachniveau=="C2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2016/2017 WiSe" & Sprachniveau=="C2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2017 SoSe" & Sprachniveau=="C2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2017/2018 WiSe" & Sprachniveau=="C2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2018 SoSe" & Sprachniveau=="C2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2018/2019 WiSe" & Sprachniveau=="C2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2019 SoSe" & Sprachniveau=="C2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2019/2020 WiSe" & Sprachniveau=="C2")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Sprachniveau=="C2")),
                      
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2015 SoSe" & Sprachniveau=="")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2015/2016 WiSe" & Sprachniveau=="")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2016 SoSe" & Sprachniveau=="")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2016/2017 WiSe" & Sprachniveau=="")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2017 SoSe" & Sprachniveau=="")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2017/2018 WiSe" & Sprachniveau=="")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2018 SoSe" & Sprachniveau=="")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2018/2019 WiSe" & Sprachniveau=="")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2019 SoSe" & Sprachniveau=="")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Studienbeginn =="2019/2020 WiSe" & Sprachniveau=="")),
                      nrow(dplyr::filter(ua_help, Studiengang=="MDKE" & Sprachniveau==""))),nrow=11, byrow=FALSE)


rownames(stats_DKE) <- c("2015 SoSe", "2015/2016 WiSe", "2016 SoSe", "2016/2017 WiSe", "2017 SoSe", "2017/2018 WiSe", "2018 SoSe", "2018/2019 WiSe", "2019 SoSe", "2019/2020 WiSe", "Insgesamt")
colnames(stats_DKE) <- c("Anzahl", "Geschlecht M", "Geschlecht W", "HZB-Note", "SD-Abweichung von Mean", "Menge, die Note hat", "Alter", "SD Alter", "B2", "C1", "C2", "Sprache leer")
stats_DKE <- as.table(stats_DKE)

return(stats_DKE)

}




plotting_df <- function(){
  
  lu$LV_Note.Disc <- cut(lu$LV_Note, c(-Inf, 1.5, 2.5, 3.5, 4, 5), 
                         labels = c("[1,1.5]", "(1.5,2.5]", "(2.5,3.5]", "(3.5,4]", "5"))
  
  ###### Master of Science & mitgebrachte Leistungen aus Statistik entfernen
  extern <- dplyr::filter(lu, LV_Extern=="J")
  lu_help <- dplyr::setdiff(lu, extern)
  extern <- dplyr::distinct(dplyr::select(extern, pseudo_matriculation_nbr))

  lu_help <- dplyr::filter(lu_help, LV_Themengebiet != "Zusaetzliche Leistungen")
 # lu_help <- dplyr::filter(lu_help, LV_Datum!="")
  lu_help$start_as_num <- make_semester_to_num(lu_help$Startsemester)
  lu_help$LVDate_as_num <- make_date_to_semester(lu_help$LV_Datum)
  lu_help$LVDate_as_num <- make_semester_to_num(lu_help$LVDate_as_num)
  lu_help$semesters_since_start <- (lu_help$LVDate_as_num - lu_help$start_as_num +1)
  
  lu_ <- lu_help %>% # get row of last attented course
    group_by(pseudo_matriculation_nbr) %>%  
    arrange(desc(semesters_since_start)) %>%
    slice(1)
  lu_ <- dplyr::select(lu_, pseudo_matriculation_nbr, semesters_since_start)
  lu_$expected_cp <- (lu_$semesters_since_start * 30)
  lu_$expected_cp <- cut_max_cp(lu_$expected_cp)
  # lu_ enth‰lt je Student Anzhal der geleisteten FS und erwartete CP
  
  lu_K1 <- lu_help %>%
    group_by(pseudo_matriculation_nbr) %>%
    summarise(sum = cut_max_cp(sum(LV_CP, na.rm=TRUE))) #Summe tatsaechlich erreichter CP
  lu_K1 <- dplyr::full_join(lu_, lu_K1, by="pseudo_matriculation_nbr")
  lu_K1$criterion1 <- (lu_K1$sum / lu_K1$expected_cp)
  lu_K1 <- dplyr::select(lu_K1, pseudo_matriculation_nbr, criterion1)
  lu_K1$criterion1[is.na(lu_K1$criterion1)] <- 0
  # lu_K1 enth‰lt je Student Wert f¸r K1
  
  ua_help <- dplyr::filter(ua, pseudo_matriculation_nbr != "?")
  
  df <- dplyr::left_join(ua_help, lu_help, by = "pseudo_matriculation_nbr") %>%
    group_by(pseudo_matriculation_nbr) %>% 
    summarise(criterion2 = (sum(LV_Status == "bestanden", na.rm = T) / sum(LV_Status %in% c("bestanden", "nicht bestanden"), na.rm = T)),
              criterion3 = mean(LV_Note[LV_Status == "bestanden" & LV_Note < 5], na.rm = T)) %>%
    inner_join(ua_help)
  df <- dplyr::left_join(df, lu_K1, by="pseudo_matriculation_nbr")
  
  # beseitige Fehler bei der Benennung 
 # df$allg_Region <- adjust_region(df$allg_Region)
  #ua$allg_Region <- adjust_region(ua$allg_Region)
  
  # Kategorien f¸r die Kriterien 
  df$criterion1_disc <- cut(df$criterion1 * 100, 
                            c(-Inf,50,75,100, Inf), 
                            c("[0%,50%]",
                              "(50%,75%]",
                              "(75%,100%]",
                              ">100%"))
  df$criterion2_disc <- cut(round(df$criterion2 * 100), 
                            c(-Inf,25,50,75,Inf), 
                            c("[0%,25%]",
                              "(25%,50%]",
                              "(50%,75%]",
                              "(75%,100%]"))
  df$criterion3_disc <- addNA(cut(df$criterion3, c(0,1.5,2.5,3.5,4.5,5)))
  
  df <- df %>% mutate(Kohorte = paste0(Studiengang, "\n", Studienbeginn))
  
  variety=df$Studienbeginn
  Studiengang=df$Studiengang
  
  note=df$criterion1
  data=data.frame(variety, Studiengang, note)
  return(data)
} 
   

