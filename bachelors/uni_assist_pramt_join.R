library(dplyr)
library(rio)
library(magrittr)

setwd("C:/Users/ysa pc/Desktop/internship/HiWi/qualiman/")

uniassist_files <- list.files(path = './data/uni-assist/',recursive = TRUE)
pramt_files <- list.files(path = './data/pramt/',recursive = TRUE)

pramt <- import('./processed_data/international_students/internationalstudentsSS17-WS19-20_corrected.csv')

temptable <- NULL
temptable <- as.data.frame(temptable)

for (i in 1:length(uniassist_files)){
  
  uniassist <- import(sprintf('./data/uni-assist/%s',uniassist_files[i]))
  #uniassist <- uniassist %>% dplyr::select(-one_of(droplist_uniassist))

  #pramt <- pramt %>% dplyr::select(-one_of(droplist_pramt))
    
  samp <- dplyr::inner_join(uniassist,pramt,by = c('Nachname', 'Vorname'))
    

  temptable <- dplyr::bind_rows(temptable,samp)
    
    
  }
  



droplist_uniassist <- c("Abschl.-Note","Fehlende Unterl.",
                        "Vollst-Datum","Status","Fachbindung","Sonstige Bemerkung",
                        "Note Studienkolleg","Note Studienkolleg",
                        "Bisherige Semester",	"Studienjahre",	
                        "TestAS Sprache",	"TestAS Datum",	
                        "TestAS Standardwert Kerntest",	
                        "TestAS Modul",	
                        "TestAS Standardwert Modultest",
                        "Exportdatum",	"Bewertung")


droplist_pramt <- c("Status","M/W","abschl","stg")


# international_df <- function(uniassist, pramt){
#   
#   uniassist <- uniassist %>% dplyr::select(-one_of(droplist_uniassist))
#   pramt <- pramt %>% dplyr::select(-one_of(droplist_pramt))
#   
#   samp <- dplyr::inner_join(uniassist,pramt,by = c("Nachname", "Vorname"))
#   
#   samp <- samp %>% dplyr::mutate(applicationsem = tools::file_path_sans_ext(uniassist_files[i]),
#                          admissionsem = tools::file_path_sans_ext(pramt_files[i]))
#   
#   temptable <- dplyr::bind_rows(temptable,samp)
#   
#   return(temptable)
#   
# }



for (i in 1:length(uniassist_files)){
  
  uniassist <- import(sprintf('./data/uni-assist/%s',uniassist_files[i]))
  uniassist <- uniassist %>% dplyr::select(-one_of(droplist_uniassist))
  
  
  for(j in 1:length(pramt_files)){
    pramt <- import(sprintf('./data/pramt/%s',pramt_files[j]))
    
    #pramt$`geb. am` <- as.character(pramt$`geb. am`)
    #sapply(pramt, class)
    
    pramt <- pramt %>% dplyr::select(-one_of(droplist_pramt))
    
    samp <- dplyr::inner_join(uniassist,pramt,by = c('Nachname', 'Vorname'))
    
    samp <- samp %>% dplyr::mutate(applicationsem = tools::file_path_sans_ext(uniassist_files[i]),
                                   admissionsem = tools::file_path_sans_ext(pramt_files[j]))
    
    temptable <- dplyr::bind_rows(temptable,samp)
    
    
  }
 
}

export(temptable,'internationalstudentsSS17-WS19-20_corrected.csv')


#' (neat trick use hash followed by apostrophe)
#' for multiline comment
#' THere's cases of admission defferment where students
#' might apply and his admission is defered to later semester
#' so its neccessary to check one uni-assist list with every
#' pramt data. Adittionally we create another dataframe based on pramt data where we
#' add an additional column international /german. 
#' Also because we add the semester they finally took admission in from pramt. It will also
#' have info. of the study course. So essentially I will have 3 frames. One with information
#' about internationals with their properties. One of pramt with international/german and 
#' admission semester details .



#pramt$`geb. am` <- as.character(pramt$`geb. am`)
#samp1 <- dplyr::inner_join(uniassist,pramt,by = c("Geburtsdatum" = "geb. am"))
#sapply(uniassist1, class)
#sapply(pramt, class)




for (i in 1:length(uniassist_files)){
  
  uniassist <- import(sprintf('./data/uni-assist/%s',uniassist_files[i]))
  uniassist <- uniassist %>% dplyr::select(-one_of(droplist_uniassist))
  
  uniassist <- uniassist %>% dplyr::mutate(applicationsem = tools::file_path_sans_ext(uniassist_files[i]))
  temptable <- dplyr::bind_rows(temptable,uniassist)
    
    
  }
  


export(temptable,'tempuni.csv')

