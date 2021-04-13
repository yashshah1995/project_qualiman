library(dplyr)
library(rio)
library(magrittr)

setwd("C:/Users/ysa pc/Desktop/internship/HiWi/qualiman/")

international_df <- import('./internationalstudentsSS17-WS19-20.csv')
pramt_files <- list.files(path = './data - Copy/pramt/',recursive = TRUE)

temptable <- NULL
temptable <- as.data.frame(temptable)


for (i in 1:length(pramt_files)){
  
  pramt <- import(sprintf('./data - Copy/pramt/%s',pramt_files[i]))
  pramt <- mutate(pramt, semester = tools::file_path_sans_ext(pramt_files[i]))
  
  
  locations <- match(international_df$`Matrikel-Nr.`
                     , pramt$`Matrikel-Nr.`)
  locations <- locations[!is.na(locations)]
  pramt <- pramt %>% mutate(international = ifelse(row_number() %in% locations, 1, 0))
  
  temptable <- dplyr::bind_rows(temptable,pramt)

  
  }
  
export(temptable,'cateogorised_data.csv')









