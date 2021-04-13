library(stringi)
library(rio)
library(dplyr)

setwd("C:/Users/ysa pc/Desktop/internship/HiWi/qualiman/")

#' R script which takes the mat. numbers for which the pseudo code needs to be generated
#' it also should take the file where the pseudo numbers are already there so to check there
#' is no conflict with the alpha-numeric nummers already there

#pseudonumbers to exam-data file

exam_data_file <- import('./processed_data/exams/allexams.xlsx')
pseudomatlist <- import('./pseudo_numbers_list.csv')

samp <- dplyr::inner_join(exam_data_file,pseudomatlist,by = c("matriculation_number" = "matriculation"))

samp$SoSe20 <- ifelse(samp$subject_year == '20', 1,0)

export(samp,'exam_data_file_pseudomat.csv')



#--------------------------------------------------------
exam_office_data <-
  import('./processed_data/german_international(pramtoffice)/cateogorised_data_touse2.xlsx')


cohort <- import('./processed_data/database/kohorten.csv')

exam_office_data_2 <- exam_office_data(semester, c("BSc_IF_SS17
", "WS18", "WS19"), 
                                          c(145, 'WS18-19', 'WS19-20'), F, list(case_insensitive = TRUE))


library(stringr)
exam_office_data$extra <- str_sub(exam_office_data$`geb. am`, start=0, end=-11)


application_sem <-gsub("\\T","",exam_office_data$`geb. am`)


check_for_repetition <- import('./masters_students.xlsx')

matriculation <- (exam_office_data$`Matrikel-Nr.`)

str_array <- stri_paste(stri_rand_strings(n = length(matriculation), length = 6, '[A-Z0-9]'))


alphanumeric_store <- data.frame(matriculation,str_array)

match(alphanumeric_store$str_array, check_for_repetition$pseudo_matriculation_nbr)



a <-c('hello',95,'cat',79,'hey')
b <-c('hey',95,'hello')

locations <- match(alphanumeric_store$str_array, check_for_repetition$pseudo_matriculation_nbr)

locations <- locations[!is.na(locations)]

str_array <- stri_paste('B', stri_rand_strings(n = length(locations), length = 5, '[A-Z0-9]'))

alphanumeric_store$str_array[locations] <- str_array

export(alphanumeric_store,'pseudo_numbers_list.csv')

exam_data <- import('./processed_data/database/leistungsdaten.csv')

exam_data <- exam_data %>% mutate(cp_awarded1 = ifelse(note == 5, 0, cp_awarded))

exam_data <- exam_data %>% mutate(lu_status = ifelse(cp_awarded1 == 0, 1, 2))

export(exam_data,"dwmd.csv")



