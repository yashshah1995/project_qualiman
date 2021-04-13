library(dplyr)
library(rio)
library(magrittr)
library(stringr) 

WS <- c('10','11','12','01','02','03')
SS <- c('04','05','06','07','08')

setwd("C:/Users/ysa pc/Desktop/internship/HiWi/qualiman/")

aud_exam_files <- list.files(path = './data/exams/aud/',recursive = TRUE)
student_df <- import('./processed_data/german_international(pramtoffice)/cateogorised_data_touse2.xlsx')

student_df$`final confirmation`[student_df$`final confirmation` == -1] <- 1


final_dataset <- NULL
final_dataset <- as.data.frame(final_dataset)

ctr = 5

import_id = sprintf('./data/exams/aud/%s', aud_exam_files[ctr])
tmpschlko <- import(import_id)

#make sure you use unlist to convert the data.frame to a vector containing 
#the wanted names
#colnames(tmpschlko) <- unlist(tmpschlko[row.names(tmpschlko)==2,])

#remove the unneeded row
#tmpschlko <- tmpschlko[!row.names(tmpschlko)==2,]
#tmpschlko <- tmpschlko[!row.names(tmpschlko)==1,]

#sapply(tmpschlko, class)
#sapply(student_df, class)
#tmpschlko <- transform(tmpschlko, `Matr.-Nr.`= as.numeric(`Matr.-Nr.`))


samp <- dplyr::inner_join(student_df,tmpschlko,by = c("Matrikel-Nr." = "Matr.-Nr."))
samp <- samp %>% dplyr::mutate(subjectsem = tools::file_path_sans_ext(aud_exam_files[ctr]))

#immatrikulation
tempimmarticulation <- c(samp$`Matrikel-Nr.`)

#degree name
tmpdegree <- str_sub(samp$semester,5,-6)

for (i in 1:length(tmpdegree)){
  if (substring(tmpdegree[i], 1, 1) == "I"){
    tmpdegree[i] <- "IF"
    
  }
  else{
    tmpdegree[i] <- "WIF"
  }
}

#degree_semester

#samp$semester

temoe <- gsub("(^.*_)","", samp$semester)
temp_degreesem <- gsub("([0-9,-])","", temoe)

# degree Year 
temp_degree_year <- gsub("([A-Z,a-z,_])","", samp$semester)

#aud semester(year month date)

temoe2 <- str_sub(samp$subjectsem,6,-4)

for (i in 1:length(temoe2)){
  
  if (temoe2[i] %in%  WS){
    
    temoe2[i] = 'WS'
    
  } else if (temoe2[i] %in%  SS){
    
    temoe2[i] = 'SS'
    
    
  }  else{
    next
  }
  
}

temp_subj_sem <- temoe2  

#einfinf year 


temoe3 <- str_sub(samp$subjectsem,3,-7)
temp_subj_year <- temoe3 

#Change subject year properly
for (i in 1:length(temp_subj_year)){
  if (temp_subj_sem[i] == 'SS'){
    next
    
  }
  else{
    temp_subj_year[i] <- stri_replace_all_regex(temp_subj_year[i], c("17", "18", "19","20"), 
                                                c('16-17', '17-18', '18-19','19-20'), F, list(case_insensitive = TRUE))
    
    
  }
  
}


temp_sub_name <- replicate(nrow(samp),"aud")

#subject participant degree (EDA for degree change)


temp_participant_degree <- gsub("(^.-)","", samp$`Studien-`)
temp_participant_degree <- gsub("(^. )","", samp$Studiengang) #only for 4th
temp_participant_degree <- gsub("(^.-)","", samp$Studiengang) #only for 8th



rm(temoe)
rm(temoe2)
rm(temoe3)


temp_points <- samp$Note

temptable <- data.frame(matriculation_number= tempimmarticulation, student_type = samp$`final confirmation`,
                        degree_name = tmpdegree, degree_semester = temp_degreesem,
                        degree_year = temp_degree_year, subject_name = temp_sub_name, 
                        subject_semester = temp_subj_sem, 
                        subject_year = temp_subj_year, points = temp_points, participant_degree = temp_participant_degree)

#sapply(temptable, class)

#temptable <- transform(temptable, points= as.numeric(points))

rm(tmpdegree)
rm(temp_degree_year)
rm(temp_degreesem)
rm(temp_points)
rm(temp_sub_name)
rm(temp_subj_sem)
rm(temp_subj_year)
rm(tempimmarticulation)
rm(temp_participant_degree)

final_dataset <- dplyr::bind_rows(final_dataset,temptable)

rm(temptable)

export(final_dataset,'audexam_processed.csv')









