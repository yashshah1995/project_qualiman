library(dplyr)
library(rio)
library(magrittr)
library(stringr) 
library(reshape2)
library(ggplot2)

setwd("C:/Users/ysa pc/Desktop/internship/HiWi/qualiman/")
exam_office_data <- import('./processed_data/german_international(pramtoffice)/cateogorised_data_touse.xlsx')

#International student EDA---------------------------------------------


uniassist_data <- import('processed_data/international_students/internationalstudentsSS17-WS19-20_corrected.csv')

#' The number of actual students reduced because some students applied to both degrees
#' IF and WIF, some applied previously and got rejected. All matched because of inner 
#' join. The query to filter them out while joining would be too much work, given the
#' amount of data. So better I manually removed it.

 
#country

uniassist_data %>% group_by(Heimatland) %>% summarise(counts = n()) %>% ggplot(., 
       aes(x = Heimatland, y = counts)) + 
  geom_col() + 
  geom_label(aes(x = Heimatland, y = counts,label = counts)) +
  theme(legend.position = "none") + 
  labs(x = 'country', y = 'count') + theme(axis.text.x = element_text(angle = 90))

#Test center

    #AUsland, inland

uniassist_data %>% group_by(`In/Ausland`) %>% summarise(counts = n()) %>% 
  ggplot(., 
       aes(x = `In/Ausland`, y = counts)) + 
  geom_col() + 
  geom_label(aes(x = `In/Ausland`, y = counts,label = counts)) +
  theme(legend.position = "none") + 
  labs(x = 'exam location', y = 'count')


    #wherre in inland


uniassist_data %>% filter(`In/Ausland` == 'Inland') %>% group_by(`Land des Abschlusses`) %>% summarise(counts = n())  %>% ggplot(., 
       aes(x = `Land des Abschlusses`, y = counts)) + 
  geom_col() + 
  geom_label(aes(x = `Land des Abschlusses`, y = counts, label = counts)) +
  theme(legend.position = "none") + 
  labs(x = 'exam location', y = 'count')



#Language skills

#'Keywords - GI - Goethe institute
#'DSH
#'telc
#'FSP
#'TestDAF
keywords <- as.matrix(c("FSP", "telc", "GI", "DSH","TestDaF","TNB", "DSD-2"))

uniassist_data <- import('processed_data/international_students/internationalstudentsSS17-WS19-20_corrected.csv')

lang_col <- sapply(stri_extract_all_regex(uniassist_data$Sprachbemerkung, paste(keywords[,1], collapse = '|')), toString)


#lang_col <- as.data.frame(gsub("(.*),.*", "\\1", lang_col))

uniassist_data$germanexam = gsub("(.*?),.*", "\\1", lang_col)


#Schlueko results

exam_data <- import('./processed_data/exams/allexams.xlsx')

exam_data <- exam_data %>% filter(subject_name == 'schlueko')


matched_schlueko_res <- 
  dplyr::inner_join(uniassist_data,exam_data,by = c("Matrikel-Nr." = "matriculation_number"))

#find unique students w.r.t to germanexam

unique_schleuko <-  matched_schlueko_res[!rev(duplicated(rev(matched_schlueko_res$`Matrikel-Nr.`))),]

#First attempt------------------------------------------
conditdf <- data.frame(
  degree_year = c('17', '17-18', '18', '18-19', '19', '19-20'),
  subject_year = c('17-18', '18', '18-19', '19', '19-20', '20')
)

grouper <- unique_schleuko  

grouper <- conditdf %>%
  rowwise() %>%
  do(
    dfs = inner_join(as.data.frame(.), grouper)
  )

grouper <- do.call(rbind.data.frame, grouper$dfs)


grouper <- grouper %>%
  group_by(germanexam, degree_year,subject_year,subject_semester,degree_semester) %>% 
  summarise(counts = n()) 


grouper$start_y <- 1:nrow(grouper) 
grouper$end_y <- 1:nrow(grouper)  
grouper <- as.data.frame(grouper)


grouper %>% mutate_each(funs(as.character), germanexam) %>%
  ggplot(., aes(x = paste(degree_semester,degree_year,sep=""), y = start_y,color = germanexam)) + 
  geom_segment(aes(xend = paste(subject_semester,subject_year,sep=""), yend = end_y),arrow = arrow()) + 
  scale_x_discrete(name ="Time",limits=c("WS16-17","SS17","WS17-18","SS18","WS18-19","SS19","WS19-20","SS20")) +
  ggtitle("schlueko")+
  geom_text(data = grouper,aes( y=start_y, label=counts), size=3.8, hjust=1.5, colour="black") +
  theme(axis.text.x = element_text(angle = 90), 
        axis.title.y = element_blank(),axis.text.y = element_blank(), plot.title = element_text(hjust = 0.5))

#attemp outlier--------------------------------------------------


outliers <- unique_schleuko  

grouper <- unique_schleuko 

grouper <- conditdf %>%
  rowwise() %>%
  do(
    dfs = inner_join(as.data.frame(.), grouper)
  )

grouper <- do.call(rbind.data.frame, grouper$dfs)

outliers <- outliers[!(outliers$`Matrikel-Nr.` %in% grouper$`Matrikel-Nr.`),]

outliers <- outliers%>%
  group_by(germanexam, degree_year,subject_year,subject_semester,degree_semester) %>% 
  summarise(counts = n()) 

outliers$start_y <- 1:nrow(outliers) 
outliers$end_y <- 1:nrow(outliers)  
outliers <- as.data.frame(outliers)

outliers %>% mutate_each(funs(as.character), germanexam) %>%
  ggplot(., aes(x = paste(degree_semester,degree_year,sep=""), y = start_y,color = germanexam)) + 
  geom_segment(aes(xend = paste(subject_semester,subject_year,sep=""), yend = end_y),arrow = arrow()) + 
  scale_x_discrete(name ="Time",limits=c("WS16-17","SS17","WS17-18","SS18","WS18-19","SS19","WS19-20",
                                         "SS20")) +
  ggtitle("schlueko")+
  geom_text(data = outliers,aes( y=start_y, label=counts), size=3.8, hjust=1.5, colour="black") +
  theme(axis.text.x = element_text(angle = 90), 
        axis.title.y = element_blank(),axis.text.y = element_blank(), plot.title = element_text(hjust = 0.5))




#---------------------------------------------------------------

#Find their tries, add a duplicate column 

#Only one student repeats

#See the first try average result according to germanexams (boxplots with counts)

ggplot(unique_schleuko, aes(x = germanexam, y = points)) + geom_boxplot() +
  geom_point()


print(unique_schleuko %>% group_by(germanexam) %>% summarise(avg = mean(points), std. =                                                                            sd(points)))

#Too less data, for schlueko

#-------------------------------------------------------------------------


library(stringi)
language_certi <- as.data.frame(table(sapply(stri_extract_all_regex(uniassist_data$Sprachbemerkung, paste(keywords[,1], collapse = '|')), toString)))

#' Some students have given multiple german exams for different levels,
#' Different as in different certificates (We take the latest one here)
#' from the given dates

language_certi[1,2] <- language_certi[1,2] + 3
language_certi[4,2] <- language_certi[4,2] + 3
language_certi[12,2] <- language_certi[12,2] + 1


language_certi <- language_certi[-c(2, 3, 5, 6, 7, 13), ]

ggplot(language_certi, 
       aes(x = Var1, y = Freq)) + 
  geom_col(aes(fill = Var1)) + 
  geom_label(aes(y = Freq,label = Freq)) +
  theme(legend.position = "none") + 
  labs(x = 'qualification_exam', y = 'count')


#








#language level

level_keyword <- as.matrix(c("B1","B2","C1","C2","DSH-2"))

language_level <- sapply(stri_extract_all_regex(uniassist_data$Sprachbemerkung, 
                              paste(level_keyword[,1], collapse = '|')), toString)

#' for students with multiple levels of language (rows with string value >2)
#' replace with highest level

language_level <- gsub(".*C1.*", "C1", language_level, perl=TRUE) 
language_level <- gsub(".*B2.*", "B2", language_level, perl=TRUE) 
language_level <- gsub(".*DSH-2.*", "DSH-2", language_level, perl=TRUE) 

language_level <- as.data.frame(table(language_level))

ggplot(language_level, 
       aes(x = language_level, y = Freq)) + 
  geom_col(aes(fill = language_level)) + 
  geom_label(aes(y = Freq,label = Freq)) +
  theme(legend.position = "none") + 
  labs(y = 'count')



#For NA values check for what test 

#which(language_level %in% c('NA'))

NA_values <- sapply(stri_extract_all_regex(uniassist_data$Sprachbemerkung, 
            paste(keywords[,1], collapse = '|')), toString)[c(which(language_level %in% c('NA')))]

NA_values <- gsub(".*FSP.*", "FSP", NA_values, perl=TRUE) 

NA_values <- as.data.frame(table(NA_values))

ggplot(NA_values, 
       aes(x = NA_values, y = Freq)) + 
  geom_col(aes(fill = NA_values)) + 
  geom_label(aes(y = Freq,label = Freq)) +
  theme(legend.position = "none") + 
  labs(x = 'qualification_exam', y = 'count')

ggsave('./plots/german_level_2.png',dpi = 800)

#Avg semester gap between applying and getting
#group by 


application_sem <-gsub("(^.*_)","",uniassist_data$applicationsem)
admission_sem <- gsub("(^.*_)","",uniassist_data$admissionsem)

application_sem <- stri_replace_all_regex(application_sem, c("WS17", "WS18", "WS19"), 
                                          c('WS17-18', 'WS18-19', 'WS19-20'), F, list(case_insensitive = TRUE))

semester_order <- c("SS17","WS17-18","SS18","WS18-19","SS19","WS19-20")

gap <- integer(0)

for (i in 1:length(application_sem)){
  
  gap[i] <- match(admission_sem[i],semester_order) - match(application_sem[i],semester_order)
  
}

gap1 <- as.data.frame(table(gap))

ggplot(gap1, 
       aes(x = gap, y = Freq)) + 
  geom_col(aes(fill = gap)) + 
  geom_label(aes(y = Freq,label = Freq)) +
  theme(legend.position = "none") + 
  labs(x = 'semester gap', y = 'count')



#gap_marks_correlation

#gap[c(gap[gap != 0])]

uniassist_data$`HZB-Note`[which(gap %in% c(gap[gap != 0]))]

#gap_germanlevel_correlation

language_level[which(gap %in% c(gap[gap != 0]))]


#-----------------------------------------------------------------
#Total students
total_students <- nrow(exam_office_data)


#International vs german students--------------------------------------------


ger_vs_int_cts <- as.data.frame(table
                                (exam_office_data$international))


ggplot(ger_vs_int_cts, 
            aes(x = Var1, y = Freq)) + 
geom_col(aes(fill = Var1)) + 
  geom_label(aes(y = Freq,label = Freq)) +
  scale_x_discrete(labels= c('german','international')) +
  theme(legend.position = "none") + 
  labs(x = 'student type', y = 'count')

ggsave('./plots/german_vs_intn.png',dpi = 800)

#course wise student division---------------------------------------------------------

tmpdegree <- str_sub(exam_office_data$semester,5,-6)

for (i in 1:length(tmpdegree)){
  if (substring(tmpdegree[i], 1, 1) == "I"){
    tmpdegree[i] <- "IF"
    
  }
  else{
    tmpdegree[i] <- "WIF"
  }
}

exam_office_data$tmpdegree = tmpdegree

course_div <- as.data.frame(table(tmpdegree,exam_office_data$international))

ggplot(course_div, aes(x = tmpdegree,y = Freq))+
  geom_col(aes(fill = Var2),position = 'dodge') + 
  geom_label(aes(y = Freq,label = Freq),
             position = position_dodge2(0.9))+
  labs(x = 'degree',fill = "type") +
  scale_fill_discrete(labels = c("German","Intn")) 

ggsave('./plots/course_wise_div.png',dpi = 800)

#barplot(tempo, 
 #       xlab="German vs International", col=c("darkblue","red"),
  #      ylim=c(0,250),legend = rownames(tempo), beside=TRUE,space=c(0.2,0,0.2,0))

#df <- melt(exam_office_data,id.vars=unique(tmpdegree))

#tempo
#ggplot(exam_office_data, aes(unique(international), tmpdegree)) +   
 # geom_bar(aes(fill = international), position = "dodge", stat="identity")

#ggplot(df, aes(value, A)) + 
  #geom_bar(position="dodge")

#-----------------------------Semester wise admission-----------------

temoe <- gsub("(^.*_)","", exam_office_data$semester)
#temp_degreesem <- gsub("([0-9,-])","", temoe)
#temp_degreesem

semester_div <- as.data.frame(table(temoe,exam_office_data$international))

semester_div <- semester_div %>%
  arrange(match(temoe, c("SS17", "WS17-18","SS18","WS18-19","SS19","WS19-20")))

semester_div$temoe <- factor(semester_div$temoe, levels = unique(semester_div$temoe))

ggplot(semester_div, aes(x = temoe,y = Freq))+
  geom_col(aes(fill = Var2),position = 'dodge') + 
  geom_label(aes(y = Freq,label = Freq),
             position = position_dodge2(0.9))+
  labs(x = 'degree',fill = "type") +
  scale_fill_discrete(labels = c("German","Intn")) 

ggsave('./plots/semester_wise_admission.png',dpi = 800)

#logscaleplot------------------------------------------------
ggplot(semester_div, aes(x = temoe,y = Freq))+
  geom_col(aes(fill = Var2),position = 'dodge') + scale_y_log10() +
  labs(x = 'degree',fill = "type") + geom_label(aes(y = Freq,label = round(log10(semester_div$Freq),2)),
  position = position_dodge2(0.9)) +
  scale_fill_discrete(labels = c("German","Intn")) 

ggsave('./plots/semester_wise_admission(normalised.png',dpi = 800)

#Degreewise admission----------------------------------------

degree_div <- as.data.frame(table(temoe,exam_office_data$tmpdegree, exam_office_data$international))

degree_div <- degree_div %>%
  arrange(match(temoe, c("SS17", "WS17-18","SS18","WS18-19","SS19","WS19-20")))

degree_div$temoe <- factor(degree_div$temoe, levels = unique(degree_div$temoe))

#degree_div_1 <- degree_div[(degree_div$Var2 == 'IF'),]
#degree_div_2 <- degree_div[(degree_div$Var2 == 'WIF'),]


#ggplot(degree_div_2,aes(x = Var3, Freq)) + 
 # geom_col(aes(fill = temoe),position = 'dodge') + 
  #geom_label(aes(y = Freq,label = Freq),
   #          position = position_dodge2(0.9))

levels(degree_div$Var3) <- c("German", "International")

ggplot(degree_div,aes(x = Var2, y = Freq, fill = temoe)) + 
  geom_bar(stat='identity',position=position_dodge()) + 
 facet_wrap(~Var3) + labs(x = 'Student type', fill = 'semester') + 
  geom_label(aes(y = Freq,label = Freq),position = position_dodge2(0.9))

ggsave('./plots/course_wise_persem_admission.png',dpi = 800)

#facet wrap label remaining 

#bp <- barplot(temoe3, 
 #       xlab="German vs International",col=c("darkblue","red","green","pink","yellow","orange"),
  #      ylim=c(0,120),legend = rownames(temoe3), 
   #     args.legend = list(x = "topleft", bty = "n", inset=c(0, -0.2)),
    #    beside=TRUE) + geom_label(aes(y = colnames(temoe3),label = temoe3))
  
#text(bp, temoe3 + 3, labels = temoe3)

#Gender Ratio----------------------------------------------

#table(exam_office_data$`M/W`)

temoe4 <- table(exam_office_data$`M/W`,exam_office_data$international)

bp <- barplot(temoe4,xlab="German vs International",col=c("darkblue","red"),
              ylim = c(0,300),legend = rownames(temoe4), beside=TRUE,space=c(0.2,0,0.2,0))
              
text(bp, temoe4 + 10, labels = temoe4)

#---------------------------------------------------------------------- EXAM data

subjects_total_data <- import('./processed_data/all_examtogether_processed.xlsx')

#Total 3 subjects exams

# 683 entries

# exams given (german vs international)

temoe5 <- table(subjects_total_data$student_type,subjects_total_data$subject_name)

bp <- barplot(temoe5,xlab="Subjects",col=c("darkblue","red"),
              ylim = c(0,300),legend = rownames(temoe5), beside=TRUE)

text(bp, temoe5 + 6, labels = temoe5)


# exams given each semester, each subject, by each individual 
#student group

type0 <- subjects_total_data[subjects_total_data$student_type == '0',] 
tempo <- table(type0$student_type, type0$subject_name, type0$degree_year)
tempo <- as.data.frame(tempo)
colnames(tempo) [2] <- "Subjects"
colnames(tempo) [3] <- "semester"

plot <- ggplot(tempo, aes(Subjects,Freq,fill = semester))
plot + geom_bar(stat = "identity", position = 'dodge')+
#scale_y_continuous(breaks=c(0,3,6,9,12))+
  geom_text(aes(Subjects,label = Freq), 
            position=position_dodge(width=0.9),size = 4)









