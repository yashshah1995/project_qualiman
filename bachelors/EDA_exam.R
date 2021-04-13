lapply(names(sessionInfo()$otherPkgs), function(pkgs)
  detach(
    paste0('package:', pkgs),
    character.only = T,
    unload = T,
    force = T
  ))


(.packages())

library(dplyr)
library(rio)
library(magrittr)
library(stringr) 
library(reshape2)
library(ggplot2)
library(CGPfunctions)


setwd("C:/Users/ysa pc/Desktop/internship/HiWi/qualiman/")

exam_data <- import('./processed_data/exams/allexams.xlsx')


#Total entries

nrow(exam_data)

# per subject

tab1 <- as.data.frame(table(exam_data$subject_name))


ggplot(tab1,aes(x = "", y =  Freq, fill = Var1)) + 
  geom_col(position = 'stack', width = 1) +
  geom_text(aes(label = Freq), position = position_stack(vjust = 0.5)) + 
  theme_classic() + 
  coord_polar("y") +
  theme(plot.title = element_text(hjust=0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = "Category",
       x = NULL,
       y = NULL) +
  scale_fill_manual(values=c("#EC7A78", "#55A5C4", "#C18ABC")) +
  theme_void()

ggplot(tab1,aes(x = "", y =  Freq, fill = Var1)) + 
  geom_col(position = 'stack', width = 1) +
 
  geom_text(aes(label = paste(round(Freq / sum(Freq) * 100, 1), "%")),
            position = position_stack(vjust = 0.5)) + 
  theme_classic() + 
  coord_polar("y") +
  theme(plot.title = element_text(hjust=0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = "Category",
                       x = NULL,
                       y = NULL,
                       title = "Pie Chart of Blue Chip Makeup") 


# per student type

tab2 <- as.data.frame(table(exam_data$student_type))


ggplot(tab2,aes(x = "", y =  Freq, fill = Var1)) + 
  geom_col(position = 'stack', width = 1) +
  
  geom_text(aes(label = paste(round(Freq / sum(Freq) * 100, 1), "%")),
            position = position_stack(vjust = 0.5)) + 
  theme_classic() + 
  coord_polar("y") +
  theme(plot.title = element_text(hjust=0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = "Category",
       x = NULL,
       y = NULL,
       title = "Pie Chart of Blue Chip Makeup") 



# degree type

tab3 <- as.data.frame(table(exam_data$degree_name))


ggplot(tab3,aes(x = "", y =  Freq, fill = Var1)) + 
  geom_col(position = 'stack', width = 1) +
  
  geom_text(aes(label = paste(round(Freq / sum(Freq) * 100, 1), "%")),
            position = position_stack(vjust = 0.5)) + 
  theme_classic() + 
  coord_polar("y") +
  theme(plot.title = element_text(hjust=0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = "Category",
       x = NULL,
       y = NULL,
       title = "Pie Chart of Blue Chip Makeup") 


# semester wise

#table(exam_data$subject_semester)

#year & semester wise

tab4 <- as.data.frame(table(exam_data$subject_year))

ggplot(tab4, 
       aes(x = Var1, y = Freq)) + 
  geom_col() + 
  geom_label(aes(y = Freq,label = Freq)) +
  theme(legend.position = "none") + 
  labs(x = 'country', y = 'count') 


#Subject pattern-------------------------------------------------------------------

# degree wise

tab5 <- as.data.frame(table(exam_data$subject_name, exam_data$degree_name))

tab1

pie_labels <- (tab5 %>% group_by(Var2) %>% 
           mutate(per=paste0(round(Freq/sum(Freq)*100, 2), "%")) %>% 
           ungroup %>% select(per))$per


ggplot(tab5, aes(x = "", y = Freq, fill = factor(Var1))) +
  geom_bar(stat = "identity", position = position_fill()) +
  geom_text(aes(label = paste(pie_labels)), position = position_fill(vjust = 0.5)) +
  coord_polar(theta = "y") +
  facet_wrap(~ Var2) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank()) + 
  theme(legend.position='bottom') + 
  guides(fill=guide_legend(nrow=2, byrow=TRUE))

#subdivide
degree_1 <- as.data.frame(table(exam_data$degree_name,exam_data$student_type,
                                exam_data$subject_name))

#Informatics


degree_1_1 <- degree_1[degree_1$Var1 == 'IF',] 
pie_labels <- (degree_1_1 %>% group_by(Var2) %>% 
                 mutate(per=paste0(round(Freq/sum(Freq)*100, 2), "%")) %>% 
                 ungroup %>% select(per))$per


labels <- c('0' = " german (319)", '1' = "foreign (66)")
ggplot(degree_1_1, aes(x = "", y = Freq, fill = factor(Var3))) +
  geom_bar(stat = "identity", position = position_fill()) +
  geom_text(aes(label = paste(pie_labels)), position = position_fill(vjust = 0.5)) +
  coord_polar(theta = "y") +
  facet_wrap(~ Var2,labeller=labeller(Var2 = labels)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank()
  ) + ggtitle('Informatics (385)') + 
  theme(plot.title = element_text(hjust = 0.5),legend.position='bottom',strip.background = element_rect(colour="black", fill="gray")) + 
  guides(fill=guide_legend(nrow=2, byrow=TRUE))+ 
  scale_fill_manual(labels = c("aud (47)", "einfinf (197)", "schlueko (141)"), values=c("#EC7A78", "#55A5C4", "#C18ABC")) + labs(fill = "subjects")

#Business informatics

degree_1_1 <- degree_1[degree_1$Var1 == 'WIF',] 
pie_labels <- (degree_1_1 %>% group_by(Var2) %>% 
                 mutate(per=paste0(round(Freq/sum(Freq)*100, 2), "%")) %>% 
                 ungroup %>% select(per))$per


labels <- c('0' = " german (143)", '1' = "foreign (82)")
ggplot(degree_1_1, aes(x = "", y = Freq, fill = factor(Var3))) +
  geom_bar(stat = "identity", position = position_fill()) +
  geom_text(aes(label = paste(pie_labels)), position = position_fill(vjust = 0.5)) +
  coord_polar(theta = "y") +
  facet_wrap(~ Var2,labeller=labeller(Var2 = labels)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank()
  ) + ggtitle('Business Informatics (225)') + 
  theme(plot.title = element_text(hjust = 0.5),legend.position='bottom',strip.background = element_rect(colour="black", fill="gray")) + 
  guides(fill=guide_legend(nrow=2, byrow=TRUE))+ 
  scale_fill_manual(labels = c("aud (22)", "einfinf (115)", "schlueko (88)"), values=c("#EC7A78", "#55A5C4", "#C18ABC")) + labs(fill = "subjects")


#sidebyside bar plot

degree_1_2 <- as.data.frame(table(exam_data$degree_name,exam_data$student_type,exam_data$subject_name))

percent_labs <- (degree_1_2 %>% group_by(Var3) %>%
  mutate(per=paste0(round(Freq/sum(Freq)*100, 2), "%")) %>% 
  ungroup %>% select(per))$per

ggplot(data = degree_1_2, aes(x = Var1, y = Freq, fill = Var2)) + 
  geom_bar(stat = "identity", width = 1, position = position_dodge()) +
  geom_label(aes(x = Var1, y = Freq, group = Var2,label = paste(Freq)),
            position = position_dodge(width = 1)) +
  geom_text(aes(x = Var1, y = Freq, group = Var2,label = paste(percent_labs)),
            position = position_dodge(width = 1),vjust = 1.9, size = 3.5)+
  facet_wrap(~Var3, strip.position = "bottom", scales = "free_x") +
  xlab("Subjects")+scale_fill_manual(labels = c("Bildungsin.", "Bildungsaus.", 
      "schlueko (88)"), values=c("#EC7A78", "#55A5C4", "#C18ABC")) + labs(fill = "HZB.") + 
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(), strip.placement = "outside",panel.border = element_blank() )

  #student wise

tab6 <- as.data.frame(table(exam_data$subject_name, exam_data$student_type))

pie_labels <- (tab6 %>% group_by(Var2) %>% 
                 mutate(per=paste0(round(Freq/sum(Freq)*100, 2), "%")) %>% 
                 ungroup %>% select(per))$per


ggplot(tab6, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = position_fill()) +
  geom_text(aes(label = paste(pie_labels)), position = position_fill(vjust = 0.5)) +
  coord_polar(theta = "y") +
  facet_wrap(~ Var2) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
  theme(legend.position='bottom') + 
  guides(fill=guide_legend(nrow=2, byrow=TRUE))


#semester wise

tab7 <- as.data.frame(table(exam_data$subject_name, exam_data$subject_semester))

pie_labels <- (tab7 %>% group_by(Var2) %>% 
                 mutate(per=paste0(round(Freq/sum(Freq)*100, 2), "%")) %>% 
                 ungroup %>% select(per))$per


ggplot(tab7, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = position_fill()) +
  geom_text(aes(label = paste(pie_labels)), position = position_fill(vjust = 0.5)) +
  coord_polar(theta = "y") +
  facet_wrap(~ Var2) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
  theme(legend.position='bottom') + 
  guides(fill=guide_legend(nrow=2, byrow=TRUE))



#semester year wise

tab8 <- as.data.frame(table(exam_data$subject_name, exam_data$subject_year))
PlotXTabs2(tab8, y =Var1 , x = Var2, counts = Freq, perc.k = 2, legend.title = "subjects")

#grade distribution
#no point in histogram as its only discreet values

#Normal

tab9 <- as.data.frame(table(exam_data$points)) 
ggplot(tab9 , aes(x = Var1, y = Freq)) + 
  geom_col() +
 geom_label(aes(y = Freq,label = Freq))

#As percentage

#unique_exam_data2 %>% filter(subject_name == 'schlueko', !degree_year %in% c('19', '19-20')) %>% 
 # mutate(start_y = 1:n(),end_y = 1:n(), examresult = case_when(points == 5.0 ~ "fail", TRUE ~"pass")) 

Percentage1 = tab9$Freq/sum(tab9$Freq)*100

ggplot(tab9, aes(x = Var1, y = Percentage1)) + 
geom_col() + geom_label(aes(y = Percentage1,label = round(Percentage1,3)))
                        

#Distribution HZB. wise
#0

tab9_0 <- as.data.frame(table(exam_data[exam_data$student_type == 0,]$points))


Percentage1 = tab9_0$Freq/sum(tab9_0$Freq)*100

ggplot(tab9_0, aes(x = Var1, y = Percentage1)) + 
  geom_col() + geom_label(aes(y = Percentage1,label = round(Percentage1,3)))

#1

tab9_1 <- as.data.frame(table(exam_data[exam_data$student_type == 1,]$points))


Percentage1 = tab9_1$Freq/sum(tab9_1$Freq)*100

ggplot(tab9_1, aes(x = Var1, y = Percentage1)) + 
  geom_col() + geom_label(aes(y = Percentage1,label = round(Percentage1,3)))


table(exam_data$student_type, exam_data$subject_name)

export(as.data.frame(table(exam_data$subject_name,exam_data$student_type)),'exam_record_comparison.csv')

#Degree wise

tab10 <- as.data.frame(table(exam_data$degree_name,exam_data$points))
PlotXTabs2(tab10, y =Var1 , x = Var2, counts = Freq, perc.k = 2)

#Student wise

tab11 <- as.data.frame(table(exam_data$student_type,exam_data$points))
PlotXTabs2(tab11, y =Var1 , x = Var2, counts = Freq, perc.k = 2)

indiviudalsubjHZB <- function(subjnamestr){
  
  tab11_1 <- exam_data[exam_data$subject_name == sprintf('%s',subjnamestr),]
  tab11_1 <- as.data.frame(table(tab11_1$student_type,tab11_1$points))
  ggplot(tab11_1, aes(x = Var2, y = Freq, fill = Var1))+
    geom_bar(stat = "identity", width = 1, position = position_dodge()) +
    geom_label(aes(x = Var2, y = Freq,label = paste(Freq)),
               position = position_dodge(width = 1)) +
    xlab("grade") + scale_fill_manual(labels = c("Bildungsin.", "Bildungsaus."), 
                                      values=c("#EC7A78", "#55A5C4")) + labs(fill = "HZB.") + 
    ggtitle(sprintf('%s',subjnamestr),) +
    theme(plot.title = element_text(hjust = 0.5))

}


indiviudalsubjHZB('einfinf')  

sidebysidesubjHZB <- function(subjnamestr){
  
  
  tab11_1 <- exam_data[exam_data$subject_name == sprintf('%s',subjnamestr),]
  tab11_1 <- as.data.frame(table(tab11_1$student_type,tab11_1$points))
  
  idx.in.list  <- sapply(levels(tab11_1$Var2), find.in.list, from)
  levels(tab11_1$Var2)  <- ifelse(is.na(idx.in.list), levels(tab11_1$Var2), to[idx.in.list])
  
  tab11_1 <- tab11_1 %>% group_by(Var1,Var2) %>%
    summarise(Freq = sum(Freq))
  
  figsave <- PlotXTabs2(tab11_1, y =Var2 , x = Var1, counts = Freq, perc.k = 2,data.label = "both",
                        results.subtitle = F,label.text.size = 4, label.fill.alpha = 0.3, sample.size.label = F,
                        legend.title = "grade") +
    scale_fill_manual(values=c("#C58C2F", "#384BB6", "#D13E3E","#7a55c4","#b6e700")) + 
    scale_x_discrete(name ="HZB.", labels=c("Bildungsin.", "Bildungsaus.")) + 
    coord_flip()
  
  return(figsave)
  
  
  
}


gradehzb1 <- sidebysidesubjHZB('aud')+ theme(axis.title.x=element_blank(),
                                             axis.text.x=element_blank(),
                                             axis.ticks.x=element_blank(), legend.position = "top",
                                             axis.title.y = element_blank(),
)
gradehzb2 <- sidebysidesubjHZB('einfinf')+ theme(axis.title.x=element_blank(),
                                                 axis.text.x=element_blank(),
                                                 axis.ticks.x=element_blank(),
                                                 axis.title.y = element_blank(),legend.position = "none",
)
gradehzb3 <- sidebysidesubjHZB('schlueko')+ theme(axis.title.x=element_blank(),
                                                  axis.text.x=element_blank(),
                                                  axis.ticks.x=element_blank(), legend.position = "none",
                                                  axis.title.y = element_blank(),
)

grid.arrange(gradehzb1, gradehzb2, gradehzb3, nrow=3)


tab11_1 <- exam_data[exam_data$subject_name == 'einfinf',]

tab11_1 <- as.data.frame(table(tab11_1$student_type,tab11_1$points))

idx.in.list  <- sapply(levels(tab11_1$Var2), find.in.list, from)
levels(tab11_1$Var2)  <- ifelse(is.na(idx.in.list), levels(tab11_1$Var2), to[idx.in.list])

temptab <- tab11_1 %>% group_by(Var1,Var2) %>%
  summarise(Freq = sum(Freq))

temptab$subjectname = 'einfinf'

library(gridExtra)

gradehzb3 <- PlotXTabs2(temptab, y =Var2 , x = Var1, counts = Freq, perc.k = 2,data.label = "both",
           results.subtitle = F,label.text.size = 4, label.fill.alpha = 0.3, sample.size.label = F,
           legend.title = "grade") +
   scale_fill_manual(values=c("#C58C2F", "#384BB6", "#D13E3E","#7a55c4","#b6e700")) + 
  scale_x_discrete(name ="HZB.", labels=c("Bildungsin.(531)", "Bildungsaus.(151)")) + 
  coord_flip()
  
gradehzb1 + theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(), legend.position = "none",
                axis.title.y = element_blank(),
                )

grid.arrange(gradehzb1, gradehzb2, gradehzb3, nrow=3)

 #subject wise

tab12 <- as.data.frame(table(exam_data$subject_name,exam_data$points))
PlotXTabs2(tab12, y =Var1 , x = Var2, counts = Freq, perc.k = 2)

sapply(tab12, class)

from <- list(c("1","1.3","1.7"), c("2","2.3", "2.7"), c("3","3.3","3.7"))
to   <- c("1-2", "2-3","3-4")

find.in.list <- function(x, y) match(TRUE, sapply(y, `%in%`, x = x))
idx.in.list  <- sapply(levels(tab12$Var2), find.in.list, from)
levels(tab12$Var2)  <- ifelse(is.na(idx.in.list), levels(tab12$Var2), to[idx.in.list])


tab12$Var2[tab12$Var2 == c('1.3','1.7')] <- '1'

tab13 <- tab12 %>% group_by(Var1,Var2) %>%
  summarise(Freq = sum(Freq))

(tab13 %>% group_by(Var2) %>% summarise(Freq = sum(Freq)))$Freq[1]

gradesumcounter <- (tab13 %>% group_by(Var2) %>% summarise(Freq = sum(Freq)))$Freq

  sprintf("1-2 (%s)",gradesumcounter[1])


PlotXTabs2(tab13, y =Var2 , x = Var1, counts = Freq, perc.k = 2,data.label = "both",
           results.subtitle = F,
           palette = "Paired", label.text.size = 4, label.fill.alpha = 0.3, sample.size.label = F ) + coord_flip()

#' Summarize the grade into 3 for better visibility 1-2,2-4,4-5, Add the subject 
#' distribution pie chart here for better comparison. Do that for every above diagram
#' Finish this tommorow before starting German vs international (Creating an R markdown doc)

#German vs International (exam record)----------------------------------------------------------------
#Find difference pattern

table(exam_data$student_type)

#degree wise
#exam taken in each year-semester 

tab14 <- as.data.frame(table(exam_data$student_type,exam_data$subject_year,exam_data$degree_name))
sapply(tab14,class)

tab14$Var1 * tab14$Var3
require(tidyverse)
tab14 <- tab14 %>% 
  unite(Var1_Var3, c("Var1", "Var3"))


newggslopegraph(tab14,Var2,Freq, Var1_Var3,LineColor = c("black", "red"),
                DataLabelFillColor = "gray",
                DataLabelPadding = .2,
                DataLabelLineSize = .5)

ggsave('samp.png',dpi = 800)

#subject wise

table(exam_data$student_type,exam_data$subject_name)

#sem wise

table(exam_data$student_type,exam_data$subject_semester)

#yearsem wise

table(exam_data$student_type,exam_data$subject_year)

#subject wise degree distri

as.data.frame(table(exam_data$student_type,exam_data$degree_name,exam_data$subject_name))

#subject wise year-sem distribution

as.data.frame(table(exam_data$student_type,exam_data$subject_name,exam_data$subject_year))

#grade distribution

as.data.frame(table(exam_data$student_type,exam_data$points))

#subjectwise grade distribution

as.data.frame(table(exam_data$student_type,exam_data$subject_name, exam_data$points))



#Students data----------------------------------------------

length(unique(exam_data$matriculation_number))


#Out of 388 students, 326 were matched with exam records. Find which where not matched and HZB.----------------

exam_office_data <- import('./processed_data/german_international(pramtoffice)/cateogorised_data_touse2.xlsx')

exam_office_data$admission_sem <- gsub("(^.*_)","",exam_office_data$semester)


tmpdegree <- str_sub(exam_office_data$semester,5,-6)

for (i in 1:length(tmpdegree)){
  if (substring(tmpdegree[i], 1, 1) == "I"){
    tmpdegree[i] <- "IF"
    
  }
  else{
    tmpdegree[i] <- "WIF"
  }
}

exam_office_data$tmpdegree <- tmpdegree

exam_office_data$`final confirmation`[exam_office_data$`final confirmation` == -1] <- 1


unique_exam_data <-  exam_data[!rev(duplicated(rev(exam_data$matriculation_number))),]

non_matching_data <- subset(exam_office_data, !(`Matrikel-Nr.` %in% unique_exam_data$matriculation_number))

semester_order <- c("SS17","WS17-18","SS18","WS18-19","SS19","WS19-20")


#table(non_matching_data$admission_sem,non_matching_data$`final confirmation`)
#table(non_matching_data$admission_sem,non_matching_data$tmpdegree)

non_matched_table <- as.data.frame(table(non_matching_data$admission_sem,non_matching_data$tmpdegree,non_matching_data$`final confirmation`))


tempo <- as.data.frame(table(exam_office_data$admission_sem,exam_office_data$tmpdegree,exam_office_data$`final confirmation`))

non_matched_table$total_students <- tempo$Freq


#non_matched_table$Var1 <- stri_replace_all_regex(non_matched_table$Var1, c("WS17-18-18", "WS18-19-19", "WS19-20-20"), 
 #                                       c('WS17-18', 'WS18-19', 'WS19-20'), F, list(case_insensitive = TRUE))

non_matched_table$non_match_percent <- paste0(round((non_matched_table$Freq/non_matched_table$total_students)*100,2), "%")

levels(non_matched_table$Var1) <- semester_order

non_matched_table %>%
  slice(match(Var1, semester_order))

non_matched_table <- arrange(transform(non_matched_table,
                           Var1=factor(Var1,levels=semester_order)),Var1)

export(iris2,'non_matched_students.csv')


ggplot(data = iris2, aes(x = Var2, y = Freq, fill = Var3)) + 
  geom_bar(stat = "identity", width = 1, position = position_dodge()) +
  geom_label(aes(x = Var2, y = Freq, group = Var3,label = paste(Freq)),
             position = position_dodge(width = 1)) +
  geom_text(aes(x = Var2, y = Freq, group = Var3,label = paste(total_students)),
            position = position_dodge(width = 1),vjust = -1.5,size = 3)+
  facet_wrap(~Var1, strip.position = "bottom", scales = "free_x", ncol = 6 ) +
  xlab("Subjects")+scale_fill_manual(labels = c("Bildungsin.", "Bildungsaus.", 
                                                "schlueko (88)"), values=c("#EC7A78", "#55A5C4", "#C18ABC")) + labs(fill = "HZB.") + 
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(), strip.placement = "outside",panel.border = element_blank())

ggsave('samp1.png',dpi = 800, height = 6,width = 9)


#Unique and repeat in each exam
#HZB.
#Just given any one exam (remove duplicate)


#unique_exam_data  final_dataset
#' exam records, i took unique students unique matricu
#' then people with two subjects, separate each subject take unique
#' match 2-2 subjects, then take aud-schulko matriculation, match einfinf and keep that as 3  



# Get individual dataframe of subjects

data_aud <- exam_data[exam_data$subject_name == "aud",]
data_einfinf <- exam_data[exam_data$subject_name == "einfinf",]
data_schlueko <- exam_data[exam_data$subject_name == "schlueko",]

#Get unique students

unique_aud <- data_aud[!rev(duplicated(rev(data_aud$matriculation_number))),] #remove all duplicate keep last
unique_einfinf <- data_einfinf[!rev(duplicated(rev(data_einfinf$matriculation_number))),]
unique_schlueko <- data_schlueko[!rev(duplicated(rev(data_schlueko$matriculation_number))),]

# Find students who took pair of 2 subjects

matching_data_aud_schlu<- subset(unique_schlueko, (matriculation_number %in% unique_aud$matriculation_number))
matching_data_aud_einfinf <- subset(unique_einfinf, (matriculation_number %in% unique_aud$matriculation_number))
matching_data_einfinf_schlu <- subset(unique_einfinf, (matriculation_number %in% unique_schlueko$matriculation_number))


# Find students who took all 3 subjects

matching_data_3 <- subset(matching_data_einfinf_schlu, (matriculation_number %in% unique_aud$matriculation_number))


#Find people who took only subject's exam

unique_exam_data2 <- unique_exam_data

unique_exam_data2 <- unique_exam_data2[!(unique_exam_data2$matriculation_number %in% matching_data_3$matriculation_number),]

unique_exam_data2 <- unique_exam_data2[!(unique_exam_data2$matriculation_number %in% matching_data_aud_einfinf$matriculation_number),]

unique_exam_data2 <- unique_exam_data2[!(unique_exam_data2$matriculation_number %in% matching_data_aud_schlu$matriculation_number),]

unique_exam_data2 <- unique_exam_data2[!(unique_exam_data2$matriculation_number %in% matching_data_einfinf_schlu$matriculation_number),]


#2 subjects
#' Take original subject data
#' remove all duplicate entries except first 
#' then match imm. of two frames

#Which student takes which subject exam
#Any 2
#remove people who have given 3


matching_data_aud_schlu <- matching_data_aud_schlu[!(matching_data_aud_schlu$matriculation_number %in% matching_data_3$matriculation_number),]
matching_data_aud_einfinf <- matching_data_aud_einfinf[!(matching_data_aud_einfinf$matriculation_number %in% matching_data_3$matriculation_number),]
matching_data_einfinf_schlu <- matching_data_einfinf_schlu[!(matching_data_einfinf_schlu$matriculation_number %in% matching_data_3$matriculation_number),]


final_dataset <- NULL
final_dataset <- as.data.frame(final_dataset)

# combine any2 of all subjects

combinestudentsexamdata <- function(frame, subjectnamestr){
  
  match_data_eda <- as.data.frame(table(frame$student_type,
                                        frame$degree_name))
  
  match_data_eda$subjects = subjectnamestr
  
  return(match_data_eda)
  
}

final_dataset <- dplyr::bind_rows(final_dataset,combinestudentsexamdata(matching_data_aud_schlu,'aud_schlu'))

final_dataset <- dplyr::bind_rows(final_dataset,combinestudentsexamdata(matching_data_aud_einfinf,'aud_einfinf')
)

final_dataset <- dplyr::bind_rows(final_dataset,combinestudentsexamdata(matching_data_einfinf_schlu,'einfinf_schlu')
)

final_dataset <- dplyr::bind_rows(final_dataset,combinestudentsexamdata(matching_data_3,'all 3')
)

final_dataset <- dplyr::bind_rows(final_dataset,combinestudentsexamdata(unique_exam_data2,'only 1')
)


# Timeline check for student enrolment exam semester

#aud

unique_exam_data2 %>% filter(subject_name == 'aud') %>% 
  mutate(start_y = 1:n(),end_y = 1:n()) %>%
  mutate_each(funs(as.character), student_type) %>% 
  ggplot(., aes(x = paste(degree_semester,degree_year,sep=""), y = start_y,color = student_type)) + 
  geom_segment(aes(xend = paste(subject_semester,subject_year,sep=""), yend = end_y),arrow = arrow()) + 
  scale_x_discrete(name ="Time",limits=c("WS16-17","SS17","WS17-18","SS18","WS18-19","SS19","WS19-20")) + 
  ggtitle("aud")+
  theme(axis.text.x = element_text(angle = 90), axis.title.y = element_blank(),axis.text.y = element_blank(), plot.title = element_text(hjust = 0.5))

#einfinf

grouper <- unique_exam_data2 %>% filter(subject_name == 'einfinf' & degree_year == subject_year) %>%
  group_by(student_type,degree_year,subject_year,subject_semester,degree_semester) %>% 
  summarise(counts = n()) 

outliers <- unique_exam_data2 %>% filter(subject_name == 'einfinf' & degree_year != subject_year) %>%
  group_by(student_type,degree_year,subject_year,subject_semester,degree_semester) %>% 
  summarise(counts = n()) 


conditdf <- data.frame(
  degree_year = c('17', '17-18', '18', '18-19', '19'),
  subject_year = c('17-18', '18', '18-19', '19', '19-20')
)

grouper <- unique_exam_data2 %>% filter(subject_name == 'schlueko') 

grouper <- conditdf %>%
  rowwise() %>%
  do(
    dfs = inner_join(as.data.frame(.), grouper)
  )

grouper <- do.call(rbind.data.frame, grouper$dfs)

outliers <- unique_exam_data2 %>% filter(subject_name == 'schlueko') 

outliers <- outliers[!(outliers$matriculation_number %in% grouper$matriculation_number),]

outliers <- outliers%>%
  group_by(student_type,degree_year,subject_year,subject_semester,degree_semester) %>% 
  summarise(counts = n()) 

grouper <- grouper %>%
  group_by(student_type,degree_year,subject_year,subject_semester,degree_semester) %>% 
  summarise(counts = n()) 


grouper <-bind_rows(grouper, outliers)

grouper$start_y <- 1:nrow(grouper) 
grouper$end_y <- 1:nrow(grouper)  
grouper <- as.data.frame(grouper)


outliers$start_y <- 1:nrow(outliers) 
outliers$end_y <- 1:nrow(outliers)  
outliers <- as.data.frame(outliers)


outliers %>% mutate_each(funs(as.character), student_type) %>%
  ggplot(., aes(x = paste(degree_semester,degree_year,sep=""), y = start_y,color = student_type)) + 
  geom_segment(aes(xend = paste(subject_semester,subject_year,sep=""), yend = end_y),arrow = arrow()) + 
  scale_x_discrete(name ="Time",limits=c("WS16-17","SS17","WS17-18","SS18","WS18-19","SS19","WS19-20")) +
  ggtitle("schlueko")+
  geom_text(data = outliers,aes( y=start_y, label=counts), size=3.8, hjust=1.5, colour="black") +
  theme(axis.text.x = element_text(angle = 90), 
        axis.title.y = element_blank(),axis.text.y = element_blank(), plot.title = element_text(hjust = 0.5))


grouper %>% mutate_each(funs(as.character), student_type) %>%
  ggplot(., aes(x = paste(degree_semester,degree_year,sep=""), y = start_y,color = student_type)) + 
  geom_segment(aes(xend = paste(subject_semester,subject_year,sep=""), yend = end_y),arrow = arrow()) + 
  scale_x_discrete(name ="Time",limits=c("WS16-17","SS17","WS17-18","SS18","WS18-19","SS19","WS19-20")) +
  ggtitle("schlueko")+
  geom_text(data = grouper,aes( y=start_y, label=counts), size=3.8, hjust=1.5, colour="black") +
  theme(axis.text.x = element_text(angle = 90), 
  axis.title.y = element_blank(),axis.text.y = element_blank(), plot.title = element_text(hjust = 0.5))



unique_exam_data2 %>% filter(subject_name == 'einfinf') %>% sample_n(10) %>% 
  mutate(start_y = 1:n(),end_y = 1:n()) %>%
  mutate_each(funs(as.character), student_type) %>% 
  ggplot(., aes(x = paste(degree_semester,degree_year,sep=""), y = start_y,color = student_type)) + 
  geom_segment(aes(xend = paste(subject_semester,subject_year,sep=""), yend = end_y),arrow = arrow()) + 
  scale_x_discrete(name ="Time",limits=c("WS16-17","SS17","WS17-18","SS18","WS18-19","SS19","WS19-20")) + 
  ggtitle("einfinf")+
  theme(axis.text.x = element_text(angle = 90), axis.title.y = element_blank(),axis.text.y = element_blank(), plot.title = element_text(hjust = 0.5))

#' Alot of students are from the WS19-20, for einfinf means we check their grade and keep them aside
#' we check other other years

unique_exam_data2 %>% filter(subject_name == 'einfinf', degree_year != '19-20') %>% 
  mutate(start_y = 1:n(),end_y = 1:n()) %>%
  mutate_each(funs(as.character), student_type) %>% 
  ggplot(., aes(x = paste(degree_semester,degree_year,sep=""), y = start_y,color = student_type)) + 
  geom_segment(aes(xend = paste(subject_semester,subject_year,sep=""), yend = end_y),arrow = arrow()) + 
  scale_x_discrete(name ="Time",limits=c("WS16-17","SS17","WS17-18","SS18","WS18-19","SS19","WS19-20")) + 
  ggtitle("einfinf")+
  theme(axis.text.x = element_text(angle = 90), axis.title.y = element_blank(),axis.text.y = element_blank(), plot.title = element_text(hjust = 0.5))


#schleuko

#' 1st offered in WS, 2nd part in SS so you can take exam then
#' Create a different plot for WS18-19 & SS19 and after, and then before

unique_exam_data2 %>% filter(subject_name == 'schlueko', !degree_year %in% c('19', '19-20')) %>% 
  mutate(start_y = 1:n(),end_y = 1:n()) %>%
  mutate_each(funs(as.character), student_type) %>% 
  ggplot(., aes(x = paste(degree_semester,degree_year,sep=""), y = start_y,color = student_type)) + 
  geom_segment(aes(xend = paste(subject_semester,subject_year,sep=""), yend = end_y),arrow = arrow()) + 
  scale_x_discrete(name ="Time",limits=c("WS16-17","SS17","WS17-18","SS18","WS18-19","SS19","WS19-20")) + 
  ggtitle("schlueko")+
  theme(axis.text.x = element_text(angle = 90), axis.title.y = element_blank(),axis.text.y = element_blank(), plot.title = element_text(hjust = 0.5))


 unique_exam_data2 %>% filter(subject_name == 'schlueko', !degree_year %in% c('19', '19-20')) %>% 
    mutate(start_y = 1:n(),end_y = 1:n(), examresult = case_when(points == 5.0 ~ "fail", TRUE ~"pass")) %>%
    mutate_each(funs(as.character), student_type) %>% 
    ggplot(., aes(x = paste(degree_semester,degree_year,sep=""), y = start_y,color = examresult)) + 
    geom_segment(aes(xend = paste(subject_semester,subject_year,sep=""), yend = end_y),arrow = arrow()) + 
    scale_x_discrete(name ="Time",limits=c("WS16-17","SS17","WS17-18","SS18","WS18-19","SS19","WS19-20")) + 
    ggtitle("schlueko")+
    theme(axis.text.x = element_text(angle = 90), axis.title.y = element_blank(),axis.text.y = element_blank(), plot.title = element_text(hjust = 0.5))
  
  
require(tidyr)
tess <- unique_exam_data2 %>% unite(degree_semester,degree_year, sep = '_')

within(unique_exam_data2, combinedegree<- paste(degree_semester,degree_year,sep=""))
remove(tess)


tab15 <- as.data.frame(table(unique_exam_data2$subject_name,unique_exam_data2$degree_name,unique_exam_data2$student_type))

#export(tab15,'onlyoneexam_eda.csv')

sum(tab15$Freq)
sum(final_dataset$Freq)
pie_labels <- (tab15 %>% group_by(Var1) %>% 
                 mutate(per=paste0(round(Freq/sum(Freq)*100, 2), "%")) %>% 
                 ungroup %>% select(per))$per

ggplot(data = tab15, aes(x = Var2, y = Freq, fill = Var3)) + 
  geom_bar(stat = "identity", width = 1, position = position_dodge()) +
  geom_label(aes(x = Var2, y = Freq, group = Var3,label = paste(Freq)),
             position = position_dodge(width = 1)) +
  # geom_text(aes(x = Var2, y = Freq, group = Var3,label = paste(total_students)),
  #      position = position_dodge(width = 1),vjust = -1.5,size = 3)+
  facet_wrap(~Var1, strip.position = "bottom", scales = "free_x", ncol = 6 ) +
  xlab("Subjects")+scale_fill_manual(labels = c("Bildungsin.", "Bildungsaus.", 
                                                "schlueko (88)"), values=c("#EC7A78", "#55A5C4", "#C18ABC")) + labs(fill = "HZB.") + 
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(), strip.placement = "outside",panel.border = element_blank())







export(final_dataset,'overviewexam_exam_students_number_EDA.csv')



#any3
#divided by HZB.

remove(matchi)


ggplot(data = final_dataset, aes(x = Var2, y = Freq, fill = Var1)) + 
  geom_bar(stat = "identity", width = 1, position = position_dodge()) +
  geom_label(aes(x = Var2, y = Freq, group = Var1,label = paste(Freq)),
             position = position_dodge(width = 1)) +
 # geom_text(aes(x = Var2, y = Freq, group = Var3,label = paste(total_students)),
      #      position = position_dodge(width = 1),vjust = -1.5,size = 3)+
  facet_wrap(~subjects, strip.position = "bottom", scales = "free_x", ncol = 6 ) +
  xlab("Subjects")+scale_fill_manual(labels = c("Bildungsin.", "Bildungsaus.", 
                                                "schlueko (88)"), values=c("#EC7A78", "#55A5C4", "#C18ABC")) + labs(fill = "HZB.") + 
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(), strip.placement = "outside",panel.border = element_blank())



#Tries by student per subject

#' Cases
#' 1. students pass on first attempt (All till 4.0)
#' 2. students fail on first attempt and repeat (All 5 and duplicated values from then)
#'      - Ultimately pass
#'      - Never pass
#' 3. students fail on first attempt and never repeat (All 5 and not duplicated)

#' Common stats
#' Student type
#' Degree
#' Subject

#1-----------------------------------------------------------------
final_dataset <- NULL
final_dataset <- as.data.frame(final_dataset)

passedstudentsdf <- function(tempframe){
  
  tempo <- tempframe[tempframe$points <= 4,]
  
  tempo <- as.data.frame(table(tempo$subject_name,tempo$degree_name, tempo$student_type))
  
  temp_cl <- as.data.frame(table(tempframe$subject_name,
                                 tempframe$degree_name,tempframe$student_type))[4]
  
  tempo <- cbind(tempo, total = temp_cl$Freq)
  
  
  
  return(tempo)
  
  
}

subjectdf <- NULL
subjectdf <- as.data.frame(subjectdf,stringval)

individualsubjectdf <- function(tempframe,stringval){
  
  tempo <- final_dataset[final_dataset$Var1 == sprintf("%s",stringval),]
  tempo <- subset(tempo, select = -c(Var2))
  tempo <- tempo %>% group_by(Var3) %>% summarize_if(is.numeric,sum,na.rm = TRUE)
  tempo$subject <- sprintf("%s",stringval)
  
  ggplot(tempo, aes(x = "", y = Freq, fill = Var3)) +
    geom_bar(stat = "identity", position = position_fill()) +
    geom_text(aes(label = paste(Freq)), position = position_fill(vjust = 0.5)) +
    coord_polar(theta = "y") +
    facet_wrap(~ Var3) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) + 
    theme(legend.position='bottom') + 
    guides(fill=guide_legend(nrow=2, byrow=TRUE))
  
  
  return(tempo)
  
  
  

}


tab6

exam_office_data

unique_exam_data2 %>% filter(subject_name == 'aud') %>% mutate(start_y = 1:nrow(.), end_y = 1:nrow(.)) %>%
  mutate_each(funs(as.character), student_type) 

unique_exam_data2 %>% filter(subject_name == 'einfinf')

unique_exam_data2 %>% filter(subject_name == 'schlueko')

remove(tempo)

conditdf <- data.frame(
  degree_year = c('17', '17-18', '18', '18-19', '19', '19-20'),
  subject_year = c('17-18', '18', '18-19', '19', '19-20', '20')
)

grouper <- unique_exam_data2 %>% filter(subject_name == 'schlueko') 

grouper <- conditdf %>%
  rowwise() %>%
  do(
    dfs = inner_join(as.data.frame(.), grouper)
  )

grouper <- do.call(rbind.data.frame, grouper$dfs)

grouper <- grouper %>%
  group_by(student_type,degree_year,subject_year,subject_semester,degree_semester) %>% 
  summarise(counts = n()) 




ggplot(subjectdf, aes(x = "", y = Freq, fill = Var3)) +
  geom_bar(stat = "identity", position = position_fill()) +
  geom_text(aes(label = paste(Freq)), position = position_fill(vjust = 0.5)) +
  coord_polar(theta = "y") +
  facet_wrap(~ subject) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
  theme(legend.position='bottom') + 
  guides(fill=guide_legend(nrow=2, byrow=TRUE))




final_dataset <- dplyr::bind_rows(final_dataset,passedstudentsdf(data_schlueko))

subjectdf <- dplyr::bind_rows(subjectdf,individualsubjectdf(final_dataset,'schlueko'))


ggplot(data = final_dataset, aes(x = Var2, y = Freq, fill = Var3)) + 
  geom_bar(stat = "identity", width = 1, position = position_dodge()) +
  geom_label(aes(x = Var2, y = Freq, group = Var3,label = paste(Freq)),
             position = position_dodge(width = 1)) +
  geom_text(aes(x = Var2, y = Freq, group = Var3,label = paste(total)),
        position = position_dodge(width = 1),vjust = -1.5,size = 3)+
  facet_wrap(~Var1, strip.position = "bottom", scales = "free_x", ncol = 6 ) +
  xlab("Subjects")+scale_fill_manual(labels = c("Bildungsin.", "Bildungsaus.", 
                                                "schlueko (88)"), values=c("#EC7A78", "#55A5C4", "#C18ABC")) + labs(fill = "HZB.") + 
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(), strip.placement = "outside",panel.border = element_blank())

#2------------------------------------------------------


tempo <- data_aud[duplicated(data_aud$matriculation_number)|duplicated(data_aud$matriculation_number, fromLast=TRUE),]

tempo <- data_einfinf[duplicated(data_einfinf$matriculation_number)|duplicated(data_einfinf$matriculation_number, fromLast=TRUE),]

tempo <- data_schlueko[duplicated(data_schlueko$matriculation_number)|duplicated(data_schlueko$matriculation_number, fromLast=TRUE),]

tempo[tempo$matriculation_number == 204859,]

#Comparison (Try to figure out how to normalize this data)

# Complex multitable plots EDA

# exams taken by students vs. admissions

subject_data <- exam_data[exam_data$subject_name == "aud",]


tempo <- subject_data[duplicated(subject_data$matriculation_number)|duplicated(subject_data$matriculation_number, fromLast=TRUE),]

length(unique(subject_data$matriculation_number))


# Things to send to Rafi

tempraf <- exam_data[exam_data$subject_name == "schlueko" & exam_data$student_type == 0,]

summary(tempraf$points)

exam_data

as.data.frame(table(exam_data$student_type, exam_data$points))

# Find confidence interval of whole and each subject.
# Plot the data for only passing distribution and a seaparte confidence intervals for it

local <- exam_data %>% filter(student_type == 0) %>% select(points)

intn <- exam_data %>% filter(student_type == 1) %>% select(points)

temp_frame <- exam_data %>% 
  mutate(student_type_n = ifelse(as.character(student_type) == 0, "Bildungsinländer(innen)", "Bildungsausländer(innen)"))

tapply(temp_frame$points, temp_frame$student_type_n, summary)

print(ddply(temp_frame, .(student_type_n), summarise, mean=mean(points), standard_dev = sd(points),
            lower_CI=CI(points, ci = 0.95)[3],upper_CI=CI(points, ci = 0.95)[1],
            median = median(points), min = min(points), max = max(points), 
            q1_quantile = quantile(points, 0.25), q3_quantile = quantile(points, 0.75))) 

samp %>% 
  group_by(subject_name,SoSe20) %>% 
  summarize(mean = mean(points),
            count = n())

library(psych)

describeBy(exam_data$points, exam_data$student_type, mat = TRUE) 

#Confidence interval 

function(confdata){
  
  error <- qnorm(0.975) * sd(confdata$points) / sqrt(nrow(confdata))
  
  lower_bound <- mean(confdata$points) - error
  
  upper_bound <- mean(confdata$points) + error
  
  return lower_bound , upper_bound
  
}

library(Rmisc)

CI(local$points, ci =0.95)




error <- qnorm(0.975) * sd(intn$points) / sqrt(nrow(intn))

mean(intn$points) - error

mean(intn$points) + error







n_fun <- function(x){
  return(data.frame(y = 0.95*70,
                    label = length(x)))
}

ggplot(data = local, aes(y = points)) + stat_boxplot(geom ='errorbar' , width = 0.6) + 
  geom_boxplot(width = 0.6, fill = "lightgrey") + stat_summary(fun="mean", geom="point", shape=23, size=4, fill="white")
 # stat_summary(aes(x = 0, y = 0.95*70 ,label = NROW(local)), geom = "text", hjust = 0.5)


tab10 <- as.data.frame(table(exam_data$degree_name,exam_data$points))

from <- list(c("1","1.3","1.7"), c("2","2.3", "2.7"), c("3","3.3","3.7"))
to   <- c("1-1.7", "2-2.7","3-3.7")

find.in.list <- function(x, y) match(TRUE, sapply(y, `%in%`, x = x))
idx.in.list  <- sapply(levels(tab10$Var2), find.in.list, from)
levels(tab10$Var2)  <- ifelse(is.na(idx.in.list), levels(tab10$Var2), to[idx.in.list])

library(plyr);
library(dplyr)

temptab <- tab10 %>% group_by(Var1,Var2) %>% summarise(Freq = sum(Freq))

gradesumcounter <- (tab10 %>% group_by(Var2) %>% summarise(Freq = sum(Freq)))$Freq


PlotXTabs2(temptab, y =Var2 , x = Var1, counts = Freq, perc.k = 2,data.label = "both",
           results.subtitle = F,label.text.size = 4, label.fill.alpha = 0.3, sample.size.label = F,
           legend.title = "grade") +
  coord_flip()+ scale_fill_manual(labels = c("1-1.7 (92)" , "2-2.7 (212)", "3-3.7 (162)", "4 (63)", "5 (153)"),values=c("#C58C2F", "#384BB6", "#D13E3E","#7a55c4","#b6e700")) + 
  scale_x_discrete(name ="course", labels=c("IF (436)", "WIF (246)"))


temp_frame <- exam_data %>% 
  mutate(student_type_n = ifelse(as.character(student_type) == 0, "Bildungsinländer(innen)", "Bildungsausländer(innen)"))


ggplot(temp_frame, aes(student_type_n, points))+geom_point()+geom_smooth()

foo = function(x){sd(x)/sqrt(length(x))}

my.aggs = cbind(aggregate(points ~ student_type_n, data = temp_frame, FUN = foo),
                aggregate(points ~ student_type_n, data = temp_frame, FUN = mean))
names(my.aggs) = c("student_type","se","","means")
ggplot()+
  geom_point(data = temp_frame, aes(as.factor(student_type_n), points)) +
  geom_errorbar(data = my.aggs, aes(student_type, ymin= means - 1.96*se, 
                                    ymax=means+1.96*se), width=.1) +
  geom_line(data = my.aggs, aes(x = student_type, y = means, group = 1)) + 
  xlab("HZB.") + ylab("grade") 
  
#geom_point(position=pd, size=4)

# Alternative method that doesn't include points
library(gplots)
plotmeans(points ~ student_type_n, data = temp_frame)


setwd("C:/Users/ysa pc/Desktop/internship/HiWi/qualiman/")

exam_office_data <-
  import('./processed_data/german_international(pramtoffice)/cateogorised_data_touse2.xlsx')

as.data.frame(table(exam_data$subject_year, exam_data$student_type))



p1 <- exam_data %>% group_by(subject_year,student_type) %>% summarise(freq = n()) %>%
  mutate_each(funs(as.character), student_type) %>%
  ggplot(.,aes(x = subject_year, y = freq, group = student_type, color = student_type))+ geom_line() + geom_point() + geom_text(aes(label = freq),hjust=-0.5, vjust=0.5) + 
  scale_color_manual(values=c("#EC7A78", "#55A5C4")) + 
  scale_x_discrete(name ="year - semester", labels=c("16-17 WS","17 SS","17-18 WS","18 SS","18-19 WS","19 SS","19-20 WS", "20 SS")) + 
scale_y_continuous(name = "records", limits = c(0,135)) +
theme(axis.text.x = element_text(angle = 90)) + ggtitle("all subjects")

p2 <- exam_data %>% filter(subject_name == 'aud') %>%group_by(subject_year,student_type) %>% summarise(freq = n()) %>%
mutate_each(funs(as.character), student_type) %>%
ggplot(.,aes(x = subject_year, y = freq, group = student_type, color = student_type))+ geom_line() + geom_point() + geom_text(aes(label = freq),hjust=-0.5, vjust=0.5) +
  scale_y_continuous(name = "records", limits = c(0,135)) +
  scale_x_discrete(name = "semester-year", labels=c("17 SS","17-18 WS","18 SS","19 SS","20 SS"))+
  theme(axis.text.x = element_text(angle = 90)) + ggtitle("Aud")

p3 <- exam_data %>% filter(subject_name == 'einfinf') %>%group_by(subject_year,student_type) %>% summarise(freq = n()) %>%
  mutate_each(funs(as.character), student_type) %>%
  ggplot(.,aes(x = subject_year, y = freq, group = student_type, color = student_type))+ geom_line() + geom_point() + geom_text(aes(label = freq),hjust=-0.5, vjust=0.5) +
  scale_y_continuous(name = "records", limits = c(0,135)) +
  scale_x_discrete(name = "semester-year", labels=c("16-17 WS","17 SS","17-18 WS","18 SS","18-19 WS","19 SS","19-20 WS", "20 SS"))+
  theme(axis.text.x = element_text(angle = 90)) + ggtitle("EinfInf")

p5 <- exam_data %>% filter(subject_name == 'schlueko') %>%group_by(subject_year,student_type) %>% summarise(freq = n()) %>%
  mutate_each(funs(as.character), student_type) %>%
  ggplot(.,aes(x = subject_year, y = freq, group = student_type, color = student_type))+ geom_line() + geom_point() + geom_text(aes(label = freq),hjust=-0.5, vjust=0.5) +
  scale_y_continuous(name = "records", limits = c(0,135)) +
  scale_x_discrete(name = "semester-year", labels=c("17 SS","17-18 WS","18 SS","18-19 WS","19 SS","19-20 WS", "20 SS")) +
  scale_color_manual(labels = c("bildungsin.", "bildungsaus."),values=c("#EC7A78", "#55A5C4")) + 
  theme(axis.text.x = element_text(angle = 90), legend.title = element_text('studen HZB.'),legend.position="bottom") + ggtitle("Schleuko")

p4 <- exam_data %>% filter(subject_name == 'tewif') %>%group_by(subject_year,student_type) %>% summarise(freq = n()) %>%
  mutate_each(funs(as.character), student_type) %>%
  ggplot(.,aes(x = subject_year, y = freq, group = student_type, color = student_type))+ geom_line() + geom_point() + geom_text(aes(label = freq),hjust=-0.5, vjust=0.5) +
  scale_y_continuous(name = "records", limits = c(0,135)) +
  scale_x_discrete(name = "semester-year", labels=c("17 SS","17-18 WS","18-19 WS","19-20 WS", "20 SS")) +
  scale_color_manual(labels = c("bildungsin.", "bildungsaus."),values=c("#EC7A78", "#55A5C4")) + 
  theme(axis.text.x = element_text(angle = 90), legend.title = element_text('studen HZB.'),legend.position="bottom") + ggtitle("EWIF")



#library(gridExtra)
#library(ggpubr)


get_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

p5_legend <- get_legend(p5)

grid.arrange(arrangeGrob(p1 + theme(legend.position="none"), 
                         p2 + theme(legend.position="none"), 
                         p3 + theme(legend.position="none"),
                         p4 + theme(legend.position="none"),
                         p5 + theme(legend.position="none"),nrow=2,ncol = 3),
             p5_legend, 
             nrow=2,heights=c(10, 1))

ggarrange(p1, p2, p3, p4,p5, ncol=2, nrow=3, common.legend = TRUE, legend="bottom")


#Admission and subject the same semester 

exam_office_data$admission_sem <- gsub("(^.*_)","",exam_office_data$semester)
exam_office_data$`final confirmation`[exam_office_data$`final confirmation` == -1] <- 1


student_admission_info <- as.data.frame(table(exam_office_data$admission_sem, exam_office_data$`final confirmation`))

semester_order <- c("SS17","WS17-18","SS18","WS18-19","SS19","WS19-20")

student_admission_info <- arrange(transform(student_admission_info,
                  Var1=factor(Var1,levels=semester_order)),Var1)


exam_taken_info <- exam_data %>% filter(subject_name != 'schlueko' , degree_year == subject_year) %>%
  group_by(degree_year,student_type) %>% 
  summarise(counts = n()) 

de<-data.frame("18",1,0)
names(de)<-c("degree_year","student_type","counts")
exam_taken_info <- rbind(exam_taken_info, de)

de<-data.frame("19",1,0)
names(de)<-c("degree_year","student_type","counts")
exam_taken_info <- rbind(exam_taken_info, de)

semester_order <- c("17","17-18","18","18-19","19","19-20")

exam_taken_info <- arrange(transform(exam_taken_info,
                  degree_year=factor(degree_year,levels=semester_order)),degree_year)

student_admission_info$exam_taken <- exam_taken_info$counts


student_admission_info <- tidyr::pivot_longer(student_admission_info, cols=c('Freq', 'exam_taken'), names_to='variable', values_to="value")


ggplot(data = student_admission_info, aes(x = Var2, y = value, fill = variable)) + 
  geom_bar(stat = "identity", width = 1, position = position_dodge()) +
  geom_label(aes(x = Var2, y = value, group = variable,label = paste(value)),
             position = position_dodge(width = 1)) +
  # geom_text(aes(x = Var2, y = Freq, group = Var3,label = paste(total_students)),
  #      position = position_dodge(width = 1),vjust = -1.5,size = 3)+
  facet_wrap(~Var1, strip.position = "bottom", scales = "free_x", ncol = 3 ) +
  xlab("year-semester")+scale_fill_manual(labels = c("exam_counts", "admission_counts"), values=c("#FFFF00", "#FF00FF")) + labs(fill = "HZB.") + 
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(), strip.placement = "outside",panel.border = element_blank())



#-------------------------------------------------


setwd("C:/Users/ysa pc/Desktop/internship/HiWi/qualiman/")

temp_sub_frame <- NULL
temp_sub_frame <- as.data.frame(temp_sub_frame)

#Get the first occurence
for (subs in c('aud','einfinf','schlueko','tewif')){
  
  exam_data <- import('./processed_data/exams/allexams.xlsx')
  exam_data <- exam_data %>% filter(subject_name == subs)
  exam_data <- exam_data[!duplicated(exam_data$matriculation_number), ] 
  
  #removed all occurences of duplicate except first (so first try results only)
  
  #exam_data_dup <- exam_data[duplicated(exam_data$matriculation_number),]
  #This is for duplicated 
  
  temp_sub_frame  <- dplyr::bind_rows(temp_sub_frame,exam_data)
}


# total 896 observations counted as first tries, we have 956 observations in total
# so 60 observations are of 2nd or more tries

# Significance test 




#dupli_exam_data <- exam_data[(duplicated(exam_data$matriculation_number)|duplicated(exam_data$matriculation_number, fromLast=TRUE)),, drop=FALSE]

#nondupli_exam_data <- exam_data[!(duplicated(exam_data$matriculation_number)|duplicated(exam_data$matriculation_number, fromLast=TRUE)),, drop=FALSE]


#find the students info who passed in their first attempt 
#subject, degree, hzb.

#year,semesterwise for each subject

exam_data <- import('./processed_data/exams/allexams.xlsx')
exam_data <- exam_data %>% filter(subject_name == 'schlueko')

exam_data_dup <- exam_data[duplicated(exam_data$matriculation_number),]


#' Get individuals admited in that year-semester, get the subject information
#' gap for when they appear in a exam, result in first attempt. average it for every semester and compare difference.


aud <- exam_data %>% filter(student_type == 1,subject_name == 'einfinf')

#removed all occurences of duplicate except first (so first try results only)

aud <- aud[!duplicated(aud$matriculation_number), ] 

aud %>% group_by(degree_year) %>% summarize(count = n(), mean = mean(points), std = sd(points), med = median(points))

bartlett.test(aud[!(aud$degree_year %in% c(18)),"points"]~aud[!(aud$degree_year %in% c(18)),"degree_year"])

aud <- aud[!(aud$degree_year %in% c(18)),]

kruskal.test(aud[!(aud$degree_year %in% c(18)),"points"]~aud[!(aud$degree_year %in% c(18)),"degree_year"])





bartlett.test(aud[!(aud$degree_year %in% c(17,18,19)),"points"]~aud[!(aud$degree_year %in% c(17,18,19)),"degree_year"])

kruskal.test(aud[!(aud$degree_year %in% c(17,18,19)),"points"]~aud[!(aud$degree_year %in% c(17,18,19)),"degree_year"])


aud <- exam_data %>% filter(student_type == 1,subject_name == 'schlueko')
aud <- aud[!duplicated(aud$matriculation_number), ] 
aud <- aud[!(aud$degree_year %in% c(18)),]

# Edit from here
x <- which(names(aud) == "degree_year") # name of grouping variable
y <- which(names(aud) == "points") # names of variables to test

method1 <- "anova" # one of "anova" or "kruskal.test"
method2 <- "t.test" # one of "wilcox.test" or "t.test"
#my_comparisons <- list(c("17", "18"), c("setosa", "virginica"), c("versicolor", "virginica")) # comparisons for post-hoc tests
# Edit until here
# Edit at your own risk
for (i in y) {
  for (j in x) {
    p <- ggboxplot(aud,
                   x = colnames(aud[j]), y = colnames(aud[i]),
                   color = colnames(aud[j]),
                   legend = "none",
                   palette = "npg",
                   add = "jitter"
    )
    print(
      p +  stat_compare_means(aes(label = paste0(..method.., ", p-value = ", ..p.format..)),
                             method = method1, label.y = max(aud[, i], na.rm = TRUE)
      )
      
    )
  }
}


setwd("C:/Users/ysa pc/Desktop/internship/HiWi/qualiman/")

exam_office_data <-
  import('./processed_data/german_international(pramtoffice)/cateogorised_data_touse2.xlsx')

exam_office_data$admission_sem <- gsub("(^.*_)","",exam_office_data$semester)


tmpdegree <- str_sub(exam_office_data$semester,5,-6)

for (i in 1:length(tmpdegree)){
  if (substring(tmpdegree[i], 1, 1) == "I"){
    tmpdegree[i] <- "IF"
    
  }
  else{
    tmpdegree[i] <- "WIF"
  }
}

exam_office_data$tmpdegree <- tmpdegree

exam_office_data$`final confirmation`[exam_office_data$`final confirmation` == -1] <- 1


unique_exam_data <-  exam_data[!rev(duplicated(rev(exam_data$matriculation_number))),]

non_matching_data <- subset(exam_office_data, !(`Matrikel-Nr.` %in% unique_exam_data$matriculation_number))


aud_sub_table <- as.data.frame(table(unique_exam_data$degree_year, unique_exam_data$degree_name, unique_exam_data$student_type))

levels(aud_sub_table$Var1) <- c("Coh-17","Coh-17-18", "Coh-18","Coh-18-19","Coh-19", "Coh19-20")

ggplot(data = aud_sub_table, aes(x = Var2, y = Freq, fill = Var3)) + 
  geom_bar(stat = "identity", width = 1, position = position_dodge()) +
  geom_label(aes(x = Var2, y = Freq, group = Var3,label = paste(Freq)),
             position = position_dodge(width = 1), size = 3) +
  facet_wrap(~Var1, strip.position = "bottom", scales = "free_x", ncol = 6 ) +
  scale_fill_manual(labels = c("Bildungsin.", "Bildungsaus."), values=c("#EC7A78", "#55A5C4")) + labs(fill = "HZB-Art") + xlab("Kohorte") + ylab("Studierende") + 
theme(panel.spacing = unit(0.1, "lines"), 
      strip.background = element_blank(), strip.placement = "outside",panel.border = element_blank(), strip.text.x = element_text(angle = 90))

setwd("C:/Users/ysa pc/Desktop/internship/HiWi/qualiman/")

tab2 <- as.data.frame(table(unique_exam_data$student_type))

ggplot(tab2,aes(x = "", y =  Freq, fill = Var1)) + 
  geom_col(position = 'stack', width = 1) +
  geom_text(aes(label = paste(round(Freq / sum(Freq) * 100, 1), "%")),
            position = position_stack(vjust = 0.5)) + 
  coord_polar("y") +
  theme(plot.title = element_text(hjust=0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), legend.position = 'top') +
  labs(fill = "HZB-Art",
       x = NULL,
       y = NULL) +
  scale_fill_manual(labels = c('Bildungsinländer(innen) (255)','Bildungsausländer(innen) (87)'),values=c("#EC7A78", "#55A5C4")) +
  theme_void()

ggsave(file="sec42.svg", width=6, height=6)


setwd("C:/Users/ysa pc/Desktop/internship/HiWi/qualiman/")

temp_sub_frame <- NULL
temp_sub_frame <- as.data.frame(temp_sub_frame)

#Get the first occurence
for (subs in c('aud','einfinf','schlueko','tewif')){
  
  exam_data <- import('./processed_data/exams/allexams.xlsx')
  exam_data <- exam_data %>% filter(subject_name == subs)
  exam_data1 <- exam_data[!duplicated(exam_data$matriculation_number), ] 
  
  #removed all occurences of duplicate except first (so first try results only)
  
  #exam_data_dup <- exam_data[duplicated(exam_data$matriculation_number),]
  #This is for duplicated 
  
  temp_sub_frame  <- dplyr::bind_rows(temp_sub_frame,exam_data1)
}

temp_frame_conf <- temp_sub_frame %>% 
  mutate(student_type_n = ifelse(as.character(student_type) == 0, "Bildungsinländer(innen)", "Bildungsausländer(innen)"))


temp_frame_conf_2 <- ddply(temp_frame_conf, .(student_type_n), summarise, bootstrapped_mean=DescTools::MeanCI(x = points, method = "boot",
                                                                                                              type = "bca")[1],mean=mean(points), standard_dev = sd(points), lower_CI=DescTools::MeanCI(x = points, method = "boot", type = "bca")[2],
                           upper_CI=DescTools::MeanCI(x = points, method = "boot", type = "bca")[3], median = median(points))

ggplot()+
  geom_point(data = temp_frame_conf, aes(as.factor(student_type_n), points)) +
  geom_point(data = temp_frame_conf_2, aes(as.factor(student_type_n), bootstrapped_mean), size = 4, color = c("#55A5C4", "#EC7A78")) +
  geom_errorbar(data = temp_frame_conf_2, aes(student_type_n, ymin= lower_CI, 
                                              ymax=upper_CI), width=.2) +
  geom_line(data = temp_frame_conf_2, aes(x = student_type_n, y = bootstrapped_mean, group = 1)) + 
  ylab("grade") + scale_x_discrete(name = 'HZB.', labels = c('Bildungsausländer(innen) (698)','Bildungsinländer(innen) (198)'))

table(temp_frame_conf$student_type)

tempo = temp_frame_conf %>% filter(subject_name == "tewif")

print(ddply(tempo, .(student_type_n), summarise, bootstrapped_mean=DescTools::MeanCI(x = points, method = "boot", type = "bca")[1],mean=mean(points), standard_dev = sd(points), lower_CI=DescTools::MeanCI(x = points, method = "boot", type = "bca")[2],
      upper_CI=DescTools::MeanCI(x = points, method = "boot", type = "bca")[3], median = median(points)))

tab1 <- as.data.frame(table(exam_data$subject_name))
ggplot(tab1,aes(x = "", y =  Freq, fill = Var1)) + 
  geom_col(position = 'stack', width = 1) +
  geom_text(aes(label = paste(round(Freq / sum(Freq) * 100, 1), "%")),
            position = position_stack(vjust = 0.5)) + 
  theme_classic() + 
  coord_polar("y") +
  theme(plot.title = element_text(hjust=0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = "Kurs",
       x = NULL,
       y = NULL) + scale_fill_manual(labels=c("AUD (201)", "EINFINF (322)", "SCHLÜKO (303)", "EWIF(130)"), values=c("#004a4a", "#d68a00", "#8b0956", "#0DED82")) +
  theme_void()

tab6 <- as.data.frame(table(exam_data$subject_name, exam_data$student_type))

pie_labels <- (tab6 %>% group_by(Var2) %>% 
                 mutate(per=paste0(round(Freq/sum(Freq)*100, 2), "%")) %>% 
                 ungroup %>% select(per))$per

labels <- c("0" = "Bildungsinländer (733)", "1" = "Bildungsausländer (223)")

ggplot(tab6, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = position_fill()) +
  geom_text(aes(label = paste(pie_labels)), position = position_fill(vjust = 0.5)) +
  coord_polar(theta = "y") +
  facet_wrap(~ Var2,labeller=labeller(Var2 = labels)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank()
  )  + 
  theme(legend.position='bottom',strip.background = element_rect(colour="black", fill="gray")) + 
  guides(fill=guide_legend(nrow=2, byrow=TRUE))+ 
  scale_fill_manual(labels = c("AUD (201)", "EINFINF (322)", "SCHLÜKO (303)", "EWIF(130)"), values=c("#004a4a", "#d68a00", "#8b0956", "#0DED82")) + labs(fill = "Kurs")


degree_1 <- as.data.frame(table(exam_data$degree_name,exam_data$student_type,
                                exam_data$subject_name))

#Informatics


degree_1_1 <- degree_1[degree_1$Var1 == 'IF',] 
pie_labels <- (degree_1_1 %>% group_by(Var2) %>% 
                 mutate(per=paste0(round(Freq/sum(Freq)*100, 2), "%")) %>% 
                 ungroup %>% select(per))$per


labels <- c('0' = " Bildungsinländer (445)", '1' = "Bildungsausländer (80)")
ggplot(degree_1_1, aes(x = "", y = Freq, fill = factor(Var3))) +
  geom_bar(stat = "identity", position = position_fill()) +
  geom_text(aes(label = paste(pie_labels)), position = position_fill(vjust = 0.5)) +
  coord_polar(theta = "y") +
  facet_wrap(~ Var2,labeller=labeller(Var2 = labels)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank()
  ) + ggtitle('Informatik IF (525)') + 
  theme(plot.title = element_text(hjust = 0.5),legend.position='bottom',strip.background = element_rect(colour="black", fill="gray")) + 
  guides(fill=guide_legend(nrow=2, byrow=TRUE))+ 
  scale_fill_manual(labels = c("AUD (137)", "EINFINF (201)", "SCHLÜKO (181)", "EWIF(6)"), values=c("#004a4a", "#d68a00", "#8b0956", "#0DED82")) + labs(fill = "Kurs")


degree_1_1 <- degree_1[degree_1$Var1 == 'WIF',] 
pie_labels <- (degree_1_1 %>% group_by(Var2) %>% 
                 mutate(per=paste0(round(Freq/sum(Freq)*100, 2), "%")) %>% 
                 ungroup %>% select(per))$per


labels <- c('0' = " Bildungsinländer (288)", '1' = "Bildungsausländer (143)")
ggplot(degree_1_1, aes(x = "", y = Freq, fill = factor(Var3))) +
  geom_bar(stat = "identity", position = position_fill()) +
  geom_text(aes(label = paste(pie_labels)), position = position_fill(vjust = 0.5)) +
  coord_polar(theta = "y") +
  facet_wrap(~ Var2,labeller=labeller(Var2 = labels)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank()
  ) + ggtitle('Wirtschaftsinformatik WIF (431)') + 
  theme(plot.title = element_text(hjust = 0.5),legend.position='bottom',strip.background = element_rect(colour="black", fill="gray")) + 
  guides(fill=guide_legend(nrow=2, byrow=TRUE))+ 
  scale_fill_manual(labels = c("AUD (64)", "EINFINF (121)", "SCHLÜKO (122)", "EWIF(124)"), values=c("#004a4a", "#d68a00", "#8b0956", "#0DED82")) + labs(fill = "Kurs")




degree_1_2 <- as.data.frame(table(exam_data$degree_name,exam_data$student_type,exam_data$subject_name))

levels(degree_1_2$Var3) <- c("AUD","EINFINF", "SCHLÜKO","EWIF")

percent_labs <- (degree_1_2 %>% group_by(Var3) %>%
                   mutate(per=paste0(round(Freq/sum(Freq)*100, 2), "%")) %>% 
                   ungroup %>% select(per))$per

ggplot(data = degree_1_2, aes(x = Var1, y = Freq, fill = Var2)) + 
  geom_bar(stat = "identity", width = 1, position = position_dodge()) +
  geom_label(aes(x = Var1, y = Freq, group = Var2,label = paste(Freq)),
             position = position_dodge(width = 1),size = 3,vjust = ifelse(degree_1_2$Freq < 6, -0.5, -0.1)) +
  facet_wrap(~Var3, strip.position = "bottom", scales = "free_x", nrow = 2) +
  xlab("Kurs")+scale_fill_manual(labels = c("Bildungsin.", "Bildungsaus."), values=c("#EC7A78", "#55A5C4")) + labs(fill = "HZB-Art") + ylab("Anzahl der Prüfungsaufzeichnungen") + scale_y_continuous(limits = c(0, 200))+
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(), strip.placement = "outside", panel.border = element_blank())

tab8 <- as.data.frame(table(exam_data$subject_name, exam_data$subject_year))
PlotXTabs2(tab8, y =Var1 , x = Var2, counts = Freq, perc.k = 2, legend.title = "Kurs", legend.position = 'top', results.subtitle = F, x.axis.orientation = "vertical", label.text.size = 3) +
  scale_fill_manual(labels = c("AUD (201)", "EINFINF (322)", "SCHLÜKO (303)", "EWIF(130)"),values=c("#004a4a", "#d68a00", "#8b0956", "#0DED82")) + 
  scale_x_discrete(name ="Kohorte", labels=c("Coh-16-17","Coh-17","Coh-17-18", "Coh-18","Coh-18-19","Coh-19", "Coh19-20","Coh-20")) + ylab("Verteilung der Prüfungsversuche") +
  theme(axis.text.y=element_blank())



tab9_0 <- as.data.frame(table(exam_data$student_type,exam_data$points))

tab9_0$Percentage1 <- ifelse(tab9_0$Var1 == 0,(tab9_0$Freq/733)*100,
                             (tab9_0$Freq/223)*100)

ggplot(data = tab9_0, aes(x = Var2, y = Percentage1, fill = Var1)) + 
  geom_bar(stat = "identity", width = 0.9, position = position_dodge(width = 1)) + 
  geom_label(aes(x = Var2, y = Percentage1, fill = Var1,label = paste(round(Percentage1,2))),position = position_dodge(width = 1),size = 2.5) + labs(x = 'Note', y = 'Prozent') +scale_fill_manual(name = "HZB-Art",labels = c("Bildungsin.", "Bildungsaus."), values=c("#EC7A78", "#55A5C4")) + theme(legend.position = 'top')
  

# Frequency of passing 
#In first attempt check how many passed


setwd("C:/Users/ysa pc/Desktop/internship/HiWi/qualiman/")

temp_sub_frame <- NULL
temp_sub_frame <- as.data.frame(temp_sub_frame)

#Get the first occurence
for (subs in c('aud','einfinf','schlueko','tewif')){
  
  exam_data <- import('./processed_data/exams/allexams.xlsx')
  exam_data <- exam_data %>% filter(subject_name == subs)
  exam_data1 <- exam_data[!duplicated(exam_data$matriculation_number), ] 
  
  #removed all occurences of duplicate except first (so first try results only)
  
  #exam_data_dup <- exam_data[duplicated(exam_data$matriculation_number),]
  #This is for duplicated 
  
  exam_data1 <- exam_data1 %>% filter(points < 5) %>% mutate(pass_attempt = 1)
  temp_sub_frame  <- dplyr::bind_rows(temp_sub_frame,exam_data1)
}

setwd("C:/Users/ysa pc/Desktop/internship/HiWi/qualiman/")

temp_sub_frame1 <- NULL
temp_sub_frame1 <- as.data.frame(temp_sub_frame1)

for (subs in c('aud','einfinf','schlueko','tewif')){
  exam_data <- import('./processed_data/exams/allexams.xlsx')
  dupli_counts <- exam_data[exam_data$subject_name == subs,]
  dupli_counts <- subset(dupli_counts,duplicated(matriculation_number) | duplicated(matriculation_number, fromLast=TRUE))
  
  for (rowid in unique(dupli_counts$matriculation_number)){
    temp_dupli_counts <- dupli_counts[dupli_counts$matriculation_number == rowid,]
    temp_dupli_counts$pass_attempt <- nrow(temp_dupli_counts)
    temp_dupli_counts <- tail(temp_dupli_counts,1)
    temp_sub_frame1  <- dplyr::bind_rows(temp_sub_frame1,temp_dupli_counts)
    
  }
  
}

temp_sub_frame1 <- temp_sub_frame1 %>% filter(points < 5)

temp_sub_frame <- dplyr::bind_rows(temp_sub_frame,temp_sub_frame1)

pass_frame <- as.data.frame(table(temp_sub_frame$pass_attempt, temp_sub_frame$student_type,temp_sub_frame$subject_name))

levels(pass_frame$Var3) <- c("AUD","EINFINF", "SCHLÜKO","EWIF")


pass_frame %>% filter(Freq > 0) %>% ggplot(., aes(x = Var1, y = Freq, fill = Var2)) + 
  geom_bar(stat = "identity", width = 1, position = position_dodge()) +
  geom_label(aes(x = Var1, y = Freq, group = Var2,label = paste(Freq)),
             position = position_dodge(width = 1),size = 2.5,vjust = 0.2) +
  facet_wrap(~Var3, strip.position = "bottom", scales = "free_x", nrow = 2) +
  xlab("Kurs")+scale_fill_manual(labels = c("Bildungsin.", "Bildungsaus."), values=c("#EC7A78", "#55A5C4")) + labs(fill = "HZB-Art") + ylab("Anzahl Studierende") + scale_y_continuous(limits = c(0, 270))+
  theme(panel.spacing = unit(0, "lines"), legend.position = 'top', 
        strip.background = element_blank(), strip.placement = "outside", panel.border = element_blank())


temp_sub_frame <- NULL
temp_sub_frame <- as.data.frame(temp_sub_frame)

#Get the last occurence
for (subs in c('aud','einfinf','schlueko','tewif')){
  
  
  exam_data <- import('./processed_data/exams/allexams.xlsx')
  exam_data <- exam_data %>% filter(subject_name == subs)
  exam_data1 <- exam_data[!rev(duplicated(rev(exam_data$matriculation_number))),]
  
  #exam_data_dup <- exam_data[duplicated(exam_data$matriculation_number),]
  #This is for duplicated 
  exam_data1 <- exam_data1 %>% filter(points == 5) 
  temp_sub_frame  <- dplyr::bind_rows(temp_sub_frame,exam_data1)
}


fail_frame <- as.data.frame(table(temp_sub_frame$degree_year, temp_sub_frame$student_type,temp_sub_frame$subject_name))

levels(fail_frame$Var3) <- c("AUD","EINFINF", "SCHLÜKO","EWIF")

fail_frame %>% filter(Freq > 0) %>% ggplot(., aes(x = Var1, y = Freq, fill = Var2)) + 
  geom_bar(stat = "identity", width = 1, position = position_dodge()) +
  geom_label(aes(x = Var1, y = Freq, group = Var2,label = paste(Freq)),
             position = position_dodge(width = 1),size = 2.5,vjust = 0.2) +
  facet_wrap(~Var3, strip.position = "bottom", scales = "free_x", nrow = 2) +
  xlab("Kurs")+scale_fill_manual(labels = c("Bildungsin.", "Bildungsaus."), values=c("#EC7A78", "#55A5C4")) + labs(fill = "HZB-Art") + ylab("Anzahl Studierende") + scale_y_continuous(limits = c(0, 35))+
  theme(panel.spacing = unit(0, "lines"), legend.position = 'top', 
        strip.background = element_blank(), strip.placement = "outside", panel.border = element_blank())


temp_sub_frame <- NULL
temp_sub_frame <- as.data.frame(temp_sub_frame)

#Get the first occurence
for (subs in c('aud','einfinf','schlueko','tewif')){
  
  exam_data <- import('./processed_data/exams/allexams.xlsx')
  exam_data <- exam_data %>% filter(subject_name == subs)
  exam_data1 <- exam_data[!duplicated(exam_data$matriculation_number), ] 
  
  #removed all occurences of duplicate except first (so first try results only)
  
  #exam_data_dup <- exam_data[duplicated(exam_data$matriculation_number),]
  #This is for duplicated 
  
  temp_sub_frame  <- dplyr::bind_rows(temp_sub_frame,exam_data1)
}

# total 896 observations counted as first tries, we have 956 observations in total
# so 60 observations are of 2nd or more tries


#setwd("C:/Users/ysa pc/Desktop/internship/HiWi/qualiman/")
#exam_data <- import('./processed_data/exams/allexams.xlsx')

#levels(temp_sub_frame) <- c("AUD","EINFINF", "SCHLÜKO","EWIF")
c("AUD","EINFINF", "SCHLÜKO","EWIF")

temp_sub_frame$subject_name <- factor(temp_sub_frame$subject_name, labels = c("AUD","EINFINF", "SCHLÜKO","EWIF"))

#durchgefallen / bestanden
temp_sub_frame %>% mutate(fail_pass = ifelse(points == 5, 'durchgefallen', 'bestanden')) %>%
  group_by(subject_name, student_type, fail_pass) %>% summarise(Freq = n()) %>% ungroup() %>% mutate_each(funs(as.character), student_type) %>%
  ggplot(data = ., aes(x = fail_pass, y = Freq, fill = student_type)) + 
  geom_bar(stat = "identity", width = 1, position = position_dodge()) +
  geom_label(aes(x = fail_pass, y = Freq, group = student_type,label = paste(Freq)),
             position = position_dodge(width = 1), size = 3) +
  facet_wrap(~subject_name, strip.position = "bottom", scales = "free_x", ncol = 2 
             ) +
  xlab("Subjects")+scale_fill_manual(labels = c("Bildungsin.", "Bildungsaus."), values=c("#EC7A78", "#55A5C4", "#C18ABC")) + labs(fill = "HZB-Art") + ylab("Anzahl Studierende") + xlab("Kurs") + scale_y_continuous(limits = c(0, 250))+
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(), strip.placement = "outside",panel.border = element_blank(), legend.position = 'top')


setwd("C:/Users/ysa pc/Desktop/internship/HiWi/qualiman/")

uniassist_data <- import('processed_data/international_students/internationalstudentsSS17-WS19-20_corrected.csv')


uniassist_data %>% group_by(Continent) %>% summarise(counts = dplyr::n()) %>% ggplot(., aes(x = Continent, y = counts)) + 
  geom_col() + 
  geom_label(aes(x = Continent, y = counts,label = counts)) +
  theme(legend.position = "none") + scale_y_continuous(limits = c(0, 20)) + 
  labs(x = 'Region', y = 'Anzahl Studierende') + theme(axis.text.x = element_text(angle = 90))

library(stringi)

keywords <- as.matrix(c("FSP", "telc", "GI", "DSH","TestDaF","TNB", "DSD-2"))
lang_col <- sapply(stri_extract_all_regex(uniassist_data$Sprachbemerkung, paste(keywords[,1], collapse = '|')), toString)
uniassist_data$germanexam = gsub("(.*?),.*", "\\1", lang_col)
lang_col <- as.data.frame(table(gsub("(.*?),.*", "\\1", lang_col)))

uniassist_data %>% group_by(germanexam) %>% summarise(counts = dplyr::n()) %>% ggplot(., aes(x = germanexam, y = counts)) + 
  geom_col() + 
  geom_label(aes(x = germanexam, y = counts,label = counts)) +
  theme(legend.position = "none") + scale_y_continuous(breaks=c(1,3,5,7,9,11)) + 
  labs(x = 'Sprachnachweis', y = 'Anzahl Studierende') 



matched_schlueko_res <- 
  dplyr::inner_join(uniassist_data,exam_data %>% filter(subject_name == 'schlueko'),by = c("Matrikel-Nr." = "matriculation_number"))
#find unique students w.r.t to germanexam

unique_schleuko <-  matched_schlueko_res[!duplicated(matched_schlueko_res$`Matrikel-Nr.`), ] 

give.n <- function(x){
  return(c(y = median(x)*1.10, label = length(x))) 
  # experiment with the multiplier to find the perfect position
}


ggplot(unique_schleuko, aes(x = germanexam, y = points)) + geom_boxplot() +
  geom_point() + stat_summary(fun.data = give.n, geom = "text", fun.y = median, position = position_dodge(width = 0.75)) + ylab("Note") + xlab("Sprachnachweis")
  



setwd("C:/Users/ysa pc/Desktop/internship/HiWi/qualiman/")

temp_sub_frame <- NULL
temp_sub_frame <- as.data.frame(temp_sub_frame)

for (subs in c('aud','einfinf','schlueko','tewif')){
  exam_data <- import('./processed_data/exams/allexams.xlsx')
  dupli_counts <- exam_data[exam_data$subject_name == subs,]
  dupli_counts <- subset(dupli_counts,duplicated(matriculation_number) | duplicated(matriculation_number, fromLast=TRUE))
  
  for (rowid in unique(dupli_counts$matriculation_number)){
    temp_dupli_counts <- dupli_counts[dupli_counts$matriculation_number == rowid,]
    temp_dupli_counts$count <- nrow(temp_dupli_counts)
    temp_dupli_counts <- tail(temp_dupli_counts,1)
    temp_sub_frame  <- dplyr::bind_rows(temp_sub_frame,temp_dupli_counts)
    
  } 
}

give.n <- function(x){
  return(c(y = median(x)*1.07, label = length(x))) 
  # experiment with the multiplier to find the perfect position
}

temp_sub_frame$subject_name <- factor(temp_sub_frame$subject_name, labels = c("AUD","EINFINF", "SCHLÜKO","EWIF"))

temp_sub_frame %>%  
  mutate_each(funs(as.character), student_type) %>% 
  ggplot(.,aes(x = subject_name, y = points, fill = student_type)) +
  geom_boxplot() + geom_point(position=position_dodge(width=0.75),aes(group=student_type)) + stat_summary(fun.data = give.n, geom = "text", fun.y = median, position = position_dodge(width = 0.75)) + xlab("Kurs") + ylab("Note") + scale_fill_manual(labels = c("Bildungsin.", "Bildungsaus."), values=c("#EC7A78", "#55A5C4", "#C18ABC")) + labs(fill = "HZB-Art") + theme(legend.position = 'top')



indiviudalsubjHZB <- function(subjnamestr){
  
  tab11_1 <- exam_data[exam_data$subject_name == sprintf('%s',subjnamestr),]
  tab11_1 <- as.data.frame(table(tab11_1$student_type,tab11_1$points))
  ggplot(tab11_1, aes(x = Var2, y = Freq, fill = Var1))+
    geom_bar(stat = "identity", width = 1, position = position_dodge()) +
    geom_label(aes(x = Var2, y = Freq,label = paste(Freq)),
               position = position_dodge(width = 1)) +
    xlab("grade") + scale_fill_manual(labels = c("Bildungsin.", "Bildungsaus."), 
                                      values=c("#EC7A78", "#55A5C4")) + labs(fill = "HZB-Art",x = "Note", y = "Anzahl der Prüfungsaufzeichnungen") + 
    ggtitle("SCHLÜKO") +
    theme(plot.title = element_text(hjust = 0.5), legend.position = 'top')
  
}

indiviudalsubjHZB('schlueko')  

from <- list(c("1","1.3","1.7"), c("2","2.3", "2.7"), c("3","3.3","3.7"))
to   <- c("1-1.7", "2-2.7","3-3.7")

find.in.list <- function(x, y) match(TRUE, sapply(y, `%in%`, x = x))



sidebysidesubjHZB <- function(subjnamestr){
  
  
  tab11_1 <- exam_data[exam_data$subject_name == sprintf('%s',subjnamestr),]
  tab11_1 <- as.data.frame(table(tab11_1$student_type,tab11_1$points))
  
  idx.in.list  <- sapply(levels(tab11_1$Var2), find.in.list, from)
  levels(tab11_1$Var2)  <- ifelse(is.na(idx.in.list), levels(tab11_1$Var2), to[idx.in.list])
  
  tab11_1 <- tab11_1 %>% group_by(Var1,Var2) %>%
    summarise(Freq = sum(Freq))
  
  figsave <- PlotXTabs2(tab11_1, y =Var2 , x = Var1, counts = Freq, perc.k = 2,data.label = "both",
                        results.subtitle = F,label.text.size = 4, label.fill.alpha = 0.3, sample.size.label = F,
                        legend.title = "Note", title = 'SCHLÜKO') +
    scale_fill_manual(values=c("#C58C2F", "#384BB6", "#D13E3E","#7a55c4","#b6e700")) + 
    scale_x_discrete(name ="HZB-Art", labels=c("Bildungsin.", "Bildungsaus.")) + 
    coord_flip() 
  
  return(figsave)
  
  
  
}

gradehzb1 <- sidebysidesubjHZB('aud')+ theme(axis.title.x=element_blank(),
                                             axis.text.x=element_blank(),
                                             axis.ticks.x=element_blank(), legend.position = "top",
                                             axis.title.y = element_blank(),
)

gradehzb2  <- sidebysidesubjHZB('tewif')+ theme(axis.title.x=element_blank(),
                                                axis.text.x=element_blank(),
                                                axis.ticks.x=element_blank(),
                                                axis.title.y = element_blank(),legend.position = "none",
)

gradehzb3 <- sidebysidesubjHZB('einfinf')+ theme(axis.title.x=element_blank(),
                                                 axis.text.x=element_blank(),
                                                 axis.ticks.x=element_blank(),
                                                 axis.title.y = element_blank(),legend.position = "none",
)

gradehzb4 <- sidebysidesubjHZB('schlueko')+ theme(axis.title.x=element_blank(),
                                                  axis.text.x=element_blank(),
                                                  axis.ticks.x=element_blank(), legend.position = "none",
                                                  axis.title.y = element_blank(),
)

library(gridExtra)
grid.arrange(gradehzb1, gradehzb2, gradehzb3, gradehzb4, nrow=4)





aud_sub_table <- exam_data[exam_data$subject_name == 'tewif',] 

aud_sub_table <- as.data.frame(table(aud_sub_table$subject_year, aud_sub_table$degree_name, aud_sub_table$student_type))

levels(aud_sub_table$Var1) <- c("Coh-17","Coh-17-18","Coh-18-19","Coh-19-20", "Coh-20")

#levels(aud_sub_table$Var1) <- c("2016-17 WiSe","2017 SoSe","2017-18 WiSe", "2018 SoSe", "2018-19 WiSe","2019 SoSe", "2019-20 WiSe","2020 SoSe")


ggplot(data = aud_sub_table, aes(x = Var2, y = Freq, fill = Var3)) + 
  geom_bar(stat = "identity", width = 1, position = position_dodge()) +
  geom_label(aes(x = Var2, y = Freq, group = Var3,label = paste(Freq)),
             position = position_dodge(width = 1)) +
  facet_wrap(~Var1, strip.position = "bottom", scales = "free_x", ncol = 6 ) +
  scale_fill_manual(labels = c("Bildungsin.", "Bildungsaus."), values=c("#EC7A78", "#55A5C4")) + labs(fill = "HZB-Art") + xlab("Kohorte") + ylab("Anzahl") + 
theme(panel.spacing = unit(0, "lines"), 
      strip.background = element_blank(), strip.placement = "outside",panel.border = element_blank(),strip.text.x = element_text(angle = 90))
























