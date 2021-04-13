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

#Separate data with SoSe2020

exam_data$SoSe20 <- ifelse(exam_data$subject_year == '20', 1,0)

SoSe_data <- exam_data[exam_data$SoSe20 == 1,]

exam_data <- exam_data[exam_data$SoSe20 != 1,]

table(SoSe_data$student_type)

#Side-by-side course and HZB. distribution in each subject

degree_1_2 <- as.data.frame(table(SoSe_data$degree_name,SoSe_data$student_type,SoSe_data$subject_name))

percent_labs <- (degree_1_2 %>% group_by(Var3) %>%
                   mutate(per=paste0(round(Freq/sum(Freq)*100, 2), "%")) %>% 
                   ungroup %>% select(per))$per

ggplot(data = degree_1_2, aes(x = Var1, y = Freq, fill = Var2)) + 
  geom_bar(stat = "identity", width = 1, position = position_dodge()) +
  geom_label(aes(x = Var1, y = Freq, group = Var2,label = paste(Freq)),
             position = position_dodge(width = 1),size = 3.5,vjust = -0.1) +
  geom_text(aes(x = Var1, y = Freq, group = Var2,label = paste(percent_labs)),
            position = position_dodge(width = 1),vjust = 1.5,size = 3.5)+
  facet_wrap(~Var3, strip.position = "bottom", scales = "free_x") +
  xlab("Subjects")+scale_fill_manual(labels = c("Bildungsin.", "Bildungsaus.", 
                                                "schlueko (88)"), values=c("#EC7A78", "#55A5C4", "#C18ABC")) + labs(fill = "HZB.") + 
  theme(panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(), strip.placement = "outside", panel.border = element_blank())


#Side-by-side subject distribution for each cohort.

tab6 <- as.data.frame(table(SoSe_data$subject_name, SoSe_data$student_type))

pie_labels <- (tab6 %>% group_by(Var2) %>% 
                 mutate(per=paste0(round(Freq/sum(Freq)*100, 2), "%")) %>% 
                 ungroup %>% select(per))$per

labels <- c("0" = "Bildungsinländer (119)", "1" = "Bildungsausländer (25)")

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
  scale_fill_manual(labels = c("aud (60)", "einfinf (10)", "schlueko (74)"), values=c("#004a4a", "#d68a00", "#8b0956")) + labs(fill = "subjects")

# Student information

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


unique_exam_data <-  SoSe_data[!rev(duplicated(rev(SoSe_data$matriculation_number))),]

# Get individual dataframe of subjects

data_aud <- SoSe_data[SoSe_data$subject_name == "aud",]
data_einfinf <- SoSe_data[SoSe_data$subject_name == "einfinf",]
data_schlueko <- SoSe_data[SoSe_data$subject_name == "schlueko",]
 

#Get unique students

unique_aud <- data_aud[!rev(duplicated(rev(data_aud$matriculation_number))),] #remove all duplicate keep last
unique_einfinf <- data_einfinf[!rev(duplicated(rev(data_einfinf$matriculation_number))),]
unique_schlueko <- data_schlueko[!rev(duplicated(rev(data_schlueko$matriculation_number))),]

#Find students who took pair of 2 subjects

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


tab15 <- as.data.frame(table(unique_exam_data2$subject_name,unique_exam_data2$degree_name,unique_exam_data2$student_type))

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




# For only 2 subjects data we need to reduce the rows which were repeated
# For all 3 subjects and then find for which subjects they were repeated



matching_data_aud_schlu <- matching_data_aud_schlu[!(matching_data_aud_schlu$matriculation_number %in% matching_data_3$matriculation_number),]


matching_data_aud_einfinf <- matching_data_aud_einfinf[!(matching_data_aud_einfinf$matriculation_number %in% matching_data_3$matriculation_number),]

matching_data_einfinf_schlu <- matching_data_einfinf_schlu[!(matching_data_einfinf_schlu$matriculation_number %in% matching_data_3$matriculation_number),]


final_dataset <- NULL
final_dataset <- as.data.frame(final_dataset)

##########

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

# Grade distribution & Comparison

## grade distribution for each cohort and compare

#```{r,fig.width = 10, fig.asp = .60}

tab9_0 <- as.data.frame(table(exam_data[exam_data$student_type == 0,]$points,
                              exam_data[exam_data$student_type == 0,]$SoSe20))

Percentage1 = (tab9_0 %>% group_by(Var2) %>% 
                 mutate(per=paste0(round(Freq/sum(Freq)*100, 2), "%")) %>% 
                 ungroup %>% select(per))$per

ggplot(tab9_0, aes(x = Var1, y = Percentage1, fill = Var2)) + 
  geom_bar(stat = "identity",width = 1, position = position_dodge(width = 0.6)) +
  geom_label(aes(x = Var1, y = Percentage1, group = Var2,label = paste(Percentage1)),
             position = position_dodge(width = 0.6),size = 3.5,vjust = -0.1) +
  geom_text(aes(x = Var1, y = Percentage1, group = Var2,label = paste(Freq)),
            position = position_dodge(width = 0.6),vjust = 1.5,size = 3.5)+
  scale_fill_manual(labels = c("other_semesters", "SoSe-20"), values = c("#3F94F4", "#F4DE3F")) + 
  labs(x = 'grade', y = 'distribution') + ggtitle("Bildungsinländer(innen)") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.y = element_blank())

#----------------------------------------------
tab9_0 <- as.data.frame(table(exam_data[exam_data$student_type == 1,]$points,
                              exam_data[exam_data$student_type == 1,]$SoSe20))

Percentage1 = (tab9_0 %>% group_by(Var2) %>% 
                 mutate(per=paste0(round(Freq/sum(Freq)*100, 2), "%")) %>% 
                 ungroup %>% select(per))$per

ggplot(tab9_0, aes(x = Var1, y = Percentage1, fill = Var2)) + 
  geom_bar(stat = "identity", width = 1, position = position_dodge(width = 0.5)) +
  geom_label(aes(x = Var1, y = Percentage1, group = Var2,label = paste(Percentage1)),
             position = position_dodge(width = 1),size = 3.5,vjust = -0.1) +
  geom_text(aes(x = Var1, y = Percentage1, group = Var2,label = paste(Freq)),
            position = position_dodge(width = 1),vjust = 1.5,size = 3.5)+
  scale_fill_manual(labels = c("other_semesters", "SoSe-20"), values = c("#3F94F4", "#F4DE3F")) + 
  labs(x = 'grade', y = 'distribution') + ggtitle("Bildungsausländer(innen)") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.y = element_blank())



from <- list(c("1","1.3","1.7"), c("2","2.3", "2.7"), c("3","3.3","3.7"))
to   <- c("1-1.7", "2-2.7","3-3.7")

find.in.list <- function(x, y) match(TRUE, sapply(y, `%in%`, x = x))
idx.in.list  <- sapply(levels(tab10$Var2), find.in.list, from)
levels(tab10$Var2)  <- ifelse(is.na(idx.in.list), levels(tab10$Var2), to[idx.in.list])

sidebysidesubjHZB <- function(subjnamestr){
  
  
  tab11_1 <- exam_data[exam_data$subject_name == sprintf('%s','aud'),]
  tab11_1 <- as.data.frame(table(tab11_1$student_type, tab11_1$SoSe20,tab11_1$points))
  tab11_1 <- tab11_1[tab11_1$Var1 == 0,]
  idx.in.list  <- sapply(levels(tab11_1$Var3), find.in.list, from)
  levels(tab11_1$Var3)  <- ifelse(is.na(idx.in.list), levels(tab11_1$Var3), to[idx.in.list])
  
  tab11_1 <- tab11_1 %>% group_by(Var2,Var3) %>%
    summarise(Freq = sum(Freq))
  
  figsave <- PlotXTabs2(tab11_1, y =Var3 , x = Var2, counts = Freq, perc.k = 2,data.label = "both", results.subtitle = F,label.text.size = 4, label.fill.alpha = 0.3, sample.size.label = F,legend.title = "grade", title = 'aud') +
    scale_fill_manual(values=c("#C58C2F", "#384BB6", "#D13E3E","#7a55c4","#b6e700")) + 
    scale_x_discrete(name ="HZB.", labels=c("other_semesters", "SoSe-20")) + 
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

library(gridExtra)
grid.arrange(gradehzb1, gradehzb2, gradehzb3, nrow=3)


#Overall difference 

#Overall difference (For each cohort)

#Subject wise difference (For each cohort)

other_semesters <- exam_data %>% filter(SoSe20 == 0) %>% select(points) %>% pull
sose20 <- exam_data %>% filter(SoSe20 == 1) %>% select(points) %>% pull

germans_isnormal = shapiro.test(other_semesters)
ifelse(germans_isnormal$p.value > 0.05, "Normality is satisfied in general", "Normality is NOT satisfied in general")
qqnorm(other_semesters, main = "Bildungsinländer(innen) Grades Q-Q Plot")
abline(0,1)

int_isnormal = shapiro.test(sose20)
ifelse(int_isnormal$p.value > 0.05, "Normality is satisfied in general", "Normality is NOT satisfied in general")
qqnorm(sose20, main = "Bildungsausländer(innen) Grades Q-Q Plot")
abline(0,1)


result = wilcox.test(points ~ SoSe20, data = exam_data, paired = FALSE)

ifelse(result$p.value >= 0.05, "not", "")


other_semesters_0 <- exam_data %>% filter(student_type == 0,SoSe20 == 0) %>% select(points) %>% pull

sose20_0 <- exam_data %>% filter(student_type == 0,SoSe20 == 1) %>% select(points) %>% pull

germans_isnormal = shapiro.test(other_semesters_0)
ifelse(germans_isnormal$p.value > 0.05, "Normality is satisfied in general", "Normality is NOT satisfied in general")
qqnorm(other_semesters_0, main = "Bildungsinländer(innen) Grades Q-Q Plot")
abline(0,1)

int_isnormal = shapiro.test(sose20_0)
ifelse(int_isnormal$p.value > 0.05, "Normality is satisfied in general", "Normality is NOT satisfied in general")
qqnorm(sose20_0, main = "Bildungsausländer(innen) Grades Q-Q Plot")
abline(0,1)


result = wilcox.test(points ~ SoSe20, data = exam_data[exam_data$student_type == 0,], paired = FALSE)


as.data.frame(table(exam_data[exam_data$SoSe20 == 1,]$student_type,exam_data[exam_data$SoSe20 == 1,]$subject_name))
