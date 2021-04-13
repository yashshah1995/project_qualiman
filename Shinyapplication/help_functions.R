substrRight <- function(x, n){
  #Input:
  # x - Complete String
  # n - number of Characters to count from right
  #Output: last n characters of the x
  substr(x, nchar(x)-n+1, nchar(x))
}

get_semester_year <- function(date_list){
  # Input: date_list - a list with on vector of three string elements; containing Day, Month, Year in different orders
  # Output:the year in short notation 2018 --> 18 as string
  month <- as.integer(date_list[[1]][2])
  year  <- as.integer( substrRight(date_list[[1]][ which.max(date_list[[1]]) ], 2) )
  if( (1<= month) && (month<4) ){ 
    year <- year-1
  }
  return(as.character(year))
}

get_semester_string <- function(date_list)  {
  # Input: date_list - a list with on vector of three string elements; containing Day, Month, Year in different orders
  # Output: String with the assignment to the semester
  if(as.integer(as.integer(date_list[[1]][2])) %in% c(4:9)){
    sem <- "SS"
  } else {
    sem <- "WS"
  }
  return(sem)
}

get_semster_from_date <- function(date_string,seperator){
  # Input: 
  # date_string - String with a date in formate YYYY-MM-DD or TT.MM.JJJJ
  # seperator - String with the seperator
  # Output: String with semster label
  sem  <- get_semester_string(strsplit(date_string,seperator))
  year <- get_semester_year(strsplit(date_string,seperator))
  return(stringr::str_c(sem,year))
}


make_date_to_semester <- function(date_column){ # Neue Funktion mit den Parametern x und y
  # Input: Column with different entries of dates
  # Output: Column with labels of semester

  result <- vector(mode="character", length=0)
  for (var in (1:length(date_column))){
    var <- date_column[var]
    var <- as.character(var)
    if( (is.na(var) ) || (var=="") || grepl(var, "NA")){
      str <- "NA"
      # print("NA") # debug
    }else{
      if(grepl("-",var, fixed=TRUE)) {
        # date as YYYY-MM-DD
        str <- get_semster_from_date(var,"-")
      }
      if(grepl(".",var, fixed=TRUE)) {
        # date as DD.MM.YYYY
        str <- get_semster_from_date(var,"[.]")    
      }
      if(grepl("^\\s*$",var, fixed=TRUE)) {
        # date as e.g SS 2017
        str <- stringr::str_c(
          substr(var, start = 1, stop = 2),
          substrRight(var,2)
        )
      }
    }#else NA
    result <- c(result,str)
  }#for
  rm(str,var)
  return(result)
}# end function

make_float_american <- function(numeric_column){
  #rm(result)
  result <- vector(mode="numeric", length=0)
  for(var in numeric_column){
    var <- as.character(var)
    if( is.na(var) || var=="" || grepl(var, "NA")){
      str <- "NA"
    }else{
      str <- gsub(",", ".", var)
    }
    result <- c(result,as.numeric(str))
  }
  rm(str,var)
  return(result)
}


make_next <- function(str){
  if(substrRight(str,2)=="WS"){
    str2 <- stringr::str_c((as.integer(substr(str, 1, 2))+1),"SS")
  }else{
    str2 <- stringr::str_c(as.integer(substr(str, 1, 2)),"WS")
  }
  return(str2)
}  


get_columns <- function(col){
  # creats from a single column with semester valuses (e.g.ss15) a sorted 
  # vector from the min value to the max value  that can be used as columnwise ts to see gaps
  sem <- vector()
  for(str in unique(col)){
    str <- stringr::str_c(substrRight(str,2),substr(str, 1, 2))
    sem <- c(sem,str)
  }
  min <- min(unique(sem))
  max <- max(unique(sem))
  sem <- vector()
  str <- min
  while(str != max){
    sem <- c(sem,stringr::str_c(substrRight(str,2),substr(str, 1, 2)))
    str <- make_next(str)
  }
  sem <- c(sem,stringr::str_c(substrRight(max,2),substr(max, 1, 2)))
  return(sem)
}


###########
adjust_startsemester <-function(str){
# {}       -> "NA" +
# ss15     -> SS15 +
# SS 15    -> SS15 +
# SoSe 2015-> SS15 +  
# WS 15/16 -> WS15 +
# WS15/16  -> WS15 +
# WS 1516  -> WS15 +
# WS1516   -> WS15 +
# WiSe 2015-> WS15 +
# WS15     -> WS15 +
# WS 15    -> WS15 +
  
  #str=""
  #str="WS15/16"
  
  # catch empty string
  
  if( (grepl("SoSe", str, fixed=TRUE)==TRUE) || (grepl("SS", str, fixed=TRUE)==TRUE) ){
    sem  <- "SS"
    year <- substr(str, 3,4)
    te <- paste0(sem,year)
    return(te)
  }
  

  
  if( (grepl("WiSe", str, fixed=TRUE)==TRUE) || (grepl("WS", str, fixed=TRUE)==TRUE) ){
    sem  <- "WS"
    year <- substr(str, 3,4)
    te <- paste0(sem, year)
    return(te)
  }
  
  
  # 
  # if( (grepl("SS", str, fixed=TRUE)==TRUE) || (grepl("SOSE", str, fixed=TRUE)==TRUE) ){
  #   sem  <- "SS"
  #   year <- substrRight(str,2)
  #   #print(stringr::str_c(sem,year))
  #   return(stringr::str_c(sem,year))
  # }
  # 
  # if( (grepl("WS", str, fixed=TRUE)==TRUE) || (grepl("WISE", str, fixed=TRUE)==TRUE) ){
  #   sem  <- "WS" 
  #   if(nchar(str)==4){ # WS15 -> WS15 
  #     #print(str)
  #     return(str)
  #   } else if(nchar(str)==5){ # WS 15 -> WS15 
  #     year <- substrRight(str,2)
  #     #print(stringr::str_c(sem,year))
  #     return(stringr::str_c(sem,year))
  #   } else if(grepl("WISE", str, fixed=TRUE)==TRUE){ # WiSe 2015 -> WS15
  #     year <- substrRight(str,2)
  #     #print(stringr::str_c(sem,year))
  #     return(stringr::str_c(sem,year))
  #   } else if(grepl("/", str, fixed=TRUE)==TRUE){ # WS 15/16 -> WS15 & WS15/16 -> WS15
  #     year <- substr(substrRight(str,5), 1, 2)
  #     #print(stringr::str_c(sem,year))
  #     return(stringr::str_c(sem,year))
  #   } else {  # WS 1516 -> WS15 & WS1516 -> WS15
  #     year <- substr(substrRight(str,4), 1, 2)
  #     #print(stringr::str_c(sem,year))
  #     return(stringr::str_c(sem,year))
  #   }
  # } 
     
  # return("ERROR")
}
###############











get_sortable_sem <-function(str){
  # ss15    --> 15.1
  # SS17    --> 17.1
  # ws15/16 --> 15.2
  # WS 15/16 --> 15.2
  
  #str="SoSe 2015"
  
  #print(str)
  
  # if(str=="NA" || str==""  || str==" "){
  #  return("NA") 
  # } else {
  # 
  # 
  # # if( ( (grepl("/", str)==TRUE) && (!grepl(" ", str)==TRUE) ) || ( (grepl("/", str)==TRUE) && (grepl(" ", str)==TRUE) ) ){
  # #   str <- adjust_startsemester(str)
  # # }
  # # print(str)
  # # if( (grepl(" ", str)==TRUE) && !(grepl("/", str)==TRUE) ){
  # #   str <- adjust_startsemester1(str)
  # # }
  # 
  # 
  # str <- adjust_startsemester(str)
  # 
  # #print(str)
  # 
  # #print(stringr::str_c(substrRight(str,2),ifelse(toupper(substr(str, 1, 2))=="SS",".1",".2")))
  # 
  # 
  # return(stringr::str_c(substrRight(str,2),ifelse(toupper(substr(str, 1, 2))=="SS",".1",".2")))
  
  #}
  
  # year <- substr(str,3,4)
  # 
  # if(grepl("SoSe",str, fixed=TRUE)==TRUE || grepl("SS",str, fixed = TRUE)){
  #   sem <- ".1"
  # } else {
  #   sem <- ".2"
  # }
  
  if((grepl(" SoSe", str, fixed=TRUE)==TRUE) || (grepl("SS", str, fixed=TRUE)==TRUE) ) {
    sem  <- ".1"
    year <- substr(str,3,4)
  } 
  if((grepl(" WiSe", str, fixed=TRUE)==TRUE) || (grepl("WS", str, fixed=TRUE)==TRUE) ){
    sem <- ".2"   
    year <- substr(str,3,4)
  }
  
  if((grepl("SoSe ", str, fixed=TRUE)==TRUE) ) {
    sem  <- ".1"
    year <- substrRight(str,2)
  } 
  if((grepl("WiSe ", str, fixed=TRUE)==TRUE) ){
    sem <- ".2"   
    year <- substrRight(str,2)
  }
  
  return(paste0(year, sem))
  
}


















get_unsortable_sem <-function(str){
  # 15.1 --> ss15
  # 15.2 --> ws15
  if(str=="NA" || str==""){
    return("NA") 
  }
  return(stringr::str_c(ifelse( substrRight(str,2)==".1","SS","WS"),substr(str, 1, 2)))
}

# adjust_startsemester1 <- function(str){
#   # SS 15   -> SS15
#   # WS 1516 -> WS15
#   if(str=="NA" || str==""){
#     return("NA") 
#   }
#   sem <- toupper(substr(str, 1, 2))
#   if(sem=="WS") {
#     year <-  substr(substrRight(str,4), 1, 2)
#   }else{
#     year <- substrRight(str,2)
#   }
#   return( stringr::str_c(sem,year) )
# }


get_unsortable_old_sem <-function(str){
  # 15.1 --> SS 15
  # 15.2 --> WS 1516
  if(str=="NA" || str==""){
    return("NA") 
  }
  return(stringr::str_c(ifelse( substrRight(str,2)==".1","SS","WS")," ",substr(str, 1, 2),ifelse( substrRight(str,2)==".1","", as.integer(substr(str, 1, 2))+1)))
}
  

  
# adjust_startsemester <- function(str){
#   # Passe das LU Startdatum des Studierenden in der Form Ws 15/16 an die neue Form WS15 an 
#   #str <- "WS 15/16"
#   if(str=="NA" || str==""){
#     return("NA") 
#   }
#   list_str <- unlist(strsplit("SoSe 2017", " ", fixed = TRUE))
#   
#   if(grepl(" ", str)==TRUE){
#     if(list_str[1]== "SoSe" || "WiSe"){
#       
#       return(adjust_startsemester_SoWiSe_YYYY(str))
#     }    
#   }
#   sem <- toupper(substr(str, 1, 1))#old: toupper
#   sem <- paste(sem, "S", sep="")
#   if(grepl("/",str, fixed=TRUE)) {
#     year <-  substr(substrRight(str,5), 1, 2)
#   }else{
#     year <- substrRight(str, 2)
#   }
#   return( stringr::str_c(sem,year) )
# }
# 
# 
# 
# adjust_startsemester_SoWiSe_YYYY <- function(str){
#   "SoSe 2017 -> SS17"
#   "WiSe 2015 --> WS15"
#   if(str=="NA" || str==""){
#     return("NA") 
#   }
#   year <- substrRight(str, 2)# 17
#   sem <- stringr::str_c(substr(str, 1, 1),"S")
#   return(stringr::str_c(sem,year))
# }    





# get_study_grade <- function(grade){
#   mod <- grade %% 1
#   if( mod<0.3){
#     mod <- as.numeric(paste0(floor(grade),".0"))
#   } else if(mod<0.5){
#     mod <- as.numeric(paste0(floor(grade),".3"))
#   }else if(mod<0.7){
#     mod <- as.numeric(paste0(floor(grade),".5"))
#   }else{
#     mod <- as.numeric(paste0(floor(grade),".5"))
#   }
#   
# } 


# plot_data_column = function (data, semester){
#   
#   ### Begin PLOT ###
#   tmp_data <- data#ts_cp_sem_rowise
#   #tmp_str_title <- "Criterium 4 - ECTS per semester"
#   tmp_str_x     <- stringr::str_c("ECTS (",semester,")")
#   tmp_str_y     <- "Number of Students"
#   ###
#   gg <- ggplot(tmp_data %>%
#                  dplyr::filter(Semester==semester) ,aes(x=CP)) +
#     geom_histogram(binwidth = 1,
#                    colour = barlines_blue, fill = barfill_blue)+
#     scale_x_continuous(name = tmp_str_x,
#                        breaks = seq(0, 40, 1),#seq(0, 130, 2)
#                        limits=c(0, 40)) +
#     scale_y_continuous(name = tmp_str_y)+
#     #ggtitle(tmp_str_title)+
#     theme_bw()+
#     theme(plot.title = element_text(hjust = 0.5))#+
#   #geom_text(stat='count', aes(label=..count..), vjust=-1)
#   gg
#   ###
#   gname    <- "Kriterium4_-_ECTS_per_semester"
#   gtype    <- "Hist"      # "Hist", "Line", "Boxplot", "Pie", "Scatterplot", etc. 
#   gwidth   <- 40
#   gheight  <- 15
#   gdate    <- stringr::str_c("_",semester)          # "_(TT.MM.2018)" e.g. _(14.05.2018)
#   ###
#   gname    <- stringr::str_c(gtype,"_",gname,"_",project_short) # e.g. Hist_ECTS_QM3_PNG_(14.05.2018).png
#   if(flag_save_plots)     {ggsave(stringr::str_c(pdf_folder,gname,       gdate,".pdf"), width = gwidth, height = gheight, units = "cm") }  
#   if(flag_save_plots_svg) {ggsave(stringr::str_c(svg_folder,gname,"_SVG",gdate,".svg"), width = gwidth, height = gheight, units = "cm") }
#   if(flag_save_plots_png) {ggsave(stringr::str_c(png_folder,gname,"_PNG",gdate,".png"), width = gwidth, height = gheight, units = "cm") } 
#   rm(tmp_data,tmp_str_title,tmp_str_x,tmp_str_y,gname,gtype,gwidth,gheight,gdate,gg)
#   
# }

lm_eqn_ECTS <- function(df){
  x <- df$Semester
  y <- df$CP  
  m <- lm(y ~ x, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

#Help fct:
make_semester_to_num <- function(semester_col){ 
  result <- vector(mode="numeric", length=0)
  for (var in semester_col){
    if(is.na(var) | var==""){
      num <- "NA"
    }else{
      if (var == "SoSe 2015" || var == "SS15" || var =="SoSe 15" || var =="SS 15"   || var =="2015 SoSe") { num <- 1}
      if (var == "WiSe 2015" || var == "WS15" || var =="WiSe 15" || var =="WS 1516" || var =="2015/2016 WiSe") { num <- 2}
      if (var == "SoSe 2016" || var == "SS16" || var =="SoSe 16" || var =="SS 16"   || var =="2016 SoSe") { num <- 3}
      if (var == "WiSe 2016" || var == "WS16" || var =="WiSe 16" || var =="WS 1617" || var =="2016/2017 WiSe") { num <- 4}
      if (var == "SoSe 2017" || var == "SS17" || var =="SoSe 17" || var =="SS 17"   || var =="2017 SoSe") { num <- 5}
      if (var == "WiSe 2017" || var == "WS17" || var =="WiSe 17" || var =="WS 1718" || var =="2017/2018 WiSe") { num <- 6}
      if (var == "SoSe 2018" || var == "SS18" || var =="SoSe 18" || var =="SS 18"   || var =="2018 SoSe") { num <- 7}
      if (var == "WiSe 2018" || var == "WS18" || var =="WiSe 18" || var =="WS 1819" || var =="2018/2019 WiSe") { num <- 8}
      if (var == "SoSe 2019" || var == "SS19" || var =="SoSe 19" || var =="SS 19"   || var =="2019 SoSe") { num <- 9}
      if (var == "WiSe 2019" || var == "WS19" || var =="WiSe 19" || var =="WS 1920" || var =="2019 WiSe") { num <- 10}
    }
    result <- c(result, num)
  }#for
  return(result)
}# end function

cut_max_cp <- function(cp_col){
  result <- vector(mode="numeric", length=0)
  for (var in cp_col) {
    if (var > 90) {
      cp <- 90
    } else {
      cp <- var
    }
    result <- c(result, cp)
  }
  return (result)
}

vec_drop_sem_before_start <- function(col){
  #start <- as.integer(as.character(substrRight(first(semester_labels),2)))
  start <- as.integer(as.character(substrRight(head(semester_labels,n=1),2)))
  
  res <- vector('character')
  for(row in col){
    if( as.integer(as.character(substrRight(row,2))) < start  ){
      res <- c(res,row)
    }
  }
  return(res)
  #rm(start,row,res,col)
}

create_semester_to_num <- function(){
  # needs data.frame "lu" and "semester_labels"
  # output: df that translates sortable semester to 
  
  # check <- FALSE
  # if(!exists("df_semester_translation")) {
  #   check <- TRUE
  #   lu <- read.csv(stringr::str_c(data_folder_sub,"lu",".csv"),stringsAsFactors=FALSE)
  # }
  
  last_semester_project <-  lu %>%
    dplyr::filter(LV.Datum.Semester != "NA") %>%
    dplyr::select("LV.Datum.Semester") %>%
    dplyr::distinct() 
  
  last_semester_project <- as.data.frame(last_semester_project)

  last_semester_project <- last_semester_project %>%
    dplyr::filter( !(last_semester_project$LV.Datum.Semester %in% vec_drop_sem_before_start(last_semester_project$LV.Datum.Semester) ) )

  for(i in 1:nrow(last_semester_project)){
    last_semester_project[i,"LV.Datum.Semester"] <- get_sortable_sem(last_semester_project[i,"LV.Datum.Semester"])
  }
  
  last_semester_project <- last_semester_project %>%
    dplyr::select(LV.Datum.Semester) %>%
    dplyr::arrange(LV.Datum.Semester) %>%
    dplyr::slice(n())

  #print(last_semester_project)
  
  # first_semester_project <- get_sortable_sem(semester_labels[1])
  first_semester_project <- get_sortable_sem("SS15")
  
  #print(first_semester_project)
  
  options(stringsAsFactors=FALSE)
  cur <- as.character(first_semester_project)
  sem_num <- 1
  df_semester_translation <- data.frame("Semester_sortabel" = as.character(cur), "semester_number" = sem_num)
  while(cur != last_semester_project){
    str <- strsplit(cur, "\\.")
    year <- as.integer(str[[1]][1])
    sem <- as.integer(str[[1]][2])
    if(sem == 1){
      cur <- as.character(stringr::str_c(year,".","2"))
    }
    if(sem == 2){
      cur <- as.character(stringr::str_c( as.integer(year+1),".","1"))
    }
    sem_num <- sem_num + 1
    df_semester_translation <- df_semester_translation %>%
      rbind(c(cur,sem_num))
    rm(str,year,sem)
  }
  rm(cur,sem_num)
  
  # if(check == TRUE){
  #   check <- FALSE
  #   rm(lu)
  # }
  return(df_semester_translation) 
}  

get_semester_number_from_string <- function(str){
  # input: string with sortable semester e.g. 18.1
  # output: translation into integer value e.g. 7
  # Start counting from SS15

    # check <- FALSE
  # if(!exists("df_semester_translation")) {
  #   check <- TRUE
  #   df_semester_translation <- read.csv(stringr::str_c(data_folder_sub,"df_semester_translation",".csv"),stringsAsFactors=FALSE)
  # }

  # if(!exists("lookup_sortable_table")) {
  #   lookup_table <- create_lookup_sortable_table()
  # }
  # 
  # lk <- as.data.frame(lookup_table)
  # i <- 0
  # for(row in 1:nrow(lk)) {
  #   lk$number[row] <- row
  # }
  
  if (grepl("14.2", str)==TRUE) { return(0) }
  if (grepl("15.1", str)==TRUE) { return(1) }
  if (grepl("15.2", str)==TRUE) { return(2) }
  if (grepl("16.1", str)==TRUE) { return(3) }
  if (grepl("16.2", str)==TRUE) { return(4) }
  if (grepl("17.1", str)==TRUE) { return(5) }
  if (grepl("17.2", str)==TRUE) { return(6) }
  if (grepl("18.1", str)==TRUE) { return(7) }
  if (grepl("18.2", str)==TRUE) { return(8) }
  if (grepl("19.1", str)==TRUE) { return(9) }
  if (grepl("19.2", str)==TRUE) { return(10) }
  if (grepl("20.1", str)==TRUE) { return(11) }
  if (grepl("20.2", str)==TRUE) { return(12) }

}
  

  # for(row in 1:nrow(lk)) {
  #   if(lk$lookup_table[row] == str) {
  #     res <- as.integer(lk$number[row])
  #   }
  # }

  #res <- lk$number[where(lk, lk$lookup_table == str)]
  # res <- lk[which(lk$lookup_table == str)]
  # res <- res$number
  # res <- as.data.frame(res)
  
  #lu[which(lu$LV_Name=="Master of Science "),"LV_Name"] <- "Master of Science"
  

  #lk <- as.table(lk)
  
  # if( is.na(str)  || is.na(str) || is_empty(str)){
  #   res <- NA
  # }else if( as.integer(substr(str, 1, 2)) < as.integer( substrRight(semester_labels[1],2) )  ){
  #   res <- NA
  # }else{
    #res <- as.integer( df_semester_translation[which(df_semester_translation$Semester_sortabel == as.character(str)  ),"semester_number"] )
    #res <- as.integer(match(str, lk))
  #}
    
  # if(check == TRUE){
  #   check <- FALSE
  #   rm(df_semester_translation)
  # }



create_lookup_sortable_table <- function(){
  lookup_table <- semester_labels
  for(i in 1:length(lookup_table)){
    lookup_table[i] <- get_sortable_sem(lookup_table[i])
  }
  lookup_table <- sort(lookup_table, decreasing = FALSE)
  return(lookup_table)
}



get_last_semester_project <- function(){
  # needs data.frame "lu"
  # output: character with sortable semester label of the latest semester in lu 
  
  check <- FALSE
  if(!exists("df_semester_translation")) {
    check <- TRUE
    lu <- read.csv(stringr::str_c(data_folder_sub,"lu",".csv"),stringsAsFactors=FALSE)
  }
  
  last_semester_project <-  lu %>%
    dplyr::filter(LV.Datum.Semester != "NA") %>%
    dplyr::select("LV.Datum.Semester") %>%
    dplyr::distinct() 
  
  last_semester_project <- as.data.frame(last_semester_project)
  
  last_semester_project <- last_semester_project %>%
    dplyr::filter( !(last_semester_project$LV.Datum.Semester %in% vec_drop_sem_before_start(last_semester_project$LV.Datum.Semester) ) )
  
  for(i in 1:nrow(last_semester_project)){
    last_semester_project[i,"LV.Datum.Semester"] <- get_sortable_sem(last_semester_project[i,"LV.Datum.Semester"])
  }
  
  last_semester_project <- last_semester_project %>%
    dplyr::select(LV.Datum.Semester) %>%
    dplyr::arrange(LV.Datum.Semester) %>%
    dplyr::slice(n())

  last_semester_project <- as.character( last_semester_project$LV.Datum.Semester[1] )
    
  
  if(check == TRUE){
    check <- FALSE
    rm(lu)
  }
  return(last_semester_project) 
}  

na_to_zero <- function(col){
  for(i in 1:length(col)){
    if(is.na(col[i])){
      col[i] <- 0
    }
  }
  return(col)
}

### Make DKE / DigiEng to MDKE / MDigiEng
adjust_studiengang <- function(studiengang_col){
  result <- vector(mode="character", length=0)
  for (var in studiengang_col) {
    if (var == "DKE") { new <- "MDKE"}
    else { new <- "MDigiEng"}
    result <- c(result, new)
  }
  return(result)
}

fmt_dcimals <- function(decimals=0){
  # return a function responpsible for formatting the 
  # axis labels with a given number of decimals 
  function(x) as.character(round(x,decimals))
}

get_GradesAsLabel <- function(mycolumn){
  # Input column with continuous mean grades
  # column discrete and with labels according to the 'Pruefungsordnung'
  if(is.null(mycolumn)){
    return(NULL)
  }else{
   return(cut(mycolumn, c(-Inf, 1.5, 2.5, 3.5, 4.5),labels=c("sehr gut","gut","befriedigend","ausreichend")))
  }
}

adjust_region <- function(region_col){
  result <- vector(mode="character", length=0)
  for (var in region_col) {
    if (var == "%Fernost%") { new <- "Fernost"}
    if (var == "%Indien%") {new <- "Indien"}
    else { new <- var}
    result <- c(result, new)
  }
  return(result)
}

