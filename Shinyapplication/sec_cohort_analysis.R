#' This program allows us to write various functions for cohort analysis
#' Currently implemented K1, K2
#' Add further functions here as required



#box::unload(db_controller)
#box::unload(help_functions)
box::use(./help_functions)
box::use(./db_controller)
box::use(dplyr = dplyr[filter, group_by, summarise, n, select, slice, arrange,
                       inner_join, mutate, setdiff, distinct, full_join, left_join, bind_rows, count], stats = stats[sd])
box::use(magrittr[...])
box::use(plyr[join_all])

#' Overall statistics table which takes degree name as string

#' @export
statstable <- function(degree_string){

  get_conn <- db_controller$create_connection()
  ua <- db_controller$load_data(get_conn,'vw_uni_assist_report')
  db_controller$disconnect_conn(get_conn)


  ua_help <- select(ua, Studiengang, Studienbeginn, HZB_Note, Geschlecht, Sprachniveau, Studienbeginn_Alter)

  rm(ua)

  ua_help$HZB_Note <- as.numeric(ua_help$HZB_Note)

    ua_help$Studienbeginn =  factor(ua_help$Studienbeginn) #To ensure that 0/empty rows get displayed

    #' Each dataframe represents one column of the table, which is concatenated
    #' at the end. Column names are as described in summarise().Write other required
    #' columns in format f'n'_df, where n is column number

  f1_df <- ua_help %>% filter(., Studiengang == degree_string ) %>%
    group_by(Studienbeginn,.drop = FALSE) %>%
    summarise(Anzahl = n())

  f2_df <- ua_help %>% filter(., Studiengang == degree_string & Geschlecht == '1') %>%
  group_by(Studienbeginn,.drop = FALSE) %>%
  summarise(Geschlecht_M = n())

  f3_df <- ua_help %>% filter(., Studiengang == degree_string & Geschlecht == '2') %>%
  group_by(Studienbeginn,.drop = FALSE) %>%
  summarise(Geschlecht_W = n())

  f4_df <- ua_help %>% filter(., Studiengang == degree_string & !is.na(HZB_Note)) %>%
  group_by(Studienbeginn,.drop = FALSE) %>%
    summarise(HZB_Note = mean(HZB_Note))

  f5_df <- ua_help %>% filter(., Studiengang == degree_string & !is.na(HZB_Note)) %>%
  group_by(Studienbeginn,.drop = FALSE) %>%
  summarise('SD-Abweichung von Mean' = sd(HZB_Note))

  f6_df <- ua_help %>% filter(., Studiengang == degree_string & !is.na(HZB_Note)) %>%
  group_by(Studienbeginn,.drop = FALSE) %>%
  summarise('Menge, die Note hat' = n())

  f7_df <- ua_help %>% filter(., Studiengang == degree_string & !is.na(Studienbeginn_Alter)) %>% group_by(Studienbeginn,.drop = FALSE) %>%
  summarise(Alter = mean(Studienbeginn_Alter))

  f8_df <- ua_help %>% filter(., Studiengang == degree_string & !is.na(Studienbeginn_Alter)) %>% group_by(Studienbeginn,.drop = FALSE) %>%
  summarise(SD_Alter = sd(Studienbeginn_Alter))

  f9_df <- ua_help %>% filter(., Studiengang == degree_string & Sprachniveau=="B2") %>%   group_by(Studienbeginn,.drop = FALSE) %>%
  summarise(B2 = n())

  f10_df <- ua_help %>% filter(., Studiengang == degree_string & Sprachniveau=="C1") %>% group_by(Studienbeginn,.drop = FALSE) %>%
  summarise(C1 = n())

  f11_df <- ua_help %>% filter(., Studiengang == degree_string & Sprachniveau=="C2") %>% group_by(Studienbeginn,.drop = FALSE) %>%
  summarise(C2 = n())

  f12_df <- ua_help %>% filter(., Studiengang == degree_string & Sprachniveau=="") %>%
  group_by(Studienbeginn,.drop = FALSE) %>%
  summarise('Sprache nicht leer' = n())


  joined <- join_all(list(f1_df, f2_df, f3_df, f4_df, f5_df, f6_df, f7_df,
                              f8_df, f9_df, f10_df, f11_df, f12_df),
                         by='Studienbeginn', type='left')

  return(joined)

}

#' Internal function which wrangles data from view of database required accordingly
#' to criterias.

criteria_preprocess <- function(){

    get_conn <- db_controller$create_connection()
    lu <- db_controller$load_data(get_conn,'vw_lu_report')
    ua <- db_controller$load_data(get_conn,'vw_uni_assist_report')
    db_controller$disconnect_conn(get_conn)

    ua_help <- filter(ua, pseudo_matriculation_nbr != "?")

    lu$LV.Datum.Semester <- help_functions$make_date_to_semester(as.character(lu$LV_Datum))
    lu$LV_Note <- help_functions$make_float_american(as.character(lu$LV_Note) )
    lu$LV_CP <- help_functions$make_float_american(as.character(lu$LV_CP) )
    #Classify note into quaters and create a new column

    lu$LV_Note.Disc <- cut(lu$LV_Note, c(-Inf, 1.5, 2.5, 3.5, 4, 5),
                           labels = c("[1,1.5]", "(1.5,2.5]", "(2.5,3.5]", "(3.5,4]", "5"))

    #lu[which(lu$LV_Name=="Master of Science "),"LV_Name"] <- "Master of Science"

    #Remove any examination which is external and show if required
    extern <- filter(lu, LV_Extern=="J")
    lu_help <- setdiff(lu, extern)
    extern <- distinct(select(extern, pseudo_matriculation_nbr))

    rm(extern) #Currently not required, Open and write script here if required in future

    #Remove Modules and subject results which fall under additional courses
    lu_help <- filter(lu_help, LV_Themengebiet != "Zusaetzliche Leistungen")

    #Find out in which semester of their masters timeline since start, course exam was taken?
    lu_help$start_as_num <- help_functions$make_semester_to_num(lu_help$Startsemester)
    lu_help$LVDate_as_num <- help_functions$make_date_to_semester(lu_help$LV_Datum)
    lu_help$LVDate_as_num <- help_functions$make_semester_to_num(lu_help$LVDate_as_num)
    lu_help$semesters_since_start <- (lu_help$LVDate_as_num - lu_help$start_as_num +1)
    # For each student get their last attended course exam info and the semesters
    # since his admission for that course exam
    lu_ <- lu_help %>%
      group_by(pseudo_matriculation_nbr) %>%
      arrange(desc(semesters_since_start)) %>%
      slice(1) %>% select(pseudo_matriculation_nbr, semesters_since_start)

    #Find out how many credits should have been completed till then
    # (Based on the criteria that student should atleast have 30 cp in each semester)

    lu_$expected_cp <- (lu_$semesters_since_start * 30)
    lu_$expected_cp <- help_functions$cut_max_cp(lu_$expected_cp)

    # Need to understand ??
    lu_K1 <- lu_help %>%
      group_by(pseudo_matriculation_nbr) %>%
      summarise(sum = help_functions$cut_max_cp(sum(LV_CP, na.rm=TRUE)))


    lu_K1 <- full_join(lu_, lu_K1, by="pseudo_matriculation_nbr")
    lu_K1$criterion1 <- (lu_K1$sum / lu_K1$expected_cp)
    lu_K1 <- select(lu_K1, pseudo_matriculation_nbr, criterion1)
    lu_K1$criterion1[is.na(lu_K1$criterion1)] <- 0


    df <- left_join(ua_help, lu_help, by = "pseudo_matriculation_nbr") %>%
      group_by(pseudo_matriculation_nbr) %>% summarise(criterion2 = (sum(LV_Status ==  "bestanden", na.rm = T) / sum(LV_Status %in% c("bestanden", "nicht bestanden"), na.rm = T)),criterion3 = mean(LV_Note[LV_Status == "bestanden" & LV_Note < 5], na.rm = T)) %>% inner_join(ua_help)

    df <- left_join(df, lu_K1, by="pseudo_matriculation_nbr")


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

    return(df)

  }


#' Get data according to required criteria with some minor changes

#' @export
criteria_plotting <- function(reqstring){


  req_frame <- criteria_preprocess()

  if (reqstring == 'k1') {

    variety <- req_frame$Studienbeginn
    Studiengang <- req_frame$Studiengang
    note <- req_frame$criterion1
    datak <- data.frame(variety, Studiengang, note)

    return(datak)
  }

  else if (reqstring == 'k2') {

    variety <- req_frame$Studienbeginn
    Studiengang <- req_frame$Studiengang
    note <- req_frame$criterion2
    datak <- data.frame(variety, Studiengang, note)

    return(datak)

  }


}

#' Get data according to required k2 criteria (Absolute) with some minor changes.

#' @export
criteria_plotting_absol <- function(reqstring){



  get_conn <- db_controller$create_connection()
  lu <- db_controller$load_data(get_conn,'vw_lu_report')
  lu$LV.Datum.Semester <- help_functions$make_date_to_semester(as.character(lu$LV_Datum    ))
  lu$LV_Note <- help_functions$make_float_american(as.character(lu$LV_Note) )
  lu$LV_CP <- help_functions$make_float_american(as.character(lu$LV_CP) )
  #Classify note into quaters and create a new column

  lu$LV_Note.Disc <- cut(lu$LV_Note, c(-Inf, 1.5, 2.5, 3.5, 4, 5),
                       labels = c("[1,1.5]", "(1.5,2.5]", "(2.5,3.5]", "(3.5,4]", "5"))

  db_controller$disconnect_conn(get_conn)
  absol <- select(lu, Studiengang, Startsemester,
                       pseudo_matriculation_nbr, LV.Datum.Semester)

   if (reqstring == 'k2absolDKE'){


    absol_MDKE <- filter(absol, Studiengang == "MDKE") %>%
    count(LV.Datum.Semester)

    absol_MDKE$Studiengang <- "MDKE"

    return(absol_MDKE)
    }


  else if (reqstring == 'k2absolDE'){

    absol_DE <- filter(absol, Studiengang == "MDigiEng") %>%
      count(LV.Datum.Semester)

    absol_DE$Studiengang <- "MDigiEng"

    return(absol_DE)
  }

  else if (reqstring == 'k2absolboth'){

    absol_MDKE <- filter(absol, Studiengang == "MDKE") %>%
      count(LV.Datum.Semester)

    absol_MDKE$Studiengang <- "MDKE"

      absol_DE <- filter(absol, Studiengang == "MDigiEng") %>%
        count(LV.Datum.Semester)

    absol_DE$Studiengang <- "MDigiEng"

    absol_comb <- bind_rows(absol_MDKE, absol_DE)

    return(absol_comb)

  }

}




