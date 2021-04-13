#'  All the data used in plotting rendered plots when required to be downloaded
#'  which are passed as functions to the server 'downloadHandler()' function


#options(box.path = "./")

box::use(./sec_cohort_analysis)
box::use(./help_functions)

box::use(magrittr[...], dplyr[filter, arrange])

#' K1 criteria plotting function which takes in degree string as argument

#' @export
k1_criteria <- function(criteria_degree){

  get_data <- sec_cohort_analysis$criteria_plotting("k1")
  print(criteria_degree)
  if (criteria_degree == "k1dke"){


    req_frame <- filter(get_data, Studiengang == "MDKE")

    #' Dynamic Labels for x-axis labels -----------------------------------------
    #' Each label on x-axis also displays absolute number which was used in
    #' calculation of that following statistic (Bar, box, etc.)
    #' The following piece of code will calculate that from the data to be added
    #' to the plot.
    #' TODO: Try to write it as a common function for all plots (Currently using
    #' different variables for its calculation)

    #req_frame$variety <- as.factor(req_frame$variety)


    #--------------------------------------------------------------------------------

    return(req_frame)

  }

  else {

    req_frame <- filter(get_data, Studiengang == "MDigiEng")

    #Dynamic Labels for plots
    #req_frame$variety <- as.factor(req_frame$variety)

    return(req_frame)

  }

}

#' K2 criteria plotting function which takes in degree string as argument

#' @export
k2_criteria <- function(criteria_degree){

  get_data <- sec_cohort_analysis$criteria_plotting("k2")

  if (criteria_degree == "k2dke"){

    req_frame <- filter(get_data, Studiengang == "MDKE")

    #Dynamic Labels for plots
    #req_frame$variety <- as.factor(req_frame$variety)


    return(req_frame)

  }

  else if(criteria_degree == "k2de") {

    req_frame <- filter(get_data, Studiengang == "MDigiEng")

    #Dynamic Labels for plots
    #req_frame$variety <- as.factor(req_frame$variety)

    return(req_frame)

  }

  else {

    req_frame <- filter(get_data, Studiengang == "MDigiEng" | Studiengang ==  "MDKE") #Overall comparison boxplots between two degrees

    #req_frame$Studiengang <- as.factor(req_frame$Studiengang)

    return(req_frame)

  }

}

#' K2 criteria (absolute) plotting function which takes in degree string as argument.
#' It plots absolute numbers

#' @export
k2_absol_criteria <- function(criteria_degree){

  if(criteria_degree == "k2dkeabsol") {

    get_data <- sec_cohort_analysis$criteria_plotting_absol("k2absolDKE")
    get_data$LV.Datum.Semester <- as.factor(get_data$LV.Datum.Semester)
    sv <- levels(get_data$LV.Datum.Semester)

    #Reset level for properly setting Year-semester (SS16, WS16-17,....)
    get_data$LV.Datum.Semester <- factor(get_data$LV.Datum.Semester, levels =
                                           levels(get_data$LV.Datum.Semester)[order(substr(sv,3,5))])

    #sem_labels <- help_functions$make_plot_label(levels(get_data$LV.Datum.Semester))
    get_data <- get_data %>% arrange(LV.Datum.Semester)


    return(get_data)

  }

  else if (criteria_degree == "k2deabsol"){

    get_data <- sec_cohort_analysis$criteria_plotting_absol("k2absolDE")
    get_data$LV.Datum.Semester <- as.factor(get_data$LV.Datum.Semester)

    sv <- levels(get_data$LV.Datum.Semester)

    #Reset level for properly setting Year-semester (SS16, WS16-17,....)
    get_data$LV.Datum.Semester <- factor(get_data$LV.Datum.Semester, levels =
                                           levels(get_data$LV.Datum.Semester)[order                                                                      (substr(sv,3,5))])

    #sem_labels <- help_functions$make_plot_label(levels(get_data$LV.Datum.Semester))
    get_data <- get_data %>% arrange(LV.Datum.Semester)

    return(get_data)

  }

  else{


    get_data <- sec_cohort_analysis$criteria_plotting_absol("k2absolboth")

    #get_data$Studiengang <- as.factor(get_data$Studiengang)

    return(get_data)

  }

}
