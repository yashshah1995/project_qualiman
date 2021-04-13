#'  All the rendering functions for the required plots are written here
#'  which are passed as functions to the server 'renderPlot()' function


#options(box.path = "./")

box::use(./sec_cohort_analysis)
box::use(./help_functions)

box::use(ggplot2[...], magrittr[...], dplyr[filter, select, group_by, summarise, n, arrange],scales[percent])

# Helpful labels for Plot Y-axis labels. (Directly copied from the Markdown report)

str_crit <- list(criterion1 = "K1 = Anz. gesammelter CP / \nAnz. Studiensemester * 30CP", criterion2 = "K2 = Anz. bestandener PrC<fungen / \nAnz. abgelegter PrC<fungen",
                 criterion3 = "K3 = Durchschnittsnote aller\nbestandenen LV",
                 criterion4 = "K4 = CP pro Semester",
                 criterion5 = "K5 = Durchschnittsnote im Semester",
                 criterion6 = "K6 = Anz. bestandener PrC<fungen im Semester / \nAnz. abgelegter PrC<fungen im Semester")

#GGplot2 global settings for plots

MDKE_Color     <- "#9fb059" #green (For DKE)
DE_Color      <- "#edae52" #orangen (For DE)

#global GGplot aesthetics

th <- theme_bw() + theme(plot.title = element_text( size=7),
                         axis.text.x =  element_text(size = 10, angle=90, margin=margin(t=7, r=-40, b=7, l=0)), axis.text.y =  element_text(size = 10),
                         axis.title.y = element_text(face="bold", size=10))

#' K1 criteria plotting function which takes in degree string as argument

#' @export
k1_criteria <- function(criteria_degree){

  get_data <- sec_cohort_analysis$criteria_plotting("k1")

  if (criteria_degree == "k1dke"){


    req_frame <- filter(get_data, Studiengang == "MDKE")

    #' Dynamic Labels for x-axis labels -----------------------------------------
    #' Each label on x-axis also displays absolute number which was used in
    #' calculation of that following statistic (Bar, box, etc.)
    #' The following piece of code will calculate that from the data to be added
    #' to the plot.
    #' TODO: Try to write it as a common function for all plots (Currently using
    #' different variables for its calculation)

    req_frame$variety <- as.factor(req_frame$variety)
    sem_labels <- help_functions$make_plot_label(levels(req_frame$variety))

    count_vals <- req_frame %>% group_by(variety) %>% summarise(count_vals = n()) %>% select(count_vals)

    label_vals = c()

    for (i in 1:length(levels(req_frame$variety))) {

      label_vals <- append(label_vals,
                           sprintf("Kohorte %s [%s]",sem_labels[i],count_vals[i,1]))

    }

    #--------------------------------------------------------------------------------

    g <- ggplot(req_frame, aes(x=variety, y=note, fill=Studiengang)) +
      geom_boxplot(show.legend = F, varwidth = T,outlier.colour=MDKE_Color, outlier.shape=16, outlier.size=1, notch=FALSE, lwd=0.1, fatten= 4) +
      scale_fill_manual(values=c(MDKE_Color)) + scale_y_continuous(labels = percent, breaks=c(0, 0.25, 0.5, 0.75, 1, 1.25), limits=c(0, 1.25)) +
      scale_x_discrete(limits=levels(req_frame$variety), labels= label_vals ) +
      scale_color_manual(values = c(MDKE_Color)) +  labs(x = "", y = str_crit$criterion1) + th

    return(g)

  }

  else {

    req_frame <- filter(get_data, Studiengang == "MDigiEng")

    #Dynamic Labels for plots
    req_frame$variety <- as.factor(req_frame$variety)
    sem_labels <- help_functions$make_plot_label(levels(req_frame$variety))

    count_vals <- req_frame %>% group_by(variety) %>% summarise(count_vals = n()) %>%        select(count_vals)

    label_vals = c()

    for (i in 1:length(levels(req_frame$variety))) {

      label_vals <- append(label_vals,
                           sprintf("Kohorte %s [%s]",sem_labels[i],count_vals[i,1]))

    }

    g <- ggplot(req_frame, aes(x=variety, y=note, fill=Studiengang)) +
      geom_boxplot(show.legend = F, varwidth = T,outlier.colour=DE_Color, outlier.shape=16, outlier.size=1, notch=FALSE, lwd=0.1, fatten= 4) +
      scale_fill_manual(values=c(DE_Color)) + scale_y_continuous(labels = percent, breaks=c(0, 0.25, 0.5, 0.75, 1, 1.25), limits=c(0, 1.25)) +
      scale_x_discrete(limits=levels(get_data$variety), labels= label_vals ) +
      scale_color_manual(values = c(DE_Color)) +  labs(x = "", y = str_crit$criterion1) + th

    return(g)

  }

}

#' K2 criteria plotting function which takes in degree string as argument

#' @export
k2_criteria <- function(criteria_degree){

  get_data <- sec_cohort_analysis$criteria_plotting("k2")

  if (criteria_degree == "k2dke"){

    req_frame <- filter(get_data, Studiengang == "MDKE")

    #Dynamic Labels for plots
    req_frame$variety <- as.factor(req_frame$variety)
    sem_labels <- help_functions$make_plot_label(levels(req_frame$variety))

    count_vals <- req_frame %>% group_by(variety) %>% summarise(count_vals = n()) %>%        select(count_vals)

    label_vals = c()

    for (i in 1:length(levels(req_frame$variety))) {

      label_vals <- append(label_vals,
                           sprintf("Kohorte %s [%s]",sem_labels[i],count_vals[i,1]))

    }

    g <- ggplot(req_frame, aes(x=variety, y=note, fill=Studiengang)) +
      geom_boxplot(show.legend = F, varwidth = T,outlier.colour=MDKE_Color, outlier.shape=16, outlier.size=1, notch=FALSE, lwd=0.1, fatten= 4) +
      scale_fill_manual(values=c(MDKE_Color)) + scale_y_continuous(labels = percent, breaks=c(0, 0.25, 0.5, 0.75, 1), limits=c(0, 1)) +
      scale_x_discrete(limits=levels(get_data$variety), labels= label_vals ) +
      scale_color_manual(values = c(MDKE_Color)) +  labs(x = "", y = str_crit$criterion2) + th

    return(g)

  }

  else if(criteria_degree == "k2de") {

    req_frame <- filter(get_data, Studiengang == "MDigiEng")

    #Dynamic Labels for plots
    req_frame$variety <- as.factor(req_frame$variety)
    sem_labels <- help_functions$make_plot_label(levels(req_frame$variety))

    count_vals <- req_frame %>% group_by(variety) %>% summarise(count_vals = n()) %>%        select(count_vals)

    label_vals = c()

    for (i in 1:length(levels(req_frame$variety))) {

      label_vals <- append(label_vals,
                           sprintf("Kohorte %s [%s]",sem_labels[i],count_vals[i,1]))

    }

    g <- ggplot(req_frame, aes(x=variety, y=note, fill=Studiengang)) +
      geom_boxplot(show.legend = F, varwidth = T,outlier.colour=DE_Color, outlier.shape=16, outlier.size=1, notch=FALSE, lwd=0.1, fatten= 4) +
      scale_fill_manual(values=c(DE_Color)) + scale_y_continuous(labels = percent, breaks=c(0, 0.25, 0.5, 0.75, 1), limits=c(0, 1)) +
      scale_x_discrete(limits=levels(req_frame$variety), labels= label_vals ) +
      scale_color_manual(values = c(DE_Color)) +  labs(x = "", y = str_crit$criterion2) + th

    return(g)

  }

  else {

    req_frame <- filter(get_data, Studiengang == "MDigiEng" | Studiengang ==       "MDKE") #Overall comparison boxplots between two degrees

    req_frame$Studiengang <- as.factor(req_frame$Studiengang)

    count_vals <- req_frame %>% group_by(Studiengang) %>% summarise(count_vals = n())      %>% select(count_vals)

    label_vals = c()

    for (i in 1:length(levels(req_frame$Studiengang))) {

      label_vals <- append(label_vals, sprintf("%s [%s]",levels(req_frame$Studiengang                                                                  )[i],count_vals[i,1]))

    }

    g <- ggplot(req_frame, aes(x=Studiengang, y=note, fill=Studiengang)) +
      geom_boxplot(show.legend = F, varwidth = T,
                   outlier.colour = "Black", outlier.shape=16, outlier.size=1,
                   lwd=0.1, fatten= 4) + scale_fill_manual(values=c(DE_Color, MDKE_Color)) + theme_bw() + scale_y_continuous(labels = percent, breaks=c(0, 0.25, 0.5, 0.75, 1), limits=c(0, 1)) + scale_x_discrete(limits=levels(req_frame$Studiengang),
                                                                                                                                                                                                                           labels=label_vals) +scale_color_manual(values = c(DE_Color, MDKE_Color)) + labs(x = "", y = str_crit$criterion2) + th

    return(g)

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

    sem_labels <- help_functions$make_plot_label(levels(get_data$LV.Datum.Semester))
    get_data <- get_data %>% arrange(LV.Datum.Semester)

    label_vals = c()

    for (i in 1:length(levels(get_data$LV.Datum.Semester))) {

      label_vals <- append(label_vals,sprintf("%s [%s]",sem_labels[i],get_data$n[i]))
    }

    g <- ggplot(get_data, aes(x=LV.Datum.Semester, y=n, fill = MDKE_Color)) + geom_bar(stat      = "identity", show.legend = F) + scale_fill_manual(values=c(MDKE_Color)) +
      theme_bw() + scale_color_manual(values = c(MDKE_Color)) + scale_x_discrete(limits =     levels(get_data$LV.Datum.Semester), labels = label_vals) +
      labs(x = "", y = str_crit$criterion2) + ylim(0,max(get_data$n)+50) + th

    return(g)

  }

  else if (criteria_degree == "k2deabsol"){

    get_data <- sec_cohort_analysis$criteria_plotting_absol("k2absolDE")
    get_data$LV.Datum.Semester <- as.factor(get_data$LV.Datum.Semester)

    sv <- levels(get_data$LV.Datum.Semester)

    #Reset level for properly setting Year-semester (SS16, WS16-17,....)
    get_data$LV.Datum.Semester <- factor(get_data$LV.Datum.Semester, levels =
                                           levels(get_data$LV.Datum.Semester)[order                                                                      (substr(sv,3,5))])

    sem_labels <- help_functions$make_plot_label(levels(get_data$LV.Datum.Semester))
    get_data <- get_data %>% arrange(LV.Datum.Semester)

    label_vals = c()

    for (i in 1:length(levels(get_data$LV.Datum.Semester))) {

      label_vals <- append(label_vals,sprintf("%s [%s]",sem_labels[i],get_data$n[i]))
    }

    g <- ggplot(get_data, aes(x=LV.Datum.Semester, y=n, fill = DE_Color)) + geom_bar(stat      = "identity", show.legend = F) + scale_fill_manual(values=c(DE_Color)) +
      theme_bw() + scale_color_manual(values = c(DE_Color)) + scale_x_discrete(limits =     levels(get_data$LV.Datum.Semester), labels = label_vals) +
      labs(x = "", y = str_crit$criterion2) + ylim(0,max(get_data$n)+50) + th

    return(g)

  }

  else{


    get_data <- sec_cohort_analysis$criteria_plotting_absol("k2absolboth")

    get_data$Studiengang <- as.factor(get_data$Studiengang)

    count_vals <- get_data %>% group_by(Studiengang) %>% summarise(count_vals = sum(n))      %>% select(count_vals)

    label_vals = c()

    for (i in 1:length(levels(get_data$Studiengang))) {

      label_vals <- append(label_vals, sprintf("%s [%s]",levels(get_data$Studiengang                                                                  )[i],count_vals[i,1]))

    }

    g <- get_data %>% group_by(Studiengang) %>% summarise(count_vals = sum(n)) %>%
      ggplot(., aes(x=Studiengang, y=count_vals, fill=Studiengang)) + geom_bar(stat = "identity", show.legend = F) + scale_fill_manual(values=c(DE_Color, MDKE_Color)) + theme_bw()  + ylim(0, max(count_vals)+100) + scale_x_discrete(limits=levels(get_data$Studiengang),labels=label_vals) + scale_color_manual(values = c(DE_Color, MDKE_Color)) +     labs(x = "", y = "Anzahl abgelegter PrC<fungen")

    return(g)

  }

}
