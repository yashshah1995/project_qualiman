
library(shiny)
library(DT)
library(data.table)
library(ggplot2)
library(dplyr)
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
#-------------------------Initialize Certain variables------------------------- 

# ACHTUNG: Bis jetzt wurden die Kohorten SoSe 15 - WiSe 19 erfasst (UA+Leistungsdate)


source("connect_preprocess.R")
runmainfunction()


source("cohort_analysis.R")

#de_tab <- DE_stat_table()
#de_tab <- as.data.frame.matrix(de_tab)

#dke_tab <- DKE_stat_table()
#dke_tab <- as.data.frame.matrix(dke_tab)

get_ploting_data <- plotting_df()

server <- function(input, output) {
  output$DEstattab = DT::renderDataTable({
    as.data.frame.matrix(DE_stat_table())
  },options = list(searching = FALSE,pageLength = 10,lengthMenu = c(5, 10, 15, 20),          scrollX = T))
  output$DKEstattab = DT::renderDataTable({
    as.data.frame.matrix(DKE_stat_table())
  },options = list(searching = FALSE,pageLength = 10,lengthMenu = c(5, 10, 15, 20),          scrollX = T))
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  output$DKEplot = renderPlot({
    
ggplot(dplyr::filter(get_ploting_data, Studiengang == "MDKE"), aes(x=variety, y=note, fill=Studiengang)) +  geom_boxplot(show.legend = F, varwidth = T,outlier.colour=MDKE_Color, outlier.shape=16, outlier.size=1, notch=FALSE, lwd=0.1, fatten= 4) +  scale_fill_manual(values=c(MDKE_Color)) +  theme_bw() +  scale_x_discrete(limits=c("2015 SoSe", "2015/2016 WiSe", "2016 SoSe", "2016/2017 WiSe", "2017 SoSe", 
                                      "2017/2018 WiSe", "2018 SoSe", "2018/2019 WiSe", "2019 SoSe"), 
                             labels=c("Kohorte SoSe 15 [17]", "Kohorte WiSe 15/16 [33]", "Kohorte SoSe 16 [22]", 
                                      "Kohorte WiSe 16/17 [33]", "Kohorte SoSe 17 [11]", "Kohorte WiSe 17/18 [23]", 
                                      "Kohorte SoSe 18 [22]", "Kohorte WiSe 18/19 [51]", "Kohorte SoSe 19 [45]") ) +  scale_color_manual(values = c(MDKE_Color)) +  labs(x = "", y = str_crit$criterion1) + theme(
  plot.title = element_text( size=7),
  axis.text.x =  element_text(size = 10, angle=90, margin=margin(t=7, r=-40, b=7, l=0)),
  axis.text.y =  element_text(size = 10),
  axis.title.y = element_text(face="bold", size=8),
)})

output$DEplot = renderPlot({ggplot(dplyr::filter(get_ploting_data, Studiengang == "MDigiEng" & !is.na(note) & !is.na(variety) & 
                             note != "" & variety != ""), aes(x=variety, y=note, fill=Studiengang)) +  geom_boxplot(show.legend = F, varwidth = T, 
                         outlier.colour=DE_Color, outlier.shape=16, outlier.size=1, notch=FALSE, 
                         lwd=0.1, fatten= 4) +  scale_fill_manual(values=c(DE_Color)) +  theme_bw() +  scale_x_discrete(limits=c("2015 SoSe", "2015/2016 WiSe", "2016 SoSe", "2016/2017 WiSe", "2017 SoSe", 
                                      "2017/2018 WiSe", "2018 SoSe", "2018/2019 WiSe", "2019 SoSe"), 
                             labels=c("Kohorte SoSe 15 [23]", "Kohorte WiSe 15/16 [13]", "Kohorte SoSe 16 [10]", 
                                      "Kohorte WiSe 16/17 [25]", "Kohorte SoSe 17 [31]", "Kohorte WiSe 17/18 [12]", 
                                      "Kohorte SoSe 18 [25]", "Kohorte WiSe 18/19 [48]", "Kohorte SoSe 19 [48]") ) +  scale_color_manual(values = c(DE_Color)) +  labs(x = "", y = str_crit$criterion1) + theme(
  plot.title = element_text( size=7),
  axis.text.x =  element_text(size = 10, angle=90, margin=margin(t=7, r=-40, b=7, l=0)),
  axis.text.y =  element_text(size = 10),
  axis.title.y = element_text(face="bold", size=8),
)
})


# When a double-click happens, check if there's a brush on the plot.
# If so, zoom to the brush bounds; if not, reset the zoom.
observeEvent(input$plot1_dblclick, {
  brush <- input$plot1_brush
  if (!is.null(brush)) {
    ranges$x <- c(brush$xmin, brush$xmax)
    ranges$y <- c(brush$ymin, brush$ymax)
    
  } else {
    ranges$x <- NULL
    ranges$y <- NULL
  }
})


}













