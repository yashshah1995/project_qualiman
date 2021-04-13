## ui.R ##
library(shinydashboard)
library(DT)

sidebar <- dashboardSidebar(
  sidebarMenu(id = "sidebarmenu",
    menuItem("Kohort_statistics", tabName = "Kohort_statistics",
             menuSubItem("stats",tabName = "stats"),
             menuSubItem("statplot", tabName = "statplot")
             
             )
    )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "stats",
  fluidRow(tabsetPanel(
                    id = "tables",
                    tabPanel("DKE",DT::dataTableOutput("DKEstattab"                                          )),
                    tabPanel("DE",DT::dataTableOutput("DEstattab"))),
                    )
    ),
  
  tabItem(tabName = "statplot",
          fluidRow(tabsetPanel(
            id = "plots",
            tabPanel("DKEplotstat" ,plotOutput("DKEplot", dblclick = "plot1_dblclick",
                                               brush = brushOpts(
                                                 id = "plot1_brush",
                                                 resetOnNew = TRUE
                                                                 
            ))),
            tabPanel("DEplotstat", plotOutput("DEplot")))
            )
          )
  )
)



dashboardPage(
  dashboardHeader(title = "Qualiman"),
  sidebar,
  body
  
)
 