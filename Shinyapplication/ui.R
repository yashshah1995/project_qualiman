## ui.R ##

#This UI functions of the application. It uses Shiny Dashboard.

box::use(shinymanager[secure_app],shinydashboard[...],shinythemes[shinytheme])
box::use(shinyWidgets[actionBttn])
box::use(DT[dataTableOutput])

#' Edit the following function to add labels of new plots and statistics
#' to the sidebar menu.

sidebar <- dashboardSidebar(
  width=280,
  sidebarMenu(id = "sidebarmenu",
              menuItem("Start", tabName = "User-id", icon=icon("flag")),
              menuItem("Kohort statistics", tabName = "Kohort_statistics",
                       icon=icon("chart-bar"),
                       menuSubItem("Overall statistics",
                                   tabName = "Overall_statistics"),
                       menuSubItem("K1 criteria",
                                   tabName = "K1_criteria"),
                       menuSubItem("K2 criteria",
                                   tabName = "K2_criteria")
              )
  ),
  sidebarMenu(id = "admin_sidebarmenu",
              conditionalPanel(condition = "output.role === 1",
                               menuItem("User Management", tabName="user_mngr",
                                        icon=icon("users-cog"),
                                        badgeLabel="ADMIN", badgeColor="orange"
                               )
              )
  )
)

#' This following function is the main body. The directory structure for
#' reference is as follows:
#' -Body
#'     - TabItems
#'         - tabitem (tabsetPanel,
#'                    tabBox(useful for adding common boxes across pane))
#'             - tabPanel (Individual panel)
#'
#'  Additional functionality of dlbclick and brush to select and zoom into plots
#'  Connected with 'ggplot2::cartesian_corr()' function in server file

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "User-id",
            fluidPage( # Apply a new theme
              theme = shinytheme("flatly"),
              # Application title
              titlePanel("Welcome to Qualiman Web App")
            )),

    tabItem(tabName = "Overall_statistics",
            fluidRow(tabsetPanel(
              id = "tables",
              tabPanel("DKE",value = "MDKE",dataTableOutput("tabMDKE")),
              tabPanel("DE", value = "MDigiEng", dataTableOutput("tabMDigiEng"))),
            )),

    tabItem(tabName = "K1_criteria",
            fluidRow(tabsetPanel(
              id = "plotsk1",
              tabPanel("DKE", value = "k1dke", plotOutput("plotk1dke"),
                       downloadButton("datak1dke","Download")),
              tabPanel("DE",value = "k1de", plotOutput("plotk1de"),
                       downloadButton("datak1de","Download"))),
            )),

    tabItem(tabName = "K2_criteria",
            fluidRow(
              tabsetPanel(
                id = "plotsk2",
                tabPanel("DKE", value = "k2dke", plotOutput("plotk2dke"),
                         downloadButton("datak2dke","Download")),

                tabPanel("DE", value = "k2de", plotOutput("plotk2de"),
                         downloadButton("datak2de", "Download")),

                tabPanel("DKE(Absol)", value = "k2dkeabsol",
                         plotOutput("plotabsolk2dkeabsol"),
                         downloadButton("dataabsolk2dkeabsol", "Download")),

                tabPanel("DE(Absol)", value = "k2deabsol",
                         plotOutput("plotabsolk2deabsol"),
                         downloadButton("dataabsolk2deabsol", "Download")),

                tabPanel("combinedplot", value = "k2dkede",
                         plotOutput("plotk2dkede"),
                         downloadButton("datak2dkede", "Download")),

                tabPanel("combinedbar", value = "k2dkedeabsol",
                         plotOutput("plotabsolk2dkedeabsol"),
                         downloadButton("dataabsolk2dkedeabsol", "Download"))
              )
            )
    ),

    tabItem(tabName="user_mngr",
            titlePanel("User Management Component"),
            fluidPage(
              dataTableOutput(outputId = "users_table"),
              actionBttn("show", "Add user", style="simple", size="sm", color = "warning"),
              actionBttn("del", "Delete user", style="simple", size="sm", color = "warning")
              )
            )
    ),  # tabItems

    tags$footer(p("Version: 0.1.210430",style="font-size: 11px; text-align: end;"
    ),
    style = "
                position:absolute;
                bottom:0;
                width:100%;
                height:22px;   /* Height of the footer */
                color: black;
                padding: 4px;
                background-color: white;
                left: 0px;"
    )
  )

  ui <- dashboardPage(
    skin="black",
    dashboardHeader(title = "Qualiman Web App", titleWidth=280),
    sidebar,
    body
  )

  ui <- secure_app(ui, theme=shinytheme("slate"))
