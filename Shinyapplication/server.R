# The back end code of QualimanApp
# Please make sure to follow PEP8 style guidelines
# Also, don't forget to document your functions and describe your commits!

# If required for some systems (Currently not required!)
#Sys.setlocale("LC_CTYPE", "en_US.UTF-8")


box::use(ggplot2[coord_cartesian], shiny[...],magrittr[...])
box::use(shinymanager[secure_server, check_credentials])
box::use(DT[renderDataTable])

#options(box.path = "./")
box::use(./sec_cohort_analysis)
box::use(./plot_renderings)
box::use(./db_controller)
box::use(./plot_renders_data)



server <- function(input, output, session) {

  # login logic: call the server part, check_credentials returns a function to
  # authenticate users
  res_auth = secure_server(
    check_credentials = db_controller$check_hashed_credentials
  )

  output$auth_output = renderPrint({
    reactiveValuesToList(res_auth)
  })

  # Define the logon details with a reactive variable
  auth_output <- reactive({
    reactiveValuesToList(res_auth)
  })

  output$user_role = renderText(paste("You are logged in as",
                                      expr=auth_output()$role))
  output$role = reactive({
    auth_output()$role
  })

  # All output variables that need to be transferred to UI should have
  # suspendWhenHidden = FALSE:
  outputOptions(output, "role", suspendWhenHidden = FALSE)

  #Generate users table output for admins
  conn = db_controller$create_connection()
  app_users = db_controller$load_data(conn, 'vw_app_users')
  db_controller$disconnect_conn(conn)
  output$users_table = renderDataTable({app_users},
    options = list(searching = TRUE, pageLength = 10,
                   lengthMenu = c(5, 10, 15, 20), scrollX = T))

  #-----------------------------------------------
  #' Observe Event has been added which is dependent on the input of the
  #' UI variable. For our case, The result changes based on the tab clicked
  #' by the user. The thumb rule of its working is as follows
  #' 1. The output of observe should match the object output of UI elements
  #' eg. dataTableOutput("tabMDKE"). Which is taken care using paste0 command
  #' here, fixing the first word and joining it with the value of tabID. The
  #' ID of tabs are taken as an input to functions which renders plots, downloads
  #' data etc.
  #' This makes a code bit slow (TODO: Look for cached plots). But it makes code
  #' extremely functional without copy pasting and defining new variables individually

  #Overall Statistical table
  observe({
    output[[paste0("tab", input$tables)]] <- renderDataTable(server=FALSE,
      sec_cohort_analysis$statstable(input$tables), extensions = 'Buttons',
      options = list(dom = 'Bfrtip', searching = FALSE, pageLength = 10,
                     lengthMenu = c(5, 10, 15, 20),scrollX = T,
                      buttons = list(
                        list(extend = 'csv', filename = paste("Overall-stats", input$tables,Sys.Date(),sep = '_')))))
    })


  #k1

  observe({
   output[[paste0("plot", input$plotsk1)]] <-
     renderPlot(plot_renderings$k1_criteria(input$plotsk1))
   })

  # Separate observe event for downloading the data. TODO: Figure out a way to
  # dynamically name the downloaded data

  observe({
    output[[paste0("data", input$plotsk1)]] <-
      downloadHandler(filename = function(){
        paste0(input$plotsk1,'_',Sys.Date(),".csv", sep='')},
        content = function(file){
          plot_renders_data$k1_criteria(input$plotsk1) %>%
            write.csv(file, row.names = FALSE)
        })
    })

  #k2

  observe({
    output[[paste0("plot", input$plotsk2)]] <-
      renderPlot(plot_renderings$k2_criteria(input$plotsk2))
  })

  observe({
    output[[paste0("data", input$plotsk2)]] <-
      downloadHandler(filename = function(){
        paste0(input$plotsk2,'_',Sys.Date(),".csv", sep='')},
        content = function(file){
          plot_renders_data$k2_criteria(input$plotsk2) %>%
            write.csv(file, row.names = FALSE)
        })
  })

  #' In UI, k2 plots are binded under one tabset. But the rendering functions are
  #' different, which is the reason we have to create separate observe events. Also
  #' keep in mind that, output$name, should be different as well when different
  #' observe event is created, else it will just point to the last created event and
  #' forget the others. This also required changing element outputs in UI respectively.

  #k2 Absolute

  observe({
    output[[paste0("plotabsol", input$plotsk2)]] <-
      renderPlot(plot_renderings$k2_absol_criteria(input$plotsk2))
  })

  observe({
    output[[paste0("dataabsol", input$plotsk2)]] <-
      downloadHandler(filename = function(){
        paste0(input$plotsk2,'_',Sys.Date(),".csv", sep='')},
        content = function(file){
          plot_renders_data$k2_absol_criteria(input$plotsk2) %>%
            write.csv(file, row.names = FALSE)
        })
  })




}



#---------------------------------------------------------------------------------
#Extra code (Just for reference in future)

#' Event which dynamically changes plot region based on selected area (Not very efficient)
#' Currently not required

# observeEvent(input$plot1_dblclick, {
#  brush <- input$plot1_brush
#  if (!is.null(brush)) {
#    ranges$x <- c(brush$xmin, brush$xmax)
#    ranges$y <- c(brush$ymin, brush$ymax)

# } else {
#    ranges$x <- NULL
#    ranges$y <- NULL
#  } })

#output$data_K1DKEplot <- downloadHandler(filename = function() {
#paste0("k1_dke",Sys.Date(),".csv", sep='')
# },
#content = function(file) {
# plot_renders_data$k1_criteria("dke")  %>% write.csv(file, row.names = FALSE)
#  }
# )

#For interactive select, and zoom into plots
#ranges <- reactiveValues(x = NULL, y = NULL)


#output$K1DEplot = renderPlot(plot_renderings$k1_criteria("de"))

#output$K2DKEplot = renderPlot(plot_renderings$k2_criteria("dke"))

#output$K2DEplot = renderPlot(plot_renderings$k2_criteria("de"))

#output$K2absolDKEplot = renderPlot(plot_renderings$k2_absol_criteria("k2absolDKE"))

#output$K2absolDEplot = renderPlot(plot_renderings$k2_absol_criteria("k2absolDE"))

#output$K2DKEDEplot = renderPlot(plot_renderings$k2_criteria("dkede"))

#output$K2DKEDEabsolplot = renderPlot(plot_renderings$k2_absol_criteria("k2absolboth"))

#output$K1DKEplot = renderPlot(plot_renderings$k1_criteria("dke"))
# + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE))




