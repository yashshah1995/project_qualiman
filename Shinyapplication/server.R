# The back end code of QualimanApp
# Please make sure to follow PEP8 style guidelines
# Also, kindly don't forget to document your functions when they are "final",
# and describe your commits to GitHub

# If required for some systems (Currently not required!)
#Sys.setlocale("LC_CTYPE", "en_US.UTF-8")


box::use(ggplot2[coord_cartesian], shiny[...],magrittr[...])
box::use(shinymanager[secure_server, check_credentials])
box::use(DT[dt_render = renderDataTable])

#options(box.path = "./")
box::use(./sec_cohort_analysis)
box::use(./plot_renderings)
box::use(./db_controller)
box::use(./plot_renders_data)
box::use(./user_mgmt)


server <- function(input, output, session) {

  # login logic: call the server part, check_credentials returns a function to
  # authenticate users
  res_auth = secure_server(
    check_credentials = db_controller$check_hashed_credentials
  )

  # Define the logon details with a reactive variable
  auth_output <- reactive({
    reactiveValuesToList(res_auth)
  })

  output$role = reactive({
    auth_output()$role
  })


  #Generate users table output for admins
  conn = db_controller$create_connection()
  app_users = db_controller$load_data(conn, 'vw_app_users')
  db_controller$disconnect_conn(conn)
  output$users_table = dt_render({app_users},
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
    output[[paste0("tab", input$tables)]] <- dt_render(server=FALSE,
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

  # Separate observe event for downloading the data.

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

  # All output variables that need to be transferred to the UI should have
  # suspendWhenHidden = FALSE:
  outputOptions(output, "role", suspendWhenHidden = FALSE)

  #------------------------------------------------------------------------------

  #' User management system; Allows to add or delete new users with different level
  #' of access (user_mgmt.R module)

  # Show add-user modal when button is clicked.
  observeEvent(input$show, {
    showModal(user_mgmt$user_modal())
  })

  #' Checks for duplicate username, returns to previous model if True
  #' Else stores successfully
  #' only store the information if the user clicks submit
  observeEvent(input$submit,{
    dupli_user = db_controller$check_dupli_user(input$usrname)
    if (dupli_user$result)
    {
      showModal(user_mgmt$user_modal(failed = TRUE, FALSE))
    }
    else
    {


      if (input$pwd_vali != input$pwd)
      {
        showModal(user_mgmt$user_modal(FALSE, failed_vali = TRUE))
      }
      else
      {

        if (input$pwd_vali != input$pwd)
        {
          showModal(user_mgmt$user_modal(FALSE, failed_vali = TRUE))
        }
        else
        {
          removeModal()

          db_controller$add_user(input$usrname, input$pwd, input$role)

        }


      }


    }
  })
  # Show delete user modal when button is clicked.
  observeEvent(input$del, {
    showModal(user_mgmt$del_modal())
  })

  #' Delete Modal conditions; Checks if user exists, otherwise return
  #' Checks if username is currently logged in, if True, then stop
  #' Checks user-role; if Admin; Ask for password
  #' if viewer, delete successfully

  observeEvent(input$submit_del_selec,{
    usr_role <- 0
    usr_details <- db_controller$check_dupli_user(input$usrname_s)
    usr_exist <- usr_details$result
    usr_role <- usr_details$role
    if (usr_exist)
    {
      if (input$usrname_s == auth_output()$user)
      {
        removeModal()
        showModal(modalDialog(
          tags$h2('Cannot delete logged user'),
          footer = tagList(
            modalButton('OK'))
        ))
      }
      else
      {
        if (usr_role == 1)
        {
          removeModal()
          showModal(user_mgmt$delete_admin_modal())
        }
        else
        {
          removeModal()
          db_controller$del_user(input$usrname_s)
          usr_role <- 0
        }
      }
    }
    else
    {
      showModal(user_mgmt$del_modal(failed = TRUE))
    }
  })

  #' Checks for password prompt when delete user is admin

  observeEvent(input$submit_a,{
    dupli_user = db_controller$check_hashed_credentials(auth_output()$user, input$pwd_a)
    if (dupli_user$result)
    {
      removeModal()
      db_controller$del_user(input$usrname_s)
    }
    else
    {
      showModal(user_mgmt$delete_admin_modal(failed = TRUE))
    }
  })
}
