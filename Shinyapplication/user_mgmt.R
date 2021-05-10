box::use(shiny[...])

#' Module for adding user; Takes in user-name, password and user-role
#' Also checks if user name isn't used before (duplicate)


#' @export
user_modal <- function(failed = FALSE, failed_vali = FALSE){
  modalDialog(
    tags$h2('Please enter your details'),
    textInput('usrname', 'User name'),
    if(failed)
      div(tags$b("Name already used", style = "color: red;")),

    passwordInput('pwd', 'Password'),
    passwordInput('pwd_vali', 'Please re-enter password'),

    if (failed_vali)
      div(tags$b("Passwords don't match", style = "color: red;")),

    selectInput('role', 'Type of role',
                c("Admin" = 1,
                  "Viewer" = 2)),
    footer=tagList(
      actionButton('submit', 'Submit'),
      modalButton('cancel')
    )
  )

}

#' Module for prompting password; Currently used in case
#' when trying to delete another admin


#' @export
delete_admin_modal <- function(failed = FALSE){
  modalDialog(
    tags$h2('Please enter your password'),
    passwordInput('pwd_a', 'Password'),
    if(failed)
      div(tags$b("Password not correct", style = "color: red;")),

    footer=tagList(
      actionButton('submit_a', 'Submit'),
      modalButton('cancel')
    )
  )
}

#' Delete user functionality
#' Admin can delete user without password
#' But admin can remove other admin with their password only
#' The logged admin cannot delete itself

#' @export
del_modal <- function(failed = FALSE){
  modalDialog(
    tags$h2('Input Username'),
    textInput('usrname_s', 'User name'),
    if(failed)
      div(tags$b("Username not found", style = "color: red;")),
    footer=tagList(
      actionButton('submit_del_selec', 'Submit'),
      modalButton('cancel')
    ))
}







