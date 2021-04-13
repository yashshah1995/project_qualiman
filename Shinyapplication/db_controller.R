#' This program is used for connection functionalities to DB.

box::use(DBI[dbConnect, dbReadTable, dbDisconnect,
             dbSendQuery, dbBind, dbFetch, dbClearResult])

box::use(config[get])
box::use(sodium[password_store, password_verify])

#' @export
create_connection <- function(){

  # Get the isolated configuration for security reasons
  # For deployment purposes, we need to use the remote database via VPN
  config = get("db_local")
  # Use the DB connection configuration to connect securely
  #Edit config.yml (according to database config in your system)
  dbconn <- dbConnect(drv = RPostgres::Postgres(),
                        host = config$server,
                        user = config$uid,
                        dbname = config$db,
                        port = config$port,
                        password = config$pwd

  )

  return(dbconn)

}

#' @export
check_hashed_credentials <- function(user, password){

  # Instead of scooping all the users table, which is inefficient and insecure
  # We will send a parametrised query to the database using the credentials
  # This is the most secure way of avoiding SQL injection attacks
  # We have to do this on two steps because of the nature of hashing
  # and collisions:
  #   first see if the user is there, then
  #   check if their password's hash is valid

  con <- create_connection()

  param_sql = dbSendQuery(
    conn = con,
    # TODO: make a customised function to accept a parametrised query and
    # executes it in the DB module
    statement = "SELECT * FROM public.app_users WHERE username=$1"
  )
  dbBind(res = param_sql, params = list(user))
  qry_res = dbFetch(param_sql)
  # Release resources and close the DB connection
  dbClearResult(param_sql)
  dbDisconnect(con)

  # Now we need to check if the username's hashed password matches a record
  if(nrow(qry_res) == 1 & nchar(password) > 0) # There should be one and only one match
  {
    # Check the validity of the secure hash
    if(password_verify(hash = qry_res$user_password, password = password))
    {
      ret = list(result = TRUE,
                 user_info = list(user = user, role=qry_res$user_role))
    }
    else
    {
      ret = list(result=FALSE)
    }
  }
  else
  {
    ret = list(result = FALSE)
  }

  return(ret)
}

# Load the required data using PostgreSQL connection

#' @export
load_data <- function(db, view_name) {
  frame_name <- dbReadTable(db, view_name)
  return(frame_name)
}

#' @export
disconnect_conn <- function(conn_name){
  dbDisconnect(conn_name)
}








