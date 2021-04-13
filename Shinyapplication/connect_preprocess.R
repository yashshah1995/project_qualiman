

#Create a postgres connection

#' @export
create_connection <- function(){
  con <- DBI::dbConnect(RPostgres::Postgres(), 
                        host = "localhost",
                        user = "postgres",
                        dbname = "qualiman",
                        password = 'yash'
                        
  )
  
  return(con)
  
}

# Load the required data using Postgres connection

#' @export
load_data <- function(db, view_name) {
  
  frame_name <- DBI::dbReadTable(db, view_name)
  return(frame_name)
  
}










