#' @title Query to postgres database
#' @description Connection to the postgres database, where inventory data is stored.
#' @param dbname Database name
#' @param host Host name
#' @param port Database connection port
#' @param user Username
#' @param password password
#' @param sql_postgres Query database (SQL)
#' @return data table with the results of the query
#' @export request_postgres
#' @examples
#' datos_postgres=request_postgres(dbname="paracou",host="localhost",port=5432,user="postgres",password="postgres",sql_postgres="SELECT DISTINCT * FROM  paracou.bd_plots_corrected WHERE dbh>9.6 ORDER BY idtree, censusyear;")
#' head(datos_postgres,10)

request_postgres<-function(dbname,host,port,user,password,sql_postgres){
  require(RPostgreSQL)
  require(data.table)
  # Save the Password to be able to delete it later
  pw <- {password}
  # Read the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  # Create the connection to the database
  con <- dbConnect(drv, dbname = dbname,
                   host = host, port = port,
                   user = user, password = pw)
  # Remove Password
  rm(pw)
  ############ End connection to postgress
  datos_postgres <- dbGetQuery(con,sql_postgres)
  # datos_postgres <- as.data.table(datos_postgres)
  datos_postgres <- as.data.table(datos_postgres)
}




