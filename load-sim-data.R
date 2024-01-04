# Before running this code, ensure that the sim data has been created and the
# postgres instance has been set up.
# Example deploy code for postgres is included in the deploy-postgres.sh file.

library(RODBC)

# Alter these to match your Postgres instance.
server_hostname <- 'hprs-postgres.lan'
server_driver <- '/opt/homebrew/Cellar/psqlodbc/16.00.0000/lib/psqlodbcw.so'
server_database_name <- 'postgres'
server_username <- 'hprs'
server_password <- 'hprs'
server_port <- '5432'

channel <-  RODBC::odbcDriverConnect(
  paste0(
    'driver=', server_driver, ';',
    'server=', server_hostname, ';',
    'database=', server_database_name, ';',
    'uid=', server_username, ';',
    'pwd=', server_password, ';',
    'port=', server_port, ';'
  )
)

do_data <- read.csv('sim_data.csv')

RODBC::sqlDrop(
  channel = channel, 
  sqtable = 'sim_data',
  errors = FALSE
)

RODBC::sqlSave(
  channel = channel, 
  dat = do_data, 
  tablename = 'sim_data',
  append = FALSE,
  rownames = 'id',
  addPK = TRUE
)