gc()

library("RSQLite")
library("arrow")

# Global variables ---------------------------------------------------------
CWD <- getwd()
input_file <- 'database.sqlite'
db_dir <- '/db/'
data_dir <- '/data/'

# Connection to database ---------------------------------------------------
path <- paste(CWD, db_dir, input_file, sep='')
con <- dbConnect(drv=RSQLite::SQLite(), dbname=path)
print('Connected to Database')

# list all tables and exclude table 'sqlite_sequence'
dbtables <- dbListTables(conn=con)
dbtables <- dbtables[dbtables != "sqlite_sequence"]

tempDataFrames <- vector("list", length=length(dbtables))

# extract all tables from sqlite database and create list of data frames
# future improvements: improve efficiency of  SQL query
print('Extracting tables...')
for (i in seq(along=dbtables)) {
  tempDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", dbtables[[i]], "'", sep=""))
}

# assign names to tables in order to flatten list of dataframes 'tempDataFrames' into different global objects
names(tempDataFrames) <- c('country', 'league', 'match', 'player', 'player_stats', 'team', 'team_stats')
list2env(tempDataFrames, globalenv())

dbDisconnect(conn = con)
print('Disconnected from Database')

rm(con, db_dir, dbtables, i, input_file, path, tempDataFrames)

# export to csv
df_names <- c('country', 'league', 'match', 'player', 'player_stats', 'team', 'team_stats')
for (name in df_names) {
  if (exists(name)){
    df <- get(name)
    write_parquet(df, sink = paste0(CWD, data_dir, name, '.parquet'))
  }
  
}

rm(df_names, name)