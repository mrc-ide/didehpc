storage_driver_psql_create <- function(path, id, args) {
  host <- Sys.getenv("CONTEXT_PGHOST", "129.31.26.142")
  ## TODO: use actual usernames here so that I can easily work out who
  ## is using what?
  user <- "didehpc" # TODO: may change
  pass <- "didehpc" # TODO: encrypt this?
  db <- "didehpc"   # this is OK
  tbl_data <- sprintf("context_%s_data", id)
  tbl_keys <- sprintf("context_%s_keys", id)
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        host = host, user = user, password = pass, dbname = db)
  storr::storr_dbi(con = con, tbl_data = tbl_data, tbl_keys = tbl_keys,
                   binary = FALSE)
}
environment(storage_driver_psql_create) <- baseenv()

storage_driver_psql <- function() {
  context:::storage_driver("postgres",
                           storage_driver_psql_create,
                           "RPostgres")
}
