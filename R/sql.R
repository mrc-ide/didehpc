storage_driver_psql_create <- function(path, id, args) {
  use_pool <- TRUE
  host <- "fi--didex1-vm"
  port <- if (use_pool) 6432 else 5432
  ## TODO: use actual usernames here so that I can easily work out who
  ## is using what?
  user <- "didehpc" # TODO: may change
  pass <- "didehpc" # TODO: encrypt this?
  dbname <- "didehpc"   # this is OK
  if (use_pool) {
    loadNamespace("RPostgreSQL")
    oo <- options(warnPartialMatchArgs = FALSE)
    if (isTRUE(oo$warnPartialMatchArgs)) {
      message("Disabling 'warnPartialMatchArgs' in order to use RPostgreSQL")
    }
    driver <- RPostgreSQL::PostgreSQL()
  } else {
    loadNamespace("RPostgres")
    driver <- RPostgres::Postgres()
  }
  con <- DBI::dbConnect(driver, host = host, port = 5432,
                        user = user, password = pass, dbname = dbname)
  tbl_data <- sprintf("context_%s_data", id)
  tbl_keys <- sprintf("context_%s_keys", id)
  storr::storr_dbi(con = con, tbl_data = tbl_data, tbl_keys = tbl_keys,
                   binary = FALSE)
}
environment(storage_driver_psql_create) <- baseenv()

storage_driver_psql <- function() {
  context:::storage_driver("postgres",
                           storage_driver_psql_create,
                           "RPostgres")
}
