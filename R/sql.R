storage_driver_psql_create_pool <- function(path, id, args) {
  host <- "fi--didex1-vm"
  port <- 6432
  ## TODO: use actual usernames here so that I can easily work out who
  ## is using what?
  user <- "didehpc" # TODO: may change
  pass <- "didehpc" # TODO: encrypt this?
  dbname <- "didehpc"   # this is OK

  loadNamespace("RPostgreSQL")
  oo <- options(warnPartialMatchArgs = FALSE)
  if (isTRUE(oo$warnPartialMatchArgs)) {
    message("Disabling 'warnPartialMatchArgs' in order to use RPostgreSQL")
  }
  driver <- RPostgreSQL::PostgreSQL()
  con <- DBI::dbConnect(driver, host = host, port = port,
                        user = user, password = pass, dbname = dbname)
  tbl_data <- sprintf("context_%s_data", id)
  tbl_keys <- sprintf("context_%s_keys", id)
  storr::storr_dbi(con = con, tbl_data = tbl_data, tbl_keys = tbl_keys,
                   binary = FALSE)
}

storage_driver_psql_create_direct <- function(path, id, args) {
  host <- "fi--didex1-vm"
  port <- 5432
  ## TODO: use actual usernames here so that I can easily work out who
  ## is using what?
  user <- "didehpc" # TODO: may change
  pass <- "didehpc" # TODO: encrypt this?
  dbname <- "didehpc"   # this is OK

  loadNamespace("RPostgres")
  driver <- RPostgres::Postgres()

  con <- DBI::dbConnect(driver, host = host, port = port,
                        user = user, password = pass, dbname = dbname)
  tbl_data <- sprintf("context_%s_data", id)
  tbl_keys <- sprintf("context_%s_keys", id)
  storr::storr_dbi(con = con, tbl_data = tbl_data, tbl_keys = tbl_keys,
                   binary = FALSE)
}
environment(storage_driver_psql_create_pool) <- baseenv()
environment(storage_driver_psql_create_direct) <- baseenv()

storage_driver_psql <- function(use_pool = TRUE) {
  if (use_pool) {
    driver <- storage_driver_psql_create_pool
    package <- "RPostgreSQL"
  } else {
    driver <- storage_driver_psql_create_direct
    package <- "RPostgres"
  }
  context:::storage_driver("postgres", driver, package)
}
