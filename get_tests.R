get_tests <- function(connection){
  query <- paste0("SELECT *
                   FROM ", connection@info$dbname, ".INFORMATION_SCHEMA.ROUTINES
                   WHERE ROUTINE_TYPE = 'PROCEDURE' and SPECIFIC_NAME like 'test%';")

  odbc::dbGetQuery(conn = connection,
                   statement = query) %>% dplyr::select(ROUTINE_SCHEMA,
                                                        ROUTINE_NAME,
                                                        ROUTINE_DEFINITION,
                                                        CREATED)
}

exec_tests_all <- function(connection){
  odbc::dbGetQuery(conn = connection, statement = "EXEC tSQLt.RunAll;")
  odbc::dbGetQuery(conn = connection, statement = "SELECT * FROM tSQLt.TestResult")
}

exec_test <- function(connection, test_class, test_name){
  odbc::dbGetQuery(conn = connection,
                   statement = paste0("EXEC tSQLt.Run '",test_class, ".[", test_name, "]';"))
  odbc::dbGetQuery(conn = connection, statement = "SELECT * FROM tSQLt.TestResult")%>%select(Class, TestCase, Result, Msg)
}

