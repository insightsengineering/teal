# this function will suppress the logger when running tests
# in order to suppress logs for a single test add this function
# call within the test_that function. To suppress logs for an entire
# test file, call this function at the start of the file
suppress_logs <- function() {
  old_log_appenders <- lapply(logger::log_namespaces(), function(ns) logger::log_appender(namespace = ns))
  logger::log_appender(logger::appender_file(nullfile()), namespace = logger::log_namespaces())
  withr::defer_parent(mapply(logger::log_appender, old_log_appenders, logger::log_namespaces()))
}
