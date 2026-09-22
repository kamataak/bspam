#' @import tryCatchLog
#' @import futile.logger
#'
log.initiating <- function() {

# Configure the package logger without changing user options
# or writing log files to the working directory.
flog.logger("orfrlog", WARN, appender=appender.console())

}
