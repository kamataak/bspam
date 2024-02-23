#' @import tryCatchLog
#' @import futile.logger
#'
log.initiating <- function() {

options(keep.source = TRUE) # source code file name and line number tracking
options("tryCatchLog.write.error.dump.file" = FALSE)
options("tryCatchLog.include.full.call.stack" = FALSE) # reduce the ouput for demo purposes
options("tryCatchLog.include.compact.call.stack" = FALSE) # reduce the ouput

# Set log level
flog.threshold(INFO) # TRACE, DEBUG, INFO, WARN, ERROR, FATAL

# Define logger, write to both console and file
flog.logger("orfrlog", INFO, appender=appender.tee('orfrlog.log'))

}
