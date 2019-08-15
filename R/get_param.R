#' Extract dataframe of all parameters from ACPy database
#'
#' Extract all parameters from database file with xml file
#'
#' @param dbFile database file; filepath to database file where calibration results are stored
#' @param acpyXML XML file; filepath to xml file used for ACPy calibration
#' @param run numeric; Run number to extract parameters from. If NULL extracts runs from the whole database. Defaults to NULL
#' @return data
#' @importfrom RSQLite dbConnect
#' @importfrom RSQLite dbGetQuery
#' @importfrom RSQLite dbDisconnect
#' @importfrom XML xmlParse
#' @importfrom XML xmlRoot
#' @importfrom XML xmlSApply
#' @importFrom tidyr separate
#' @export
get_param <- function(dbFile, acpyXML, run = NULL){
  xml = xmlParse(acpyXML)
  rootNode = xmlRoot(xml)
  data = xmlSApply(rootNode, function(x) xmlSApply(x, xmlAttrs))
  # db = data$transports
  params = data$parameters
  var.nam = c()
  for (p in params) {
    if (is.null(p)) {
      next
    }
    if (!is.na(p["dummy"])) {
      var.nam = append(var.nam, "dummy")
      next
    }
    var.nam = append(var.nam, p["variable"])
  }
  dbcon = dbConnect(dbDriver("SQLite"), dbname = dbFile)
  table = dbGetQuery(dbcon, "select * from results")
  dbDisconnect(dbcon)
  # pars <- data.frame(do.call('rbind', strsplit(as.character(table$parameters),';',fixed=TRUE)))
  pars <- separate(table, parameters, var.nam, sep = ';')
  pars[,c(var.nam)] <- sapply(pars[,c(var.nam)], as.numeric)
  # pars <- mutate(pars, function(x) as.numeric(as.character(x)))
  colnames(pars) <- var.nam
  pars$run <- table$run
  pars$id <- table$id
  pars$lnlikelihood <- table$lnlikelihood
  pars$time <- as.POSIXct(table$time, tz = 'UTC')
  pars <- pars[,c('id', 'run', 'time', var.nam, 'lnlikelihood')]
  if(!is.null(run)){
    pars <- pars[(pars$run == run),]
  }
  return(pars)
}
