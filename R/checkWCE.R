#' Verify that the arguments passed to the WCE function are in correct format
#'
#' This function checks whether the arguments passed to the WCE function are in the correct format. If at least one argument is incorrectly specified, the function returns an error message specifying what needs to be fixed.
#'
#' @param data A data frame in the long (interval) format with one line per unit of time.
#' @param id The name of the variable in \code{data} that identifies subjects.
#' @param event The name of the variable in \code{data} that identifies the events (must be 0 or 1).
#' @param start The name of the variable in \code{data} that identifies the beginning of the interval.
#' @param stop The name of the variable in \code{data} that identifies the end of the interval.
#' @param expos The name of the variable in \code{data} that represents the exposure of interest.
#'
#' @details The arguments passed to \code{checkWCE} must be exactly those passed to \code{WCE}.
#'
#' @return \code{checkWCE} returns a message on the screen indicating whether the arguments are correctly specified or not.
#'
#' @examples
#' checkWCE(drugdata, id = "Id", event = "Event",  start = "Start", stop = "Stop", expos = "dose")
#' @importFrom stats na.omit
#' @export



checkWCE <- function(data, id, event, start, stop, expos){

  Error <- NULL
  if (is.data.frame(data) == F)  {
    Error <- "The data supplied is not in a data.frame format.\n"}
  if (missing(id)) {
    stop("Need to specify id variable, e.g. id = 'Id', in the arguments of the function.")}
  if (missing(event)) {
    stop("Need to specify event variable, e.g. event = 'Event', in the arguments of the function.")}
  if (missing(start)) {
    stop("Need to specify start variable, e.g. start = 'Start', in the arguments of the function.")}
  if (missing(stop)) {
    stop("Need to specify stop variable, e.g. stop = 'Stop', in the arguments of the function.")}
  if (missing(expos)) {
    stop("Need to specify exposure (expos) variable, e.g. expos = 'dose', in the arguments of the function.")}
  #if (is.null(covariates) == F & sum(covariates %in% names(data))!= length(covariates)) stop("ERROR: At least one covariate does not belong to the data set supplied.")

  if (sum(c(id,event,start,stop,expos) %in% names(data)) != 5){
    if (!(id %in% names(data))) {
      stop('ERROR: The variable passed to the argument "id" does not belong to the data set supplied.')}
    if (!(event %in% names(data))) {
      stop('ERROR: The variable passed to the argument "event" does not belong to the data set supplied.')}
    if (!(start %in% names(data))) {
      stop('ERROR: The variable passed to the argument "start" does not belong to the data set supplied.')}
    if (!(stop %in% names(data))) {
      stop('ERROR: The variable passed to the argument "stop" does not belong to the data set supplied.')}
    if (!(expos %in% names(data))) {
      stop('ERROR: The variable passed to the argument "expos" does not belong to the data set supplied.')}
  }

  data <- data[, c(id, event, start, stop, expos)]
  names(data)[names(data) == id] <- 'Id'
  names(data)[names(data) == event] <- 'Event'
  names(data)[names(data) == start] <- 'Start'
  names(data)[names(data) == stop] <- 'Stop'
  names(data)[names(data) == expos] <- 'dose'

  if (sum(data$Start == data$Stop) > 0)  {Error <- c(Error,"Start and stop values are equal for at least one individual.\n")}
  if (nrow(na.omit(data[,c("Id","Event","Start","Stop","dose")])) < nrow(data[,c("Id","Event","Start","Stop","dose")]))  {Error <- c(Error,"There are missing values in at least one of the id, event, start, stop, or expos columns.\n")}
  if (gap(data))  {Error <- c(Error,"There is at least one gap or an overlap for at least one individual in the start and stop values.\n")}
  if (!is.numeric(data[,c("Event")]) | !is.numeric(data[,c("dose")]) | !is.numeric(data[,c("Start")]) |!is.numeric(data[,c("Stop")]))   {Error <- c(Error,"At least one value of start, stop, event or expos is not numeric.\n")}
  if (length(unique(data[,c("Event")]))!=2 | (sum(unique(data[,c("Event")]) %in% c(0,1))!=2))  {Error <- c(Error,"Values supplied for events are not either 0 or 1.\n")}

  if (is.null(Error) == F) {Message <- Error} else {Message <- "Data are in the right format for WCE estimation.\n"}
  cat(Message)
}
