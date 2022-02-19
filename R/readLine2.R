#' Version of readlines which works with interactive an command-line modes.
#' @param message Message prompt for the user.
#' @export
readLine2 <- function(message){
  message(message)
  if(interactive()){
    readline("")
  }else{
    readLines(con = "stdin",n = 1)[[1]]
  }
}
