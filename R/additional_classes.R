#' Enter value with compliance rule.
#' @param prompt Message to guide user input
#' @param compliance Function which returns a boolean vector.
#' @param fail_message Message indicating that the input was non-compliant.
#' @param coerce Coerce to output format?
#' @param coerce_function Function to coerce to desired output format.
#' @export
#' @examples
#' fun <- function(){
#'   enter_compliant_value(prompt = "Enter time in 24 hour format (HH:MM):",
#'                         compliance = function(x){grepl(pattern = "(0|1|2)\\d:[0-5]\\d", x)},
#'                         fail_message = "Input is an invalid time or format")
#' }
#' if(interactive()) fun()
enter_compliant_value <- function(prompt = "Enter compliant value",
                                  compliance = function(x){TRUE},
                                  fail_message = "Input does not comply.",
                                  coerce = FALSE,
                                  coerce_function = function(x){x}){

  if(is.null(prompt)){prompt = "Enter compliant value"}
  COMPLETE <- FALSE
  while(!COMPLETE){
    user_response <- readLine2(message = prompt)
    if(compliance(user_response)){
      COMPLETE <- TRUE

      if(coerce){
        return(coerce_function(user_response))
      }

      return(user_response)
    }
    message(fail_message)
  }
}
