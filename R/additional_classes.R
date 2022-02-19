#' Enter value with compliance rule.
#' @param prompt Message to guide user input
#' @param compliance Function which returns a boolean vector.
#' @param fail_message Message
#' @export
enter_compliant_value <- function(prompt = "Enter compliant value",
                                  compliance = function(x){TRUE},
                                  fail_message = "Input does not comply."){

  if(is.null(prompt)){prompt = "Enter compliant value"}
  COMPLETE <- FALSE
  while(!COMPLETE){
    user_response <- readLine2(message = prompt)
    if(compliance(user_response)){
      COMPLETE <- TRUE
      return(user_response)
    }
    message(fail_message)
  }


}
