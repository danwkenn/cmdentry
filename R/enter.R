#' Enter a character string
#'
#' @param prompt Message to guide user input
#' @export
#' @examples
#' fun <- function() {
#'   ANSWER <- enter_character("Are you a satisfied R user? ")
#'   ## a better version would check the answer less cursorily, and
#'   ## perhaps re-prompt
#'   if (substr(ANSWER, 1, 1) == "n")
#'     cat("This is impossible.  YOU LIED!\n")
#'   else
#'     cat("I knew it.\n")
#' }
#' if(interactive()) fun()
enter_character <- function(prompt = "Enter characters:"){
  if(is.null(prompt)){prompt = "Enter characters:"}
  readLine2(prompt)
}

#' Enter an integer
#' @param prompt Message to guide user input
#' @param coerce Convert to integer type?
#' @export
#' @examples
#' fun <- function() {
#'   ANSWER <- enter_integer("How many times have you thought about just how satisfying R is? ")
#'   ## a better version would check the answer less cursorily, and
#'   ## perhaps re-prompt
#'   if (ANSWER == 0)
#'     cat("This is impossible.  YOU LIED!\n")
#'   else
#'     cat("I knew it.\n")
#' }
#' if(interactive()) fun()
enter_integer <- function(prompt = "Enter an integer value:",
                          coerce = TRUE){
  if(is.null(prompt)){prompt = "Enter an integer value:"}
  COMPLETE <- FALSE
  while(!COMPLETE){
    user_response <- readLine2(message = prompt)
    if(grepl(user_response,pattern = "^\\d+$")){
      COMPLETE <- TRUE
      if(coerce){
        return(as.integer(user_response))
      }else{
        return(user_response)
      }
    }
    message("Must be a number.")
  }

}

#' Enter a date
#' @param prompt Message to guide user input
#' @param coerce Convert to Date type?
#' @examples
#' fun <- function() {
#'   ANSWER <- enter_date("When was the first time you felt satisfied by R? ")
#'   ## a better version would check the answer less cursorily, and
#'   ## perhaps re-prompt
#'   if (ANSWER < "2000-02-29")
#'     cat("This is impossible.  YOU LIED!\n")
#'   else
#'     cat("I knew it.\n")
#' }
#' if(interactive()) fun()
enter_date <- function(prompt = "Enter a date in the form DD/MM/YYYY",
                       coerce = TRUE){

  if(is.null(prompt)){prompt = "Enter a date in the form DD/MM/YYYY"}

  COMPLETE <- FALSE
  while(!COMPLETE){
    user_response <- readLine2(message = prompt)
    if(grepl(user_response,pattern = "^\\d{1,2}/\\d{1,2}/\\d{4}")){
      COMPLETE <- TRUE
      if(coerce){
        return(lubridate::dmy(user_response))
      }else{
        return(user_response)
      }
    }
    message("Must be of the form DD/MM/YYYY.")
  }
}

#' Enter a floating-point (double precision) number:
#' @param prompt Message to guide user input
#' @param coerce Convert to Date type?
#' @export
#' @examples
#' fun <- function() {
#'   ANSWER <- enter_real("On a scale of 0 to 1, how satisfied with 1 are you (fractions allowed)?")
#'   ## a better version would check the answer less cursorily, and
#'   ## perhaps re-prompt
#'   if (ANSWER < 1)
#'     cat("This is impossible.  YOU LIED!\n")
#'   else
#'     cat("I knew it.\n")
#' }
#' if(interactive()) fun()
enter_real <- function(prompt = "Enter number in a non-ambiguous form:",
                       coerce = TRUE){

  if(is.null(prompt)){prompt = "Enter number in a non-ambiguous form:"}

  COMPLETE <- FALSE
  while(!COMPLETE){
    user_response <- readLine2(message = prompt)
    if(user_response %in% c("NA","NA_real_","NA_integer_")){
      return(NA)
    }
    if(!is.na(as.double(user_response))){
      COMPLETE <- TRUE
      if(coerce){
        return(as.double(user_response))
      }else{
        return(user_response)
      }
    }
    message("Must be a number in a non-ambiguous format.")
  }
}

#' Enter a date-time value:
#' @param prompt Message to guide user input
#' @param coerce Convert to Date-time type?
#' @export
#' @examples
#' fun <- function() {
#'   ANSWER <- enter_datetime("When exactly did you become fully satisfied with R?")
#'   ## a better version would check the answer less cursorily, and
#'   ## perhaps re-prompt
#'   if (ANSWER < "2000-02-29 00:00:00 GMT")
#'     cat("This is impossible.  YOU LIED!\n")
#'   else
#'     cat("I knew it.\n")
#' }
#' if(interactive()) fun()
enter_datetime <- function(prompt = "Enter date-time in the form of DD/MM/YYYY HH:MM:SS",
                           coerce = TRUE){

  if(is.null(prompt)){prompt = "Enter date-time in the form of DD/MM/YYYY HH:MM:SS"}

  COMPLETE <- FALSE
  while(!COMPLETE){
    user_response <- readLine2(message = prompt)
    if(grepl(user_response,pattern = "^\\d{1,2}/\\d{1,2}/\\d{4} \\d{1,2}:\\d{1,2}:\\d{1,2}")){
      COMPLETE <- TRUE
      if(coerce){
        return((lubridate::dmy_hms(user_response)))
      }else{
        return(user_response)
      }
    }
    message("Must be of the form DD/MM/YYYY HH:MM:SS.")
  }

}

#' Enter a factor value:
#' @param prompt Message to guide user input
#' @param levels Possible factor levels that can be entered.
#' @param print_list Print the options/levels as a list?
#' @param allow_enter If there is only a single value, allow the user to press enter to select that value.
#' @param coerce Convert to factor?
#' @export
#' @examples
#' fun <- function() {
#'   ANSWER <- enter_factor("How satisfied with R are you?",
#'   levels = c("Extremely", "Quite a lot", "Very","Surprisingly"))
#'   cat("I knew it.\n")
#' }
#' if(interactive()) fun()
enter_factor <- function(prompt = "Select an option from the list",
                         levels,
                         print_list = TRUE,
                         allow_enter = TRUE,
                         coerce = TRUE){

  if(is.null(prompt)){prompt = "Select an option from the list"}

  if(allow_enter & (length(levels)==1)){
    return(levels)
  }
  COMPLETE <- FALSE
  while(!COMPLETE){
    if(print_list){
      cat("Options:\n")
      cat(paste0("- ", levels, collapse = "\n"),"\n")
    }
    user_response <- readLine2(prompt)
    if(user_response %in% c("NA")){
      user_response <- NA
      COMPLETE <- TRUE
    }
    if(user_response %in% levels){
      COMPLETE = TRUE
    }else{
      message("Response must be from the list")
    }
  }
  if(coerce){
    factor(user_response, levels = levels)
  }else{
    user_response
  }
}


#' Enter a TRUE/FALSE value:
#' @param prompt Message to guide user input
#' @param coerce Coerce to boolean value?
#' @export
#' @examples
#' fun <- function() {
#'   ANSWER <- enter_boolean("True or False: \"I am satisfed with R.\"")
#'   ## a better version would check the answer less cursorily, and
#'   ## perhaps re-prompt
#'   if (!ANSWER)
#'     cat("This is impossible.  YOU LIED!\n")
#'   else
#'     cat("I knew it.\n")
#' }
#' if(interactive()) fun()
enter_boolean <- function(prompt = "Enter TRUE/FALSE or T/F",
                          coerce = TRUE){

  if(is.null(prompt)){prompt = "Enter TRUE/FALSE or T/F"}

  COMPLETE <- FALSE
  while(!COMPLETE){

    user_response <- readLine2(prompt)
    if(user_response %in% c("NA")){
      return(NA)
    }
    if(grepl(x = user_response, pattern = '^(T)|(F)|(TRUE)|(FALSE)$')){
      COMPLETE = TRUE
    }else{
      message("Response must be TRUE/FALSE or T/F")
    }
  }
  if(coerce){
    as.logical(user_response)
  }else{
    user_response
  }
}

#' Enter a value:
#' @param prompt Message to guide user input
#' @param template atomic vector to guess the vector type.
#' @param print_list If a factor, print the options/levels as a list?
#' @param allow_enter If a factor and there is only a single value, allow the user to press enter to select that value.
#' @param coerce Coerce to output format?
#' @export
#' @examples
#' fun <- function() {
#'  ANSWER <- enter_value("True or False: \"I am satisfed with R.\"",template = TRUE)
#'  ## a better version would check the answer less cursorily, and
#'  ## perhaps re-prompt
#'  if (!ANSWER)
#'    cat("This is impossible.  YOU LIED!\n")
#'  else
#'    cat("I knew it.\n")
#' }
#' if(interactive()) fun()
enter_value <- function(prompt = NULL,template, print_list = TRUE,allow_enter = FALSE,coerce = TRUE){

  if(class(template) == "factor"){
    return(enter_factor(prompt = prompt,
                        levels = levels(template),
                        print_list = print_list,
                        allow_enter = allow_enter,
                        coerce = coerce))
  }

  if(class(template) == "integer"){
    return(enter_integer(prompt = prompt,
                         coerce = coerce))
  }

  if(class(template) == "Date"){
    return(enter_date(prompt,
                      coerce = coerce))
  }

  if(class(template) == "numeric"){
    return(enter_real(prompt,
                      coerce = coerce))
  }

  if(class(template) == "logical"){
    return(enter_boolean(prompt,
                         coerce = coerce))
  }
  enter_character(prompt)
}

#' Enter a value:
#' @param data Data-set for values list.
#' @param auto_values Value here is used instead of prompting for user input.
#' @export
#' @examples
#' fun <- function(){
#' enter_multiple(data = iris, auto_values = c(Sepal.Length = 10.5))
#' }
#' if(interactive()) fun()
enter_multiple <- function(data, auto_values = NULL){

  values <- list()
  column_spec <- data
  for(i in names(column_spec)){
    if(i %in% names(auto_values)){
      values[[i]] <- auto_values[[i]]
    }else{
      values[[i]] <- cmdentry::enter_value(
        prompt = paste0('Enter value of type:\"',class(column_spec[[i]]),
                        '\" for column: \"',i,'\".'),template = column_spec[[i]])
    }
  }

  values
}
