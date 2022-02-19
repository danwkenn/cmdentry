#' Enter a character
#' @param prompt Message to guide user input
#' @export
enter_character <- function(prompt = "Enter characters:"){
  if(is.null(prompt)){prompt = "Enter characters:"}
  readLine2(prompt)
}

#' Enter an integer
#' @param prompt Message to guide user input
#' @export
enter_integer <- function(prompt = "Enter an integer value:"){
  if(is.null(prompt)){prompt = "Enter an integer value:"}
  COMPLETE <- FALSE
  while(!COMPLETE){
    user_response <- readLine2(message = prompt)
    if(grepl(user_response,pattern = "^\\d+$")){
      COMPLETE <- TRUE
      return(user_response)
    }
    message("Must be a number.")
  }

}

#' Enter a date
#' @param prompt Message to guide user input
#' @export
enter_date <- function(prompt = "Enter a date in the form DD/MM/YYYY"){

  if(is.null(prompt)){prompt = "Enter a date in the form DD/MM/YYYY"}

  COMPLETE <- FALSE
  while(!COMPLETE){
    user_response <- readLine2(message = prompt)
    if(grepl(user_response,pattern = "^\\d{1,2}/\\d{1,2}/\\d{4}")){
      COMPLETE <- TRUE
      return((lubridate::dmy(user_response)))
    }
    message("Must be of the form DD/MM/YYYY.")
  }
}

#' Enter a time duration
#' @param prompt Message to guide user input
#' @export
enter_duration <- function(prompt = "Enter a duration of time in the form HH:MM:SS"){

  if(is.null(prompt)){prompt = "Enter a duration of time in the form HH:MM:SS"}

  COMPLETE <- FALSE
  while(!COMPLETE){
    user_response <- readLine2(message = prompt)
    if(user_response %in% c("NA","NA_real_","NA_integer_")){
      return(NA)
    }
    if(!is.na(lubridate::period(user_response,units = c('day','hour','minute','second')))){
      COMPLETE <- TRUE
    }else{
    message("Must be of a form recognised by period.")
    }
  }

  Period_c(as.character(lubridate::period(user_response)))
}

#' Enter a floating-point (double precision) number:
#' @param prompt Message to guide user input
#' @export
enter_real <- function(prompt = "Enter number in a non-ambiguous form:"){

  if(is.null(prompt)){prompt = "Enter number in a non-ambiguous form:"}

  COMPLETE <- FALSE
  while(!COMPLETE){
    user_response <- readLine2(message = prompt)
    if(user_response %in% c("NA","NA_real_","NA_integer_")){
      return(NA)
    }
    if(!is.na(as.double(user_response))){
      COMPLETE <- TRUE
      return(as.double(user_response))
    }
    message("Must be a number in a non-ambiguous format.")
  }

}

#' Enter a date-time value:
#' @param prompt Message to guide user input
#' @export
enter_datetime <- function(prompt = "Enter date-time in the form of DD/MM/YYYY HH:MM:SS"){

  if(is.null(prompt)){prompt = "Enter date-time in the form of DD/MM/YYYY HH:MM:SS"}

  COMPLETE <- FALSE
  while(!COMPLETE){
    user_response <- readLine2(message = prompt)
    if(grepl(user_response,pattern = "^\\d{1,2}/\\d{1,2}/\\d{4} \\d{1:2}:\\d{1,2}:\\d{1,2}")){
      COMPLETE <- TRUE
      return((lubridate::dmy_hms(user_response)))
    }
    message("Must be of the form DD/MM/YYYY HH:MM:SS.")
  }

}

#' Enter a factor value:
#' @param prompt Message to guide user input
#' @param levels Possible factor levels that can be entered.
#' @param print_list Print the options/levels as a list?
#' @param allow_enter If there is only a single value, allow the user to press enter to select that value.
#' @export
enter_factor <- function(prompt = "Select an option from the list",
                         levels,
                         print_list = TRUE,
                         allow_enter = TRUE){

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
  factor(user_response, levels = levels)
  }


#' Enter a TRUE/FALSE value:
#' @param prompt Message to guide user input
#' @export
enter_boolean <- function(prompt = "Enter TRUE/FALSE or T/F"){

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
  as.logical(user_response)
}

#' Enter a value:
#' @param prompt Message to guide user input
#' @param template atomic vector to guess the vector type.
#' @param print_list If a factor, print the options/levels as a list?
#' @param allow_enter If a factor and there is only a single value, allow the user to press enter to select that value.
#' @export
enter_value <- function(prompt = NULL,template, print_list = TRUE,allow_enter = FALSE){

  if(class(template) == "factor"){
    return(enter_factor(prompt = prompt,levels = levels(template),print_list = print_list,allow_enter = allow_enter))
  }

  if(class(template) == "integer"){
    return(enter_integer(prompt = prompt))
  }

  if(class(template) %in% c("Period", "Period_c")){
    return(enter_duration(prompt))
  }

  if(class(template) == "Date"){
    return(enter_date(prompt))
  }

  if(class(template) == "numeric"){
    return(enter_real(prompt))
  }

  if(class(template) == "logical"){
    return(enter_boolean(prompt))
  }
  enter_character(prompt)
}

#' Enter a value:
#' @param data Data-set for values list.
#' @param auto_values Value here is used instead of prompting for user input.
#' @export
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
