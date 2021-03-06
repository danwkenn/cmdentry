% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enter.R
\name{enter_date}
\alias{enter_date}
\title{Enter a date}
\usage{
enter_date(prompt = "Enter a date in the form DD/MM/YYYY", coerce = TRUE)
}
\arguments{
\item{prompt}{Message to guide user input}

\item{coerce}{Convert to Date type?}
}
\description{
Enter a date
}
\examples{
fun <- function() {
  ANSWER <- enter_date("When was the first time you felt satisfied by R? ")
  ## a better version would check the answer less cursorily, and
  ## perhaps re-prompt
  if (ANSWER < "2000-02-29")
    cat("This is impossible.  YOU LIED!\n")
  else
    cat("I knew it.\n")
}
if(interactive()) fun()
}
