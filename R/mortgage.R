#' @return dataframe of monthly payment
#'
#' @example mortgage(100000,30,0.05)
#'
#' @description The package gives a table that lists each regular payment on a mortgage based on a loan amount, annual interest rate, and duration of the laon.
#'
#'@export

mortgage<- function(loan_amt, term, APR) {
  # calculate simple monthly rate
  monthly_rate = APR / 12

  # calculate (constant) contractual monthly payment amount

  r = (1 + monthly_rate) ^ term - 1
  payment = loan_amt * monthly_rate * (r + 1) / r

  # initialize output variables
  interest = principal = balance = vector("numeric", term)

  # calc amortization schedule
  outstanding_principal = loan_amt
  for (i in 1:term) {
    intr = outstanding_principal * monthly_rate
    prnp = payment - intr
    outstanding_principal = outstanding_principal - prnp

    interest[i]  = round(as.numeric(intr),0)
    principal[i] = round(as.numeric(prnp),0)
    balance[i] = round(as.numeric(outstanding_principal),0)
  }
  #return(outstanding_principal)
  data.frame(month = 1:term, interest, principal, balance)

}
