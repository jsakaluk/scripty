#' A Function That Fits a Pseudo-Indicator Model (PIM) to a Set of Variables
#'
#' This function fits a PIM (Rose et al., 2019), to a set of indicators. Follows an adapted version of the
#' coding approach provided by McNeish and Wolf (2020, https://osf.io/7ea3k/)
#' @param df input data frame consisting of variables in the measurement model
#' @param model input character for whether measurement model to reproduce sum score (default) or average score is desired
#' @return a fitted lavaan object
#' @export
#' @examples
#' pim.hs <- pim(df, model = "sum")

pim <- function(df, model = "sum"){
  #Store number of last indicator variable
  last <- length(names(df))
  #Specify a one indicator LV of last variable;
  #Fix loading to 1 for sum, and to num of vars for average
  if(model == "sum"){
    eta <- sprintf("LV =~1*%s", names(df)[[last]])

  }else if(model == "average"){
    eta <- sprintf("LV =~%s*%s", last, names(df)[[last]])
  }

  #Save vector of remaining indicator names and length
  upnames <- names(df)
  upnames <- upnames[1:length(upnames)-1]
  upnamesleng <- length(upnames)

  #Create lists of remaining single loading LVs and their names
  fac.list <- list()
  fac.name.list <- list()
  for (i in 1:upnamesleng) {
    fac.list[[i]]<-sprintf("f%s =~ %s", i, upnames[[i]])
    fac.name.list[[i]] <-sprintf("f%s", i)
  }
  facs = paste(fac.list, collapse = "\n")

  #Specify constrained regression of last indicator onto all others.
  reg <- sprintf("%s ~a* ", names(df)[[last]])
  regs <- gsub(" ", "",paste(reg,paste(upnames, collapse = "+a*")), fixed = T)

  a <- "a == -1"

  #All indicators have untercepts estimated except for final indicator
  lastm1 = last-1
  int.list = list()
  for (i in 1:lastm1) {
    int.list[[i]]=sprintf("%s ~ 1",names(df)[[i]])
  }
  int.list[[last]] = sprintf("%s ~ 0*1",names(df)[[last]])
  ints = paste(int.list, collapse = "\n")

  #All LVs correlate with the focal LV, and then with each other
  lv <- "~~LV"
  cor.1 <- gsub(" ", "", paste(paste(fac.name.list, collapse = "+"), lv),   fixed = T)

  cor.list <- list()
  i1 <- 1
  counter <- upnamesleng-1

  while (counter > 0) {
    name <- sprintf(" ~~%s", fac.name.list[[1]])
    fac.name.list <- fac.name.list[2:length(fac.name.list)]
    cor.list[[i1]] <- gsub(" ", "", paste(paste(fac.name.list, collapse = "+"), name),   fixed = T)

    i1 <- i1+1
    counter <- counter-1
  }

  cors <- paste(cor.list, collapse= "\n")

  #Patch the pieces of syntax together
  pim.model <- sprintf("#First item loads on latent variable with loading constrained to 1 by default #\n%s\n%s\n\n#All other itmes predict the last item with the same coefficient#\n%s\n\n#constrain the regression coefficient for all items to -1#
%s\n\n#estimate intercepts of all other items, first item intercept is constrained to 0#\n%s\n\n##All predictor items covary with latent variable##\n%s\n%s", eta, facs, regs, a, ints, cor.1, cors)
  #Concatenate And print stitched together lavaan syntax
  cat(pim.model,"\n")
  #Submit to lavaan::sem
  model.fit <- lavaan::sem(pim.model, data=df)
  return(model.fit)
}
