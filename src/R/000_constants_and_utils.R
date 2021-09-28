library(here)

DATA_DIR <- here("data")
MATCHES_DIR <- file.path(DATA_DIR, "calculated")
OUTPUT_DIR <- here("build")
FIGURES_DIR <- file.path(OUTPUT_DIR, "figures")

# Create directories if they don't exist
dir.create(DATA_DIR, showWarnings = FALSE)
dir.create(MATCHES_DIR, showWarnings = FALSE)
dir.create(OUTPUT_DIR, showWarnings = FALSE)
dir.create(FIGURES_DIR, showWarnings = FALSE)

#' Convert the output into a factor variable for use in analysis
nmatch_to_df <- function(obj, origid) {
  ## We want a factor that we can merge onto our
  ## existing dataset. Here returning a data.frame so that
  ## we can merge --- seems less error prone than using
  ## rownames even if it is slower.
  matchesdat <- data.frame(
    bm = obj$group_id,
    match_id = c(obj$id_1, obj$id_2)
  )
  matchesdat$id <- origid[matchesdat$match_id]
  return(matchesdat)
}


#' Return the solver indicated by the passed environment variables.
#'
#' @return A `list` of information to be passed to `nmatch`
get_solverlist <- function() {
  if (toupper(Sys.getenv("VACCINE_OPTIMIZER")) == "GLPK") {
    optimizer <- "glpk"
    solverlist <- list(name = "glpk", approximate = 1, t_max = 100, trace = 1)
  } else {
    optimizer <- "gurobi"
    solverlist <- solverlist <- list(
      name = "gurobi",
      approximate = 0,
      t_max = 2000,
      trace = 1
    )
  }
  return(solverlist)
}

#' Find a Rosenbaum sensitivity parameter (Gamma)
#'
#' @param g A proposed value for Gamma (1= randomized experiment: under the null of no effects, both members of a pair equally likely to perceive more; more than 1: under the null of no effects, the higher perceiver is gamma odds more likely to be the higher perceive because of an unobserved covariate that also strongly predicts outcomes.
#' @param y A vector of outcomes.
#' @param z A vector of "treatment" indicators (here using "rank of perceptions within pair" or "higher versus lower perceiver")
#' @param dat A data frame
#' @param p_limit The p-value above which we would say we do not have evidence against the null of no effect
#' @param return Either "just_p_diff" a scalar difference between the p-value
#' implied by the gamma value or if "all" the whole senm object
#' @return either a p-value or the entire senm object
#' @details If a binary outcome, then using proportions (using trim=Inf). Else using
#' the robust test statistic default in senm.
find_gamma_lim <- function(g, y, z, dat, p_limit = .051, return = "just_p_diff") {
  require(sensitivitymult)
  tmpdat <- dat[dat[[z]] != .5, ] ## remove pairs with the same perceptions

  ## The test statistic used by senm should differ for binary outcomes.
  ## trying to use the default test statistic from that package for non-binary outcomes.
  thetrim <- if (length(unique(tmpdat[[y]])) == 2) {
    Inf
  } else {
    3
  }

  ## Producing both tailed p-values to make the function agnostic about
  ## direction of effect
  res_g <- senm(
    y = tmpdat[[y]], z = tmpdat[[z]],
    mset = tmpdat$bm, trim = thetrim, gamma = g, alternative = "greater"
  )
  res_l <- senm(
    y = tmpdat[[y]], z = tmpdat[[z]],
    mset = tmpdat$bm, trim = thetrim, gamma = g, alternative = "less"
  )
  res <- if (res_g$pval <= res_l$pval) {
    res_g
  } else {
    res_l
  }

  if (return == "just_p_diff" & res$pval <= p_limit) {
    ## return a zero when p=p_limit so just over .05
    ## We want to return a difference here because we are searching for the
    ## gamma value at which the res$pval crosses over p_limit and becomes
    ## larger than it. We are using uniroot() to look for zeros rather than
    ## optim to minimize the function (at this point). Could make this more like
    ## a squared difference to impose some smoothness and unique minimum if we
    ## wanted to go that way. Seems to work well enough now.
    return(res$pval - p_limit)
  }
  if (return == "just_p_diff" & res$pval >= p_limit) {
    return(1)
  }
  if (return == "all") {
    return(res)
  }
}

#' A version of sensitivitymult::amplify that uses a matrix instead of a named vector
my_amplify <- function(gamma, lambda) {
  ## differs from sensitivitymult::amplify by using matrix rather than a named vector
  ## for results
  stopifnot(length(gamma) == 1)
  stopifnot(gamma > 1)
  stopifnot(min(lambda) > gamma)
  delta <- (gamma * lambda - 1) / (lambda - gamma)
  res <- cbind(delta = delta, lambda = lambda)
  return(res)
}
