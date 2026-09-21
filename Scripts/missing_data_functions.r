### Functions to work with missing data analysis

## Function to create vector of names/factors following use of st_contains function
# x = objected created by st_contains()
# n = names to apply 
# factor = logical. Return factor (Default), otherwise character.
contains_point_name <- function(x, n, factor=TRUE) {
    r <- vector("character", length(unlist(x)))
    for(i in 1:length(x)) {
        r[x[[i]]] <- n[i]
    }
    if(factor==TRUE) {return(as.factor(r))} else {return(r)}
}

### Round up value to even
round_up_even <- function(x) {  2 * ceiling(x / 2)}

### Littles MCAR test (Claude code)
library(MASS)
#
safe_solve <- function(S) tryCatch(solve(S), error = function(e) MASS::ginv(S))
#
littles_mcar_test <- function(dt, tol = 1e-6, max_iter = 200) {
  dt   <- as.data.table(dt)
  vars <- names(dt)
  p    <- length(vars)
  X    <- as.matrix(dt)
  n    <- nrow(X)
  M    <- !is.na(X)

  # precompute pattern groups ONCE
  pat_vec <- apply(M, 1, function(r) paste0(as.integer(r), collapse = ""))
  groups  <- data.table(row = seq_len(n), pat = pat_vec)[, .(idx = list(row)), by = pat]
  groups[, obs := lapply(pat, function(s) as.logical(as.integer(strsplit(s, "")[[1]])))]
  G <- nrow(groups)

  mu    <- colMeans(X, na.rm = TRUE)
  Sigma <- cov(X, use = "pairwise.complete.obs")
  Sigma[is.na(Sigma)] <- 0
  Ximp  <- X

  for (iter in seq_len(max_iter)) {
    mu_old <- mu

    for (g in seq_len(G)) {
      idx <- groups$idx[[g]]
      obs <- groups$obs[[g]]
      if (all(obs)) next
      if (!any(obs)) {
        Ximp[idx, ] <- matrix(mu, length(idx), p, byrow = TRUE)
        next
      }
      inv_oo <- safe_solve(Sigma[obs, obs, drop = FALSE])
      beta   <- inv_oo %*% Sigma[obs, !obs, drop = FALSE]
      Xo     <- sweep(X[idx, obs, drop = FALSE], 2, mu[obs])
      Ximp[idx, !obs] <- matrix(mu[!obs], length(idx), sum(!obs), byrow = TRUE) + Xo %*% beta
    }

    mu    <- colMeans(Ximp)
    Sigma <- cov(Ximp)
    if (max(abs(mu - mu_old)) < tol) break
  }

  stat_dt <- groups[, {
    o    <- obs[[1]]
    idxg <- idx[[1]]
    kj   <- sum(o)
    if (kj == 0) {
      .(d2 = 0, dfj = 0)
    } else {
      xbar_j <- colMeans(X[idxg, o, drop = FALSE])
      diff   <- xbar_j - mu[o]
      .(d2 = length(idxg) * as.numeric(t(diff) %*% safe_solve(Sigma[o, o, drop = FALSE]) %*% diff),
        dfj = kj)
    }
  }, by = pat]

  D2  <- sum(stat_dt$d2)
  dof <- sum(stat_dt$dfj) - p

  list(statistic = D2, df = dof, p.value = 1 - pchisq(D2, dof))
}

#res <- littles_mcar_test(dt)
#res$statistic   # Little's D2 statistic
#res$df          # degrees of freedom
#res$p.value     # p-value


### Function to identify high/low clusters that are significant
### Used with Gettis-Ord Gi* Hot spot analysis to classify clusters for easy plotting
# p = p folded sim 
# g = gi*
# s = significance level
# Output: -1 = significant low spot, 1 = significant high spot, 0 = not significant.
gi_c <- function(p, g, s=0.05) {
    # Low
    c <- ifelse(p < s & g < 0, -1, 0)
    # High
    c <- ifelse(p < s & g > 0, 1, c)
    return(c)    
}