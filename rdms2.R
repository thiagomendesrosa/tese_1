rdms2 <- function (Y, X, C, X2 = NULL, zvar = NULL, C2 = NULL, rangemat = NULL, 
          xnorm = NULL, fuzzy = NULL, derivvec = NULL, pooled_opt = NULL, 
          pvec = NULL, qvec = NULL, hmat = NULL, bmat = NULL, rhovec = NULL, 
          covsvec = NULL, covs_dropvec = NULL, kernelvec = NULL, weightsvec = NULL, 
          bwselectvec = NULL, scaleparvec = NULL, scaleregulvec = NULL, 
          masspointsvec = NULL, bwcheckvec = NULL, bwrestrictvec = NULL, 
          stdvarsvec = NULL, vcevec = NULL, nnmatchvec = NULL, cluster = NULL, 
          level = 95, plot = FALSE) 
{
  if (!is.null(X2) & is.null(zvar)) 
    stop("Need to specify zvar when X2 is specified")
  if (!is.null(X2) & is.null(C2)) 
    stop("Need to specify C2 if X2 is specified")
  cnum <- length(C)
  if (!is.null(C2)) {
    if (cnum != length(C2)) 
      stop("cutoff coordinates incorrectly specified")
  }
  if (!is.null(rangemat)) {
    if (is.null(dim(rangemat))) {
      rangemat <- matrix(rangemat, nrow = cnum, ncol = 2)
    }
  }
  else {
    rangemat <- cbind(rep(-Inf, cnum), rep(Inf, cnum))
  }
  if (!is.null(hmat)) {
    if (is.null(dim(hmat))) {
      hmat <- matrix(hmat, nrow = cnum, ncol = 2)
    }
  }
  if (!is.null(bmat)) {
    if (is.null(dim(bmat))) {
      bmat <- matrix(bmat, nrow = cnum, ncol = 2)
    }
  }
  if (is.null(covs_dropvec)) 
    covs_dropvec <- rep(TRUE, cnum)
  if (is.null(kernelvec)) 
    kernelvec <- rep("tri", cnum)
  if (is.null(bwselectvec)) 
    bwselectvec <- rep("mserd", cnum)
  if (is.null(vcevec)) 
    vcevec <- rep("nn", cnum)
  if (is.null(nnmatchvec)) 
    nnmatchvec <- rep(3, cnum)
  if (is.null(scaleparvec)) 
    scaleparvec <- rep(1, cnum)
  if (is.null(scaleregulvec)) 
    scaleregulvec <- rep(1, cnum)
  if (is.null(masspointsvec)) 
    masspointsvec <- rep("adjust", cnum)
  if (is.null(bwrestrictvec)) 
    bwrestrictvec <- rep(TRUE, cnum)
  if (is.null(stdvarsvec)) 
    stdvarsvec <- rep(FALSE, cnum)
  B <- matrix(NA, nrow = 1, ncol = cnum + 1)
  V <- matrix(NA, nrow = 1, ncol = cnum + 1)
  Coefs <- matrix(NA, nrow = 1, ncol = cnum + 1)
  Nh <- matrix(NA, nrow = 2, ncol = cnum + 1)
  CI <- matrix(NA, nrow = 2, ncol = cnum + 1)
  Pv <- matrix(NA, nrow = 1, ncol = cnum + 1)
  H <- matrix(NA, nrow = 2, ncol = cnum + 1)
  c.disp <- NULL
  if (is.null(X2)) {
    for (c in 1:cnum) {
      xc <- X - C[c]
      Rc <- rangemat - C
      yc <- Y[xc >= Rc[c, 1] & xc <= Rc[c, 2]]
      xc <- xc[xc >= Rc[c, 1] & xc <= Rc[c, 2]]
      rdr.tmp <- rdrobust::rdrobust(yc, xc, 
                                    fuzzy=fuzzy,
                                    #fuzzy = if(c==1){fuzzy[,1]}else{if(c==2){fuzzy[,2]}else{fuzzy[,3]}}, 
                                    deriv = derivvec[c], p = pvec[c], q = qvec[c], 
                                    h = hmat[c, ], b = bmat[c, ], rho = rhovec[c], 
                                    covs = covsvec[c], covs_drop = covs_dropvec[c], 
                                    kernel = kernelvec[c], weights = weightsvec[c], 
                                    bwselect = bwselectvec[c], scalepar = scaleparvec[c], 
                                    scaleregul = scaleregulvec[c], masspoints = masspointsvec[c], 
                                    bwcheck = bwcheckvec[c], bwrestrict = bwrestrictvec[c], 
                                    stdvars = stdvarsvec[c], vce = vcevec[c], nnmatch = nnmatchvec[c], 
                                    cluster = cluster, level = level)
      B[1, c] <- rdr.tmp$Estimate[2]
      V[1, c] <- rdr.tmp$se[3]^2
      Coefs[1, c] <- rdr.tmp$Estimate[1]
      CI[, c] <- rdr.tmp$ci[3, ]
      H[, c] <- rdr.tmp$bws[1, ]
      Nh[, c] <- rdr.tmp$N_h
      Pv[1, c] <- rdr.tmp$pv[3]
      c.disp <- c(c.disp, round(C[c], 2))
    }
  }
  else {
    for (c in 1:cnum) {
      xc <- sqrt((X - C[c])^2 + (X2 - C2[c])^2) * (2 * 
                                                     zvar - 1)
      yc <- Y[xc >= rangemat[c, 1] & xc <= rangemat[c, 
                                                    2]]
      xc <- xc[xc >= rangemat[c, 1] & xc <= rangemat[c, 
                                                     2]]
      rdr.tmp <- rdrobust::rdrobust(yc, xc, fuzzy = fuzzy, 
                                    deriv = derivvec[c], p = pvec[c], q = qvec[c], 
                                    h = hmat[c, ], b = bmat[c, ], rho = rhovec[c], 
                                    covs = covsvec[c], covs_drop = covs_dropvec[c], 
                                    kernel = kernelvec[c], weights = weightsvec[c], 
                                    bwselect = bwselectvec[c], scalepar = scaleparvec[c], 
                                    scaleregul = scaleregulvec[c], masspoints = masspointsvec[c], 
                                    bwcheck = bwcheckvec[c], bwrestrict = bwrestrictvec[c], 
                                    stdvars = stdvarsvec[c], vce = vcevec[c], nnmatch = nnmatchvec[c], 
                                    cluster = cluster, level = level)
      B[1, c] <- rdr.tmp$Estimate[2]
      V[1, c] <- rdr.tmp$se[3]^2
      Coefs[1, c] <- rdr.tmp$Estimate[1]
      CI[, c] <- rdr.tmp$ci[3, ]
      H[, c] <- rdr.tmp$bws[1, ]
      Nh[, c] <- rdr.tmp$N_h
      Pv[1, c] <- rdr.tmp$pv[3]
      c.disp <- c(c.disp, paste0("(", round(C[c], 2), 
                                 ",", round(C2[c], 2), ")"))
    }
  }
  if (!is.null(xnorm)) {
    aux1 <- paste0("rdrobust::rdrobust(Y,xnorm,covs=covsvec,cluster=cluster,", 
                   pooled_opt, ")")
    rdr <- eval(parse(text = aux1))
    B[1, cnum + 1] <- rdr$Estimate[2]
    V[1, cnum + 1] <- rdr$se[3]^2
    Coefs[1, cnum + 1] <- rdr$Estimate[1]
    CI[, cnum + 1] <- rdr$ci[3, ]
    H[, cnum + 1] <- rdr$bws[1, ]
    Nh[, cnum + 1] <- rdr$N_h
    Pv[1, cnum + 1] <- rdr$pv[3]
  }
  cat("\n")
  cat(paste0(rep("=", 80), collapse = ""))
  cat("\n")
  cat(format("Cutoff", width = 17))
  cat(format("Coef.", width = 9))
  cat(format("P-value", width = 17))
  cat(format("95% CI", width = 16))
  cat(format("hl", width = 9))
  cat(format("hr", width = 10))
  cat(format("Nh", width = 10))
  cat("\n")
  cat(paste0(rep("=", 80), collapse = ""))
  cat("\n")
  for (k in 1:cnum) {
    if (is.null(C2)) {
      cat(format(sprintf("%4.2f", c.disp[k]), width = 17))
    }
    else {
      cat(paste0("(", format(sprintf("%4.2f", C[k])), 
                 ",", format(sprintf("%4.2f", C2[k])), format(")", 
                                                              width = 5)))
    }
    cat(format(sprintf("%7.3f", Coefs[k]), width = 10))
    cat(format(sprintf("%1.3f", Pv[k]), width = 10))
    cat(format(sprintf("%4.3f", CI[1, k]), width = 10))
    cat(format(sprintf("%4.3f", CI[2, k]), width = 10))
    cat(format(sprintf("%4.3f", H[1, k]), width = 9))
    cat(format(sprintf("%4.3f", H[2, k]), width = 10))
    cat(format(sprintf("%4.0f", Nh[1, k] + Nh[2, k]), width = 10))
    cat("\n")
  }
  if (!is.null(xnorm)) {
    cat(paste0(rep("-", 80), collapse = ""))
    cat("\n")
    cat(format("Pooled", width = 17))
    cat(format(sprintf("%7.3f", Coefs[cnum + 1]), width = 10))
    cat(format(sprintf("%1.3f", Pv[cnum + 1]), width = 10))
    cat(format(sprintf("%4.3f", CI[1, cnum + 1]), width = 10))
    cat(format(sprintf("%4.3f", CI[2, cnum + 1]), width = 10))
    cat(format(sprintf("%4.3f", H[1, cnum + 1]), width = 9))
    cat(format(sprintf("%4.3f", H[2, cnum + 1]), width = 10))
    cat(format(sprintf("%4.0f", Nh[1, cnum + 1] + Nh[2, 
                                                     cnum + 1]), width = 10))
    cat("\n")
  }
  cat(paste0(rep("=", 80), collapse = ""))
  cat("\n")
  colnames(B) <- c(1:cnum, "pooled")
  colnames(V) <- c(1:cnum, "pooled")
  colnames(Coefs) <- c(1:cnum, "pooled")
  colnames(CI) <- c(1:cnum, "pooled")
  colnames(Nh) <- c(1:cnum, "pooled")
  colnames(H) <- c(1:cnum, "pooled")
  colnames(Pv) <- c(1:cnum, "pooled")
  rownames(Nh) <- c("left", "right")
  rownames(H) <- c("left", "right")
  output <- list(B = B, V = V, Coefs = Coefs, Nh = Nh, CI = CI, 
                 H = H, Pv = Pv,C=C)
  return(output)
}
