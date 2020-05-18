rdmcplot_adj<-function (Y, X, C, nbinsmat = NULL, binselectvec = NULL, scalevec = NULL, 
          supportmat = NULL, pvec = NULL, hmat = NULL, kernelvec = NULL, 
          weightsvec = NULL, covsvec = NULL, covs_evalvec = NULL, 
          covs_dropvec = NULL, ci = NULL, col_bins = NULL, pch_bins = NULL, 
          col_poly = NULL, lty_poly = NULL, col_xline = NULL, lty_xline = NULL, 
          nobins = FALSE, nopoly = FALSE, noxline = FALSE, nodraw = FALSE) 
{
  if (!is.numeric(C)) {
    stop("C has to be numeric")
  }
  if (max(C, na.rm = TRUE) >= max(X, na.rm = TRUE) | min(C, 
                                                         na.rm = TRUE) <= min(X, na.rm = TRUE)) {
    stop("cutoff variable outside range of running variable")
  }
  clist <- sort(unique(C))
  cnum <- length(clist)
  D <- as.numeric(X >= C)
  if (is.null(pvec)) {
    pvec = rep(4, cnum)
  }
  if (!is.null(hmat)) {
    if (is.null(dim(hmat))) {
      hmat <- matrix(hmat, nrow = cnum, ncol = 2)
    }
    haux <- hmat
  }
  else {
    haux <- matrix(Inf, ncol = 2, nrow = cnum)
  }
  if (!is.null(nbinsmat)) {
    if (is.null(dim(nbinsmat))) {
      nbinsmat <- matrix(nbinsmat, nrow = cnum, ncol = 2)
    }
  }
  if (is.null(binselectvec)) 
    binselectvec <- rep("esmv", cnum)
  if (is.null(scalevec)) 
    scalevec <- rep(1, cnum)
  if (is.null(kernelvec)) 
    kernelvec <- rep("uni", cnum)
  if (is.null(covs_evalvec)) 
    covs_evalvec <- rep(0, cnum)
  if (is.null(covs_dropvec)) 
    covs_dropvec <- rep(TRUE, cnum)
  X0 <- matrix(NA, nrow = length(Y), ncol = cnum)
  X1 <- matrix(NA, nrow = length(Y), ncol = cnum)
  YHAT0 <- matrix(NA, nrow = length(Y), ncol = cnum)
  YHAT1 <- matrix(NA, nrow = length(Y), ncol = cnum)
  XMEAN <- matrix(NA, nrow = length(Y), ncol = cnum)
  YMEAN <- matrix(NA, nrow = length(Y), ncol = cnum)
  CI_l <- matrix(NA, nrow = length(Y), ncol = cnum)
  CI_r <- matrix(NA, nrow = length(Y), ncol = cnum)
  count = 1
  for (c in clist) {
    yc <- Y[C == c & X <= c + haux[count, 2] & X >= c - 
              haux[count, 1]]
    xc <- X[C == c & X <= c + haux[count, 2] & X >= c - 
              haux[count, 1]]
    dc <- D[C == c & X <= c + haux[count, 2] & X >= c - 
              haux[count, 1]]
    yc0 <- yc[dc == 0]
    yc1 <- yc[dc == 1]
    xc0 <- xc[dc == 0]
    xc1 <- xc[dc == 1]
    aux <- rdrobust::rdplot(yc, xc, c = c, nbins = nbinsmat[count, 
                                                            ], binselect = binselectvec[count], scale = scalevec[count], 
                            support = supportmat[count, ], p = pvec[count], 
                            h = hmat[count, ], kernel = kernelvec[count], weights = weightsvec[count], 
                            covs = covsvec[count], covs_eval = covs_evalvec[count], 
                            covs_drop = covs_dropvec[count], ci = ci, hide = TRUE)
    xmean <- aux$vars_bins[, 2]
    ymean <- aux$vars_bins[, 3]
    x0 <- aux$vars_poly[aux$vars_poly[, 1] < c, 1]
    yhat0 <- aux$vars_poly[aux$vars_poly[, 1] < c, 2]
    x1 <- aux$vars_poly[aux$vars_poly[, 1] > c, 1]
    yhat1 <- aux$vars_poly[aux$vars_poly[, 1] > c, 2]
    length(xmean) <- length(Y)
    length(ymean) <- length(Y)
    length(x0) <- length(Y)
    length(yhat0) <- length(Y)
    length(x1) <- length(Y)
    length(yhat1) <- length(Y)
    XMEAN[, count] <- xmean
    YMEAN[, count] <- ymean
    X0[, count] <- x0
    X1[, count] <- x1
    YHAT0[, count] <- yhat0
    YHAT1[, count] <- yhat1
    if (!is.null(ci)) {
      ci_l <- aux$vars_bins[, 8]
      ci_r <- aux$vars_bins[, 9]
      length(ci_l) <- length(Y)
      length(ci_r) <- length(Y)
      CI_l[, count] <- ci_l
      CI_r[, count] <- ci_r
    }
    count <- count + 1
  }
  Xmean <- data.frame(XMEAN)
  Xmean <- Xmean[1:max(colSums(!is.na(Xmean))), ]
  names(Xmean) <- paste0(rep("Xmean"), 1:cnum)
  Ymean <- data.frame(YMEAN)
  Ymean <- Ymean[1:max(colSums(!is.na(Ymean))), ]
  names(Ymean) <- paste0(rep("Ymean"), 1:cnum)
  X0 <- data.frame(X0)
  X0 <- X0[1:max(colSums(!is.na(X0))), ]
  names(X0) <- paste0(rep("X0_"), 1:cnum)
  X1 <- data.frame(X1)
  X1 <- X1[1:max(colSums(!is.na(X1))), ]
  names(X1) <- paste0(rep("X1_"), 1:cnum)
  Yhat0 <- data.frame(YHAT0)
  Yhat0 <- Yhat0[1:max(colSums(!is.na(Yhat0))), ]
  names(Yhat0) <- paste0(rep("Yhat0_"), 1:cnum)
  Yhat1 <- data.frame(YHAT1)
  Yhat1 <- Yhat1[1:max(colSums(!is.na(Yhat1))), ]
  names(Yhat1) <- paste0(rep("Yhat1_"), 1:cnum)
  if (!is.null(ci)) {
    CI_l <- data.frame(CI_l)
    CI_l <- CI_l[1:max(colSums(!is.na(CI_l))), ]
    names(CI_l) <- paste0(rep("CI_l_"), 1:cnum)
    CI_r <- data.frame(CI_r)
    CI_r <- CI_r[1:max(colSums(!is.na(CI_r))), ]
    names(CI_r) <- paste0(rep("CI_r_"), 1:cnum)
  }
  colorlist <- c("darkblue", "darkred", "darkgreen", "darkorange", 
                 "gray50", "khaki4", "brown3", "blue", "darkgoldenrod4", 
                 "cyan4")
  if (is.null(col_bins)) {
    col_bins <- colorlist
  }
  if (is.null(pch_bins)) {
    pch_bins <- rep(1, cnum)
  }
  if (is.null(col_poly)) {
    col_poly <- colorlist
  }
  if (is.null(lty_poly)) {
    lty_poly <- rep("solid", cnum)
  }
  if (is.null(col_xline)) {
    col_xline <- colorlist
  }
  if (is.null(lty_xline)) {
    lty_xline <- rep("dashed", cnum)
  }
  rdmc_plot <- ggplot() + theme_bw() + labs(x = "Running variable", 
                                            y = "Outcome")+
    scale_x_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))
  if (nobins == FALSE) {
    rdmc_plot <- rdmc_plot + geom_point(aes(x = Xmean[, 
                                                      1], y = Ymean[, 1]), col = col_bins[1], shape = pch_bins[1], 
                                        na.rm = TRUE)
    for (c in 2:cnum) {
      rdmc_plot <- rdmc_plot + geom_point(data=data.frame(x = Xmean[,c],
                                                          y = Ymean[, c]),
                                          aes(x = x, 
                                              y = y), col = col_bins[c], shape = pch_bins[c], 
                                          na.rm = TRUE)
    }
  }
  if (nopoly == FALSE) {
    rdmc_plot <- rdmc_plot + geom_line(aes(x = X0[, 1], 
                                           y = Yhat0[, 1]), col = col_poly[1], linetype = lty_poly[1], 
                                       na.rm = TRUE) + geom_line(aes(x = X1[, 1], y = Yhat1[, 
                                                                                            1]), col = col_poly[1], linetype = lty_poly[1], 
                                                                 na.rm = TRUE)
    for(c in 2:cnum) {
      rdmc_plot <- rdmc_plot + 
        geom_line(data=data.frame(x = X0[,c],
                                  y = Yhat0[, c]),
                  aes(x = x, 
                      y = y), 
                  col = col_poly[c], 
                  linetype = lty_poly[c], 
                  na.rm = TRUE) + 
        geom_line(data=data.frame(x=X1[, c],
                             y=Yhat1[, c]),
                  aes(x = x, 
                      y = y), 
                  col = col_poly[c], 
                  linetype = lty_poly[c], 
                  na.rm = TRUE)
    }
  }
  if (noxline == FALSE) {
    rdmc_plot <- rdmc_plot + geom_vline(xintercept = clist[1], 
                                        col = col_xline[1], linetype = lty_xline[c])
    for (c in 2:cnum) {
      rdmc_plot <- rdmc_plot + geom_vline(xintercept = clist[c], 
                                          col = col_xline[c], linetype = lty_xline[c])
    }
  }
  if (!is.null(ci)) {
    rdmc_plot <- rdmc_plot + geom_errorbar(aes(x = Xmean[, 
                                                         1], ymin = CI_l[, 1], ymax = CI_r[, 1]), col = col_bins[1], 
                                           linetype = 1)
    for (c in 2:cnum) {
      rdmc_plot <- rdmc_plot + geom_errorbar(aes(x = Xmean[, 
                                                           c], ymin = CI_l[, c], ymax = CI_r[, c]), col = col_bins[c], 
                                             linetype = 1)
    }
  }
  if (nodraw == FALSE) {
    print(rdmc_plot)
  }
  if (is.null(ci)) {
    output <- list(clist = clist, cnum = cnum, X0 = X0, 
                   X1 = X1, Yhat0 = Yhat0, Yhat1 = Yhat1, Xmean = Xmean, 
                   Ymean = Ymean, rdmc_plot = rdmc_plot)
  }
  else {
    output <- list(clist = clist, cnum = cnum, X0 = X0, 
                   X1 = X1, Yhat0 = Yhat0, Yhat1 = Yhat1, Xmean = Xmean, 
                   Ymean = Ymean, rdmc_plot = rdmc_plot, CI_l = CI_l, 
                   CI_r = CI_r)
  }
  return(output)
}
