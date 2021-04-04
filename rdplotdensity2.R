rdplotdensity2<-function (rdd, X, plotRange = NULL, plotN = 10, plotGrid = c("es", 
                                                             "qs"), alpha = 0.05, type = NULL, lty = NULL, lwd = NULL, 
          lcol = NULL, pty = NULL, pwd = NULL, pcol = NULL, CItype = NULL, 
          CIuniform = FALSE, CIsimul = 2000, CIshade = NULL, CIcol = NULL, 
          bwselect = NULL, hist = TRUE, histBreaks = NULL, histFillCol = 3, 
          histFillShade = 0.2, histLineCol = "white", title = "", 
          xlabel = "", ylabel = "", legendTitle = NULL, legendGroups = NULL) 
{
  c <- rdd$opt$c
  p <- rdd$opt$p
  q <- rdd$opt$q
  hl <- rdd$h$left
  hr <- rdd$h$right
  kernel <- rdd$opt$kernel
  regularize <- rdd$opt$regularize
  nLocalMin <- rdd$opt$nLocalMin
  nUniqueMin <- rdd$opt$nUniqueMin
  massPoints <- rdd$opt$massPoints
  X <- as.vector(X)
  if (any(is.na(X))) {
    warning(paste(sum(is.na(X)), " missing ", switch((sum(is.na(X)) > 
                                                        1) + 1, "observation is", "observations are"), " ignored.\n", 
                  sep = ""))
    X <- X[!is.na(X)]
  }
  if (length(plotRange) == 0) {
    plotRange <- c(max(min(X), c - 3 * hl), min(max(X), 
                                                c + 3 * hr))
  }
  else if (length(plotRange) != 2) {
    stop("Plot range incorrectly specified.\n")
  }
  else if (plotRange[1] >= c | plotRange[2] <= c) {
    stop("Plot range incorrectly specified.\n")
  }
  if (length(plotN) == 0) {
    plotN <- c(10, 10)
  }
  else if (length(plotN) == 1) {
    plotN <- c(plotN, plotN)
  }
  else if (length(plotN) > 2) {
    stop("Number of grid points incorrectly specified.\n")
  }
  if (plotN[1] <= 1 | plotN[2] <= 1) {
    stop("Number of grid points incorrectly specified.\n")
  }
  if (length(plotGrid) == 0) {
    plotGrid <- "es"
  }
  else {
    plotGrid <- plotGrid[1]
  }
  if (!plotGrid %in% c("es", "qs")) {
    stop("Grid specification invalid.\n")
  }
  if (hist & is.null(histBreaks)) {
    temp_hist_n_l <- sum(X >= plotRange[1] & X < c)
    temp_hist_n_l <- ceiling(min(sqrt(temp_hist_n_l), 10 * 
                                   log(temp_hist_n_l)/log(10)))
    temp_hist_n_r <- sum(X <= plotRange[2] & X >= c)
    temp_hist_n_r <- ceiling(min(sqrt(temp_hist_n_r), 10 * 
                                   log(temp_hist_n_r)/log(10)))
    histBreaks <- c(seq(plotRange[1], c, length.out = temp_hist_n_l + 
                          1), seq(c, plotRange[2], length.out = temp_hist_n_r + 
                                    1)[2:(temp_hist_n_r + 1)])
  }
  scalel <- (sum(X <= c) - 1)/(length(X) - 1)
  scaler <- (sum(X >= c) - 1)/(length(X) - 1)
  if (plotGrid == "es") {
    gridl <- seq(plotRange[1], c, length.out = plotN[1])
    gridl[plotN[1]] <- c
    gridr <- seq(c, plotRange[2], length.out = plotN[2])
    gridr[1] <- c
  }
  else {
    gridl <- seq(mean(X <= plotRange[1]), mean(X <= c), 
                 length.out = plotN[1])
    gridl <- quantile(X, gridl)
    gridr <- seq(mean(X <= c), mean(X <= plotRange[2]), 
                 length.out = plotN[2])
    gridr <- quantile(X, gridr)
    gridl[plotN[1]] <- c
    gridr[1] <- c
  }
  if (!is.null(bwselect)) {
    if (bwselect %in% c("mse-dpi", "imse-dpi", "mse-rot", 
                        "imse-rot")) {
      Estl <- lpdensity(data = X[X <= c], grid = gridl, 
                        bwselect = bwselect, p = p, q = q, v = 1, kernel = kernel, 
                        scale = scalel, regularize = regularize, nLocalMin = nLocalMin, 
                        nUniqueMin = nUniqueMin, massPoints = massPoints)
      Estr <- lpdensity(data = X[X >= c], grid = gridr, 
                        bwselect = bwselect, p = p, q = q, v = 1, kernel = kernel, 
                        scale = scaler, regularize = regularize, nLocalMin = nLocalMin, 
                        nUniqueMin = nUniqueMin, massPoints = massPoints)
    }
    else {
      stop("Option bwselect incorrectly specified.\n")
    }
  }
  else {
    Estl <- lpdensity(data = X[X <= c], grid = gridl, bw = hl, 
                      p = p, q = q, v = 1, kernel = kernel, scale = scalel, 
                      regularize = regularize, nLocalMin = nLocalMin, 
                      nUniqueMin = nUniqueMin, massPoints = massPoints)
    Estr <- lpdensity(data = X[X >= c], grid = gridr, bw = hr, 
                      p = p, q = q, v = 1, kernel = kernel, scale = scaler, 
                      regularize = regularize, nLocalMin = nLocalMin, 
                      nUniqueMin = nUniqueMin, massPoints = massPoints)
  }
  Estplot <- lpdensity.plot2(Estl, Estr, alpha = alpha, type = type, 
                            lty = lty, lwd = lwd, lcol = lcol, pty = pty, pwd = pwd, 
                            pcol = pcol, CItype = CItype, CIuniform = CIuniform, 
                            CIsimul = CIsimul, CIshade = CIshade, CIcol = CIcol, 
                            hist = hist, histData = X, histBreaks = histBreaks, 
                            histFillCol = histFillCol, histFillShade = histFillShade, 
                            histLineCol = histLineCol, title = title, xlabel = xlabel, 
                            ylabel = ylabel, legendTitle = legendTitle, legendGroups = legendGroups) + 
    theme(legend.position = "none")
  #print(Estplot)
  return(list(Estl = Estl, Estr = Estr, Estplot = Estplot))
}

lpdensity.plot2<- function (..., alpha = NULL, type = NULL, lty = NULL, lwd = NULL, 
          lcol = NULL, pty = NULL, pwd = NULL, pcol = NULL, grid = NULL, 
          CItype = NULL, CIuniform = FALSE, CIsimul = 2000, CIshade = NULL, 
          CIcol = NULL, hist = FALSE, histData = NULL, histBreaks = NULL, 
          histFillCol = 3, histFillShade = 0.2, histLineCol = "white", 
          title = "", xlabel = "", ylabel = "", legendTitle = NULL, 
          legendGroups = NULL) 
{
  x <- list(...)
  nfig <- length(x)
  if (nfig == 0) 
    stop("Nothing to plot.\n")
  flagToPlot <- rep(TRUE, nfig)
  for (i in 1:length(x)) {
    if (class(x[[i]])[1] == "lpbwdensity") {
      flagToPlot[i] <- FALSE
      warning(paste("Input ", i, " is an \"lpbwdensity\" object, which is not supported by the plot method.\n", 
                    sep = ""))
      next
    }
    if (nrow(x[[i]]$Estimate) < 2) {
      flagToPlot[i] <- FALSE
      warning(paste("At least two grid points are needed to plot input ", 
                    i, ".\n", sep = ""))
      next
    }
  }
  x <- x[flagToPlot]
  nfig <- length(x)
  if (nfig == 0) 
    stop("Nothing to plot.\n")
  isLpbwdensity <- FALSE
  for (i in 1:nfig) {
    if (class(x[[i]])[1] == "lpbwdensity") 
      isLpbwdensity <- TRUE
  }
  if (isLpbwdensity) 
    stop("The plot method does not support \"lpbwdensity\" objects.\n")
  if (length(alpha) == 0) {
    alpha <- rep(0.05, nfig)
  }
  else if (!all(alpha > 0 & alpha < 1)) {
    stop("Significance level incorrectly specified.\n")
  }
  else {
    alpha <- rep(alpha, length.out = nfig)
  }
  if (length(type) == 0) {
    type <- rep("line", nfig)
  }
  else {
    if (!all(type %in% c("line", "points", "both"))) {
      stop("Plotting type incorrectly specified.\n")
    }
    type <- rep(type, length.out = nfig)
  }
  if (length(CItype) == 0) {
    CItype <- rep("region", nfig)
  }
  else {
    if (!all(CItype %in% c("region", "line", "ebar", "all", 
                           "none"))) {
      stop("Confidence interval type incorrectly specified.\n")
    }
    CItype <- rep(CItype, length.out = nfig)
  }
  if (length(lty) == 0) {
    lty <- rep(1, nfig)
  }
  else {
    lty <- rep(lty, length.out = nfig)
  }
  if (length(lwd) == 0) {
    lwd <- rep(0.5, nfig)
  }
  else {
    lwd <- rep(lwd, length.out = nfig)
  }
  if (length(lcol) == 0) {
    lcol <- 1:nfig
  }
  else {
    lcol <- rep(lcol, length.out = nfig)
  }
  if (length(pty) == 0) {
    pty <- rep(1, nfig)
  }
  else {
    pty <- rep(pty, length.out = nfig)
  }
  if (length(pwd) == 0) {
    pwd <- rep(1, nfig)
  }
  else {
    pwd <- rep(pwd, length.out = nfig)
  }
  if (length(pcol) == 0) {
    pcol <- lcol
  }
  else {
    pcol <- rep(pcol, length.out = nfig)
  }
  if (length(CIshade) == 0) {
    CIshade <- rep(0.2, nfig)
  }
  else {
    CIshade <- rep(CIshade, length.out = nfig)
  }
  if (length(CIcol) == 0) {
    CIcol <- lcol
  }
  else {
    CIcol <- rep(CIcol, length.out = nfig)
  }
  if (length(legendTitle) == 0) {
    legendTitle <- ""
  }
  else {
    legendTitle <- legendTitle[1]
  }
  if (length(legendGroups) > 0) {
    legendGroups <- rep(legendGroups, length.out = nfig)
    legend_default <- FALSE
  }
  else {
    legend_default <- TRUE
  }
  if (!is.null(grid)) {
    if (!is.numeric(grid)) {
      stop("Option grid incorrectly specified.\n")
    }
  }
  if (hist & !is.null(histData)) {
    histData <- as.data.frame(histData)
    colnames(histData) <- c("v1")
    if (is.null(histBreaks)) {
      histBreaks <- seq(from = min(histData[, 1]), to = max(histData[, 
                                                                     1]), length.out = 21)
    }
    histScale <- mean(histData[, 1] >= min(histBreaks) & 
                        histData[, 1] <= max(histBreaks))
    temp_plot <- ggplot() + geom_histogram(data = histData, 
                                           aes(x = v1, y = ..density.. * histScale), breaks = histBreaks, 
                                           fill = histFillCol, col = histLineCol, alpha = histFillShade) + 
      theme_bw()
  }
  else {
    temp_plot <- ggplot() + theme_bw()
  }
  CI_l <- CI_r <- f_p <- Sname <- v1 <- ..density.. <- NULL
  col_all <- lty_all <- pty_all <- c()
  for (i in 1:nfig) {
    if (is.null(grid)) {
      plotIndex <- 1:nrow(x[[i]]$Estimate)
    }
    else {
      gridTemp <- grid[grid >= min(x[[i]]$Estimate[, "grid"]) & 
                         grid <= max(x[[i]]$Estimate[, "grid"])]
      if (length(gridTemp) == 0) {
        plotIndex <- NULL
      }
      else {
        plotIndex <- rep(NA, length(gridTemp))
        for (ii in 1:length(gridTemp)) {
          plotIndex[ii] <- which.min(abs(gridTemp[ii] - 
                                           x[[i]]$Estimate[, "grid"]))
        }
        plotIndex <- unique(plotIndex)
      }
    }
    data_x <- data.frame(x[[i]]$Estimate[, c("grid", "f_p", 
                                             "f_q", "se_p", "se_q"), drop = FALSE])
    if (x[[i]]$opt$q == x[[i]]$opt$p) {
      data_x$f_q <- data_x$f_p
      data_x$se_q <- data_x$se_p
    }
    if (CIuniform) {
      if (length(CIsimul) == 0) {
        CIsimul <- 2000
      }
      if (!is.numeric(CIsimul) | is.na(CIsimul)) {
        warning("Option CIsimul incorrectly specified. Will only plot pointwise confidence intervals.\n")
        z_val <- qnorm(1 - alpha[i]/2)
      }
      else if (ceiling(CIsimul) < 2) {
        warning("Option CIsimul incorrectly specified. Will only plot pointwise confidence intervals.\n")
        z_val <- qnorm(1 - alpha[i]/2)
      }
      else {
        CIsimul <- ceiling(CIsimul)
        if (x[[i]]$opt$q == x[[i]]$opt$p) {
          x[[i]]$CovMat_q <- x[[i]]$CovMat_p
        }
        corrMat <- sweep(sweep(x[[i]]$CovMat_q, MARGIN = 1, 
                               FUN = "*", STATS = 1/data_x$se_q), MARGIN = 2, 
                         FUN = "*", STATS = 1/data_x$se_q)
        normalSimu <- try(mvrnorm(n = CIsimul, mu = rep(0, 
                                                        nrow(corrMat)), Sigma = corrMat), silent = TRUE)
        if (is.character(normalSimu)) {
          print(normalSimu)
          warning("Variance-Covariance is not positive semidefinite. Will only plot pointwise confidence intervals.\n")
          z_val <- qnorm(1 - alpha[i]/2)
        }
        else {
          z_val <- quantile(apply(normalSimu, MARGIN = 1, 
                                  FUN = function(x) {
                                    max(abs(x))
                                  }), 1 - alpha[i])
        }
      }
    }
    else {
      z_val <- qnorm(1 - alpha[i]/2)
    }
    data_x$CI_l <- data_x$f_q - z_val * data_x$se_q
    data_x$CI_r <- data_x$f_q + z_val * data_x$se_q
    if (legend_default) {
      data_x$Sname <- paste("Series", i, sep = " ")
      legendGroups <- c(legendGroups, data_x$Sname)
    }
    else {
      data_x$Sname <- legendGroups[i]
    }
    if (CItype[i] %in% c("region", "all")) 
      temp_plot <- temp_plot + geom_ribbon(data = data_x, 
                                           aes(x = grid, ymin = CI_l, ymax = CI_r), alpha = CIshade[i], 
                                           fill = CIcol[i])
    if (CItype[i] %in% c("line", "all")) 
      temp_plot <- temp_plot + geom_line(data = data_x, 
                                         aes(x = grid, y = CI_l), linetype = 2, alpha = 1, 
                                         col = CIcol[i]) + geom_line(data = data_x, aes(x = grid, 
                                                                                        y = CI_r), linetype = 2, alpha = 1, col = CIcol[i])
    if (CItype[i] %in% c("ebar", "all") & !is.null(plotIndex)) 
      temp_plot <- temp_plot + geom_errorbar(data = data_x[plotIndex, 
      ], aes(x = grid, ymin = CI_l, ymax = CI_r), 
      alpha = 1, col = CIcol[i], linetype = 1)
    if (type[i] %in% c("line", "both")) {
      temp_plot <- temp_plot + geom_line(data = data_x, 
                                         aes(x = grid, y = f_p, colour = Sname, linetype = Sname), 
                                         size = lwd[i])
    }
    if (type[i] %in% c("points", "both") & !is.null(plotIndex)) {
      temp_plot <- temp_plot + geom_point(data = data_x[plotIndex, 
      ], aes(x = grid, y = f_p, colour = Sname, shape = Sname), 
      size = pwd[i])
    }
    if (type[i] == "line") {
      col_all <- c(col_all, lcol[i])
      lty_all <- c(lty_all, lty[i])
      pty_all <- c(pty_all, NA)
    }
    else if (type[i] == "both") {
      col_all <- c(col_all, lcol[i])
      lty_all <- c(lty_all, lty[i])
      pty_all <- c(pty_all, pty[i])
    }
    else {
      col_all <- c(col_all, pcol[i])
      lty_all <- c(lty_all, NA)
      pty_all <- c(pty_all, pty[i])
    }
  }
  index <- sort.int(legendGroups, index.return = TRUE)$ix
  temp_plot <- temp_plot + scale_color_manual(values = col_all[index]) + 
    scale_linetype_manual(values = lty_all[index]) + scale_shape_manual(values = pty_all[index]) + 
    scale_x_continuous(labels=function(x) format(round(x),
                                                 big.mark=",",
                                                 decimal.mark="."))+
    scale_y_continuous(labels=scales::comma)+
    guides(colour = guide_legend(title = legendTitle)) + 
    guides(linetype = guide_legend(title = legendTitle)) + 
    guides(shape = guide_legend(title = legendTitle))
  temp_plot <- temp_plot + labs(x = xlabel, y = ylabel) + 
    ggtitle(title)
  return(temp_plot)
}

  
  