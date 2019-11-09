rdmc_adj <- function (Y, X, C, pooled.opt = NULL, hvec = NULL, bvec = NULL, 
                      pvec = NULL, kernelvec = NULL, fuzzy = NULL, plot = FALSE, 
                      verbose = FALSE, data=NULL) 
{
  
  C=data[,C][[1]]
  X=data[,X][[1]]
  Y=data[,Y][[1]]
  
  if (!is.numeric(C)) {
    stop("C has to be numeric")
  }
  if (max(C, na.rm = TRUE) >= max(X, na.rm = TRUE) | min(C, 
                                                         na.rm = TRUE) <= min(X, na.rm = TRUE)) {
    stop("cutoff variable outside range of running variable")
  }

  clist = sort(unique(C))
  cnum = length(clist)
  Xc = X - C
  B = NULL
  V = matrix(0, nrow = cnum + 1, ncol = cnum + 1)
  Nh = NULL
  W = NULL
  Coefs = NULL
  CI = matrix(NA, nrow = 2, ncol = cnum + 1)
  Pv = NULL
  H = NULL
 
  
  fuzzy=data[,fuzzy][[1]]
  
  if(!is.null(pooled.opt)){
    
  vb<-unlist(strsplit(pooled.opt,"[+]"))
  
  vb2= paste0("data[,","'",vb,"'","][[1]]",collapse = "+")
  
  pooled.opt=paste0("covs=",vb2)
  }
  
  
  aux1 = paste0("rdrobust::rdrobust(Y,Xc,", pooled.opt, ",fuzzy=fuzzy)")
  rdr = eval(parse(text = aux1))
  hl = rdr$bws[1, 1]
  hr = rdr$bws[1, 2]
  Nhl = rdr$Nh[1]
  Nhr = rdr$Nh[2]
  Nh = c(Nh, Nhl + Nhr)
  B = c(B, rdr$Estimate[2])
  V[1, 1] = (rdr$se[3])^2
  Coefs = c(Coefs, rdr$Estimate[1])
  CI[, 1] = c(rdr$ci[3, ])
  Pv = c(Pv, rdr$pv[3])
  H = c(H, rdr$bws[1, 1])
  count = 1
  for (c in clist) {
    n.aux = length(Y[C == c & Xc <= hr & Xc >= -hl])
    weight = n.aux/(Nhl + Nhr)
    W = c(W, weight)
    yc = Y[C == c]
    xc = Xc[C == c]
    h = hvec[count]
    b = bvec[count]
    p = pvec[count]
    if (!is.null(kernelvec)) {
      kernel = kernelvec[count]
    }
    else {
      kernel = "tri"
    }
    rdr.tmp = rdrobust::rdrobust(yc, xc, h = h, b = b, p = p, 
                                 kernel = kernel, fuzzy = fuzzy[C == c])
    Nh = c(Nh, rdr.tmp$Nh[1] + rdr.tmp$Nh[2])
    B = c(B, rdr.tmp$Estimate[2])
    V[count + 1, count + 1] = (rdr.tmp$se[3])^2
    Coefs = c(Coefs, rdr.tmp$Estimate[1])
    CI[, count + 1] = c(rdr.tmp$ci[3, ])
    Pv = c(Pv, rdr.tmp$pv[3])
    H = c(H, rdr.tmp$bws[1, 1])
    count = count + 1
  }
  if (verbose == TRUE) {
    cat(summary(rdr))
    cat("\n")
  }
  cat("\n")
  cat(paste0(format("Cutoff", width = 11), format("Coef.", 
                                                  width = 11), format("P-value", width = 19), format("95% CI", 
                                                                                                     width = 20), format("h", width = 10), format("Nh", width = 10), 
             format("Weight", width = 10)))
  cat("\n")
  for (k in 1:cnum) {
    cat(paste0(format(toString(round(clist[k], 3)), width = 11), 
               format(toString(round(Coefs[k + 1], 3)), width = 12), 
               format(toString(round(Pv[k + 1], 3)), width = 12), 
               format(toString(round(CI[1, k + 1], 3)), width = 12), 
               format(toString(round(CI[2, k + 1], 3)), width = 12), 
               format(toString(round(H[k + 1], 3)), width = 12), 
               format(toString(Nh[k + 1]), width = 11), format(toString(round(W[k], 
                                                                              3)), width = 12)))
    cat("\n")
  }
  cat(paste0(format("Pooled", width = 11), format(toString(round(Coefs[1], 
                                                                 3)), width = 12), format(toString(round(Pv[1], 3)), 
                                                                                          width = 12), format(toString(round(CI[1, 1], 3)), width = 12), 
             format(toString(round(CI[2, 1], 3)), width = 12), format(toString(round(H[1], 
                                                                                     3)), width = 12), format(toString(Nh[1]), width = 11), 
             format(" .", width = 12)))
  cat("\n")
  
  results_dt = c()
  
  for (k in 1:cnum) {
    
    r_temp=dplyr::tibble(Cutoff=as.character(round(clist[k], 3)),
                             Coef=round(Coefs[k + 1], 3),
                             pvalue=round(Pv[k + 1], 3),
                             ci_inf=round(CI[1, k + 1], 3),
                             ci_sup=round(CI[2, k + 1], 3),
                             h=round(H[k + 1], 3),
                             Nh=Nh[k + 1],
                             SE=V[k,k],
                             Weight=round(W[k],3))
    
    results_dt <- rbind(results_dt,r_temp)
    
  }
  
  results_dt <<- rbind(results_dt,
                      dplyr::tibble(
                        Cutoff="Pooled",
                        Coef=round(Coefs[1], 3),
                        pvalue=round(Pv[1], 3),
                        ci_inf=round(CI[1,1], 3),
                        ci_sup=round(CI[2,1], 3),
                        h=round(H[1], 3),
                        Nh=Nh[1],
                        SE=V[4,4],
                        Weight=1))
  
  
  if (plot == TRUE) {
    ylim = c(min(CI * 1.3), max(CI) * 1.3)
    xlim = c(min(clist), max(clist))
    par(mfrow = c(1, 2))
    plot(NA, ylim = ylim, xlim = xlim, ylab = "Treatment effect", 
         xlab = "Cutoff")
    polygon(x = c(min(clist), min(clist), max(clist), max(clist)), 
            y = c(CI[1, 1], CI[2, 1], CI[2, 1], CI[1, 1]), border = NA, 
            col = "gray87")
    points(clist, Coefs[-1], col = "darkblue", pch = 16)
    arrows(clist, CI[1, -1], clist, CI[2, -1], length = 0.05, 
           angle = 90, code = 3)
    abline(h = rdr$Estimate[1], col = "gray34")
    abline(h = 0, lty = "dotted")
    legend("bottomright", legend = c("Estimates", "Pooled estimate"), 
           pch = c(16, NA), lty = c(NA, 1), col = c("darkblue", 
                                                    "gray34"), bty = "n", cex = 0.75)
    mtext("Bars are 95% CIs for estimates. \nShaded area is the 95% CI for pooled.", 
          cex = 0.8)
    barplot(W, xlab = "Cutoff", ylab = "Weight", names.arg = clist, 
            space = 1.5)
  }
  output = list(tau = rdr$Estimate[1], se.rb = rdr$se[3], 
                pv.rb = rdr$pv[3], ci.rb.l = rdr$ci[3, 1], ci.rb.r = rdr$ci[3, 
                                                                            2], hl = rdr$bws[1, 1], hr = rdr$bws[1, 2], Nhl = rdr$Nh[1], 
                Nhr = rdr$Nh[2], B = B, V = V, Coefs = Coefs, W = W, 
                Nh = Nh, CI = CI, H = H, Pv = Pv, rdrobust.results = rdr)
  return(output)
}
