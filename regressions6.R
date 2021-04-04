rodar_modelo2 <- function(Y=NULL,
                          df=rdd,
                          ano=NULL,
                          turno=1,
                          cargo_m="PREFEITO",
                          cargo_n="PRESIDENTE",
                          orient="Esquerda",
                          partido="PT",
                          base2=FALSE,
                          geral=FALSE,
                          bw=3500,
                          cut=NULL,
                          cov=TRUE,
                          covsdrop=NULL){
  
  
  
  # Define covariates
  covs2000<- c("renda_med_2000","Sixty_2000","rural_2000","negro_2000",
               "radio_2000","televisao_2000","ens_sup_2000","casado_2000",
               "pea_2000","PRECIP_DIA","TEMP_MED","valor_bolsa",
               "valor_bpc_idoso","PIB","salarios_rais","fibra","fpm_valor")
  
  covs2000 <- setdiff(covs2000,covsdrop)
  
  covs2010<- c("renda_med_2010","Sixty_2010","rural_2010","negro_2010",
               "radio_2010","televisao_2010","ens_sup_2010","casado_2010",
               "pea_2010","PRECIP_DIA","TEMP_MED","valor_bolsa",
               "valor_bpc_idoso","PIB","salarios_rais","fibra","fpm_valor")
  
  covs2010 <- setdiff(covs2010,covsdrop)
  
  
  
  # esp <- if(cov==TRUE){as.formula(paste0(Y,"~pop_anterior+",
  #                                        if(cut==20000){"treat20"}else{if(cut==40000){"treat40"}else{"treat60"}},
  #                                        " | ",
  #                              paste(if(ano<2010){covs2000}else{covs2010},collapse="+")))}else{
  #                                as.formula(paste0(Y,"~pop_anterior+",
  #                                                  if(cut==20000){"treat20"}else{if(cut==40000){"treat40"}else{"treat60"}}
  #                                                  ))}
  
  esp <- if(cov==TRUE){as.formula(paste0(Y,"~pop_anterior+",
                                         "Velocity",
                                         " | ",
                                         paste(if(ano<2010){covs2000}else{covs2010},collapse="+")))}else{
                                           as.formula(paste0(Y,"~pop_anterior+",
                                                             "Velocity"
                                           ))}
  
  modelo<-
    rddapp::rd_est(formula=esp,
                   data=df %>% 
                     dplyr::filter(ANO_ELEICAO==ano,
                                   NR_TURNO==turno,
                                   if(geral==TRUE){is.na(DS_CARGO)==F}else{DS_CARGO==if(ano==2010){cargo_n}else{cargo_m}},
                                   if(geral==TRUE){is.na(orientacao)==F}else{if(base2==FALSE){orientacao%in%orient}else{SG_PARTIDO==partido}}) %>%
                     dplyr::filter(duplicated(COD_IBGE)==F) %>%
                     dplyr::rename(Sixty_2000=`60_anos_2000`,
                                   Sixty_2010=`60_anos_2010`),
                   cutpoint=cut,
                   #bw=bw20fpm,
                   bw=bw,
                   t.design="geq",
                   cluster=df %>%
                     dplyr::filter(ANO_ELEICAO==ano,
                                   NR_TURNO==turno,
                                   if(geral==TRUE){is.na(DS_CARGO)==F}else{DS_CARGO==if(ano==2010){cargo_n}else{cargo_m}},
                                   if(geral==TRUE){is.na(orientacao)==F}else{if(base2==FALSE){orientacao%in%orient}else{SG_PARTIDO==partido}}) %>% 
                     
                     dplyr::filter(duplicated(COD_IBGE)==F) %>%
                     pull(regiao),
                   #verb
    )
  
  modelo1<-
    rddapp::rd_est(formula=esp,
                   data=df %>% 
                     dplyr::filter(ANO_ELEICAO==ano,
                                   NR_TURNO==turno,
                                   if(geral==TRUE){is.na(DS_CARGO)==F}else{DS_CARGO==if(ano==2010){cargo_n}else{cargo_m}},
                                   if(geral==TRUE){is.na(orientacao)==F}else{if(base2==FALSE){orientacao%in%orient}else{SG_PARTIDO==partido}}) %>%
                     dplyr::filter(duplicated(COD_IBGE)==F) %>%
                     dplyr::rename(Sixty_2000=`60_anos_2000`,
                                   Sixty_2010=`60_anos_2010`) %>% 
                     dplyr::mutate(pop_anterior=pop_anterior-cut_off),
                   #cutpoint=cut,
                   #bw=bw20fpm,
                   #bw=bw,
                   t.design="g",
                   cluster=df %>%
                     dplyr::filter(ANO_ELEICAO==ano,
                                   NR_TURNO==turno,
                                   if(geral==TRUE){is.na(DS_CARGO)==F}else{DS_CARGO==if(ano==2010){cargo_n}else{cargo_m}},
                                   if(geral==TRUE){is.na(orientacao)==F}else{if(base2==FALSE){orientacao%in%orient}else{SG_PARTIDO==partido}}) %>% 
                     
                     dplyr::filter(duplicated(COD_IBGE)==F) %>%
                     pull(regiao),
                   #verb
    )
  
  assign(gsub('["]',"",paste0("results2_",deparse(substitute(Y)))),
         dplyr::tibble(Year=rep(ano,12),
                       Model=c(names(modelo$est),names(modelo1$est)),
                       Cutoff=c(rep(paste0(str_sub(cut,1,2),
                                         ",",
                                         str_sub(cut,3,5)),6),
                                rep("Pooled",6)),
                       Type=if(cov==TRUE){rep("With covariates",12)}else{rep("Without covariates",12)},
                       Bw=c(modelo$bw,modelo1$bw),
                       Obs.=c(modelo$obs,modelo1$obs),
                       Coef.=c(modelo$est,modelo1$est),
                       SE=c(modelo$se,modelo1$se),
                       P.value=c(modelo$p,modelo1$p),
                       Outcome=rep(Y,12)),
         envir=globalenv())
  
  fs <- unlist(modelo$model$firststage[4],
               recursive=F)
  
  fs1 <- unlist(modelo1$model$firststage[4],
               recursive=F)
  
  class(fs) <- "lm"
  
  class(fs1) <- "lm"
  
  assign(gsub('["]',"",paste0("fs_",deparse(substitute(Y)))),
         dplyr::bind_rows(
         broom::tidy(fs) %>% 
           dplyr::mutate(term=case_when(str_detect(term,"renda_med")==T~"Income",
                                        str_detect(term,"Sixty")==T~"Pop. over 60 years",
                                        str_detect(term,"rural")==T~"Rural",
                                        str_detect(term,"negro")==T~"Black",
                                        str_detect(term,"radio")==T~"Radio",
                                        str_detect(term,"televisao")==T~"Television",
                                        str_detect(term,"ens_sup")==T~"College",
                                        str_detect(term,"casado")==T~"Married",
                                        str_detect(term,"pea")==T~"Working Pop.",
                                        str_detect(term,"PRECIP_DIA")==T~"Rain (elect. day)",
                                        str_detect(term,"TEMP_MED")==T~"Avg. Temperature",
                                        str_detect(term,"valor_bolsa")==T~"PBF",
                                        str_detect(term,"valor_bpc_idoso")==T~"BPC",
                                        str_detect(term,"PIB")==T~"GDP",
                                        str_detect(term,"valor_bpc_idoso")==T~"BPC",
                                        str_detect(term,"salarios_rais")==T~"Formal Wages",
                                        str_detect(term,"fibra")==T~"Fiber-optic",
                                        str_detect(term,"fpm_valor")==T~"FPM",
                                        TRUE~term),
                         year=rep(ano,if(cov==TRUE){NROW(.)}else{NROW(.)}),
                         Cutoff=rep(paste0(str_sub(cut,1,2),
                                           ",",
                                           str_sub(cut,3,5)),if(cov==TRUE){NROW(.)}else{NROW(.)}),
                         Type=if(cov==TRUE){rep("With covariates",NROW(.))}else{rep("Without covariates",NROW(.))},
                         Outcome=if(cov==TRUE){rep(Y,NROW(.))}else{rep(Y,NROW(.))}),
         broom::tidy(fs1) %>% 
           dplyr::mutate(term=case_when(str_detect(term,"renda_med")==T~"Income",
                                        str_detect(term,"Sixty")==T~"Pop. over 60 years",
                                        str_detect(term,"rural")==T~"Rural",
                                        str_detect(term,"negro")==T~"Black",
                                        str_detect(term,"radio")==T~"Radio",
                                        str_detect(term,"televisao")==T~"Television",
                                        str_detect(term,"ens_sup")==T~"College",
                                        str_detect(term,"casado")==T~"Married",
                                        str_detect(term,"pea")==T~"Working Pop.",
                                        str_detect(term,"PRECIP_DIA")==T~"Rain (elect. day)",
                                        str_detect(term,"TEMP_MED")==T~"Avg. Temperature",
                                        str_detect(term,"valor_bolsa")==T~"PBF",
                                        str_detect(term,"valor_bpc_idoso")==T~"BPC",
                                        str_detect(term,"PIB")==T~"GDP",
                                        str_detect(term,"valor_bpc_idoso")==T~"BPC",
                                        str_detect(term,"salarios_rais")==T~"Formal Wages",
                                        str_detect(term,"fibra")==T~"Fiber-optic",
                                        str_detect(term,"fpm_valor")==T~"FPM",
                                        TRUE~term),
                         year=rep(ano,if(cov==TRUE){NROW(.)}else{NROW(.)}),
                         Cutoff=rep(paste0(0),if(cov==TRUE){NROW(.)}else{NROW(.)}),
                         Type=if(cov==TRUE){rep("With covariates",NROW(.))}else{rep("Without covariates",4)},
                         Outcome=if(cov==TRUE){rep(Y,NROW(.))}else{rep(Y,NROW(.))})),
         envir=globalenv())
  
  fs_sum <- summary(fs)
  
  fs_sum1 <- summary(fs1)
  
  assign(gsub('["]',"",paste0("fspar_",deparse(substitute(Y)))),
         dplyr::bind_rows(
         dplyr::tibble(Obs=NROW(complete.cases(fs$model)),
                       F.stat=case_when(pf(fs_sum$fstatistic[1],
                                           fs_sum$fstatistic[2],
                                           fs_sum$fstatistic[3],
                                           lower.tail=FALSE)<0.01~paste0(format(round(fs_sum$fstatistic[1],2),
                                                                                nsmall=2),"***"),
                                        pf(fs_sum$fstatistic[1],
                                           fs_sum$fstatistic[2],
                                           fs_sum$fstatistic[3],
                                           lower.tail=FALSE)>=0.01&
                                          pf(fs_sum$fstatistic[1],
                                             fs_sum$fstatistic[2],
                                             fs_sum$fstatistic[3],
                                             lower.tail=FALSE)<0.05~paste0(format(round(fs_sum$fstatistic[1],2),
                                                                                  nsmall=2),"**"),
                                        pf(fs_sum$fstatistic[1],
                                           fs_sum$fstatistic[2],
                                           fs_sum$fstatistic[3],
                                           lower.tail=FALSE)>=0.05&
                                          pf(fs_sum$fstatistic[1],
                                             fs_sum$fstatistic[2],
                                             fs_sum$fstatistic[3],
                                             lower.tail=FALSE)<0.1~paste0(format(round(fs_sum$fstatistic[1],2),
                                                                                 nsmall=2),"*"),
                                        TRUE~paste0(format(round(fs_sum$fstatistic[1],2),
                                                           nsmall=2))),
                       DF=fs_sum$df[2],
                       R2=round(fs_sum$r.squared,2),
                       R2.adj=round(fs_sum$adj.r.squared,2),
                       year=ano,
                       Cutoff=cut,
                       Type=if(cov==TRUE){"With covariates"}else{"Without covariates"},
                       Outcome=Y),
         dplyr::tibble(Obs=NROW(complete.cases(fs1$model)),
                       F.stat=case_when(pf(fs_sum1$fstatistic[1],
                                           fs_sum1$fstatistic[2],
                                           fs_sum1$fstatistic[3],
                                           lower.tail=FALSE)<0.01~paste0(format(round(fs_sum1$fstatistic[1],2),
                                                                                nsmall=2),"***"),
                                        pf(fs_sum1$fstatistic[1],
                                           fs_sum1$fstatistic[2],
                                           fs_sum1$fstatistic[3],
                                           lower.tail=FALSE)>=0.01&
                                          pf(fs_sum1$fstatistic[1],
                                             fs_sum1$fstatistic[2],
                                             fs_sum1$fstatistic[3],
                                             lower.tail=FALSE)<0.05~paste0(format(round(fs_sum1$fstatistic[1],2),
                                                                                  nsmall=2),"**"),
                                        pf(fs_sum1$fstatistic[1],
                                           fs_sum1$fstatistic[2],
                                           fs_sum1$fstatistic[3],
                                           lower.tail=FALSE)>=0.05&
                                          pf(fs_sum1$fstatistic[1],
                                             fs_sum1$fstatistic[2],
                                             fs_sum1$fstatistic[3],
                                             lower.tail=FALSE)<0.1~paste0(format(round(fs_sum1$fstatistic[1],2),
                                                                                 nsmall=2),"*"),
                                        TRUE~paste0(format(round(fs_sum1$fstatistic[1],2),
                                                           nsmall=2))),
                       DF=fs_sum1$df[2],
                       R2=round(fs_sum1$r.squared,2),
                       R2.adj=round(fs_sum1$adj.r.squared,2),
                       year=ano,
                       Cutoff=0,
                       Type=if(cov==TRUE){"With covariates"}else{"Without covariates"},
                       Outcome=Y)),
         envir=globalenv())
  
}

# drop <- c("renda_med_2000","Sixty_2000","rural_2000","negro_2000",
#           "radio_2000","televisao_2000","ens_sup_2000","casado_2000",
#           "pea_2000","renda_med_2010","Sixty_2010","rural_2010","negro_2010",
#           "radio_2010","televisao_2010","ens_sup_2010","casado_2010",
#           "pea_2010")

drop=NULL

rodar_modelo2(Y="participacao",
              geral=TRUE,
              ano=2008,
              cut=20000,
              bw=3152.404,
              cov=F,
              covsdrop=drop)

resultados_part2 <- c()
resultados_part2_fs <- c()
resultados_part2_fspar <- c()

for(a in c(2008,2010,2012)){
  
  for(c in c(20000,40000,60000)){
    
    for(t in c("Without covariates","With covariates")){
      
      rodar_modelo2(Y="participacao",
                    geral=TRUE,
                    ano=a,
                    cut=c,
                    covsdrop=drop,
                    bw=resultados_part_cc %>% 
                      dplyr::filter(Year==a,
                                    Cutoff==paste0(str_sub(c,1,2),
                                                   ",",
                                                   str_sub(c,3,5)),
                                    Type==t) %>% 
                      dplyr::pull(Bw),
                    cov=if(t=="With covariates"){TRUE}else{FALSE})
      
      resultados_part2 <<- rbind(resultados_part2,
                                 get(paste0("results2_","participacao")))
      
      resultados_part2_fs <<- rbind(resultados_part2_fs,
                                    get(paste0("fs_","participacao")))
      resultados_part2_fspar <<- rbind(resultados_part2_fspar,
                                       get(paste0("fspar_","participacao")))
    }
  }
  
  resultados_part2 <- resultados_part2 %>% 
    dplyr::distinct()
  
  resultados_part2_fs <- resultados_part2_fs %>% 
    dplyr::distinct()
  
  resultados_part2_fspar <- resultados_part2_fspar %>% 
    dplyr::distinct()
  
}

resultados_bn2 <- c()
resultados_bn2_fs <- c()
resultados_bn2_fspar <- c()

for(a in c(2008,2010,2012)){
  
  for(c in c(20000,40000,60000)){
    
    for(t in c("Without covariates","With covariates")){
      
      rodar_modelo2(Y="pct_bn",
                    geral=FALSE,
                    ano=a,
                    cut=c,
                    covsdrop=drop,
                    cargo_m="PREFEITO",
                    cargo_n="PRESIDENTE",
                    orient=c("Esquerda","Direita","Centro"),
                    turno=1,
                    bw=resultados_bn_cc %>% 
                      dplyr::filter(Year==a,
                                    Cutoff==paste0(str_sub(c,1,2),
                                                   ",",
                                                   str_sub(c,3,5)),
                                    Type==t) %>% 
                      dplyr::pull(Bw),
                    cov=if(t=="With covariates"){TRUE}else{FALSE})
      
      resultados_bn2 <<- rbind(resultados_bn2,
                               get(paste0("results2_","pct_bn")))
      
      resultados_bn2_fs <<- rbind(resultados_bn2_fs,
                                  get(paste0("fs_","pct_bn")))
      
      resultados_bn2_fspar <<- rbind(resultados_bn2_fspar,
                                     get(paste0("fspar_","pct_bn")))
    }
  }
  
  resultados_bn2 <- resultados_bn2 %>% 
    dplyr::distinct()
  
  resultados_bn2_fs <- resultados_bn2_fs %>% 
    dplyr::distinct()
  
  resultados_bn2_fspar <- resultados_bn2_fspar %>% 
    dplyr::distinct()
  
}


resultados_vse2 <- c()
resultados_vse2_fs <- c()
resultados_vse2_fspar <- c()

for(a in c(2008,2010,2012)){
  
  for(c in c(20000,40000,60000)){
    
    for(t in c("Without covariates","With covariates")){
      
      rodar_modelo2(Y="pct_vote",
                    geral=FALSE,
                    ano=a,
                    cut=c,
                    covsdrop=drop,
                    cargo_m="PREFEITO",
                    cargo_n="PRESIDENTE",
                    orient="Esquerda",
                    turno=1,
                    bw=resultados_vse_cc %>% 
                      dplyr::filter(Year==a,
                                    Cutoff==paste0(str_sub(c,1,2),
                                                   ",",
                                                   str_sub(c,3,5)),
                                    Type==t) %>% 
                      dplyr::pull(Bw),
                    cov=if(t=="With covariates"){TRUE}else{FALSE})
      
      resultados_vse2 <<- rbind(resultados_vse2,
                                get(paste0("results2_","pct_vote")))
      
      resultados_vse2_fs <<- rbind(resultados_vse2_fs,
                                   get(paste0("fs_","pct_vote")))
      resultados_vse2_fspar <<- rbind(resultados_vse2_fspar,
                                      get(paste0("fspar_","pct_vote")))
    }
  }
  
  resultados_vse2 <- resultados_vse2 %>% 
    dplyr::distinct()
  
  resultados_vse2_fs <- resultados_vse2_fs %>% 
    dplyr::distinct()
  
  resultados_vse2_fspar <- resultados_vse2_fspar %>% 
    dplyr::distinct()
  
}

resultados_ncand2 <- c()
resultados_ncand2_fs <- c()
resultados_ncand2_fspar <- c()

for(a in c(2008,2012)){
  
  for(c in c(20000,40000,60000)){
    
    for(t in c("Without covariates","With covariates")){
      
      rodar_modelo2(Y="n_cand",
                    geral=TRUE,
                    ano=a,
                    cut=c,
                    covsdrop=drop,
                    cargo_m="PREFEITO",
                    cargo_n="GOVERNADOR",
                    orient="Esquerda",
                    turno=1,
                    bw=resultados_ncand_cc %>% 
                      dplyr::filter(Year==a,
                                    Cutoff==paste0(str_sub(c,1,2),
                                                   ",",
                                                   str_sub(c,3,5)),
                                    Type==t) %>% 
                      dplyr::pull(Bw),
                    cov=if(t=="With covariates"){TRUE}else{FALSE})
      
      resultados_ncand2 <<- rbind(resultados_ncand2,
                                  get(paste0("results2_","n_cand")))
      
      resultados_ncand2_fs <<- rbind(resultados_ncand2_fs,
                                     get(paste0("fs_","n_cand")))
      resultados_ncand2_fspar <<- rbind(resultados_ncand2_fspar,
                                        get(paste0("fspar_","n_cand")))
    }
  }
  
  resultados_ncand2 <- resultados_ncand2 %>% 
    dplyr::distinct()
  
  resultados_ncand2_fs <- resultados_ncand2_fs %>% 
    dplyr::distinct()
  
  resultados_ncand2_fspar <- resultados_ncand2_fspar %>% 
    dplyr::distinct()
  
}

resultados_pt2 <- c()
resultados_pt2_fs <- c()
resultados_pt2_fspar <- c()

for(a in c(2008,2010,2012)){
  
  for(c in c(20000,40000,60000)){
    
    for(t in c("Without covariates","With covariates")){
      
      rodar_modelo2(Y="pct_vote_part",
                    df=rdd2,
                    geral=FALSE,
                    ano=a,
                    cut=c,
                    covsdrop=drop,
                    cargo_m="VEREADOR",
                    cargo_n="DEPUTADO FEDERAL",
                    partido="PSOL",
                    base2=TRUE,
                    turno=1,
                    bw=resultados_pt_cc %>% 
                      dplyr::filter(Year==a,
                                    Cutoff==paste0(str_sub(c,1,2),
                                                   ",",
                                                   str_sub(c,3,5)),
                                    Type==t) %>% 
                      dplyr::pull(Bw),
                    cov=if(t=="With covariates"){TRUE}else{FALSE})
      
      resultados_pt2 <<- rbind(resultados_pt2,
                               get(paste0("results2_","pct_vote_part")))
      
      resultados_pt2_fs <<- rbind(resultados_pt2_fs,
                                  get(paste0("fs_","pct_vote_part")))
      resultados_pt2_fspar <<- rbind(resultados_pt2_fspar,
                                     get(paste0("fspar_","pct_vote_part")))
    }
  }
  
  
  resultados_pt2 <- resultados_pt2 %>% 
    dplyr::distinct()
  
  resultados_pt2_fs <- resultados_pt2_fs %>% 
    dplyr::distinct()
  
  resultados_pt2_fspar <- resultados_pt2_fspar %>% 
    dplyr::distinct()
  
}

resultados_receita2 <- c()
resultados_receita2_fs <- c()
resultados_receita2_fspar <- c()

for(a in c(2008,2012)){
  
  for(c in c(20000,40000,60000)){
    
    for(t in c("Without covariates","With covariates")){
      
      rodar_modelo2(Y="receita2",
                    df=rdd2,
                    geral=FALSE,
                    ano=a,
                    cut=c,
                    covsdrop=drop,
                    cargo_m="VEREADOR",
                    #cargo_n="DEPUTADO FEDERAL",
                    partido="PSOL",
                    base2=TRUE,
                    turno=1,
                    bw=resultados_receita_cc %>% 
                      dplyr::filter(Year==a,
                                    Cutoff==paste0(str_sub(c,1,2),
                                                   ",",
                                                   str_sub(c,3,5)),
                                    Type==t) %>% 
                      dplyr::pull(Bw),
                    cov=if(t=="With covariates"){TRUE}else{FALSE})
      
      resultados_receita2 <<- rbind(resultados_receita2,
                               get(paste0("results2_","receita2")))
      
      resultados_receita2_fs <<- rbind(resultados_receita2_fs,
                                  get(paste0("fs_","receita2")))
      resultados_receita2_fspar <<- rbind(resultados_receita2_fspar,
                                     get(paste0("fspar_","receita2")))
    }
  }
  
  
  resultados_receita2 <- resultados_receita2 %>% 
    dplyr::distinct()
  
  resultados_receita2_fs <- resultados_receita2_fs %>% 
    dplyr::distinct()
  
  resultados_receita2_fspar <- resultados_receita2_fspar %>% 
    dplyr::distinct()
  
}


resultados_jovem_vote2 <- c()
resultados_jovem_vote_fs <- c()
resultados_jovem_vote_fspar <- c()

for(a in c(2008,2010,2012)){
  
  for(c in c(20000,40000,60000)){
    
    for(t in c("Without covariates","With covariates")){
      
      rodar_modelo2(Y="voto_jovem_pct",
                    geral=TRUE,
                    ano=a,
                    cut=c,
                    covsdrop=drop,
                    cargo_m="VEREADOR",
                    cargo_n="DEPUTADO FEDERAL",
                    df=rdd3 %>% 
                      dplyr::filter(jovem=="Jovem"),
                    bw=resultados_votos_jovem_cc %>% 
                      dplyr::filter(Year==a,
                                    Cutoff==paste0(str_sub(c,1,2),
                                                   ",",
                                                   str_sub(c,3,5)),
                                    Type==t) %>% 
                      dplyr::pull(Bw),
                    cov=if(t=="With covariates"){TRUE}else{FALSE})
      
      resultados_jovem_vote2 <<- rbind(resultados_jovem_vote2,
                                 get(paste0("results2_","voto_jovem_pct")))
      
      resultados_jovem_vote_fs <<- rbind(resultados_jovem_vote_fs,
                                    get(paste0("fs_","voto_jovem_pct")))
      resultados_jovem_vote_fspar <<- rbind(resultados_jovem_vote_fspar,
                                       get(paste0("fspar_","voto_jovem_pct")))
    }
  }
  
  resultados_jovem_vote2 <- resultados_jovem_vote2 %>% 
    dplyr::distinct()
  
  resultados_jovem_vote_fs <- resultados_jovem_vote_fs %>% 
    dplyr::distinct()
  
  resultados_jovem_vote_fspar <- resultados_jovem_vote_fspar %>% 
    dplyr::distinct()
  
}


resultados_receita_jovem_vote2 <- c()
resultados_receita_jovem_vote_fs <- c()
resultados_receita_jovem_vote_fspar <- c()

for(a in c(2008,2012)){
  
  for(c in c(20000,40000,60000)){
    
    for(t in c("Without covariates","With covariates")){
      
      rodar_modelo2(Y="receita_jovem_pct",
                    geral=TRUE,
                    ano=a,
                    cut=c,
                    covsdrop=drop,
                    cargo_m="VEREADOR",
                    cargo_n="DEPUTADO FEDERAL",
                    df=rdd3 %>% 
                      dplyr::filter(jovem=="Jovem"),
                    bw=resultados_receita_jovem_cc %>% 
                      dplyr::filter(Year==a,
                                    Cutoff==paste0(str_sub(c,1,2),
                                                   ",",
                                                   str_sub(c,3,5)),
                                    Type==t) %>% 
                      dplyr::pull(Bw),
                    cov=if(t=="With covariates"){TRUE}else{FALSE})
      
      resultados_receita_jovem_vote2 <<- rbind(resultados_receita_jovem_vote2,
                                       get(paste0("results2_","receita_jovem_pct")))
      
      resultados_receita_jovem_vote_fs <<- rbind(resultados_receita_jovem_vote_fs,
                                         get(paste0("fs_","receita_jovem_pct")))
      resultados_receita_jovem_vote_fspar <<- rbind(resultados_receita_jovem_vote_fspar,
                                            get(paste0("fspar_","receita_jovem_pct")))
    }
  }
  
  resultados_receita_jovem_vote2 <- resultados_receita_jovem_vote2 %>% 
    dplyr::distinct()
  
  resultados_receita_jovem_vote_fs <- resultados_receita_jovem_vote_fs %>% 
    dplyr::distinct()
  
  resultados_receita_jovem_vote_fspar <- resultados_receita_jovem_vote_fspar %>% 
    dplyr::distinct()
  
}

# resultados_sig <- resultados_bn2 %>% 
#   dplyr::filter(P.value<.1) %>% 
#   dplyr::bind_rows(resultados_ncand2 %>% 
#                      dplyr::filter(P.value<.1),
#                    resultados_part2 %>% 
#                      dplyr::filter(P.value<.1),
#                    resultados_pt2 %>% 
#                      dplyr::filter(P.value<.1),
#                    resultados_vse2 %>% 
#                      dplyr::filter(P.value<.1),
#                    resultados_receita2 %>% 
#                      dplyr::filter(P.value<.1)) 

resultados_parametrico <- resultados_part2 %>% 
  dplyr::bind_rows(resultados_bn2,
                   resultados_vse2,
                   resultados_ncand2,
                   resultados_pt2,
                   resultados_receita2,
                   resultados_jovem_vote2,
                   resultados_receita_jovem_vote2) %>% 
  dplyr::filter(Model %in% c("Linear","Quadratic","Cubic")) %>% 
  dplyr::mutate(Outcome=case_when(Outcome=="n_cand"~"N. cand.",
                                  Outcome=="participacao"~"Turnout",
                                  Outcome=="pct_bn"~"Blank or Null votes",
                                  Outcome=="pct_vote"~"Left wing vote share",
                                  Outcome=="pct_vote_part"~"PSOL vote share",
                                  Outcome=="receita2"~"PSOL budget",
                                  Outcome=="voto_jovem_pct"~"Young votes",
                                  Outcome=="receita_jovem_pct"~"Young budget"))


resultados_first <- resultados_part2_fs %>% 
  dplyr::bind_rows(resultados_bn2_fs,
                   resultados_vse2_fs,
                   resultados_ncand2_fs,
                   resultados_pt2_fs,
                   resultados_receita2_fs,
                   resultados_jovem_vote_fs,
                   resultados_receita_jovem_vote_fs) 

resultados_first_par <- resultados_part2_fspar %>% 
  dplyr::bind_rows(resultados_bn2_fspar,
                   resultados_vse2_fspar,
                   resultados_ncand2_fspar,
                   resultados_pt2_fspar,
                   resultados_receita2_fspar,
                   resultados_jovem_vote_fspar,
                   resultados_receita_jovem_vote_fspar) 

resultados_first_final<-
resultados_first %>% 
  dplyr::filter(Type=="Without covariates") %>% 
  dplyr::mutate(valor=case_when(p.value<0.001~paste0(format(round(estimate,3),nsmall=3),"***"),
                                p.value>=0.001&p.value<0.05~paste0(format(round(estimate,3),nsmall=3),"**"),
                                p.value>=0.05&p.value<0.1~paste0(format(round(estimate,3),nsmall=3),"*"),
                                TRUE~paste0(format(round(estimate,3),nsmall=3))),
                estimate=round(estimate,3)) %>% 
  dplyr::select(year,Cutoff,Type,Outcome,term,valor) %>% 
  dplyr::distinct() %>% 
  tidyr::spread(term,valor) %>% 
  dplyr::left_join(resultados_first_par %>% 
                     dplyr::filter(Type=="Without covariates") %>% 
                     dplyr::select(year,Cutoff,Type,Outcome,F.stat) %>% 
                     dplyr::mutate(Cutoff=case_when(nchar(Cutoff)!=1~paste0(str_sub(Cutoff,1,2),
                                                                            ",",
                                                                            str_sub(Cutoff,3,5)),
                                                    TRUE~paste0(Cutoff)))) %>% 
  dplyr::mutate(Cutoff=case_when(Cutoff==0~"Pooled",
                                 TRUE~Cutoff),
                Outcome=case_when(Outcome=="n_cand"~"N. cand.",
                                  Outcome=="participacao"~"Turnout",
                                  Outcome=="pct_bn"~"Blank or Null votes",
                                  Outcome=="pct_vote"~"Left wing vote share",
                                  Outcome=="pct_vote_part"~"PSOL vote share",
                                  Outcome=="receita2"~"PSOL budget",
                                  Outcome=="voto_jovem_pct"~"Young votes",
                                  Outcome=="receita_jovem_pct"~"Young budget")) %>% 
  dplyr::rename(Year=year) %>% 
  dplyr::bind_rows(
    resultados_first %>% 
      dplyr::filter(Type=="With covariates") %>% 
      dplyr::mutate(valor=case_when(p.value<0.001~paste0(format(round(estimate,3),nsmall=3),"***"),
                                    p.value>=0.001&p.value<0.05~paste0(format(round(estimate,3),nsmall=3),"**"),
                                    p.value>=0.05&p.value<0.1~paste0(format(round(estimate,3),nsmall=3),"*"),
                                    TRUE~paste0(format(round(estimate,3),nsmall=3))),
                    estimate=round(estimate,3)) %>% 
      dplyr::filter(term %in% c("(Intercept)","Tr","Xl","Xr")) %>% 
      dplyr::select(year,Cutoff,Type,Outcome,term,valor) %>% 
      dplyr::distinct() %>% 
      tidyr::spread(term,valor) %>% 
      dplyr::left_join(resultados_first_par %>% 
                         dplyr::filter(Type=="With covariates") %>% 
                         dplyr::select(year,Cutoff,Type,Outcome,F.stat) %>% 
                         dplyr::mutate(Cutoff=case_when(nchar(Cutoff)!=1~paste0(str_sub(Cutoff,1,2),
                                                                                ",",
                                                                                str_sub(Cutoff,3,5)),
                                                        TRUE~paste0(Cutoff)))) %>% 
      dplyr::mutate(Cutoff=case_when(Cutoff==0~"Pooled",
                                     TRUE~Cutoff),
                    Outcome=case_when(Outcome=="n_cand"~"N. cand.",
                                      Outcome=="participacao"~"Turnout",
                                      Outcome=="pct_bn"~"Blank or Null votes",
                                      Outcome=="pct_vote"~"Left wing vote share",
                                      Outcome=="pct_vote_part"~"PSOL vote share",
                                      Outcome=="receita2"~"PSOL budget",
                                      Outcome=="voto_jovem_pct"~"Young votes",
                                      Outcome=="receita_jovem_pct"~"Young budget")) %>% 
      dplyr::rename(Year=year)
    
  )

  
