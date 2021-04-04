# Source code
source("rdmc_adj4.R")

rodar_modelo <- function(df=NULL,
                         ano=NULL,
                         turno=1,
                         geral=FALSE,
                         cargo_m="PREFEITO",
                         cargo_n="PRESIDENTE",
                         orient="Esquerda",
                         partido="PT",
                         base2=FALSE,
                         Y=NULL,
                         cov=TRUE,
                         covsdrop=NULL,
                         fuz=T){
  
  outcome <- enquo(Y)
  
  # Define outcome
  y= df %>% 
    dplyr::filter(ANO_ELEICAO==ano,
                  NR_TURNO==turno,
                  if(geral==TRUE){is.na(DS_CARGO)==F}else{DS_CARGO==if(ano==2010){cargo_n}else{cargo_m}},
                  if(geral==TRUE){is.na(orientacao)==F}else{if(base2==FALSE){orientacao%in%orient}else{SG_PARTIDO==partido}}) %>% 
    dplyr::filter(duplicated(COD_IBGE)==F) %>% 
    pull(!!outcome)
  
  # Define running variable
  x= df %>% 
    dplyr::filter(ANO_ELEICAO==ano,
                  NR_TURNO==turno,
                  if(geral==TRUE){is.na(DS_CARGO)==F}else{DS_CARGO==if(ano==2010){cargo_n}else{cargo_m}},
                  if(geral==TRUE){is.na(orientacao)==F}else{if(base2==FALSE){orientacao%in%orient}else{SG_PARTIDO==partido}}) %>% 
    dplyr::filter(duplicated(COD_IBGE)==F) %>% 
    pull(pop_anterior)
  
  # Define fuzzy
  # f <- df %>% 
  #   dplyr::filter(ANO_ELEICAO==ano,
  #                 NR_TURNO==turno,
  #                 if(geral==TRUE){is.na(DS_CARGO)==F}else{DS_CARGO==if(ano==2010){cargo_n}else{cargo_m}},
  #                 if(geral==TRUE){is.na(orientacao)==F}else{if(base2==FALSE){orientacao%in%orient}else{SG_PARTIDO==partido}}) %>% 
  #   dplyr::filter(duplicated(COD_IBGE)==F) %>% 
  #   dplyr::select(treat20,treat40,treat60) %>% 
  #   as.matrix
  
  f <- df %>%
    dplyr::filter(ANO_ELEICAO==ano,
                  NR_TURNO==turno,
                  if(geral==TRUE){is.na(DS_CARGO)==F}else{DS_CARGO==if(ano==2010){cargo_n}else{cargo_m}},
                  if(geral==TRUE){is.na(orientacao)==F}else{if(base2==FALSE){orientacao%in%orient}else{SG_PARTIDO==partido}}) %>%
    dplyr::filter(duplicated(COD_IBGE)==F) %>%
    dplyr::pull(Velocity)
  
  # Define cutoff
  c <- df %>% 
    dplyr::filter(ANO_ELEICAO==ano,
                  NR_TURNO==turno,
                  if(geral==TRUE){is.na(DS_CARGO)==F}else{DS_CARGO==if(ano==2010){cargo_n}else{cargo_m}},
                  if(geral==TRUE){is.na(orientacao)==F}else{if(base2==FALSE){orientacao%in%orient}else{SG_PARTIDO==partido}}) %>% 
    dplyr::filter(duplicated(COD_IBGE)==F) %>% 
    pull(cut_off)
  
  # Define covariates
  covs2000<- c("renda_med_2000","60_anos_2000","rural_2000","negro_2000",
               "radio_2000","televisao_2000","ens_sup_2000","casado_2000",
               "pea_2000","PRECIP_DIA","TEMP_MED","valor_bolsa",
               "valor_bpc_idoso","PIB","salarios_rais","fibra","fpm_valor")
  
  covs2000 <- setdiff(covs2000,covsdrop)
  
  covs2010<- c("renda_med_2010","60_anos_2010","rural_2010","negro_2010",
               "radio_2010","televisao_2010","ens_sup_2010","casado_2010",
               "pea_2010","PRECIP_DIA","TEMP_MED","valor_bolsa",
               "valor_bpc_idoso","PIB","salarios_rais","fibra","fpm_valor")
  
  covs2010 <- setdiff(covs2010,covsdrop)
  
  covs <- df %>% 
    dplyr::filter(ANO_ELEICAO==ano,
                  NR_TURNO==turno,
                  if(geral==TRUE){is.na(DS_CARGO)==F}else{DS_CARGO==if(ano==2010){cargo_n}else{cargo_m}},
                  if(geral==TRUE){is.na(orientacao)==F}else{if(base2==FALSE){orientacao%in%orient}else{SG_PARTIDO==partido}}) %>% 
    dplyr::filter(duplicated(COD_IBGE)==F) %>% 
    select(if(ano<2010){covs2000}else{covs2010})
  
  # Define cluster errors
  cluster <- df %>% 
    dplyr::filter(ANO_ELEICAO==ano,
                  NR_TURNO==turno,
                  if(geral==TRUE){is.na(DS_CARGO)==F}else{DS_CARGO==if(ano==2010){cargo_n}else{cargo_m}},
                  if(geral==TRUE){is.na(orientacao)==F}else{if(base2==FALSE){orientacao%in%orient}else{SG_PARTIDO==partido}}) %>% 
    dplyr::filter(duplicated(COD_IBGE)==F) %>% 
    pull(regiao)
  
  
  
  
  # teste2<-
  #   rdrobust::rdrobust(y=y,
  #                      x=x,
  #                      c=20000,
  #                      fuzzy=f,
  #                      covs=covs,
  #                      cluster=cluster)
  
  
  
  # Run regression
  modelo<-
    rdmc4(Y=y,
          X=x,
          C=c,
          fuzzy=if(fuz==TRUE){f}else{NULL},
          covsvec=if(cov==TRUE){covs}else{NULL},
          covs_dropvec=if(cov==TRUE){TRUE}else{NULL}, 
          cluster=cluster)
  
  # Construct output
  assign(paste0("results_",deparse(substitute(Y))),
         dplyr::tibble(Cutoff=c(paste0(modelo$C),"Weighted","Pooled"),
                       Type=if(cov==TRUE){rep("With covariates",5)}else{rep("Without covariates",5)},
                       Bw=modelo$H[1,],
                       Obs.=modelo$Nh[1,]+modelo$Nh[2,],
                       Coef.=modelo$Coefs[1,],
                       SE=sqrt(modelo$V[1,]),
                       P.value=modelo$Pv[1,],
                       Weights=c(format(round(modelo$W,3),nsmall=3),"1",".")),
         envir=globalenv())
  
}


resultados_part <- c()

for(a in c(2008,2010,2012)){

  for(t in c(FALSE,TRUE)){
  
  rodar_modelo(df=rdd,
             ano=a,
             turno=1,
             Y=participacao,
             geral=TRUE,
             #fuz=F,
             cargo_m="PREFEITO",
             cargo_n="PRESIDENTE",
             orient="Esquerda",
             cov=t)
  
  tmp <- cbind(Year=rep(a,5),
              get(paste0("results_","participacao")))
  
  resultados_part <<- rbind(resultados_part,tmp)
}
}

# resultados_difpart <- c()
# 
# for(a in c(2008,2010,2012)){
#   
#   rodar_modelo(df=rdd,
#                ano=a,
#                turno=1,
#                Y=dif_part,
#                cargo_m="PREFEITO",
#                cargo_n="PRESIDENTE",
#                orient="Esquerda",
#                cov=T,
#                # covsdrop=c(#"renda_med_2000",
#                #            #"60_anos_2000",
#                #            #"rural_2000",
#                #            #"negro_2000",
#                #            #"radio_2000",
#                #            #"televisao_2000",
#                #            "ens_sup_2000",
#                #            #"casado_2000",
#                #            #"pea_2000",
#                #            "PRECIP_DIA",
#                #            "TEMP_MED",
#                #            "valor_bolsa",
#                #            #"valor_bpc_idoso",
#                #            "PIB",
#                #            "salarios_rais",
#                #            "fibra"
#                #            #"fpm_valor"
#                #            )
#                )
#   
#   tmp <- cbind(Year=rep(a,5),
#                get(paste0("results_","dif_part")))
#   
#   resultados_difpart <<- rbind(resultados_difpart,tmp)
# }

resultados_bn <- c()

for(a in c(2008,2010,2012)){
  
  for(t in c(FALSE,TRUE)){
  
  rodar_modelo(df=rdd,
               ano=a,
               turno=1,
               Y=pct_bn,
               cargo_m="PREFEITO",
               cargo_n="PRESIDENTE",
               orient=c("Esquerda","Direita","Centro"),
               cov=t)
  
  tmp <- cbind(Year=rep(a,5),
               get(paste0("results_","pct_bn")))
  
  resultados_bn <<- rbind(resultados_bn,tmp)
  }
}

# resultados_difbn <- c()
# 
# for(a in c(2008,2010,2012)){
#   
#   rodar_modelo(df=rdd,
#                ano=a,
#                turno=1,
#                Y=dif_bn,
#                cargo_m="PREFEITO",
#                cargo_n="PRESIDENTE",
#                orient="Esquerda",
#                cov=T,
#                covsdrop=c(#"renda_med_2000",
#                  #"60_anos_2000",
#                  #"rural_2000",
#                  #"negro_2000",
#                  #"radio_2000",
#                  #"televisao_2000",
#                  "ens_sup_2000",
#                  #"casado_2000",
#                  #"pea_2000",
#                  "PRECIP_DIA",
#                  "TEMP_MED",
#                  "valor_bolsa",
#                  #"valor_bpc_idoso",
#                  "PIB",
#                  "salarios_rais",
#                  "fibra"
#                  #"fpm_valor"
#                ))
#   
#   tmp <- cbind(Year=rep(a,5),
#                get(paste0("results_","dif_bn")))
#   
#   resultados_difbn <<- rbind(resultados_difbn,tmp)
# }


resultados_vse<- c()

for(a in c(2008,2010,2012)){
  
  for(t in c(FALSE,TRUE)){
  
  rodar_modelo(df=rdd,
               ano=a,
               turno=1,
               Y=pct_vote,
               cargo_m="PREFEITO",
               cargo_n="PRESIDENTE",
               orient="Esquerda",
               cov=t)
  
  tmp <- cbind(Year=rep(a,5),
               get(paste0("results_","pct_vote")))
  
  resultados_vse <<- rbind(resultados_vse,tmp)
  }
}

resultados_ncand<- c()

for(a in c(2008,2012)){
  
  for(t in c(FALSE,TRUE)){
  
  rodar_modelo(df=rdd,
               ano=a,
               turno=1,
               Y=n_cand,
               geral=T,
               cargo_m="PREFEITO",
               cargo_n="GOVERNADOR",
               orient="Esquerda",
               cov=t)
  
  tmp <- cbind(Year=rep(a,5),
               get(paste0("results_","n_cand")))
  
  resultados_ncand <<- rbind(resultados_ncand,tmp)
  }
}

resultados_pt <- c()

for(a in c(2008,2010,2012)){
  
  for(t in c(FALSE,TRUE)){
  
  rodar_modelo(df=rdd2,
               ano=a,
               turno=1,
               Y=pct_vote_part,
               cargo_m="VEREADOR",
               cargo_n="DEPUTADO FEDERAL",
               partido="PT",
               base2=TRUE,
               cov=t)
  
  tmp <- cbind(Year=rep(a,5),
               get(paste0("results_","pct_vote_part")))
  
  resultados_pt <<- rbind(resultados_pt,tmp)
  }
}

# Outras especificações
