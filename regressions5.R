source("rdms2.R")

rodar_modelo_cc2 <- function(df=NULL,
                            ano=NULL,
                            turno=1,
                            geral=FALSE,
                            cargo_m="PREFEITO",
                            cargo_n="PRESIDENTE",
                            orient="Esquerda",
                            partido="PT",
                            pooled=NULL,
                            base2=FALSE,
                            bw=NULL,
                            Y=NULL,
                            cov=TRUE,
                            covsdrop=NULL){
  
  outcome <- enquo(Y)
  
  outtext <- deparse(substitute(Y))
  
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
  
  xnorm= df %>% 
    dplyr::filter(ANO_ELEICAO==ano,
                  NR_TURNO==turno,
                  if(geral==TRUE){is.na(DS_CARGO)==F}else{DS_CARGO==if(ano==2010){cargo_n}else{cargo_m}},
                  if(geral==TRUE){is.na(orientacao)==F}else{if(base2==FALSE){orientacao%in%orient}else{SG_PARTIDO==partido}}) %>% 
    dplyr::filter(duplicated(COD_IBGE)==F) %>% 
    dplyr::mutate(pop_anterior=pop_anterior-cut_off) %>% 
    pull(pop_anterior)
  
  # Define fuzzy
  f <- df %>%
    dplyr::filter(ANO_ELEICAO==ano,
                  NR_TURNO==turno,
                  if(geral==TRUE){is.na(DS_CARGO)==F}else{DS_CARGO==if(ano==2010){cargo_n}else{cargo_m}},
                  if(geral==TRUE){is.na(orientacao)==F}else{if(base2==FALSE){orientacao%in%orient}else{SG_PARTIDO==partido}}) %>%
    dplyr::filter(duplicated(COD_IBGE)==F) %>%
    dplyr::pull(Velocity) 
  
  # f <- df %>% 
  #   dplyr::filter(ANO_ELEICAO==ano,
  #                 NR_TURNO==turno,
  #                 if(geral==TRUE){is.na(DS_CARGO)==F}else{DS_CARGO==if(ano==2010){cargo_n}else{cargo_m}},
  #                 if(geral==TRUE){is.na(orientacao)==F}else{if(base2==FALSE){orientacao%in%orient}else{SG_PARTIDO==partido}}) %>% 
  #   dplyr::filter(duplicated(COD_IBGE)==F) %>% 
  #   dplyr::pull(Velocity) 
  
  # Define cutoff
  c <- c(20000,40000,60000)
  
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
    rdms2(Y=y,
          X=x,
          C=c,
          fuzzy=f,
          xnorm=xnorm,
          hmat=bw/2,
          pooled_opt=paste0("h=",pooled/2),
          covsvec=if(cov==TRUE){covs}else{NULL},
          covs_dropvec=if(cov==TRUE){TRUE}else{NULL}, 
          cluster=cluster)
  
  modelo1<-
    rdms2(Y=y,
          X=x,
          C=c,
          fuzzy=f,
          xnorm=xnorm,
          hmat=bw*2,
          pooled_opt=paste0("h=",pooled*2),
          covsvec=if(cov==TRUE){covs}else{NULL},
          covs_dropvec=if(cov==TRUE){TRUE}else{NULL}, 
          cluster=cluster)
  
  # Construct output
  assign(paste0("results_",deparse(substitute(Y))),
         dplyr::tibble(Cutoff=rep(c(paste0(str_sub(c,1,2),
                                       ",",
                                       str_sub(c,3,5)),"Pooled"),2),
                       Type=if(cov==TRUE){rep("With covariates",8)}else{rep("Without covariates",8)},
                       Model=c(rep("Half-Bw",4),rep("Double-Bw",4)),
                       Bw=c(modelo$H[1,],modelo1$H[1,]),
                       Obs.=c(modelo$Nh[1,]+modelo$Nh[2,],modelo1$Nh[1,]+modelo1$Nh[2,]),
                       Coef.=c(modelo$Coefs[1,],modelo1$Coefs[1,]),
                       SE=c(sqrt(modelo$V[1,]),sqrt(modelo1$V[1,])),
                       P.value=c(modelo$Pv[1,],modelo1$Pv[1,]),
                       Outcome=rep(outtext,8)
                       ),
         envir=globalenv())
  
}

rodar_modelo_cc2(df=rdd,
                ano=2008,
                turno=1,
                geral=T,
                cargo_m="PREFEITO",
                cargo_n="PRESIDENTE",
                orient="Esquerda",
                partido="PT",
                base2=F,
                bw=c(4767.975/2,8244.834/2,13753.391/2),
                pooled=3500,
                Y=participacao,
                cov=F,
                covsdrop=NULL)

# drop <- c("renda_med_2000","Sixty_2000","rural_2000","negro_2000",
#           "radio_2000","televisao_2000","ens_sup_2000","casado_2000",
#           "pea_2000","renda_med_2010","Sixty_2010","rural_2010","negro_2010",
#           "radio_2010","televisao_2010","ens_sup_2010","casado_2010",
#           "pea_2010")

drop=NULL

resultados_part_cc2 <- c()

for(a in c(2008,2010,2012)){
  
  for(t in c("Without covariates","With covariates")){
    
    b<-resultados_part_cc %>% 
      dplyr::filter(Year==a,
                    Type==t) %>% 
      dplyr::pull(Bw) 
    
    rodar_modelo_cc2(df=rdd,
                    ano=a,
                    turno=1,
                    covsdrop=drop,
                    Y=participacao,
                    geral=TRUE,
                    bw=b[1:3],
                    pooled=b[4],
                    cargo_m="PREFEITO",
                    cargo_n="PRESIDENTE",
                    orient="Esquerda",
                    cov=if(t=="Without covariates"){FALSE}else{TRUE})
    
    tmp <- cbind(Year=rep(a,8),
                 get(paste0("results_","participacao")))
    
    resultados_part_cc2 <<- rbind(resultados_part_cc2,tmp)
  }
}

resultados_bn_cc2 <- c()

for(a in c(2008,2010,2012)){
  
  for(t in c("Without covariates","With covariates")){
    
    b<-resultados_bn_cc %>% 
      dplyr::filter(Year==a,
                    Type==t) %>% 
      dplyr::pull(Bw) 
    
    rodar_modelo_cc2(df=rdd,
                     ano=a,
                     turno=1,
                     covsdrop=drop,
                     Y=pct_bn,
                     bw=b[1:3],
                     pooled=b[4],
                     cargo_m="PREFEITO",
                     cargo_n="PRESIDENTE",
                     orient="Esquerda",
                     cov=if(t=="Without covariates"){FALSE}else{TRUE})
    
    tmp <- cbind(Year=rep(a,8),
                 get(paste0("results_","pct_bn")))
    
    resultados_bn_cc2 <<- rbind(resultados_bn_cc2,tmp)
  }
}

resultados_vse_cc2 <- c()

for(a in c(2008,2010,2012)){
  
  for(t in c("Without covariates","With covariates")){
    
    b<-resultados_vse_cc %>% 
      dplyr::filter(Year==a,
                    Type==t) %>% 
      dplyr::pull(Bw) 
    
    rodar_modelo_cc2(df=rdd,
                     ano=a,
                     turno=1,
                     covsdrop=drop,
                     Y=pct_vote,
                     geral=TRUE,
                     bw=b[1:3],
                     pooled=b[4],
                     cargo_m="PREFEITO",
                     cargo_n="PRESIDENTE",
                     orient="Esquerda",
                     cov=if(t=="Without covariates"){FALSE}else{TRUE})
    
    tmp <- cbind(Year=rep(a,8),
                 get(paste0("results_","pct_vote")))
    
    resultados_vse_cc2 <<- rbind(resultados_vse_cc2,tmp)
  }
}

resultados_ncand_cc2  <- c()

for(a in c(2008,2012)){
  
  for(t in c("Without covariates","With covariates")){
    
    b<-resultados_ncand_cc %>% 
      dplyr::filter(Year==a,
                    Type==t) %>% 
      dplyr::pull(Bw) 
    
    rodar_modelo_cc2(df=rdd,
                     ano=a,
                     turno=1,
                     covsdrop=drop,
                     Y=n_cand,
                     bw=b[1:3],
                     pooled=b[4],
                     geral=T,
                     cargo_m="PREFEITO",
                     cargo_n="GOVERNADOR",
                     orient="Esquerda",
                     cov=if(t=="Without covariates"){FALSE}else{TRUE})
    
    tmp <- cbind(Year=rep(a,8),
                 get(paste0("results_","n_cand")))
    
    resultados_ncand_cc2 <<- rbind(resultados_ncand_cc2,tmp)
  }
}

resultados_pt_cc2 <- c()

for(a in c(2008,2010,2012)){
  
  for(t in c("Without covariates","With covariates")){
    
    b<-resultados_pt_cc %>% 
      dplyr::filter(Year==a,
                    Type==t) %>% 
      dplyr::pull(Bw) 
    
    try(rodar_modelo_cc2(df=rdd2,
                     ano=a,
                     turno=1,
                     covsdrop=drop,
                     Y=pct_vote_part,
                     bw=b[1:3],
                     pooled=b[4],
                     cargo_m="VEREADOR",
                     cargo_n="DEPUTADO FEDERAL",
                     partido="PSOL",
                     base2=T,
                     cov=if(t=="Without covariates"){FALSE}else{TRUE}))
    
    tmp <- cbind(Year=rep(a,8),
                 get(paste0("results_","pct_vote_part")))
    
    resultados_pt_cc2 <<- rbind(resultados_pt_cc2,tmp)
  }
}

resultados_receita_cc2 <- c()

for(a in c(2008,2012)){
  
  for(t in c("Without covariates","With covariates")){
    
    b<-resultados_receita_cc %>% 
      dplyr::filter(Year==a,
                    Type==t) %>% 
      dplyr::pull(Bw) 
    
    try(rodar_modelo_cc2(df=rdd2,
                     ano=a,
                     turno=1,
                     covsdrop=drop,
                     Y=receita2,
                     bw=b[1:3],
                     pooled=b[4],
                     cargo_m="VEREADOR",
                     #cargo_n="DEPUTADO FEDERAL",
                     partido="PSOL",
                     base2=T,
                     cov=if(t=="Without covariates"){FALSE}else{TRUE}))
    
    tmp <- cbind(Year=rep(a,8),
                 get(paste0("results_","receita2")))
    
    resultados_receita_cc2 <<- rbind(resultados_receita_cc2,tmp)
  }
}

resultados_votos_jovem_cc2 <- c()

for(a in c(2008,2010,2012)){
  
  for(t in c("Without covariates","With covariates")){
    
    b<-resultados_votos_jovem_cc %>% 
      dplyr::filter(Year==a,
                    Type==t) %>% 
      dplyr::pull(Bw) 
    
    rodar_modelo_cc2(df=rdd3 %>% 
                       dplyr::filter(jovem=="Jovem"),
                     ano=a,
                     turno=1,
                     covsdrop=drop,
                     Y=voto_jovem_pct,
                     geral=TRUE,
                     bw=b[1:3],
                     pooled=b[4],
                     cargo_m="VEREADOR",
                     cargo_n="DEPUTADO FEDERAL",
                     orient="Esquerda",
                     cov=if(t=="Without covariates"){FALSE}else{TRUE})
    
    tmp <- cbind(Year=rep(a,8),
                 get(paste0("results_","voto_jovem_pct")))
    
    resultados_votos_jovem_cc2 <<- rbind(resultados_votos_jovem_cc2,tmp)
  }
}

resultados_receita_jovem_cc2 <- c()

for(a in c(2008,2012)){
  
  for(t in c("Without covariates","With covariates")){
    
    b<-resultados_receita_jovem_cc %>% 
      dplyr::filter(Year==a,
                    Type==t) %>% 
      dplyr::pull(Bw) 
    
    rodar_modelo_cc2(df=rdd3 %>% 
                       dplyr::filter(jovem=="Jovem"),
                     ano=a,
                     turno=1,
                     covsdrop=drop,
                     Y=receita_jovem_pct,
                     geral=TRUE,
                     bw=b[1:3],
                     pooled=b[4],
                     cargo_m="VEREADOR",
                     cargo_n="DEPUTADO FEDERAL",
                     orient="Esquerda",
                     cov=if(t=="Without covariates"){FALSE}else{TRUE})
    
    tmp <- cbind(Year=rep(a,8),
                 get(paste0("results_","receita_jovem_pct")))
    
    resultados_receita_jovem_cc2 <<- rbind(resultados_receita_jovem_cc2,tmp)
  }
}

resultados_sig <- resultados_part_cc2 %>% 
  dplyr::filter(P.value<.1) %>% 
  dplyr::bind_rows(resultados_bn_cc2 %>% 
                     dplyr::filter(P.value<.1),
                   resultados_vse_cc2 %>% 
                     dplyr::filter(P.value<.1),
                   resultados_ncand_cc2 %>% 
                     dplyr::filter(P.value<.1),
                   resultados_pt_cc2 %>% 
                     dplyr::filter(P.value<.1),
                   resultados_receita_cc2 %>% 
                     dplyr::filter(P.value<.1),
                   resultados_votos_jovem_cc2 %>% 
                     dplyr::filter(P.value<.1),
                   resultados_receita_jovem_cc2 %>% 
                     dplyr::filter(P.value<.1)) %>% 
  dplyr::mutate(Outcome=case_when(Outcome=="n_cand"~"N. cand.",
                                  Outcome=="participacao"~"Turnout",
                                  Outcome=="pct_bn"~"Blank or Null votes",
                                  Outcome=="pct_vote"~"Left wing vote share",
                                  Outcome=="pct_vote_part"~"PSOL vote share",
                                  Outcome=="receita2"~"PSOL budget",
                                  Outcome=="voto_jovem_pct"~"Young votes",
                                  Outcome=="receita_jovem_pct"~"Young budget"))
