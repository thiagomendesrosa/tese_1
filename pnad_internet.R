library(RODBC)
library(srvyr)
library(survey)
library(tidyverse)

db <- RODBC::odbcConnect("db_codeplan",uid=Sys.getenv("matricula"),pwd=Sys.getenv("senha"))

estimativa_internet <- c()

estimativa_bandalarga <- c()

estimativa_celular <- c()



pnad <- RODBC::sqlQuery(db,"select uf,v4609,v4610,pre_wgt,v4617,v4618,v4619, v8005,v2203, v22032,
                        v22006 from pnad.pnad2008")

post.pop <- pnad %>%
  dplyr::group_by(v4609) %>%
  dplyr::summarise(Freq=first(v4609))

sample.pnad <-
  survey::svydesign(id = ~ v4618,
                    strata = ~ v4617,
                    data = pnad,
                    weights = ~ pre_wgt,
                    nest = TRUE)

pnad_design <- 
  survey::postStratify( 
    design = sample.pnad ,
    strata = ~ v4609 ,
    population = post.pop
  )

options( survey.lonely.psu = "adjust")

estimativa <-
as_survey(pnad_design) %>%
  srvyr::filter(v8005>=16) %>%
  srvyr::mutate(internet=factor(case_when(v2203==1~"Sim",
                                   v2203==3~"N?o")),
                banda_larga=factor(case_when(v22032==1~"Sim",
                                             v22032==3~"N?o")),
                celular=factor(case_when(v22006==2~"Sim",
                                         v22006==4~"N?o"))) %>%
  srvyr::group_by(internet) %>%
  srvyr::summarise(n=survey_total(vartype = "ci",na.rm=T),
                   pct=survey_mean(vartype = "ci",na.rm=T)) %>%
  srvyr::mutate(Ano=paste(2008))

estimativa_internet <<- rbind(estimativa_internet, estimativa)

estimativa <-
  as_survey(pnad_design) %>%
  srvyr::filter(v8005>=16) %>%
  srvyr::mutate(internet=factor(case_when(v2203==1~"Sim",
                                          v2203==3~"N?o")),
                banda_larga=factor(case_when(v22032==1~"Sim",
                                             v22032==3~"N?o")),
                celular=factor(case_when(v22006==2~"Sim",
                                         v22006==4~"N?o"))) %>%
  srvyr::group_by(banda_larga) %>%
  srvyr::summarise(n=survey_total(vartype = "ci",na.rm=T),
                   pct=survey_mean(vartype = "ci",na.rm=T)) %>%
  srvyr::mutate(Ano=paste(2008))

estimativa_bandalarga <<- rbind(estimativa_bandalarga, estimativa)

estimativa <-
  as_survey(pnad_design) %>%
  srvyr::filter(v8005>=16) %>%
  srvyr::mutate(internet=factor(case_when(v2203==1~"Sim",
                                          v2203==3~"N?o")),
                banda_larga=factor(case_when(v22032==1~"Sim",
                                             v22032==3~"N?o")),
                celular=factor(case_when(v22006==2~"Sim",
                                         v22006==4~"N?o"))) %>%
  srvyr::group_by(celular) %>%
  srvyr::summarise(n=survey_total(vartype = "ci",na.rm=T),
                   pct=survey_mean(vartype = "ci",na.rm=T)) %>%
  srvyr::mutate(Ano=paste(2008))

estimativa_celular <<- rbind(estimativa_celular, estimativa)





anos <- c(2009,2011:2015)

for(i in anos){
  
  pnad <- RODBC::sqlQuery(db,paste0("select uf,v4609,v4610,pre_wgt,v4617,v4618,v4619, v8005,v06111, v06112 from pnad.pnad",i))
  
  post.pop <- pnad %>%
    dplyr::group_by(v4609) %>%
    dplyr::summarise(Freq=first(v4609))
  
  sample.pnad <-
    survey::svydesign(id = ~ v4618,
                      strata = ~ v4617,
                      data = pnad,
                      weights = ~ pre_wgt,
                      nest = TRUE)
  
  pnad_design <- 
    survey::postStratify( 
      design = sample.pnad ,
      strata = ~ v4609 ,
      population = post.pop
    )
  
  options( survey.lonely.psu = "adjust")
  
  estimativa <-
    as_survey(pnad_design) %>%
    srvyr::filter(v8005>=16) %>%
    srvyr::mutate(internet=factor(case_when(v06111==1~"Sim",
                                            v06111==3~"N?o")),
                  celular=factor(case_when(v06112==2~"Sim",
                                           v06112==4~"N?o"))) %>%
    srvyr::group_by(internet) %>%
    srvyr::summarise(n=survey_total(vartype = "ci",na.rm=T),
                     pct=survey_mean(vartype = "ci",na.rm=T)) %>%
    srvyr::mutate(Ano=paste(i))
  
estimativa_internet <<- rbind(estimativa_internet, estimativa)
  

  estimativa <-
    as_survey(pnad_design) %>%
    srvyr::filter(v8005>=16) %>%
    srvyr::mutate(internet=factor(case_when(v06111==1~"Sim",
                                            v06111==3~"N?o")),
                  celular=factor(case_when(v06112==1~"Sim",
                                           v06112==3~"N?o"))) %>%
    srvyr::group_by(celular) %>%
    srvyr::summarise(n=survey_total(vartype = "ci",na.rm=T),
                     pct=survey_mean(vartype = "ci",na.rm=T)) %>%
    srvyr::mutate(Ano=paste(i))
  
  estimativa_celular <<- rbind(estimativa_celular, estimativa)
  
}


# Pegar informa??es da PNAD cont?nua

for(i in 2016:2019){

pnad <- RODBC::sqlQuery(db,paste0("select V2009, S01021, S01029,S01030A1,S01030A3,S07001,S07006,S07007,UPA, Estrato, V1027, V1029, posest from pnadc.pnada",i,"sup_tic"))

post.pop <- pnad %>%
  dplyr::group_by(posest) %>%
  dplyr::summarise(Freq=first(V1029))

sample.pnad <-
  survey::svydesign(id = ~ UPA,
                    strata = ~ Estrato,
                    data = pnad,
                    weights = ~ V1027,
                    nest = TRUE)

pnad_design <- 
  survey::postStratify( 
    design = sample.pnad ,
    strata = ~ posest ,
    population = post.pop
  )

options( survey.lonely.psu = "adjust")

as_survey(pnad_design) %>%
  srvyr::mutate(count=1) %>%
  srvyr::summarise(n=survey_total(count))


estimativa <-
  as_survey(pnad_design) %>%
  srvyr::filter(V2009>=16) %>%
  srvyr::mutate(internet=factor(case_when(S07001==1~"Sim",
                                          S07001==2~"N?o")),
                celular=factor(case_when(S07006==1~"Sim",
                                         S07006==2~"N?o"))) %>%
  srvyr::group_by(internet) %>%
  srvyr::summarise(n=survey_total(vartype = "ci",na.rm=T),
                   pct=survey_mean(vartype = "ci",na.rm=T)) %>%
  srvyr::mutate(Ano=paste(i))

estimativa_internet <<- rbind(estimativa_internet, estimativa)


estimativa <-
  as_survey(pnad_design) %>%
  srvyr::filter(V2009>=16) %>%
  srvyr::mutate(internet=factor(case_when(S07001==1~"Sim",
                                          S07001==2~"N?o")),
                celular=factor(case_when(S07006==1~"Sim",
                                         S07006==2~"N?o"))) %>%
  srvyr::group_by(celular) %>%
  srvyr::summarise(n=survey_total(vartype = "ci",na.rm=T),
                   pct=survey_mean(vartype = "ci",na.rm=T)) %>%
  srvyr::mutate(Ano=paste(i))

estimativa_celular <<- rbind(estimativa_celular, estimativa)

}


final <- estimativa_internet %>%
  dplyr::rename(resposta=internet) %>%
  dplyr::mutate(var="Internet") %>%
  dplyr::bind_rows(estimativa_celular %>%
                     dplyr::rename(resposta=celular) %>%
                     dplyr::mutate(var="Cell phone"))

write.table(final,"bases/internet.csv",row.names = F,dec = ",",
            fileEncoding = "latin1",sep = ";")


final %>% 
dplyr::filter(resposta=="Sim") %>%
  ggplot(aes(x=Ano,y=pct,color=var, linetype=var), size=1)+
  geom_line()+
  geom_point()+
  geom_text(aes(y=pct+0.01, label=paste0(round(pct*100,2),"%")),
            show.legend = F) +
  scale_y_continuous(labels=scales::percent)+
  scale_x_discrete(breaks = 2008:2019) +
  labs(x="Year",
       y="%",
       caption = "Source: National Household Survey (PNAD e PNADc)\nObs1.: Usage of internet in the last 3 months (survey date month as reference)\nObs2.: Cell phone ownership for personal use\nObs.3: No value for 2010 (Census year)")+
  scale_color_discrete(name="Usage of:")+
  scale_linetype_discrete(name="Usage of:")+
  theme(panel.background = element_blank(),
        axis.title = element_text(face="bold",size=12),
        axis.text = element_text(size = 10),
        legend.text = element_text(size=10),
        legend.key=element_blank(),
        plot.caption = element_text(hjust = 0,size = 8))
