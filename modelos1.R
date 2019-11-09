#setwd("F:/OneDrive/Doutorado/Tese/Bases/")
setwd("D:/Backup Thiago/Thiago/Bases")
load("paper1.rda")

library(rdmulti)
library(rdrobust)
library(tidyverse)
library(lubridate)
library(sf)

back_2008 <- backhaul %>%
  # Retirar da análise municípios que já possuiam infraestrutura anteriormente
  dplyr::filter(Situao %in% c("Atendido","Não atendido")) %>%
  # Agrupar por município
  dplyr::group_by(fk_cod_municipio) %>% 
  # Recuperar a velocidade do backhaul para todas as linhas,
  dplyr::mutate(backhaul=max(backhaul,na.rm=T)) %>% 
  # Desagrupar
  dplyr::ungroup() %>% 
  # Ficar somente com a informação de um período.
  # Queremos saber somente a velocidade e o ano de implementação do backhaul
  dplyr::filter(ano==2009) %>% 
  # Colocal como colunas o ano de implementação do backhaul com a velocidade
  tidyr::spread(backhaul_ano, backhaul,sep="_") %>%
  # Colocar como zero quem não teve backhaul implementado em 2008
  # e, caso contrário, a velocidade implantada
  dplyr::mutate(Velocity=case_when(is.na(backhaul_ano_2008)==T~0,
                                     TRUE~as.numeric(backhaul_ano_2008)),
                COD_IBGE=as.character(fk_cod_municipio)) %>%
  # Ficar somente com a informação de velocidade e o código do município
  dplyr::select(COD_IBGE,Velocity) %>%
  # Recuperar as informações de população
  dplyr::left_join(populacao %>%
                     dplyr::select(COD_IBGE,pop_2007)) %>%
  # Renomear a variável de população
  dplyr::rename(pop_anterior=pop_2007) %>%
  # Retirar os municípios sem população (criados após 2007)
  dplyr::filter(is.na(pop_anterior)==F) %>%
  # Criar as faixas de população, os cutoffs, um marcador para internet e o ano
  dplyr::mutate(faixa_pop=cut(pop_anterior,
                              c(-Inf,20000,40000,60000,Inf),
                              labels = c("Up to 20k",
                                         "from 20k to 40k",
                                         "from 40k to 60k",
                                         "Above 60k"),
                              ordered_result = T),
                cut_off=case_when(pop_anterior<=30000~20000,
                                  pop_anterior %in% c(30001:50000)~40000,
                                  pop_anterior>=50001~60000),
                cut_off2=case_when(pop_anterior %in% c(15000:25000)~20000,
                                   pop_anterior %in% c(35000:45000)~40000,
                                   pop_anterior %in% c(55000:65000)~60000),
                internet=case_when(Velocity>0~1,
                                   TRUE~0),
                year=2008)

# Replicar a lógica acima para 2009, com a diferença de 
# considerar a situação de momento no município, ou seja,
# caso o backhaul tenha sido implementado em 2008,
# considerar-se-á a velocidade daquele momento de implantação

back_2009 <- backhaul %>%
  dplyr::filter(Situao %in% c("Atendido","Não atendido")) %>%
  #dplyr::filter(!(Situao=="Atendido"&is.na(backhaul)==T)) %>%
  dplyr::group_by(fk_cod_municipio) %>% 
  dplyr::mutate(backhaul=max(backhaul,na.rm=T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(ano==2009) %>% 
  tidyr::spread(backhaul_ano, backhaul,sep="_") %>%
  dplyr::mutate(Velocity=case_when(is.na(backhaul_ano_2008)==T&
                                     is.na(backhaul_ano_2009)==T~0,
                                     TRUE~rowSums(.[,c("backhaul_ano_2008",
                                                    "backhaul_ano_2009")],
                                                  na.rm=T)),
                COD_IBGE=as.character(fk_cod_municipio)) %>%
  dplyr::select(COD_IBGE,Velocity) %>%
  dplyr::left_join(populacao %>%
                     dplyr::select(COD_IBGE,pop_2008)) %>%
  dplyr::rename(pop_anterior=pop_2008) %>%
  dplyr::filter(is.na(pop_anterior)==F) %>%
  dplyr::distinct() %>% 
  dplyr::mutate(faixa_pop=cut(pop_anterior,
                              c(-Inf,20000,40000,60000,Inf),
                              labels = c("Up to 20k",
                                         "from 20k to 40k",
                                         "from 40k to 60k",
                                         "Above 60k"),
                              ordered_result = T),
                cut_off=case_when(pop_anterior <=30000~20000,
                                  pop_anterior %in% c(30001:50000)~40000,
                                  pop_anterior>=50001~60000),
                cut_off2=case_when(pop_anterior %in% c(15000:25000)~20000,
                                   pop_anterior %in% c(35000:45000)~40000,
                                   pop_anterior %in% c(55000:65000)~60000),
                internet=case_when(Velocity>0~1,
                                   TRUE~0),
                year=2009)

# Replicar para 2010
back_2010 <-backhaul %>%
  dplyr::filter(Situao %in% c("Atendido","Não atendido")) %>%
  #dplyr::filter(!(Situao=="Atendido"&is.na(backhaul)==T)) %>%
  dplyr::group_by(fk_cod_municipio) %>% 
  dplyr::mutate(backhaul=max(backhaul,na.rm=T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(ano==2009) %>% 
  tidyr::spread(backhaul_ano, backhaul,sep="_") %>%
  dplyr::mutate(Velocity=case_when(is.na(backhaul_ano_2008)==T&
                                       is.na(backhaul_ano_2009)==T&
                                       is.na(backhaul_ano_2010)==T~0,
                                     TRUE~rowSums(.[,c("backhaul_ano_2008",
                                                       "backhaul_ano_2009",
                                                       "backhaul_ano_2010")],
                                                  na.rm=T)),
                COD_IBGE=as.character(fk_cod_municipio)) %>%
  dplyr::select(COD_IBGE,Velocity) %>%
  dplyr::left_join(populacao %>%
                     dplyr::select(COD_IBGE,pop_2009)) %>%
  dplyr::rename(pop_anterior=pop_2009) %>%
  dplyr::filter(is.na(pop_anterior)==F) %>%
  dplyr::distinct() %>%
  dplyr::filter(is.na(Velocity)==F) %>%
  dplyr::mutate(faixa_pop=cut(pop_anterior,
                              c(-Inf,20000,40000,60000,Inf),
                              labels = c("Up to 20k",
                                         "from 20k to 40k",
                                         "from 40k to 60k",
                                         "Above 60k"),
                              ordered_result = T),
                cut_off=case_when(pop_anterior <=30000~20000,
                                  pop_anterior %in% c(30001:50000)~40000,
                                  pop_anterior>=50001~60000),
                cut_off2=case_when(pop_anterior %in% c(15000:25000)~20000,
                                   pop_anterior %in% c(35000:45000)~40000,
                                   pop_anterior %in% c(55000:65000)~60000),
                internet=case_when(Velocity>0~1,
                                   TRUE~0),
                year=2010) 

# Replicar para 2011
back_2011 <- backhaul %>%
  dplyr::filter(Situao %in% c("Atendido","Não atendido")) %>%
  #dplyr::filter(!(Situao=="Atendido"&is.na(backhaul)==T)) %>%
  dplyr::group_by(fk_cod_municipio) %>% 
  dplyr::mutate(backhaul=max(backhaul,na.rm=T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(ano==2009) %>% 
  tidyr::spread(backhaul_ano, backhaul,sep="_") %>%
  dplyr::mutate(Velocity=case_when(is.na(backhaul_ano_2008)==T&
                                       is.na(backhaul_ano_2009)==T&
                                       is.na(backhaul_ano_2010)==T&
                                       is.na(backhaul_ano_2011)==T~0,
                                     TRUE~rowSums(.[,c("backhaul_ano_2008",
                                                       "backhaul_ano_2009",
                                                       "backhaul_ano_2010",
                                                       "backhaul_ano_2011")],
                                                  na.rm=T)),
                COD_IBGE=as.character(fk_cod_municipio)) %>%
  dplyr::select(COD_IBGE,Velocity) %>%
  dplyr::left_join(populacao %>%
                     dplyr::select(COD_IBGE,pop_2010)) %>%
  dplyr::rename(pop_anterior=pop_2010) %>%
  dplyr::filter(is.na(pop_anterior)==F) %>%
  dplyr::distinct() %>%
  dplyr::filter(is.na(Velocity)==F) %>%
  dplyr::mutate(faixa_pop=cut(pop_anterior,
                              c(-Inf,20000,40000,60000,Inf),
                              labels = c("Up to 20k",
                                         "from 20k to 40k",
                                         "from 40k to 60k",
                                         "Above 60k"),
                              ordered_result = T),
                cut_off=case_when(pop_anterior <=30000~20000,
                                  pop_anterior %in% c(30001:50000)~40000,
                                  pop_anterior>=50001~60000),
                cut_off2=case_when(pop_anterior %in% c(15000:25000)~20000,
                                   pop_anterior %in% c(35000:45000)~40000,
                                   pop_anterior %in% c(55000:65000)~60000),
                internet=case_when(Velocity>0~1,
                                   TRUE~0),
                year=2011)

# Replicar para 2012
back_2012 <- backhaul %>%
  dplyr::filter(Situao %in% c("Atendido","Não atendido")) %>%
  #dplyr::filter(!(Situao=="Atendido"&is.na(backhaul)==T)) %>%
  dplyr::group_by(fk_cod_municipio) %>% 
  dplyr::mutate(backhaul=max(backhaul,na.rm=T)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(ano==2009) %>% 
  tidyr::spread(backhaul_ano, backhaul,sep="_") %>%
  dplyr::mutate(Velocity=case_when(is.na(backhaul_ano_2008)==T&
                                       is.na(backhaul_ano_2009)==T&
                                       is.na(backhaul_ano_2010)==T&
                                       is.na(backhaul_ano_2011)==T&
                                       is.na(backhaul_ano_2012)==T~0,
                                     TRUE~rowSums(.[,c("backhaul_ano_2008",
                                                       "backhaul_ano_2009",
                                                       "backhaul_ano_2010",
                                                       "backhaul_ano_2011",
                                                       "backhaul_ano_2012")],
                                                  na.rm=T)),
                COD_IBGE=as.character(fk_cod_municipio)) %>%
  dplyr::select(COD_IBGE,Velocity) %>%
  dplyr::left_join(populacao %>%
                     dplyr::select(COD_IBGE,pop_2011)) %>%
  dplyr::rename(pop_anterior=pop_2011) %>%
  dplyr::filter(is.na(pop_anterior)==F) %>%
  dplyr::distinct() %>%
  dplyr::filter(is.na(Velocity)==F) %>%
  dplyr::mutate(faixa_pop=cut(pop_anterior,
                              c(-Inf,20000,40000,60000,Inf),
                              labels = c("Up to 20k",
                                         "from 20k to 40k",
                                         "from 40k to 60k",
                                         "Above 60k"),
                              ordered_result = T),
                cut_off=case_when(pop_anterior <=30000~20000,
                                  pop_anterior %in% c(30001:50000)~40000,
                                  pop_anterior>=50001~60000),
                cut_off2=case_when(pop_anterior %in% c(15000:25000)~20000,
                                   pop_anterior %in% c(35000:45000)~40000,
                                   pop_anterior %in% c(55000:65000)~60000),
                internet=case_when(Velocity>0~1,
                                   TRUE~0),
                year=2012)

# Recuperar informação do ano de implantação do backhaul
back_ano <- backhaul %>% 
  dplyr::group_by(fk_cod_municipio) %>% 
  dplyr::summarise(back_ano=max(backhaul_ano,na.rm=T)) %>% 
  dplyr::rename(COD_IBGE=fk_cod_municipio) %>% 
  dplyr::mutate(COD_IBGE=as.character(COD_IBGE),
                back_ano=case_when(back_ano<0~0,
                                   TRUE~back_ano))

# Juntar as informações do backhaul de 2008, 2010 e 2012
back_junto <- back_2008 %>% 
  dplyr::bind_rows(back_2010,
                   back_2012) %>% 
  # Pegar as informações do ano de implantação
  dplyr::left_join(back_ano)

# Pegar outcomes
# Pegar resultados para candidatos
cand_orient <- candidato %>%
  # Renomear variáveis de identificação do município
  dplyr::rename(CD_MUNICIPIO=SG_UE,
                NM_MUNICIPIO=DS_UE) %>% 
  # Recuperar código dos municípios do IBGE
  dplyr::left_join(censolegis %>%
                     dplyr::mutate(CD_MUNICIPIO=as.character(CD_MUNICIPIO))) %>% 
  # Remover informações de canditados fora do nível municipal
  # (Deputados estaduais, deputadores federais, presidente, senadores e governadores)
  na.omit %>% 
  # Agrupar por ano da eleioção, município, cargo e orientação
  dplyr::group_by(ANO_ELEICAO,COD_IBGE,DS_CARGO,orientacao) %>%
  # Verificar o total de candidatos
  dplyr::summarise(n=sum(n,na.rm=T)) %>%
  # Verificar o percentual de candidatos
  dplyr::mutate(pct_cand=n/sum(n)) %>% 
  # Desagrupar
  dplyr::ungroup()

# Definir o percentual de votação nos candidatos
vote_share <- votacao %>%
  # Desagrupar informações
  dplyr::ungroup() %>% 
  # Transformar a identificaçãod o município em caracter para o join
  dplyr::mutate(CD_MUNICIPIO=as.character(CD_MUNICIPIO)) %>% 
  # Pegar o cod_IBGE para os municípios
  dplyr::left_join(censolegis %>%
                     dplyr::mutate(CD_MUNICIPIO=as.character(CD_MUNICIPIO))) %>% 
  # Retirar informações de votantes do exterior
  na.omit %>% 
  # Agrupar pelo ano da eleição, município, cargo e orientação
  dplyr::group_by(ANO_ELEICAO,COD_IBGE,DS_CARGO, orientacao) %>%
  # Verificar o total de votos
  dplyr::summarise(votos=sum(votos,na.rm=T)) %>%
  # Verificar o percentual de votos
  dplyr::mutate(pct_vote=votos/sum(votos)) %>%
  # Desagrupar
  dplyr::ungroup()

# Jutar os dois outcomes
outcomes_cad_vote <- vote_share %>%
  dplyr::left_join(cand_orient)

# Pegar as informações de participação e juntar a coisa toda
eleic_out <- outcomes_cad_vote %>%
  # Filtrar para os anos de interesse
  dplyr::filter(ANO_ELEICAO %in% c(2004,2006,2008,2010,2012)) %>%
  # Pegar as informações de participação
  dplyr::left_join(resultado %>%
                     # Filtrar para os anos de interesse
                     dplyr::filter(ANO_ELEICAO %in% c(2004,2006,2008,2010,2012)) %>%
                     # Desagrupar a base
                     dplyr::ungroup() %>%
                     # Transformar o código do município em caracter para o join
                     dplyr::mutate(CD_MUNICIPIO=as.character(CD_MUNICIPIO)) %>% 
                     # Pegar os códigos dos municípios do IBGE
                     dplyr::left_join(censolegis %>%
                                        dplyr::mutate(CD_MUNICIPIO=as.character(CD_MUNICIPIO))) %>% 
                     # Retirar informações dos votantes no exterior
                     na.omit) %>%
  # Selecionar as variáveis de interesse
  dplyr::select(ANO_ELEICAO,COD_IBGE,DS_CARGO,orientacao,pct_cand,
                pct_vote,participacao,pct_nulos,pct_brancos) %>%
  # Organizar por município, cargo, orientação e ano da eleição
  dplyr::arrange(COD_IBGE,DS_CARGO,orientacao,ANO_ELEICAO) %>%
  # Agrupar por município, cargo e orientação
  dplyr::group_by(COD_IBGE,DS_CARGO,orientacao) %>%
  # Calcular novas variáveis de interesse
  dplyr::mutate(
    # Percentual de brancos + nulos
    pct_bn = pct_brancos+pct_nulos,
    # Diferença, em pontos percentuais, da votação atual e da passada
    dif_vote=pct_vote-lag(pct_vote),
    # Diferença, em pontos percentuais, da participação atual e da passada
    dif_part=participacao-lag(participacao),
    # Diferença, em pontos percentuais, da votação nula atual e da passada
    dif_nulos=pct_nulos-lag(pct_nulos),
    # Diferença, em pontos percentuais, da votação branca atual e da passada
    dif_brancos=pct_brancos-lag(pct_brancos),
    # Diferença, em pontos percentuais, da votação branca+nula atual e da passada
    dif_bn=pct_bn-lag(pct_bn)) %>%
  # Desagrupar
  dplyr::ungroup() %>% 
  # Deixar código do IBGE como caracter
  dplyr::mutate(COD_IBGE=as.character(COD_IBGE))

# Coletar informações do censo e do backhaul
# juntando com os outcomes

rdd <- eleic_out %>%
  # Pegar informações do censo
  dplyr::left_join(censo %>%
                     dplyr::mutate(COD_IBGE=as.character(COD_IBGE)) %>%
                     dplyr::select(-localidade)) %>%
  # Pegar informações do backhaul
  dplyr::left_join(back_junto,
                   by=c("COD_IBGE"="COD_IBGE",
                        "ANO_ELEICAO"="year")) %>%
  # Manter apenar os municípios passíveis de análise (participantes do Backhaul e os sem atendimento)
  dplyr::filter(is.na(Velocity)==F) %>%
  # Criar códigos para as grandes regiões do país
  dplyr::mutate(regiao=case_when(substr(COD_IBGE,1,1)==1~"Norte",
                                 substr(COD_IBGE,1,1)==2~"Nordeste",
                                 substr(COD_IBGE,1,1)==3~"Sudeste",
                                 substr(COD_IBGE,1,1)==4~"Sul",
                                 substr(COD_IBGE,1,1)==5~"Centro-oeste"))


# Coletar informações meteorológicas das datas das eleições
# Carregar informação com as datas das eleições
dt_eleicao <- readxl::read_excel("D:/Backup Thiago/Thiago/Bases/TSE/data_eleicao.xlsx") %>%
  dplyr::mutate(ANO=year(DATA))

# Pegar a data de referência das eleiçõeo
dt_eleicao_anos <- dt_eleicao %>%
  dplyr::filter(ANO %in% c(2004,2006,2008,2010,2012)) %>% 
  dplyr::select(DATA,ANO) %>% 
  dplyr::group_by(ANO) %>% 
  dplyr::summarise(DATA_ELEC=first(DATA)) %>% 
  dplyr::ungroup()

# Calcular a temperatura média e a soma de precipitação dos 7 dias anteriores e
# posteriores a eleição
estacao <- dados_estacao %>%
  dplyr::mutate(DATA=lubridate::dmy(DATA),
                ANO=year(DATA)) %>% 
  dplyr::filter(ANO %in% c(2008,2010,2012)) %>% 
  dplyr::left_join(dt_eleicao_anos) %>% 
  dplyr::mutate(intervalo=interval(DATA,DATA_ELEC)%/%days(1)) %>%
  dplyr::filter(intervalo %in% c(-7:7)) %>%
  dplyr::mutate_at(vars(PRECIP_TOT:LONG),
                   list(~as.numeric(gsub(",",".",.)))) %>%
  dplyr::group_by(COD_WMO,ANO,LAT,LONG) %>%
  dplyr::summarise(PRECIP=sum(PRECIP_TOT,na.rm=T),
                   PRECIP_ANTES=sum(PRECIP_TOT[intervalo<0],na.rm=T),
                   PRECIP_DEPOIS=sum(PRECIP_TOT[intervalo>0],na.rm=T),
                   PRECIP_DIA=sum(PRECIP_TOT[intervalo==0],na.rm=T),
                   TEMP_MED=mean(TEMP_MED)) %>%
  na.omit %>% 
  st_as_sf(coords = c("LONG", "LAT"), 
           crs = 4674)

# teste <- mapa %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(COD_WMO=df.SP[st_nearest_feature(mapa$centroide,df.SP),]$COD_WMO) %>%
#   dplyr::left_join(estacao_2008)

mapa <- brazilmaps::get_brmap("City") %>%
  #dplyr::group_by(City) %>%
  #dplyr::mutate(centroide=sf::st_centroid(geometry))
  sf::st_join(estacao %>% 
                dplyr::filter(ANO==2008),join=st_nearest_feature)

# Plotar um mapa com a precipitação
mapa %>%
  ggplot()+
  geom_sf(aes(fill=PRECIP_DIA))

# Levar informações metereológicas para base do RDD
rdd <- rdd %>%
  dplyr::left_join(mapa %>%
                     dplyr::select(City,PRECIP_DIA,PRECIP,
                                   PRECIP_ANTES,PRECIP_DEPOIS,
                                   TEMP_MED) %>%
                     dplyr::mutate(COD_IBGE=as.character(City)) %>% 
                     st_set_geometry(NULL))

# Pegar informações do Bolsa família, BPC e ipea
rdd2 <- rdd %>%
  dplyr::mutate(COD_IBGE2=substr(COD_IBGE,1,6)) %>%
  dplyr::left_join(BOLSA_anual %>%
                     dplyr::mutate(Ano=as.numeric(Ano)) %>% 
                     dplyr::filter(Ano %in% c(2008,2010,2012)),
                   by=c("COD_IBGE2"="COD_IBGE","ANO_ELEICAO"="Ano")) %>%
  dplyr::left_join(BPC_anual%>%
                     dplyr::mutate(Ano=as.numeric(Ano)) %>%
                     dplyr::filter(Ano %in% c(2008,2010,2012)),
                   by=c("COD_IBGE2"="COD_IBGE","ANO_ELEICAO"="Ano")) %>%
  dplyr::left_join(ipea %>%
                     dplyr::filter(lubridate::year(date) %in% c(2008,2010,2012)) %>%
                     dplyr::rename(COD_IBGE=tcode) %>%
                     dplyr::mutate(COD_IBGE=as.character(COD_IBGE),
                                   ANO_ELEICAO=lubridate::year(date)) %>%
                     dplyr::select(COD_IBGE,code,value,ANO_ELEICAO) %>%
                     tidyr::spread(code,value))

# Teste t entre as variáveis
t <- rdd %>%
  dplyr::filter(duplicated(COD_IBGE)==F) %>%
  dplyr::summarise_at(vars(renda_med_2000,televisao_2000,linha_tel_2000,
                           `60_anos_2000`,rural_2000,radio_2000,
                           pc_2000,pea_2000,qtd_fam_bolsa,qtd_bpc_idoso,
                           DESPCORRM,THOMIC,TSUICID,Velocity),
                      list(~t.test(.[pop_anterior %in% c(15000:20000)],
                                   .[pop_anterior %in% c(20001:25000)])$statistic)) %>%
  t() %>%
  data.frame %>%
  tibble::rownames_to_column("var") %>%
  dplyr::bind_cols(rdd %>%
                     dplyr::filter(duplicated(COD_IBGE)==F) %>%
                     dplyr::summarise_at(vars(renda_med_2000,televisao_2000,linha_tel_2000,
                                              `60_anos_2000`,rural_2000,radio_2000,
                                              pc_2000,pea_2000,qtd_fam_bolsa,qtd_bpc_idoso,
                                              DESPCORRM,THOMIC,TSUICID,Velocity),
                                         list(~t.test(.[pop_anterior %in% c(15000:20000)],
                                                      .[pop_anterior %in% c(20001:25000)])$p.value))%>%
                     t() %>%
                     data.frame) %>%
  dplyr::rename_all(list(~c("Var","t","p.value")))


u <- rdd %>%
  dplyr::filter(duplicated(COD_IBGE)==F) %>%
  dplyr::summarise_at(vars(renda_med_2000,televisao_2000,linha_tel_2000,
                           `60_anos_2000`,rural_2000,radio_2000,
                           pc_2000,pea_2000,qtd_fam_bolsa,qtd_bpc_idoso,
                           DESPCORRM,THOMIC,TSUICID,Velocity),
                      list(~wilcox.test(.[pop_anterior %in% c(15000:20000)],
                                   .[pop_anterior %in% c(20001:25000)])$statistic)) %>%
  t() %>%
  data.frame %>%
  tibble::rownames_to_column("var") %>%
  dplyr::bind_cols(rdd %>%
                     dplyr::filter(duplicated(COD_IBGE)==F) %>%
                     dplyr::summarise_at(vars(renda_med_2000,televisao_2000,linha_tel_2000,
                                              `60_anos_2000`,rural_2000,radio_2000,
                                              pc_2000,pea_2000,qtd_fam_bolsa,qtd_bpc_idoso,
                                              DESPCORRM,THOMIC,TSUICID,Velocity),
                                         list(~wilcox.test(.[pop_anterior %in% c(15000:20000)],
                                                      .[pop_anterior %in% c(20001:25000)])$p.value))%>%
                     t() %>%
                     data.frame) %>%
  dplyr::rename_all(list(~c("Var","u","p.value")))

# Rodar descontinuidade na variável resposta
rdmulti::rdmcplot(rdd[rdd$pop_anterior<=70000,]$Velocity,
                  rdd[rdd$pop_anterior<=70000,]$pop_anterior,
                  rdd[rdd$pop_anterior<=70000,]$cut_off,
                  pvec = c(4,4,4),
                  hvec = c(5000,5000,5000))

# Rodar multiplos tratamentos com:
# Participação

# Carregar função ajustada para fuzzy
library(rdmulti)
source("D:/Backup Thiago/Thiago/tese_1/rdmc_adj2.R")


# modelo <-
# rdmc_adj(Y=rdd[duplicated(rdd$COD_IBGE)==F,]$participacao,
#          X=rdd[duplicated(rdd$COD_IBGE)==F,]$pop_anterior,
#          C=rdd[duplicated(rdd$COD_IBGE)==F,]$cut_off,
#          plot = T,
#          fuzzy = rdd[duplicated(rdd$COD_IBGE)==F,]$Velocity,
#          pooled.opt ="covs=rdd[duplicated(rdd$COD_IBGE)==F,]$renda_med_2000"
#          )

# rdrobust::rdrobust(y=rdd[duplicated(rdd$COD_IBGE)==F,]$participacao,
#                    x=rdd[duplicated(rdd$COD_IBGE)==F,]$pop_anterior-
#                    rdd[duplicated(rdd$COD_IBGE)==F,]$cut_off,
#                    fuzzy = rdd[duplicated(rdd$COD_IBGE)==F,]$Velocity,
#                    covs=rdd[duplicated(rdd$COD_IBGE)==F,]$renda_med_2000)


# rdrobust::rdrobust(y=rdd[C==c,]$participacao,
#                    x=rdd[C==c,]$pop_anterior-
#                    rdd[C==c,]$cut_off,
#                    fuzzy = rdd[C==c,]$Velocity,
#                    covs=rdd[C==c,'renda_med_2000'][[1]]+rdd[C==c,'60_anos_2000'][[1]]+rdd[C==c,'televisao_2000'][[1]]+rdd[C==c,'linha_tel_2000'][[1]]+rdd[C==c,'rural_2000'][[1]])

# modelo<-
# rdmulti::rdmc(Y=rdd[duplicated(rdd$COD_IBGE)==F,]$participacao,
#               X=rdd[duplicated(rdd$COD_IBGE)==F,]$pop_anterior,
#               C=rdd[duplicated(rdd$COD_IBGE)==F,]$cut_off,
#               plot = T,
#               pooled.opt ="covs=rdd[duplicated(rdd$COD_IBGE)==F,]$renda_med_2000"
#               )

covs= "renda_med_2000+60_anos_2000+televisao_2000+linha_tel_2000+rural_2000+radio_2000+televisao_2000+pc_2000+pea_2000+qtd_fam_bolsa+THOMIC+TSUICID+PRECIP_DIA"

modelo<-
rdmc_adj(Y="participacao",
         X="pop_anterior",
         C="cut_off",
         plot=T,
         fuzzy = "Velocity",
         data= rdd %>%
           dplyr::filter(duplicated(COD_IBGE)==F))

modelo<-
  rdmc_adj(Y="participacao",
           X="pop_anterior",
           C="cut_off",
           plot=T,
           fuzzy = "Velocity",
           data= rdd %>%
             dplyr::filter(duplicated(COD_IBGE)==F),
           pooled.opt = covs)

modelo<-
  rdmc_adj(Y="dif_part",
           X="pop_anterior",
           C="cut_off",
           plot=T,
           fuzzy = "Velocity",
           data= rdd %>%
             dplyr::filter(duplicated(COD_IBGE)==F))

modelo<-
  rdmc_adj(Y="dif_part",
           X="pop_anterior",
           C="cut_off",
           plot=T,
           fuzzy = "Velocity",
           data= rdd %>%
             dplyr::filter(duplicated(COD_IBGE)==F),
           pooled.opt = covs)



# Voto branco
modelo<-
  rdmc_adj(Y="pct_brancos",
           X="pop_anterior",
           C="cut_off",
           plot = T,
           fuzzy = "Velocity",
           data= rdd %>%
             dplyr::filter(duplicated(COD_IBGE)==F,
                           DS_CARGO=="PREFEITO"))

modelo<-
  rdmc_adj(Y="pct_brancos",
           X="pop_anterior",
           C="cut_off",
           plot=T,
           fuzzy = "Velocity",
           data= rdd %>%
             dplyr::filter(duplicated(COD_IBGE)==F),
           pooled.opt = covs)

# Diferença
modelo<-
  rdmc_adj(Y="dif_bn",
           X="pop_anterior",
           C="cut_off",
           plot = T,
           fuzzy = "Velocity",
           data= rdd %>%
             dplyr::filter(duplicated(COD_IBGE)==F,
                           DS_CARGO=="PREFEITO"))

modelo<-
  rdmc_adj(Y="dif_bn",
           X="pop_anterior",
           C="cut_off",
           plot = T,
           fuzzy = "Velocity",
           data= rdd %>%
             dplyr::filter(duplicated(COD_IBGE)==F,
                           DS_CARGO=="PREFEITO"),
           pooled.opt = covs)


# Vote share da esquerda
modelo<-
  rdmc_adj(Y="pct_vote",
           X="pop_anterior",
           C="cut_off",
           plot = T,
           fuzzy = "Velocity",
           data= rdd %>%
             dplyr::filter(orientacao=="Esquerda",
                           DS_CARGO=="PREFEITO"))

modelo<-
  rdmc_adj(Y="pct_vote",
           X="pop_anterior",
           C="cut_off",
           plot = T,
           fuzzy = "Velocity",
           data= rdd %>%
             dplyr::filter(orientacao=="Esquerda",
                           DS_CARGO=="PREFEITO"),
           pooled.opt = covs)

# Pacote rdd
# Participação
modelo <-
rdd::RDestimate(participacao ~ pop_anterior + Velocity,
                data = rdd %>%
                  dplyr::filter(duplicated(COD_IBGE)==F),
                bw=5000,
                cutpoint = 20000,
                cluster = rdd[duplicated(rdd$COD_IBGE)==F,]$regiao)

summary(modelo)

psych::lowerCor(rdd[duplicated(rdd$COD_IBGE)==F,
                    c("renda_med_2000","televisao_2000","linha_tel_2000",
                      "60_anos_2000","rural_2000","radio_2000",
                      "pc_2000","pea_2000","qtd_fam_bolsa","qtd_bpc_idoso",
                      "DESPCORRM","THOMIC","TSUICID","PRECIP_DIA")])

modelo <-
  rdd::RDestimate(participacao ~ pop_anterior + Velocity |
                    #renda_med_2000+
                    televisao_2000+
                    #linha_tel_2000+
                    pop_60_anos_2000+
                    rural_2000+
                    radio_2000+
                    pc_2000+
                    pea_2000+
                    PRECIP_DIA+
                    qtd_fam_bolsa+
                    THOMIC+
                    TSUICID
                  ,
                  data = rdd %>%
                    dplyr::filter(duplicated(COD_IBGE)==F) %>%
                    dplyr::rename(pop_60_anos_2000=`60_anos_2000`),
                  bw=5000,
                  cutpoint = 20000,
                  cluster = rdd[duplicated(rdd$COD_IBGE)==F,]$regiao)

summary(modelo)

# Testar brancos

modelo <-
  rdd::RDestimate(pct_brancos ~ pop_anterior + Velocity,
                  data = rdd %>%
                    dplyr::filter(duplicated(COD_IBGE)==F),
                  bw=5000,
                  cutpoint = 20000,
                  cluster = rdd[duplicated(rdd$COD_IBGE)==F,]$regiao)

summary(modelo)


modelo <-
  rdd::RDestimate(pct_brancos ~ pop_anterior + Velocity |
                    #renda_med_2000+
                    televisao_2000+
                    #linha_tel_2000+
                    pop_60_anos_2000+
                    rural_2000+
                    radio_2000+
                    pc_2000+
                    pea_2000+
                    PRECIP_DIA+
                    qtd_fam_bolsa+
                    THOMIC+
                    TSUICID
                  ,
                  data = rdd %>%
                    dplyr::filter(duplicated(COD_IBGE)==F) %>%
                    dplyr::rename(pop_60_anos_2000=`60_anos_2000`),
                  bw=5000,
                  cutpoint = 20000,
                  cluster = rdd[duplicated(rdd$COD_IBGE)==F,]$regiao)

summary(modelo)


# Testar nulos

modelo <-
  rdd::RDestimate(pct_nulos ~ pop_anterior + Velocity,
                  data = rdd %>%
                    dplyr::filter(duplicated(COD_IBGE)==F),
                  bw=5000,
                  cutpoint = 20000,
                  cluster = rdd[duplicated(rdd$COD_IBGE)==F,]$regiao)

summary(modelo)


modelo <-
  rdd::RDestimate(pct_nulos ~ pop_anterior + Velocity |
                    #renda_med_2000+
                    televisao_2000+
                    #linha_tel_2000+
                    pop_60_anos_2000+
                    rural_2000+
                    radio_2000+
                    pc_2000+
                    pea_2000+
                    PRECIP_DIA+
                    qtd_fam_bolsa+
                    THOMIC+
                    TSUICID
                  ,
                  data = rdd %>%
                    dplyr::filter(duplicated(COD_IBGE)==F) %>%
                    dplyr::rename(pop_60_anos_2000=`60_anos_2000`),
                  bw=5000,
                  cutpoint = 20000,
                  cluster = rdd[duplicated(rdd$COD_IBGE)==F,]$regiao)

summary(modelo)

# Testar vote share

modelo <-
  rdd::RDestimate(pct_vote ~ pop_anterior + Velocity,
                  data = rdd %>%
                    dplyr::filter(orientacao=="Esquerda",
                                  DS_CARGO=="PREFEITO"),
                  bw=5000,
                  cutpoint = 20000,
                  cluster = rdd[rdd$orientacao=="Esquerda"&
                                  rdd$DS_CARGO=="PREFEITO",]$regiao)

summary(modelo)


modelo <-
  rdd::RDestimate(pct_nulos ~ pop_anterior + Velocity |
                    #renda_med_2000+
                    televisao_2000+
                    #linha_tel_2000+
                    pop_60_anos_2000+
                    rural_2000+
                    radio_2000+
                    pc_2000+
                    pea_2000+
                    PRECIP_DIA+
                    qtd_fam_bolsa+
                    THOMIC+
                    TSUICID
                  ,
                  data = rdd %>%
                    dplyr::filter(orientacao=="Esquerda",
                                  DS_CARGO=="PREFEITO") %>%
                    dplyr::rename(pop_60_anos_2000=`60_anos_2000`),
                  bw=5000,
                  cutpoint = 20000,
                  cluster = rdd[rdd$orientacao=="Esquerda"&
                                  rdd$DS_CARGO=="PREFEITO",]$regiao)

summary(modelo)

# Testar modelo com os dados da anatel
anatel <-SCM_2007_2018 %>% 
  dplyr::filter(Ano==2008) %>%
  dplyr::group_by(`Cod IBGE`) %>%
  dplyr::summarise(conexoes=sum(valor)) %>%
  dplyr::rename(COD_IBGE=`Cod IBGE`) %>%
  dplyr::mutate(COD_IBGE=as.character(COD_IBGE)) %>% 
  dplyr::left_join(censo) %>%
  dplyr::left_join(eleic_2008_out %>%
                     dplyr::filter(duplicated(COD_IBGE)==F) %>%
                     dplyr::select(-DS_CARGO,-orientacao,-ANO_ELEICAO,
                                   -localidade)) %>%
  dplyr::left_join(back_2008) %>%
  dplyr::mutate(regiao=case_when(substr(COD_IBGE,1,1)==1~"Norte",
                                 substr(COD_IBGE,1,1)==2~"Nordeste",
                                 substr(COD_IBGE,1,1)==3~"Sudeste",
                                 substr(COD_IBGE,1,1)==4~"Sul",
                                 substr(COD_IBGE,1,1)==5~"Centro-oeste"))

summary(anatel)

teste <- lm(participacao~conexoes+
              `60_anos_2000`+
              #renda_med_2000+
              televisao_2000+
              #linha_tel_2000+
              rural_2000+
              radio_2000+
              pc_2000+
              pea_2000,
            data=anatel)

summary(teste)

ivmodel<-
AER::ivreg(participacao~conexoes +
             `60_anos_2000`+
             #renda_med_2000+
             televisao_2000+
             #linha_tel_2000+
             rural_2000+
             radio_2000+
             pc_2000+
             pea_2000 |
             . -conexoes+Velocity,
              data=anatel)

summary(ivmodel)

library(ivpack)

# Erro-padrão clusterizado com região
cluster.robust.se(ivmodel, anatel[row.names(ivmodel$model),]$regiao)

# Tentar fazer alguma coisa com a base de DDD


get_brmap("City") %>% 
  dplyr::mutate(City=as.character(City)) %>%
  dplyr::left_join(dic_DDD %>%
                     dplyr::mutate(`Código IBGE`=as.character(`Código IBGE`),
                                   CoDIGO_NACIONAL=as.character(CoDIGO_NACIONAL)),
                   by=c("City" = "Código IBGE")) %>% 
  ggplot() +
  geom_sf(aes(fill = CoDIGO_NACIONAL),
          colour = "black", size = 0.05, alpha=0.5) +
  #muda escala de cores
  # scale_fill_viridis_d(option = 3,
  #                      direction = -1) +
  # scale_fill_distiller(palette="Blues",
  #                      direction = 1)+
  
  geom_sf(data = get_brmap("State"),
          fill = "transparent",
          colour = "black", size = 0.8) +
  
  # tira sistema cartesiano
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom") 

x<-
resultado %>%
  dplyr::filter(DS_CARGO=="PREFEITO") %>%
  dplyr::left_join(censolegis) %>%
  dplyr::mutate(COD_IBGE=as.character(COD_IBGE),
                ANO_ELEICAO=as.character(ANO_ELEICAO)) %>%
  dplyr::left_join(dic_DDD %>%
                     dplyr::mutate(`Código IBGE`=as.character(`Código IBGE`)),
                   by=c("COD_IBGE"="Código IBGE")) %>%
  dplyr::left_join(smp2009_2018, 
                   by=c("CoDIGO_NACIONAL"="DDD","ANO_ELEICAO"="Ano")) %>%
  dplyr::left_join(populacao %>%
                     tidyr::gather("Ano","Populacao",c(2:13)) %>%
                     dplyr::mutate(Ano=gsub("pop_","",Ano)),
                   by=c("COD_IBGE"="COD_IBGE","ANO_ELEICAO"="Ano")) %>%
  dplyr::group_by(CoDIGO_NACIONAL,ANO_ELEICAO,Tipo) %>%
  dplyr::summarise(aptos=sum(aptos,na.rm = T),
                   compareceu=sum(compareceu,na.rm = T),
                   brancos=sum(brancos,na.rm = T),
                   nulos=sum(nulos,na.rm = T),
                   Populacao=sum(Populacao,na.rm = T),
                   Valor=first(Valor)) %>%
  dplyr::mutate(participacao=compareceu/aptos,
                pct_brancos=brancos/aptos,
                pct_nulos=nulos/aptos,
                acesso=Valor/Populacao) %>%
  dplyr::filter(Tipo=="Banda Larga Móvel")

summary(lm(participacao~acesso,data=x))



                   