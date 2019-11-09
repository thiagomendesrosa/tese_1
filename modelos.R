#setwd("F:/OneDrive/Doutorado/Tese/Bases/")
#setwd("D:/Backup Thiago/Thiago/Bases")
load("F:/OneDrive/Doutorado/Tese/Bases/paper1.rda")

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
  dplyr::group_by(ANO_ELEICAO,COD_IBGE,DS_CARGO,NR_TURNO,orientacao) %>%
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
  dplyr::group_by(ANO_ELEICAO,COD_IBGE,DS_CARGO,NR_TURNO, orientacao) %>%
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
  dplyr::select(ANO_ELEICAO,COD_IBGE,DS_CARGO,NR_TURNO,orientacao,pct_cand,
                pct_vote,participacao,pct_nulos,pct_brancos) %>%
  # Organizar por município, cargo, orientação e ano da eleição
  dplyr::arrange(COD_IBGE,DS_CARGO,NR_TURNO,orientacao,ANO_ELEICAO) %>%
  # Agrupar por município, cargo e orientação
  dplyr::group_by(COD_IBGE,DS_CARGO,NR_TURNO,orientacao) %>%
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
dt_eleicao <- readxl::read_excel("F:/OneDrive/Doutorado/Tese/Bases/TSE/data_eleicao.xlsx") %>%
  dplyr::mutate(ANO=year(DATA))

# Pegar a data de referência das eleiçõeo
dt_eleicao_anos <- dt_eleicao %>%
  dplyr::filter(ANO %in% c(2004,2006,2008,2010,2012)) %>% 
  dplyr::select(DATA,ANO, NR_TURNO) %>% 
  dplyr::distinct() %>% 
  dplyr::rename(DATA_ELEC=DATA) %>% 
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
  dplyr::group_by(COD_WMO,ANO,NR_TURNO,LAT,LONG) %>%
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

mapa <- dplyr::bind_rows(
  
  brazilmaps::get_brmap("City") %>%
    sf::st_join(estacao %>% 
                  dplyr::filter(ANO==2008,NR_TURNO==1),
                join=st_nearest_feature) %>%
    sf::st_set_geometry(NULL),
  
  brazilmaps::get_brmap("City") %>%
    sf::st_join(estacao %>% 
                  dplyr::filter(ANO==2008,NR_TURNO==2),
                join=st_nearest_feature) %>%
    sf::st_set_geometry(NULL),
  
  brazilmaps::get_brmap("City") %>%
    sf::st_join(estacao %>% 
                  dplyr::filter(ANO==2010,NR_TURNO==1),
                join=st_nearest_feature) %>%
    sf::st_set_geometry(NULL),
  
  brazilmaps::get_brmap("City") %>%
    sf::st_join(estacao %>% 
                  dplyr::filter(ANO==2010,NR_TURNO==2),
                join=st_nearest_feature) %>%
    sf::st_set_geometry(NULL),
  
  brazilmaps::get_brmap("City") %>%
    sf::st_join(estacao %>% 
                  dplyr::filter(ANO==2012,NR_TURNO==1),
                join=st_nearest_feature) %>%
    sf::st_set_geometry(NULL),
  
  brazilmaps::get_brmap("City") %>%
    sf::st_join(estacao %>% 
                  dplyr::filter(ANO==2012,NR_TURNO==2),
                join=st_nearest_feature) %>%
    sf::st_set_geometry(NULL)) %>% 
  dplyr::rename(ANO_ELEICAO=ANO)

# Plotar um mapa com a precipitação
# mapa %>%
#   ggplot()+
#   geom_sf(aes(fill=PRECIP_DIA))

# Levar informações metereológicas para base do RDD
rdd <- rdd %>%
  dplyr::left_join(mapa %>%
                     dplyr::select(ANO_ELEICAO,NR_TURNO,City,PRECIP_DIA,PRECIP,
                                   PRECIP_ANTES,PRECIP_DEPOIS,
                                   TEMP_MED) %>%
                     dplyr::mutate(COD_IBGE=as.character(City)))

# Pegar informações do Bolsa família, BPC e ipea, PIB e MTE
rdd <- rdd %>%
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
                     tidyr::spread(code,value)) %>% 
  dplyr::left_join(pib %>%
                     dplyr::filter(Ano %in% c(2008,2010,2012),
                                   VAR=="Produto Interno Bruto a preços correntes (Mil Reais)") %>%
                     dplyr::mutate(COD_IBGE=as.character(COD_IBGE),
                                   ANO_ELEICAO=as.numeric(Ano),
                                   PIB=as.numeric(PIB)) %>%
                     dplyr::select(COD_IBGE,ANO_ELEICAO,PIB)) %>% 
  dplyr::left_join(rais %>%
                     dplyr::filter(Ano %in% c(2008,2010,2012)) %>%
                     dplyr::mutate(COD_IBGE=as.character(COD_IBGE),
                                   ANO_ELEICAO=as.numeric(Ano),
                                   salarios_rais=as.numeric(salarios_rais)) %>%
                     dplyr::select(COD_IBGE,ANO_ELEICAO,vinculos_rais,salarios_rais),
                   by=c("COD_IBGE2"="COD_IBGE","ANO_ELEICAO"="ANO_ELEICAO")) %>% 
  # Fazer BPC e Bolsa pelo PIB
  dplyr::mutate_at(vars(valor_bolsa,valor_bpc_idoso,salarios_rais),
                   list(~./(PIB*1000)))

# Verificar descritivas da base
# Hmisc::describe(rdd %>% 
#                   dplyr::distinct(ANO_ELEICAO,COD_IBGE,.keep_all = T)
#                 )

# Verificar a correlação entre as variáveis
# x <-
# rdd %>%
#   dplyr::filter(ANO_ELEICAO==2008) %>%
#   dplyr::filter(duplicated(COD_IBGE)==F) %>% 
#   dplyr::select("Total_2000","0_14_anos_2000","15_29_anos_2000",
#                 "30_59_anos_2000","60_anos_2000","Total_2010",
#                 "0_14_anos_2010","15_29_anos_2010","30_59_anos_2010",
#                 "60_anos_2010","negro_2000","negro_2010","iluminacao_2000",
#                 "linha_tel_2000","radio_2000","televisao_2000","pc_2000",
#                 "carro_2000","iluminacao_2010","linha_tel_2010","cel_2010",
#                 "radio_2010","televisao_2010","pc_2010","pc_internet_2010",
#                 "carro_2010","ens_med_2000","ens_sup_2000","ens_med_2010",
#                 "ens_sup_2010","casado_2000","casado_2010","pea_2000","pea_2010",
#                 "renda_med_2000","renda_med_2010","rural_2000","rural_2010",
#                 "PRECIP_DIA","PRECIP","PRECIP_ANTES","PRECIP_DEPOIS",
#                 "TEMP_MED","qtd_fam_bolsa","valor_bolsa",
#                 "qtd_bpc_idoso","qtd_bpc_pcd","valor_bpc_idoso","valor_bpc_pcd",
#                 "DESPCORRM","DESPCUPM","DINVESTM","THOMIC","TSUICID","PIB",
#                 "vinculos_rais","salarios_rais") %>% 
#   psych::lowerCor() %>% 
#   t() %>% 
#   data.frame(stringsAsFactors = F)

# Variáveis com muito missing
# TSUICID,THOMI, DESPCORRM, DESPCUPM, DINVESTM, pc_2000, linha_tel_2000 

# x <-
#   rdd %>%
#   dplyr::filter(ANO_ELEICAO==2008) %>%
#   dplyr::filter(duplicated(COD_IBGE)==F) %>% 
#   dplyr::select("Total_2000","0_14_anos_2000","15_29_anos_2000",
#                 "30_59_anos_2000","60_anos_2000","Total_2010",
#                 "0_14_anos_2010","15_29_anos_2010","30_59_anos_2010",
#                 "60_anos_2010","negro_2000","negro_2010","iluminacao_2000",
#                 "linha_tel_2000","radio_2000","televisao_2000","pc_2000",
#                 "carro_2000","iluminacao_2010","linha_tel_2010","cel_2010",
#                 "radio_2010","televisao_2010","pc_2010","pc_internet_2010",
#                 "carro_2010","ens_med_2000","ens_sup_2000","ens_med_2010",
#                 "ens_sup_2010","casado_2000","casado_2010","pea_2000","pea_2010",
#                 "renda_med_2000","renda_med_2010","rural_2000","rural_2010",
#                 "PRECIP_DIA","PRECIP","PRECIP_ANTES","PRECIP_DEPOIS",
#                 "TEMP_MED","qtd_fam_bolsa","valor_bolsa",
#                 "qtd_bpc_idoso","qtd_bpc_pcd","valor_bpc_idoso","valor_bpc_pcd",
#                 "DESPCORRM","DESPCUPM","DINVESTM","THOMIC","TSUICID","PIB",
#                 "vinculos_rais","salarios_rais") %>% 
#   psych::lowerCor() %>% 
#   data.frame %>% 
#   tibble::rownames_to_column("var") %>% 
#   dplyr::filter(var %in% c("TSUICID","THOMIC", "DESPCORRM", "DESPCUPM", 
#                                     "DINVESTM", "pc_2000", "linha_tel_2000")) %>% 
#   t() %>% 
#   data.frame(stringsAsFactors = F) 
# 
# colnames(x) <- x["var",]
# 
# x <- x[-1,] %>% 
#   tibble::rownames_to_column("var") %>% 
#   dplyr::mutate_at(vars(2:length(.)),
#                    list(~as.numeric(.)))

# renda e escolaridade tem alta correlação com tel e pc em 2000
# PIB e vinc_rais com as variáveis de despesa
# Casamento com suicídio

# Ver correlação das possíveis variáveis controle em 2000
# x <-
#   rdd %>%
#   dplyr::filter(ANO_ELEICAO==2008) %>%
#   dplyr::filter(duplicated(COD_IBGE)==F) %>% 
#   dplyr::select(Total_2000,
#                 renda_med_2000,
#                 `60_anos_2000`,
#                 rural_2000,
#                 negro_2000,
#                 iluminacao_2000,
#                 radio_2000,
#                 televisao_2000,
#                 carro_2000,
#                 ens_sup_2000,
#                 casado_2000,
#                 pea_2000,
#                 PRECIP_DIA,
#                 TEMP_MED,
#                 valor_bolsa,
#                 valor_bpc_idoso,
#                 PIB,
#                 salarios_rais) %>% 
#   psych::lowerCor() %>% 
#   t() %>% 
#   data.frame(stringsAsFactors = F)

# Fazer o mesmo para 2010
# x <-
#   rdd %>%
#   dplyr::filter(ANO_ELEICAO==2008) %>%
#   dplyr::filter(duplicated(COD_IBGE)==F) %>% 
#   dplyr::select(Total_2010,
#                 renda_med_2010,
#                 `60_anos_2010`,
#                 rural_2010,
#                 negro_2010,
#                 iluminacao_2010,
#                 pc_internet_2010,
#                 radio_2010,
#                 televisao_2010,
#                 carro_2010,
#                 ens_sup_2010,
#                 casado_2010,
#                 pea_2010,
#                 PRECIP_DIA,
#                 TEMP_MED,
#                 valor_bolsa,
#                 valor_bpc_idoso,
#                 PIB,
#                 salarios_rais) %>% 
#   psych::lowerCor() %>% 
#   t() %>% 
#   data.frame(stringsAsFactors = F)

# Teste t entre as variáveis
# Construir função para calcular o teste t entre as variáveis
teste_t <- function(Ano){
       rdd %>%
    # Filtrar para o ano de interesse
    dplyr::filter(ANO_ELEICAO==Ano) %>%
    # Retirar entradas duplicadas
    dplyr::filter(duplicated(COD_IBGE)==F) %>% 
    # Aplicar o teste t nas variáveis de interesse
    dplyr::summarise_at(vars(participacao,
                             Total_2000,
                             renda_med_2000,
                             `60_anos_2000`,
                             rural_2000,
                             negro_2000,
                             radio_2000,
                             televisao_2000,
                             ens_sup_2000,
                             casado_2000,
                             pea_2000,
                             Total_2010,
                             renda_med_2010,
                             `60_anos_2010`,
                             rural_2010,
                             negro_2010,
                             radio_2010,
                             televisao_2010,
                             ens_sup_2010,
                             casado_2010,
                             pea_2010,
                             PRECIP_DIA,
                             TEMP_MED,
                             valor_bolsa,
                             valor_bpc_idoso,
                             PIB,
                             salarios_rais,
                             Velocity),
                        # Selecionar o valor da estatística
                        list(~t.test(.[pop_anterior %in% c(16500:20000)],
                                   .[pop_anterior %in% c(20001:23500)])$statistic)) %>%
    # Transpor a matriz
    t() %>%
    # Transformar resultado em data frame
    data.frame %>%
    # Recuperar nome das variáveis
    tibble::rownames_to_column("var") %>%
    # Fazer o mesmo processo para pegar o P-valor
    dplyr::bind_cols(rdd %>%
                     dplyr::filter(ANO_ELEICAO==Ano) %>%
                     dplyr::filter(duplicated(COD_IBGE)==F) %>% 
                     dplyr::summarise_at(vars(participacao,
                                              Total_2000,
                                              renda_med_2000,
                                              `60_anos_2000`,
                                              rural_2000,
                                              negro_2000,
                                              radio_2000,
                                              televisao_2000,
                                              ens_sup_2000,
                                              casado_2000,
                                              pea_2000,
                                              Total_2010,
                                              renda_med_2010,
                                              `60_anos_2010`,
                                              rural_2010,
                                              negro_2010,
                                              radio_2010,
                                              televisao_2010,
                                              ens_sup_2010,
                                              casado_2010,
                                              pea_2010,
                                              PRECIP_DIA,
                                              TEMP_MED,
                                              valor_bolsa,
                                              valor_bpc_idoso,
                                              PIB,
                                              salarios_rais,
                                              Velocity),
                                         list(~t.test(.[pop_anterior %in% c(16500:20000)],
                                                      .[pop_anterior %in% c(20001:23500)])$p.value))%>%
                     t() %>%
                     data.frame) %>%
  dplyr::rename_all(list(~c("Var",paste0("t_", Ano),paste0("p.value_", Ano))))
}

# Juntar os resultados de cada ano
t_base <- dplyr::bind_cols(
  teste_t(Ano=2008)[c(1:11,22:28),],
  teste_t(Ano=2010)[c(1,12:28),-1],
  teste_t(Ano=2012)[c(1,12:28),-1])

# Construir uma função para calcular o teste u
teste_u <- function(Ano){
  rdd %>%
    # Filtar para o ano de interesse
    dplyr::filter(ANO_ELEICAO==Ano) %>%
    # Remover entradas duplicadas
    dplyr::filter(duplicated(COD_IBGE)==F) %>% 
    # Calcular o teste u para as variáveis de interesse
    dplyr::summarise_at(vars(participacao,
                             Total_2000,
                             renda_med_2000,
                             `60_anos_2000`,
                             rural_2000,
                             negro_2000,
                             radio_2000,
                             televisao_2000,
                             ens_sup_2000,
                             casado_2000,
                             pea_2000,
                             Total_2010,
                             renda_med_2010,
                             `60_anos_2010`,
                             rural_2010,
                             negro_2010,
                             radio_2010,
                             televisao_2010,
                             ens_sup_2010,
                             casado_2010,
                             pea_2010,
                             PRECIP_DIA,
                             TEMP_MED,
                             valor_bolsa,
                             valor_bpc_idoso,
                             PIB,
                             salarios_rais,
                             Velocity),
                        # Pegar o valor da estatística
                        list(~wilcox.test(.[pop_anterior %in% c(16500:20000)],
                                     .[pop_anterior %in% c(20001:23500)])$statistic)) %>%
    # Transpor os resultados
    t() %>%
    # Deixar como data frame
    data.frame %>%
    # Recuperar o nome das variáveis
    tibble::rownames_to_column("var") %>%
    # Repetir o processo para pegar o p-valor
    dplyr::bind_cols(rdd %>%
                       dplyr::filter(ANO_ELEICAO==Ano) %>%
                       dplyr::filter(duplicated(COD_IBGE)==F) %>% 
                       dplyr::summarise_at(vars(participacao,
                                                Total_2000,
                                                renda_med_2000,
                                                `60_anos_2000`,
                                                rural_2000,
                                                negro_2000,
                                                radio_2000,
                                                televisao_2000,
                                                ens_sup_2000,
                                                casado_2000,
                                                pea_2000,
                                                Total_2010,
                                                renda_med_2010,
                                                `60_anos_2010`,
                                                rural_2010,
                                                negro_2010,
                                                radio_2010,
                                                televisao_2010,
                                                ens_sup_2010,
                                                casado_2010,
                                                pea_2010,
                                                PRECIP_DIA,
                                                TEMP_MED,
                                                valor_bolsa,
                                                valor_bpc_idoso,
                                                PIB,
                                                salarios_rais,
                                                Velocity),
                                           list(~wilcox.test(.[pop_anterior %in% c(16500:20000)],
                                                        .[pop_anterior %in% c(20001:23500)])$p.value))%>%
                       t() %>%
                       data.frame) %>%
    dplyr::rename_all(list(~c("Var",paste0("t_", Ano),paste0("p.value_", Ano))))
}

# Juntar os resultados
u_base <- dplyr::bind_cols(
  teste_u(Ano=2008),
  teste_u(Ano=2010)[-1],
  teste_u(Ano=2012)[-1])

# Rodar descontinuidade na variável resposta
# rdmulti::rdmcplot(rdd[duplicated(rdd$COD_IBGE)==FALSE&rdd$pop_anterior<=70000,]$Velocity,
#                   rdd[duplicated(rdd$COD_IBGE)==FALSE&rdd$pop_anterior<=70000,]$pop_anterior,
#                   rdd[duplicated(rdd$COD_IBGE)==FALSE&rdd$pop_anterior<=70000,]$cut_off,
#                   #pvec = c(2,2,2),
#                   #hvec = c(5000,5000,5000)
#                   )

# Rodar multiplos tratamentos com:
# Participação

# Carregar função ajustada para fuzzy
library(rdmulti)
#source("F:/Backup Thiago/Thiago/tese_1/rdmc_adj2.R")
source("F:/OneDrive/Doutorado/Tese/tese_1/rdmc_adj2.R")

covs_2008= "Total_2000+renda_med_2000+60_anos_2000+rural_2000+negro_2000+radio_2000+televisao_2000+ens_sup_2000+casado_2000+pea_2000+PRECIP_DIA+TEMP_MED+valor_bolsa+valor_bpc_idoso+PIB+salarios_rais"

covs_2010= "Total_2010+renda_med_2010+60_anos_2010+rural_2010+negro_2010+radio_2010+televisao_2010+ens_sup_2010+casado_2010+pea_2010+PRECIP_DIA+TEMP_MED+valor_bolsa+valor_bpc_idoso+PIB+salarios_rais"

# Estatísticas descritivas das covariáveis
descriptives_2008 <- rdd %>% 
  dplyr::filter(ANO_ELEICAO==2008) %>% 
  dplyr::distinct(ANO_ELEICAO,COD_IBGE,.keep_all=T) %>% 
  dplyr::select(Total_2000,renda_med_2000,`60_anos_2000`,
                rural_2000,negro_2000,radio_2000,televisao_2000,
                ens_sup_2000,casado_2000,pea_2000,PRECIP_DIA,
                TEMP_MED,valor_bolsa,valor_bpc_idoso,
                PIB,salarios_rais) %>% 
  dplyr::summarise_all(list(V.Obs.=~n(),
                            V.Average=~mean(.,na.rm=T),
                            V.Std.Dev.=~sd(.,na.rm=T),
                            V.Max=~max(.,na.rm=T),
                            V.Min=~min(.,na.rm=T))) %>% 
  tidyr::gather("vars","value") %>% 
  tidyr::separate(col=vars,
                  into=c("Variable","stat"),
                  sep="_V.") %>% 
  tidyr::spread(stat,value) %>% 
  dplyr::mutate_if(is.numeric,
                   list(~round(.,2))) %>% 
  dplyr::mutate(Variable=c("Pop. over 60 years",
                            "Married",
                            "College",
                            "Black",
                            "Working Pop.",
                            "GDP",
                            "Rain (elect. day)",
                            "Radio",
                            "Median Income",
                            "Rural",
                            "Formal Wages",
                            "Television",
                            "Avg. Temperature",
                            "Population",
                            "PBF",
                            "BPC")) %>% 
  dplyr::select(1,5,2,6,4,3) %>% 
  dplyr::arrange(Variable)

descriptives_2010 <- rdd %>% 
  dplyr::filter(ANO_ELEICAO==2010) %>% 
  dplyr::distinct(ANO_ELEICAO,COD_IBGE,.keep_all=T) %>% 
  dplyr::select(Total_2010,renda_med_2010,`60_anos_2010`,
                rural_2010,negro_2010,radio_2010,televisao_2010,
                ens_sup_2010,casado_2010,pea_2010,PRECIP_DIA,
                TEMP_MED,valor_bolsa,valor_bpc_idoso,
                PIB,salarios_rais) %>% 
  dplyr::summarise_all(list(V.Obs.=~n(),
                            V.Average=~mean(.,na.rm=T),
                            V.Std.Dev.=~sd(.,na.rm=T),
                            V.Max=~max(.,na.rm=T),
                            V.Min=~min(.,na.rm=T))) %>% 
  tidyr::gather("vars","value") %>% 
  tidyr::separate(col=vars,
                  into=c("Variable","stat"),
                  sep="_V.") %>% 
  tidyr::spread(stat,value) %>% 
  dplyr::mutate_if(is.numeric,
                   list(~round(.,2))) %>% 
  dplyr::mutate(Variable=c("Pop. over 60 years",
                           "Married",
                           "College",
                           "Black",
                           "Working Pop.",
                           "GDP",
                           "Rain (elect. day)",
                           "Radio",
                           "Median Income",
                           "Rural",
                           "Formal Wages",
                           "Television",
                           "Avg. Temperature",
                           "Population",
                           "PBF",
                           "BPC")) %>% 
  dplyr::select(1,5,2,6,4,3) %>% 
  dplyr::arrange(Variable)

descriptives_2012 <- rdd %>% 
  dplyr::filter(ANO_ELEICAO==2012) %>% 
  dplyr::distinct(ANO_ELEICAO,COD_IBGE,.keep_all=T) %>% 
  dplyr::select(Total_2010,renda_med_2000,`60_anos_2010`,
                rural_2010,negro_2010,radio_2010,televisao_2010,
                ens_sup_2010,casado_2010,pea_2010,PRECIP_DIA,
                TEMP_MED,valor_bolsa,valor_bpc_idoso,
                PIB,salarios_rais) %>% 
  dplyr::summarise_all(list(V.Obs.=~n(),
                            V.Average=~mean(.,na.rm=T),
                            V.Std.Dev.=~sd(.,na.rm=T),
                            V.Max=~max(.,na.rm=T),
                            V.Min=~min(.,na.rm=T))) %>% 
  tidyr::gather("vars","value") %>% 
  tidyr::separate(col=vars,
                  into=c("Variable","stat"),
                  sep="_V.") %>% 
  tidyr::spread(stat,value) %>% 
  dplyr::mutate_if(is.numeric,
                   list(~round(.,2))) %>% 
  dplyr::mutate(Variable=c("Pop. over 60 years",
                           "Married",
                           "College",
                           "Black",
                           "Working Pop.",
                           "GDP",
                           "Rain (elect. day)",
                           "Radio",
                           "Median Income",
                           "Rural",
                           "Formal Wages",
                           "Television",
                           "Avg. Temperature",
                           "Population",
                           "PBF",
                           "BPC")) %>% 
  dplyr::select(1,5,2,6,4,3) %>% 
  dplyr::arrange(Variable)

# modelo<-
# rdmc_adj(Y="participacao",
#          X="pop_anterior",
#          C="cut_off",
#          plot=T,
#          fuzzy = "Velocity",
#          data= rdd %>%
#            dplyr::filter(ANO_ELEICAO==2008,
#                          NR_TURNO==1) %>%
#            dplyr::filter(duplicated(COD_IBGE)==F))

# Rodar participação

# Criar função para rodar modelos nos resultados

rodar_modelo <- function(y,
                         turno=1){

  assign(paste0("resultados_",y),
         c())
  
for(ano in c(2008,2010,2012)){

  rdmc_adj(Y=y,
           X="pop_anterior",
           C="cut_off",
           plot=T,
           fuzzy = "Velocity",
           data= rdd %>%
             dplyr::filter(ANO_ELEICAO==ano,
                           NR_TURNO==turno) %>% 
             dplyr::filter(duplicated(COD_IBGE)==F))

  mod <- results_dt %>% 
    dplyr::mutate(Year=paste0(ano),
                  Type="Without covariates")

  
  assign(paste0("resultados_",y),
         rbind(get(paste0("resultados_",y)),
                   mod))
         

  if(ano==2008){  
  
  rdmc_adj(Y=y,
           X="pop_anterior",
           C="cut_off",
           plot=T,
           fuzzy = "Velocity",
           data= rdd %>%
             dplyr::filter(ANO_ELEICAO==ano,
                           NR_TURNO==turno) %>% 
             dplyr::filter(duplicated(COD_IBGE)==F) %>% 
             dplyr::mutate(nordeste=case_when(regiao=="Nordeste"~1,
                                              TRUE~0),
                           norte=case_when(regiao=="Norte"~1,
                                              TRUE~0),
                           centro=case_when(regiao=="Centro-oeste"~1,
                                              TRUE~0),
                           sudeste=case_when(regiao=="Sudeste"~1,
                                            TRUE~0)),
           pooled.opt = paste0(covs_2008,"+nordeste+norte+centro+sudeste")
           )
  
  mod <- results_dt %>% 
    dplyr::mutate(Year=paste0(ano),
                  Type="With covariates")
  
  assign(paste0("resultados_",y),
         rbind(get(paste0("resultados_",y)),
               mod))
  
  } else {
    
    rdmc_adj(Y=y,
             X="pop_anterior",
             C="cut_off",
             plot=T,
             fuzzy = "Velocity",
             data= rdd %>%
               dplyr::filter(ANO_ELEICAO==ano,
                             NR_TURNO==turno) %>% 
               dplyr::filter(duplicated(COD_IBGE)==F)%>% 
               dplyr::mutate(nordeste=case_when(regiao=="Nordeste"~1,
                                                TRUE~0),
                             norte=case_when(regiao=="Norte"~1,
                                             TRUE~0),
                             centro=case_when(regiao=="Centro-oeste"~1,
                                              TRUE~0),
                             sudeste=case_when(regiao=="Sudeste"~1,
                                               TRUE~0)),
             pooled.opt = paste0(covs_2010,"+nordeste+norte+centro+sudeste"))
    
    mod <- results_dt %>% 
      dplyr::mutate(Year=paste0(ano),
                    Type="With covariates")
    
    assign(paste0("resultados_",y),
           rbind(get(paste0("resultados_",y)),
                 mod))
  }
}

  
  assign(paste0("resultados_",y),
         get(paste0("resultados_",y)) %>% 
           dplyr::filter(!(Cutoff!="Pooled"&Type=="With covariates")),
         envir = parent.frame() )

}

rodar_modelo_vs <- function(y,
                            cargo_m="PREFEITO",
                            cargo_n="PRESIDENTE",
                            orient="Esquerda",
                            turno=1){
  
  assign(paste0("resultados_",y),
         c())
  
  for(ano in c(2008,2010,2012)){
    
    if(ano %in% c(2008,2012)){
    
    rdmc_adj(Y=y,
             X="pop_anterior",
             C="cut_off",
             plot=T,
             fuzzy = "Velocity",
             data= rdd %>%
               dplyr::filter(ANO_ELEICAO==ano) %>% 
               dplyr::filter(NR_TURNO==turno) %>% 
               dplyr::filter(DS_CARGO==cargo_m) %>% 
               dplyr::filter(orientacao==orient))
    
    mod <- results_dt %>% 
      dplyr::mutate(Year=paste0(ano),
                    Type="Without covariates",
                    Y=y,
                    Office=cargo_m,
                    Orientation=orient)
    
    
    assign(paste0("resultados_",y),
           rbind(get(paste0("resultados_",y)),
                 mod))
    } else {
      
      
      rdmc_adj(Y=y,
               X="pop_anterior",
               C="cut_off",
               plot=T,
               fuzzy = "Velocity",
               data= rdd %>%
                 dplyr::filter(ANO_ELEICAO==ano) %>% 
                 dplyr::filter(NR_TURNO==turno) %>% 
                 dplyr::filter(DS_CARGO==cargo_n) %>% 
                 dplyr::filter(orientacao==orient))
      
      mod <- results_dt %>% 
        dplyr::mutate(Year=paste0(ano),
                      Type="Without covariates",
                      Y=y,
                      Office=cargo_n,
                      Orientation=orient)
      
      
      assign(paste0("resultados_",y),
             rbind(get(paste0("resultados_",y)),
                   mod))
    }
    
    if(ano==2008){  
      
      rdmc_adj(Y=y,
               X="pop_anterior",
               C="cut_off",
               plot=T,
               fuzzy = "Velocity",
               data= rdd %>%
                 dplyr::filter(ANO_ELEICAO==ano) %>% 
                 dplyr::filter(NR_TURNO==turno) %>% 
                 dplyr::filter(DS_CARGO==cargo_m) %>% 
                 dplyr::filter(orientacao==orient)%>% 
                 dplyr::mutate(nordeste=case_when(regiao=="Nordeste"~1,
                                                  TRUE~0),
                               norte=case_when(regiao=="Norte"~1,
                                               TRUE~0),
                               centro=case_when(regiao=="Centro-oeste"~1,
                                                TRUE~0),
                               sudeste=case_when(regiao=="Sudeste"~1,
                                                 TRUE~0)),
               pooled.opt = paste0(covs_2008,"+nordeste+norte+centro+sudeste")
      )
      
      mod <- results_dt %>% 
        dplyr::mutate(Year=paste0(ano),
                      Type="With covariates",
                      Y=y,
                      Office=cargo_m,
                      Orientation=orient)
      
      assign(paste0("resultados_",y),
             rbind(get(paste0("resultados_",y)),
                   mod))
      
    } else if(ano == 2010) {
      
      rdmc_adj(Y=y,
               X="pop_anterior",
               C="cut_off",
               plot=T,
               fuzzy = "Velocity",
               data= rdd %>%
                 dplyr::filter(ANO_ELEICAO==ano) %>% 
                 dplyr::filter(NR_TURNO==turno) %>% 
                 dplyr::filter(DS_CARGO==cargo_n) %>% 
                 dplyr::filter(orientacao==orient)%>% 
                 dplyr::mutate(nordeste=case_when(regiao=="Nordeste"~1,
                                                  TRUE~0),
                               norte=case_when(regiao=="Norte"~1,
                                               TRUE~0),
                               centro=case_when(regiao=="Centro-oeste"~1,
                                                TRUE~0),
                               sudeste=case_when(regiao=="Sudeste"~1,
                                                 TRUE~0)),
               pooled.opt = paste0(covs_2010,"+nordeste+norte+centro+sudeste"))
      
      mod <- results_dt %>% 
        dplyr::mutate(Year=paste0(ano),
                      Type="With covariates",
                      Y=y,
                      Office=cargo_n,
                      Orientation=orient)
      
      assign(paste0("resultados_",y),
             rbind(get(paste0("resultados_",y)),
                   mod))
    } else {
      
      rdmc_adj(Y=y,
               X="pop_anterior",
               C="cut_off",
               plot=T,
               fuzzy = "Velocity",
               data= rdd %>%
                 dplyr::filter(ANO_ELEICAO==ano) %>% 
                 dplyr::filter(NR_TURNO==turno) %>% 
                 dplyr::filter(DS_CARGO==cargo_m) %>% 
                 dplyr::filter(orientacao==orient)%>% 
                 dplyr::mutate(nordeste=case_when(regiao=="Nordeste"~1,
                                                  TRUE~0),
                               norte=case_when(regiao=="Norte"~1,
                                               TRUE~0),
                               centro=case_when(regiao=="Centro-oeste"~1,
                                                TRUE~0),
                               sudeste=case_when(regiao=="Sudeste"~1,
                                                 TRUE~0)),
               pooled.opt = paste0(covs_2010,"+nordeste+norte+centro+sudeste"))
      
      mod <- results_dt %>% 
        dplyr::mutate(Year=paste0(ano),
                      Type="With covariates",
                      Y=y,
                      Office=cargo_m,
                      Orientation=orient)
      
      assign(paste0("resultados_",y),
             rbind(get(paste0("resultados_",y)),
                   mod))
      
    }
  }

  
  
  assign(paste0("resultados_",y),
         get(paste0("resultados_",y)) %>% 
           dplyr::filter(!(Cutoff!="Pooled"&Type=="With covariates")),
         envir = parent.frame() )
  
}

rodar_modelo("participacao")
rodar_modelo("dif_part")
rodar_modelo_vs("pct_bn")
rodar_modelo_vs("dif_bn")
rodar_modelo_vs("pct_vote")

# Pacote rdd

# Participação
rodar_modelo2 <- function(y,
                          bw1=5000,
                          bw2=5000,
                          bw3=5000,
                          cut=20000,
                          turno=1){

  assign(paste0("resultados2_",y),
         c())
  
  for(ano in c(2008,2010,2012)) {
    
    if(ano==2008){

 modelo <- 
   rdd::RDestimate(get(y) ~ pop_anterior + Velocity,
                   data = rdd %>%
                     dplyr::filter(ANO_ELEICAO==ano,
                                   NR_TURNO==turno) %>% 
                     dplyr::filter(duplicated(COD_IBGE)==F),
                   bw=bw1,
                   cutpoint = cut,
                   cluster = rdd %>%
                     dplyr::filter(ANO_ELEICAO==ano,
                                   NR_TURNO==turno) %>%
                     dplyr::filter(duplicated(COD_IBGE)==F) %>% pull(regiao))



mod <- dplyr::tibble(Year=rep(ano,3),
                     Y=rep(y,3),
                     Type=rep("Without covariates",3),
                     Model=names(modelo$est),
                     Bw=modelo$bw,
                     Observations=modelo$obs,
                     Estimates=modelo$est,
                     Std.Error=modelo$se,
                     Z=modelo$z,
                     p.value=modelo$p)

assign(paste0("resultados2_",y),
       rbind(get(paste0("resultados2_",y)),
             mod))} else if(ano==2010){
               
               modelo <- 
                 rdd::RDestimate(get(y) ~ pop_anterior + Velocity,
                                 data = rdd %>%
                                   dplyr::filter(ANO_ELEICAO==ano,
                                                 NR_TURNO==turno) %>% 
                                   dplyr::filter(duplicated(COD_IBGE)==F),
                                 bw=bw2,
                                 cutpoint = cut,
                                 cluster = rdd %>%
                                   dplyr::filter(ANO_ELEICAO==ano,
                                                 NR_TURNO==turno) %>%
                                   dplyr::filter(duplicated(COD_IBGE)==F) %>% pull(regiao))
               
               
               
               mod <- dplyr::tibble(Year=rep(ano,3),
                                    Y=rep(y,3),
                                    Type=rep("Without covariates",3),
                                    Model=names(modelo$est),
                                    Bw=modelo$bw,
                                    Observations=modelo$obs,
                                    Estimates=modelo$est,
                                    Std.Error=modelo$se,
                                    Z=modelo$z,
                                    p.value=modelo$p)
               
               assign(paste0("resultados2_",y),
                      rbind(get(paste0("resultados2_",y)),
                            mod))
               
               
             } else {
               
               modelo <- 
                 rdd::RDestimate(get(y) ~ pop_anterior + Velocity,
                                 data = rdd %>%
                                   dplyr::filter(ANO_ELEICAO==ano,
                                                 NR_TURNO==turno) %>% 
                                   dplyr::filter(duplicated(COD_IBGE)==F),
                                 bw=bw3,
                                 cutpoint = cut,
                                 cluster = rdd %>%
                                   dplyr::filter(ANO_ELEICAO==ano,
                                                 NR_TURNO==turno) %>%
                                   dplyr::filter(duplicated(COD_IBGE)==F) %>% pull(regiao))
               
               
               
               mod <- dplyr::tibble(Year=rep(ano,3),
                                    Y=rep(y,3),
                                    Type=rep("Without covariates",3),
                                    Model=names(modelo$est),
                                    Bw=modelo$bw,
                                    Observations=modelo$obs,
                                    Estimates=modelo$est,
                                    Std.Error=modelo$se,
                                    Z=modelo$z,
                                    p.value=modelo$p)
               
               assign(paste0("resultados2_",y),
                      rbind(get(paste0("resultados2_",y)),
                            mod))
               
               
             }

if(ano==2008){
  
  f <- as.formula(paste0(y,"~","pop_anterior + Velocity |",
                         gsub("60_anos_2000","pop_60_anos_2000",covs_2008)))
  
  modelo <- 
    rdd::RDestimate(f,
                    data = rdd %>%
                      dplyr::rename(pop_60_anos_2000=`60_anos_2000`) %>% 
                      dplyr::filter(ANO_ELEICAO==ano,
                                    NR_TURNO==turno) %>% 
                      dplyr::filter(duplicated(COD_IBGE)==F),
                    bw=bw1,
                    cutpoint = cut,
                    cluster = rdd %>%
                      dplyr::filter(ANO_ELEICAO==ano,
                                    NR_TURNO==turno) %>%
                      dplyr::filter(duplicated(COD_IBGE)==F) %>% pull(regiao))
  
  
  
  mod <- dplyr::tibble(Year=rep(ano,3),
                       Y=rep(y,3),
                       Type=rep("With covariates",3),
                       Model=names(modelo$est),
                       Bw=modelo$bw,
                       Observations=modelo$obs,
                       Estimates=modelo$est,
                       Std.Error=modelo$se,
                       Z=modelo$z,
                       p.value=modelo$p)
  
  assign(paste0("resultados2_",y),
         rbind(get(paste0("resultados2_",y)),
               mod))
} else if(ano==2010) {
  
  f <- as.formula(paste0(y,"~","pop_anterior + Velocity |",
                         gsub("60_anos_2010","pop_60_anos_2010",
                              covs_2010)))
  
  
  modelo <- 
    rdd::RDestimate(f,
                    data = rdd %>%
                      dplyr::rename(pop_60_anos_2010=`60_anos_2010`) %>% 
                      dplyr::filter(ANO_ELEICAO==ano,
                                    NR_TURNO==turno) %>% 
                      dplyr::filter(duplicated(COD_IBGE)==F),
                    bw=bw2,
                    cutpoint = cut,
                    cluster = rdd %>%
                      dplyr::filter(ANO_ELEICAO==ano,
                                    NR_TURNO==turno) %>%
                      dplyr::filter(duplicated(COD_IBGE)==F) %>% pull(regiao))
  
  mod <- dplyr::tibble(Year=rep(ano,3),
                       Y=rep(y,3),
                       Type=rep("With covariates",3),
                       Model=names(modelo$est),
                       Bw=modelo$bw,
                       Observations=modelo$obs,
                       Estimates=modelo$est,
                       Std.Error=modelo$se,
                       Z=modelo$z,
                       p.value=modelo$p)

  
  assign(paste0("resultados2_",y),
         rbind(get(paste0("resultados2_",y)),
               mod))
} else {
  
  f <- as.formula(paste0(y,"~","pop_anterior + Velocity |",
                         gsub("60_anos_2010","pop_60_anos_2010",
                              covs_2010)))
  
  
  modelo <- 
    rdd::RDestimate(f,
                    data = rdd %>%
                      dplyr::rename(pop_60_anos_2010=`60_anos_2010`) %>% 
                      dplyr::filter(ANO_ELEICAO==ano,
                                    NR_TURNO==turno) %>% 
                      dplyr::filter(duplicated(COD_IBGE)==F),
                    bw=bw3,
                    cutpoint = cut,
                    cluster = rdd %>%
                      dplyr::filter(ANO_ELEICAO==ano,
                                    NR_TURNO==turno) %>%
                      dplyr::filter(duplicated(COD_IBGE)==F) %>% pull(regiao))
  
  mod <- dplyr::tibble(Year=rep(ano,3),
                       Y=rep(y,3),
                       Type=rep("With covariates",3),
                       Model=names(modelo$est),
                       Bw=modelo$bw,
                       Observations=modelo$obs,
                       Estimates=modelo$est,
                       Std.Error=modelo$se,
                       Z=modelo$z,
                       p.value=modelo$p)
  
  
  assign(paste0("resultados2_",y),
         rbind(get(paste0("resultados2_",y)),
               mod))
  
}
  }
  
  assign(paste0("resultados2_",y),
         get(paste0("resultados2_",y)),
         envir = parent.frame())
}

# Modelos para voteshare
rodar_modelo2_vs <- function(y,
                             cargo_m="PREFEITO",
                             cargo_n="PRESIDENTE",
                             orient="Esquerda",
                             turno=1,
                             bw1=5000,
                             bw2=5000,
                             bw3=5000,
                             cut=20000){
  
  assign(paste0("resultados2_",y),
         c())
  
  for(ano in c(2008,2010,2012)) {
    
    if(ano==2008){
      
      modelo <- 
        rdd::RDestimate(get(y) ~ pop_anterior + Velocity,
                        data = rdd %>%
                          dplyr::filter(ANO_ELEICAO==ano) %>% 
                          dplyr::filter(NR_TURNO==turno) %>% 
                          dplyr::filter(DS_CARGO==cargo_m) %>% 
                          dplyr::filter(orientacao==orient),
                        bw=bw1,
                        cutpoint = cut,
                        cluster = rdd %>%
                          dplyr::filter(ANO_ELEICAO==ano) %>% 
                          dplyr::filter(NR_TURNO==turno) %>% 
                          dplyr::filter(DS_CARGO==cargo_m) %>% 
                          dplyr::filter(orientacao==orient) %>% 
                          pull(regiao))
      
      
      
      mod <- dplyr::tibble(Year=rep(ano,3),
                           Y=rep(y,3),
                           Office=cargo_m,
                           Orientation=orient,
                           Type=rep("Without covariates",3),
                           Model=names(modelo$est),
                           Bw=modelo$bw,
                           Observations=modelo$obs,
                           Estimates=modelo$est,
                           Std.Error=modelo$se,
                           Z=modelo$z,
                           p.value=modelo$p)
      
      assign(paste0("resultados2_",y),
             rbind(get(paste0("resultados2_",y)),
                   mod))
      
    } else if(ano==2010) {
      
      modelo <- 
        rdd::RDestimate(get(y) ~ pop_anterior + Velocity,
                        data = rdd %>%
                          dplyr::filter(ANO_ELEICAO==ano) %>% 
                          dplyr::filter(NR_TURNO==turno) %>% 
                          dplyr::filter(DS_CARGO==cargo_n) %>% 
                          dplyr::filter(orientacao==orient),
                        bw=bw2,
                        cutpoint = cut,
                        cluster = rdd %>%
                          dplyr::filter(ANO_ELEICAO==ano) %>% 
                          dplyr::filter(NR_TURNO==turno) %>% 
                          dplyr::filter(DS_CARGO==cargo_n) %>% 
                          dplyr::filter(orientacao==orient) %>% 
                          pull(regiao))
      
      
      
      mod <- dplyr::tibble(Year=rep(ano,3),
                           Y=rep(y,3),
                           Office=cargo_n,
                           Orientation=orient,
                           Type=rep("Without covariates",3),
                           Model=names(modelo$est),
                           Bw=modelo$bw,
                           Observations=modelo$obs,
                           Estimates=modelo$est,
                           Std.Error=modelo$se,
                           Z=modelo$z,
                           p.value=modelo$p)
      
      assign(paste0("resultados2_",y),
             rbind(get(paste0("resultados2_",y)),
                   mod))} else {
                     
                     modelo <- 
                       rdd::RDestimate(get(y) ~ pop_anterior + Velocity,
                                       data = rdd %>%
                                         dplyr::filter(ANO_ELEICAO==ano) %>% 
                                         dplyr::filter(NR_TURNO==turno) %>% 
                                         dplyr::filter(DS_CARGO==cargo_m) %>% 
                                         dplyr::filter(orientacao==orient),
                                       bw=bw3,
                                       cutpoint = cut,
                                       cluster = rdd %>%
                                         dplyr::filter(ANO_ELEICAO==ano) %>% 
                                         dplyr::filter(NR_TURNO==turno) %>% 
                                         dplyr::filter(DS_CARGO==cargo_m) %>% 
                                         dplyr::filter(orientacao==orient) %>% 
                                         pull(regiao))
                     
                     
                     
                     mod <- dplyr::tibble(Year=rep(ano,3),
                                          Y=rep(y,3),
                                          Office=cargo_m,
                                          Orientation=orient,
                                          Type=rep("Without covariates",3),
                                          Model=names(modelo$est),
                                          Bw=modelo$bw,
                                          Observations=modelo$obs,
                                          Estimates=modelo$est,
                                          Std.Error=modelo$se,
                                          Z=modelo$z,
                                          p.value=modelo$p)
                     
                     assign(paste0("resultados2_",y),
                            rbind(get(paste0("resultados2_",y)),
                                  mod))
                     
                   }
    if(ano==2008){
      
      f <- as.formula(paste0(y,"~","pop_anterior + Velocity |",
                             gsub("60_anos_2000","pop_60_anos_2000",covs_2008)))
      
      
      modelo <- 
        rdd::RDestimate(f,
                        data = rdd %>%
                          dplyr::rename(pop_60_anos_2000=`60_anos_2000`) %>% 
                          dplyr::filter(ANO_ELEICAO==ano) %>% 
                          dplyr::filter(NR_TURNO==turno) %>% 
                          dplyr::filter(DS_CARGO==cargo_m) %>% 
                          dplyr::filter(orientacao==orient),
                        bw=bw1,
                        cutpoint = cut,
                        cluster = rdd %>%
                          dplyr::filter(ANO_ELEICAO==ano) %>% 
                          dplyr::filter(NR_TURNO==turno) %>% 
                          dplyr::filter(DS_CARGO==cargo_m) %>% 
                          dplyr::filter(orientacao==orient) %>% 
                          pull(regiao))
      
      
      
      mod <- dplyr::tibble(Year=rep(ano,3),
                           Y=rep(y,3),
                           Office=cargo_m,
                           Orientation=orient,
                           Type=rep("With covariates",3),
                           Model=names(modelo$est),
                           Bw=modelo$bw,
                           Observations=modelo$obs,
                           Estimates=modelo$est,
                           Std.Error=modelo$se,
                           Z=modelo$z,
                           p.value=modelo$p)
      
      assign(paste0("resultados2_",y),
             rbind(get(paste0("resultados2_",y)),
                   mod))
      
    } else if(ano==2010){
      
      f <- as.formula(paste0(y,"~","pop_anterior + Velocity |",
                             gsub("60_anos_2010","pop_60_anos_2010",
                                  covs_2010)))
      
      
      modelo <- 
        rdd::RDestimate(f,
                        data = rdd %>%
                          dplyr::rename(pop_60_anos_2010=`60_anos_2010`) %>% 
                          dplyr::filter(ANO_ELEICAO==ano) %>% 
                          dplyr::filter(NR_TURNO==turno) %>% 
                          dplyr::filter(DS_CARGO==cargo_n) %>% 
                          dplyr::filter(orientacao==orient),
                        bw=bw2,
                        cutpoint = cut,
                        cluster = rdd %>%
                          dplyr::filter(ANO_ELEICAO==ano) %>% 
                          dplyr::filter(NR_TURNO==turno) %>% 
                          dplyr::filter(DS_CARGO==cargo_n) %>% 
                          dplyr::filter(orientacao==orient)%>% 
                          pull(regiao))
      
      mod <- dplyr::tibble(Year=rep(ano,3),
                           Y=rep(y,3),
                           Office=cargo_n,
                           Orientation=orient,
                           Type=rep("With covariates",3),
                           Model=names(modelo$est),
                           Bw=modelo$bw,
                           Observations=modelo$obs,
                           Estimates=modelo$est,
                           Std.Error=modelo$se,
                           Z=modelo$z,
                           p.value=modelo$p)
      
      
      assign(paste0("resultados2_",y),
             rbind(get(paste0("resultados2_",y)),
                   mod))
      
      
      
    }   else {
      
      f <- as.formula(paste0(y,"~","pop_anterior + Velocity |",
                             gsub("60_anos_2010","pop_60_anos_2010",
                                  covs_2010)))
      
      
      modelo <- 
        rdd::RDestimate(f,
                        data = rdd %>%
                          dplyr::rename(pop_60_anos_2010=`60_anos_2010`) %>% 
                          dplyr::filter(ANO_ELEICAO==ano) %>% 
                          dplyr::filter(NR_TURNO==turno) %>% 
                          dplyr::filter(DS_CARGO==cargo_m) %>% 
                          dplyr::filter(orientacao==orient),
                        bw=bw3,
                        cutpoint = cut,
                        cluster = rdd %>%
                          dplyr::filter(ANO_ELEICAO==ano) %>% 
                          dplyr::filter(NR_TURNO==turno) %>% 
                          dplyr::filter(DS_CARGO==cargo_m) %>% 
                          dplyr::filter(orientacao==orient)%>% 
                          pull(regiao))
      
      mod <- dplyr::tibble(Year=rep(ano,3),
                           Y=rep(y,3),
                           Office=cargo_m,
                           Orientation=orient,
                           Type=rep("With covariates",3),
                           Model=names(modelo$est),
                           Bw=modelo$bw,
                           Observations=modelo$obs,
                           Estimates=modelo$est,
                           Std.Error=modelo$se,
                           Z=modelo$z,
                           p.value=modelo$p)
      
      
      assign(paste0("resultados2_",y),
             rbind(get(paste0("resultados2_",y)),
                   mod))
    }
  }
  
  assign(paste0("resultados2_",y),
         get(paste0("resultados2_",y)),
         envir = parent.frame())
}

r2_participacao_final <- c()

for(cut in c(20000,40000,60000)){

rodar_modelo2("participacao",
              bw1=resultados_participacao %>% 
                dplyr::filter(Year==2008,Cutoff==cut) %>% 
                .[["h"]],
              bw2=resultados_participacao %>% 
                dplyr::filter(Year==2010,Cutoff==cut) %>% 
                .[["h"]],
              bw3=resultados_participacao %>% 
                dplyr::filter(Year==2012,Cutoff==cut) %>% 
                .[["h"]],
              cut = cut)

  r_temp <- resultados2_participacao %>% 
    dplyr::mutate(Cut=cut)
  
  r2_participacao_final <- rbind(r2_participacao_final,r_temp)

}

r2_dif_part_final <- c()

for(cut in c(20000,40000,60000)){


rodar_modelo2("dif_part",
              bw1=resultados_dif_part %>% 
                dplyr::filter(Year==2008,Cutoff==cut) %>% 
                .[["h"]],
              bw2=resultados_dif_part %>% 
                dplyr::filter(Year==2010,Cutoff==cut) %>% 
                .[["h"]],
              bw3=resultados_dif_part %>% 
                dplyr::filter(Year==2012,Cutoff==cut) %>% 
                .[["h"]],
              cut = cut)
  
  r_temp <- resultados2_dif_part %>% 
    dplyr::mutate(Cut=cut)
  
  r2_dif_part_final <- rbind(r2_dif_part_final,r_temp)
  
}

r2_pct_bn_final <- c()
    
for(cut in c(20000,40000,60000)){  
  
rodar_modelo2_vs("pct_bn",
                 bw1=resultados_pct_bn %>% 
                   dplyr::filter(Year==2008,Cutoff==cut) %>% 
                   .[["h"]],
                bw2=resultados_pct_bn %>% 
                  dplyr::filter(Year==2010,Cutoff==cut) %>% 
                  .[["h"]],
                bw3=resultados_pct_bn %>% 
                  dplyr::filter(Year==2012,Cutoff==cut) %>% 
                  .[["h"]],
                cut = cut)
      
  r_temp <- resultados2_pct_bn %>% 
    dplyr::mutate(Cut=cut)
  
  r2_pct_bn_final <- rbind(r2_pct_bn_final,r_temp)
  
}

r2_dif_bn_final <- c()

for(cut in c(20000,40000,60000)){ 

rodar_modelo2_vs("dif_bn",
                 bw1=resultados_dif_bn %>% 
                   dplyr::filter(Year==2008,Cutoff==cut) %>% 
                   .[["h"]],
                 bw2=resultados_dif_bn %>% 
                   dplyr::filter(Year==2010,Cutoff==cut) %>% 
                   .[["h"]],
                 bw3=resultados_dif_bn %>% 
                   dplyr::filter(Year==2012,Cutoff==cut) %>% 
                   .[["h"]],
                 cut = cut)
  
  r_temp <- resultados2_dif_bn %>% 
    dplyr::mutate(Cut=cut)
  
  r2_dif_bn_final <- rbind(r2_dif_bn_final,r_temp)
  
}

r2_pct_vote_final <- c()

for(cut in c(20000,40000,60000)){

rodar_modelo2_vs("pct_vote",
                 bw1=resultados_pct_vote %>% 
                   dplyr::filter(Year==2008,Cutoff==cut) %>% 
                   .[["h"]],
                 bw2=resultados_pct_vote %>% 
                   dplyr::filter(Year==2010,Cutoff==cut) %>% 
                   .[["h"]],
                 bw3=resultados_pct_vote %>% 
                   dplyr::filter(Year==2012,Cutoff==cut) %>% 
                   .[["h"]],
                 cut = cut)
  
  r_temp <- resultados2_pct_vote %>% 
    dplyr::mutate(Cut=cut)
  
  r2_pct_vote_final <- rbind(r2_pct_vote_final,r_temp)
  
}

# Montar resultados finais
r_part <- r2_participacao_final %>% 
  dplyr::filter(Model=="LATE") %>% 
  dplyr::select(Year,Cut,Type,Bw,Observations,Estimates, Std.Error,p.value) %>% 
  dplyr::rename(Cutoff=Cut,
                Obs.=Observations,
                SE=Std.Error) %>% 
  dplyr::mutate(Year=as.character(Year),
                Cutoff=as.character(Cutoff)) %>% 
  dplyr::bind_rows(resultados_participacao %>% 
                     dplyr::select(Year,Cutoff,Type,h,Nh,Coef,SE,pvalue) %>% 
                     dplyr::filter(Cutoff=="Pooled") %>% 
                     dplyr::rename_all(list(~c("Year","Cutoff","Type","Bw",
                                               "Obs.","Estimates","SE",
                                               "p.value")))) %>% 
  dplyr::left_join(resultados_participacao %>% 
                      dplyr::select(Year,Cutoff,Weight))

r_dif_part <- r2_dif_part_final %>% 
  dplyr::filter(Model=="LATE") %>% 
  dplyr::select(Year,Cut,Type,Bw,Observations,Estimates, Std.Error,p.value) %>% 
  dplyr::rename(Cutoff=Cut,
                Obs.=Observations,
                SE=Std.Error) %>% 
  dplyr::mutate(Year=as.character(Year),
                Cutoff=as.character(Cutoff)) %>% 
  dplyr::bind_rows(resultados_dif_part %>% 
                     dplyr::select(Year,Cutoff,Type,h,Nh,Coef,SE,pvalue) %>% 
                     dplyr::filter(Cutoff=="Pooled") %>% 
                     dplyr::rename_all(list(~c("Year","Cutoff","Type","Bw",
                                               "Obs.","Estimates","SE",
                                               "p.value")))) %>% 
  dplyr::left_join(resultados_dif_part %>% 
                     dplyr::select(Year,Cutoff,Weight))

r_pct_bn <- r2_pct_bn_final %>% 
  dplyr::filter(Model=="LATE") %>% 
  dplyr::select(Year,Cut,Type,Bw,Observations,Estimates, Std.Error,p.value) %>% 
  dplyr::rename(Cutoff=Cut,
                Obs.=Observations,
                SE=Std.Error) %>% 
  dplyr::mutate(Year=as.character(Year),
                Cutoff=as.character(Cutoff)) %>% 
  dplyr::bind_rows(resultados_pct_bn %>% 
                     dplyr::select(Year,Cutoff,Type,h,Nh,Coef,SE,pvalue) %>% 
                     dplyr::filter(Cutoff=="Pooled") %>% 
                     dplyr::rename_all(list(~c("Year","Cutoff","Type","Bw",
                                               "Obs.","Estimates","SE",
                                               "p.value")))) %>% 
  dplyr::left_join(resultados_pct_bn %>% 
                     dplyr::select(Year,Cutoff,Weight))

r_bn_dif <- r2_dif_bn_final %>% 
  dplyr::filter(Model=="LATE") %>% 
  dplyr::select(Year,Cut,Type,Bw,Observations,Estimates, Std.Error,p.value) %>% 
  dplyr::rename(Cutoff=Cut,
                Obs.=Observations,
                SE=Std.Error) %>% 
  dplyr::mutate(Year=as.character(Year),
                Cutoff=as.character(Cutoff)) %>% 
  dplyr::bind_rows(resultados_dif_bn %>% 
                     dplyr::select(Year,Cutoff,Type,h,Nh,Coef,SE,pvalue) %>% 
                     dplyr::filter(Cutoff=="Pooled") %>% 
                     dplyr::rename_all(list(~c("Year","Cutoff","Type","Bw",
                                               "Obs.","Estimates","SE",
                                               "p.value")))) %>% 
  dplyr::left_join(resultados_dif_bn %>% 
                     dplyr::select(Year,Cutoff,Weight))

r_pct_vote <- r2_pct_vote_final %>% 
  dplyr::filter(Model=="LATE") %>% 
  dplyr::select(Year,Cut,Type,Bw,Observations,Estimates, Std.Error,p.value) %>% 
  dplyr::rename(Cutoff=Cut,
                Obs.=Observations,
                SE=Std.Error) %>% 
  dplyr::mutate(Year=as.character(Year),
                Cutoff=as.character(Cutoff)) %>% 
  dplyr::bind_rows(resultados_pct_vote %>% 
                     dplyr::select(Year,Cutoff,Type,h,Nh,Coef,SE,pvalue) %>% 
                     dplyr::filter(Cutoff=="Pooled") %>% 
                     dplyr::rename_all(list(~c("Year","Cutoff","Type","Bw",
                                               "Obs.","Estimates","SE",
                                               "p.value")))) %>% 
  dplyr::left_join(resultados_pct_vote %>% 
                     dplyr::select(Year,Cutoff,Weight))

r_sig <-
  dplyr::bind_rows(
    
    r2_participacao_final %>% 
      dplyr::filter(p.value<0.1) %>% 
      dplyr::select(Year,Model,Cut,Type,Bw,Observations,Estimates, Std.Error,p.value) %>% 
      dplyr::rename(Cutoff=Cut,
                Obs.=Observations,
                SE=Std.Error) %>% 
      dplyr::mutate(Year=as.character(Year),
                Cutoff=as.character(Cutoff),
                Outcome="Turnout"),
    
    r2_pct_bn_final %>% 
      dplyr::filter(p.value<0.1) %>% 
      dplyr::select(Year,Model,Cut,Type,Bw,Observations,Estimates, Std.Error,p.value) %>% 
      dplyr::rename(Cutoff=Cut,
                    Obs.=Observations,
                    SE=Std.Error) %>% 
      dplyr::mutate(Year=as.character(Year),
                    Cutoff=as.character(Cutoff),
                    Outcome="Blank and Null"),
    
    r2_pct_vote_final %>% 
      dplyr::filter(p.value<0.1) %>% 
      dplyr::select(Year,Model,Cut,Type,Bw,Observations,Estimates, Std.Error,p.value) %>% 
      dplyr::rename(Cutoff=Cut,
                    Obs.=Observations,
                    SE=Std.Error) %>% 
      dplyr::mutate(Year=as.character(Year),
                    Cutoff=as.character(Cutoff),
                    Outcome="Left Vote Share")) %>% 
  dplyr::arrange(Year,Cutoff,Type,Outcome)
  

# Testar modelo com os dados da anatel ####
# anatel <-SCM_2007_2018 %>% 
#   dplyr::filter(Ano==2008) %>%
#   dplyr::group_by(`Cod IBGE`) %>%
#   dplyr::summarise(conexoes=sum(valor)) %>%
#   dplyr::rename(COD_IBGE=`Cod IBGE`) %>%
#   dplyr::mutate(COD_IBGE=as.character(COD_IBGE)) %>% 
#   dplyr::left_join(censo) %>%
#   dplyr::left_join(eleic_2008_out %>%
#                      dplyr::filter(duplicated(COD_IBGE)==F) %>%
#                      dplyr::select(-DS_CARGO,-orientacao,-ANO_ELEICAO,
#                                    -localidade)) %>%
#   dplyr::left_join(back_2008) %>%
#   dplyr::mutate(regiao=case_when(substr(COD_IBGE,1,1)==1~"Norte",
#                                  substr(COD_IBGE,1,1)==2~"Nordeste",
#                                  substr(COD_IBGE,1,1)==3~"Sudeste",
#                                  substr(COD_IBGE,1,1)==4~"Sul",
#                                  substr(COD_IBGE,1,1)==5~"Centro-oeste"))
# 
# summary(anatel)
# 
# teste <- lm(participacao~conexoes+
#               `60_anos_2000`+
#               #renda_med_2000+
#               televisao_2000+
#               #linha_tel_2000+
#               rural_2000+
#               radio_2000+
#               pc_2000+
#               pea_2000,
#             data=anatel)
# 
# summary(teste)
# 
# ivmodel<-
# AER::ivreg(participacao~conexoes +
#              `60_anos_2000`+
#              #renda_med_2000+
#              televisao_2000+
#              #linha_tel_2000+
#              rural_2000+
#              radio_2000+
#              pc_2000+
#              pea_2000 |
#              . -conexoes+Velocity,
#               data=anatel)
# 
# summary(ivmodel)
# 
# library(ivpack)
# 
# # Erro-padrão clusterizado com região
# cluster.robust.se(ivmodel, anatel[row.names(ivmodel$model),]$regiao)
# 
# # Tentar fazer alguma coisa com a base de DDD
# 
# 
# get_brmap("City") %>% 
#   dplyr::mutate(City=as.character(City)) %>%
#   dplyr::left_join(dic_DDD %>%
#                      dplyr::mutate(`Código IBGE`=as.character(`Código IBGE`),
#                                    CoDIGO_NACIONAL=as.character(CoDIGO_NACIONAL)),
#                    by=c("City" = "Código IBGE")) %>% 
#   ggplot() +
#   geom_sf(aes(fill = CoDIGO_NACIONAL),
#           colour = "black", size = 0.05, alpha=0.5) +
#   #muda escala de cores
#   # scale_fill_viridis_d(option = 3,
#   #                      direction = -1) +
#   # scale_fill_distiller(palette="Blues",
#   #                      direction = 1)+
#   
#   geom_sf(data = get_brmap("State"),
#           fill = "transparent",
#           colour = "black", size = 0.8) +
#   
#   # tira sistema cartesiano
#   theme(panel.grid = element_line(colour = "transparent"),
#         panel.background = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         legend.position = "bottom") 
# 
# x<-
# resultado %>%
#   dplyr::filter(DS_CARGO=="PREFEITO") %>%
#   dplyr::left_join(censolegis) %>%
#   dplyr::mutate(COD_IBGE=as.character(COD_IBGE),
#                 ANO_ELEICAO=as.character(ANO_ELEICAO)) %>%
#   dplyr::left_join(dic_DDD %>%
#                      dplyr::mutate(`Código IBGE`=as.character(`Código IBGE`)),
#                    by=c("COD_IBGE"="Código IBGE")) %>%
#   dplyr::left_join(smp2009_2018, 
#                    by=c("CoDIGO_NACIONAL"="DDD","ANO_ELEICAO"="Ano")) %>%
#   dplyr::left_join(populacao %>%
#                      tidyr::gather("Ano","Populacao",c(2:13)) %>%
#                      dplyr::mutate(Ano=gsub("pop_","",Ano)),
#                    by=c("COD_IBGE"="COD_IBGE","ANO_ELEICAO"="Ano")) %>%
#   dplyr::group_by(CoDIGO_NACIONAL,ANO_ELEICAO,Tipo) %>%
#   dplyr::summarise(aptos=sum(aptos,na.rm = T),
#                    compareceu=sum(compareceu,na.rm = T),
#                    brancos=sum(brancos,na.rm = T),
#                    nulos=sum(nulos,na.rm = T),
#                    Populacao=sum(Populacao,na.rm = T),
#                    Valor=first(Valor)) %>%
#   dplyr::mutate(participacao=compareceu/aptos,
#                 pct_brancos=brancos/aptos,
#                 pct_nulos=nulos/aptos,
#                 acesso=Valor/Populacao) %>%
#   dplyr::filter(Tipo=="Banda Larga Móvel")
# 
# summary(lm(participacao~acesso,data=x))



                   