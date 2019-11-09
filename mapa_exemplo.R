teste <- SCM_2007_2018 %>% 
  dplyr::group_by(`Cod IBGE`,Ano) %>% 
  dplyr::summarise(n=sum(valor)) %>%
  dplyr::filter(Ano %in% c(2008,2010,2012))

pop <- populacao %>%
  tidyr::gather("Ano","pop",2:13) %>%
  dplyr::mutate(Ano=gsub("pop_","",Ano),
                COD_IBGE=as.numeric(COD_IBGE))

x<-
get_brmap("City") %>% 
  dplyr::left_join(teste, by=c("City" = "Cod IBGE")) %>% 
  dplyr::left_join(pop,
                   by=c("City" ="COD_IBGE",
                        "Ano"="Ano")) %>%
  na.omit %>%
  dplyr::mutate(value=n/pop*100,
                faixa=cut(value,
                          breaks=c(-Inf,quantile(value,c(.2,.4,.6,.8)),Inf),
                          labels = c("0",quantile(value,c(.2,.4,.6,.8))))) %>%
  ggplot() +
  geom_sf(aes(fill = faixa),
          colour = "black", size = 0.1) +
  #muda escala de cores
  # scale_fill_viridis_d(option = 3,
  #                      direction = -1) +
  scale_fill_brewer(palette="Blues",direction = -1)+
  
  geom_sf(data = get_brmap("State"),
          fill = "transparent",
          colour = "black", size = 0.8) +
  
  # tira sistema cartesiano
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  facet_wrap(~Ano)

p<-20000

ggplot(back_2008 %>%
         dplyr::filter(pop_anterior<100000) , 
                       aes(x = pop_anterior, 
                      y = velocidade,
                      colour=pop_anterior<p)) + 
  geom_point() + 
  xlim(p-5000,p+5000) + 
  ylim(0,20)+
  stat_smooth(method = loess)

y<-
  get_brmap("City") %>% 
  dplyr::left_join(teste, by=c("City" = "Cod IBGE")) %>% 
  dplyr::left_join(pop,
                   by=c("City" ="COD_IBGE",
                        "Ano"="Ano")) %>%
  na.omit %>%
  dplyr::mutate(value=n/pop*100,
                faixa=cut(value,
                          breaks=c(-Inf,quantile(value,c(.2,.4,.6,.8)),Inf),
                          labels = c("0",quantile(value,c(.2,.4,.6,.8))))) %>%
  dplyr::filter(Ano==2008)


p<-20000

ggplot(y, aes(x = pop, y = n,colour=pop>p)) + 
  geom_point() + 
  xlim(p-5000,p+5000) + 
  ylim(0,50)+
  stat_smooth(method = "loess")


