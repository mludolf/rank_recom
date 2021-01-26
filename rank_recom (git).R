
#################################################################
#################################################################
  #######                                               #######
   #####                                                 #####
    ###                                                   ###
    ###                                                   ###
    ###                                                   ###
    ###               TCC - OLIST CONSUMMERS              ###
    ###                                                   ###
    ###                                                   ###
    ###                                                   ###
    ###           ALUNOS:                                 ###
    ###                 GABRIEL ALMEIDA                   ###
    ###                 MATHEUS LUDOLF                    ###
    ###                                                   ###
    ###                                                   ###
    ###           ORIENTADOR:                             ### 
    ###                 GUSTAVO FERREIRA                  ###
    ###                                                   ###
    ###                                                   ###
    ###                                                   ###
    ###                                                   ###
    ###                                                   ###
   #####                                                 #####
  #######                                               #######
#################################################################
#################################################################


# CONFIG ------------------------------------------------------------------

## Pacotes
for(i in 1){
  list.of.packages <- c("ggplot2"
                        ,"tidyverse"
                        ,"openxlsx"
                        ,"lubridate"
                        ,"reshape2"
                        ,"stringi"
                        ,"recommenderlab"
                        ,"cluster"
                        ,"Rtsne"
                        ,"maps"         # mapas simples, eixos, escala, cidades 
                        ,"mapdata"      # base de dados WorldHires e rios
                        ,"rworldmap"    # outra base de dados de mapas do mundo
                        ,"maptools"     # Ler ESRI shapefiles 
                        ,"mapproj"      # Projeções e grids
                        ,"ggmap"        # Gmaps, OSM + mapas baseados em ggplot2
                        ,"rgdal"
                        ,"sp"
                        ,"rgeos"
                        ,"sf"
                        ,"ggplot2"
                        #,"clustMixType"
                        ,"purrr"
                        #,"berryFunctions"
                        ,"gtools"          # rdirichlet
                        )
  
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)
  
  for(i in list.of.packages) library(i, character.only = TRUE); rm(i, list.of.packages, new.packages)
}


## Setando o caminho para o diretorio do arquivo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

dump_env = function(){
  # Esta função serve para limpar o cache do environment, sem afetar a base
  objs <- ls(pos = ".GlobalEnv")
  rm(list = objs[(objs != "base") & (objs != "dump_env")], pos = ".GlobalEnv")
}

dump_env()

# LEITURA -----------------------------------------------------------------

cliente =     read_csv('../Bases/olist_customers_dataset.csv')
cliente.geo = read_csv('../Bases/olist_geolocation_dataset.csv')
ordem.item =  read_csv('../Bases/olist_order_items_dataset.csv')
ordem.pag =   read_csv('../Bases/olist_order_payments_dataset.csv')
ordem.op =    read_csv('../Bases/olist_order_reviews_dataset.csv')
ordem =       read_csv('../Bases/olist_orders_dataset.csv')
produto =     read_csv('../Bases/olist_products_dataset.csv')
vendedor =    read_csv('../Bases/olist_sellers_dataset.csv')
produto.cat = read_csv('../Bases/product_category_name_translation.csv')

pib.pc =      read.xlsx('../Bases/ext_pib_pc_municipal_2017.xlsx')

# REFERENCIA PIB PC
# PIB Municipal e População (ref mais recente é o pib de 2017)
# https://sidra.ibge.gov.br/pesquisa/pib-munic/tabelas
# https://www.ibge.gov.br/estatisticas/sociais/populacao

cep = read.xlsx('../Bases/ext_ceps_2018.xlsx')

# REFERENCIA CEPS
# Ano de ref: 2018 - Declara ser do site dos correios
# http://cep.la/baixar


mun.shape = read_sf('../Mapas/BR_Municipios_2019.shp')

# Referencia mapa de municípios
# Ano de ref: 2019
# https://www.ibge.gov.br/geociencias/organizacao-do-territorio/15774-malhas.html



# # ANALISE DESCRITIVA ------------------------------------------------------
# 
# ## Base ORDEM DE PRODUTOS
# 
# length(ordem$order_id)
# 
# ### QUANTIDADE DE PEDIDOS NO TEMPO
# 
# g_ordem = ordem %>%
#   filter(order_status=='delivered') %>%
#   mutate(ano_mes = format(as.Date(order_purchase_timestamp, format = "%Y-%m-%d"),"%Y-%m")) %>%
#   group_by(ano_mes) %>%
#   select(order_id) %>%
#   count() %>%
#   as.data.frame()
# 
# sum(g_ordem$n)
# 
# g_ordem$ano_mes2 = as.Date.character(sapply(g_ordem$ano_mes,FUN = function(x) paste0(x,"-01")),format = "%Y-%m-%d")
# plot(g_ordem$n, x = g_ordem$ano_mes2, type="l")
# 
# 
# 
# ### STATUS DOS PEDIDOS
# 
# status_pedidos = as.data.frame(table(ordem$order_status))
# status_pedidos$perc = round(status_pedidos$Freq/sum(status_pedidos$Freq)*100,3)
# status_pedidos
# # Gráfico
# barplot(height = status_pedidos$perc,names.arg = status_pedidos$Var1,las=2, ylim = c(0,100))
# 
# 
# # QUANTIDADE TOTAL DE CLIENTES
# length(unique(cliente$customer_id))
# length(unique(cliente$customer_unique_id))
# 
# # Os clientes únicos representam quase que a totalidade da base. Portanto, a análise de sugestão de pedidos 
# # fica comprometida pois não há forma de "trackear" a atividade dos usuários. Uma maneira de fazer isto seria
# # utilizar da região da compra.
# 
# 
# 
# 
# 
# ## Base ORDEM ITEM
# 
# length(unique(ordem.item$order_id))
# length(unique(cliente$customer_id))
# length(unique(cliente$customer_unique_id))
# length(unique(cliente$customer_zip_code_prefix))
# length(unique(cliente.geo$geolocation_zip_code_prefix))
# 
# 
# length(unique(produto$product_category_name))
# length(unique(vendedor$seller_zip_code_prefix))
# 
# 
# #
# 
# 
# 
# 
# 


# JUNCAO DE BASES ---------------------------------------------------------

length(unique(ordem$order_id)) # "order_id" é chave primária para a base ORDEM.

# Temos 775 ordem sem designação de produtos. Como essas ordens não possuem esta
# associação, não são úteis para nossa análise e serão filtradas.
sum(ordem$order_id %in% ordem.item$order_id) 
sum(!ordem$order_id %in% ordem.item$order_id)

# Vamos verificar se temos vendas a mais na base ORDEM.ITEM para testar a integridade:
sum(ordem.item$order_id %in% ordem$order_id)
sum(!ordem.item$order_id %in% ordem$order_id) # OK!





# Para o REVIEW das ordens temos de realizar a mesma checagem.
sum(ordem$order_id %in% ordem.op$order_id)
sum(!ordem$order_id %in% ordem.op$order_id) # Todas as ordens possuem avaliação

sum(ordem.op$order_id %in% ordem$order_id)
sum(!ordem.op$order_id %in% ordem$order_id) # A volta é válida

# Não existem avaliações de ordens inexistentes. PORÉM, há mais avaliações do que ordens.
# Como não há maneira de linkar a avaliação ao produto segundo as variáveis descritas na
# base, o melhor a fazer é agrupar pelo ID da ordem e fazer uma média ou arredonde do review.
length(unique(ordem.op$order_id)) # Igual ao número de ordens, vamos seguir...

# Por opção de análise, foi feito o arredonde para o inteiro mais próximo
order.op.n = ordem.op %>% group_by(order_id) %>% summarise(review = as.integer(round(mean(review_score), 0)))
nrow(order.op.n) # Show!





# A avaliação dos clientes através da base customer.
length(unique(cliente$customer_id)) # Ligado diretamente ao número de ordens
length(unique(cliente$customer_unique_id)) # Identifica o mesmo usuário

# O número de usuários com mais de uma compra são pequenos: 3345 apenas
length(unique(cliente$customer_id)) - length(unique(cliente$customer_unique_id))

# Se o cliente for o menor grão da base, isto implicará em um algoritmo de recomendação ruim
# devido à pobreza da base no que se refere ao histórico de consumo pelos usuários.
# Para tentar contornar este problema, uma alternativa que adotaremos mais adiante será adotar
# como menor grão o Sufixo do ZIP Code.





# Identificamos na base de Produtos que existem 610 produtos sem descrições de categorias.
# Esses produtos têm apenas descrição de tamanho e peso e portanto não irão compor a base final.
sum(is.na(produto$product_category_name))
produto.n = produto[!is.na(produto$product_category_name),]
nrow(produto.n) == length(unique(produto.n$product_id)) # product_id é chave primária





# Meio de pagamento
length(unique(ordem.pag$order_id))

z1 = ordem.pag %>%
  group_by(order_id) %>%
  count() %>% 
  filter(n > 1)

z2 = left_join(ordem.pag[ordem.pag$order_id %in% z1$order_id,], z1)
nrow(ordem.pag[ordem.pag$order_id %in% z1$order_id,]) == nrow(z2) # Ok

unique(z2$payment_type)
length(unique(z2$order_id))

# Apenas voucher
z3 = z2[!z2$order_id %in% z2[z2$payment_type != 'voucher', ]$order_id,]
length(unique(z3$order_id))

# Como não temos informação prévia de "possibilidades de pagamento" para cada produto, acaba
# não sendo uma informação confiável para o algorítmo de recomendação. Além disso, temos 2961
# ordens com mais de um método de pagamento empregado, sendo dividido em vouchers + cartão em
# sua grande maioria: Das 2961 ordens, 427 são pagas APENAS com voucher.

rm(z1, z2, z3)





# Cliente - Geolocalização !!!! NÃO VAMOS USAR... AS INFORMAÇÕES DA BASE CLIENTES BASTAM !!!!

# length(unique(cliente.geo$geolocation_zip_code_prefix)) # 19015
# unique(cliente.geo$geolocation_state) # 27 - OK
# 
# # O nome das cidades está com problemas. O código abaixo tenta resolver este problema.
# cliente.geo.n = cliente.geo %>%
#   mutate(
#     geolocation_city = toupper(str_replace_all(
#       iconv(geolocation_city, to = "ASCII//TRANSLIT"),"[^a-zA-Z0-9]", ""))
#   ) %>%
#   select(geolocation_zip_code_prefix,geolocation_city,geolocation_state) %>%
#   unique(.)
# 
# nrow(cliente.geo.n)
# 
# nl = (cliente.geo.n %>% group_by(geolocation_zip_code_prefix) %>%
#   count() %>% 
#   filter(n>1) %>%
#   select(geolocation_zip_code_prefix) %>%
#   as.data.frame(.))
# 
# # checar os repetidos
# View(cliente.geo.n[cliente.geo.n$geolocation_zip_code_prefix %in% nl$geolocation_zip_code_prefix,])
# 
# # O preenchimento das cidades desta base está muito aquém do esperado, deixando a desejar, e portanto
# # não é possível utilizarmos este campo confiavelmente. A melhor opção seria trazer este campo de outra
# # fonte ou simplesmente não utiliza-lo, trabalhando apenas com o prefixo do zip e o estado associado.
# 
# cliente.geo.n2 = cliente.geo %>%
#   select(geolocation_zip_code_prefix,geolocation_state) %>%
#   unique(.)
# 
# nl2 = (cliente.geo.n2 %>% group_by(geolocation_zip_code_prefix) %>%
#         count() %>% 
#         filter(n>1) %>%
#         select(geolocation_zip_code_prefix) %>%
#         as.data.frame(.))
# 
# # checar os repetidos
# View(cliente.geo.n2[cliente.geo.n2$geolocation_zip_code_prefix %in% nl2$geolocation_zip_code_prefix,])
# 
# # Mesmo assim, ainda temos alguns zips com estados repetidos, talvez mesmo estes sejam importantes
# # buscar em uma fonte externa. Esta checagem é válida para que dê mais fidelidade ao dado.
# # Após a captação dos ceps, foi feita a adaptação manual para que ficasse condizente com o nível
# # de granularidade da base - prefixo de cinco números do cep - e pudesse ser anexado.
# 
# length(unique(cep$zip_pref))
# 
# nl3 = (cep %>% group_by(zip_pref) %>%
#          count() %>% 
#          filter(n>1) %>%
#          select(zip_pref) %>%
#          as.data.frame(.))
# 
# # checar os repetidos
# View(cep[cep$zip_pref %in% nl3$zip_pref,])
# 
# # As duplicidades de ceps analisadas manualmente, devido à baixa quantidade, confirmaram que de
# # fato, alguns municípios possuem o mesmo prefixo e não estão necessariamente próximos, porém 
# # sempre na mesma unidade federativa. Este é o caso de Cumaru e São José da Coroa Grande, em
# # Pernambuco, separados por 150 Kilometros de distância. 
# 
# Para ver a tabela abaixo é necessário rodar a base
# View(base[(!base$customer_zip_code_prefix %in% cep$zip_pref),])
#
# rm(nl, nl2, nl3)






# PIB per Capita

# Para adicionar o PIB per capita municipal, segundo os dados da base, foi necessário utilisar de
# um recurso não muito prático, pois o campo de "cidades" estava mal preenchido e a base não possuia
# alguma sigla de referência para as regiões geográficas. A saída encontrada consistiu em baixar
# o arquivo com os municípios mapeados por uma fonte oficial e verificar a geolocalização dos
# pontos em relação a estes municípios. O tempo de processamento desta etapa é levemente custoso,
# uma vez que a capilaridade é muito grande, e identificar a localização de cada usuário da base 
# de clientes pode levar pelo menos 25 segundos entre a leitura do arquivo do mapa e a 
# identificação pelo algoritmo.

# Adicionando o Código do município (IBGE) na base clientes.geo
pnts = data.frame("x" = cliente.geo$geolocation_lat, "y" = cliente.geo$geolocation_lng)

pnts_sf <- st_as_sf(pnts, coords = c('y', 'x'), crs = st_crs(mun.shape))

pnts <- pnts_sf %>% 
  mutate(
    intersection = as.integer(st_intersects(geometry, mun.shape)),
    mun_ref = if_else(is.na(intersection), '', mun.shape$CD_MUN[intersection]),
    mun = if_else(is.na(intersection), '', mun.shape$NM_MUN[intersection]),
    uf = if_else(is.na(intersection), '', mun.shape$SIGLA_UF[intersection])
  ) 

# A junção revelou algumas localizações geográficas referenciadas fora do Brasil. Embora uma
# checagem manual no Google revelou que os nomes das cidades estavam corretos em sua destinação,
# as unidades federativas brasileiras ainda estavam atreladas. É possível que tenha havido algum erro
# conceitual na construção da base por parte da Olist, então a opção tomada foi retirar a
# informação desses clientes. Caso essa decisão impactasse fortemente na base, talvez fosse 
# necessário mais um tratamento, usando uma metodologia que pudesse complementar os dados
# faltantes, mas no final o impacto foi mínimo e não se fez necessário.

hist(pib.pc.n$pib_pc)
mean(pib.pc.n$pib_pc)

pib.pc.n = pib.pc %>%
  mutate(pib_pc_q5 = classify(pib_pc, method = "quantile", breaks = 5)$index,
         pib_pc_q10 = classify(pib_pc, method = "quantile", breaks = 10)$index)

cliente.geo.n = bind_cols(cliente.geo,as.data.frame(pnts)[,c(2:5)]) %>% # OK - Perfeito!
  filter(mun_ref!='') %>%
  select(geolocation_zip_code_prefix, mun_ref, uf) %>%
  distinct() %>%
  select(-uf) %>%
  inner_join(pib.pc.n, by = c('mun_ref'='cd_ref')) # OK - cd_ref é PK em pib.pc 
# Edit: 4300002 não tem pib_pc, mas tem ordens na base de lá
# daí precisei fazer inner_join pra não trazer esse mun.

length(unique(cliente.geo.n$mun)) # OK - Menor que 5570, total de munic segundo o IBGE.

# Por opção, os ceps que aparecerem repetidos na base de clientes.geo serão tratados de maneira
# que PREVALEÇA o município de maior população.

aux_1 = as.data.frame(table(cliente.geo.n$geolocation_zip_code_prefix)) %>%
  filter(Freq == 1) %>%
  select(Var1)

aux_2 = as.data.frame(table(cliente.geo.n$geolocation_zip_code_prefix)) %>%
  filter(Freq > 1) %>%
  select(Var1)

# Forçando com que CEPs repetidos sejam da maior população
cliente.geo.n.2 = cliente.geo.n %>%
  filter(geolocation_zip_code_prefix %in% aux_1$Var1) %>%
  
  bind_rows(cliente.geo.n %>%
              filter(geolocation_zip_code_prefix %in% aux_2$Var1) %>%
              group_by(geolocation_zip_code_prefix) %>%
              summarise(pop = max(pop)) %>%
              left_join(cliente.geo.n %>%
                          filter(geolocation_zip_code_prefix %in% aux_2$Var1), by = c("geolocation_zip_code_prefix", "pop"))
  ) %>%
  select(-cd_mun, -pib, -pop)



length(unique(cliente.geo.n.2$geolocation_zip_code_prefix)) 

# Agora temos uma informação confiável de dados de cliente, com CEPs como Chave Primária para esta base, permitindo a junção
# com a base principal. 

rm(aux_1, aux_2, pnts, pnts_sf, cliente.geo.n)







# Vias de fato ----- FAZENDO A JUNÇÃO DA BASE -------------------------------------------------------
base = ordem %>%
  select(1:4) %>% 
  
  inner_join(ordem.item, by = 'order_id') %>% # Ok - Apenas as ordens que possuem produtos
  left_join(order.op.n, by = 'order_id') %>% # Ok
  filter(order_status == 'delivered') %>% # Filtrando compras concretizadas
  
  left_join(cliente[,1:3], by = 'customer_id') %>% # Ok
  
  inner_join(cliente.geo.n.2, by = c('customer_zip_code_prefix'='geolocation_zip_code_prefix')) %>% # Ok
  
  inner_join(produto.n, by = 'product_id') %>% # Ok - Apenas produtos com descrição
  
  mutate(
    dia_faixa = case_when(
      between(hour(order_purchase_timestamp),0,5)   ~ "00 -| 06",
      between(hour(order_purchase_timestamp),6,11)  ~ "06 -| 12",
      between(hour(order_purchase_timestamp),12,17) ~ "12 -| 18",
      between(hour(order_purchase_timestamp),18,23) ~ "18 -| 00"),
    
    dia_semana = weekdays(order_purchase_timestamp),
    
    mes = month(order_purchase_timestamp, label = TRUE),
    
    mes_quinzena = case_when(
      between(day(order_purchase_timestamp),1,15) ~ 'Primeira',
      day(order_purchase_timestamp) > 15 ~ 'Segunda')
  ) %>%
  mutate(
    customer_unique_id = as.factor(customer_unique_id),
    customer_zip_code_prefix = as.factor(customer_zip_code_prefix),
    mun_ref = as.factor(mun_ref),
    cd_uf = as.factor(cd_uf),
    uf = as.factor(uf),
    mun = as.factor(mun),
    dia_faixa = as.factor(dia_faixa),
    dia_semana = as.factor(dia_semana),
    mes = as.factor(as.character(mes)),
    mes_quinzena = as.factor(mes_quinzena)
  ) #%>%
#as.data.frame()

glimpse(base)

## Testando alguns outputs
# summary(base)
# sum(is.na(base$pic_pc))
# sum(is.na(base$product_category_name))
# sum(is.na(base$mun_ref))
# estudo = base[is.na(base$pic_pc),]















# !!!!!!!! IMPORTANTE !!!!!!!!  

# Filtro Status da ordem (PARA VER A ANALISE ABAIXO É PRECISO COMENTAR O FILTRO NA JUNÇÃO)

# A base continha 2131 ordens com status diferentes de 'entregue'. Essas ordens representavam
# 2387 produtos no total. Cerca de 50% desses produtos possuiam status como 'aprovado', porém
# como seria necessário entendendimento mais profundo dos dados para saber se isto significaria
# que os produtos com esse status,  ou qualquer diferente daqueles que foram entregues, de
# fato chegaram aos destinatários, foi feita a opção por removê-los da base.

# unique(base$order_status)
# length(unique(base[base$order_status != 'delivered',]$order_id))
# sum(base$order_status != 'delivered')
# View(base[base$order_status != 'delivered',])

# aux = as.data.frame(table(base[base$order_status != 'delivered',]$order_status)) %>%
#   arrange(desc(Freq))
# names(aux) = c('Status','Freq')

# ggplot(data = aux, aes(x = sort(Status), y = Freq)) +
#   geom_bar(stat = "identity", fill = "steelblue")+
#   geom_text(aes(label=Freq), vjust= -0.55, color="black",
#             position = position_dodge(), size=3.5)+
#   ylim(0, max(aux$Freq + 100)) +
#   xlab('Status') +
#   ylab('Produtos') +
#   theme_minimal()

##### rm(aux)


# Temos uma quantidade razoavelmente baixa de vendedores para o número de produtos vendidos
# Pode ser que o vendedor influencie no momento da compra... O estado do vendedor pode ser
# captado através da base "vendedor"
length(unique(base$seller_id))


write_csv(base, '../Bases/Output/base_clean.csv')








# METODOLOGIA -------------------------------------------------------------

# attach(base)
# detach(base)

# <><><><><><><><> Dump de memória, caso seja necessário <><><><><><><><>
# rm(list = ls()[ls() != "base"])

# names(base)
### !!!!!!!!!!! NÃO DEU CERTO PELO PACOTE RECOMMENDERLAB !!!!!!!!!!!!!!! 
# # Criando a matriz de usuários com relação aos pedidos de cada categoria.
# # Não é necessário adicionar o fator temporal
# comp.mat = dcast(base, 
#                  customer_zip_code_prefix ~ product_category_name,
#                  value.var = "product_category_name",
#                  fun.agg = function(x) length(x)) %>%
#   select(-1) %>%  # Remove a informação de usuário
#   as.matrix
#   
# # POR PRODUTO EXPLODE A MEMÓRIA RAM, IMPOSSÍVEL SEM UM SERVIDOR
# # 14.695 * 31.589 = 46.420.0355      matriz muito absurdamente grande (product_id)
# # 14.695 * 73 = 1.072.735            mais factível, roda liso
# 
# 
# 
# # Para realizar a recomendação no R, usaremos o "recommenderlab". Esse pacote nos dá a Filtragem
# # Colaborativa, calculando a similaridade entre o consumo dos usuários. 
# 
# 
# ## A PARTIR DAQUI NÃO FUNCIONA PARA NÓS, POIS COMO A VARIÁVEL RESPOSTA É DIFERENTE, ESTE METODO FALHA
# ## Ref: https://www.data-mania.com/blog/how-to-build-a-recommendation-engine-in-r/ (Furada)


# # Esse daqui esplicita os metodos do pacote.
# ## !!!!! NÃO CONSEGUI GERAR UMA RECOMENDAÇÃO PELO RECOMMENDERLAB !!!!!
# # https://rpubs.com/jeknov/movieRec
# # A metodologia está toda descrita no help! ("user guides")
# 
# teste = as(comp.mat, "realRatingMatrix")
# 
# ## Entendendo o método de recomendação
# #recommenderRegistry$get_entries(dataType = "realRatingMatrix")
# 
# # O mais perto que eu vi foi o POPULAR, mas não consegui implementar uma similaridade
# # entre os usuários, daí, pelo que eu entendi, não fez os clusters. O método é esse, mas
# # não tá implementado. Acho que vamos ter que implementar...
# 
# rec_mod = Recommender(data = teste, method = "RERECOMMEND")
# (as.data.frame(as(predict(rec_mod, teste[1], n=5), "list")))
# 
# rec_mod = Recommender(data = teste, method = "UBCF", param=(list(method="pearson",
#                                                                  nn=1,
#                                                                  min_matching_items = 5)))
# 
# (as.data.frame(as(predict(rec_mod, teste[1,], n=10), "list")))
# 
# rec_mod = Recommender(data = teste, method = "POPULAR")
# names(getModel(rec_mod))
# 
# as(predict(rec_mod, teste[1000], type="ratingMatrix"),"list")[[1]] %>%
#   as.data.frame %>%
#   arrange(desc(.)) %>%
#   head(5)
# 
# # Esse não serve de nada
# (as(getModel(rec_mod)$topN, "list"))[[1]][1:5]
# 
# sum(comp.mat$cama_mesa_banho)
# sum(comp.mat$beleza_saude)
# sum(comp.mat$esporte_lazer)
# sum(comp.mat$moveis_decoracao)
# sum(comp.mat$informatica_acessorios)



# dump_env()

# Vou começar a implementação daqui

# FILTRAGEM COLABORATIVA

# ## SEGREGANDO A BASE PARA "TREINO" E "TESTE"
# for(i in 1){
#   dt_lim = '2018-06-01' # 3 meses antes do fim
#   #dt_lim = '2018-08-01' # 1 meses antes do fim
#   
#   base.trn = base %>%
#     filter(order_purchase_timestamp < dt_lim) 
#   
#   base.tst = base %>%
#     filter(order_purchase_timestamp >= dt_lim)
#   
#   # Percentual da data escolhida
#   print(paste0(round(nrow(base.tst)/nrow(base.trn)* 100,2), " %")) 
#   rm(dt_lim)
# }
# 
# names(base.trn)






# ## !!!!! TENTATIVA DE REALIZAR A CLUSTERIZAÇÃO. MAS NÃO DEVE SER FEITO DESTA MANEIRA !!!!!!!!!!!
# # Desta maneira perde-se a sensibilidade temporal e torna clusteriza o cep diretamente
# # Isso é um problema pois a ideia era que a gente pudesse recomendar utilizando, além da
# # localização, as informações extras do período, como o dia da seman, a hora, etc, e dessa maneira
# # apenas o CEP "seria suficiente" para identificar o local. Daí é como se fosse uma "interseção" entre
# # colaborativa e baseada em conteúdo.
# 
# # Reshaping as variáveis
# # "mun_ref"
# # "uf"
# # "dia_faixa"
# # "mes"
# # "mes_quinzena"
# 
# explode = function(x, nome = ""){
#   aux = x %>%
#     as.data.frame() %>%
#     group_by_all() %>%
#     count() %>%
#     spread(as.name(nome),n, fill = 0)
#   return(aux)
#   }
# 
# spread_dia_faixa=
#   explode(x = base.trn %>% select(customer_zip_code_prefix,dia_faixa), nome = "dia_faixa")
# 
# spread_mes=
#   explode(x = base.trn %>% select(customer_zip_code_prefix,mes),nome = "mes")
# 
# spread_mes_quinzena=
#   explode(x = base.trn %>% select(customer_zip_code_prefix,mes_quinzena), nome = "mes_quinzena")
# 
# spread_categoria=
#   explode(x = base.trn %>% select(customer_zip_code_prefix,product_category_name), nome = "product_category_name")
# 
# 
# 
# # explode_bool = function(x, nome = ""){
# #   aux = x %>%
# #     as.data.frame() %>%
# #     distinct() %>%
# #     bind_cols(n = rep(1, nrow(.))) %>%
# #     spread(as.name(nome),n, fill = 0)
# #   return(aux)
# #   }
# 
# # # BASES SPREADS BOOL
# # spread_mun_ref=
# #   explode_bool(x = base.trn %>% select(customer_zip_code_prefix,mun_ref),nome = "mun_ref")
# #
# # spread_uf=
# #   explode_bool(x = base.trn %>% select(customer_zip_code_prefix,uf), nome = "uf")
# 
# 
# 
# 
# ## Gerando a base explodida
# names(base.trn)
# 
# b.trn.cf = base.trn %>%
#   select("customer_zip_code_prefix"   ,
#          "mun_ref"                    ,
#          "uf"                         ,
#          "price"                      ,
#          "freight_value"              ,
#          "review"                     ,
#          "pic_pc"                     ,
# 
#          "order_purchase_timestamp"   ,
#          "shipping_limit_date"
#          ) %>%
#   group_by(customer_zip_code_prefix) %>%
#   summarise(
#     prc_med = round(mean(price),2),
#     frt_med = round(mean(freight_value),2),
#     rev_med = round(mean(review),2),
#     pic_pc  = round(mean(pic_pc),2),
#     shp_lmt_m = round(mean(shipping_limit_date - order_purchase_timestamp),2),
#     mun_ref = unique(mun_ref),
#     uf = unique(uf)
#     ) %>%
#   left_join(spread_dia_faixa, by = "customer_zip_code_prefix") %>%
#   left_join(spread_mes, by = "customer_zip_code_prefix") %>%
#   left_join(spread_mes_quinzena, by = "customer_zip_code_prefix") %>%
#   #left_join(spread_mun_ref, by = "customer_zip_code_prefix") %>%
#   #left_join(spread_uf, by = "customer_zip_code_prefix") %>%
#   left_join(spread_categoria, by = "customer_zip_code_prefix") %>%      # VARIAVEL RESPOSTA
#   mutate(
#     shp_lmt_m = as.numeric(shp_lmt_m),
#     mun_ref= as.factor(mun_ref),
#     uf = as.factor(uf)
#   ) %>%
#   as.data.frame()
# 
# 
# 
# 
# # REALIZAÇÃO DA CLUSTERIZAÇÃO DE CEPS POR AFINIDADE
# set.seed(100)
# 
# #cbind(names(b.trn.cf),c(1:99))
# gower_dist <- daisy(b.trn.cf[,c(2:26)], metric = "gower")
# gower_mat <- as.matrix(gower_dist)
# 
# 
# View(b.trn.cf[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ])
# View(b.trn.cf[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ])
# 
# 
# sil_width <- c(NA)
# for(i in 1:40){
#   pam_fit <- pam(gower_dist, diss = TRUE, k = i)
#   sil_width[i] <- pam_fit$silinfo$avg.width
# }
# 
# plot(15:19, sil_width[15:19],
#      xlab = "Number of clusters",
#      ylab = "Silhouette Width")
# lines(15:19, sil_width[15:19])






# ###     !!!!!   Não rodar! Aqui eu tava vendo a distribuição de cada categoria.
# # Estudo de categorias
# 
# aux = base %>%
#   group_by(product_category_name) %>%
#   count() %>%
#   arrange(desc(n)) %>%
#   bind_cols(acum = cumsum(.$n)/sum(.$n))
# 
# 
# plot(1:73, aux$n)
# plot(1:73, aux$acum)
# 
# c(sum(aux$acum < 0.98),sum(aux$acum < 0.97),sum(aux$acum < 0.96),sum(aux$acum < 0.95),sum(aux$acum < 0.94),
#   sum(aux$acum < 0.93),sum(aux$acum < 0.92),sum(aux$acum < 0.91),sum(aux$acum < 0.90))
# 
# write.xlsx(aux, 'teste.xlsx')





# APRESENTACAO DE RESULTADOS ----------------------------------------------
names(base)

## k-prototype
# Fontes
# http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.94.9984
# https://www.rdocumentation.org/packages/clustMixType/versions/0.2-5/topics/kproto
# https://journal.r-project.org/archive/2018/RJ-2018-048/RJ-2018-048.pdf

## AINDA ESTOU TESTANDO!!!!!!!!!!!
# trn.clst = base.trn %>%
#   select(price, mun_ref, uf, pic_pc, dia_faixa, dia_semana,mes,mes_quinzena)
# 
# glimpse(trn.clst)
# 
# elbow <- NA
# m = 1
# 
# for(i in c(100, 120, 140)){
#   kpres <- kproto(trn.clst, k = i, nstart = 2)
#   elbow[m] <- kpres$tot.withinss
#   m = m+1
# }
# 
# plot(1:(m-1), elbow, type = "b", ylab = "Objective Function", xlab = "# Clusters",
#      main = "Scree Plot")





# ### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ### !!!                                          !!!
# ### !!!                IMPORTANTE                !!!
# ### !!!                                          !!!
# ### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# # Tentando implementar a solução encontrada em:
# # https://towardsdatascience.com/clean-a-complex-dataset-for-modelling-with-recommendation-algorithms-c977f7ba28b1
# #
# # Conceito: MARKET BASKET ANALYTICS
# #
# # Esta solução é LEGAL na hora de recomendar produtos novos, como se a pessoa tivesse consumido alguns e a gente
# # quisesse recomendar algo após isso. Essa situação se aplica em dois casos, no histórico, e em uma possivel
# # compra "conjunta" (ou cross-sell). A crítica que eu (Matheus) enxergo é que por esta técnica não é recomendado
# # um produto que foi comprado anteriormente. Na minha concepção isso é uma falha estratégica. Imagine a situação:
# # Compra de ração de pets, deveria ser sugerido a que compra sempre. O mesmo acontece com papelaria, produtos de
# # beleza, entre outros. Vamos guardar essa carta na manga e caso dê tempo a gente implementa em conjunto com a ideia
# # da recomendação de categoria. Poderiamos inclusive fazer 'tipo' um site, onde esse algoritmo rodaria no carrinho
# # de compras.
# 
# ratings_matrix <- base %>%
#   select(customer_zip_code_prefix, product_category_name) %>% # Verificando "QUEM JÁ COMPROU DE DETERMINADA CATEGORIA"
#   distinct() %>%
#   mutate(value = 1) %>%
#   spread(product_category_name, value, fill = 0) %>%
#   select(-customer_zip_code_prefix) %>%
#   # Convert to matrix
#   as.matrix() %>%
#   # Convert to recommenderlab class 'binaryRatingsMatrix'
#   as("binaryRatingMatrix")
# 
# ratings_matrix
# ## 14695 x 73 rating matrix of class 'binaryRatingMatrix' with 71009 ratings.
# 
# scheme <- ratings_matrix %>%
#   evaluationScheme(method = "cross", # Cross-validation
#                    k = 5,            # k-fold
#                    train = 0.8,      # Tamanho da amostra de treino
#                    given = -1)
# 
# # Testando vários algoritmos no recommenderlab
# algorithms <- list(
#   "association rules" = list(name  = "AR",      param = list(supp = 0.01, conf = 0.01)),
#   "random items"      = list(name  = "RANDOM",  param = NULL),
#   "popular items"     = list(name  = "POPULAR", param = NULL),
#   "item-based CF"     = list(name  = "IBCF",    param = list(k = 5)),
#   "user-based CF"     = list(name  = "UBCF",    param = list(method = "Cosine", nn = 500)))
# 
# results <- evaluate(scheme,
#                     algorithms,
#                     type = "topNList",
#                     n = c(1, 3, 5, 10, 15, 20))
# 
# results
# 
# results_tbl = function(results, aux=c()){
#   for(i in aux){
#     # Pull into a list all confusion matrix information for one model
#     tmp <- results[[i]] %>%
#     getConfusionMatrix()  %>%
#     as.list()
# 
#     # Calculate average value of 5 cross-validation rounds
#     new = as.data.frame( Reduce("+",tmp) / length(tmp)) %>%
#       # Add a column to mark the number of recommendations calculated
#       mutate(n = c(1, 3, 5, 10, 15, 20)) %>%
#       # Select only columns needed and sorting out order
#       select('n', 'precision', 'recall', 'TPR', 'FPR')
# 
#     if(i==aux[1]) df = new %>% mutate(name = i)
#     else df = df %>% bind_rows(new %>% mutate(name = i))
#   }
#   return(df)
#   }
# 
# results_tbl = results_tbl(results, aux = names(results))
# 
# 
# 
# 
# results_tbl %>%
#   ggplot(aes(FPR, TPR,
#              colour = fct_reorder2(as.factor(name),
#                                    FPR, TPR))) +
#   geom_line() +
#   geom_label(aes(label = n))  +
#   labs(title = "ROC curves", colour = "Model") +
#   theme_grey(base_size = 14)
# 
# 
# results_tbl %>%
#   ggplot(aes(recall, precision,
#              colour = fct_reorder2(as.factor(name),
#                                    precision, recall))) +
#   geom_line() +
#   geom_label(aes(label = n))  +
#   labs(title = "Precision-Recall curves", colour = "Model") +
#   theme_grey(base_size = 14)














### METODO DE RECOMENDAÇÃO BASEADO EM RANKEAMENTO -------------------------------
#
# A ideia aqui é gerar um RANK segundo CADA variável observada e unir estes ranks,
# dando pesos para cada uma das variáveis na composição final. Acho que a ideia é
# parecida com a análise de regressão multivariada, de certa maneira, por conta da
# "importância" de cada variável.
base = read_csv('../Bases/Output/base_clean.csv')
names(base)


recomenda = function(base = NULL,
                     met_tst = "end",
                     prop_tst = NA,
                     var_list_dep = c(),
                     var_list_ind = c(),
                     var_list_cls = c(),
                     var_recom = NA,
                     func_rank = NA,
                     param = c(),
                     iter = 5,
                     topk = NA,
                     set_seed = NULL){
  
  # TIME-ATTACK!
  ptm <- proc.time()
  
  # ETAPA 0 - VALIDANDO PARAMETROS DA FUNCAO -----------------
  # base
  if(is.null(base)){
    print("Informe a base");return()}
  
  # prop_tst
  if(!is.numeric(prop_tst)){
    print("prop_tst da base de teste deve ser numérico entre 0 e 1");return()}
  if((prop_tst<=0) | (prop_tst>=1)){
    print("Proporção da base de teste deve ser numérico entre 0 e 1");return()}
  
  # met_tst
  if(!((met_tst!="end")|(met_tst!="random"))){
    print("Especifique o método de separação corretamente");return()}
  
  # var_list_dep
  if(!is.list(var_list_dep)){
    print("var_list_dep deve ser lista de vetores")
    print("Nota: Cada primeiro elemento do vetor é grão mínimo");return()}
  
  for(i in 1:length(var_list_dep)){
    if(any(!(var_list_dep[[i]] %in% names(base)))){
      print('var_list_dep deve conter nomes que constam nas colunas da base')
      print(paste0('Erro no item ', i, ' da lista'))
      print("Nota: Cada primeiro elemento do vetor é grão mínimo"); return()}}
  
  # var_list_ind
  if(any(!(var_list_ind %in% names(base)))){
    print("var_list_ind deve conter nomes que constam nas colunas da base"); return()}
  
  # var_list_cls
  
  # var_recom
  if(!(var_recom %in% names(base))){
    print("var_recom deve conter um nome que consta nas colunas da base"); return()}
  
  ## Não precisa do timestamp. Basta pensar em cada variável ts como "grão", pois já estão tratadas.
  # # var_ts
  # if(length(var_ts)!=0)
  #   if(!is.Date(tryCatch(date(base[1,var_ts[1]][[1]]),error=function(e)return("NOT DATA")))){
  #     print("var_ts[1] deve ser do tipo 'timestamp'"); return()}
  
  
  
  
  
  # ETAPA 1 - CRIANDO AS BASES DE REFERENCIA PADRAO -----------------
  # As bases geradas aqui tem como característica as variáveis ligadas à PK.
  # desde que a PK seja categórica.
  
  for(i in 1:length(var_list_dep)){
    aux1 = base[,var_list_dep[[i]]] %>%
      distinct()
    assign(paste0("b_rp_",i),aux1)
  }
  # Armazenando os nomes das bases criadas
  brp_nomes = ls()[contains("b_rp",T,ls())]
  
  
  
  
  
  # ETAPA 2 - SEPARACAO ENTRE TESTE E TREINO [OK] -----------------
  if(met_tst == "end"){
    aux1 = round(nrow(base)*prop_tst,0)
    b_trn = base[c(1:aux1),]
    b_tst = base[c((aux1+1):nrow(base)),]
  } else if(met_tst == "random"){
    aux1 = sample(nrow(base),round(nrow(base)*prop_tst,0),replace = FALSE)
    b_trn = base[aux1,]
    b_tst = base[-aux1,]}
  
  
  
  
  
  # ETAPA 3 - CRIACAO DO PERFIL DE COMPRADOR [UP] -----------------
  # Classificacao de user a priori Ex: valor, frete, etc, usando quantis definidos na entrada.
  # Serão criadas novas colunas na base com o sufixo "_cls" de 'classification'.
  if(length(var_list_cls)!=0){
    for(i in 1:length(var_list_cls)){
      aux1 = unlist(as.data.frame(b_trn[, var_list_cls[[i]][1]] ))
      b_trn[, paste0(var_list_cls[[i]][1],"_cls")] = classify(
        aux1,method = "quantile",
        breaks = as.numeric(var_list_cls[[i]][2]))$index
    }
  }
  
  
  
  
  
  # ETAPA 4 - GERANDO AS MATRIZES SPREAD-RANK DA BASE DE TREINO [OK] -----------------
  # Upgrade -> var_list_cls precisaria ser atualizado pelo perfil do comprador
  
  if(length(var_list_dep)!=0){
    for(i in 1:length(var_list_dep)){
      for(j in 1:length(var_list_dep[[i]])){
        
        aux1 = b_tst[,c(var_list_dep[[i]][j],var_recom)] %>%
          group_by_all %>%
          count() %>%
          arrange(desc(n)) %>%
          spread(key=names(.[,2]), value=names(.[,3]), fill = 0)
        
        aux1 = bind_cols(aux1[,1],
                         t(apply(aux1[,2:ncol(aux1)],1,function(x){1/rank(-x,ties.method = c("min"),na.last = "keep")})) %>%
                           as.data.frame())
        
        assign(paste0("spread_",var_list_dep[[i]][j]), aux1)
      }}}
  
  if(length(var_list_ind)!=0){
    for(i in 1:length(var_list_ind)){
      aux1 = b_tst[,c(var_list_ind[i],var_recom)] %>%
        group_by_all %>%
        count() %>%
        arrange(desc(n)) %>%
        spread(key=names(.[,2]), value=names(.[,3]), fill = 0)
      
      aux1 = bind_cols(aux1[,1],
                       t(apply(aux1[,2:ncol(aux1)],1,function(x){1/rank(-x,ties.method = c("min"),na.last = "keep")})) %>%
                         as.data.frame())
      
      assign(paste0("spread_",var_list_ind[i]), aux1)
    }}
  
  if(length(var_list_cls)!=0){
    for(i in 1:length(var_list_cls)){
      aux1 = b_tst[,c(paste0(var_list_cls[[i]][1],"_cls"),var_recom)] %>%
        group_by_all %>%
        count() %>%
        arrange(desc(n)) %>%
        spread(key=names(.[,2]), value=names(.[,3]), fill = 0)
      
      aux1 = bind_cols(aux1[,1],
                       t(apply(aux1[,2:ncol(aux1)],1,function(x){1/rank(-x,ties.method = c("min"),na.last = "keep")})) %>%
                         as.data.frame())
      
      assign(paste0("spread_",var_list_cls[[i]][j]), aux1)
    }}
  
  
  
  
  # ETAPA 5 - SETANDO OS PARAMETROS [OK] -----------------
  # A primeira iteração é de bom grado que comece com "peso zero"
  sprd_names = ls()[contains("spread_",T,ls())]
  var_names = gsub("spread_","",sprd_names)
  n_par = length(sprd_names)
  n_row_b_tst = nrow(b_tst)
  set.seed(set_seed)
  # param = rbind(rep(1/n_par, n_par),
  #               rdirichlet(iter-1, rep(1,n_par))) %>%
  #   as.data.frame() %>%
  #   remove_rownames()
  param = rdirichlet(iter, rep(1,n_par))
  
  
  
  
  
  # ETAPA 6 - ALGORITMO DO "CONTIDO NO TOP K" + TUNNING DOS PARAMETROS [IMP] -----------------
  #names(b_tst)
  b_tst_2 = b_tst[,c(var_names, var_recom)]
  
  mapping_params = mapping_metric = c()
  temp_metric = 0
  
  # Recriando as matrizes de pesos baseadas nos ranks, antes de multiplicar 
  # pelo peso
  for(i in 1:n_par){
    assign(paste0("lj_tst_",var_names[i]),b_tst_2[,var_names[i]] %>%
             left_join(get(sprd_names[i]), by = var_names[i]) %>%
             select(-var_names[i]))
  }
  
  # Realizando o TRIMMING dos parâmetros
  for(m in c(1:nrow(param))){
    # Este FOR é responsável por acumular as matrizes rankeadas ponderadas
    for (j in c(1:n_par)){
      par_aux = as.numeric(as.character(param[m,j]))
      aux2 = get(paste0("lj_tst_",var_names[j])) %>%
        mapply(., FUN = function(x) x*par_aux) %>%
        as.data.frame()
      
      ifelse(j == 1,
             aux1 <<- aux2,
             aux1 <<- aux1 + aux2)
    }
    
    # Aplicando o RANK na matriz acumulada
    rank_mat = aux1 %>%
      apply(.,1,FUN = function(x) rank(-x)) %>%
      t() %>%
      as.data.frame() %>%
      remove_rownames()

    res_rank_mat = rank_mat %>%
      apply(.,1, FUN = function(x){names(x[x<=topk])}) %>%
      t() %>%
      as.data.frame() %>%
      bind_cols(.,b_tst_2[,var_recom]) %>%
      as.data.frame()
    
    # Verifica quantas das recomendações estão nas topK
    contador1 = 0
    apply(res_rank_mat,1,FUN = function(x){
      if(x[length(x)] %in% x[1:(length(x)-1)]) contador1 <<- contador1+1})
    
    
    aux4 = contador1/n_row_b_tst
    if(aux4>=temp_metric){
      temp_metric = aux4
      temp_param = param[m,]
      mapping_params = c(mapping_params, m)
      mapping_metric = c(mapping_metric, aux4)
    }
  }
  
  
  
  
  
  # ETAPA 7 - SAIDA DO MODELO [UP] -----------------
  
  # Histórico Parâmetros e evolução da métrica
  chosen_param = param[mapping_params,] %>%
    `colnames<-`(var_names) %>%
    bind_cols(iter = mapping_params,metric = mapping_metric)
  # write.xlsx(chosen_param,"param.xlsx")
  
  # Ajustando a matriz rankeada ponderada com os parâmetros obtidos
  # EEE os índices para filtrar na recomendação.
  chosen_par_last = param[mapping_params[length(mapping_params)],]
  
  for (j in c(1:n_par)){
    numcol = ncol(get(paste0("spread_",var_names[j])))
    assign(paste0("output_",var_names[j]),
           bind_cols(
             get(paste0("spread_",var_names[j]))[,1],
             get(paste0("spread_",var_names[j]))[,c(2:numcol)] %>%
               mapply(.,FUN = function(x) x*chosen_par_last[j]) %>%
               as.data.frame()
           ))
  }
  
  outp_names = ls()[contains("output_",T,ls())]
  
  # Tempo de execução do modelo
  time_attack = proc.time() - ptm
  # write.table(time_attack[3],"time.txt")
  
  # Matriz rankeada já multiplicada pelo parâmetro
  lista = list()
  
  # Ordem das matrizes 
  lista[[1]] = outp_names
  
  # Output
  for(i in c(2:(n_par+1))){lista[[i]] = get(outp_names[i-1])}
  
  # Histórico de Parametros
  i = i+1
  lista[[i]] = chosen_param
  
  return(lista) 
}

### SETANDO PARAMETROS PARA O TREINO :) ========

var_list_dep = list(c("customer_zip_code_prefix",
                      "mun_ref",
                      "cd_uf",
                      "pib_pc_q5",
                      "pib_pc_q10"))

# var_list_dep = list(c("mun_ref",
#                       "cd_uf",
#                       "pib_pc_q5",
#                       "pib_pc_q10"))

var_list_ind = c("dia_faixa",
                 "dia_semana",
                 "mes",
                 "mes_quinzena")
# 
# var_list_cls = list(c("price",5),
#                     c("freight_value",4))
var_list_cls = c()
met_tst = "end"
var_recom = "product_category_name"
prop_tst = 0.8
iter = 1500
topk = 3
set_seed = 99


zzz = recomenda(base = base_clean,
                prop_tst = 0.8,
                var_list_dep = var_list_dep,
                var_list_ind = var_list_ind,
                var_recom = "product_category_name")
rm(zzz)

nvar = call("produto")

nrow(testando$base_treino)
nrow(testando$base_teste)


# RECOMENDAÇÃO ENFIM! -----------------
vars_rec_resp = c("customer_zip_code_prefix", "mun_ref",
"cd_uf","pib_pc_q5", "pib_pc_q10","dia_faixa",
"dia_semana", "mes", "mes_quinzena","product_category_name")

# Manter como dataframe para enviar os títulos das colunas
rec_row = base[sample(nrow(base),1),vars_rec_resp]

## FUNÇÃO DE RECOMENDAÇÂO
rank_recom = function(rec_row, lista){
  # Número de parâmetros
  nparam = length(rec_row)-1
  
  # Variáveis da recomendação
  rec_vars = colnames(rec_row)[1:nparam]
  mod_vars = lista[[1]]
  mod_vars_clean = sub("output_","",mod_vars)
  
  if(!any(rec_vars %in% mod_vars_clean) | nparam!=length(lista[[1]]))
    return('As variáveis da recomendação devem ser idênticas às usadas no treino do modelo')
  
  # Variável resposta
  resp_var = colnames(rec_row)[nparam+1]
  
  
  for(i in c(1:nparam)){
    # Recriando o DF para ser filtrado.
    # O intúito é manter a indexação correta.
    assign(mod_vars_clean[i], lista[[i+1]] %>% as.data.frame())
    
    # Valor a ser filtrado
    aux_val = as.character(rec_row[,sub("output_","",mod_vars[i])])
    
    
    df_parcial = get(mod_vars_clean[i]) %>%
      filter(as.character(.[,1])==aux_val) %>%
      select(-1)
    
    if(i == 1){aux_rec <<- df_parcial} else
      if(nrow(df_parcial)!=0){aux_rec <<- aux_rec + df_parcial}
  }
  
  output_recom = data.frame(rank(-aux_rec)) %>%
    rownames_to_column() %>%
    `colnames<-`(c("Categoria","Rank")) %>%
    arrange(Rank)
  
  return(output_recom)
  
  }
  
  
# Manter como dataframe para enviar os títulos das colunas
(rec_row = base[sample(nrow(base),1),vars_rec_resp])
rank_recom(rec_row, lista)[1:5,]


# Playground -----
baux = base[,vars_rec_resp]
baux = base[round(nrow(base)*0.8,0):nrow(base),vars_rec_resp]
aux_df = baux %>%
  group_by(product_category_name) %>%
  count() %>%
  arrange(desc(n)) %>%
  mutate(n_pc = round(n/sum(.$n)*100,2)) %>%
  as.data.frame()

barplot(aux_df$n[1:20], names.arg=aux_df$product_category_name[1:20], las =2)

rec_row = baux[sample(nrow(baux),1),vars_rec_resp]
rank_recom(rec_row, lista)[1:5,]

for(i in 1:10){
  rec_row = baux[i,vars_rec_resp]
  print(rank_recom(rec_row, lista)[1:5,])
}
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################



# função de teste -> gera/trata para recomendação usando CEP e Time.Stamp ------------
gera_user = function(cep, ts, base){}
# cep, mun, uf, pib_pc
# dia_fx, dia_semana, mes, mes_quinz

# função de predict -> pega as matrizes multiplicadas pelo parâmetro, filtra, rankeia e dá o TOPN  ------------
recomenda_predict = function(result_recomenda, user, topN){}
# pegar somente param e matrizes, multiplicá-los
# filter por cada matriz
# somar para o rank_final
# topN











# Etapa 1 - Spread das bases
var_to_spread = c("customer_zip_code_prefix","mun_ref","cd_uf",)




# Spreading da base segundo o zip (recomendação baseada em conteúdo)
teste1 = base.trn %>%
  select(customer_zip_code_prefix, product_category_name)

t1 = spread(data = as.data.frame(table(teste1)), product_category_name, Freq)

# Rank para baseado em conteúdo (usando o primeiro Zip como referência)
rank1 = t(t1[100,2:ncol(t1)]) %>%
  bind_cols(nomes = rownames(.)) %>%
  `colnames<-`(c("cont","cat")) %>%
  arrange(desc(cont)) %>%
  relocate(where(is.character), .before = where(is.numeric)) %>%
  filter(cont>0)


#-----------------------------------------------------------------#
# Spreading da base segundo a UF
teste2 = base.trn %>%
  select(uf, product_category_name)

t2 = spread(data = as.data.frame(table(teste2)), product_category_name, Freq)

# Rank para a primeira UF
rank2 = t(t2[1,2:ncol(t2)]) %>%
  bind_cols(nomes = rownames(.)) %>%
  `colnames<-`(c("cont","cat")) %>%
  arrange(desc(cont)) %>%
  relocate(where(is.character), .before = where(is.numeric)) %>%
  filter(cont>0)



#-----------------------------------------------------------------#
# Spreading da base segundo o Município
teste3 = base.trn %>%
  select(mun_ref, product_category_name)

t3 = spread(data = as.data.frame(table(teste3)), product_category_name, Freq)

# Exemplo de Rank para o primeiro Município da lista
rank3 = t(t3[1,2:ncol(t3)]) %>%
  bind_cols(nomes = rownames(.)) %>%
  `colnames<-`(c("cont","cat")) %>%
  arrange(desc(cont)) %>%
  relocate(where(is.character), .before = where(is.numeric)) %>%
  filter(cont>0)

# Note que os ranks são exemplos, não generalizados. Caso a gente faça um shiny (recomendo inclusive)
# esses "rank" serão atrelados ao Zip (pois o zip pertence a um munic, que pertence a uma uf, and so on).
# No caso, os seguintes estarão atrelados ao "momento da navegação", que será o timestamp do cidadão.
#
# Fazer os outros ranks se formos continuar por esse caminho
# dia_faixa
# dia_semana
# mes
# mes_quinzena

base.tst

base.trn %>%
  filter()
































