#Análise inicial dos arquivos geraos pelo e-Lattes

#Pacotes para serem ativados
library(tidyverse)
library(jsonlite); library(listviewer)
library(igraph)

#Pasta com códigos e arquivos
setwd("C:/Users/berna/Desktop/nanotecnologia&saude_animal") #Pasta contendo os arquivos

#upload de arquivo com funções para transformar listas em Data Frames e objeto igraph
source("elattes.ls2df.R")

file.info("JSON/834.profile.json")
file.info("JSON/847.profile.json")
file.info("JSON/834.advise.json")
file.info("JSON/847.advise.json")
file.info("JSON/834.publication.json")
file.info("JSON/847.publication.json")
file.info("JSON/834.graph.json")
file.info("JSON/847.graph.json")

#Leitura de arquivos de nanociencia_nanobiotecnologia e saude_animal
perfil_nano <- fromJSON("JSON/834.profile.json")
perfil_saude <- fromJSON("JSON/847.profile.json")

public_nano <- fromJSON("JSON/834.publication.json")
public_saude <- fromJSON("JSON/847.publication.json")

orient_nano <- fromJSON("JSON/834.advise.json")
orient_saude <- fromJSON("JSON/847.advise.json")

#Número de Pessoas que participam do programa
length(perfil_nano)
length(perfil_saude)

# Descricao de perfil de docente
glimpse(perfil_nano[[1]], width = 5)
glimpse(perfil_saude[[1]], width = 5)

# Descricao dos dados de orientacoes
names(orient_nano)
names(orient_saude)

length(orient_nano$ORIENTACAO_CONCLUIDA_DOUTORADO)
length(orient_saude$ORIENTACAO_EM_ANDAMENTO_DOUTORADO)

#Descricao dos dados bibliograficos
names(public_nano)
names(public_saude)


#Perfil
#Análise dos dados em formato list

# Número de produções por ano
table(unlist(sapply(perfil_nano, function(x) (x$producao_bibiografica$ARTIGO_ACEITO$ano))))
table(unlist(sapply(perfil_nano, function(x) (x$producao_bibiografica$LIVRO$ano))))
table(unlist(sapply(perfil_nano, function(x) (x$producao_bibiografica$PERIODICO$ano))))
table(unlist(sapply(perfil_nano, function(x) (x$producao_bibiografica$TEXTO_EM_JORNAIS$ano))))

table(unlist(sapply(perfil_saude, function(x) (x$producao_bibiografica$ARTIGO_ACEITO$ano))))
table(unlist(sapply(perfil_saude, function(x) (x$producao_bibiografica$LIVRO$ano))))
table(unlist(sapply(perfil_saude, function(x) (x$producao_bibiografica$PERIODICO$ano))))
table(unlist(sapply(perfil_saude, function(x) (x$producao_bibiografica$TEXTO_EM_JORNAIS$ano))))

# Número de áreas de atuação por pessoa
table(unlist(sapply(perfil_nano, function(x) nrow(x$areas_de_atuacao))))
table(unlist(sapply(perfil_saude, function(x) nrow(x$areas_de_atuacao))))
# Número de pessoas por grande area
table(unlist(sapply(perfil_nano, function(x) (x$areas_de_atuacao$grande_area))))
table(unlist(sapply(perfil_saude, function(x) (x$areas_de_atuacao$grande_area))))
# Número de pessoas que produziram os específicos tipos de produção
table(unlist(sapply(perfil_nano, function(x) names(x$producao_bibiografica))))
table(unlist(sapply(perfil_saude, function(x) names(x$producao_bibiografica))))

#Converte perfil_nano e perfil_saude em data frame
dataframe_perfil_nano <- extrai.perfis(perfil_nano)
dataframe_perfil_saude <- extrai.perfis(perfil_saude)

#Distribuiçao de pesquisadores por grandes áreas
area_atuacao_nano <- extrai.areas.atuacao(perfil_nano)
area_atuacao_saude <- extrai.areas.atuacao(perfil_saude)

ggplot(area_atuacao_nano, aes(x = 1, y = grande_area, fill = grande_area)) +
  # Use a column geometry.
  geom_col() +
  # Change coordinate system to polar.
  coord_polar(theta = "y") +
  theme_void() +
  # Clean up the background with theme_void and give it a proper title with ggtitle.
  ggtitle("Distribuiçao de pesquisadores por grande área na Nanociencia e Nanobiotecnologia")

ggplot(area_atuacao_saude, aes(x = 1, y = grande_area, fill = grande_area)) +
  # Use a column geometry.
  geom_col() +
  # Change coordinate system to polar.
  coord_polar(theta = "y") +
  theme_void() +
  # Clean up the background with theme_void and give it a proper title with ggtitle.
  ggtitle("Distribuiçao de pesquisadores por grande área na Saude Animal")


##Publicação
##Análise dos dados no formato DF
public_nano.periodico.df <- pub.ls2df(public_nano, 1) #artigos
public_nano.livros.df <- pub.ls2df(public_nano, 2) #livros
public_nano.eventos.df <- pub.ls2df(public_nano, 5) #eventos

public_saude.periodico.df <- pub.ls2df(public_saude, 1) #artigos
public_saude.livros.df <- pub.ls2df(public_saude, 2) #livros
public_saude.eventos.df <- pub.ls2df(public_saude, 5) #eventos

#Publicação por ano
table(public_nano.periodico.df$ano)
table(public_saude.periodico.df$ano)

#publicação de livros por pais
table(public_nano.livros.df$pais_de_publicacao)
table(public_saude.livros.df$pais_de_publicacao)


#Eventos nos paises ao longo dos anos
public_nano.eventos.df %>%
  filter(pais_do_evento %in%
           c(names(head(sort(table(public_nano.eventos.df$pais_do_evento)
                             , decreasing = TRUE), 10)))) %>%
  group_by(ano_do_trabalho,pais_do_evento) %>%
  ggplot(aes(x=ano_do_trabalho,y=pais_do_evento, color= pais_do_evento)) +
  xlab("Ano") + ylab("Pais") + geom_point() + geom_jitter() +
  ggtitle("Eventos de Nanociencia e Nanobiotecnologia nos paises ao longo dos anos")

public_saude.eventos.df %>%
  filter(pais_do_evento %in%
           c(names(head(sort(table(public_nano.eventos.df$pais_do_evento)
                             , decreasing = TRUE), 10)))) %>%
  group_by(ano_do_trabalho,pais_do_evento) %>%
  ggplot(aes(x=ano_do_trabalho,y=pais_do_evento, color= pais_do_evento)) +
  xlab("Ano") + ylab("Pais") + geom_point() + geom_jitter() +
  ggtitle("Eventos de Saude Animal nos paises ao longo dos anos")


#Orientação
#Análise dos dados em formato lista
##Número de Orientações Mestrado e Doutorado
table(unlist(sapply(orient_nano, function(x) names(x))))
table(unlist(sapply(orient_saude, function(x) names(x))))
length(orient_nano)
sum(sapply(orient_nano$ORIENTACAO_CONCLUIDA_DOUTORADO, function(x) length(x$natureza)))

sum(sapply(orient_saude$ORIENTACAO_CONCLUIDA_DOUTORADO, function(x) length(x$natureza)))

##Análise dos dados no formato DF
orient_nano.posdoutorado.df <- ori.ls2df(orient_nano, 6) #pos-Doutorado concluído
orient_nano.doutorado.df <- ori.ls2df(orient_nano, 7) #Doutorado concluído
orient_nano.mestrado.df <- ori.ls2df(orient_nano, 8) #Mestrado concluído
orient_nano.df <- rbind(rbind(orient_nano.posdoutorado.df, orient_nano.doutorado.df), orient_nano.mestrado.df)

orient_saude.posdoutorado.df <- ori.ls2df(orient_saude, 6) #pos-Doutorado concluído
orient_saude.doutorado.df <- ori.ls2df(orient_saude, 7) #Doutorado concluído
orient_saude.mestrado.df <- ori.ls2df(orient_saude, 8) #Mestrado concluído
orient_saude.df <- rbind(rbind(orient_saude.posdoutorado.df, orient_saude.doutorado.df), orient_saude.mestrado.df)

#Orientaçoes que recebem bolsas
table(orient_nano.df$bolsa)
table(orient_saude.df$bolsa)

#Natureza das orientacoes completas por ano
ggplot(orient_nano.df,aes(ano,fill=natureza)) +
  geom_bar(stat = "count", position="dodge") +
  ggtitle("Natureza das orientações Completas Por Ano de Nanociencia e Nanobiotecnologia") +
  theme(legend.position="right",legend.text=element_text(size=7)) +
  guides(fill=guide_legend(nrow=5, byrow=TRUE, title.position = "top")) +
  labs(x="Ano",y="Quantidade")

ggplot(orient_saude.df,aes(ano,fill=natureza)) +
  geom_bar(stat = "count", position="dodge") +
  ggtitle("Natureza das orientações Completas Por Ano de Saude Animal") +
  theme(legend.position="right",legend.text=element_text(size=7)) +
  guides(fill=guide_legend(nrow=5, byrow=TRUE, title.position = "top")) +
  labs(x="Ano",y="Quantidade")



graphl_nano <- fromJSON("JSON/834.graph.json")
graphl_saude <- fromJSON("JSON/847.graph.json")

#Grafo de nanobiotecnologia
g <- g.ls2ig(graphl_nano)


# Quais sao os dois vertices mais afastados do grafo?
farthest_vertices(g)

# Mostra o caminho que estes vertices mais distantes tem que percorrer para se encontrarem
get_diameter(g)




# identificando os nós chaves usando eigenvector_centrality
g.ec <- eigen_centrality(g)
which.max(g.ec$vector)

colors <- c("#006699", "#00cc66", "#cccc00", "#ff3300", "#ff0066", "#ff6600", "#cc3300",
            "#ff99cc", "#ff6699", "#99ccff", "#00ff99", "#cc99ff", "#99ff33", "#ccff66",
            "#99ff66", "#ffff00", "#9999ff", "#ff00ff", "#ff9999", "#0099ff", "#cc9900")

# Atribuindo cores aos vertices da rede g
V(g)$color <- colors

# Plot grafo nanobiotecnologia
plot(g,
     vertex.label.color = "black",
     vertex.label.cex = 0.8,
     vertex.size = 25*(g.ec$vector),
     edge.color = 'gray88',
     main = "Rede de influencia centralizada"
)



# Identificando as maiores panelinas na rede
largest_cliques(g)

# Determine todas as panelas maximas na rede e atribuindo ao objeto
clq <- max_cliques(g)

# Calculando o tamanho maximo de cada panela.
table(unlist(lapply(clq, length)))

# atribuindo largest_cliques ao objeto lc
lc <- largest_cliques(g)

# Criando dois novos subgrafos nao difecionados cada um contendo somente os vertices com as maiores panelinhas
gs1 <- as.undirected(subgraph(g, lc[[1]]))
gs2 <- as.undirected(subgraph(g, lc[[2]]))

# Plot os dois subgrafos lado a lado

par(mfrow=c(1,2)) # To plot two plots side-by-side

plot(gs1,
     vertex.label.color = "black",
     vertex.label.cex = 0.9,
     vertex.size = 0,
     edge.color = 'gray28',
     main = "Panelinha 1",
     layout = layout.circle(gs1)
)

plot(gs2,
     vertex.label.color = "black",
     vertex.label.cex = 0.9,
     vertex.size = 0,
     edge.color = 'gray28',
     main = "Panelinha 2",
     layout = layout.circle(gs2)
)

# Analise de texto

library(rJava)
library(qdap)
library(tm)
library(wordcloud)

public_nano.periodico.df <- pub.ls2df(public_nano, 1) #artigos
public_nano.livros.df <- pub.ls2df(public_nano, 2) #livros

public_saude.periodico.df <- pub.ls2df(public_saude, 1) #artigos
public_saude.livros.df <- pub.ls2df(public_saude, 2) #livros


# Texto puro
public_nano.periodico.df$titulo[1]
# converter todos os titulos em minusculos
public <- tolower(public_nano.periodico.df$titulo)
# Remove pontuaççao
public <- removePunctuation(public)
# Remove numeros
public <- removeNumbers(public)
# Remove espaços em branco
public <- stripWhitespace(public)
new_stopwords <- c(stopwords("en"), stopwords("pt-br"), Top200Words)
# Print text without standard stop words
public <- removeWords(public, new_stopwords)
public[1]


# Achando os 10 termos mais frequentes
term_count <- freq_terms(public, 100)
# Plot term_count
plot(term_count)


# Cria uma nuvem de palavras para valores de frequencia de palavras
wordcloud(term_count$WORD,term_count$FREQ, max.words = 50, colors = c("grey80","darkgoldenrod1","tomato"))

#
# ######
# #Análise do arquivo perfil
# glimpse(perfil[[1]], width = 1)
#
# #Número de Pessoas que foram encontradas
# length(perfil)
# ##Análise dos dados em formato list
# # Número de áreas de atuação cumulativo
# sum(sapply(perfil, function(x) nrow(x$areas_de_atuacao)))
# # Número de áreas de atuação por pessoa
# table(unlist(sapply(perfil, function(x) nrow(x$areas_de_atuacao))))
# # Número de pessoas por grande area
# table(unlist(sapply(perfil, function(x) (x$areas_de_atuacao$grande_area))))
# # Número de pessoas que produziram os específicos tipos de produção
# table(unlist(sapply(perfil, function(x) names(x$producao_bibiografica))))
# # Número de publicações por tipo
# sum(sapply(perfil, function(x) length(x$producao_bibiografica$ARTIGO_ACEITO$ano)))
# sum(sapply(perfil, function(x) length(x$producao_bibiografica$CAPITULO_DE_LIVRO$ano)))
# sum(sapply(perfil, function(x) length(x$producao_bibiografica$LIVRO$ano)))
# sum(sapply(perfil, function(x) length(x$producao_bibiografica$PERIODICO$ano)))
# sum(sapply(perfil, function(x) length(x$producao_bibiografica$TEXTO_EM_JORNAIS$ano)))
# # Número de pessoas por quantitativo de produções por pessoa 0 = 1; 1 = 2...
# table(unlist(sapply(perfil, function(x) length(x$producao_bibiografica$ARTIGO_ACEITO$ano))))
# table(unlist(sapply(perfil, function(x) length(x$producao_bibiografica$CAPITULO_DE_LIVRO$ano))))
# table(unlist(sapply(perfil, function(x) length(x$producao_bibiografica$LIVRO$ano))))
# table(unlist(sapply(perfil, function(x) length(x$producao_bibiografica$PERIODICO$ano))))
# table(unlist(sapply(perfil, function(x) length(x$producao_bibiografica$TEXTO_EM_JORNAIS$ano))))
# # Número de produções por ano
# table(unlist(sapply(perfil, function(x) (x$producao_bibiografica$ARTIGO_ACEITO$ano))))
# table(unlist(sapply(perfil, function(x) (x$producao_bibiografica$CAPITULO_DE_LIVRO$ano))))
# table(unlist(sapply(perfil, function(x) (x$producao_bibiografica$LIVRO$ano))))
# table(unlist(sapply(perfil, function(x) (x$producao_bibiografica$PERIODICO$ano))))
# table(unlist(sapply(perfil, function(x) (x$producao_bibiografica$TEXTO_EM_JORNAIS$ano))))
# # Número de pessoas que realizaram diferentes tipos de orientações
# length(unlist(sapply(perfil, function(x) names(x$orientacoes_academicas))))
# # Número de pessoas por tipo de orientação
# table(unlist(sapply(perfil, function(x) names(x$orientacoes_academicas))))
# #Número de orientações concluidas
# sum(sapply(perfil, function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_MESTRADO$ano)))
# sum(sapply(perfil, function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_DOUTORADO$ano)))
# sum(sapply(perfil, function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_POS_DOUTORADO$ano)))
#
# # Número de pessoas por quantitativo de orientações por pessoa 0 = 1; 1 = 2...
# table(unlist(sapply(perfil, function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_MESTRADO$ano))))
# table(unlist(sapply(perfil, function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_DOUTORADO$ano))))
# table(unlist(sapply(perfil, function(x) length(x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_POS_DOUTORADO$ano))))
#
# # Número de orientações por ano
# table(unlist(sapply(perfil, function(x) (x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_MESTRADO$ano))))
# table(unlist(sapply(perfil, function(x) (x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_DOUTORADO$ano))))
# table(unlist(sapply(perfil, function(x) (x$orientacoes_academicas$ORIENTACAO_CONCLUIDA_POS_DOUTORADO$ano))))

# ###Análise dos dados em formato Data Frame
# #Arquivo Profile por Currículo
# # extrai perfis dos professores
# perfil.df.professores <- extrai.perfis(perfil)
#
# # extrai producao bibliografica de todos os professores
# perfil.df.publicacoes <- extrai.producoes(perfil)
#
# #extrai orientacoes
# perfil.df.orientacoes <- extrai.orientacoes(perfil)
#
# #extrai areas de atuacao
# perfil.df.areas.de.atuacao <- extrai.areas.atuacao(perfil)
#
# #cria arquivo com dados quantitativos para análise
# perfil.df <- data.frame()
# perfil.df <- perfil.df.professores %>%
#   select(idLattes, nome, resumo_cv, senioridade) %>%
#   left_join(
#     perfil.df.orientacoes %>%
#       select(orientacao, idLattes) %>%
#       filter(!grepl("EM_ANDAMENTO", orientacao)) %>%
#       group_by(idLattes) %>%
#       count(orientacao) %>%
#       spread(key = orientacao, value = n),
#     by = "idLattes") %>%
#   left_join(
#     perfil.df.publicacoes %>%
#       select(tipo_producao, idLattes) %>%
#       filter(!grepl("ARTIGO_ACEITO", tipo_producao)) %>%
#       group_by(idLattes) %>%
#       count(tipo_producao) %>%
#       spread(key = tipo_producao, value = n),
#     by = "idLattes") %>%
#   left_join(
#     perfil.df.areas.de.atuacao %>%
#       select(area, idLattes) %>%
#       group_by(idLattes) %>%
#       summarise(n_distinct(area)),
#     by = "idLattes")
#
# glimpse(perfil.df)
#
#
# ####
# ###Publicação
# ##Análise dos dados no formato lista
# #Número de Publicações em periódicos
# sum(sapply(public$PERIODICO, function(x) length(x$natureza)))
# #anos analisados
# names(public$PERIODICO)
# #20 revistas mais publicadas
# head(sort(table(as.data.frame(unlist
#     (sapply(public$PERIODICO, function(x) unlist(x$periodico)))
#   )), decreasing = TRUE),20)
#
# ##Análise dos dados no formato DF
# public.periodico.df <- pub.ls2df(public, 1) #artigos
# public.livros.df <- pub.ls2df(public, 2) #livros
# public.eventos.df <- pub.ls2df(public, 5) #eventos
# #Publicação por ano
# table(public.periodico.df$ano)
# #20 revistas mais publicadas
# #Mesma visão que anterior mas agora trabalhando no DataFrame
# head(sort(table(public.periodico.df$periodico), decreasing = TRUE), 20)
#
# #Visualização
# # Gráfico de barras
# public.periodico.df %>%
#   group_by(ano) %>%
#   summarise(Quantidade = n()) %>%
#   ggplot(aes(x = ano, y = Quantidade)) +
#   geom_bar(position = "stack",stat = "identity", fill = "darkcyan")+
#   geom_text(aes(label=Quantidade), vjust=-0.3, size=2.5)+
#   theme_minimal()
#
# #publicação de livros fora do Brasil
# public.livros.df %>%
#   group_by(pais_de_publicacao) %>%
#   summarise(Quantidade = n()) %>%
#   filter(pais_de_publicacao != "Brasil") %>%
#   ggplot(aes(x = pais_de_publicacao, y = Quantidade)) +
#   geom_bar(width=0.8, height = 0.3, position = "stack",stat = "identity", fill = "coral")+
#   geom_text(aes(label=Quantidade), vjust=-0.3, size=2.5) +
#   theme_minimal()
#
# public.livros.df %>%
#   filter(pais_de_publicacao %in% c("Brasil", "Estados Unidos", "Holanda",
#                                    "Grã-Bretanha", "Alemanha", "Suiça")) %>%
#   group_by(ano,pais_de_publicacao) %>%
#   ggplot(aes(x=ano,y=pais_de_publicacao, color= pais_de_publicacao)) +
#   xlab("Ano") + ylab("Pais") + geom_point() + geom_jitter()
#
#   #Eventos
# public.eventos.df %>%
#   filter(pais_do_evento %in%
#            c(names(head(sort(table(public.eventos.df$pais_do_evento)
#                              , decreasing = TRUE), 10)))) %>%
#   group_by(ano_do_trabalho,pais_do_evento) %>%
#   ggplot(aes(x=ano_do_trabalho,y=pais_do_evento, color= pais_do_evento)) +
#   xlab("Ano") + ylab("Pais") + geom_point() + geom_jitter()
#
#
# ####
# #Orientação
# #Análise dos dados em formato lista
# ##Número de Orientações Mestrado e Doutorado
# sum(sapply(orient$ORIENTACAO_CONCLUIDA_DOUTORADO, function(x) length(x$natureza))) +
#   sum(sapply(orient$ORIENTACAO_CONCLUIDA_MESTRADO, function(x) length(x$natureza)))
#
# ##Análise dos dados no formato DF
# orient.posdoutorado.df <- ori.ls2df(orient, 6) #pos-Doutorado concluído
# orient.doutorado.df <- ori.ls2df(orient, 7) #Doutorado concluído
# orient.mestrado.df <- ori.ls2df(orient, 8) #Mestrado concluído
#
# orient.df <- rbind(rbind(orient.posdoutorado.df, orient.doutorado.df), orient.mestrado.df)
#
# ggplot(orient.df,aes(ano,fill=natureza)) +
#   geom_bar(stat = "count", position="dodge") +
#   ggtitle("Natureza das Orientações Completas Por Ano") +
#   theme(legend.position="right",legend.text=element_text(size=7)) +
#   guides(fill=guide_legend(nrow=5, byrow=TRUE, title.position = "top")) +
#   labs(x="Ano",y="Quantidade")
#
# ###
# #Grafo
# g <- g.ls2ig(graphl)
# df <- as.data.frame(V(g)$name); colnames(df) <- "Idlattes"
# #Apenas para fins de análise inicial, foram retiradas as observações
# #com duplicação de pesquisadores no caso de haver professores em mais
# #de um programa
# df <- df %>% group_by(Idlattes) %>%
#   slice(1L)
# V(g)$programa <- df$Programa
# V(g)$orient_dout <- perfil.df$ORIENTACAO_CONCLUIDA_DOUTORADO
# V(g)$orient_mest <- perfil.df$ORIENTACAO_CONCLUIDA_MESTRADO
# V(g)$publicacao <- perfil.df$PERIODICO
# V(g)$eventos <- perfil.df$EVENTO
