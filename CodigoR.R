library(dplyr)
library(tidytext)
library(wordcloud)
library(tidyr)
library(igraph)
library(ggraph)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(purrr)
library(plotly)
library(tm)


#creación del corpus, elegir cualquier base de datos
df<-Econo_Limpio
corpus=Corpus(VectorSource(df$T13))
#esta parte sirve para crear una matriz de documentos y poder usar algunas cosas de la libreria tm
term.doc.matrix <- TermDocumentMatrix(corpus, 
                                      control = list(
                                        removePunctuation = T, 
                                        stopwords = c("la", "las", "el", "los", "a", "ante", "bajo", "cabe", "con", "contra", "de",
                                                      "desde", "durante", "en", "entre", "hacia", "hasta", "mediante", "para", "por",
                                                      "según", "sin", "so", "sobre", "tras", "versus","vía", "b", "c", "d", "e",
                                                      "f", "g", "h", "i", "j", "k", "l", "m", "n", "ñ", "o", "p", "q", "r", "s", "t",
                                                      "u", "v", "w", "x", "y", "z", "del", "al","n°", "etc", "que", "un", "lo", "es",
                                                      "me", "se", "una", "te", "esta", "tu", "pero", "yo", "como", "ya", "mi",
                                                      "aqui","le", "no", "si", "ha", "mas", "su", "nos", "hay", "he","no","si","ha",
                                                      "eso","mas","todo","su","nos","hay","he","va","voy","porque","eh","nada",
                                                      "muy","ahi","asi","todos","estas","favor", "hacer", "pues", "esto","cuando",
                                                      "este", "soy", "ni", "tengo", "donde" ,"dos","has","ese", "estan",
                                                      "han" ,"algo", "ser", "esa", "vamos", "bien", "estoy", "tiene", "quiero",
                                                      "estoy", "era","solo", "les", "eres","tiene", "ud", "ahora", "tenemos","mm",
                                                      "estamos", "pasa","otra", "hace", "da", "gracias", "cualquier", "otro", "ti", "ah",
                                                      "son",'luego','lugar','porxala','paraderoxa','instantáneamentexaxa','tenientexa'
                                                      ,'rené','martínez','agustín','además','través','valdebenito',stopwords("spanish")),
                                        removeNumbers=T, 
                                        tolower=T))

#para la nube de palabras se debe pasar la base de arriba a una matriz
matriz_doc<- as.matrix(term.doc.matrix)
word.freq <- sort(rowSums(matriz_doc), decreasing = T)
dm <- data.frame(word=names(word.freq), freq = word.freq)
head(dm, 10)

#Asociacion de Palabras
#encontrar asociaciones a ciertas palabras, se debe ingresar lo primero que hicimos el termdocumentmatrix
findAssocs(term.doc.matrix, c("virus","niño","salud","ministerio","medida","minsal", "gobierno" ),c(0.7,0.7,0.6,0.7,0.6,0.6,0.6))#Salud
findAssocs(term.doc.matrix, c("carabinero","vehicular","delincuente","policial","arma","sujeto","gobierno" ),c(0.6,0.6,0.6,0.7,0.6,0.6,0.6))#Policia
findAssocs(term.doc.matrix, c("precio","pobreza","familia","gobierno" ),c(0.6,0.4,0.1,0.4))#economía

#cluster
#clustering
nov_new <- removeSparseTerms(term.doc.matrix, sparse = .67)
#verificar los documentos 
term.doc.matrix
nov_new
#estandarizar
ov_new <- nov_new %>% as.matrix()
ov_new
nov_new <- ov_new/rowSums(ov_new)
#definir distancia y método
nov_dist <- dist(nov_new, method = "euclidian")
nov_hclust <-  hclust(nov_dist, method = "ward.D")
#plot gráfico
plot(nov_hclust, main = "Clustering economia TVN", sub = "", xlab = "")
rect.hclust(nov_hclust, k = 8, border="blue")

#Similitud de coseno
m<-lsam$dk

lsaMatrix <- diag(lsam$sk) %*% t(lsam$dk)
distMatrix <- cosine(lsaMatrix)
corm=round(distMatrix, 4)
corm
cormm=matrix(corm,nrow = 4,ncol = 4,dimnames = list(c('T13','TVN','MEGA','CHV'),
                                                    c('T13','TVN','MEGA','CHV')))
cormm
corrplot(cormm,is.corr = F,order = 'AOE',
         addCoef.col = 'black',tl.col = 'black')

#Analasis de Sentimientos
#Defino una funcion para limpiar y tokenizar los datos
limpiar_df <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  #Elimino tildes en las letras
  nuevo_texto<-stringi::stri_trans_general(nuevo_texto,"Latin-ASCII")
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  # Tokenización por palabras individuales
  nuevo_texto <- str_split(nuevo_texto, " ")[[1]]
  # Eliminación de tokens con una longitud < 2
  nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1})
  return(nuevo_texto)
}

#limpio y tokenizo mis datos
df= df %>% mutate(texto_tokenizado = map(.x = T13,
                                         .f = limpiar_df))

data_tidy <- df %>% select(-T13) %>% unnest()
data_tidy <- data_tidy %>% rename(token = texto_tokenizado)


stop_word_1 <- c("la", "las", "el", "los", "a", "ante", "bajo", "cabe", "con", "contra", "de",
                 "desde", "durante", "en", "entre", "hacia", "hasta", "mediante", "para", "por",
                 "según", "sin", "so", "sobre", "tras", "versus","vía", "b", "c", "d", "e",
                 "f", "g", "h", "i", "j", "k", "l", "m", "n", "ñ", "o", "p", "q", "r", "s", "t",
                 "u", "v", "w", "x", "y", "z", "del", "al","n°", "etc", "que", "un", "lo", "es",
                 "me", "se", "una", "te", "esta", "tu", "pero", "yo", "como", "ya", "mi",
                 "aqui","le", "no", "si", "ha", "mas", "su", "nos", "hay", "he","no","si","ha",
                 "eso","mas","todo","su","nos","hay","he","va","voy","porque","eh","nada",
                 "muy","ahi","asi","todos","estas","favor", "hacer", "pues", "esto","cuando",
                 "este", "soy", "ni", "tengo", "donde" ,"dos","has","ese", "estan",
                 "han" ,"algo", "ser", "esa", "vamos", "bien", "estoy", "tiene", "quiero",
                 "estoy", "era","solo", "les", "eres","tiene", "ud", "ahora", "tenemos","mm",
                 "estamos", "pasa","otra", "hace", "da", "gracias", "cualquier", "otro", "ti", "ah",
                 "son")

# Se filtran las stopwords
data_tidy <- data_tidy %>% filter(!(token %in% stop_word_1))

#para hacer la red de palabras(similar a un lda pero más sencillo de explicar)





word.network = data_tidy %>% count(token, sort = TRUE) %>% filter(n > 
                                                                    2) %>% graph_from_data_frame()
a = arrow(angle = 100, length = unit(0.1, "inches"), ends = "last", type = "open")
ggraph(word.network, layout = "fr")+geom_node_point() + 
  geom_node_text(aes(label = name), vjust = 1, hjust = 0.4) + 
  labs(title = "BWCB Areas of Expertise", caption = "Data from 12/24/2022" ) + 
  theme(legend.position="bottom", plot.margin = unit(c(1,1,1,1), "mm"))




#deprecate the following
refund.reader <-readTabular( mapping = list( content =" text", id =" ID"))
refund.df=data.frame(doc_id=seq(1:nrow(refund)),text=refund$text) 
df1 =data.frame( ID = seq( 1: nrow( df)), text = df$T13)
df1
library(tm)
df$T13
refund <- df[ grep("incremento", df$T13, ignore.case = T), ]



sentimientos_df <- get_nrc_sentiment(data_tidy$token, lang="spanish")
sentimientos=data.frame(Furia=sentimientos_df$anger,Anticipación=sentimientos_df$anticipation, Disgusto=sentimientos_df$disgust, Miedo=sentimientos_df$fear, Alegría=sentimientos_df$joy, Tristeza=sentimientos_df$sadness,Sorpresa=sentimientos_df$surprise, Confianza=sentimientos_df$trust, Negativo=sentimientos_df$negative, Positivo=sentimientos_df$positive)

barplot(
  colSums(prop.table(sentimientos[, 1:10])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 1.2,
  col = brewer.pal(n = 8, name = "Set2"),
  main = "Análisis de Sentimientos Noticias policiales CHV",
  xlab=NULL, ylab = NULL)


