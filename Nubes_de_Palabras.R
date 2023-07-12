# TVN policial ------------------------------------------------------------

#creación del corpus
corpus=Corpus(VectorSource(base$TVN))
corpus=tm_map(corpus,removePunctuation)
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
                                                      "son","daniel","araneda","onda","dejo","centro","central","hora",stopwords("spanish")),
                                        removeNumbers=T, 
                                        tolower=T))
term.doc.matrix
#para la nube de palabras se debe pasar la base de arriba a una matriz
matriz_doc<- as.matrix(term.doc.matrix);matriz_doc
word.freq <- sort(rowSums(matriz_doc), decreasing = T)
dm <- data.frame(word=names(word.freq), freq = word.freq);dm
head(dm, 5)
sum(dm$freq)
## Para usar la nube de palabras
wordcloud2(dm, size = 1, minRotation = -0.55, maxRotation = -0.55, rotateRatio = 2)
# Mega --------------------------------------------------------------------
#creación del corpus
corpus=Corpus(VectorSource(base$MEGA))
corpus=tm_map(corpus,removePunctuation)
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
                                                      "son","ocurrio","alta","estacion","especie","paradero","acceso","aclarar","adn","carabinerosclausuran",
                                                      "eduardo","espera","hijo","ingresar","meganoticias","mercaderia","ministerio","moneda","pareja","radio",
                                                      "refirio","sucedido","tamaño","afectar","after","bombero","camion","costado","discoteque","fumar","jose",
                                                      "motivo","parcela","participacion","pircar","producir","rancagua","registrar","rios","seriar","silva",
                                                      "tenian","actualizar","alvarez","dirigida","ocurrio","salud","sucedio","tec","unir","visar","waiver","afirmar",
                                                      "agua","carihueico","delegado","espacio","estructura","frontal","llegar","presente","region","reír","santo",
                                                      "teresita","cali","cambio","direccion","inmediato","poste","eduacion","invierno","ministro","paso","posibilidad",
                                                      "añadio","furgon","pleno","relato","resulta","ronda","sur","turno","indeterminado","proceder","buscar","cantante",
                                                      "instruir","kilo","movil","norte","perder","administrativo","general","llamado","presunto","tipo","andar","compra",
                                                      "cuestionar","gasto","localidad","mantener","millonario","monte","obra","respiratorio","virus","calama",
                                                      "castro","enigma","florido","grua","jugar","vidente","alcance","aún","corresponderian","cranear","ecologica","lactante",
                                                      "nacido","recien","resto","ritual","simbolico","finalmente","habia","dia","lete","adema","bebida","alcoholicas","efectivo",
                                                      "navia","traves","vergara","hospital","red","matta","cerda","pedro","mira","humanar","bernardo","centimetros","hora",
                                                      "palacio","conchali","concurria","posteriormente","bordo","cerrillos","macul","baja","descender","habrian","colina","data",
                                                      "cuello","rinconada","comuna","año","central","avenida","cantidad","entregar","fiesta","cuento","tio","clandestino","centro","quilicura","santiago","cerro",
                                                      "conectar","prefectura","informacion","recoleto","sede","causa","muestra","receptacion","comercial","atravesar","camaras","ciudadano","identidad","identificacion",
                                                      "interceptar","lanza","lesionar","mujer","placa","portar","seguridad","violento","abusar","agredir","detectar","diligencia","intrafamiliar",
                                                      "prevencion","violencia","afectado","armar","camión","fiscalizar","ladrón","reportar","retener","accidentado","chile","dirigida","falsificar","fugar","mentir",
                                                      "municipal","quincho","utilizar","vida","villa","antecedente","barrera","contencion","debajo","delictual","escondia","jovenes","morir","motocicleta","muerto","organizacion",
                                                      "portonazo","sorpresivamente","adelantarlar","descubierto","educacion","civil","embestir","grupo","propinar","rostro","denuncia","detallar","filtración","reunión","acusado",
                                                      "arriesgar","brigada","crimen","disparo","polvora","reporte","urbano","sumario","turbazo","adolescente","contraloria","atacar","colaboracion","colgar","desaparicion","incidente",
                                                      "pelea","prevencionista","denuncio","eriazo","percatar","riña","traicionar","violentar","dirigia","vitacura","volcado",stopwords("spanish")),
                                        removeNumbers=T, 
                                        tolower=T))
term.doc.matrix
#para la nube de palabras se debe pasar la base de arriba a una matriz
matriz_doc<- as.matrix(term.doc.matrix);matriz_doc
word.freq <- sort(rowSums(matriz_doc), decreasing = T)
dm <- data.frame(word=names(word.freq), freq = word.freq);dm
head(dm, 5)
## Para usar la nube de palabras
wordcloud2(dm, size =0.7,minRotation = -0.55, maxRotation = -0.55, rotateRatio = 2)

# CHV ------------------------------------------------------------
#creación del corpus
corpus=Corpus(VectorSource(base$CHV))
corpus=tm_map(corpus,removePunctuation)
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
                                                      "son",stopwords("spanish")),
                                        removeNumbers=T, 
                                        tolower=T))
term.doc.matrix
#para la nube de palabras se debe pasar la base de arriba a una matriz
matriz_doc<- as.matrix(term.doc.matrix);matriz_doc
word.freq <- sort(rowSums(matriz_doc), decreasing = T)
dm <- data.frame(word=names(word.freq), freq = word.freq);dm
head(dm, 5)
## Para usar la nube de palabras
wordcloud2(dm, size = 1,minRotation = -0.55, maxRotation = -0.55, rotateRatio = 2)

# T13 ---------------------------------------------------------------------
#creación del corpus
corpus=Corpus(VectorSource(base$T13))
corpus=tm_map(corpus,removePunctuation)
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
                                                      "son",stopwords("spanish")),
                                        removeNumbers=T, 
                                        tolower=T))
term.doc.matrix
#para la nube de palabras se debe pasar la base de arriba a una matriz
matriz_doc<- as.matrix(term.doc.matrix);matriz_doc
word.freq <- sort(rowSums(matriz_doc), decreasing = T)
dm <- data.frame(word=names(word.freq), freq = word.freq);dm
head(dm, 5)
## Para usar la nube de palabras
wordcloud2(dm, size = 1, minRotation = -0.55, maxRotation = -0.55, rotateRatio = 2)