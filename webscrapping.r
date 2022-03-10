# Wordcloud das m�sicas de um artista com webscraping e text mining

# Carrega as fun��es necess�rias
library(rvest)
library(dplyr)
library(stringr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# Configura��o do link do artista do site vagalume

link <- "https://www.vagalume.com.br/the-beatles/"

# Leitura do html da p�gina do artista
page <- read_html(link) 



# Leitura dos links das m�sicas 

musicas_links <- page %>% 
  html_nodes("#alfabetMusicList .nameMusic") %>% # Pega o link da p�gina de cada m�sica
  html_attr("href") %>%
  paste("https://www.vagalume.com.br", ., sep = "")

head(musicas_links)

# Fun��o para extra��o das letras de cada m�sica que os links foram conseguidos acima 
get_letras <- function(musica_link){
  letras_page <- read_html(musica_link)
  todas_letras <- letras_page %>% html_node("#lyrics") %>% html_text2(preserve_nbsp = TRUE) %>% paste(collapse = " ")
}


letras <- sapply(musicas_links, FUN = get_letras, USE.NAMES = FALSE)
letras <- str_replace_all(letras, "\n", " ")



#########    Convers�o dos textos para wordcloud  ###########

df <- as.data.frame(letras)

# Convertendo para corpus
dfcorpus <- Corpus(VectorSource(df))
class(dfcorpus)

# Convertendo para texto plano 
dfcorpus <- tm_map(dfcorpus, PlainTextDocument)

# Removendo puntua��o 
dfcorpus <- tm_map(dfcorpus, removePunctuation)

# Remover stopwords Portugu�s
dfcorpus <- tm_map(dfcorpus, removeWords, stopwords("portuguese"))
dfcorpus <- tm_map(dfcorpus, removeWords, c("pra", "vai", "t�","ser","tudo"," que","n�o",
                                            "faz","vou","pro","ond","gent","todo","que ","que"))

# Convertendo palavras de diferentes vers�es em uma s�
dfcorpus <- tm_map(dfcorpus, stemDocument)


# Cria uma matriz para vizualiza��o das palavras 
dtm <- TermDocumentMatrix(dfcorpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
head(d, 20)

# Removendo palavras espec�ficas
dfcorpus <- tm_map(dfcorpus, removeWords, c("pra","this", "and", "dont", stopwords('english')))

# Cria��o do wordcloud
set.seed(1234)
wordcloud(dfcorpus, min.freq = 1, max.words = 100,
          random.order = FALSE, rot.per = 0.2,
          colors = brewer.pal(8, "Dark2"))














































