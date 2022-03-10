# Wordcloud das músicas de um artista com webscraping e text mining

# Carrega as funções necessárias
library(rvest)
library(dplyr)
library(stringr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# Configuração do link do artista do site vagalume

link <- "https://www.vagalume.com.br/the-beatles/"

# Leitura do html da página do artista
page <- read_html(link) 



# Leitura dos links das músicas 

musicas_links <- page %>% 
  html_nodes("#alfabetMusicList .nameMusic") %>% # Pega o link da página de cada música
  html_attr("href") %>%
  paste("https://www.vagalume.com.br", ., sep = "")

head(musicas_links)

# Função para extração das letras de cada música que os links foram conseguidos acima 
get_letras <- function(musica_link){
  letras_page <- read_html(musica_link)
  todas_letras <- letras_page %>% html_node("#lyrics") %>% html_text2(preserve_nbsp = TRUE) %>% paste(collapse = " ")
}


letras <- sapply(musicas_links, FUN = get_letras, USE.NAMES = FALSE)
letras <- str_replace_all(letras, "\n", " ")



#########    Conversão dos textos para wordcloud  ###########

df <- as.data.frame(letras)

# Convertendo para corpus
dfcorpus <- Corpus(VectorSource(df))
class(dfcorpus)

# Convertendo para texto plano 
dfcorpus <- tm_map(dfcorpus, PlainTextDocument)

# Removendo puntuação 
dfcorpus <- tm_map(dfcorpus, removePunctuation)

# Remover stopwords Português
dfcorpus <- tm_map(dfcorpus, removeWords, stopwords("portuguese"))
dfcorpus <- tm_map(dfcorpus, removeWords, c("pra", "vai", "tá","ser","tudo"," que","não",
                                            "faz","vou","pro","ond","gent","todo","que ","que"))

# Convertendo palavras de diferentes versões em uma só
dfcorpus <- tm_map(dfcorpus, stemDocument)


# Cria uma matriz para vizualização das palavras 
dtm <- TermDocumentMatrix(dfcorpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
head(d, 20)

# Removendo palavras específicas
dfcorpus <- tm_map(dfcorpus, removeWords, c("pra","this", "and", "dont", stopwords('english')))

# Criação do wordcloud
set.seed(1234)
wordcloud(dfcorpus, min.freq = 1, max.words = 100,
          random.order = FALSE, rot.per = 0.2,
          colors = brewer.pal(8, "Dark2"))














































