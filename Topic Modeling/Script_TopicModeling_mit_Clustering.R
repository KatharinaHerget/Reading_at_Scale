######################## TOPIC MODELING & CLUSTERING  ##########################
#------------------------------------------------------------------------------#
#Basis des Skriptes ist: '5-4-topicModeling.R' in: 
#Weitin, Thomas: Skripte und Daten zu Digitale Literaturgeschichte:
#Eine Versuchsreihe in 7 Experimenten, 2022 
#https://github.com/thomasweitin/Digitale_Literaturgeschichte.
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
## Voreinstellungen und Korpusvorbereitung-------------------------------------#
#------------------------------------------------------------------------------#

##Bei Bedarf: Speicherbelegung erhöhen
options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx16g"))
gc()

## Korpusvorbereitung
# Funktion zur Erstellung variabler Text-Chunks aus Buchstabenvektoren (Jockers 2014)
# Entfernt Zeilenumbrüche, Satz- und Leerzeichen und wandelt alle Wörter in Kleinschreibung um
# Gibt einen Dataframe mit Text-Chunks zurück
makeFlexTextChunks <- function (doc.object, chunk.size = 10, percentage = TRUE) 
  {
  words.v <- paste (doc.object, collapse=" ")
  words.lower.v <- tolower(words.v)
  words.regex.v <- gsub ("[^[:alnum:][:space:]'-]", " ", words.lower.v)
  words.l <- strsplit (words.regex.v, "\\s+")
  word.v <- unlist (words.l)
  x.i <- seq_along (word.v)
  if (percentage) {
    max.length <- length (word.v) / chunk.size
    chunks.l <- split (word.v, ceiling (x.i / max.length))
  } else { 
    if (length(word.v) > chunk.size) {
      chunks.l <- split (word.v, ceiling (x.i / chunk.size))
      if (length(chunks.l[[length(chunks.l)]]) <=
          chunk.size / 2) {
        chunks.l[[length(chunks.l)-1]] <-
          c(chunks.l[[length(chunks.l)-1]],
            chunks.l[[length(chunks.l)]])
        chunks.l[[length(chunks.l)]] <- NULL
      }
    } else {
      chunks.l <- split (word.v, ceiling (x.i / chunk.size))
    }
  }
  chunks.l <- lapply (chunks.l, paste, collapse = " ")
  chunks.df <- do.call (rbind, chunks.l)
  return (chunks.df)
}


#------------------------------------------------------------------------------#
## Verwendete Packages aufrufen------------------------------------------------#
#------------------------------------------------------------------------------#

library(gtools) 
library(mallet)
library(tokenizers) 
library(wordcloud)
library(xlsx)

#------------------------------------------------------------------------------#
## Parametereinstellung--------------------------------------------------------#
#------------------------------------------------------------------------------#

## Modellparameter einstellen
# Anzahl der Topics, Iterationen, Chunk-Größe und Top-Wörter pro Topic
num.topics <- 100
iteration <- 100000
chunk.size <- 500
num.topic.words <- 20

## Name des Korpus
corpus <- "DNS-NDNS"

#Dateibenennung mit Modelldaten
filename <- paste0("TM_",
                   chunk.size,
                   "chunk_",
                   num.topics,
                   "topics_",
                   iteration,
                   "iter_",
                   num.topic.words,
                   "words_")



## Arbeitsumgebung und Korpora ansteuern
path <- "C:\\TopicModeling"             
path_data <- paste0(path, "\\", "")
path_corpus <- paste0(path, "\\", "corpora\\DNS_NDNS")
output_dir <- paste0(path, "\\", "DNS_NDNS_output\\")

## Stoppwortliste einlesen
stopwords <- paste0(path_data , 
                    "stoppwods_umfangreich.txt")


#------------------------------------------------------------------------------#
## Preprocessing --------------------------------------------------------------#
#------------------------------------------------------------------------------#

setwd(path_corpus)

corpus.files.v <- dir(path = path_corpus, "\\.txt$")
len <- length(corpus.files.v)
corpus.files.v <- mixedsort(corpus.files.v)

#Chunking der Texte
topic.m <- NULL
texttitle <- rep("", length(corpus.files.v))
for (i in 1:length(corpus.files.v)) {
  text.v <- scan(file.path(path_corpus, corpus.files.v[i]),
                 what = "character", sep = "\n" , encoding = "UTF-8", quiet = TRUE)
  chunk.m <- makeFlexTextChunks(text.v, chunk.size, percentage = FALSE)
  textname <- gsub("\\..*","", corpus.files.v[i]) # delete the fileending
  textname <- gsub("_", "", textname) 
  texttitle[i] <- paste(textname, collapse = " ")
  segments.m <- cbind(paste(textname, segment = 1:nrow(chunk.m), sep="_"), 
                      chunk.m)
  topic.m <- rbind(topic.m, segments.m)
}

documents <- as.data.frame(topic.m, stringsAsFactors = F)
colnames (documents) <- c ("id", "text")


#mallet starten
#verbliebene Einzelbuchstaben aus dem Korpus entfernen
mallet.instances <- mallet.import (documents$id,
                                   documents$text,
                                   stopwords,
                                   FALSE,
                                   token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")


#------------------------------------------------------------------------------#
## Topic Modeling starten------------------------------------------------------#
#------------------------------------------------------------------------------#

#Topic Modeling ausfuehren
topic.model <- MalletLDA(num.topics)

#Trainingskorpus erstellen
topic.model$loadDocuments(mallet.instances)

#Wortfrequenzen des Modells erheben
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)
write.table(word.freqs, paste0(output_dir, "/frequencies", filename, 
                               "_wordfreqs.csv"), dec = ".", sep = " ")


#------------------------------------------------------------------------------#
## Modell trainieren-----------------------------------------------------------#
#------------------------------------------------------------------------------#

##Optimierung der Hyperparameter
# default 20, 50
topic.model$setAlphaOptimization(20, 80)

#Modelltraining ausfuehren
topic.model$train(iteration)

# Topic-Type Zuordnung
topic.model$maximize(10)


#------------------------------------------------------------------------------#
## Topic Modeling Daten erheben------------------------------------------------#
#------------------------------------------------------------------------------#

##Dataframe der Topics und Topicwoerter erstellen
#Topics in den Spalten, Topicwoerter in den Zeilen schreiben
doc.topics.m <- mallet.doc.topics(topic.model,
                                  smoothed = T,
                                  normalized = T)
                                  
topic.words.m <- mallet.topic.words(topic.model,
                                    smoothed = TRUE,
                                    normalized = TRUE)

#------------------------------------------------------------------------------#
## Clustering des Modells------------------------------------------------------#
#------------------------------------------------------------------------------#

##Hierarchisches Clustering der Topics
#Benennung der Topics nach den Top 3 Woertern
topic.labels <- mallet.topic.labels(topic.model, topic.words.m, num.top.words = 3)

##Einstellung der Berechnungsgrundlage
#'Balance' definiert die Aehnlichkeitsreferenz 
#Skala von 0,0 (Dokumentenebene) bis 1,0 (Wortebene)
hbalance <- 0.5
hclust_data <- mallet.topic.hclust(doc.topics.m, topic.words.m, balance = hbalance)
hclust_data$labels <- topic.labels

##Clustering-Daten speichern
hclust_file <- paste0(output_dir, "hclust_data_", num.topics, "topics_", 
                      iteration, "iter_", hbalance, "bal.RDA")
saveRDS(hclust_data, file = hclust_file )

##Visualisierung als Dendrogramm, abspeichern
numk <- 40
require(factoextra)
new_svg <- paste0(output_dir, "hclust_dendrogram_", "topics_", iteration, 
                  "iter_", hbalance, "bal_", numk,"colors.svg")
svg(new_svg)
fviz_dend(x = hclust_data, 
          cex = 0.3, 
          lwd = 0.5, 
          k=numk, 
          k_colors = "jco",
          rect_border = "jco",
          rect_fill = TRUE,
          horiz = TRUE, 
          repel = TRUE)

dev.off() 

#------------------------------------------------------------------------------#
## Werte auslesen und speichern------------------------------------------------#
#------------------------------------------------------------------------------#

#Topicwoerter erheben und als Matrize abspeichern
colnames(topic.words.m) <- vocabulary

topic.word.weight.m <- NULL

for (i in 1: num.topics) {
  topic.top.words <- mallet.top.words(topic.model,
                                      topic.words.m[i,], num.topic.words)
  topic.wrd <- cbind(paste("Topic ", i), rbind(topic.top.words$term))
  topic.wght <- cbind(paste("Word Weights Topic", i), 
                      rbind(topic.top.words$weight))
  topic.ww <- rbind(topic.wrd, topic.wght)
  topic.word.weight.m <- rbind(topic.word.weight.m, topic.ww)
}

topic.word.weight.m <- t(topic.word.weight.m)
topic.word.weight.df <- as.data.frame(topic.word.weight.m, stringsAsFactors = FALSE)
colnames(topic.word.weight.df) <- topic.word.weight.df[1,]
colnames_df <- colnames(topic.word.weight.df)
topic.word.weight.df <- topic.word.weight.df[-1,]

#Datensortierung: Wortgewichte als numerische Daten erheben und speichern

for (i in 1:ncol(topic.word.weight.df)) {
  if (i %% 2){
    next
  }
  topic.word.weight.df[, i] <- as.numeric(topic.word.weight.df[, i])
}


write.table(topic.word.weight.df,
            paste0(output_dir, "/", filename, 
                   "_wordweight.csv"), dec = ".", sep = " ")

#Wortgewichte normalisieren
ls <- list()
for (i in 1:ncol(topic.word.weight.df)) {
  if (class(topic.word.weight.df[,i]) == "character"){
    ls[i] <- 0
  } else {
    ls[i] <- sum(topic.word.weight.df[,i])
  }
}

topic.word.weight.df[num.topic.words+1,] <- ls

normalised.word.weight.df <- topic.word.weight.df
for (i in 1:ncol(normalised.word.weight.df)) {
  if (i %% 2 == 0) {
    normalised.word.weight.df[,i] <- (normalised.word.weight.df[,i]/
                                        normalised.word.weight.df
                                      [num.topic.words+1,i])*100
  }
}


write.table(normalised.word.weight.df,
            paste0(output_dir, "/", filename, 
                   "_normalised_wordweight.csv"), dec = ".", sep = " ")



#------------------------------------------------------------------------------#
## Topic-Text-Wahrscheinlichkeiten---------------------------------------------#
#------------------------------------------------------------------------------#

#Dokumententitel erheben
file.ids.v <- documents[,1]
file.id.l <- strsplit(as.character(file.ids.v), "_")
file.chunk.id.l <- lapply(file.id.l, rbind)
file.chunk.id.m <- do.call(rbind, file.chunk.id.l)

# Dataframe mit Dokumententitel und Topic-Text-Gewichten erstellen
doc.topics.df <- as.data.frame(doc.topics.m)
doc.topics.df <- cbind(file.chunk.id.m[,1], doc.topics.df)
doc.topic.means.df <- aggregate(doc.topics.df[, 2:ncol(doc.topics.df)],
                                list(doc.topics.df[,1]),
                                mean)
doc.topic.means.order.df <- doc.topic.means.df[order(order(texttitle)),]
colnames(doc.topic.means.order.df) <- c(c("Text"), sprintf("Topic%s", 
                                                           1:num.topics))

doc.topic.means.order.df$Text <- gsub("([A-Z][a-z]+?)([A-Z].+)(\\d{4})", 
                                      "\\1_\\2_\\3", 
                                      doc.topic.means.order.df$Text, 
                                      perl = TRUE)

write.table(doc.topic.means.order.df,
            paste0(output_dir, "/", filename, 
                   "topics-in-docs.csv"),
            row.names = F, dec = ".", sep = " ")



#------------------------------------------------------------------------------#
## Wordclouds erstellen--------------------------------------------------------#
#------------------------------------------------------------------------------#

new_pdf <- paste0(output_dir, "/Wordclouds_allTopics.pdf")
my.plots <- vector(num.topics, mode='list')

for(i in 1:num.topics){
  topic.top.words <- mallet.top.words(topic.model,
                                      topic.words.m[i,], 25)
  
  wordcloud(topic.top.words$term,
            topic.top.words$weight,
            scale = c(2,.5),
            min.freq = 1, max.words = 100, rot.per=0,
            random.order=FALSE)
  my.plots[[i]] <- recordPlot()
  
}

graphics.off()

pdf(new_pdf, 10, 10, onefile=TRUE)

for (my.plot in my.plots) {
  replayPlot(my.plot)
}
graphics.off()


#------------------------------------------------------------------------------#
## Topic-Text-Profile erstellen------------------------------------------------#
#------------------------------------------------------------------------------#

file <- paste0(output_dir, filename, "topics-in-docs.csv")

topicswithnames <- read.table(file, sep = " ", dec = ".", header = TRUE)
names <- topicswithnames[,1]
topicswith_nonames <- topicswithnames[,2:length(topicswithnames)]

new_pdf <- paste0(output_dir, "/TopicsPerText_", corpus, ".pdf")
topics_text.plots <- vector(len, mode='list')

for (i in 1:len){
  subrow <- topicswith_nonames[i, 1:length(topicswith_nonames)]
  barplot(unlist(subrow), main = paste0("Topic-Verteilung in ", 
                                        names[i]), las = 2,
          cex.names = 0.8)
  topics_text.plots[[i]] <- recordPlot()
}

graphics.off()

pdf(new_pdf, 20, 15, onefile=TRUE)
for (my.plot in topics_text.plots) {
  replayPlot(my.plot)
}
graphics.off()


new_pdf <- paste0(output_dir, "/TextsPerTopic_", corpus, ".pdf")
texts_topic.plots <- vector(num.topics, mode='list')


for (i in 1:length(topicswith_nonames)){
  subcolumn <- topicswith_nonames[1:len, i]
  barplot(unlist(subcolumn), main = paste0("Anteile aller Texte an Topic ", i), 
          cex.names= 0.5,
          names.arg  = names, las = 2)
  texts_topic.plots[[i]] <- recordPlot()
}

graphics.off()

pdf(new_pdf, 50, 25, onefile=TRUE)
for (my.plot in texts_topic.plots) {
  replayPlot(my.plot)
}
graphics.off()
dev.off()
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
