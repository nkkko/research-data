# source: https://gist.github.com/briatte/7b9b70859a4bd5ff7ec6ddda6452e516#file-5_network_object-r

library(here)
library(readxl) # read excel
library(dplyr)
library(ggnetwork)
library(ggplot2)
library(readr)
library(stringr)
library(tnet)
library(network) # keep after tnet
library(cowplot)

data <- read_xlsx("results-survey517778_tools.xlsx")
data[data == "Izvanredni ili redoviti profesor" | data == "Uprava" | data == "Asistent ili docent" | data == "Predavač" ] <- "University teacher"
data[data == "Student prediplomskog studija" | data == "Student diplomskog studija" | data == "Student poslijediplomskog studija"] <- "Student"
data[data == "Humanističke znanosti"] <- "Humanities"    
data[data == "Društvene znanost"] <- "Social Sciences"  
data[data == "Umjetničko područje"] <- "Arts"
data[data == "Tehničke znanosti"] <- "Technical Sciences"       
data[data == "Biomedicina i zdravstvo"] <- "Biomedicine and Health"     
data[data == "Biotehničke znanosti"] <- "Biotechnical Sciences"        
data[data == "Interdisciplinarno područje"] <- "Interdisciplinary"
data[data == "Prirodne znanosti"] <- "Natural Sciences"           

### novi stupac u kojem smo grupirali drustvene znanosti i umjetnicke naspram STEM
data <- data %>%
  mutate(STEMvsSSHA=if_else(`Područje znanosti ili umjetnosti`=="Humanities","SSHA", 
                            if_else(`Područje znanosti ili umjetnosti`=="Social Sciences","SSHA",
                                    if_else(`Područje znanosti ili umjetnosti`=="Arts","SSHA","STEM"))),
         .after=`Područje znanosti ili umjetnosti`)

data_filtered <- data 
# %>% filter(data$STEMvsSSHA == "STEM")

data_tmp <- data_filtered$Tools %>%
  strsplit(",")

for (i in 1:length(data_tmp)) {
  data_tmp[[i]] <- tolower(unique(data_tmp[[i]]))
}

e <- data_tmp %>%
  lapply(function(x) {
    expand.grid(x, x, w = 1 / length(x), stringsAsFactors = FALSE)
  }) %>%
  bind_rows

e <- apply(e[, -3], 1, str_sort) %>%
  t %>%
  data.frame(stringsAsFactors = FALSE) %>%
  mutate(w = e$w)

e <- group_by(e, X1, X2) %>%
  summarise(w = sum(w)) %>%
  filter(X1 != X2)

e <- e %>% # filtriranje po tezini da ne prikaze sve
  filter(w > 0.5)

# undirected network
n <- network(e[, -3], directed = FALSE)

stopifnot(nrow(e) == network.edgecount(n))
set.edge.attribute(n, "weight", e$w)

# weighted degree at alpha = 1
t <- as.edgelist(n, attrname = "weight") %>%
  symmetrise_w %>%
  as.tnet %>%
  degree_w

stopifnot(nrow(t) == network.size(n))
set.vertex.attribute(n, "degree_w", t[, "output" ])

# show only keywords at or above median weighted degree
l <- n %v% "degree_w"
l <- ifelse(l >= median(l), network.vertex.names(n), NA) # median(l)

stopifnot(length(l) == network.size(n))
set.vertex.attribute(n, "label", l)

ggplot(n, aes(x, y, xend = xend, yend = yend)) +
  geom_edges(aes(color = weight), curvature = 0.15) +
  geom_nodes(color = "grey50") +
  geom_nodelabel(aes(size = degree_w, label = label),
                 color = "grey20", label.size = NA, alpha = 0.5, 
                 nudge_y = 0.02,) +
  scale_size_continuous(range = c(1, 5)) +
  scale_color_gradient2(low = "grey25", midpoint=0, mid="gray85", high = "red") +
  guides(size = "none", color = "none") + 
  theme_blank()

### END


df_freq <- data$Tools %>%
  strsplit(",")

for (i in 1:length(df_freq)) {
  df_freq[[i]] <- tolower(unique(df_freq[[i]]))
}

df_freq <- df_freq %>%
  unlist %>%
  trimws %>%
  table %>%
  data.frame %>%
  arrange(-Freq) 
# filter(Freq > 1)

colnames(df_freq) <- c("word", "freq")

# ggplot(df_freq, aes(reorder(word, desc(freq)),freq)) +               # Draw barchart with ggplot2 package
#   geom_bar(stat = "identity", position = "dodge") +
#   theme(axis.text.x = element_text(angle = 90,
#                                    hjust = 1))

sum(df_freq$freq)
dfs2xlsx(withNames("sheet1",df_freq), "summary/tools_freq.xlsx")

linch <-  max(strwidth(df_freq[1:30,]$word, "inch")+0.2, na.rm = TRUE)
par(mai=c(linch,1,0,0))

barplot <- barplot(df_freq[1:30,]$freq, las = 2, names.arg = df_freq[1:30,]$word,
        col ="lightblue", 
        ylab = "Tool frequencies",
        ylim = c(0,110),
        cex.names=1)

text(barplot,df_freq[1:30,]$freq+4,labels=as.character(df_freq[1:30,]$freq), cex = 0.8)

df_freq$word %>%
  unique %>%
  count()

