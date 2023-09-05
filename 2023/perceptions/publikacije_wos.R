library(here)
library(dplyr)
library(ggplot2)

analyze <- read.table("analyze (5).txt", sep="\t", header=TRUE)


ggplot(analyze,aes(x= reorder(Publication.Years,-Record.Count),Record.Count))+
  geom_bar(stat ="identity")

par(mai=c(1,1,0.25,0.25))

df <- analyze[order(analyze$Publication.Years,decreasing = FALSE),]
barplot <- barplot(df$Record.Count,names.arg = df$Publication.Years,las = 2,
        col ="lightblue",
        ylab = "Number of results",
        ylim = c(0,110))

text(barplot,df$Record.Count+4,labels=as.character(df$Record.Count), cex = 0.8)
