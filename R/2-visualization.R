library(tidyverse)
library(ggplot2)

#2.1 užduotis

data = readRDS("../data/692000.rds")
png(file="../img/1_Graph.png", width = 1500, height = 1000)
Histo = hist(data$avgWage, breaks = 200, main = paste("Vidutinio atlyginimo histograma"),
             xlab = "Atlyginimas", ylab = "Kiekis", col = 'green')
dev.off()

#2.2 užduotis

top5 = data %>%
  group_by(name) %>%
  summarise(top5i = max(avgWage)) %>%
  arrange(desc(top5i)) %>%
  head(5)

Histo2 = data %>%
  filter(name %in% top5$name) %>%
    mutate(Months = ym(month)) %>%
    ggplot(aes(x = Months, y = avgWage, color = name)) + geom_line() +
    labs(title = "5 įmonės, kurių vidutinės algos didžiausios", x = "Mėnesiai", y = "Vidutinis atlyginimas")
    ggsave("../img/Graph_2.png", Histo2, width = 2600, height = 1300, units = ("px"))
    
#2.3 užduotis
    
insured = data %>%
  filter(name %in% top5$name) %>%
  group_by(name) %>%
  summarise(MostInsured = max(numInsured)) %>%
  arrange(desc(MostInsured))

insured$name = factor(insured$name,levels = insured$name[order(insured$MostInsured, decreasing = T)] )
  
Histo3 = insured%>%
  ggplot(aes(x = name, y = MostInsured, fill = name)) +geom_col()+
  labs(title = "5 įmonės, kurių vidutinės algos didžiausios", x = "Mėnesiai", y  = "Apdraustieji")
ggsave("../img/Graph_3.png", Histo3, width = 4000, height = 2500, units = ("px"))
