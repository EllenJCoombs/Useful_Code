
#Ryan's super nice ggtree phylogeny

library(deeptime)
library(ggtree)
library(treeio)
library(tidyverse)

timetree <- treeio::as.treedata(treelist2[[1]])

eco <-  mutate(eco, label = eco$Tip_label)
tree_w_data <- full_join(timetree, eco, by = "label")

tree_w_data@extraInfo

#plot the topology:
ggtree(tree_w_data, size=2)
#fiddle with appearance:
ggtree(tree_w_data, size=2)
ggtree(tree_w_data, size=1.5, col='blue')

ggtree(tree_w_data, layout="circular")

#add labels
ggtree(tree_w_data) +
  geom_tiplab(label= sub("_", " ",tree_w_data@phylo$tip.label),
              size=3,
              color = "black",
              family = "Arial",
              fontface="italic") 

#add margin so the tip labels arent chopped off
ggtree(tree_w_data) +
  geom_tiplab(label= sub("_", " ",tree_w_data@phylo$tip.label),
              size=3,
              color = "black",
              family = "Arial",
              fontface="italic") +
  coord_cartesian(clip = 'off') +
  theme(plot.background = element_blank(), plot.margin = margin(.2,3,.2,.2, "cm"))

#add symbols on the tips
ggtree(tree_w_data) +
  geom_tiplab(label= sub("_", " ",tree_1_w_data@phylo$tip.label),
              size=3,
              color = "black",
              family = "Arial",
              fontface="italic",
              offset=5.5) + ####note that you need to offset the tip labs to make room for symbols
  coord_cartesian(clip = 'off') +
  theme(plot.background = element_blank(), plot.margin = margin(.2,3,.2,.2, "cm"), legend.position=c(.32,.83))+
  geom_tippoint(aes(fill=Diet2, x=x+4),color="black", shape=23, )+ 
  scale_fill_manual(breaks = c("Carnivore","Omnivore/Herbivore","Piscivore"), values = c("blue","red","grey"))

#now the hard part: adding geologic time scale:
plot1 <- ggtree(tree_w_data) +
  geom_tiplab(label= sub("_", " ",tree_1_w_data@phylo$tip.label),
              size=3,
              color = "black",
              family = "Arial",
              fontface="italic",
              offset=5.5) + ####note that you need to offset the tip labs to make room for symbols
  coord_cartesian(clip = 'off') +
  theme(plot.background = element_blank(), plot.margin = margin(.2,3,.2,.2, "cm"), legend.position=c(.32,.83))+
  geom_tippoint(aes(fill=Diet2, x=x+4),color="black", shape=23, )+ 
  scale_fill_manual(breaks = c("Carnivore","Omnivore/Herbivore","Piscivore"), values = c("blue","red","grey"))+
  coord_cartesian(xlim = c(-200, 90), #you have to fiddle with these values to get your tip labels to show. the first value should be just before your root time, second value pads out space for tip labels
                ylim = c(-2, 45), #first value makes room for geo timescale, second value is vertical space and should be a few more than your number of tips
                expand = FALSE) +
  scale_x_continuous(breaks=-periods$max_age[c(1:5)], labels=periods$max_age[c(1:5)]) + 
  theme(panel.grid.major.x = element_line(colour="grey", size=0.5), legend.key.height =unit(.4,"cm"))#should also be modified based on your time scale limits

plot1 #bad

plot2 <- revts(plot1);plot2 #good!
plot2 <-  gggeo_scale(plot2, neg = FALSE, center_end_labels = TRUE, height = unit(1, "line"), size=3)
plot2

