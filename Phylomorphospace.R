

#Load packages
library(tidyverse)
library(ggfortify)
library(ggphylomorpho)
library(gginnards)
#_______________________________________________________________________________
#Create dataframe for plotting
#First create column with specimen ID to match with the ID from the species_data_Mc
pcscores_Resid <- as_tibble(PCA_Resid$x) %>%  mutate(.,ID=rownames(PCA_Resid$x))
#Combine the pcscores with species_data_Mc
pcscores_Resid <- left_join(pcscores_Resid,species_data_Mc)
#Sort the table by custom Clade order (Sauropodomorphs, Theropods, Ornithiscians) and Stance (Bipedal, Facultative, Quadrupedal). This is important especially for plot legend
#Clade names will be different for each Mc!
#Clade
pcscores_Resid$Clade3 <- factor(pcscores_Resid$Clade3, levels = c("Sauropodomorpha", "Sauropoda", "Titanosauriformes", "Theropoda", "Ceratosauria", "Tetanurae", "Tyrannoraptora", "Maniraptoriformes", "Stegosauria", "Ankylosauria", "Ceratopsia", "Coronosauria", "Neornithischia", "Iguanodontia"))
#Stance
pcscores_Resid$Stance <- factor(pcscores_Resid$Stance, levels = c("Bipedal", "Facultative", "Quadrupedal"))
pcscores_Resid <- pcscores_Resid[order(pcscores_Resid$Clade3,pcscores_Resid$Stance),]
#_______________________________________________________________________________
#_______________________________________________________________________________
#Generate PCA plot
#First use package ggphylomorpho to create a first phylomorphospace plot
g <- ggphylomorpho(tree=tree_Mc, tipinfo = pcscores_Resid, xvar=PC1, yvar = PC2, factorvar = Stance, labelvar = ID, tree.alpha = 0.7)
#Then, using the package gginnards, remove the GeomPoint (points at the tip of the "tree") and GeomTextRepel (text by each point identifiying each specimen) so you just have the tree network
g <- g %>%
  delete_layers("GeomPoint") %>%
  delete_layers("GeomTextRepel")
#Creating convex hulls
hull_clade <- pcscores_Resid %>%
  group_by(Clade3) %>%
  slice(chull(PC1,PC2)) %>%
  rename(x=PC1) %>%
  rename(y=PC2)
#PCA variation percentage for axis labels in plot
Summary_PCA_Resid <- summary(PCA_Resid)
#_______________________________________________________________________________
#Main plot (with everything: phylogeney + convex hulls + ID)
g + geom_polygon(data = hull_clade, aes(x=x,y=y,fill=Clade3), alpha = .5, show.legend = FALSE) +
  geom_text(data = pcscores_Resid, aes(x=PC1, y=PC2, label=ID, size = 1, hjust=0,vjust=0), show.legend = FALSE) +
  geom_point(data = pcscores_Resid, aes(x=PC1, y=PC2, shape = Stance , colour = Clade3), size = 5) +
  #colour scheme
  scale_colour_manual(values=c("skyblue2", "steelblue4", "navyblue", #Sauropodomorpha
                               "gold1", "violetred", "darkorange3", "tomato3", "darkred", #Theropoda
                               "seagreen1", "palegreen1", "seagreen4", "palegreen4", #Thyreophora & Ceratopsia
                               "lawngreen", "green")) + #Neornithischia & Ornithopoda
  scale_shape_manual(values=c(17, 18, 15)) + #polygon shapes for stance
  #polygon fill colours
  scale_fill_manual(values=c("skyblue2", "steelblue4", "navyblue", #Sauropodomorpha
                             "gold1", "violetred", "darkorange3", "tomato3", "darkred", #Theropoda
                             "seagreen1", "palegreen1", "seagreen4", "palegreen4", #Thyreophora & Ceratopsia
                             "lawngreen", "green")) + #Neornithischia & Ornithopoda
  theme_classic() +
  ggtitle("McI: PC1 vs PC2") +
  guides(fill = guide_legend(override.aes = list(shape = NA))) +  #remove annoying dots in the colour legend
  labs(colour = "Clade") + #change legend title name
  theme(legend.title = element_text(face="bold"), #Legend titles in bold
        legend.justification = "top") + #Legend position
  #Axis labels
  labs(x=paste("PC1 ", "(", round(Summary_PCA_Resid$tips[2,1]*100, 1), "%)", sep=""),
       y=paste("PC2 ", "(", round(Summary_PCA_Resid$tips[2,2]*100, 1), "%)", sep=""), axes=FALSE, cex.lab = 10) + 
  theme(axis.title.x = element_text(size = rel(1.15))) +
  theme(axis.title.y = element_text(size = rel(1.15)))
#Save plot to folder
ggsave(paste("McI_PCA_everything.pdf"), path = "Plots", width = 30, height = 15)
