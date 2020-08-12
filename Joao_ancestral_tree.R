

# Load packages
library(tidyverse)
library(ggtree)
library(ggnewscale)
#
#Random analyses
#Making the probability dataframe for pie charts
pieProb <- as.data.frame(Describe_SIMMAP$ace)
pieProb$node <- 1:tree$Nnode+Ntip(tree)
#First create base tree
p11 <- ggtree(tree) +
  geom_tiplab(size=1, fontface = "italic", align = TRUE, linetype = "dotted", linesize = 0.2, offset = 2.3) + 
#Then add clade labels (always a lot of tweaking on letter size and spacing of each different thing)
  #Outgroups
  geom_strip("Azendohsaurus", "Postosuchus", barsize=0.3, color='black', hjust = "center", extend = 0.1,
             label="Outgroup", fontsize = 2, angle = -90, align = TRUE, offset = 7, offset.text = 1) +
  #Ornithischia
  geom_strip("Lesothosaurus", "Talarurus", barsize=0.3, color='black', hjust = "center", extend = 0.1,        
		label="Thyreophora", fontsize = 1.5, angle = -90, align = TRUE, offset = 7, offset.text = 1) +
  geom_strip("Yinlong", "Chasmosaurus", barsize=0.3, color='black', hjust = "center", extend = 0.1,
             label="Ceratopsia", fontsize = 1.5, angle = -90, align = TRUE, offset = 7, offset.text = 1) +
  geom_strip("Tenontosaurus", "Hypacrosaurus", barsize=0.3, color='black', hjust = "center", extend = 0.1,
       label="Ornithopoda", fontsize = 1.5, angle = -90, align = TRUE, offset = 7, offset.text = 1) +
  geom_strip("Tianyulong", "Hypacrosaurus", barsize=0.3, color='black', hjust = "center", extend = 0.1,
       label="Ornithischia", fontsize = 2, angle = -90, align = TRUE, offset = 13, offset.text = 1) +
  #Sauropodomorpha
  geom_strip("Melanorosaurus", "Eoraptor", barsize=0.3, color='black', hjust = "center", extend = 0.1,  
	label="Basal Sauropodomorpha", fontsize = 1.5, angle = -90, align = TRUE, offset = 7, offset.text = 1) +
  geom_strip("Tazoudasaurus", "Camarasaurus", barsize=0.3, color='black', hjust = "center", extend = 0.1,
             label="Sauropoda", fontsize = 1.5, angle = -90, align = TRUE, offset = 7, offset.text = 1) +
  geom_strip("Tazoudasaurus", "Camarasaurus", barsize=0.3, color='black', hjust = "center", extend = 0.1,
             label="Sauropoda", fontsize = 1.5, angle = -90, align = TRUE, offset = 7, offset.text = 1) +
  geom_strip("Vouivria", "Opisthocoelicaudia", barsize=0.3, color='black', hjust = "center", extend = 0.1,
      label="Titanosauriformes", fontsize = 1.5, angle = -90, align = TRUE, offset = 7, offset.text = 1) +
  geom_strip("Opisthocoelicaudia", "Eoraptor", barsize=0.3, color='black', hjust = "center", extend = 0.1,
        label="Sauropodomorpha", fontsize = 2, angle = -90, align = TRUE, offset = 13, offset.text = 1) +
  #Theropoda
  geom_strip("Limusaurus", "Aucasaurus", barsize=0.3, color='black', hjust = "center", extend = 0.1,
         label="Ceratosauria", fontsize = 1.5, angle = -90, align = TRUE, offset = 7, offset.text = 1) +
  geom_strip("Afrovenator", "Megaraptor", barsize=0.3, color='black', hjust = "center", extend = 0.1,
             label="Tetanurae", fontsize = 1.5, angle = -90, align = TRUE, offset = 7, offset.text = 1) +
  geom_strip("Guanlong", "Tarbosaurus", barsize=0.3, color='black', hjust = "center", extend = 0.1,
     label="Tyrannosauroidea", fontsize = 1.5, angle = -90, align = TRUE, offset = 7, offset.text = 1) +
  geom_strip("Nqwebasaurus", "Struthiomimus", barsize=0.3, color='black', hjust = "center", extend = 0.1,
    label="Ornithomimosauria", fontsize = 1.5, angle = -90, align = TRUE, offset = 7, offset.text = 1) +
  geom_strip("Aorun", "Linhenykus", barsize=0.3, color='black', hjust = "center", extend = 0.1,
     label="Alvarezsauria", fontsize = 1.5, angle = -90, align = TRUE, offset = 7, offset.text = 1) +
  geom_strip("Falcarius", "Nothronychus", barsize=0.3, color='black', hjust = "center", extend = 0.1,
     label="Therizinosauria", fontsize = 1.5, angle = -90, align = TRUE, offset = 7, offset.text = 1) +
  geom_strip("Protarchaeopteryx", "Heyuannia", barsize=0.3, color='black', hjust = "center", extend = 0.1,
     label="Oviraptorosauria", fontsize = 1.5, angle = -90, align = TRUE, offset = 7, offset.text = 1) +
  geom_strip("Scansoriopteryx", "Sinornithoides",barsize=0.3,color='black',hjust = "center", extend = 0.1,
             label="Paraves", fontsize = 1.5, angle = -90, align = TRUE, offset = 7, offset.text = 1) +
  geom_strip("Xiaotingia", "Ichthyornis", barsize=0.3, color='black', hjust = "center", extend = 0.1,
             label="Avialae", fontsize = 1.5, angle = -90, align = TRUE, offset = 7, offset.text = 1) +
  geom_strip("Eodromaeus", "Ichthyornis", barsize=0.3, color='black', hjust = "center", extend = 0.1,
             label="Theropoda", fontsize = 2, angle = -90, align = TRUE, offset = 13, offset.text = 1)
#Add the heatmap (basically one coloured column along the tips of the tree)
#First create heatmap dataset, and convert character scores to character states (easier for legend)
data_heatmap <- Matrix_Digit_Count %>% select(PhIII4, Char = PhIII4) %>% rownames_to_column(var = "Taxa") %>%  mutate(Char_ = case_when(Char == "0" ~ "Absence", Char == "1" ~ "Presence", Char == "?" ~ "Unknown")) %>%  select (-Char) %>% column_to_rownames(var = "Taxa")
p11 <- gheatmap(p11, data_heatmap, offset=0.05, width=0.01, colnames = FALSE, color="black") +
  scale_fill_manual(values=c("grey","#51127CFF","#F1ED6FFF"), name = "PhIII4", breaks = c("Unknown", "Absence", "Presence")) + theme(legend.position=c(0.1, 0.88))
#Add pie charts
pies <- nodepie(pieProb, cols=1:2, color = c("#51127CFF","#F1ED6FFF"),  alpha=0.9)
p11 <- inset(p11, pies, x="node", width=0.04, height = 0.04)
#Save plot
ggsave("Figures/PhIII4_Plot.pdf", width = 14, height = 20)
