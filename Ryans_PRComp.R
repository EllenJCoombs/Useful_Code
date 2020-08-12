#Ryan's nice morphospace 

#shape is procrustes aligned data, in a 3d array
#skull pca
pca_res <- gm.prcomp(shape)
#or just good specs
#pca_res <- plotTangentSpace(shape[,,-which(dimnames(shape)[[3]]%in%reconstructed)])
pca_scores <- as_tibble(pca_res$x)
pca_scores <- pca_scores %>% mutate(., Species=rownames(pca_res$x))
comp2<-completeness %>% filter(., Tip_label %in% pca_scores$Species) %>% arrange(., Tip_label) %>% filter(., !Filename%in%c('Crocodylus_acutus_AMNH_7857_cranium',"Crocodilus_biporcatus_cranium_MNHN_A_5316"))
#pca_scores <- pca_scores %>% mutate(., CladeA = comp2$Clade2, CladeB = as.factor(comp2$Clade),Extinct=as.factor(comp2$`extinct?`),Diet=as.factor(comp2$Diet2),Habitat=as.factor(comp2$Habitat))
library(hablar)
pca_scores2 <- left_join(comp2, pca_scores, by = c("Tip_label" = "Species"))
pca_scores2 <- pca_scores2 %>% convert(fct(Diet, Diet2, `extinct?`, Habitat))
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(pca_scores2, aes(x=Comp1, y=Comp2, color=Clade))+
  geom_point()
g <- ggplot(pca_scores2, aes(x=Comp1, y=Comp2, fill=Clade, color=as.factor(`extinct?`), label=sub("_", " ", Taxon))) +
  geom_point(size=5,aes(shape=Clade,alpha=Reconstructed))+
  theme_bw()+
  theme(aspect.ratio = 1) +
  scale_shape_manual(values=c(21, 21,22,22,23,24,25,25)) +
  scale_color_manual(values=c("gray","black"),labels=c("Extant","Extinct"))+
  scale_fill_manual(values=cbPalette)+
  scale_alpha(range=c(0.4, 1),guide=FALSE)+
  #ggtitle("Principal Components Analysis") +
  xlab(paste0("PC Axis 1 (", signif((pca_res$pc.summary$importance[2,1]*100),3), "% of Total Variance)")) + 
  ylab(paste0("PC Axis 2 (",signif((pca_res$pc.summary$importance[2,2]*100),3),"% of Total Variance)")) +
  labs(fill = "Clade", shape= "Clade", color = "Extinct or Extant", alpha = "Reconstructed?")
g2<- g + geom_text_repel(inherit.aes = FALSE, aes(fontface=3,x=Comp1, y=Comp2,label=sub("_", " ", Taxon) ))
