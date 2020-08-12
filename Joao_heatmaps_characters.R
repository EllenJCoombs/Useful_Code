

#Load packages
library(phytools)
library(ggtree)
library(ggimage)
library(tidyverse)
library(ggnewscale)
#Upload your tree and dataset
#First create base tree
plot <- ggtree(tree) +
  geom_tiplab(size=1, fontface = "italic", align = TRUE, linetype = "dotted", linesize = 0.2, offset = 2.3)
#Add heatmap Digit I
Digit1 <- Matrix_Digit_Count %>% select(McI, PhI1, PhI2, Ung1)
plot1 <- gheatmap(plot, Digit1, offset=12, width=0.1,
                 colnames_angle=315, colnames_offset_y = -3, colnames_position = "bottom", font.size = 3) +
  scale_fill_manual(
    values=c("grey","#51127CFF","#F1ED6FFF", "#FEBD82FF","#F97A5DFF","#CC3F71FF","#238A8DFF"),
    breaks = c("Unknown", "Absence", "Presence", "Claw Type I", "Claw Type II", "Hoof", "Thumb-Spike"))
#Add heatmap Digit II
Digit2 <- Matrix_Digit_Count %>% select(McII, PhII1, PhII2, PhII3, Ung2)
plot1 <- gheatmap(plot1, Digit2, offset=32, width=0.125,
                  colnames_angle=315, colnames_offset_y = -3, colnames_position = "bottom", font.size = 3) +
  scale_fill_manual(
    values=c("grey","#51127CFF","#F1ED6FFF", "#FEBD82FF","#F97A5DFF","#CC3F71FF","#238A8DFF"),
    breaks = c("Unknown", "Absence", "Presence", "Claw Type I", "Claw Type II", "Hoof", "Thumb-Spike"))
#Add heatmap Digit III
Digit3 <- Matrix_Digit_Count %>% select(McIII, PhIII1, PhIII2, PhIII3, PhIII4, Ung3)
plot1 <- gheatmap(plot1, Digit3, offset=56, width=0.15,
                  colnames_angle=315, colnames_offset_y = -3, colnames_position = "bottom", font.size = 3) +
  scale_fill_manual(
    values=c("grey","#51127CFF","#F1ED6FFF", "#FEBD82FF","#F97A5DFF","#CC3F71FF","#238A8DFF"),
    breaks = c("Unknown", "Absence", "Presence", "Claw Type I", "Claw Type II", "Hoof", "Thumb-Spike"))
#Add heatmap Digit IV
Digit4 <- Matrix_Digit_Count %>% select(McIV, PhIV1, PhIV2, PhIV3, PhIV4, PhIV5, Ung4)
plot1 <- gheatmap(plot1, Digit4, offset=87, width=0.175,
                  colnames_angle=315, colnames_offset_y = -3, colnames_position = "bottom", font.size = 3) +
  scale_fill_manual(
    values=c("grey","#51127CFF","#F1ED6FFF", "#FEBD82FF","#F97A5DFF","#CC3F71FF","#238A8DFF"),
    breaks = c("Unknown", "Absence", "Presence", "Claw Type I", "Claw Type II", "Hoof", "Thumb-Spike"))
#Add heatmap Digit V
Digit5 <- Matrix_Digit_Count %>% select(McV, PhV1, PhV2, PhV3, PhV4, Ung5)
plot1 <- gheatmap(plot1, Digit5, offset=122, width=0.15,
                  colnames_angle=315, colnames_offset_y = -3, colnames_position = "bottom", font.size = 3) +
  scale_fill_manual(
    values=c("grey","#51127CFF","#F1ED6FFF", "#FEBD82FF","#F97A5DFF","#CC3F71FF","#238A8DFF"),
    breaks = c("Unknown", "Absence", "Presence", "Claw Type I", "Claw Type II", "Hoof", "Thumb-Spike"))
#Add heatmap Fusion
Fusion <- Matrix_Digit_Count %>% select(Fus1_carpal, FusMcI_II, Fus2_carpal, FusMcII_III, Fus3_carpal, FusMcIII_IV)
plot1 <- gheatmap(plot1, Fusion, offset=152, width=0.15,
                  colnames_angle=315, colnames_offset_y = -3, colnames_position = "bottom", font.size = 3) +
  scale_fill_manual(
    values=c("grey","#51127CFF","#F1ED6FFF", "#FEBD82FF","#F97A5DFF","#CC3F71FF","#238A8DFF"),
    breaks = c("Unknown", "Absence", "Presence", "Claw Type I", "Claw Type II", "Hoof", "Thumb-Spike")) + 
  theme(legend.position=c(0.1, 0.88))
#Add heatmap Diet and Stance
Diet_Stance <- Matrix_Digit_Count %>% select(Diet, Stance)
plot2 <- plot1 + new_scale_fill()
      gheatmap(plot2, Diet_Stance, offset=182, width=0.05,
               colnames_angle=315, colnames_offset_y = -3, colnames_position = "bottom", font.size = 3) +
      scale_fill_manual(
        values=c("#7AD151FF", "#CB2314", "#7294D4",   #Diet colours
             "#EAD3BF", "#39312F", "#B6854D", "#AA9486"), #Stance colours
      breaks = c("Herbivore", "Faunivore", "Omnivore",
                 "Bipedal", "Quadrupedal", "Facultative Bipedal", "Facultative Quadrupedal")) + 
      theme(legend.position=c(0.1, 0.8))
#Save plot
ggsave("Figures/Dataset_Summary_Figure/Dataset_Summary_Figure.pdf", width = 15, height = 25)
