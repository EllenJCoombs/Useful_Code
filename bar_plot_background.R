
#Plot a bar plot with the background (total data in the background)
specimens_count <- read.csv('specimens_count.csv', fileEncoding="UTF-8-BOM") #read in specimens data 

d = specimens_count

d_bg <- d[, -2] #remove the suborder column

p <- ggplot(d, aes(x = fct_inorder(age), fill = suborder)) + #fct in order that you load it in 
  geom_bar(data = d_bg, fill = "grey", alpha = .5) +
  geom_bar(colour = "black") +
  facet_wrap(~ suborder) +
  guides(fill = FALSE) +  
  theme_bw()  

p
