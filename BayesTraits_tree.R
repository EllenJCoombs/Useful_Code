#Run this function first 

#uses treeio to combine tree topology and bayestraits posterior into single S4 object for plotting.
add_rjpp_to_tree <- function(rjpp_out){
  rjpp_data <- as_tibble(rjpp_out$data)
  timetree <- rjpp_out$meantree
  timetree$edge.length <- rjpp_data$orgBL[-1]
  timetree$root.time <- max(nodeHeights(timetree))
  rjpp_data_nodes <- rjpp_data %>% rename(., node=descNode) %>% mutate(., iters = rjpp_out$niter) %>% mutate(., ppRate = round(nOrgnNRate/iters,2))
  timetree <- treeio::as.treedata(timetree)
  treedata <- treeio::full_join(timetree, y = rjpp_data_nodes, by = "node")
  return(treedata)
}

#You can do cool things like this 
tree_1_BTraits<-BTRTools::rjpp(rjlog = here("BayesTraits_v2","Phylo_PC_SCORES_tree_1_run_a.txt.VarRates.txt"), rjtrees = here("BayesTraits_v2","Phylo_PC_SCORES_tree_1_run_a.txt.Output.trees"), tree = treelist2[[1]]) #this is your time scaled tree that was used to input into bayestraits
tree_1_w_data <- add_rjpp_to_tree(tree_1_BTraits)
p<-ggtree(tree_1_w_data, aes(color = log(meanRate)), size=1))
