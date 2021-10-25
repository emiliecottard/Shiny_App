library(dplyr)

# Get the columns containing the values 
dta <- apply(dataset, 1, FUN = function(x)(which(!is.na(x[c(30:41)])))) ;dta

# Replace the column number by its name
dta2 <- sapply(dta, function(x)(colnames(dataset[,c(30:41)])[x]))

# Merge it to the dataset 
dataset2 <- dataset
dataset2$dta2 <- as.vector(dta2)


# Verify that there is one type of value for each PMID (and note several)
# for (i in 1:length(list_pmid)){
#   test_1 <- indiv_dataset %>% filter(PMID == list_pmid[i])
#   if (length(levels(as.factor(test_1$group))) > 2){
#     print(i)
#   }
# }


# 1 je rentre un pmid 

indiv_dataset <- dataset2 %>% filter(data_type == "individual")
indiv_dataset <- indiv_dataset[-c(which(!indiv_dataset$dta2 %in% colnames(indiv_dataset))),]    #remove rows without values
list_pmid <- levels(as.factor(indiv_dataset$PMID))
list_pmid <- droplevels(as.factor(list_pmid))


ratio <- function(data, pmid){
   
  # get column with the values 
  col <- levels(as.factor(data$dta2[which(data$PMID == pmid)]))
  
  # separate cases from controls
  crop_data <- data %>% filter(PMID == pmid)
  means <- tapply(crop_data[,col], crop_data$group, mean)
  var <- tapply(crop_data[,col], crop_data$group, sd)
 
### PROBLEME SAVOIR DANS QUEL ORDRE EST CONTROLE / PD 
  
  # calculate ratio
  ratio = means[2]/means[1]
  #var_ratio = var_pd / ((mean_control)^2) + (var_control*(mean_pd**2)) / (mean_control^4)
  
}
