library(dplyr)

# Get the columns containing the values
dta <- apply(dataset, 1, FUN = function(x)(which(!is.na(x[c(30:41)])))) ;dta

# Replace the column number by its name
dta2 <- lapply(dta,function(x)(colnames(dataset[,c(30:41)])[x])) ; dta2

# Merge it to the dataset
dataset2 <- dataset
dta2 <- unlist(dta2)
dataset2$dta2 <- dta2


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
indiv_dataset <- droplevels(indiv_dataset)
list_pmid <- levels(indiv_dataset$PMID)
list_pmid <- droplevels(as.factor(list_pmid))


ratio <- function(data, pmid){
  
  # get column with the values
  col <- levels(droplevels(as.factor(data$dta2[which(data$PMID == pmid)])))
  
  # separate cases from controls
  crop_data <- data %>% filter(PMID == pmid)
  means <- tapply(crop_data[,col], crop_data$group, mean)
  vars <- tapply(crop_data[,col], crop_data$group, sd)
  
  # group means of cases
  mean_control <- means[1]
  mean_pd <- mean(means[-1])
  var_control <- vars[1]
  var_pd <- sum(vars[-1])/length(vars[-1])
  
  # calculate ratio
  ratio = mean_pd/mean_control
  print(ratio)
  var_ratio = var_pd / (mean_control^2) + (var_control*(mean_pd**2)) / (mean_control^4)
  
  return(c(ratio, var_ratio))
}



r1 <- ratio(dataset, list_pmid[2])


# For 10 first PMIDs
data_10 <- indiv_dataset %>% filter(PMID == list_pmid[1] | PMID == list_pmid[2] | PMID == list_pmid[3] | PMID == list_pmid[4] | PMID == list_pmid[5] | PMID == list_pmid[6] | PMID == list_pmid[7] | PMID == list_pmid[8] | PMID == list_pmid[9] | PMID == list_pmid[10])
vect_ratio <- c()
vect_v_ratio <- c()
for (i in 1:10){
  print(i)
  r <- ratio(indiv_dataset, list_pmid[i])
  vect_ratio <- c(vect_ratio, r[1])
  vect_v_ratio <- c(vect_v_ratio, r[2])
}

data_10_ratio <- data.frame("PMID" = levels(droplevels(data_10$PMID)), "ratio" = vect_ratio, "v-ratio" = vect_v_ratio)
data_10_merge <- merge(data_10, data_10_ratio, by = "PMID", all = TRUE)

# Plot
ggplot(data_10_merge, aes(x = PMID, y = ratio)) + geom_point() +
  geom_errorbar(aes(ymin=ratio-v.ratio, ymax=ratio+v.ratio), width = .2)





# For 10 first PMIDs with individuals and groups
list_pmid_tot <- levels(dataset2$PMID)
list_pmid_tot <- droplevels(as.factor(list_pmid_tot))


ratio <- function(data, pmid){
  
  # get column with the values
  col <- levels(droplevels(as.factor(data$dta2[which(data$PMID == pmid)])))
  
  # separate cases from controls
  crop_data <- data %>% filter(PMID == pmid)
  means <- tapply(crop_data[,col], crop_data$group, mean)
  vars <- tapply(crop_data[,col], crop_data$group, sd)
  
  # group means of cases
  mean_control <- means[1]
  mean_pd <- mean(means[-1])
  var_control <- vars[1]
  var_pd <- sum(vars[-1])/length(vars[-1])
  
  # calculate ratio
  ratio = mean_pd/mean_control
  print(ratio)
  var_ratio = var_pd / (mean_control^2) + (var_control*(mean_pd**2)) / (mean_control^4)
  
  return(c(ratio, var_ratio))
}




data_tot_10 <- dataset2 %>% filter(PMID == list_pmid_tot[1] | PMID == list_pmid_tot[2] | PMID == list_pmid_tot[3])
vect_ratio <- c()
vect_v_ratio <- c()
for (i in 1:length(list_pmid_tot)){
  print(i)
  r <- ratio(dataset2, levels(as.factor(dataset2$PMID))[i])
  print(c(r[1],r[2]))
  vect_ratio <- c(vect_ratio, r[1])
  vect_v_ratio <- c(vect_v_ratio, r[2])
}

data_10_ratio_tot <- data.frame("PMID" = levels(droplevels(dataset2$PMID))[1:9], "ratio" = vect_ratio, "v-ratio" = vect_v_ratio)
data_10_merge_tot <- merge(dataset2, data_10_ratio_tot, by = "PMID", all = TRUE)

# Plot
ggplot(data_10_merge_tot, aes(x = PMID, y = ratio)) + geom_point() +
  geom_errorbar(aes(ymin=ratio-v.ratio, ymax=ratio+v.ratio), width = .2)








crop_data <- indiv_dataset %>% filter(PMID == list_pmid[21])
col <- levels(droplevels(as.factor(indiv_dataset$dta2[which(indiv_dataset$PMID == list_pmid[20])])))
means <- tapply(crop_data[,col], crop_data$group, mean)
var <- tapply(crop_data[,col], crop_data$group, sd)


crop_data <- dataset %>% filter(PMID == levels(as.factor(dataset$PMID))[1])
col <- levels(droplevels(as.factor(indiv_dataset$dta2[which(dataset$PMID == levels(as.factor(dataset$PMID))[1])])))
means <- tapply(crop_data[,col], crop_data$group, mean)
var <- tapply(crop_data[,col], crop_data$group, sd)