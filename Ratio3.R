# Packages
library(devtools)
library(plyr)
library(ggplot2)
library(gtools)
library(Hmisc)
library(radiant.data)
library(tibble)

# Import data
rawdata <- read.csv("https://raw.githubusercontent.com/neurogenomics/SelectiveVulnerabilityMetaAnalysis/main/Data/derived/all_data_cleaned.csv?token=AVOQPRO6BWIGE62LMPQFNTTBSN3GA",
                      header = TRUE,sep = ",")

# Remove empty columns 
dataset <- Filter(function(x)!all(is.na(x) | x == ""), rawdata)
dataset <- subset(dataset, select = -X1)


# Merging groups and sub groups 
dataset$region <- c(paste(dataset$region, dataset$sub.region, sep = " "))  ; head(dataset)
dataset$group <- c(paste(dataset$group, dataset$sub.group, sep = " ")) ; head(dataset)
dataset <- subset(dataset, select = -c(sub.region,sub.group)) ; colnames(dataset)


# Removing unnecessary NAs in  merged columns
dataset$group <- gsub("NA", "", dataset$group) ; head(dataset$group)
dataset$region <- gsub("NA", "", dataset$region) ; head(dataset$region)

# Harmonizing terms 
dataset$region <- mapvalues(dataset$region,
                            c("A10 Cli", "A10 Rli", "caudate_nucleus ", "olfactory_bulb ", "SN dorsolateral",
                              "SN dorsomedial", "SN lateral", "SN medial", "SN middle", "SN posterolateral",
                              "SN ventral", "SN ventrolateral", "SN medial_2", "SN medial_3"),
                            c("A10 CLi", "A10 RLi", "caudate_Nucleus ", "olfactory_bulb", "SN Dorsolateral",
                              "SN Dorsomedial", "SN Lateral", "SN Medial", "SN Middle", "SN Posterolateral",
                               "SN Ventral", "SN Ventrolateral", "SN Medial", "SN Medial"))

dataset$group <- mapvalues(dataset$group, 
                           c("Control young", "PD without_l-dopa_reponse","PD without_l-dope_response", "LB Disorder "),
                           c("control young", "PD without_l-dopa_response", "PD without_l-dopa_response","LB_Disorder "))

dataset$stain_marker <- mapvalues(dataset$stain_marker, 
                                  c("ChAt", "ACh"),
                                  c("ChAT", "ACH"))

dataset$cell_type <- mapvalues(dataset$cell_type, 
                               c("CaN-pos", "noradrenergic", "purkinje_cell", "NADPHD_pos"),
                               c("CaN_pos", "Noradrenergic", "Purkinje_Cell", "NADPH-d_pos"))
dataset$cell_type[which(grepl("golgi_type",dataset$cell_type))] <- "golgi_type"

dataset$quantification_method <- mapvalues(dataset$quantification_method,
                                           c("manual", "stereology", "Sterology"),
                                           c("Manual", "Stereology", "Stereology"))

# Duplicate a control
rowid <- which(dataset$PMID == 17894336 & dataset$group == "Control " &
                 dataset$region == "putamen dorsolateral" & dataset$cell_type == "CaN_pos")
dataset <- add_row(dataset, dataset[rowid,], .before = rowid)
dataset$region[rowid] <- dataset$region[rowid+2]
dataset$region[rowid+1] <- dataset$region[rowid+3]


# Remove empty raws (percent of total == FALSE )
dataset <- dataset[-which(!is.na(dataset$percent_of_total)),]

# Remove empty column percent of total
dataset <- Filter(function(x)!all(is.na(x)), dataset)

# Remove specific publications due to lack of information or because values are NA
dataset <- dataset[-which(dataset$PMID == 8809817),]
dataset <- dataset[-which(dataset$PMID == 10459912 & dataset$region == "central_grey_substance " &
                            dataset$stain_marker == "ACH" & dataset$cell_type == "Dopaminergic_melanised"),]
dataset <-dataset[-which(dataset$PMID == 2570794 & is.na(dataset$percentage_loss)),]

# Get the columns containing the values
dta <- apply(dataset, 1, FUN = function(x)(which(!is.na(x[c("percent_of_control", "mean_number", "number",
                                                            "number_x10.3", "number_x10.6", "density",
                                                            "number_per_mm.2", "number_per_mm.3", 
                                                            "percentage_loss", "AU","per_ganglion")])))) ; head(dta)

# list of empty cells 
empty_cells <- c()
for (i in 1:length(dta)){
  if (length(dta[[i]])== 0)
  {empty_cells <- c(empty_cells,i)}
}


# Remove lines with no measure from original dataset
dataset <- dataset[-c(empty_cells),]
dta <- dta[-c(empty_cells)]

# Replace the column number by its name
dta <- lapply(dta,function(x)(colnames(dataset[,c("percent_of_control", "mean_number", "number",
                                                  "number_x10.3", "number_x10.6", "density",
                                                  "number_per_mm.2", "number_per_mm.3", 
                                                  "percentage_loss", "AU","per_ganglion")])[x])) ; head(dta)

# Merge it to the dataset
dataset2 <- dataset
dta <- unlist(dta)
dataset2$dta <- dta
dataset2 <- dataset2[-which(dataset2$PMID == "6089493" & dataset2$dta == "number"),]  # remove an anormal line  


# Transfrom NAs or " " in Region/stain marker / cell type / quantification method into factors 
# Column region
dataset2[c(which(dataset2[,"region"]== " ")),"region"] <- 
  gsub(" ", "NA", dataset2[c(which(dataset2[,"region"]== " ")),"region"])
dataset2[,c("region","stain_marker","cell_type","quantification_method")] <- na.replace(dataset2[,c("region","stain_marker","cell_type","quantification_method")],"NA")

# Transform Nas of columns n (number of cases) by 1 for weighted meqn calculations
dataset2$n <- na.replace(dataset2$n,1)





### Calculate the means and sds per groups with same variables 

# Create new data 
dataset2$group_high_level <- apply(dataset2,1,function(x)ifelse(grepl("ontrol", x["group"]) |
                                                                  grepl("NPD", x["group"]) | 
                                                                  grepl("NMD", x["group"]),
                                                                "Control",
                                                                ifelse(grepl("PD", x["group"]),
                                                                       "PD",
                                                                       ifelse(grepl("AD", x["group"]),
                                                                              "AD",
                                                                              x["group"]))))

group_high_level2 <- vector(length= dim(dataset2)[1])
for (i in 1:length(group_high_level2)){
  a <- levels(as.factor(dataset2$group_high_level[paste(dataset2$PMID, dataset2$region,
                                                        dataset2$stain_marker, dataset2$cell_type, 
                                                        dataset2$quantification_method, dataset2$dta) %in%
                                                    paste(dataset2$PMID[i],
                                                          dataset2$region[i], dataset2$stain_marker[i],
                                                          dataset2$cell_type[i], dataset2$quantification_method[i],
                                                          dataset2$dta[i])]))
  group_high_level2[i] <- ifelse(length(a)==1, a, ifelse(length(a)==2,a[-grepl("Control", a)], ifelse(dataset2$group_high_level[i] != "Control", dataset2$group_high_level[i], NA )))
  
}

dataset2 <- cbind(dataset2, group_high_level2)
lines <- which(dataset2$group_high_level == "Control" & is.na(dataset2$group_high_level2))   # Lines to duplicate
ref_dataset2 <- dataset2    # reference dataset for the lines index

for (i in 1:length(lines)){
  line<- ref_dataset2[lines[i],]
  rowid <- which(dataset2$PMID == line$PMID & dataset2$group == line$group & 
                   dataset2$region == line$region & dataset2$cell_type == line$cell_type &
                   dataset2$quantification_method == line$quantification_method & is.na(dataset2$group_high_level2))
  dataset2 <- add_row(dataset2,dataset2[rowid,], .before = rowid)
  factor_level <- levels(as.factor(dataset2$group_high_level[paste(dataset2$PMID, dataset2$region,
                                      dataset2$stain_marker, dataset2$cell_type, 
                                      dataset2$quantification_method, dataset2$dta) %in%
                                  paste(dataset2$PMID[rowid],
                                        dataset2$region[rowid], dataset2$stain_marker[rowid],
                                        dataset2$cell_type[rowid], dataset2$quantification_method[rowid],
                                        dataset2$dta[rowid])]))
  factor_level <- factor_level[-grepl("Control", factor_level)]
  print(i)
  print(factor_level)
  print(length(rowid))
  if(length(rowid)==1){
    dataset2$group_high_level2[rowid] <- factor_level[1]
    dataset2$group_high_level2[rowid+1] <- factor_level[2]
    print(lines)
  } else {
    dataset2$group_high_level2[rowid[1]:(rowid[1]-1 +length(rowid))] <- factor_level[1]
    dataset2$group_high_level2[(rowid[1]+length(rowid)):(rowid[1]-1+2*length(rowid))] <- factor_level[2]
    lines<- lines[-c((i+1):(min(i+length(rowid)-1, length(lines))))]
    print(lines)
  }
}


# Remove duplicated rows of region / stain marker / cell type and quantification method 
synth_data <- dataset2[!duplicated(dataset2[,c("PMID","region","stain_marker","cell_type",
                                               "quantification_method","dta","group_high_level","group_high_level2" )]),] 


# Function calculates mean and sd for a group 
mean_sd <- function(data, col){
  
  # Calculate weighted mean
  means <- wtd.mean(data[,col], data[,"n"]/sum(data[,"n"]))
  
  # Get the different values of dispersion available
  values_sd = data[,"SD"]
  values_sem = data[,"SEM"]
  values_cv = data[,"CV"]
  
  # Calculate SDs
  # If there is SD values available
  if (all(!is.na(values_sd))){                     
    sd <- 1 / sum( 1/values_sd) 
  } 
  # If there is SEM values available
  else if (all(!is.na(values_sem))) {
    sd <- 1 / sum( 1/(values_sem*c(sqrt(data[,"n"]))))
  } 
  # If there is CV values available
  else if (all(!is.na(values_cv))){
    sd <- 1 / sum( 1/(values_cv*c(sqrt(data[,col]))))
  } 
  # If there is no SD/SEM/CV values available
  else {
    sd <- weighted.sd(data[,col], data[,"n"]/sum(data[,"n"]))
  }
  return (c(means, sd))
}


# Initialize vectors for means and sds
vect_mean <-  vector(length = dim(synth_data)[1])
vect_sd <- vector(length = dim(synth_data)[1])

for (i in 1:dim(synth_data)[1]){
  
  # Get all the rows that have the same variables 
  crop_data <- dataset2[paste(dataset2$PMID, dataset2$group_high_level, dataset2$region,
                              dataset2$stain_marker, dataset2$cell_type, 
                              dataset2$quantification_method, dataset2$dta,
                              dataset2$group_high_level2) %in%
                          paste(synth_data$PMID[i],synth_data$group_high_level[i],
                                synth_data$region[i], synth_data$stain_marker[i],
                                synth_data$cell_type[i], synth_data$quantification_method[i],
                                synth_data$dta[i], synth_data$group_high_level2[i]),]
  
  # Get the columns with the values
  col <- levels(droplevels(as.factor(crop_data$dta)))
  
  # Calculate mean and sd 
  result <- mean_sd(crop_data, col)
  vect_mean[i] <- result[1]
  vect_sd[i] <- result[2]
}

synth_data <- cbind(synth_data, vect_mean, vect_sd)



### Calculate the ratio for PDs and Controls with same variables 

ratio_data <- synth_data[!duplicated(synth_data[,c("PMID","region","stain_marker","cell_type",
                                                   "quantification_method","dta", "group_high_level2")]),]

vect_ratio <- vector(length=dim(ratio_data)[1])
vect_sd_ratio <- vector(length=dim(ratio_data)[1])

for (i in 1:dim(ratio_data)[1]){
  crop_data <-synth_data[paste(synth_data$PMID, synth_data$region,
                               synth_data$stain_marker, synth_data$cell_type, 
                               synth_data$quantification_method, synth_data$dta,
                               synth_data$group_high_level2) %in%
                           paste(ratio_data$PMID[i],
                                 ratio_data$region[i], ratio_data$stain_marker[i],
                                 ratio_data$cell_type[i], ratio_data$quantification_method[i],
                                 ratio_data$dta[i], ratio_data$group_high_level2[i]),]
  
  # Get the columns with the values
  col <- levels(droplevels(as.factor(crop_data$dta)))
  
  # For rows where values are a percentage 
  if (col %in% c("percent_of_control","percentage_loss")){
    ratio <- wtd.mean(crop_data[,"vect_mean"], 
                      crop_data[,"n"]/sum(crop_data[,"n"]))/100  
    
    sd_ratio <- weighted.sd(crop_data[,"vect_sd"], 
                            crop_data[,"n"]/sum(crop_data[,"n"]))/100 ### right ??
  }
  # For rows where values are not a percentage 
  else {
    mean_control <- crop_data[grepl("Control", crop_data$group_high_level), "vect_mean"]
    mean_pd <- crop_data[-grepl("Control", crop_data$group_high_level), "vect_mean"]
    sd_control <- crop_data[grepl("Control", crop_data$group_high_level), "vect_sd"]
    sd_pd <- crop_data[-grepl("Control", crop_data$group_high_level), "vect_sd"]
    
    # Ratio 
    ratio <- mean_pd / mean_control

    # Sd of ratio
    var_ratio <- (mean_pd^2)/(mean_control^2)*(((sd_pd^2) / (mean_pd^2))
                                               + ((sd_control^2) / (mean_control^2)))
    sd_ratio <- max(0, sqrt(var_ratio))
  }
  vect_ratio[i] <- max(0,ratio)
  vect_sd_ratio[i] <- max(0,sd_ratio)
}

ratio_data <- cbind(ratio_data, vect_ratio, vect_sd_ratio)

# Regroup PMIDS with same variables but values initially in different units and
# do the mean of the ratio and the sd 
ratio_data0 <- ddply(ratio_data, 
                     c("PMID","region","stain_marker","cell_type",
                                   "quantification_method","group_high_level2"), 
                     summarize, 
                     mean(vect_ratio),
                     mean(vect_sd_ratio))   
ratio_data <- ratio_data[!duplicated(ratio_data[,c("PMID","region","stain_marker","cell_type",
                                                   "quantification_method", "group_high_level2")]),]
ratio_data[,c("vect_ratio","vect_sd_ratio")] <- ratio_data0[,c("mean(vect_ratio)",
                                                               "mean(vect_sd_ratio)")]



# Plot
ggplot(ratio_data, aes(x = PMID, y = vect_ratio, color = group_high_level2)) + geom_point(size = 1) +
  geom_errorbar(aes(ymin=ratio_data$vect_ratio-ratio_data$vect_sd_ratio,
                    ymax=ratio_data$vect_ratio+ratio_data$vect_sd_ratio), width = .2) + 
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.line = element_line(size = 0.5, colour = "darkgrey"),
        axis.text.x = element_text(size = 7, angle = 90, hjust = 1))

