# Packages
library(devtools)
library(plyr)
library(ggplot2)
library(gtools)
library(Hmisc)
library(radiant.data)
library(tibble)

# Import data
rawdata <- read.csv("https://raw.githubusercontent.com/neurogenomics/SelectiveVulnerabilityMetaAnalysis/main/Data/derived/all_data_cleaned.csv?token=AVOQPRNUVJAIP5C7S4YXSV3BROMMI",
                      header = TRUE,sep = ",")

# Remove empty columns 
dataset <- Filter(function(x)!all(is.na(x) | x == ""), rawdata)
dataset <- dataset[,-1]


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
                           c("control young", "PD without_l-dopa_response", "PD without_l-dopa_response","LB_Disorder"))

dataset$stain_marker <- mapvalues(dataset$stain_marker, 
                                  c("ChAt", "ACh"),
                                  c("ChAT", "ACH"))

dataset$cell_type <- mapvalues(dataset$cell_type, 
                               c("CaN-pos", "noradrenergic", "purkinje_cell"),
                               c("CaN_pos", "Noradrenergic", "Purkinje_Cell"))
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
dta <- apply(dataset, 1, FUN = function(x)(which(!is.na(x[c(30:40)])))) ; head(dta)

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
dta2 <- lapply(dta,function(x)(colnames(dataset[,c(30:40)])[x])) ; head(dta2)

# Merge it to the dataset
dataset2 <- dataset
dta2 <- unlist(dta2)
dataset2$dta2 <- dta2
dataset2 <- dataset2[-which(dataset2$PMID == "6089493" & dataset2$dta2 == "number"),]  # remove an anormal line  


# Transfrom NAs or " " in Region/stain marker / cell type / quantification method into factors 
# Column region
dataset2[c(which(dataset2[,"region"]== " ")),"region"] <- 
  gsub(" ", "NA", dataset2[c(which(dataset2[,"region"]== " ")),"region"])
dataset2[,c(21:24)] <- na.replace(dataset2[,c(21:24)],"NA")

# Transform Nas of columns n (number of cases) by 1 for weighted meqn calculations
dataset2$n <- na.replace(dataset2$n,1)



### Calculate the means and sds per groups with same variables 

# Create new data 
dataset2$group_high_level <- ifelse(grepl("ontrol", dataset2$group) |
                                      grepl("NPD", dataset2$group) | 
                                      grepl("NMD", dataset2$group),
                                      "Control", "PD") 
dataset3 <- cbind("line"= 1:dim(dataset2)[1], dataset2)

# Remove duplicated rows of region / stain marker / cell type and quantification method 
synth_data <- dataset2[!duplicated(dataset2[,c(1,21:24,47:48)]),] 


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
  crop_data <- dataset3[paste(dataset3$PMID, dataset3$group_high_level, dataset3$region,
                              dataset3$stain_marker, dataset3$cell_type, 
                              dataset3$quantification_method, dataset3$dta2) %in%
                          paste(synth_data$PMID[i],synth_data$group_high_level[i],
                                synth_data$region[i], synth_data$stain_marker[i],
                                synth_data$cell_type[i], synth_data$quantification_method[i],
                                synth_data$dta2[i]),]
  
  # Get the columns with the values
  col <- levels(droplevels(as.factor(crop_data$dta2)))
  
  # Calculate mean and sd 
  result <- mean_sd(crop_data, col)
  vect_mean[i] <- result[1]
  vect_sd[i] <- result[2]
}

synth_data <- cbind(synth_data, vect_mean, vect_sd)



### Calculate the ratio for PDs and Controls with same variables 

ratio_data <- synth_data[!duplicated(synth_data[,c(1,21:24,47)]),]

vect_ratio <- vector(length=dim(ratio_data)[1])
vect_sd_ratio <- vector(length=dim(ratio_data)[1])

for (i in 1:dim(ratio_data)[1]){
  crop_data <-synth_data[paste(synth_data$PMID, synth_data$region,
                               synth_data$stain_marker, synth_data$cell_type, 
                               synth_data$quantification_method, synth_data$dta2) %in%
                           paste(ratio_data$PMID[i],
                                 ratio_data$region[i], ratio_data$stain_marker[i],
                                 ratio_data$cell_type[i], ratio_data$quantification_method[i],
                                 ratio_data$dta2[i]),]
  
  # Get the columns with the values
  col <- levels(droplevels(as.factor(crop_data$dta2)))
  
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
    mean_pd <- crop_data[grepl("PD", crop_data$group_high_level), "vect_mean"]
    sd_control <- crop_data[grepl("Control", crop_data$group_high_level), "vect_sd"]
    sd_pd <- crop_data[grepl("PD", crop_data$group_high_level), "vect_sd"]
    
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
ratio_data0 <- ddply(ratio_data, c(1,21:24), summarize, mean(vect_ratio),mean(vect_sd_ratio))   
ratio_data <- ratio_data[!duplicated(ratio_data[,c(1,21:24)]),]
ratio_data[,c("vect_ratio","vect_sd_ratio")] <- ratio_data0[,c("mean(vect_ratio)","mean(vect_sd_ratio)")]

# Remove odd values
ratio_data <- ratio_data[-which(vect_ratio >=1 | vect_sd_ratio >50),]


# Plot
ggplot(ratio_data[,c(1,51)], aes(x = PMID, y = vect_ratio)) + geom_point(size = 1) +
  geom_errorbar(aes(ymin=ratio_data$vect_ratio-ratio_data$vect_sd_ratio,
                    ymax=ratio_data$vect_ratio+ratio_data$vect_sd_ratio), width = .2) + 
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.line = element_line(size = 0.5, colour = "darkgrey"),
        axis.text.x = element_text(size = 7, angle = 90, hjust = 1))
colnames(ratio_data)
