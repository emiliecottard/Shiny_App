# Packages
library(shiny)
library(shinyWidgets)
library(devtools)
library(plyr)
library(ggplot2)
library(DT)
library(gtools)

# Import data
rawdata <- read.table("https://raw.githubusercontent.com/neurogenomics/SelectiveVulnerabilityMetaAnalysis/main/Data/derived/all_data_cleaned.csv?token=AVOQPRMGLEIVUFQMWHFDWHTBPLFHO",
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
                              "SN ventral", "SN ventrolateral"),
                            c("A10 CLi", "A10 RLI", "caudate_Nucleus ", "olfactory_bulb", "SN Dorsolateral",
                              "SN Dorsomedial", "SN Lateral", "SN Medial", "SN Middle", "SN Posterolateral",
                              "SN Ventral", "SN Ventrolateral"))

dataset$group <- mapvalues(dataset$group, 
                           c("Control young", "PD without_l-dopa_reponse","PD without_l-dope_response", "LB Disorder "),
                           c("control young", "PD without_l-dopa_response", "PD without_l-dopa_response","LB_Disorder"))

dataset$stain_marker <- mapvalues(dataset$stain_marker, 
                                  c("ChAt"),
                                  c("ChAT"))

dataset$cell_type <- mapvalues(dataset$cell_type, 
                               c("CaN-pos", "large ", "noradrenergic", "purkinje_cell"),
                               c("CaN_pos", "large", "Noradrenergic", "Purkinje_Cell"))

dataset$quantification_method <- mapvalues(dataset$quantification_method,
                                           c("manual", "stereology", "Sterology"),
                                           c("Manual", "Stereology", "Stereology"))


# Create new data 
# remove duplicated rows of region / stain marker / cell type and quantification method 
summarized_data <- dataset 
summarized_data <- summarized_data[!duplicated(summarized_data[,c(21:24)]),]    





# Get the columns containing the values
dta <- apply(dataset, 1, FUN = function(x)(which(!is.na(x[c(30:41)])))) ; head(dta)

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
dta2 <- lapply(dta,function(x)(colnames(dataset[,c(30:41)])[x])) ; head(dta2)

# Merge it to the dataset
dataset2 <- dataset
dta2 <- unlist(dta2)
dataset2$dta2 <- dta2


# list of the PMIDs
list_pmid_tot <- levels(as.factor(dataset2$PMID))
list_pmid_tot <- droplevels(as.factor(list_pmid_tot))

# Function calculating the ration and the variance of the ratio
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


# Transfrom NAs or " " in Region/stain marker / cell type / quantification method into factors 
# Column region
dataset2[c(which(dataset2[,"region"]== " ")),"region"] <- 
  gsub(" ", "NA", dataset2[c(which(dataset2[,"region"]== " ")),"region"])
dataset2$region <- dataset2$region %>% na.replace("NA")
# Column stain marker 
dataset2$stain_marker <- dataset2$stain_marker %>% na.replace("NA")
# Column cell type 
dataset2$cell_type <- dataset2$cell_type %>% na.replace("NA")
# Column quantification method
dataset2$quantification_method <- dataset2$quantification_method %>% na.replace("NA")


# 
pmid <- c()
vect_ratio <- c()
vect_v_ratio <- c()
for (i in 1:length(list_pmid_tot)){
  sub_data <- dataset2 %>% filter( PMID == list_pmid_tot[i])
  if (isTRUE(length(levels(as.factor(sub_data[,21])))==1) & isTRUE(length(levels(as.factor(sub_data[,22])))==1) & 
      isTRUE(length(levels(as.factor(sub_data[,23])))==1) & isTRUE(length(levels(as.factor(sub_data[,24])))==1)){
    r <- ratio(dataset2, list_pmid_tot[i])
    pmid <- c(pmid,i )
    vect_ratio <- c(vect_ratio, r[1])
    vect_v_ratio <- c(vect_v_ratio, r[2])
  }
}

data_ratio <- data.frame("PMID" = c(list_pmid_tot[pmid]), "ratio" = vect_ratio, "v-ratio" = vect_v_ratio)
#data_ratio_merged <- merge(dataset2, data_10_ratio_tot, by = "PMID", all = TRUE)

# Plot
ggplot(data_ratio, aes(x = PMID, y = ratio)) + geom_point() +
  geom_errorbar(aes(ymin=ratio-v.ratio, ymax=ratio+v.ratio), width = .2)

