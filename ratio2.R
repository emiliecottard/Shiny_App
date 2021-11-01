# Packages
library(shiny)
library(shinyWidgets)
library(devtools)
library(plyr)
library(ggplot2)
library(DT)
library(gtools)
library(Hmisc)
library(radiant.data)

# Import data
rawdata <- read.csv("https://raw.githubusercontent.com/neurogenomics/SelectiveVulnerabilityMetaAnalysis/main/Data/derived/all_data_cleaned.csv?token=AVOQPRMHMRBTKF7IF4YKB33BRFACA",
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
dataset2 <- dataset2[-which(dataset2$PMID == "6089493" & dataset2$dta2 == "number"),]  # remove an anormal line  


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

# Transform Nas of columns n (number of cases) by 1 for weighted meqn calculations
dataset2$n <- dataset2$n %>% na.replace(1)


# list of the PMIDs
list_pmid_tot <- levels(as.factor(dataset2$PMID))
list_pmid_tot <- droplevels(as.factor(list_pmid_tot))

# Function calculating the ration and the variance of the ratio
ratio <- function(crop_data, col){
  
  # add column for high level groupings 
  crop_data$group_high_level <- ifelse(grepl("ontrol", crop_data$group), "Control", "PD")
  
  # calculate mean by group 
  means <- ddply(crop_data, "group_high_level", function(x)wtd.mean(x[,col], x[,"n"]/sum(x[,"n"])))
  
  # Define controls 
  controls <- grep("ontrol",crop_data$group)
  
  # group means of cases
  mean_control <- mean(means[grep("ontrol", means[,1]),2])
  mean_pd <- mean(means[-grep("ontrol", means[,1]),2])
  
  
  # Get the different values of dispersion available
  values_sd_control = crop_data[controls,"SD"]
  values_sd_pd = crop_data[-controls,"SD"]
  values_sem_control = crop_data[controls,"SEM"]
  values_sem_pd = crop_data[-controls,"SEM"]
  values_cv_control = crop_data[controls,"CV"]
  values_cv_pd = crop_data[-controls,"CV"]
  
  # Calculate SDs
  # If there is SD values available
  if (all(!is.na(values_sd_control)) && all(!is.na(values_sd_pd))){                     
    sd_control <- 1 / sum( 1/values_sd_control) 
    sd_pd <- 1 / sum( 1/values_sd_pd) 
    print("sd")
  } 
  # If there is SEM values available
  else if (all(!is.na(values_sem_control)) && all(!is.na(values_sem_pd))) {
    sd_control <- 1 / sum( 1/(values_sem_control*c(sqrt(crop_data[controls,"n"]))))
    sd_pd <- 1 / sum( 1/(values_sem_pd*c(sqrt(crop_data[-controls,"n"]))))
    print("sem")
  } 
  # If there is CV values available
  else if (all(!is.na(values_cv_control)) && all(!is.na(values_cv_pd))){
    sd_control <- 1 / sum( 1/(values_cv_control*c(sqrt(crop_data[controls,col]))))
    sd_pd <- 1 / sum( 1/(values_cv_pd*c(sqrt(crop_data[-controls,col]))))
    print("CV")
  } 
  # If there is no SD/SEM/CV values available
  else {
    sd <- ddply(crop_data, "group_high_level", function(x)weighted.sd(x[,col], x[,"n"]/sum(x[,"n"])))
    sd_control <- max(0,mean(sd[grep("ontrol", sd[,1]),2]))   # if mean is a NA then sd control get the value 0
    sd_pd <- max(0,mean(sd[-grep("ontrol", sd[,1]),2]))
    print("calculation") 
  }
  
  # Calculate ratio 
  ratio <- mean_pd/mean_control
  sd_ratio <- max(0,sqrt(sd_pd^2 / (mean_control^2) + (sd_control^2*(mean_pd^2)) / (mean_control^4)))
  
  return(c(ratio, sd_ratio))
}


# 
vect_ratio <- vector(length=length(list_pmid_tot))
vect_sd_ratio <- vector(length=length(list_pmid_tot))
for (i in 1:length(list_pmid_tot)){
  
  # Get data filtered with PMID 
  crop_data <- dataset2[dataset2$PMID == list_pmid_tot[i],]
  
  if (isTRUE(length(levels(as.factor(crop_data[,21])))==1) & isTRUE(length(levels(as.factor(crop_data[,22])))==1) & 
      isTRUE(length(levels(as.factor(crop_data[,23])))==1) & isTRUE(length(levels(as.factor(crop_data[,24])))==1)){
    
    # Get columns with values
    col <- levels(droplevels(as.factor(crop_data$dta2)))
    
    # When measures are all in the same unit and is not a percentage 
    if ((length(col) == 1) && (!(col %in% c("percent_of_control",
                                            "percent_of_total", "percent_of_loss")))){
      
      print(c("general", i))
      r <- ratio(crop_data, col)
    } 
    # When groups are not meausred in the same unit 
    else if (length(col) >1){
      print(c("col>1", i))
      crop_data <- dataset2 %>% filter(PMID == list_pmid_tot[i])
      col <- levels(droplevels(as.factor(crop_data$dta2)))
      crop_data1 <- crop_data[which(!is.na(crop_data[,col[1]])),]
      crop_data2 <- crop_data[which(!is.na(crop_data[,col[2]])),]
      
      # ratio by group
      r1 <- ratio(crop_data1,col[1])
      r2 <- ratio(crop_data2,col[2])
      
      #global ratio
      r <- c(mean(r1[1],r2[1]), mean(r1[2], r2[2]))
      
    } 
    # When measures are percentages 
    else if (col %in% c("percent_of_control",
                        "percent_of_total", "percent_of_loss")){
      print(c("%", i))
      means <- wtd.mean(crop_data[,col], crop_data[,"n"]/sum(crop_data[,"n"]))
      sd <- weighted.sd(crop_data[,col], crop_data[,"n"]/sum(crop_data[,"n"]))
      r <- c(means, sd)
    }
    
    vect_ratio[i] <- r[1]
    vect_sd_ratio[i] <- r[2]
  }
}

data_ratio <- data.frame("PMID" = list_pmid_tot, "ratio" = vect_ratio, "v-ratio" = vect_sd_ratio)



# Plot
ggplot(data_ratio, aes(x = PMID, y = ratio)) + geom_point(size = 1) +
  geom_errorbar(aes(ymin=ratio-v.ratio, ymax=ratio+v.ratio), width = .2) + 
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.line = element_line(size = 0.5, colour = "darkgrey"),
        axis.text.x = element_text(size = 7, angle = 90, hjust = 1))
