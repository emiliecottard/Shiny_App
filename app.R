# Packages
library(shiny)
library(shinyWidgets)
library(DT)

  
#functions of the shinyWidget package 
devtools::source_url("https://raw.githubusercontent.com/ismirsehregal/shinyWidgets/master/R/module-selectizeGroup.R")
devtools::source_url("https://raw.githubusercontent.com/ismirsehregal/shinyWidgets/master/R/module-utils.R")


#### ARRANGE DATASET ####




##### Import data #####
rawdata <- read.csv("https://raw.githubusercontent.com/neurogenomics/SelectiveVulnerabilityMetaAnalysis/main/www/derived/all_data_cleaned.csv?token=AVOQPROXCPJOOJM7LMR7OWDBS6C6O",
                    header = TRUE,sep = ",")

##### Import glossary #####
glossary <- read.csv("https://raw.githubusercontent.com/neurogenomics/SelectiveVulnerabilityMetaAnalysis/main/www/glossary.md?token=AVOQPRKWHTDY6HGKVEWETYTBT5U34",
                      header = TRUE,sep = "|")

##### Re-arrange glossary ##### 
glossary <- glossary[-1,-c(1,5)]
glossary$Term <- gsub("`", "", glossary$Term)


# Remove empty columns 
dataset <- Filter(function(x)!all(is.na(x) | x == ""), rawdata)
dataset <- subset(dataset, select = -X1)


##### Remove whitespace from data #####
for (x in c("region","sub.region",
            "group","stain_marker",
            "cell_type","quantification_method")){ 
  dataset[[x]] <- trimws(dataset[[x]])
}
remove(x)

# Merging groups and sub groups 
dataset$group <- c(paste(dataset$group, dataset$sub.group, sep = " ")) ; head(dataset)
dataset <- subset(dataset, select = -c(sub.group)) ; colnames(dataset)


# Removing unnecessary NAs in  merged columns
dataset$group <- gsub("NA", "", dataset$group) ; head(dataset$group)


##### Harmonizing terms #####
dataset$region <- plyr::mapvalues(dataset$region,
                            c("caudate_nucleus", "Hypothal"),
                            c("caudate_Nucleus", "hypothal"))

dataset$sub.region <- plyr::mapvalues(dataset$sub.region,
                            c("Caudal", "Cli", "dorsolateral", "dorsomedial", "intermediate",
                              "lateral", "medial", "middle", "posterolateral", "Rli", "rostral",
                              "ventral", "ventrolateral", "ventromedial", "medial_2", "medial_3"),
                            c("caudal", "CLi", "Dorsolateral", "Dorsomedial", "Intermediate",
                              "Lateral", "Medial", "Middle", "Posterolateral", "RLi", "Rostral",
                              "Ventral", "Ventrolateral", "Ventromedial", "Medial", "Medial"))


dataset$group <- plyr::mapvalues(dataset$group, 
                           c("Control young", "PD without_l-dopa_reponse"),
                           c("control young", "PD without_l-dopa_response"))

dataset$stain_marker <- plyr::mapvalues(dataset$stain_marker, 
                                  c("ChAt", "ACh"),
                                  c("ChAT", "ACH"))

dataset$cell_type <- plyr::mapvalues(dataset$cell_type, 
                               c("CaN-pos", "noradrenergic", "purkinje_cell", "NADPHD_pos"),
                               c("CaN_pos", "Noradrenergic", "Purkinje_Cell", "NADPH-d_pos"))
dataset$cell_type[which(grepl("golgi_type",dataset$cell_type))] <- "golgi_type"

dataset$quantification_method <- plyr::mapvalues(dataset$quantification_method,
                                           c("manual", "stereology"),
                                           c("Manual", "Stereology"))

#### Duplicate controls for specific publications ####
# For some publications some factors between controls and cases do
# not match correctly so have to remake them 
# Identify the row to duplicate
rowid <- which(dataset$PMID == 17894336 &
                 dataset$group == "Control " &
                 dataset$cell_type == "CaN_pos")
# Duplicate the row
dataset <- tibble::add_row(dataset, dataset[rowid,], .before = rowid)
# Original row gets the first sub.region factors
dataset$subregion[rowid] <- levels(as.factor(
  dataset$sub.region[dataset$PMID == 17894336 &
                       dataset$cell_type == "CaN_pos" &
                       dataset$group!="Control "]))[1]
# Duplicated row gets the second sub.region factors
dataset$region[rowid+1] <- levels(as.factor(
  dataset$sub.region[dataset$PMID == 17894336 &
                       dataset$cell_type == "CaN_pos" &
                       dataset$group!="Control "]))[2]

rowid2 <- which(dataset$PMID == 9039456 & dataset$stain_marker == "HLA-DR" &
                 is.na(dataset$cell_type))
dataset <- tibble::add_row(dataset, dataset[rowid2,], .before = rowid2)
dataset$cell_type[rowid2] <- levels(as.factor(dataset$cell_type[dataset$PMID == 9039456 & dataset$group!="Control"]))[1]
dataset$cell_type[rowid2+1] <- levels(as.factor(dataset$cell_type[dataset$PMID == 9039456 & dataset$group !="Control"]))[2]

rowid3 <- which(dataset$PMID == 9039456 & dataset$stain_marker == "Ki-M1P" &
                  is.na(dataset$cell_type))
dataset <- tibble::add_row(dataset, dataset[rowid3,], .before = rowid3)
dataset$cell_type[rowid3] <- levels(as.factor(dataset$cell_type[dataset$PMID == 9039456 & dataset$group != "Control"]))[1]
dataset$cell_type[rowid3+1] <- levels(as.factor(dataset$cell_type[dataset$PMID == 9039456 & dataset$group != "Control"]))[2]


# Remove empty raws (percent of total == FALSE )
dataset <- dataset[-which(!is.na(dataset$percent_of_total)),]

# Remove empty column percent of total
dataset <- Filter(function(x)!all(is.na(x)), dataset)

# Remove specific publications due to lack of information or because values are NA
dataset <- dataset[-which(dataset$PMID == 8809817),]
dataset <- dataset[-which(dataset$PMID == 10459912 & dataset$region == "central_grey_substance" &
                            dataset$stain_marker == "ACH" & dataset$cell_type == "Dopaminergic_melanised"),]
dataset <-dataset[-which(dataset$PMID == 2570794 & is.na(dataset$percentage_loss)),]
dataset <- dataset[-which(dataset$PMID == 2575832 & dataset$group == "Control "),]
dataset <- dataset[-which(dataset$PMID == 6089493 & dataset$dta == "number"),]

###### Get the columns containing the values #####
dta <- apply(dataset, 1, FUN = function(x)(which(!is.na(x[c("percent_of_control", "mean_number", "number",
                                                            "number_x10.3", "number_x10.6", "density",
                                                            "number_per_mm.2", "number_per_mm.3", 
                                                            "percentage_loss", "AU","per_ganglion")]))))

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
                                                  "percentage_loss", "AU","per_ganglion")])[x]))

# Merge it to the dataset
dataset2 <- dataset
dta <- unlist(dta)
dataset2$dta <- dta


# Transfrom NAs or " " in Region/stain marker / cell type / quantification method into factors 
# Column region
dataset2[,c("region", "sub.region","stain_marker","cell_type","quantification_method")] <- gtools::na.replace(dataset2[,c("region", "sub.region","stain_marker","cell_type","quantification_method")],"NA")

# Transform Nas of columns n (number of cases) by 1 for weighted meqn calculations
dataset2$n <- gtools::na.replace(dataset2$n,1)


##### New group level #####
dataset2$group_high_level <- apply(dataset2,1,function(x)ifelse(grepl("ontrol", x["group"]) |
                                                                  grepl("NPD", x["group"]) | 
                                                                  grepl("NMD", x["group"]),
                                                                "Control",
                                                                ifelse(grepl("PD", x["group"]),
                                                                       "PD",
                                                                       ifelse(grepl("AD", x["group"]),
                                                                              "AD",
                                                                              x["group"]))))

#first let's identify all PMID's with a control
pmids <- unique(dataset2[dataset2$group_high_level=="Control",]$PMID)
#now get disease names for these cases
diseases <- 
  dataset2[dataset2$PMID %in% pmids & dataset2$group_high_level!="Control",]
#only keep PMID and group_high_level columns
diseases <- diseases[,c("PMID","group_high_level")]
#These are the number of rows we need to create multiple control entries
#set group_high_level to group_high_level2
names(diseases)[names(diseases) == "group_high_level"] <- "group_high_level2"
#join on the same control data for each (left join), note only for PMIDs w/ controls
diseases <-
  merge(x = diseases, y = dataset2[dataset2$PMID %in% pmids & 
                                     dataset2$group_high_level=="Control",], 
        by = "PMID", all.x = TRUE)
#now just create the dataset without the multi disease cases' controls and add
#the new column
dataset2 <- 
  dataset2[!(dataset2$PMID %in% pmids & 
               dataset2$group_high_level=="Control"),]
#add new column, set name to group_high_level value
dataset2$group_high_level2 <- dataset2$group_high_level
#reorder columns to match other df and union
dataset2 <- rbind(dataset2,diseases[,names(dataset2)])




#### FILTER DATASET ####




# Remove duplicated rows of region / stain marker / cell type and quantification method 
synth_data <- dataset2[!duplicated(dataset2[,c("PMID","region","sub.region","stain_marker","cell_type",
                                               "quantification_method","dta","group_high_level","group_high_level2")]),] 


##### Function calculates mean and sd for a group #####
mean_sd <- function(data, col){
  
  # Calculate weighted mean
  means <- Hmisc::wtd.mean(data[,col], data[,"n"]/sum(data[,"n"]))
  
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
    sd <- radiant.data::weighted.sd(data[,col], data[,"n"]/sum(data[,"n"]))
  }
  return (c(means, sd))
}


# Initialize vectors for means and sds
vect_mean <-  vector(length = dim(synth_data)[1])
vect_sd <- vector(length = dim(synth_data)[1])

for (i in 1:dim(synth_data)[1]){
  
  # Get all the rows that have the same variables 
  crop_data <- dataset2[paste(dataset2$PMID, dataset2$group_high_level, dataset2$region,
                              dataset2$sub.region, dataset2$stain_marker, dataset2$cell_type, 
                              dataset2$quantification_method, dataset2$dta,
                              dataset2$group_high_level2) %in%
                          paste(synth_data$PMID[i],synth_data$group_high_level[i], synth_data$region[i],
                                synth_data$sub.region[i], synth_data$stain_marker[i], 
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



##### Calculate the ratio for PDs and Controls with same variables #####

ratio_data <- synth_data[!duplicated(synth_data[,c("PMID","region","sub.region", "stain_marker","cell_type",
                                                   "quantification_method","dta","group_high_level2")]),]

vect_ratio <- vector(length=dim(ratio_data)[1])
vect_sd_ratio <- vector(length=dim(ratio_data)[1])

for (i in 1:dim(ratio_data)[1]){
  crop_data <-synth_data[paste(synth_data$PMID, synth_data$region, synth_data$sub.region,
                               synth_data$stain_marker, synth_data$cell_type, 
                               synth_data$quantification_method, synth_data$dta,
                               synth_data$group_high_level2) %in%
                           paste(ratio_data$PMID[i],ratio_data$region[i],
                                 ratio_data$sub.region[i], ratio_data$stain_marker[i],
                                 ratio_data$cell_type[i], ratio_data$quantification_method[i],
                                 ratio_data$dta[i], ratio_data$group_high_level2[i]),]
  
  # Get the columns with the values
  col <- levels(droplevels(as.factor(crop_data$dta)))
  
  # For rows where values are a percentage 
  if (col %in% c("percent_of_control","percentage_loss")){
    ratio <- Hmisc::wtd.mean(crop_data[,"vect_mean"], 
                      crop_data[,"n"]/sum(crop_data[,"n"]))/100  
    
    sd_ratio <- radiant.data::weighted.sd(crop_data[,"vect_sd"], 
                            crop_data[,"n"]/sum(crop_data[,"n"]))/100 ### right ??
  }
  # For rows where values are not a percentage 
  else {
    mean_control <- crop_data[grepl("Control", crop_data$group_high_level), "vect_mean"]
    mean_pd <- crop_data[!grepl("Control", crop_data$group_high_level), "vect_mean"]
    sd_control <- crop_data[grepl("Control", crop_data$group_high_level), "vect_sd"]
    sd_pd <- crop_data[!grepl("Control", crop_data$group_high_level), "vect_sd"]
    
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
ratio_data0 <- plyr::ddply(ratio_data, 
                    c("PMID","region", "sub.region","stain_marker","cell_type",
                      "quantification_method","group_high_level2"), 
                    summarise, 
                    "global_mean" = mean(vect_ratio),
                    "global_sd" =mean(vect_sd_ratio))
ratio_data <- ratio_data[!duplicated(ratio_data[,c("PMID","region", "sub.region", "stain_marker","cell_type",
                                                    "quantification_method","group_high_level2")]),]
ratio_data[,c("vect_ratio","vect_sd_ratio")] <- ratio_data0[,c("global_mean",
                                                                "global_sd")]

final_data <- ratio_data


#### General graphs ####

# Publications by region
nb_by_region <- plyr::ddply(dataset2, c("region","quantification_method"),"nrow")
nb_by_region$indiv_pd <- apply(nb_by_region, 1, function(x,dataset2) 
                          sum(dataset2[(paste(dataset2$region, dataset2$quantification_method) 
                                                   %in% paste(x["region"], x["quantification_method"])) 
                                              & dataset2$group_high_level!="Control",
                                       "n"]), dataset2)
nb_by_region$indiv_cont <- apply(nb_by_region, 1, function(x,dataset2) 
  sum(dataset2[(paste(dataset2$region, dataset2$quantification_method) 
                %in% paste(x["region"], x["quantification_method"])) 
               & dataset2$group_high_level=="Control",
               "n"]), dataset2)


# Publications by cell_type
nb_by_cell <- plyr::ddply(dataset2, c("cell_type","quantification_method"),"nrow")

nb_by_cell$indiv_pd <- apply(nb_by_cell, 1, function(x,dataset2) 
  sum(dataset2[(paste(dataset2$cell_type, dataset2$quantification_method) 
                %in% paste(x["cell_type"], x["quantification_method"])) 
               & dataset2$group_high_level!="Control",
               "n"]), dataset2)
nb_by_cell$indiv_cont <- apply(nb_by_cell, 1, function(x,dataset2) 
  sum(dataset2[(paste(dataset2$cell_type, dataset2$quantification_method) 
                %in% paste(x["cell_type"], x["quantification_method"])) 
               & dataset2$group_high_level=="Control",
               "n"]), dataset2)

#### APP ####



##### Define UI #####
  
ui <- navbarPage("Selective Vulnerability Meta Analysis", id = "main_navbar",
                 tabPanel("General data", 
                          mainPanel(plotOutput("plot_region", click = "plot_region_click"),
                                    verbatimTextOutput("info_region"),
                                    plotOutput("plot_cell", click = "plot_cell_click"),
                                    verbatimTextOutput("info_cell"),
                                    plotOutput("plot_region-cell", click = "plot_region-cell_click"),
                                    verbatimTextOutput("info_region-cell")        
                            )
                 ),
                 
                 tabPanel("Plot",
  sidebarPanel(h3("Select filters :", style ="color: steelblue"),
               hr(),
    selectizeGroupUI(
      id = "my_filters",
      inline = FALSE,
      params = list(
        year_published = list( inputId = "year_published",
                               placeholder = 'Select a year of publication'),
        rob_score = list( inputId = "rob_score",
                               placeholder = 'Select a rob score'),
        data_type = list( inputId = "data_type",
                          placeholder = 'Select a type of data'),
        group_high_level = list( inputId = "group_high_level", 
                      placeholder = 'Select a group'),
        region = list( inputId = "region", 
                      placeholder = 'Select a region'),
        sub.region = list( inputId = "sub.region", 
                       placeholder = 'Select a sub region'),
        stain_marker = list( inputId = "stain_marker", 
                              placeholder = 'Select a stain marker'),
        cell_type = list( inputId = "cell_type", 
                          placeholder = 'Select a cell type'),
        quant_meth = list( inputId = "quantification_method", 
                           placeholder = 'Select a quantification method')
      )
    ),
    fluidRow(
      h5("Ratio", style ="color: steelblue"),
      column(6,
             numericInput("min_ratio",h5("Min"), 
                          value = 0, 
                          min = 0, 
                          max = round(max(final_data$vect_ratio), digits = 0))),
      column(6,
             numericInput("max_ratio",h5("Max"), 
                          value = round(max(final_data$vect_ratio), digits = 0), 
                          min = 0, 
                          max = round(max(final_data$vect_ratio)+5, digits = 0)))),
    fluidRow(
      h5("Standard deviation of the ratio", style ="color: steelblue"),
      column(6,
             numericInput("min_sd_ratio",h5("Min"), 
                          value = 0, 
                          min = 0, 
                          max = round(max(final_data$vect_sd_ratio), digits = 0))),
      column(6,
             numericInput("max_sd_ratio",h5("Max"), 
                          value = round(max(final_data$vect_sd_ratio), digits = 0), 
                          min = 0, 
                          max = round(max(final_data$vect_sd_ratio)+5, digits = 0))))
  ),
  mainPanel(
     fluidRow(plotOutput("plot1", click = "plot_click"),
             verbatimTextOutput("info")
))),

      tabPanel("Glossary",
               mainPanel(DTOutput("glossary")
               )
)
)


##### Define server #####

server <- function(input, output, session){
  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "my_filters",
    data = final_data,
    vars = c("year_published", "rob_score", "data_type", "group_high_level", "region", "sub.region",
             "stain_marker", "cell_type", "quantification_method"),
    inline = FALSE
  )
  
  res_mod2 <- reactive({
    res_mod2 <- res_mod()[res_mod()$vect_ratio >= input$min_ratio &
                            res_mod()$vect_ratio <= input$max_ratio &
                            res_mod()$vect_sd_ratio >= input$min_sd_ratio &
                            res_mod()$vect_sd_ratio <= input$max_sd_ratio,]
    
  })
  # Create a boxplot
  output$plot1 <- renderPlot({
    ggplot2::ggplot(res_mod2(), aes(x = PMID, y = vect_ratio, color = group_high_level2)) + geom_point(size = 1) +
      geom_errorbar(aes(ymin=res_mod2()$vect_ratio-res_mod2()$vect_sd_ratio,
                        ymax=res_mod2()$vect_ratio+res_mod2()$vect_sd_ratio), width = .2) + 
      theme(panel.background = element_rect(fill = "white"),
            panel.grid = element_blank(),
            axis.line = element_line(size = 0.5, colour = "darkgrey"),
            axis.text.x = element_text(size = 7, angle = 90, hjust = 1))
  })
  
  # Coordinates of selected points
  output$info <- renderPrint({
    req(input$plot_click)
    xvalue <- round(input$plot_click$x, 2)
    yvalue <- round(input$plot_click$y, 2)
    cat("PMID :", xvalue, "\t", "Ratio : ",yvalue)
  })

  # Plot Number of publication by region 
  output$plot_region <- renderPlot({
  ggplot2::ggplot(nb_by_region, mapping=aes(x=region, y=nrow, fill= quantification_method))+
    geom_bar(stat = "identity") +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid = element_blank(),
          axis.text.x = element_text(size = 7, angle = 90, hjust = 1))
  })
  output$info_region <- renderPrint({
    req(input$plot_region_click)
    xvalue <- levels(as.factor(nb_by_region$region))[round(input$plot_region_click$x,0)]
    return(nb_by_region[which(nb_by_region$region == as.factor(xvalue)),])
    
  })
  
  
  # Plot Number of publication by cell type 
  output$plot_cell <- renderPlot({
    ggplot2::ggplot(nb_by_cell, mapping=aes(x=cell_type, y=nrow, fill= quantification_method))+
      geom_bar(stat = "identity") +
      theme(panel.background = element_rect(fill = "white"),
            panel.grid = element_blank(),
            axis.text.x = element_text(size = 7, angle = 90, hjust = 1))
  })
  output$info_cell <- renderPrint({
    req(input$plot_cell_click)
    xvalue <- levels(as.factor(nb_by_cell$cell_type))[round(input$plot_cell_click$x,0)]
    return(nb_by_cell[which(nb_by_cell$cell_type == as.factor(xvalue)),])
    
  })
  
  # Glossary
  output$glossary <- renderDT(glossary)
}


##### Run the app #####
shinyApp(ui,server)

