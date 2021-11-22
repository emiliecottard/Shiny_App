region1 <- "VTA"
region2 <- "SNc"
cell <- "Dopaminergic" 


# Final Dataset filtered with regions and cell input 
summarized_data <- final_data[which(final_data$cell_type == cell & 
                                  (final_data$region == region1 | 
                                  final_data$region == region2)),]
summarized_data<- data.table::setDT(summarized_data)
summarized_data$conditions <- paste(summarized_data$PMID, summarized_data$region,
                                    summarized_data$sub.region,
                                    summarized_data$stain_marker,
                                    summarized_data$cell_type,
                                    summarized_data$quantification_method, 
                                    summarized_data$dta,
                                    summarized_data$group_high_level2)
summarized_data[,coeff:=1:.N]
summarized_data[,label:=paste("condition ",coeff)]


# Dataset with all individuals filtered with inputs 
individuals_data <- dataset2[which(dataset2$cell_type == cell & 
                                   (dataset2$region == region1 |
                                   dataset2$region == region2)),]
individuals_data<- data.table::setDT(individuals_data)


# Keep only the cases 
indiv <- individuals_data[individuals_data$group_high_level != "Control",]
indiv$conditions <- paste(indiv$PMID, indiv$region,indiv$sub.region,
                           indiv$stain_marker,indiv$cell_type,
                           indiv$quantification_method, indiv$dta,
                           indiv$group_high_level2)

# Get the mean of each controls (in synth_data dataset)
controls <- synth_data[synth_data$group_high_level == "Control",]
controls$conditions <- paste(controls$PMID, controls$region, controls$sub.region,
                           controls$stain_marker, controls$cell_type, 
                           controls$quantification_method, controls$dta,
                           controls$group_high_level2)

# Identify matching conditions between cases and controls 
# return the row in Controls data table with matching conditions
indiv$match <- apply(indiv, 1, 
      function(x, controls)which(x["conditions"] == controls["conditions"]),
      controls)
# When there is no match means the measure is a percentage of control

# Make the ratio for each individual
# Separate calculation for measures in percentage (measure = ratio*100)
indiv$ratio <- apply(indiv, 1, 
                     function(x, controls)ifelse(length(unlist(x["match"]))>0,
                                as.numeric(x[x["dta"][[1]]][[1]])/ 
            as.numeric(controls[x["match"][[1]],controls[x["match"][[1]],"dta"]]),
          x[x["dta"][[1]]][[1]]/100),
       controls) 

# Calculate sd if possible
indiv$sd <- apply(indiv, 1, 
                  function(x, controls)ifelse(length(unlist(x["match"]))>0,
                                              as.numeric(x[x["dta"][[1]]][[1]])/ 
                                                as.numeric(controls[x["match"][[1]],controls[x["match"][[1]],"dta"]]),
                                              x[x["dta"][[1]]][[1]]/100),
                  controls)

#Scale for plot 
indiv[,rank:=1:.N, by = conditions]
indiv[,sum:=max(rank)+1, by = conditions]
conditions <- unique(indiv$conditions)
conditions <- cbind(conditions, "coeff" = c(1:length(conditions)))
indiv <- merge(x = indiv, y = conditions, 
               by = "conditions", all.x = TRUE)
indiv[,coeff:=(as.numeric(coeff) +((1/sum)*rank))]

#Region1                                     
ggplot(data = summarized_data[summarized_data$region == region1,],
       mapping = aes(x = coeff, y = vect_ratio, color = "red"))+
  geom_point(size = 0.9)+
  geom_errorbar(mapping = aes(ymin=vect_ratio-vect_sd_ratio,
                              ymax=vect_ratio+vect_sd_ratio),
                width = .2) +
  
  geom_point(data = indiv[indiv$region == region1,],
             aes(x = coeff, y = ratio), color = "blue", size = 0.9)

# Region2
ggplot(data = summarized_data[summarized_data$region == region2,],
       mapping = aes(x = coeff, y = vect_ratio, color = "red"))+
  geom_point(size = 0.9)+
  geom_errorbar(mapping = aes(ymin=vect_ratio-vect_sd_ratio,
                              ymax=vect_ratio+vect_sd_ratio),
                width = .2) +
  
  geom_point(data = indiv[indiv$region == region2,],
             aes(x = coeff, y = ratio), color = "blue", size = 0.9)+
  annotate("text", x = summarized_data$coeff[summarized_data$region == region2]+0.5,
           y= 1, label = summarized_data$label[summarized_data$region == region2], angle = 90)
  
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.line = element_line(size = 0.5, colour = "darkgrey"),
        axis.text.x = element_text(size = 7, angle = 90, hjust = 1),
        
