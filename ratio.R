# Remove the spaces after PD , Control....



f <- function(dataset, pmid, pd, control){
  dta <- dataset %>% filter(PMID == pmid) %>% filter(group == case)
  mean_pd <- mean(f(dataset,pmid,pd))
  mean_control <- mean(f(dataset,pmid,control))
  var_pd <- var(f(dataset,pmid,pd))
  var_control <- var(f(dataset,pmid,control))
  ratio = mean_pd / mean_control
  var_ratio = var_pd / ((mean_control)^2) + (var_control*(mean_pd**2)) / (mean_control^4)
  return(ratio, var_ratio)
}

pmid = "6198483"
PD = "PD "
Control = "Control "
