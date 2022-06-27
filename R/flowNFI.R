#' flowNFI() Function
#'
#' This function loop function
#' @param data : data 
#' @param grpby : grpby
#' @param type : biomass; iv
#' @return merge data
#' @export


flowNFI <- function(data, grpby="", type = "biomass"){
  
  for(i in 1:(length(unique(data$"조사연도"))-4)){ 
    
    flow <- vector("list", length = (length(unique(data$"조사연도"))-4))
    s_year <- min(data$"조사연도")+i-1
    e_year <- min(data$"조사연도")+i-1+4
    data_temp <- data %>% filter(data$"조사연도" >= s_year & data$"조사연도" <= e_year)
    
    
    if(type == "biomass"){
      flow[[i]] <- biomass2(data_temp, byplot = FALSE, grpby = grpby )
      flow[[i]]$year <- e_year
    }
    else if(type == "iv"){
      flow[[i]] <- importancevalue_data(data_temp)
      flow[[i]]$year <- e_year
    }
  }
  
  flow_df <- do.call(rbind, flow)
  
}
  



