#' timeloop() Function
#'
#' This function loop function
#' @param data : data 
#' @param grpby : grpby
#' @param type : biomass; importance value
#' @return merge data
#' @export


timeloop <- function(data, grpby="시군구코드", type = "biomass"){
  
  for(i in 1:(length(unique(data$"조사연도"))-4)){ 
    
    timeloop <- vector("list", length = (length(unique(data$"조사연도"))-4))
    s_year <- min(data$"조사연도")+i-1
    e_year <- min(data$"조사연도")+i-1+4
    data_temp <- data %>% filter(data$"조사연도" >= s_year & data$"조사연도" <= e_year)
    
    
    if(type == "biomass"){
      timeloop[[i]] <- biomass2(data_temp, byplot = FALSE, grpby = grpby )
      timeloop[[i]]$year <- e_year
    }
    else if(type == "iv"){
      timeloop[[i]] <- importancevalue_data(data_temp)
      timeloop[[i]]$year <- e_year
    }
  }
  
  timeloop_df <- do.call(rbind, timeloop)
  
}
  



