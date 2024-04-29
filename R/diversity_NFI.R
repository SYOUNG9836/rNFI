#' diversity Function
#'
#' This function 
#' @param data : data
#' @param grpby : grpby
#' @param basal : 흉고단면적/개체수
#' @param largetreearea : 대경목조사원
#' @param Stockedland : 임목지
#' @param talltree : 교목
#' @param byplot : byplot
#' @param clusterplot : byplot TRUE 집락 부 
#' @keywords plot
#' @return diversity
#' @export 
#' 
#' 


##  

diversity_NFI <- function(data, grpby=NULL, type="tree" , byplot= FALSE,  basal=FALSE, clusterplot=FALSE, largetreearea=TRUE, Stockedland=TRUE, talltree=TRUE){
  
  # 경고 
  required_names <- c("plot", "tree")
  
  if (!all(required_names %in% names(data))) {
    missing_dfs <- required_names[!required_names %in% names(data)]
    stop("Missing required data frames in the list: ", paste(missing_dfs, collapse = ", "), call. = FALSE)
  }
  
  
  if (!is.null(grpby)){
    if(!is.character(grpby)) {
      stop("param 'grpby' must be 'character'")
    }}
  
  
  if (type != "tree"){
    if(basal) {
      stop("param 'basal' must be 'FALSE' if param 'type' is ", type)
    }}
  
  
  
  
  # 전처리 
  if(clusterplot){
    plot_id <- c('CLST_PLOT')
  }else{
    plot_id <- c('SUB_PLOT')
  }
  
  
  if(type=="tree"){
    
    if (Stockedland){ #임목지
      data <- filter_NFI(data, c("plot$LAND_USECD == 1"))
    }
    
    if(talltree){#수목형태구분
      data$tree <- data$tree %>% filter(WDY_PLNTS_TYP_CD == 1)
    }
    
    if(!largetreearea){ #대경목조사원내존재여부
      data$tree <- data$tree %>% filter(SUBPTYP == 0)
    }
    
    
    df <- left_join(data$tree[, c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'WDY_PLNTS_TYP_CD','SP', 
                                  'basal_area', 'SUBPTYP')], 
                    data$plot[,c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'INVYR', "LAND_USE", "LAND_USECD", grpby)],
                    by = c("CLST_PLOT", "SUB_PLOT", "CYCLE"))
    
    

  }else if(type=="herb"){
    df <- left_join(data$herb[, c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'SP')], 
                    data$plot[,c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'INVYR', "LAND_USE", "LAND_USECD", grpby)],
                    by = c("CLST_PLOT", "SUB_PLOT", "CYCLE"))
    
  }else if(type=="veg"){
    df <- left_join(data$veg[, c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'SP', 'VEGPLOT', 'NUMINDI')], 
                    data$plot[,c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'INVYR', "LAND_USE", "LAND_USECD", grpby)],
                    by = c("CLST_PLOT", "SUB_PLOT", "CYCLE"))
    
  }else if(type="sapling"){
    
    df <- left_join(data$sapling[, c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'SP', 'TREECOUNT')], 
                    data$plot[,c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'INVYR', "LAND_USE", "LAND_USECD", grpby)],
                    by = c("CLST_PLOT", "SUB_PLOT", "CYCLE"))
    
  }else(
    
    stop("param 'type' must be one of c('tree', 'herb', 'veg', 'sapling')")
  )
  
  
  plot_id  <- rlang::sym(plot_id)
  grpby  <- rlang::syms(grpby)
  

  
  
  # 종다양성 구하기
  
  if(basal & type=="tree"){ # 흉고단면적 기준 종다양성 
    
    indices_temp <- df %>%
      group_by(CYCLE, !!plot_id, !!!grpby, SP) %>%
      summarise(value = sum(basal_area), .groups = 'drop')
    
  }else{ # 개체수 기준 종다양성
    
    if(type="tree"||type= "herb"){
      
      indices_temp <- df %>%
        group_by(CYCLE, !!plot_id, !!!grpby, SP) %>%
        summarise(value = n(), .groups = 'drop')
      
    }else if(type=="veg"){
      
      indices_temp <- df %>%
        group_by(CYCLE, !!plot_id, !!!grpby, SP) %>%
        summarise(value = sum(NUMINDI), .groups = 'drop')
      
    }else{ #sapling
      
      indices_temp <- df %>%
        group_by(CYCLE, !!plot_id, !!!grpby, SP) %>%
        summarise(value = sum(TREECOUNT), .groups = 'drop')
    }
  }
  
  
  indices_temp <- indices_temp %>% tidyr::spread(key = SP, value = value )
  
  
  indices <- indices_temp[,1:(length(grpby)+2)]
  abundance.matrix <- indices_temp[,-c(1:(length(grpby)+2))]
  abundance.matrix[is.na(abundance.matrix)] <- 0
  
  indices$Richness <- rowSums(abundance.matrix>0)
  indices$Shannon <- vegan::diversity(abundance.matrix) # shannon is default
  indices$simpson <- vegan::diversity(abundance.matrix, "simpson")
  indices$evenness  <- indices$Shannon/log(indices$Richness)
  
  
  if(!byplot){
    
    indices <- indices %>% 
      group_by(CYCLE, !!!grpby) %>% 
      summarise(mean_Richness = mean(Richness , na.rm=TRUE),
                se_Richness =  plotrix::std.error(Richness, na.rm=TRUE),
                mean_Shannon = mean(Shannon, na.rm=TRUE),
                se_Shannon =  plotrix::std.error(Shannon, na.rm=TRUE),
                mean_simpson = mean(simpson, na.rm=TRUE),
                se_simpson =  plotrix::std.error(simpson, na.rm=TRUE),
                mean_evenness = mean(evenness, na.rm=TRUE),
                se_evenness =  plotrix::std.error(evenness, na.rm=TRUE),.groups = 'drop')
    
    
  }
  
  
  return(indices)
  
  
}
