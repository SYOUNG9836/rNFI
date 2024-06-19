#' @description
#' diversity_NFI() is a function that species richness, evenness and the Shannon and Simpson diversity indices.
#'
#' @details
#' 
#' 
#' 
#' 
#' @param data : A `list` produced by \code{\link{read_NFI}} that contains 'plot' and one of ('tree', 'herb', 'veg', 'sapling') data frames.
#' @param sp : A character value indicating the column name of tree species.
#' @param grpby : grpby
#' @param type : grpby
#' @param byplot : byplot
#' @param basal : 흉고단면적/개체수
#' @param clusterplot : A logical value indicating whether to calculate for cluster plot collectively or calculate for each subplot separately.
#' @param largetreearea : A logical value indicating whether to include a large tree plot as well, or only a tree plot.
#' @param Stockedland : A logical value indicating whether to include only stocked land or also include other types of land.
#' @param talltree : A logical value indicating whether to include only tall trees or also shrubs.
#' 
#' @return A `data.frame` that includes diversity indices or numbers of species. standard error
#' 
#' @note 
#' 식생데이터 데이터 이상함
#' 
#' @export 
#' 
#' 


##  

diversity_NFI <- function(data, sp="SP", grpby=NULL, type="tree" , byplot= FALSE,  basal=FALSE, clusterplot=FALSE, largetreearea=TRUE, Stockedland=TRUE, talltree=TRUE){
  
  # 경고 
  required_names <- c("plot", type)
  
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
      data$tree <- data$tree %>% filter(LARGEP_TREE == 0)
    }
    
    
    df <- left_join(data$tree[, c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'WDY_PLNTS_TYP_CD', 
                                  'basal_area', 'LARGEP_TREE', sp)], 
                    data$plot[,c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'INVYR', "LAND_USE", "LAND_USECD", grpby)],
                    by = c("CLST_PLOT", "SUB_PLOT", "CYCLE"))
    
    

  }else if(type=="herb"){
    df <- left_join(data$herb[, c('CLST_PLOT', 'SUB_PLOT', "CYCLE", sp)], 
                    data$plot[,c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'INVYR', "LAND_USE", "LAND_USECD", grpby)],
                    by = c("CLST_PLOT", "SUB_PLOT", "CYCLE"))
    
  }else if(type=="veg"){
    df <- left_join(data$veg[, c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'VEGPLOT', 'NUMINDI', sp)], 
                    data$plot[,c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'INVYR', "LAND_USE", "LAND_USECD", grpby)],
                    by = c("CLST_PLOT", "SUB_PLOT", "CYCLE"))
    
  }else if(type=="sapling"){
    
    df <- left_join(data$sapling[, c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'TREECOUNT', sp)], 
                    data$plot[,c('CLST_PLOT', 'SUB_PLOT', "CYCLE", 'INVYR', "LAND_USE", "LAND_USECD", grpby)],
                    by = c("CLST_PLOT", "SUB_PLOT", "CYCLE"))
    
  }else(
    
    stop("param 'type' must be one of c('tree', 'herb', 'veg', 'sapling')")
  )
  
  
  plot_id  <- rlang::sym(plot_id)
  grpby  <- rlang::syms(grpby)
  sp<- rlang::sym(sp)
  

  
  
  # 종다양성 구하기
  
  if(basal & type=="tree"){ # 흉고단면적 기준 종다양성 
    
    indices_temp <- df %>%
      group_by(CYCLE, !!plot_id, !!!grpby, !!sp) %>%
      summarise(value = sum(basal_area), .groups = 'drop')
    
  }else{ # 개체수 기준 종다양성
    
    if(type=="tree"||type== "herb"){
      
      indices_temp <- df %>%
        group_by(CYCLE, !!plot_id, !!!grpby, !!sp) %>%
        summarise(value = n(), .groups = 'drop')
      
    }else if(type=="veg"){
      
      indices_temp <- df %>%
        group_by(CYCLE, !!plot_id, !!!grpby, !!sp) %>%
        summarise(value = sum(NUMINDI), .groups = 'drop')
      
    }else{ #sapling
      
      indices_temp <- df %>%
        group_by(CYCLE, !!plot_id, !!!grpby, !!sp) %>%
        summarise(value = sum(TREECOUNT), .groups = 'drop')
    }
  }
  
  
  indices_temp <- indices_temp %>% tidyr::spread(key = !!sp, value = value )
  
  
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
