#' Reads Korean National Forest Inventory
#'
#' @description
#' read_NFI() is a function that reads Korean National Forest Inventory (NFI).
#' Loads the annual NFI file from the local computer.
#' And change the data to an easy-to-analyze format and perform integrity verification.
#' read_NFI() allows you to select and load specific districts and desired tables.
#' NFI is downloaded from \url{https://kfss.forest.go.kr/stat/}.
#' 
#' @details 
#' `plot` This table contains data for each subplot, including site, stand, non-forest area, and other information. It is a base part, so there is no need to configure it separately. It includes many other variables as well.
#' `tree` This table records tree survey data, including species, diameter at breast height (DBH), and tree height, among others.
#' `cwd` Coarse Woody Debris table. It includes information on species, tree decay level, and cause of death, among other details. Data is collected only at the center subplot of the cluster.
#' `stump` This table provides data on stumps, including species and diameter at 20 cm above the ground, among other details. Data is collected only at the center subplot of the cluster.
#' `sapling` This table includes information on saplings, such as species, diameter at 20 cm above the ground, and the number of individuals, among other details. Data is collected only at the sapling plot of the subplot.
#' `veg` This table includes data on vegetation, covering both woody and herbaceous plants. It records species, number of individuals, and dominance information, among others. Data is collected from three vegetation plots located within 25% of total center subplot.
#' `herb` This table lists herbaceous species. Data is collected only at the sapling plot of the subplot. It includes many other variables as well. 
#' `soil` This table contains soil data, including the thickness of the organic layer and soil depth, among others. Data is collected from three soil plots located within 25% of total center subplot.
#' For more details, refer to the National Forest Inventory guidelines.
#' 
#' This functionality performs integrity validation on data provided by the Korea Forest Service, based on the database of subplot and species. 
#' It corrects errors in the administrative regions of subplots and in the classification of species as coniferous or deciduous. 
#' The existing data, which only provides species names, has been augmented with the Korean and English names of families and genera, as well as the scientific names of species. 
#' The classification of species into coniferous and broadleaf categories, as well as plant taxonomy, follows the standards of the Korean Plant Names Index Committee of the Korea National Arboretum \url{http://www.nature.go.kr/kpni/index.do}. 
#' Additionally, this functionality calculates the forest type, dominant species, and dominant species percentage for each subplot and cluster plot. 
#'  
#' @param dir : A character vector; directory of NFI files.
#' @param district : A character vector; the district's Korean name within levels such as sido, sigungu, or eupmyondong. If `NULL`, the entire dataset is loaded. Use \code{c()} to combine multiple variables. e.g., `c('tree', 'cwd', 'stump', 'sapling', 'veg', 'herb', 'soil')`.  
#' @param tables : A character vector; names of specific tables to be imported. Can be any of 'tree', 'cwd', 'stump', 'sapling', 'veg', 'herb', 'soil'. Use \code{c()} to combine multiple variables. 
#' 
#' @return A `data.frame`; the loaded and transformed NFI data, structured for easy analysis. Columns and structure depend on the survey tables loaded.
#' 
#' @examples
#' \dontrun{
#' read_NFI("D:/NFI/NFI5", district=NULL, tables=c("tree", "cwd"))
#' }
#' 
#' @note  
#' To download subsets of the annual NFI file manually, go online to the Korea Forest Service Forestry Statistics Platform (\url{https://kfss.forest.go.kr/stat/}), downloaded .zip files, and extract them.
#' Load the data \code{rNFI::col_name} to find out the Korean and English names of the column names. 
#' 
#' The National Forest Inventory conducts internal reviews, field inspections, and error prevention efforts to maintain quality. 
#' However, given that approximately 4,000 plots and over 70 items are surveyed in the 7th phase, various errors may still exist. 
#' Please use the data with caution, and share any anomalies you discover with us so we can incorporate them into our algorithms.
#' 
#' @export


read_NFI <- function(dir, district=NULL, tables=c("tree", "cwd")){
  
  
  ## Load a list of .xlsx files located in the path--------------------------------------------------
  if (stringr::str_sub(dir,-1) != '/'){
    dir <- paste(dir, '/', sep = "")} 
  
  
  ## error message--------------------------------------------------------------
  if(!dir.exists(dir)) {
    stop(paste('Directory ', dir, ' does not exist.'))
  }
  
  
  
  if(!is.null(tables)){
    if(!tables %in%  c('plot', 'tree', 'cwd', 'stump', 'herb', 'veg', 'sapling', 'soil')){
      stop("param 'tables' must be one of 'plot', 'tree', 'cwd', 'stump', 'sapling', 'veg', 'herb', 'soil'")
    }
  }
  
  
  
  filenames <- list.files(path=dir, pattern="xlsx")
  
  plot_list <- vector("list", length = length(filenames))
  tree_list <- vector("list", length = length(filenames))
  cwd_list <- vector("list", length = length(filenames))
  stump_list <- vector("list", length = length(filenames))
  sapling_list <- vector("list", length = length(filenames))
  veg_list <- vector("list", length = length(filenames))
  herb_list <- vector("list", length = length(filenames))
  soil_list <- vector("list", length = length(filenames))
  

  for(i in 1:length(filenames)){
    
    ## General_info sheet --------------------------------------------------------------
    General_info <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = Sheet_name[1,1], 
                                       col_names = TRUE, col_types = "text")
    
    colnames(General_info) <- gsub("\\s+", " ", gsub("[`]", "", colnames(General_info)))
    
    general_info_colnames <- names(General_info)
    missing_names <- general_info_colnames[!general_info_colnames %in% col_name$Korean_Column_Name]
    
    for (name in missing_names) {
      col_name <- rbind(col_name, data.frame(Column_Name = name, English_Name = NA, Korean_Column_Name = name))
    }
    
    new_names <- stats::setNames(col_name$Column_Name, col_name$Korean_Column_Name)
    General_info <- stats::setNames(General_info, new_names[names(General_info)])
    
    General_info <- General_info[(names(General_info) != c("CREATED_DATE"))]
    
    
    
    ## Non_forest sheet -------------------------------------------------------------
    Non_forest <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = Sheet_name[2,1],
                                     col_names = TRUE, col_types = "text")
    colnames(Non_forest) <- gsub("\\s+", " ", gsub("[`]", "", colnames(Non_forest)))
    
    Non_forest_colnames <- names(Non_forest)
    missing_names <- Non_forest_colnames[!Non_forest_colnames %in% col_name$Korean_Column_Name]
    
    for (name in missing_names) {
      col_name <- rbind(col_name, data.frame(Column_Name = name, English_Name = NA, Korean_Column_Name = name))
    }
    
    new_names <- stats::setNames(col_name$Column_Name, col_name$Korean_Column_Name)
    Non_forest <- stats::setNames(Non_forest, new_names[names(Non_forest)])
    
    
    
    ## Stand_inve sheet --------------------------------------------------------------
    Stand_inve <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = Sheet_name[3,1],
                                     col_names = TRUE, col_types = "text")
    colnames(Stand_inve) <- gsub("\\s+", " ", gsub("[`]", "", colnames(Stand_inve)))
    
    Stand_inve_colnames <- names(Stand_inve)
    missing_names <- Stand_inve_colnames[!Stand_inve_colnames %in% col_name$Korean_Column_Name]
    
    for (name in missing_names) {
      col_name <- rbind(col_name, data.frame(Column_Name = name, English_Name = NA, Korean_Column_Name = name))
    }
    
    new_names <- stats::setNames(col_name$Column_Name, col_name$Korean_Column_Name)
    Stand_inve <- stats::setNames(Stand_inve, new_names[names(Stand_inve)])
    
    
    ## NFI_plot_DB 
    Stand_inve <- Stand_inve[!(names(Stand_inve) %in% c("SIDO_CD", "SGG_CD", "EMD_CD", "SIDO", "SGG", "EMD"))]
    Stand_inve <- left_join(Stand_inve, NFI_plot_DB, by=c('SUB_PLOT'))
    
    
    ## district filtering --------------------------------------------------------------
    if(!is.null(district)){
      
      if(!is.character(district)) {
        stop("param 'district' must be 'character'")
      }
      
      
      if(any(district %in% district_code[,2] == FALSE )) {
        stop(paste( 'District ', district, ' does not exist.'))
      }
      
      
      site_codes <- sapply(district, function(d) {
        gsub("-", "", district_code[district_code[, "district_name"] == d, "district_CD"][1])
      })
      

      Stand_inve <- bind_rows(lapply(site_codes, function(site_code) {
        if(nchar(site_code) == 10) {
          Stand_inve %>% filter(EMD_CD == substr(site_code, 1, 8))
        } else if(nchar(site_code) == 5) {
          Stand_inve %>% filter(SGG_CD == site_code)
        } else {
          Stand_inve %>% filter(SIDO_CD == site_code)
        }
      }))
      
      
      ## error: No NFI data for the district-----------------------------------------------------------
      if(nrow(Stand_inve) == 0){
        stop(paste('NFI data in ',district ,' does not exist.'))}
      
    }
    
    
    
    
    ## Merge General_info and Non_forest based on Stand_inve----------------------------------------------
    Stand_inve <- left_join(x=Stand_inve, y=General_info, 
                            by=c('CLST_PLOT', 'SUB_PLOT', 'CYCLE',  'INVYR', 'FORTYPCD', 'FORTYP'))
    
    Stand_inve <- left_join(x=Stand_inve, y=Non_forest, 
                            by=c('CLST_PLOT', 'SUB_PLOT', 'CYCLE',  'INVYR'))
    
    
    ## Plot data by .xlsx (yearly)------------------------------------------
    plot_list[[i]] <- Stand_inve
    plot_all <- unique(plot_list[[i]]$SUB_PLOT)
    
    
    ## tree_list sheet --------------------------------------------------------------
    if("tree" %in% tables){
      tree_list[[i]] <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = Sheet_name[4,1],
                                           col_names = TRUE, col_types = "text")
      
      colnames(tree_list[[i]]) <- gsub("\\s+", " ", gsub("[`]", "", colnames(tree_list[[i]])))
      
      tree_list_colnames <- names(tree_list[[i]])
      missing_names <- tree_list_colnames[!tree_list_colnames %in% col_name$Korean_Column_Name]
      
      for (name in missing_names) {
        col_name <- rbind(col_name, data.frame(Column_Name = name, English_Name = NA, Korean_Column_Name = name))
      }
      
      new_names <- stats::setNames(col_name$Column_Name, col_name$Korean_Column_Name)
      tree_list[[i]] <- stats::setNames(tree_list[[i]], new_names[names(tree_list[[i]])])
      
      
      tree_list[[i]] <- tree_list[[i]][tree_list[[i]]$SUB_PLOT %in% plot_all,]
    }
    
    
    if("cwd" %in% tables){
      ## cwd_list sheet --------------------------------------------------------------
      cwd_list[[i]] <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = Sheet_name[5,1], range = cellranger::cell_cols("A:M"),
                                          col_names = TRUE, col_types = "text")
      
      colnames(cwd_list[[i]]) <- gsub("\\s+", " ", gsub("[`]", "", colnames(cwd_list[[i]])))
      
      cwd_list_colnames <- names(cwd_list[[i]])
      missing_names <- cwd_list_colnames[!cwd_list_colnames %in% col_name$Korean_Column_Name]
      
      for (name in missing_names) {
        col_name <- rbind(col_name, data.frame(Column_Name = name, English_Name = NA, Korean_Column_Name = name))
      }
      
      new_names <- stats::setNames(col_name$Column_Name, col_name$Korean_Column_Name)
      cwd_list[[i]] <- stats::setNames(cwd_list[[i]], new_names[names(cwd_list[[i]])])
      
      cwd_list[[i]] <- cwd_list[[i]][cwd_list[[i]]$SUB_PLOT %in% plot_all,]
    }
    
    
    if("stump" %in% tables){
      ## stump_list sheet --------------------------------------------------------------
      stump_list[[i]] <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = Sheet_name[6,1], 
                                            col_names = TRUE, col_types = "text")
      
      colnames(stump_list[[i]]) <- gsub("\\s+", " ", gsub("[`]", "", colnames(stump_list[[i]])))
      
      stump_list_colnames <- names(stump_list[[i]])
      missing_names <- stump_list_colnames[!stump_list_colnames %in% col_name$Korean_Column_Name]
      
      for (name in missing_names) {
        col_name <- rbind(col_name, data.frame(Column_Name = name, English_Name = NA, Korean_Column_Name = name))
      }
      
      new_names <- stats::setNames(col_name$Column_Name, col_name$Korean_Column_Name)
      stump_list[[i]] <- stats::setNames(stump_list[[i]], new_names[names(stump_list[[i]])])
      
      stump_list[[i]] <- stump_list[[i]][stump_list[[i]]$SUB_PLOT %in% plot_all,]
    }
    
    
    if("sapling" %in% tables){
      ## sapling_list sheet--------------------------------------------------------------
      sapling_list[[i]] <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = Sheet_name[7,1], 
                                              col_names = TRUE, col_types = "text")
      
      colnames(sapling_list[[i]]) <- gsub("\\s+", " ", gsub("[`]", "", colnames(sapling_list[[i]])))
      
      sapling_list_colnames <- names(sapling_list[[i]])
      missing_names <- sapling_list_colnames[!sapling_list_colnames %in% col_name$Korean_Column_Name]
      
      for (name in missing_names) {
        col_name <- rbind(col_name, data.frame(Column_Name = name, English_Name = NA, Korean_Column_Name = name))
      }
      
      new_names <- stats::setNames(col_name$Column_Name, col_name$Korean_Column_Name)
      sapling_list[[i]] <- stats::setNames(sapling_list[[i]], new_names[names(sapling_list[[i]])])
      
      sapling_list[[i]] <- sapling_list[[i]][sapling_list[[i]]$SUB_PLOT %in% plot_all,]
      
    }
    
    
    if("veg" %in% tables){
      ## veg_list sheet --------------------------------------------------------------
      veg_list[[i]] <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = Sheet_name[8,1], 
                                          col_names = TRUE, col_types = "text")
      
      colnames(veg_list[[i]]) <- gsub("\\s+", " ", gsub("[`]", "", colnames(veg_list[[i]])))
      
      veg_list_colnames <- names(veg_list[[i]])
      missing_names <- veg_list_colnames[!veg_list_colnames %in% col_name$Korean_Column_Name]
      
      for (name in missing_names) {
        col_name <- rbind(col_name, data.frame(Column_Name = name, English_Name = NA, Korean_Column_Name = name))
      }
      
      new_names <- stats::setNames(col_name$Column_Name, col_name$Korean_Column_Name)
      veg_list[[i]] <- stats::setNames(veg_list[[i]], new_names[names(veg_list[[i]])])
      
      veg_list[[i]] <- veg_list[[i]][veg_list[[i]]$SUB_PLOT %in% plot_all,]
    }
    
    
    
    if("herb" %in% tables){
      ## herb_list sheet --------------------------------------------------------------
      herb_list[[i]] <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = Sheet_name[9,1], 
                                           col_names = TRUE, col_types = "text")
      
      colnames(herb_list[[i]]) <- gsub("\\s+", " ", gsub("[`]", "", colnames(herb_list[[i]])))
      
      herb_list_colnames <- names(herb_list[[i]])
      missing_names <- herb_list_colnames[!herb_list_colnames %in% col_name$Korean_Column_Name]
      
      for (name in missing_names) {
        col_name <- rbind(col_name, data.frame(Column_Name = name, English_Name = NA, Korean_Column_Name = name))
      }
      
      new_names <- stats::setNames(col_name$Column_Name, col_name$Korean_Column_Name)
      herb_list[[i]] <- stats::setNames(herb_list[[i]], new_names[names(herb_list[[i]])])
      
      herb_list[[i]] <- herb_list[[i]][herb_list[[i]]$SUB_PLOT %in% plot_all,]
    }
    
    
    if("soil" %in% tables){
      ## soil_list sheet --------------------------------------------------------------
      soil_list[[i]] <- readxl::read_excel(paste(dir, filenames[i], sep = ""), sheet = Sheet_name[10,1], 
                                           col_names = TRUE, col_types = "text")
      
      colnames(soil_list[[i]]) <- gsub("\\s+", " ", gsub("[`]", "", colnames(soil_list[[i]])))
      
      soil_list_colnames <- names(soil_list[[i]])
      missing_names <- soil_list_colnames[!soil_list_colnames %in% col_name$Korean_Column_Name]
      
      for (name in missing_names) {
        col_name <- rbind(col_name, data.frame(Column_Name = name, English_Name = NA, Korean_Column_Name = name))
      }
      
      new_names <- stats::setNames(col_name$Column_Name, col_name$Korean_Column_Name)
      soil_list[[i]] <- stats::setNames(soil_list[[i]], new_names[names(soil_list[[i]])])
      
      soil_list[[i]] <- soil_list[[i]][soil_list[[i]]$SUB_PLOT %in% plot_all,]
    }
    
    
  }  

  ## Merge data by .xlsx (yearly)--------------------------------------------------------------
  plot_df <- data.table::rbindlist(plot_list, fill=TRUE, use.names=TRUE)
  plot_df <- as.data.frame(plot_df)
  
  
  NFI <- list(plot = plot_df)
  plot_subset <- NFI$plot[,c('CLST_PLOT', 'SUB_PLOT', 'CYCLE', 'INVYR'), drop = FALSE]
  
  if("tree" %in% tables){
    tree_df <- data.table::rbindlist(tree_list, fill=TRUE, use.names=TRUE)
    tree_df <- as.data.frame(tree_df)
    
    NFI$tree <- tree_df
    NFI$tree <- left_join(NFI$tree, plot_subset,
                          by = c('CLST_PLOT', 'SUB_PLOT', 'CYCLE'))
  }
  
  
  if("cwd" %in% tables){
    cwd_df <- data.table::rbindlist(cwd_list, fill=TRUE, use.names=TRUE)
    cwd_df <- as.data.frame(cwd_df)
    
    NFI$cwd <- cwd_df
    NFI$cwd <- left_join(NFI$cwd, plot_subset, 
                         by = c('CLST_PLOT', 'SUB_PLOT', 'CYCLE'))
  }
  
  
  if("stump" %in% tables){
    stump_df <- data.table::rbindlist(stump_list, fill=TRUE, use.names=TRUE)
    stump_df <- as.data.frame(stump_df)
    
    NFI$stump <- stump_df
    NFI$stump <- left_join(NFI$stump, plot_subset, 
                           by = c('CLST_PLOT', 'SUB_PLOT', 'CYCLE'))
  }
  
  
  if("sapling" %in% tables){
    sapling_df <- data.table::rbindlist(sapling_list, fill=TRUE, use.names=TRUE)
    sapling_df <- as.data.frame(sapling_df)
    
    NFI$sapling <- sapling_df
    NFI$sapling <- left_join(NFI$sapling, plot_subset, 
                             by = c('CLST_PLOT', 'SUB_PLOT', 'CYCLE'))
  }
  
  
  if("veg" %in% tables){
    veg_df <- data.table::rbindlist(veg_list, fill=TRUE, use.names=TRUE)
    veg_df <- as.data.frame(veg_df)
    
    NFI$veg <- veg_df
    NFI$veg <- left_join(NFI$veg, plot_subset, 
                         by = c('CLST_PLOT', 'SUB_PLOT', 'CYCLE'))
  }
  
  if("herb" %in% tables){
    herb_df <- data.table::rbindlist(herb_list, fill=TRUE, use.names=TRUE)
    herb_df <- as.data.frame(herb_df)
    
    NFI$herb <- herb_df
    NFI$herb <- left_join(NFI$herb, plot_subset, 
                          by = c('CLST_PLOT', 'SUB_PLOT', 'CYCLE'))
  }
  
  if("soil" %in% tables){
    soil_df <- data.table::rbindlist(soil_list, fill=TRUE, use.names=TRUE)
    soil_df <- as.data.frame(soil_df)
    
    NFI$soil <- soil_df
    NFI$soil <- left_join(NFI$soil, plot_subset, 
                          by = c('CLST_PLOT', 'SUB_PLOT', 'CYCLE'))
  }


  ## Assign column attributes --------------------------------------------------------------
  log_col <- c("FORCD", "SVYCD")
  NFI$plot[ , colnames(NFI$plot) %in% log_col ] <- lapply(lapply(NFI$plot[ , colnames(NFI$plot) %in% log_col ], as.numeric), as.logical)
  
  
  fac_col <- c("LAND_USECD", "LAND_USE","FORTYPCD","FORTYP", "DECAYCD")
  NFI$plot[ , colnames(NFI$plot) %in% fac_col ] <- lapply(NFI$plot[ , colnames(NFI$plot) %in% fac_col ], as.factor)
  
  
  num_col <- c("NONFR_INCL_AREA_SUBP", "NONFR_INCL_AREA_LARGEP","RDDIST", "ELEV","SLOPE",
               "ASPCT",
               "DBH", "BOLE_HT", "HT", "DIST", "AZIMUTH", "TOTAGE",	"PITH_BARK_LEN",	"TRG_5YRS",
               "BARK_THICK",	"STD_DIAM_PROP", "ACTUALVOL",	"HT_EST",	"VOL_EST", "INVYR", "VOL", "HT", "DIA")
  NFI$plot[ , colnames(NFI$plot) %in% num_col ] <- lapply(NFI$plot[ , colnames(NFI$plot) %in% num_col ], as.numeric)
  NFI$tree[ , colnames(NFI$tree) %in% num_col ] <- lapply(NFI$tree[ , colnames(NFI$tree) %in% num_col ], as.numeric)
  
  
  char_col <- c("SUB_PLOT", "CLST_PLOT", "CREATED_DATE", "CTPRVN_CD","SIG_CD", "EMD_CD")
  NFI$plot[ , colnames(NFI$plot) %in% char_col ] <- lapply(NFI$plot[ , colnames(NFI$plot) %in% char_col ], as.character)
  NFI$tree[ , colnames(NFI$tree) %in% char_col ] <- lapply(NFI$tree[ , colnames(NFI$tree) %in% char_col ], as.character)
  
  
  if("cwd" %in% tables){
    NFI$cwd[ , colnames(NFI$cwd) %in% num_col ] <- lapply(NFI$cwd[ , colnames(NFI$cwd) %in% num_col ], as.numeric)
    NFI$cwd[ , colnames(NFI$cwd) %in% char_col ] <- lapply(NFI$cwd[ , colnames(NFI$cwd) %in% char_col ], as.character)
    NFI$cwd <- left_join(NFI$cwd, Species_DB, by= c("SP") )
  }
  
  
  if("stump" %in% tables){
    NFI$stump[ , colnames(NFI$stump) %in% num_col ] <- lapply(NFI$stump[ , colnames(NFI$stump) %in% num_col ], as.numeric)
    NFI$stump[ , colnames(NFI$stump) %in% char_col ] <- lapply(NFI$stump[ , colnames(NFI$stump) %in% char_col ], as.character)
    NFI$stump <- left_join(NFI$stump, Species_DB, by= c("SP") )
  }
  
  
  if("sapling" %in% tables){
    NFI$sapling <- left_join(NFI$sapling, Species_DB, by= c("SP") )
  }
  
  if("veg" %in% tables){
    NFI$veg <- left_join(NFI$veg, Species_DB, by= c("SP") )
  }
  
  if("herb" %in% tables){
    NFI$herb <- left_join(NFI$herb, Species_DB, by= c("SP") )
  }
  
  
  
  if("tree" %in% tables){
    
    NFI$tree <- NFI$tree[!(names(NFI$tree) %in% c("CONDEC_CLASS", "WDY_PLNTS_TYP"))]
    
    ## Species_DB --------------------------------------------------------------
    NFI$tree <- left_join(NFI$tree, Species_DB, by= c("SP") )
    
    # FORTYP based on basal area (subplot)  --------------------------------------------------------------
    NFI$tree$basal_area <- (pi*(NFI$tree$DBH/2)^2)/10000
    
    stand_sub <- NFI$tree %>% filter(LARGEP_TREE == 0) 
    stand_sub <- stand_sub %>%  
      mutate(deciduous_ba = ifelse(CONDEC_CLASS_CD == 1,  basal_area, 0)) %>% # deciduous
      group_by(SUB_PLOT, CYCLE) %>% 
      summarise(all_ba = sum(basal_area), 
                deciduous_ba = sum(deciduous_ba),
                .groups = 'drop')
    
    stand_sub$percent <- (stand_sub$deciduous_ba/stand_sub$all_ba) *100
    stand_sub$FORTYP_SUB <- ifelse(stand_sub$percent>=75, "Deciduous", 
                                   ifelse(stand_sub$percent>25, "Mixed", "Coniferous"))
    
    
    domin <- NFI$tree %>% filter(LARGEP_TREE ==0) 
    domin <- domin %>%
      group_by(SUB_PLOT, CYCLE,  SP) %>%
      summarise(domin_ba = sum(basal_area), .groups = 'drop') %>%
      group_by(SUB_PLOT, CYCLE) %>%
      arrange(desc(domin_ba)) %>%
      slice(1) %>%
      ungroup()
    
    
    stand_sub <- left_join(stand_sub, domin, by= c("SUB_PLOT","CYCLE"))
    stand_sub$DOMIN_PERCNT_SUB <- (stand_sub$domin_ba/stand_sub$all_ba) *100
    stand_sub$DOMIN_SP_SUB <- stand_sub$SP
    
    condition <- (names(stand_sub) %in% c("SUB_PLOT","CYCLE", "FORTYP_SUB", "DOMIN_SP_SUB", "DOMIN_PERCNT_SUB"))
    NFI$plot <- left_join(NFI$plot, stand_sub[condition], by= c("SUB_PLOT","CYCLE"))
    
    
    # FORTYP based on basal area (clusterplot)
    stand_clust <- NFI$tree %>% filter(LARGEP_TREE == 0) 
    stand_clust <- stand_clust %>%
      mutate(deciduous_ba = ifelse(CONDEC_CLASS_CD == 1,  basal_area, 0)) %>%
      group_by(CLST_PLOT, CYCLE) %>% 
      summarise(all_ba = sum(basal_area), 
                deciduous_ba = sum(deciduous_ba),
                .groups = 'drop')
    
    stand_clust$percent <- (stand_clust$deciduous_ba/stand_clust$all_ba) *100
    stand_clust$FORTYP_CLST <- ifelse(stand_clust$percent>=75, "Deciduous", 
                                      ifelse(stand_clust$percent>25, "Mixed", "Coniferous"))
    
    
    domin <- NFI$tree %>% filter(LARGEP_TREE == 0) 
    domin <- domin %>%
      group_by(CLST_PLOT, CYCLE,  SP) %>%
      summarise(domin_ba = sum(basal_area), .groups = 'drop') %>%
      group_by(CLST_PLOT, CYCLE) %>%
      arrange(desc(domin_ba)) %>%
      slice(1) %>%
      ungroup()
    
    stand_clust <- left_join(stand_clust, domin, by= c("CLST_PLOT","CYCLE"))
    stand_clust$DOMIN_PERCNT_CLST <- (stand_clust$domin_ba/stand_clust$all_ba) *100
    stand_clust$DOMIN_SP_CLST <- stand_clust$SP
    
    condition <- (names(stand_clust) %in% c("CLST_PLOT","CYCLE", "FORTYP_CLST", "DOMIN_SP_CLST", "DOMIN_PERCNT_CLST"))
    
    
    
    NFI$plot <- left_join(NFI$plot, stand_clust[condition], by= c("CLST_PLOT","CYCLE"))
    
  }
  
  
  return(NFI) 
  
  
}


