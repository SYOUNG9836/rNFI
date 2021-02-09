#' readNFI() Function
#'
#' This function read NFI data
#' @param dir : dir
#' @keywords readNFI
#' @export
#' @examples
#' readNFI()



readNFI <- function(dir){


  filenames <- list.files(path=dir, pattern="xlsx")
  data <- vector("list", length = length(filenames))


  for(i in 1:length(filenames)){


    General_info <- readxl::read_excel(paste(dir, filenames[i], sep = "/"), sheet=1, col_names=TRUE, col_types = c())
    Non_forest <-readxl:: read_excel(paste(dir, filenames[i], sep = "/"), sheet=2, col_names=TRUE, col_types = "text")
    Stand_inve <- readxl::read_excel(paste(dir, filenames[i], sep = "/"), sheet=3, col_names=TRUE, col_types = "text")
    Tree_inve <- readxl::read_excel(paste(dir, filenames[i], sep = "/"), sheet=4, col_names=TRUE, col_types = "text")


    data_merge <- merge(x=Tree_inve, y=General_info, by=c('집락번호','표본점번호','조사차기'), all.x=TRUE)
    data_merge <- merge(x=data_merge, y=Non_forest, by=c('집락번호','표본점번호','조사차기','조사연도'), all.x=TRUE)
    data_merge <- merge(x=data_merge, y=Stand_inve, by=c('집락번호','표본점번호','조사차기','조사연도','임상코드','임상'), all.x=TRUE)


    data[[i]] <- data_merge

  }


  NFI <- do.call(rbind, data)

  return(NFI)

}

