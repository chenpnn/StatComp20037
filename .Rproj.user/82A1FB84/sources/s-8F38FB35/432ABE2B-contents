#view the missing data
#' @title view the pattern of missing data
#' @name miss.view
#' @param data the matrix with missing data
#' @param type the form of the output
#' @return the barplot or heatplot of the missing data
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics barplot
#' @useDynLib StatComp20037
#' @examples
#' \dontrun{
#' data <- matrix(c(1, NA, 3, 4, 5, 6), ncol = 2)
#' res <- miss.view(data, type='heat')
#' }
#' @export
miss.view <- function(data, type='bar'){
  if (type == 'bar'){
    miss.num <- apply(is.na(data), 2, sum)
    barplot(miss.num, col='red')
  } else if (type == 'heat') {
    data <- apply(is.na(data), 2, as.numeric)
    heatmap(data,  Rowv = NA, Colv = NA,
            col = colorRampPalette(colors = c("white",'white',"red"))(100))
  } else {
    print('error: the type should be bar or heat')
  }
}


#' @title imputate the missing data
#' @name miss.imp
#' @param data the matrix with missing data
#' @param method the method used to imputate the missing data
#' @return a complete matrix with the missing data imputated
#' @import stats
#' @useDynLib StatComp20037
#' @examples
#' \dontrun{
#' data <- matrix(c(1, NA, 3, 4, 5, 6), ncol = 2)
#' res <- miss.imp(data, method='reg')
#' }
#' @export
miss.imp <- function(data, method='mean'){
  requireNamespace('stats')
  m <- nrow(data)
  n <- ncol(data)
  #imputate with mean
  if (method == 'mean'){
    for (j in 1:n){
      data[,j][is.na(data[,j])] <- mean(data[,j], na.rm = TRUE)
    }
  }
  # imputate with median
  else if (method == 'median'){
    for (j in 1:n){
      data[,j][is.na(data[,j])] <- median(data[,j], na.rm = TRUE)
    }
  }
  #imputate with reg
  else if (method == 'reg'){
    s <- which(is.na(data)==TRUE) # the place of missing data
    x <- s%%m # the rows of the missing data
    y <- 1 + s %/% m # the columns of the missing data
    
    cor <- cor(data, use = "complete.obs") # correlation between columns
    for (j in 1:n){
      miss.y <- data[,j] #response y of lm
      i <- order(abs(cor[j,]))[n-1]
      miss.x <- data[,i] #variable x of lm
      res <- lm(miss.y~miss.x)
      imp <- res$coefficients[1] + res$coefficients[2]*miss.x
      # imputation with lm prediction
      for (i in 1:m){
        if (is.na(miss.y[i])==TRUE & is.na(imp[i])==TRUE){
          miss.y[i] <- mean(miss.y)
        } else if (is.na(miss.y[i])==TRUE & is.na(imp[i])==FALSE){
          miss.y[i] <- imp[i]
        } else {
          miss.y[i] <- miss.y[i]
        }
      }
      data[,j] <- miss.y
    }
  }
  else {
    data <- 'error: the method should be mean, median or reg'
  }
  return(data)
}
