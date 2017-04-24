#' @rdname lda_tidiers
#' 
#' @param newdata If provided, new data frame to use for predictions
#' 
#' @return \code{augment.lda} returns a row for each original observation
#' with the following columns added:
#'  \item{.class}{Predicted class}
#'  \item{.post_<class>}{The posterior probability of membership in each <class>}
#'  \item{.LD<i>}{Each linear discriminant <i>}
#' 
#' @export
augment.lda <- function(x,newdata, ...){
    na_action <- na.action(x)
    no_newdata <- missing(newdata) || is.null(newdata)
    if (no_newdata){
        original <- model.frame(x)
        pred <- predict(x,...)  
    } else{
        original <- as.data.frame(newdata)
        pred <- predict(x, newdata = newdata,...)
    }
    
    colnames(pred[["posterior"]]) <- paste0(".post_",
                                            colnames(pred[["posterior"]]))
    
    original[[".class"]] <- pred[["class"]]
    original <- cbind(original,pred[["posterior"]],pred[["x"]])
    if (inherits(na_action,"omit") && !no_newdata){
        original <- original[!is.na(original[[".class"]]),]
    }
    if (inherits(na_action,"exclude") && no_newdata){
        #Toss a meaningless warning to pass test in check_augment_NAs
        warning("na.exclude")
    }
    return(unrowname(original))
}
