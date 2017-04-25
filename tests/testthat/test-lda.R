if (require("MASS",quietly = TRUE)){
    context("lda augment")
    lda_func <- function(.data, ...) lda(Species ~ ., .data, ...)
    iris_test <- iris %>%
        group_by(Species) %>%
        sample_n(size = 5)
    check_augment_NAs(lda_func,iris_test , "Sepal.Length")
}
