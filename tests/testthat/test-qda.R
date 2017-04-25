if (require("MASS",quietly = TRUE)){
    context("qda augment")
    qda_func <- function(.data, ...) qda(Species ~ ., .data, ...)
    iris_test <- iris %>%
        group_by(Species) %>%
        sample_n(size = 20)
    check_augment_NAs(qda_func,iris_test , "Sepal.Length")
}
