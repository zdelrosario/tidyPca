#' PCA in a tidyverse framework
#'
#' A simple wrapper for pca on a data frame
#' @param data input data frame
#' @param ... variables to include in PCA
#' @param doCenter passed to prcomp, center the data? TRUE / FALSE
#' @param doScale passed to prcomp, scale the data? TRUE / FALSE
#' @return nested tibble with entries:\cr
#'   pca        = prcomp() output\cr
#'   data_aug   = original data with bound PC scores\cr
#'   pc_weights = tibble of PCA vector weights\cr
#'   pc_frac    = tibble of PCA eigenvalues and their cumulative (normalized) sum\cr
#' @keywords pca, tibble
#' @seealso \code{\link{prcomp}} which this function wraps
#' @export
#' @examples
#' df_pca <-
#'   iris %>%
#'   tidy_pca(
#'     Sepal.Length,
#'     Sepal.Width,
#'     Petal.Length,
#'     Petal.Width
#'   )
#'
#' df_pca %>%
#'   unnest(pc_frac) %>%
#'   knitr::kable()
#'
#' df_pca %>%
#'   unnest(data_aug) %>%
#'   ggplot(aes(PC1, PC2, color = Species)) +
#'   geom_point()

tidy_pca <- function(data, ..., doCenter = TRUE, doScale = TRUE) {
  group_var <- dplyr::quos(...)

  data %>%
    tidyr::nest() %>%
    dplyr::mutate(
      pca = purrr::map(data, ~ stats::prcomp(
        .x %>%
          dplyr::select(!!! group_var),
        center = doCenter,
        scale = doScale
      )),
      data_aug = purrr::map2(
        pca,
        data,
        ~ broom::augment(.x, data = .y) %>%
          dplyr::select(-.rownames) %>%
          dplyr::rename_all(~stringr::str_replace(., "\\.fitted", ""))
      ),
      pc_weights = purrr::map(
        pca,
        ~ .$rotation %>%
          dplyr::as_tibble(rownames = "var")
      ),
      pc_frac = purrr::map(
        pca,
        ~ .$sdev %>%
          dplyr::tibble(sig = ., sig_frac = normsum(.))
      )
    ) %>%
    dplyr::select(-data)
}
