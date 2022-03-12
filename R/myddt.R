library(dplyr)
library(ggplot2)

#' "My DDT" function
#'
#' Creates a ggplot of the fish SPECIES, length vs weight, coloured by river.
#' Saves the data, filtered to a specific species, to the working directory.
#' Creates a list with three attributes:
#'
#' \describe{
#'   \item{pre_subset}{the data frame passed to the function (the `df` parameter).}
#'   \item{post_subset}{the data frame filtered by `SPECIES`.}
#'   \item{river_freq}{a frequency table of `RIVER`s for the given `SPECIES`.}
#' }
#'
#' @param df - the data frame. This should be ddt.
#' @param SPECIES - the species of fish to filter for.
#'
#' @return A LENGTH vs WEIGHT plot and a named list with the pre- and post-filtering data frame as well as a frequency table of rivers.
#' @export
#'
#' @examples
#' \dontrun{myddt(df=ddt, SPECIES="CCATFISH")}
myddt <- function (df, SPECIES) {
  RIVER <-  WEIGHT <- LENGTH <- NULL # solve the global variable issue
  filtered <- df %>% filter(SPECIES == {{SPECIES}})
  g <- ggplot(filtered, aes_string(x="LENGTH", y="WEIGHT")) +
    geom_point(aes_string(color="RIVER")) +
    geom_smooth(formula= y ~ x + I(x^2), method="lm") +
    ggtitle("Sam Bird - filtered DDT plot")
  print(g)

  filename <- paste("LvsWfor", SPECIES, ".csv", sep="")
  write.csv(filtered, filename)

  output <- list(df, filtered, table(df$RIVER)/length(df$RIVER))
  names(output) <- c("pre_subset", "post_subset", "river_freq")

  return(output)
}
