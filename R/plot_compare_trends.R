# Declare global variables to avoid R CMD check NOTES
utils::globalVariables(c("Date", "Year", "Quarter", "Income_per_capita",
                         "Australia", "Year_inc", "Year_house", "Series", "Index"))


#' Compare two time series on a base-100 index scale (quarterly)
#'
#' Creates a ggplot showing two quarterly series (income and housing)
#' normalized to a base of 100 using a chosen base year (default = 1990).
#' Uses quarterly data for smoother visualization.
#'
#' @param income Data frame containing Date, Year, Quarter, Income_per_capita.
#' @param housing Data frame containing Date, Year, Quarter, Australia (detached house index).
#' @param base_year Year to use as normalization base (default = 1990).
#'
#' @return A ggplot object showing normalized quarterly trends for both series.
#' @examples
#' compare_trends(income_pc, detached_full, base_year = 1990)
#' @export
compare_trends <- function(income, housing, base_year = 1990) {
  df <- dplyr::inner_join(
    income |> dplyr::select(Date, Year, Quarter, Income_per_capita),
    housing |> dplyr::select(Date, Year, Quarter, Australia),
    by = "Date",
    suffix = c("_inc", "_house")
  ) |>
    dplyr::mutate(
      Year = dplyr::coalesce(Year_inc, Year_house)
    ) |>
    dplyr::mutate(
      Income_index = housepack::to_index(Income_per_capita, year = Year, base_year = base_year),
      House_index  = housepack::to_index(Australia, year = Year, base_year = base_year)
    ) |>
    tidyr::pivot_longer(
      cols = c("Income_index", "House_index"),
      names_to = "Series",
      values_to = "Index"
    ) |>
    dplyr::mutate(
      Series = dplyr::recode(Series,
                             Income_index = "Income per capita",
                             House_index  = "Detached house price"),
      Series = factor(Series, levels = c("Income per capita", "Detached house price"))
    )

  ggplot2::ggplot(df, ggplot2::aes(
    x = Date, y = Index, color = Series, linetype = Series
  )) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::scale_linetype_manual(values = c("dashed", "solid")) +
    ggplot2::scale_color_manual(values = c("#6cb4e4", "#1c4c74")) +
    ggplot2::labs(
      title = "Income vs Housing Prices in Australia",
      subtitle = paste0("Indexed to ", base_year, " = 100"),
      y = "Index (Base 100)",
      x = ""
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 700),
      breaks = seq(0, 700, by = 100))+
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      panel.background = ggplot2::element_blank(),
      panel.grid  = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "grey50"),
      legend.title = ggplot2::element_blank(),
      legend.position = "bottom",
      aspect.ratio = 0.5
    )
}
