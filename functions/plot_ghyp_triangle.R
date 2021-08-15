plot_ghyp_triangle <- function(.prices, .returns_period) {

  if (!is.numeric(.returns_period)) {
    stop("`.returns_period` must be a numeric vector.")
  }

  fitted_points <- tibble::tibble(.lags = .returns_period, data = list(.prices)) %>%
          dplyr::transmute(purrr::map2(.x = .data$.lags, .y = .data$data, .f = ~ .y / dplyr::lag(x = .y, n = .x) - 1)) %>%
          tibble::deframe(.) %>%
          dplyr::bind_cols() %>%
          stats::na.omit() %>%
          purrr::map(ghyp::fit.hypuv, symmetric = FALSE, silent = TRUE) %>%
          purrr::map(xichi) %>%
          purrr::reduce(dplyr::bind_rows)

  # Text that will be feeded into the triangule
  gg_text <- tibble::tibble(
    x = c(-1.0, 0.0, 1.0, 0.7, 0.0, -0.7),
    y = c(1.1 , 1.1, 1.1, 0.4, -0.1, 0.4),
    text = c('Exponencial', 'Laplace', 'Exponential', 'Hyperbolic', 'Normal', 'Hyperbolic')
  )

  fitted_points %>%
    dplyr::mutate(Periodicity =
                    dplyr::if_else(condition = .returns_period %in% 1,
                                   true      = c('1 day', stringr::str_c(.returns_period[2:length(.returns_period)], ' days')),
                                   false     = stringr::str_c(.returns_period, ' day')) |>
                    forcats::as_factor()) |>
    dplyr::select(.data$Periodicity, .data$chi, .data$xi) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$chi, y = .data$xi, color = .data$Periodicity)) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = -1, yend = 1), color = '#9F9573') +
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 1, yend = 1), color = '#9F9573') +
    ggplot2::geom_segment(ggplot2::aes(x = -1, y = 1, xend = 1, yend = 1), color = '#9F9573') +
    ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 0, yend = 1), color = '#9F9573', linetype = 2) +
    ggplot2::geom_text(data = gg_text, ggplot2::aes(x = .data$x, y = .data$y, label = .data$text), inherit.aes = FALSE, color = 'black', family = "mono") +
    ggplot2::geom_point() +
    ggplot2::geom_point(ggplot2::aes(size = 1), show.legend = FALSE) +
    ggplot2::ylim(-0.2, 1.2) +
    ggplot2::xlim(-1.2, 1.2) +
    ggplot2::labs(x = "Assimetry",
                  y = "Kurtosis") +
    ggplot2::theme(legend.position = "none", panel.background = element_rect(fill = "white")) +
    ggplot2::scale_color_manual(values = c('#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58'))

}
