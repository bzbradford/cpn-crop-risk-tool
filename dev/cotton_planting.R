library(tidyverse)

# message and print an object to the console for testing
echo <- function(x) {
  message(deparse(substitute(x)), " <", paste(class(x), collapse = ", "), ">")
  str(x)
}

# Logistic function to convert logit to probability
logistic <- function(logit) exp(logit) / (1 + exp(logit))

#' Cotton planting risk model
#' @param pythium present, T/F user input
#' @param planting_pop ~30,000 - 50,000, user input
#' @param MeanMinTemp9ma 9-day moving average of daily minimum air temperatures (C)
#' @param TotPrecip3day 3-day future/forecast total precipitation (mm)
cotton_planting_model <- function(
  MeanMinTemp9ma,
  Precip3day,
  pythium,
  planting_pop,
  year,
  limiting_stand = 15000
) {
  # captured from inputs or date
  py <- pythium # pythyium T/F
  pp <- planting_pop # planting population

  # constants
  yr <- year # year, derived from date
  s_crit <- limiting_stand # yield-limiting stand, emerged plants per acre

  # 4.	Generate the predicted emergence from the following formula:
  mu <- -22.4611 +
    -0.3359 * py +
    0.01118 * yr +
    -0.00750 * Precip3day +
    0.03929 * MeanMinTemp9ma

  # 5.	Perform the logit transformation to generate the proportion of emerged seedlings and the confidence interval around the prediction.
  p_emerge <- logistic(mu)

  # 6.	Scale the predictors on the response scale. Multiply the 1x5 X matrix shown below to get the 1x5 g matrix.
  g_mat <- (p_emerge * (1 - p_emerge)) *
    matrix(c(1, py, yr, Precip3day, MeanMinTemp9ma), ncol = 5)
  # echo(g_mat)

  # 7.	Variance-Covariance matrix (VCOV) is needed to calculate the confidence intervals around the prediction. It is fixed based on the model fit and will not change based on new user input.
  # fmt: skip
  cov_mat <- matrix(
    byrow = TRUE, nrow = 5,
    data = c(
      89.9570756007,  9.339412e-04, -4.501716e-02,  1.145290e-03,  2.718934e-02,
      9.339412e-04,  2.248671e-02, -1.077674e-05, -2.326803e-05,  1.823261e-05,
      -4.501716e-02, -1.077674e-05,  2.253921e-05, -6.018538e-07, -1.454077e-05,
      1.145290e-03, -2.326803e-05, -6.018538e-07,  4.905222e-06,  1.451229e-06,
      2.718934e-02,  1.823261e-05, -1.454077e-05,  1.451229e-06,  1.518877e-04
    )
  )
  # echo(cov_mat)

  # 8.	Multiply the g matrix by the variance-covariance matrix (VCOV) and then by the transposed g matrix (gT). The result is the variance around the prediction.
  var <- as.numeric(g_mat %*% cov_mat %*% t(g_mat))
  # echo(var)

  # 9.	Take the square root of the variance around the prediction (Vη) to calculate the standard error around the mean prediction (SEη)
  # TODO: NaNs being produced here under certain conditions
  se <- sqrt(var)
  # echo(se)

  # 10.	Compute the 95% confidence interval around the prediction by multiplying the standard error (SEη) by 1.959964 and adding or subtracting the prediction (Pemerge)
  se_mult <- 1.959964
  ci_upper <- p_emerge + se * se_mult
  ci_lower <- p_emerge - se * se_mult

  # 11.	Pemerge, CIupper,  CIlower are proportions. Therefore, multiply them by the planting population (PP) to convert the prediction and confidence interval to predicted number of emerged plants.
  pred_emerge <- p_emerge * pp
  pred_emerge_up <- ci_upper * pp
  pred_emerge_low <- ci_lower * pp
  # echo(pred_emerge)

  # 12.	Calculate the probability of going below the stand threshold of 15,000 plants per acre assuming a normal distribution with the mean and standard deviation defined by the predicted mean (Pemerge) and the standard deviation defined by the standard deviation on the emerged plant scale (SD).
  sd <- (pred_emerge_up - pred_emerge_low) / (2 * se_mult)
  p_below <- pnorm((s_crit - pred_emerge) / sd)

  p_below
}

# single value
cotton_planting_model(
  MeanMinTemp9ma = 5,
  Precip3day = 50,
  pythium = TRUE,
  planting_pop = 40000,
  year = 2026
)

# run a full grid of values
res <- expand_grid(
  MeanMinTemp9ma = 1:10,
  Precip3day = 0:10 * 10,
  pythium = c(TRUE, FALSE),
  planting_pop = c(30, 35, 40, 45, 55) * 1000,
  year = 2020:2030
) |>
  rowwise() |>
  mutate(
    prob = cotton_planting_model(
      MeanMinTemp9ma,
      Precip3day,
      pythium,
      planting_pop,
      year
    )
  ) |>
  mutate(
    risk = case_when(
      prob >= 0.35 ~ "high",
      prob >= 0.2 ~ "medium",
      prob > 0 ~ "low",
      TRUE ~ "none"
    ) |>
      factor(levels = c("high", "medium", "low", "none"))
  )

# which values of inputs produced nans?
nrow(res)
nrow(filter(res, !is.nan(prob)))
hist(res$prob)
local({
  nans <- res |> filter(is.nan(prob))
  for (nm in names(nans)) {
    cat(sprintf("%s: %s", nm, paste(unique(nans[[nm]]), collapse = ", ")), "\n")
  }
})
