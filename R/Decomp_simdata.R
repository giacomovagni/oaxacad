#' Generate Simulated Data for Oaxaca Decomposition
#' @import magrittr
#' @import broom
#' @import dplyr
#' @import ggplot2
#' @import oaxaca
#' @import gtools
#' @import stats
#' @export
#' @name oaxaca_data
#' @param interceptA_avr intercept of group A
#' @param interceptB_avr intercept of group B
#' @param betaA Coefficient size of group A
#' @param betaB Coefficient size of group B
#' @param xA_avr Group A characteristics
#' @param xB_avr Group B characteristics
#' @param xA_sd Variation in group A characteristics (SD)
#' @param xB_sd Variation in group B characteristics (SD)
#' @returns A list
#' @examples
#' oaxaca_data(betaA = 5, betaB = 5, xA_avr = 5, xB_avr = 20)

#
oaxaca_data = function(interceptA_avr = 100,
                   interceptB_avr = 105,
                   betaA = 5,
                   betaB = 8,
                   xA_avr = 10,
                   xA_sd = 5,
                   xB_avr = 10,
                   xB_sd = 5,
                   errorA_avr = 0,
                   errorA_sd = 1,
                   errorB_avr = 0,
                   errorB_sd = 1, N = 1000){


  #
  n=N
  #
  # intercepts
  interceptA = rnorm(n, interceptA_avr, 10)
  interceptB = rnorm(n, interceptB_avr, 10)
  # group A has higher values of X
  xA = rnorm(n, mean = xA_avr, sd = xA_sd)
  xB = rnorm(n, mean = xB_avr, sd = xB_sd)
  # residual errors -> into un-explained #
  errorA = rnorm(n, errorA_avr, errorA_sd)
  errorB = rnorm(n, errorB_avr, errorB_sd)
  # Y outcome
  YA = interceptA + xA*betaA + errorA
  YB = interceptB + xB*betaB + errorB
  #
  dA = data.frame(X = xA, Y = YA) %>% mutate(group = 0)
  dB = data.frame(X = xB, Y = YB) %>% mutate(group = 1)
  #
  df = bind_rows(dA,dB)
  #

  #
  df$group = factor(df$group)
  #
  fig = ggplot(data=df, aes(X, Y, colour = group, shape = group, linetype = group)) +
    geom_point() +
    geom_hline(yintercept = mean(YA), col = 'red', linetype = 2) +
    geom_hline(yintercept = mean(YB), col = 'blue4', linetype = 3) +

    geom_vline(xintercept = mean(xA), col = 'red', linetype = 2) +
    geom_vline(xintercept = mean(xB), col = 'blue4', linetype = 3) +

    geom_smooth(method = "lm", se = F, fullrange=TRUE) +
    scale_color_manual(values = c('red', 'blue4')) +
    theme_minimal()
  #
  return(list(dataframe = df, fig = fig))
}
#


