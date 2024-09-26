#' Generate Simulated Data for Oaxaca Decomposition
#' @name oaxaca_sim
#' @inheritParams oaxaca_data
#' @returns A list
#' @examples
#' test = oaxaca_sim(betaA = 5, betaB = 6)
#' # the simulated data
#' test$simulated_data$dataframe
#' # the figure
#' test$simulated_data$fig
#' # The oaxaca object from the oaxaca package
#' test$oaxaca_obj
#' @export

#
oaxaca_sim = function(interceptA_avr = 100,
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

  df = oaxaca_data(interceptA_avr,
                   interceptB_avr,
                   betaA,
                   betaB,
                   xA_avr,
                   xA_sd,
                   xB_avr,
                   xB_sd,
                   errorA_avr,
                   errorA_sd,
                   errorB_avr,
                   errorB_sd, N)
  #

  # oaxaca #
  ox = oaxaca::oaxaca(formula = Y ~ X | group, data = df$dataframe)
  #

  #
  decomp_reg = Decomp_Regression(ox)
  decomp_twofold = Decomp(ox, decomp = "twofold")
  decomp_threefold = Decomp(ox, decomp = "threefold")[[5]]
  decomp_simple = Decomp_simple(ox)
  #

  decomp_twofold$var = row.names(decomp_twofold)
  decomp_twofold = decomp_twofold[, c('var', 'Decomposition')]
  #

  #
  return(list(simulated_data = df,
              oaxaca_obj = ox,
              decomp_threefold = decomp_threefold,
              decomp_reg = decomp_reg,
              decomp_twofold = decomp_twofold,
              decomp_simple = decomp_simple))
}
#

