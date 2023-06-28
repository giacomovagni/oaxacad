#' Generate Simulated Data for Oaxaca Decomposition
#' @name Decomp_Regression
#' @param oax_obj a oaxaca object
#' @returns A list
#' @examples
#' #' Only differences in characteristics (endowments)
#' dff = oaxaca_data(interceptA_avr = 100, interceptB_avr = 100, betaA = 5, betaB = 5, xA_avr = 20, xB_avr = 5)
#' oax_obj = oaxaca(Y ~ X | group, data = dff$dataframe)
#' Decomp_Regression(oax_obj)
#'
#' Only differences in intercepts
#' dff = oaxaca_data(interceptA_avr = 115, interceptB_avr = 100, betaA = 5, betaB = 5, xA_avr = 10, xB_avr = 10)
#' oax_obj = oaxaca(Y ~ X | group, data = dff$dataframe)
#' Decomp_Regression(oax_obj)
#'
#' Differences in intercepts and coefficients
#' dff = oaxaca_data(interceptA_avr = 115, interceptB_avr = 100, betaA = 5, betaB = 5, xA_avr = 10, xB_avr = 10)
#' oax_obj = oaxaca(Y ~ X | group, data = dff$dataframe)
#' Decomp_Regression(oax_obj)
#' @export

#
Decomp_Regression = function(oax_obj){

  oax_obj$twofold
  oax_obj$threefold

  #
  threeFOLD = oax_obj$threefold$variables %>% as.data.frame() %>%
    dplyr::select(endowments = `coef(endowments)`, coef = `coef(coefficients)`, inter = `coef(interaction)`)

  reg1 = broom::tidy(oax_obj$reg$reg.A) %>% dplyr::select(est_1 = estimate, SE_1 = std.error, pv_1 = p.value)
  reg2 = broom::tidy(oax_obj$reg$reg.B) %>% dplyr::select(est_2 = estimate, SE_2 = std.error, pv_2 = p.value)

  beta_diff = reg1$est_1-reg2$est_2

  df = data.frame(reg1, reg2, beta_diff, X_1 = oax_obj$x$x.mean.A, X2 = oax_obj$x$x.mean.B, X_diff = oax_obj$x$x.mean.diff, threeFOLD)

  #
  df = rbind(df, colSums(df))
  #
  df$sum = df$endowments + df$coef + df$inter
  #
  df = apply(df, MARGIN = 2, FUN = function(x) round(x, 3))
  #

  #
  df[nrow(df), 1:10] <- NA
  #

  #
  df = as.data.frame(df)
  #
  # df
  #
  explained = round(df$endowments[nrow(df)] / df$sum[nrow(df)], 3)
  unexplained1 = round(df$coef[nrow(df)] / df$sum[nrow(df)], 3)
  unexplained2 = round(df$inter[nrow(df)] / df$sum[nrow(df)], 3)
  #

  unexplained1 + unexplained2

  # 10 columns
  df = rbind(df, c(rep(NA, 10), explained, unexplained1, unexplained2, 1))
  #

  #
  df$var = rownames(df)
  #
  df = df %>% dplyr::select(var, everything())
  #
  rownames(df) = 1:nrow(df)
  df[nrow(df)-1, 1] <- 'decomposition'
  df[nrow(df), 1] <- 'decomposition (\\%)'
  #

  df
  #
  colnames(df) = c("variable","β1", "β1(SE)", "β1(pvalue)", "β2", "β2(SE)", "β2(pvalue)", "β1-β2", "X1", "X2", "X1-X2", "ΔXβ [E]", "ΔβX [coef]", "ΔβΔX","Σ (gap)")
  #

  # df[is.na(df)] <- ''
  # message("ΔXβ [endowment] keeping constant the coefficient for group of reference, we compute the difference in the X characteristics" )

  message("Group 2 used as reference")
  message("ΔXβ [endowment] = (X1-X2)β2" )
  message("ΔβX [coef] =  (β1-β2)X2" )
  message("ΔβΔX = interaction" )

  message("Total gap = ", df$`Σ (gap)`[nrow(df)-1])

  message("Explained = endowment / gap" )
  message("Explained = ",  df$`ΔXβ [E]`[nrow(df)], "%")

  message("If people in Group 2 had the same measured characteristics as those in Group 1, we WOULD have observed ", df$`ΔXβ [E]`[nrow(df)-1],
          if(df$`ΔXβ [E]`[nrow(df)-1] < 0) sign = " less " else sign = " more ", "of Y")

  #message("Unexplained Share of Difference in Intercepts = ", round(df$`Σ (gap)`[1] / df$`Σ (gap)`[3], 2))
  #message("Unexplained Share of Difference in Coefficients = ", roun(df$`Σ (gap)`[2] / df$`Σ (gap)`[3], 2))

  return(df)
  #
}
#
