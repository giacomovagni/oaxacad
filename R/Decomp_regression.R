#' Generate Simulated Data for Oaxaca Decomposition
#' @name Decomp_Regression
#' @param oax_obj a oaxaca object
#' @returns A list
#' @examples
#' dff = oaxaca_data()
#' output = oaxaca(Y ~ X | group, data = dff$dataframe)
#' Decomp_Regression(output)
#' @export

#
Decomp_Regression = function(oax_obj){

  #
  threeFOLD = oax_obj$threefold$variables %>% as.data.frame() %>% dplyr::select(endowments = `coef(endowments)`, coef = `coef(coefficients)`, inter = `coef(interaction)`)

  reg1 = tidy(oax_obj$reg$reg.A) %>% dplyr::select(est_1 = estimate, SE_1 = std.error, pv_1 = p.value)
  reg2 = tidy(oax_obj$reg$reg.B) %>% dplyr::select(est_2 = estimate, SE_2 = std.error, pv_2 = p.value)

  df = data.frame(reg1, reg2, X_1 = oax_obj$x$x.mean.A, X2 = oax_obj$x$x.mean.B, X_diff = oax_obj$x$x.mean.diff, threeFOLD)

  #
  df = rbind(df, colSums(df))
  #
  df$sum = df$endowments + df$coef + df$inter
  #
  df = apply(df, MARGIN = 2, FUN = function(x) round(x, 3))
  #

  #
  df[nrow(df), 1:9] <- NA
  #

  #
  df = as.data.frame(df)
  #
  #
  explained = round(df$endowments[nrow(df)] / df$sum[nrow(df)], 3)
  unexplained1 = round(df$coef[nrow(df)] / df$sum[nrow(df)], 3)
  unexplained2 = round(df$inter[nrow(df)] / df$sum[nrow(df)], 3)
  #

  # 9 columns
  df = rbind(df, c(rep(NA, 9), explained, unexplained1, unexplained2, 1))
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

  #
  colnames(df) = c("variable","β1", "β1(SE)", "β1(pvalue)", "β2", "β2(SE)", "β2(pvalue)", "X1", "X2", "X1-X2", "ΔXβ [endowment]", "ΔβX [coef]", "ΔβΔX","Σ")
  #

  # df[is.na(df)] <- ''
  # message("ΔXβ [endowment] keeping constant the coefficient for group of reference, we compute the difference in the X characteristics" )

  #
  return(df)
  #
}
#
