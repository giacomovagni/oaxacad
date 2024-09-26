
#' Generate Simulated Data for Oaxaca Decomposition
#' @name Decomp_simple
#' @param results a oaxaca object
#' @returns A list
#' @examples
#' dff = oaxaca_data()
#' output = oaxaca(Y ~ X | group, data = dff$dataframe)
#' Decomp_simple(output)
#' @export

#
Decomp_simple = function(results, omega = 0){

  #
  overall_res = data.frame(results$twofold$overall)
  overall_res = overall_res[overall_res$group.weight == omega, ]
  # Oaxaca # Omega = 0 #
  rb = overall_res
  #

  #
  oaxaca = data.frame(explained = rb[2], se_ex = rb[3], unexplained = rb[4], se_unex = rb[5])
  #

  #
  tex = oaxaca$coef.explained. / oaxaca$se.explained.
  tunex =  oaxaca$coef.unexplained. / oaxaca$se.unexplained.

  pvalue1 = pnorm( abs(tex), lower.tail = F)
  pvalue2 = pnorm( abs(tunex), lower.tail = F)
  #

  #
  oaxaca$pvalue_ex = stars.pval(pvalue1)
  oaxaca$pvalue_unex = stars.pval(pvalue2)
  #

  oaxaca$gap = oaxaca$coef.explained. + oaxaca$coef.unexplained.
  oaxaca$perc_explained = (oaxaca$coef.explained. / oaxaca$gap)*100

  # rearrange #
  oxx = oaxaca[,c(7,1,5,3,6,8)]
  #

  oxx$gap = round(oxx$gap, 5)
  oxx$coef.explained. = round(oxx$coef.explained., 5)
  oxx$coef.unexplained. = round(oxx$coef.unexplained., 5)

  #
  colnames(oxx) = c('gap', 'Explained', 'pvale', 'Unexplained', 'pvalu', '% explained')

  oxx$Explained = paste(oxx$Explained, oxx$pvale)
  oxx$Unexplained = paste(oxx$Unexplained, oxx$pvalu)

  oxx = oxx[, c(1,2,4,6)]

  return(oxx)
}

