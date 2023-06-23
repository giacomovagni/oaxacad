
#' Generate Simulated Data for Oaxaca Decomposition
#' @name Decomp_info_OaxacaBlinder
#' @param results a oaxaca object
#' @returns A list
#' @examples
#' dff = oaxaca_data()
#' output = oaxaca(Y ~ X | group, data = dff$dataframe)
#' Decomp_info_OB(output)
#' @export


#
Decomp_info_OaxacaBlinder = function(results, full = F, SE = F){

  # Oaxaca #
  rb = rbind(results$twofold$variables[[1]], apply(results$twofold$variables[[1]], 2, sum))
  rb = as.data.frame(rb)
  rb = apply(rb, 2, function(x) round(x,5))

  oaxaca = rb[, c(2,3,4,5)]
  oaxaca[nrow(oaxaca), c(2,4)] <- 0
  oaxaca = as.data.frame(oaxaca)
  #
  oaxaca$sum = rowSums(oaxaca[, c(1,3)])
  # total gap #
  gap_oaxaca = oaxaca$sum[length(oaxaca$sum)]

  # if no SE #
  if(SE == F){
    #
    tex = oaxaca[,1] / oaxaca[,2]
    tunex = oaxaca[,3] / oaxaca[,4]

    pvalue1 = pnorm( abs(tex), lower.tail = F)
    pvalue2 = pnorm( abs(tunex), lower.tail = F)

    #
    oaxaca = oaxaca[c(1,3,5)]
    oaxaca$perc_ex = round( (oaxaca[, 1] / gap_oaxaca) * 100, 2)
    oaxaca$perc_unexp = round( (oaxaca[, 2] / gap_oaxaca) * 100, 2)
    #

    oaxaca$pvalue_ex = stars.pval(pvalue1)
    oaxaca$pvalue_unex = stars.pval(pvalue2)

    # rearrange #
    oaxaca = oaxaca[c(1,4,6,2,5,3,7)]
    #
  }

  message('Endow_effets = Beta0 (X1 - X0)')
  message('Coefficients_effets = X0 (Beta1 - Beta0)')
  message('Total Gap Oaxaca = ', gap_oaxaca)

  # ------------------------------------------------------------------------------------------------------------ #
  # ------------------------------------------------------------------------------------------------------------ #

  # Blinder #
  rb = rbind(results$twofold$variables[[2]], apply(results$twofold$variables[[2]], 2, sum))
  rb = as.data.frame(rb)
  rb = apply(rb, 2, function(x) round(x,2))

  blinder = rb[, c(2,3,4,5)]
  blinder[nrow(blinder), c(2,4)] <- 0
  blinder = as.data.frame(blinder)

  #
  blinder$sum = rowSums(blinder[, c(1,3)])
  # total gap #
  gap_blinder = blinder$sum[length(blinder$sum)]

  # if no SE #
  if(SE == F){
    #
    tex = blinder[,1] / blinder[,2]
    tunex = blinder[,3] / blinder[,4]

    pvalue1 = pnorm( abs(tex), lower.tail = F)
    pvalue2 = pnorm( abs(tunex), lower.tail = F)

    #
    blinder = blinder[c(1,3,5)]
    blinder$perc_ex = round( (blinder[, 1] / gap_blinder) * 100, 2)
    blinder$perc_unexp = round( (blinder[, 2] / gap_blinder) * 100, 2)
    #

    blinder$pvalue_ex = stars.pval(pvalue1)
    blinder$pvalue_unex = stars.pval(pvalue2)

    # rearrange #
    blinder = blinder[c(1,4,6,2,5,3,7)]
    #
  }

  message('Total Gap Blinder = ', gap_oaxaca)
  #

  oaxaca_blinder = list(oaxaca, blinder)

  names(oaxaca_blinder) = c('oaxaca', 'blinder')

  # ------------------------------------------------------------------------------------------------------------ #
  # ------------------------------------------------------------------------------------------------------------ #


  if(full == T){

    #
    CI_composition = as.data.frame( cbind(results$threefold$variables[,1] - results$threefold$variables[,2]*1.96, results$threefold$variables[,1] + results$threefold$variables[,2]*1.96) )
    CI_unexplained = as.data.frame( cbind(results$threefold$variables[,3] - results$threefold$variables[,4]*1.96, results$threefold$variables[,3] + results$threefold$variables[,4]*1.96) )

    tvalue_composition = results$threefold$variables[,1] / results$threefold$variables[,2]
    pvalues_composition = 2 * pnorm( abs(tvalue_composition), lower.tail = F)

    tvalue_unexplained = results$threefold$variables[,3] / results$threefold$variables[,4]
    pvalues_unexplained = 2 * pnorm( abs(tvalue_unexplained), lower.tail = F)

    #
    names(CI_composition) = c('conf.low', 'conf.high')
    names(CI_unexplained) = c('conf.low', 'conf.high')

    # Compositional #
    brows = bind_rows(results$x) %>% as.data.frame()
    rownames(brows) = names(results$x$x.mean.A)

    brows = cbind(brows, CI_composition)
    brows = apply(brows, 2, function(x) round(x, 3))
    brows = as.data.frame(brows)
    # p-value #
    brows$pvalue = stars.pval(pvalues_composition)
    # colnames #
    colnames(brows) = c('Composition_GroupRef', 'Composition_Group1', 'diff_comp', 'conf.low', 'conf.high', 'pvalue_comp')
    #
    brows$variables  = row.names(brows)
    #
    #brows$diff_comp = paste(brows$diff_comp, brows$pvalue_comp, sep = '')
    #
    endow = brows[, c('variables','Composition_GroupRef', 'Composition_Group1', 'diff_comp', 'pvalue_comp')]
    #

    # Coefficients #
    betas = cbind( round(results$beta[[1]], 3), round(results$beta[[2]], 3), round(results$beta[[3]],3) ) %>% as.data.frame()
    #
    betas = cbind(betas, CI_unexplained)
    betas = apply(betas, 2, function(x) round(x, 3))
    betas = as.data.frame(betas)
    # p-value #
    betas$pvalue = stars.pval(pvalues_unexplained)
    # colnames #
    colnames(betas) = c('Coeff_GroupRef', 'Coeff_Group1', 'diff_coeff', 'conf.low', 'conf.high', 'pvalue_coeff')
    #
    #
    betas$variables  = row.names(betas)
    #
    #
    # betas$diff_coeff = paste(betas$diff_coeff, betas$pvalue_coeff, sep = '')
    #
    betas = betas[, c('variables', 'Coeff_GroupRef', 'Coeff_Group1', 'diff_coeff', 'pvalue_coeff')]
    #
    regression = merge(endow, betas, by = 'variables')
    #

    #
    oaxaca_blinder = list(oaxaca, blinder, regression)
    #
    names(oaxaca_blinder) = c('oaxaca', 'blinder', 'estimates')
    #
  }

  return(oaxaca_blinder)
}
