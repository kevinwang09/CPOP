#' @title CPOP internal functions
#' @description Step 3 of the CPOP method
#' @param cpop2_result cpop2 result
#' @param intercept default to FALSE
#' @param ... Extra parameter settings for cv.glmnet
#' @param family see glmnet family
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet coef.glmnet
#' @rdname cpop_internals
#' @return A vector
#' @export
cpop3 = function(z1, z2, y1, y2, cpop2_result, family, intercept, ...){
  if(family == "cox"){
    intercept = TRUE
  }

  if(length(cpop2_result) < 2){

    z1_sub = z1[,cpop2_result]
    z2_sub = z2[,cpop2_result]

    if(family == "cox"){
      stop("Performing single-variable Cox regression is not currently supported by the cpop package.")
    } else {
      cpop_mode = "glm"
      model1 = stats::glm(y1~z1_sub, family = family)
      model2 = stats::glm(y1~z2_sub, family = family)
    }

  } else {

    cpop_mode = "glmnet"
    model1 = glmnet::cv.glmnet(
      x = z1[,cpop2_result],
      y = y1,
      family = family,
      alpha = 0,
      intercept = intercept,
      ...)

    model2 = glmnet::cv.glmnet(
      x = z2[,cpop2_result],
      y = y2,
      family = family,
      alpha = 0,
      intercept = intercept,
      ...)
  }

  result = list(cpop_mode = cpop_mode,
                model1 = model1,
                model2 = model2,
                feature = cpop2_result)
  return(result)
}
