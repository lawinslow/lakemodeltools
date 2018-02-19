#' @title Run optimizer on GLM model
#'
#' @description
#'
#'
#' @inheritParams run_glm_nml
#' @param init_params Named list of initial paramters.
#' The parameters optimized are based on this list.
#'
#' @param field_file Path to observation file to run optimizations
#' against. Must match format used by \code{\link[glmtools]{resample_to_field}}.
#'
#' @param method Optimization method to be used. See details for more info
#'
#' @param ... Remaining paramters are passed on to optimization function
#' (depending on selection).
#'
#' @details
#' Currently supported optim routines. 'nelder-mead', 'metropolis'.
#'
#'
#'
#' @export
run_glm_optim = function(nml, drivers, dest, init_params, field_file, method=c('differential-evolution', 'nelder-mead', 'metropolis'), ...){

  method = match.arg(method)

  #prepare environment and NML
  param_names = names(init_params)
  if(is.null(param_names)){
    stop('init_params must be a named vector of starting paramter names and values')
  }

  #set initial params in NML
  nml = set_nml(nml, arg_list = as.list(init_params))

  #check for existence of dest folder
  if(!file.exists(dest)){
    dir.create(dest, recursive = TRUE)
  }

  #' @title loss function
  #' @noRd
  #' @description Returns Negative Log Likelihood or RMSE (default)
  #' for the GLM run. Applies simple constraint that no parameter
  #' can be less than or equal to (<=) zero.
  #' May not always apply, but good to get started.
  #'
  loss_fun = function(params, retval='rmse'){

    if(any(params <= 0)){
      return(Inf)
    }

    #many of the optimization routines clobber vector names, so
    # forcing them back on here
    names(params) = param_names

    tryCatch({
      nml = set_nml(nml, arg_list = as.list(params))

      res = run_glm_nml(nml, dest = dest, drivers = drivers)

      modobs = resample_to_field(res$ncfile, field_file, var_name = 'temp')
      if(retval == 'nll'){
        res = na.omit(modobs[,3] - modobs[,4])
        sigma2 <- sum(res^2)/length(res)
        #NLL <- 0.5*((SSE/sigma2) + nRes*log(2*pi*sigma2))
        NLL = -sum(dnorm(modobs[,4], modobs[,3], sd=sqrt(sigma2), log=TRUE), na.rm=TRUE)
        return(NLL)
      }else if(retval == 'rmse'){
        return(RMSE(modobs[,3:4]))
      }else{
        stop('unknown `retval`')
      }

    }, error=function(e){
      message(e)
      return(Inf)
      })

  }

  # fit different models based on what was requested
  if(method == 'nelder-mead'){
    # Set parameter scale to deal with large difference in parameter magnitude (e.g., factors vs ce, ch, others << 1)
    final_mod = optim(init_params, fn = loss_fun, method = 'Nelder-Mead', control=list(maxit=2000, parscale=init_params/4), ...)

  }else if(method == 'metropolis'){
    #metrop, maximize likelihood
    final_mod = mcmc::metrop(function(x)-10*loss_fun(x, retval='rmse'), init_params, nbatch=100,
                          blen=10, scale=init_params/10, debug=TRUE)

  }else{
    stop('Method currently unsupported')
  }

  return(final_mod)
}

