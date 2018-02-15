#' @title Directly Run GLM NML file
#'
#' @description
#' Accepts NML object (and other optional paramters) and
#' quickly sets up a temporary environment and runs the GLM model.
#' Returns data.frame row with paths to relevant model files.
#'
#'
#' @param nml An NML object based on object format
#' used in \code{glmtools}. Required.
#'
#' @param drivers A \code{data.frame} of input drivers. To
#' be saved adjacent to model run (useful for fast temp drives).
#'
#' @param dest Temporary destination for model run. Defaults
#' to a random subfolder of the R \code{tempdir} directory.
#'
#' @import glmtools
#' @import GLMr
#'
#' @export
run_glm_nml = function(nml, drivers, dest){

  if(missing(dest)){
    dest = file.path(tempdir(), paste0('glm-', runif(1, 0, 10^15)))
  }

  if(!file.exists(dest)){
    dir.create(dest, recursive = TRUE)
  }

  if(!missing(drivers)){
    dvrfile = gsub('\\\\', '/', file.path(dest, 'drivers.csv'))
    write.table(drivers, dvrfile, row.names=FALSE, quote=FALSE, sep=',')
    nml = glmtools::set_nml(nml, 'meteo_fl', dvrfile)
  }

  #force file output to local directory
  nml = glmtools::set_nml(nml, 'out_dir', '.')
  nml = glmtools::set_nml(nml, 'out_fn', 'output')

  nmlfile = file.path(dest, 'glm2.nml')
  glmtools::write_nml(glm_nml = nml, file = nmlfile)

  GLMr::run_glm(dest)

  #going to guess at this
  ncfile = file.path(dest, 'output.nc')

  return(data.frame(dest, nmlfile, ncfile))
}
