#' Supported families 
#' @export
     get_available_glmm_families <- function() { 
       c("poisson","nbinom1","nbinom2","compois","genpois", 
       "truncated_poisson","truncated_nbinom1","truncated_nbinom2", 
       "gaussian","Gamma","lognormal", 
       "beta","betabinomial","tweedie") 
       } 
       
#' Family constructor
#' @export
make_glmm_family <- function(name, link = "log") {
  switch(name,
    "poisson" = poisson(link = link),
    "nbinom1" = glmmTMB::nbinom1(link = link),
    "nbinom2" = glmmTMB::nbinom2(link = link),
    "compois" = glmmTMB::compois(link = link),
    "genpois" = glmmTMB::genpois(link = link),
    "truncated_poisson" = glmmTMB::truncated_poisson(link = link),
    "truncated_nbinom1" = glmmTMB::truncated_nbinom1(link = link),
    "truncated_nbinom2" = glmmTMB::truncated_nbinom2(link = link),
    "gaussian" = gaussian(link = link),
    "Gamma" = Gamma(link = link),
    "lognormal" = glmmTMB::lognormal(link = link),
    "beta" = glmmTMB::beta_family(link = link),
    "betabinomial" = glmmTMB::betabinomial(link = link),
    "tweedie" = glmmTMB::tweedie(link = link),
    stop(paste("Unsupported family:", name))
  )
}

#' GLMM runner 
#' @export
     run_glmm <- function(data, count_cols, model_cfg) {       
       family_obj <- make_glmm_family( 
         model_cfg$family, 
         model_cfg$link %||% "log" 
       )       
       testGLMM( 
         data, 
         "Group", 
         add_formula_terms = model_cfg$formula, 
         cols = count_cols, 
         family = family_obj 
       ) 
     } 
#' Run Wilcoxon tests
#' @export
run_wilcox <- function(data, dens_cols) {
  testWilcox(data, "Group", dens_cols)
}

