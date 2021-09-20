library(harp)
library(dplyr)

parameters <- c(
  "T2m",
  "T850",
  "T700",
  "T500",
  "Td2m",
  "Td850",
  "Td700",
  "Td500",
  "RH2m",
  "RH850",
  "RH700",
  "RH500",
  "S10m",
  "S850",
  "S700",
  "S500",
  "AccPcp3h", 
  "AccPcp6h",
  "AccPcp12h",
  "Pmsl",
  "Z850",
  "Z700",
  "Z500",
  "CClow",
  "CCtot",
  "Cbase"
)

verif_domain <- "METCOOP" #c("METCOOP", "IBERIA")

bs_func <- function(param, domain, num_replicates, groupings) {
  
  switch(
    domain, 
    "METCOOP" = {
      start_date <- 20190201
      end_date   <- 20190214
      fc_temp    <- "fctable_eps_all_leads"
    },
    "IBERIA" = {
      start_date <- 20181007
      end_date   <- 20181021
      fc_temp    <- "fctable_eps"
    }
  )
  
  message("Verifying: ", param, " for ", domain)
  message("===========", rep("=", nchar(param)), "=====", rep("=", nchar(domain)))
  cat("\n")
  message("Reading forecast data")
  fcst <- read_point_forecast(
    start_date, 
    end_date, 
    c("CY40h111", "CY43b7_2"), 
    "EPS", 
    param, 
    lead_time     = seq(0, 48, 3), 
    file_path     = file.path("/home/andrewts/data/harp/hirlam", domain, "FCTABLE"), 
    file_template = fc_temp, 
    by            = "1d"
  )
  
  fcst <- common_cases(fcst)
  
  stations <- unique(unlist(pull(fcst, SID)))
  
  cat("\n")
  message("Reading obs data")
  obs <- read_point_obs(
    first_validdate(fcst), 
    last_validdate(fcst), 
    param, 
    obs_path = file.path("/home/andrewts/data/harp/hirlam", domain, "OBSTABLE"),
    stations = stations
  )

  if (nrow(obs) < 1) return(NULL)
  
  fcst <- join_to_fcst(fcst, obs)
  
  cat("\n")
  message("Computing score")
  res <- pooled_bootstrap_score(
    fcst,
    ens_verify,
    {{param}},
    num_replicates,
    groupings = groupings
  )
  
  cat("\n")
  message("=========================")
  cat("\n")
  
  res
  
}

domain_func <- function(dom, parameters, num_replicates, groupings) {
  bs <- lapply(parameters, bs_func, domain = dom, num_replicates = num_replicates, groupings = groupings)
  bs <- bind_bootstrap_score(bs)
  harpPoint:::new_harp_bootstrap(lapply(bs, mutate, domain = dom))
}

bs <- lapply(verif_domain, domain_func, parameters, num_replicates = 1000, groupings = "leadtime")

bs_sc <- bind_bootstrap_score(bs)
