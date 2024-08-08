# !===========================================================
# ! Quota analysis with heterogeneous random allocation
# !===========================================================

#++++++++++++++++++++++++++++++++++++
#+ Generate farmer parameter data
#++++++++++++++++++++++++++++++++++++
# ! beta is backed out based on the other parameters
gen_farmer_data <- function(par_set, num_farmers) {
  farmer_data <-
    data.table(
      opt_w = par_set$opt_w,
      farmer = 1:num_farmers,
      #--- maximum yield ---#
      ymax = runif(num_farmers, min = par_set$ymax_min, max = par_set$ymax_max),
      alpha = runif(num_farmers, min = par_set$alpha_min, max = par_set$alpha_max)
    ) %>%
    #--- find beta that matches with the optimal water use ---#
    expand_grid_df(., data.table(beta = seq(0, 1, by = 0.001))) %>%
    .[, opt_w_true := -log(w_price / (corn_price * ymax * alpha * beta)) / beta] %>%
    .[, .SD[which.min(abs(opt_w_true - opt_w)), ], by = farmer]

  return(farmer_data)
}

#++++++++++++++++++++++++++++++++++++
#+ Generate production data
#++++++++++++++++++++++++++++++++++++
# For each farmer, yield is generated for a sequence of values of water use. Based on these numbers, marginal profit wrt water and energy, optimal water and energy uses are also calculated.

gen_prod_data <- function(farmer_data, w_resolution = 0.01) {

  #! create water use sequence for each farmer (up to their unconstrained optimal water uses)
  w_seq_data <-
    rowwise(farmer_data) %>%
    dplyr::mutate(w = list(
      seq(0, max(opt_w) * 1.1, by = w_resolution)
    )) %>%
    dplyr::select(farmer, w) %>%
    unnest(cols = c(w)) %>%
    data.table()

  #! calcualte yield, profit, and others at each level of water uses for each farmer. Marginal profit of water will be used for trading analysis.
  prod_data <-
    farmer_data[w_seq_data, on = "farmer"] %>%
    .[, yield := ymax * (1 - alpha * exp(-beta * w))] %>%
    .[, w_cost := w_price * w] %>%
    .[, profit := corn_price * yield - w_cost - other_cost] %>%
    .[, marginal_cost_w := w_price] %>%
    .[, marginal_prod_w := ymax * alpha * beta * exp(-beta * w)] %>%
    .[, marginal_profit_w := marginal_prod_w * corn_price - marginal_cost_w]

  return(prod_data)
}

#++++++++++++++++++++++++++++++++++++
#+ Run quota and trading analyses
#++++++++++++++++++++++++++++++++++++
# ! Allocation is randomly determined independent of optimal water use. This imitates water use limits based on its proxy where measurement error is independent of actual water use

# parameter_sets <- par_sets_p
# farmer_data <- system_data$farmer_data[[1]]
# prod_data <- system_data$prod_data[[1]]
# num_it <- 100
# num_cores <- 8
# set <- 272

run_analysis <- function(parameter_sets, farmer_data, prod_data, num_it = 1, num_cores = NULL) {
  results <-
    parallel::mclapply(
      1:nrow(parameter_sets),
      \(set) {
        print(paste0("working on parameter set ", set, "/", nrow(parameter_sets)))

        #--- set parameters ---#
        target_w <- parameter_sets[set, target_w]
        u_dev <- parameter_sets[set, u_dev]
        dir_cor <- parameter_sets[set, dir_cor]

        #--- create effective quota data ---#
        # ! w_lim is the effective water use conditional on the uniform proxy quota
        quota_data <-
          make_quota_data(
            farmer_data,
            target_w = target_w,
            u_dev = u_dev,
            cor = dir_cor,
            num_it = num_it
          )

        # temp <-
        #   copy(quota_data) %>%
        #   .[, w := rnorm(.N, 12, sd = 3)] %>%
        #   .[, est_w := w / proxy_to_water]
        
        # lm(est_w ~ w, data = temp) %>% summary() %>% .$r.squared

        #--- analysis ---#
        # ! loop over sets of quota assignments
        results <-
          run_analysis_by_case(
            prod_data = prod_data,
            quota_data = quota_data
          ) %>%
          .[, target_w := target_w] %>%
          .[, dir_cor := dir_cor] %>%
          .[, u_dev := u_dev]

        return(results)
      },
      mc.cores = ifelse(is.null(num_cores), max(2, parallel::detectCores() - 5), num_cores),
      mc.preschedule = FALSE
    ) %>%
    rbindlist()
}

# ! This function runs quota and trading analysis for a single quota allocation case, which is used internally in run_analysis().

run_analysis_by_case <- function(prod_data, quota_data) {
  results <-
    lapply(
      unique(quota_data$case),
      \(x) {

        #---------------------
        #- Set up data
        #---------------------
        data_w <-
          copy(prod_data) %>%
          quota_data[case == x, ][., on = "farmer"] %>%
          #--- proxy use ---#
          .[, proxy := w / proxy_to_water] %>%
          .[, marginal_profit_p := proxy_to_water * marginal_profit_w]

        #---------------------
        #- Quota Analysis
        #---------------------
        proxy_quota_res <-
          data_w %>%
          #--- if limit is over optimal w, then set w_lim to opt_w ---#
          # this happens by farmers
          .[, alloc_limit := ifelse(w_lim > opt_w, opt_w, w_lim)] %>%
          #--- find the water amount that is closest to the limit ---#
          .[, .SD[which.min(abs(w - alloc_limit)), ], by = farmer] %>%
          #--- select variables needed ---#
          .[, .(farmer, w_lim, case, opt_w, proxy, w, profit, target_w)] %>%
          .[, instrument_type := "Quota"]

        #---------------------
        #- Trading Analysis
        #---------------------
        proxy_trading_res <-
          lapply(
            quantile(data_w[marginal_profit_p > 0, marginal_profit_p], prob = seq(0, 1, length = 100)) %>% unique(),
            \(mpp) {
              # ! equate the working permit price with marginal profit of proxy for each farmer
              data_w[, .SD[which.min(abs(marginal_profit_p - mpp)), ], by = farmer] %>%
                .[, permit_price := mpp]
            }
          ) %>%
          rbindlist() %>%
          #--- find the permit price that achieves the target water use ---#
          .[, mean_w := mean(w), by = permit_price] %>%
          .[abs(mean_w - target_w) == min(abs(mean_w - target_w)), ] %>%
          #--- it is possible that the above process picks two obs per farmer ---#
          .[order(farmer, permit_price), ] %>%
          .[, .SD[1, ], by = farmer] %>%
          #--- calculate permit sales and update profit ---#
          .[, permit_sales := (w_lim - w) * permit_price] %>%
          .[, profit_prod := profit] %>%
          .[, profit := profit + permit_sales] %>%
          #--- select variables needed ---#
          .[, .(farmer, w_lim, case, opt_w, proxy, w, profit, target_w, permit_price, permit_sales, profit_prod)] %>%
          .[, instrument_type := "Trading"]

        analysis_res <- rbind(proxy_quota_res, proxy_trading_res, fill = TRUE)

        # analysis_res[, mean(profit), by = instrument_type]

        return(analysis_res)
      }
    ) %>%
    rbindlist()

  return(results)
}


# temp <-
#   prod_data %>%
#   .[, water_to_proxy := max(0, rnorm(1, mean = 1, sd = 0.1)), by = farmer] %>%
#   .[, proxy := w * water_to_proxy]

#++++++++++++++++++++++++++++++++++++
#+ Assign quota values for individual farmers
#++++++++++++++++++++++++++++++++++++
# w_lim: effective water use limit

make_quota_data <- function(farmer_data, target_w, u_dev, cor, num_it = 100) {
  mean_opt_w <- farmer_data[, mean(opt_w)]

  if (cor == "+") {
    quota_data <-
      lapply(
        1:num_it,
        \(x) {
          copy(farmer_data)[, .(farmer, opt_w)] %>%
            .[, dev_from_mean := (opt_w - mean(opt_w)) / mean_opt_w * target_w] %>%
            #--- effective water use limit ---#
            .[, w_lim := (target_w + dev_from_mean) * runif(1, min = 1 - u_dev, max = 1 + u_dev), by = farmer]
        }
      )
  } else if (cor == "-") {
    quota_data <-
      lapply(
        1:num_it,
        \(x) {
          copy(farmer_data)[, .(farmer, opt_w)] %>%
            .[, dev_from_mean := (opt_w - mean(opt_w)) / mean_opt_w * target_w] %>%
            #--- effective water use limit ---#
            .[, w_lim := (target_w - dev_from_mean) * runif(1, min = 1 - u_dev, max = 1 + u_dev), by = farmer]
        }
      )
  } else if (cor == "0") {
    quota_data <-
      lapply(
        1:num_it,
        \(x) {
          copy(farmer_data)[, .(farmer)] %>%
            #--- effective water use limit ---#
            .[, w_lim := target_w * runif(1, min = 1 - u_dev, max = 1 + u_dev), by = farmer]
        }
      )
  }

  return_data <-
    quota_data %>%
    data.table::rbindlist(idcol = "case") %>%
    .[, w_lim := w_lim / mean(w_lim) * target_w, by = case] %>%
    #--- water to proxy conversion factor ---#
    # multiply proxy with proxy_to_water (eta) to get water use
    .[, proxy_to_water := w_lim / target_w] %>%
    .[, .(farmer, w_lim, proxy_to_water, case)] %>%
    .[, target_w := target_w]

  return(return_data)
}

# !===========================================================
# ! Utility functions
# !===========================================================
expand_grid_df <- function(data_1, data_2) {
  data_1_ex <-
    data_1[rep(1:nrow(data_1), each = nrow(data_2)), ] %>%
    data.table() %>%
    .[, rowid := 1:nrow(.)]

  data_2_ex <-
    data_2[rep(1:nrow(data_2), nrow(data_1)), ] %>%
    data.table() %>%
    .[, rowid := 1:nrow(.)]

  expanded_data <-
    data_1_ex[data_2_ex, on = "rowid"] %>%
    .[, rowid := NULL]

  if ("tbl" %in% class(data_1)) {
    expanded_data <- as_tibble(expanded_data)
  }

  if ("rowwise_df" %in% class(data_1)) {
    expanded_data <- rowwise(expanded_data)
  }

  return(expanded_data)
}
