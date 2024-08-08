# !===========================================================
# ! Quota analysis
# !===========================================================

#++++++++++++++++++++++++++++++++++++
#+ Generate farmer data
#++++++++++++++++++++++++++++++++++++
# Create a dataset of farmers where their yield production function and energy efficiency are determined
gen_farmer_data <- function(num_farmers, parameters) {
  farmer_data <-
    data.table(
      farmer = 1:num_farmers,
      #--- energy use efficiency ---#
      # factor converting water use to energy use (kwh/acre-inch)
      eta = runif(num_farmers, min = parameters$eta_min, max = parameters$eta_max), # kwh/inch
      #--- maximum yield ---#
      ymax = runif(num_farmers, min = parameters$ymax_min, max = parameters$ymax_max),
      alpha = runif(num_farmers, min = parameters$alpha_min, max = parameters$alpha_max),
      beta = runif(num_farmers, min = parameters$beta_min, max = parameters$beta_max)
    ) %>%
    #--- find the optimal w and e ---#
    .[, opt_w := -log((w_price + e_price * eta / 12) / (corn_price * ymax * alpha * beta)) / beta] %>%
    .[, opt_e := measurements::conv_unit(opt_w, "inch", "ft") * eta]

  return(farmer_data)
}

#++++++++++++++++++++++++++++++++++++
#+ Generate production data
#++++++++++++++++++++++++++++++++++++
# For each farmer, yield is generated for a sequence of values of water use. Based on these numbers, marginal profit wrt water and energy, optimal water and energy uses are also calculated.

gen_prod_data <- function(farmer_data, w_resolution = 0.01) {
  prod_data <-
    expand_grid_df(
      farmer_data,
      data.table(w = seq(0, max(farmer_data$opt_w) * 1.2, by = w_resolution))
    ) %>%
    .[, yield := ymax * (1 - alpha * exp(-beta * w))] %>%
    .[, e := measurements::conv_unit(w, "inch", "ft") * eta] %>%
    .[, e_only_cost := e_price * e] %>%
    .[, w_only_cost := w_price * w] %>%
    .[, profit := corn_price * yield - e_only_cost - w_only_cost - other_cost] %>%
    .[, marginal_cost_w := w_price + e_price * eta / 12] %>%
    .[, marginal_prod_w := ymax * alpha * beta * exp(-beta * w)] %>%
    .[, marginal_profit_w := marginal_prod_w * corn_price - marginal_cost_w] %>%
    .[, marginal_cost_e := e_price] %>%
    .[, marginal_prod_e := ymax * alpha * beta * exp(-beta * w) / eta * 12] %>%
    .[, marginal_profit_e := marginal_prod_e * corn_price - marginal_cost_e]

  return(prod_data)
}

quota_analysis <- function(prod_data, quota_data, mean_eta) {
  #---------------------
  #- Quota analysis (Water-based)
  #---------------------

  w_quota_res <-
    quota_data %>%
    rowwise() %>%
    dplyr::mutate(w_quota_res = list(
      quota_analysis_w(w_lim = w_lim)
    )) %>%
    unnest() %>%
    data.table()

  #---------------------
  #- Quota analysis (Energy)
  #---------------------
  e_quota_res <-
    quota_data %>%
    rowwise() %>%
    dplyr::mutate(e_quota_res = list(
      quota_analysis_e(e_lim = e_lim)
    )) %>%
    unnest() %>%
    data.table()

  #---------------------
  #- Results smoothing
  #---------------------
  all_res <-
    rbind(w_quota_res, e_quota_res) %>%
    nest_by(instrument_type) %>%
    dplyr::mutate(w_data = list(
      data.table(w = seq(min(data$w), max(data$w), length = 1000))
    )) %>%
    dplyr::mutate(g_pi_w = list(
      gam(profit ~ s(w, k = 4), data = data)
    )) %>%
    dplyr::mutate(g_e_w = list(
      gam(e ~ s(w, k = 4), data = data)
    )) %>%
    dplyr::mutate(g_limit_w = list(
      gam(w_alloc_limit ~ s(w, k = 4), data = data)
    )) %>%
    dplyr::mutate(g_limit_e = list(
      gam(e_alloc_limit ~ s(w, k = 4), data = data)
    )) %>%
    dplyr::mutate(results = list(
      w_data %>%
        .[, profit := predict(g_pi_w, newdata = .)] %>%
        .[, e := predict(g_e_w, newdata = .)] %>%
        .[, w_alloc_limit := predict(g_limit_w, newdata = .)] %>%
        .[, e_alloc_limit := predict(g_limit_e, newdata = .)] %>%
        .[, inst_type := instrument_type]
    )) %>%
    .$results %>%
    rbindlist() %>%
    .[, regulation_type := "Quota"]

  return(all_res)
}

#++++++++++++++++++++++++++++++++++++
#+ Quota analysis (water)
#++++++++++++++++++++++++++++++++++++
quota_analysis_w <- function(w_lim) {
  w_quota_res_sum <-
    copy(prod_data) %>%
    .[, w_alloc_limit := w_lim] %>%
    #--- if limit is over optimal w, then set w_lim to opt_w ---#
    # this happens by farmers
    .[, w_lim := ifelse(w_alloc_limit > opt_w, opt_w, w_alloc_limit)] %>%
    .[, .SD[which.min(abs(w - w_lim)), ], by = farmer] %>%
    .[, .(w = mean(w), profit = mean(profit), e = mean(e)), by = w_alloc_limit] %>%
    .[, instrument_type := "Water-based"] %>%
    .[, e_alloc_limit := w_alloc_limit * mean_eta / 12]

  return(w_quota_res_sum)
}

quota_analysis_e <- function(e_lim) {
  e_quota_res_sum <-
    copy(prod_data) %>%
    .[, e_alloc_limit := e_lim] %>%
    #--- if limit is over optimal e, then set e_lim to opt_e ---#
    .[, e_lim := ifelse(e_alloc_limit > opt_e, opt_e, e_alloc_limit)] %>%
    .[, .SD[which.min(abs(e - e_lim)), ], by = farmer] %>%
    .[, .(w = mean(w), profit = mean(profit), e = mean(e)), by = e_alloc_limit] %>%
    .[, instrument_type := "Energy-based"] %>%
    .[, w_alloc_limit := e_alloc_limit / mean_eta * 12]

  return(e_quota_res_sum)
}

#++++++++++++++++++++++++++++++++++++
#+ Permit Trading Analysis
#++++++++++++++++++++++++++++++++++++
trading_analysis <- function(prod_data) {
  #---------------------
  #- Trading analysis (Water-based)
  #---------------------
  w_trading_res <-
    lapply(
      pmax(0, quantile(prod_data[, marginal_profit_w], prob = seq(0, 1, length = 500))) %>% unique(),
      \(x) {
        # print(x)
        prod_data[, .SD[which.min(abs(marginal_profit_w - x)), ], by = farmer] %>%
          .[, permit_price := x]
      }
    ) %>%
    rbindlist() %>%
    .[, .(w = mean(w), profit = mean(profit), e = mean(e)), by = permit_price] %>%
    .[, instrument_type := "Water-based"]

  #---------------------
  #- Trading analysis (Energy)
  #---------------------
  e_trading_res <-
    lapply(
      pmax(0, quantile(prod_data[, marginal_profit_e], prob = seq(0, 1, length = 500))) %>% unique(),
      \(x) {
        # print(x)
        prod_data[, .SD[which.min(abs(marginal_profit_e - x)), ], by = farmer] %>%
          .[, permit_price := x]
      }
    ) %>%
    rbindlist() %>%
    .[, .(w = mean(w), profit = mean(profit), e = mean(e)), by = permit_price] %>%
    .[, instrument_type := "Energy-based"]

  all_res <-
    rbind(w_trading_res, e_trading_res) %>%
    # .[permit_price > 0, ] %>%
    nest_by(instrument_type) %>%
    dplyr::mutate(w_data = list(
      data.table(w = seq(min(data$w), max(data$w), length = 1000))
    )) %>%
    dplyr::mutate(g_pi_w = list(
      gam(profit ~ s(w), data = data)
    )) %>%
    dplyr::mutate(g_e_w = list(
      gam(e ~ s(w), data = data)
    )) %>%
    dplyr::mutate(g_limit_w = list(
      gam(permit_price ~ s(w), data = data)
    )) %>%
    dplyr::mutate(results = list(
      w_data %>%
        .[, profit := predict(g_pi_w, newdata = .)] %>%
        .[, e := predict(g_e_w, newdata = .)] %>%
        .[, permit_price := predict(g_limit_w, newdata = .)] %>%
        .[, inst_type := instrument_type]
    )) %>%
    .$results %>%
    rbindlist() %>%
    .[, regulation_type := "Permit Trading"]

  return(all_res)
}

