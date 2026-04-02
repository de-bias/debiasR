# Global variable declarations for dplyr NSE checks.
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      ".data", "area", "coef_factor", "destination", "destination_population",
      "destination_user_count", "flow", "flow_adj", "flow_bench", "mpd_source",
      "origin", "origin_population", "origin_user_count", "p", "P", "p_d", "P_d",
      "p_o", "P_o", "pen", "pen_d", "pen_o", "population", "user_count",
      "U", "U_d", "U_o", "w_sel",
      "weight_destination", "weight_ipf", "weight_missing", "weight_origin",
      "x", "y"
    )
  )
}
