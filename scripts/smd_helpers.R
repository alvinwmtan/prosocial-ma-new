calculate_d <- function(method, ni, xi = NULL, s = NULL, t = NULL) {
  if (method == "success") {
    d <- (escalc(
      measure = "PLO",
      xi = xi, ni = ni
    ))$yi * sqrt(3) / pi
    d_var <- (escalc(
      measure = "PLO",
      xi = xi, ni = ni
    ))$vi * (3 / (pi^2))
  } else if (method == "proportion") {
    d <- (s - 0.5) * 2 * sqrt(ni)
    d_var <- (1 / ni) + (d^2 / (2 * ni))
  } else if (method == "ttest") {
    d <- t / sqrt(ni)
    d_var <- (1 / ni) + (d^2 / (2 * ni))
  } else {
    d <- NA
    d_var <- NA
  }
  return(tibble("d" = d, "d_var" = d_var))
}

d_to_prop <- function(d) {
  odds_ratio <- exp(d * (pi / sqrt(3)))
  prop <- odds_ratio / (1 + odds_ratio)
  return(prop)
}

print_num <- function(x, dec = 3) {
  round(x, dec) |> format(nsmall = dec)
}

print_est_ci <- function(x, ci.lb, ci.ub) {
  glue("{print_num(x)} [{print_num(ci.lb)}, {print_num(ci.ub)}]")
}

print_p <- function(p) {
  if (p < .001) {
    "$p <$ .001"
  } else {
    glue("$p =$ {print_num(p, 3)}")
  }
}

print_qm <- function(mod) {
  glue("$QM$({mod$QMdf[1]}) = {print_num(mod$QM)}, {print_p(mod$QMp)}")
}

print_emm <- function(emm_df, d_to_prop = TRUE) {
  if (d_to_prop) {
    glue(
      "{print_num(d_to_prop(emm_df$emmean[1]))} ",
      "[{print_num(d_to_prop(emm_df$asymp.LCL[1]))}, ",
      "{print_num(d_to_prop(emm_df$asymp.UCL[1]))}]"
    )
  } else {
    glue(
      "{print_num(emm_df$emmean[1])} ",
      "[{print_num(emm_df$asymp.LCL[1])}, ",
      "{print_num(emm_df$asymp.UCL[1])}]"
    )
  }
}

separate_contrasts <- function(cons) {
  cons_df <- cons |>
    as_tibble() |>
    separate(contrast, c("group1", "group2"), " - ") |>
    mutate(
      group1 = gsub("[)(]", "", group1),
      group2 = gsub("[)(]", "", group2),
      p.signif = cut(p.value,
        breaks = c(-Inf, .0001, .001, .01, .05, .1, 1),
        labels = c("****", "***", "**", "*", "ns", "ns")
      )
    )
  cons_df
}
