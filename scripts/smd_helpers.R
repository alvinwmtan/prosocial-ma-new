calculate_d <- function(method, ni, xi = NULL,
                        s_mean = NULL, s_sd = NULL, t = NULL) {
  if (method == "success") {
    es <- escalc(
      measure = "PLO",
      xi = xi, ni = ni
    )
    d <- es$yi * sqrt(3) / pi
    d_var <- es$vi * (3 / (pi^2))
  } else if (method == "proportion") {
    d <- (s_mean - 0.5) / s_sd
    d_var <- 1 / ni + d^2 / (2 * (ni - 1))
  } else if (method == "ttest") {
    d <- t / sqrt(ni)
    d_var <- 1 / ni + d^2 / (2 * (ni - 1))
  } else {
    d <- NA
    d_var <- NA
  }

  # Hedges' small sample correction
  df <- ni - 1
  J <- 1 - 3 / (4 * df - 1)
  g <- J * d
  g_var <- J^2 * (
    1 / ni + d^2 / (2 * df)
  )

  return(tibble("d" = d, "d_var" = d_var, "g" = g, "g_var" = g_var))
}

d_to_prop <- function(d) {
  odds_ratio <- exp(d * (pi / sqrt(3)))
  prop <- odds_ratio / (1 + odds_ratio)
  return(prop)
}

print_num <- function(x, dec = 2) {
  round(x, dec) |> format(nsmall = dec)
}

print_est_ci <- function(x, ci.lb, ci.ub) {
  glue("{print_num(x)} [{print_num(ci.lb)}, {print_num(ci.ub)}]")
}

print_p <- function(p) {
  if (p < .001) {
    "$p <$ .001"
  } else {
    glue("$p =$ {print_num(p, 3) |> str_sub(2, 5)}")
  }
}

# print_qm <- function(mod) {
#   glue("$QM$({mod$QMdf[1]}) = {print_num(mod$QM)}, {print_p(mod$QMp)}")
# }

omnibus_test <- function(ma, term) {
  cn <- names(coef(ma))

  if (grepl(":", term)) {
    terms <- str_split(term, ":", simplify = TRUE)
    idx <- which(
      grepl(glue("{terms[[1]]}.*:.*{terms[[2]]}"), cn) |
        grepl(glue("{terms[[2]]}.*:.*{terms[[1]]}"), cn)
    )
  } else {
    idx <- which(
      grepl(glue("^{term}"), cn) &
        !grepl(":", cn)
    )
  }

  if (length(idx) == 0) {
    stop(glue("{term} not found in model coefficients."))
  }

  anova(ma, btt = idx)
}

print_qm <- function(ma, term = NULL) {
  mod <- if (is.null(term)) ma else omnibus_test(ma, term)
  glue("$Q_M$({mod$QMdf[1]}) = {print_num(mod$QM)}, {print_p(mod$QMp)}")
}

get_emm <- function(ma, term, tibble = TRUE) {
  emm <- ma |>
    emmprep(rg.limit = 13000) |>
    emmeans(as.formula(paste0("~ ", term)), weights = "proportional")
  if (tibble) as_tibble(emm) else emm
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
