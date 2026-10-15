PLOT_COL <- "deepskyblue3"
PUB_SCALE <- scale_shape_manual(
  values = c(1, 16),
  labels = c("Unpublished", "Published"),
  name = "Publication status"
)

study_label <- function(study) {
  study |>
    str_replace_all("_", " ") |>
    str_replace_all("(\\w+)$", "(\\1)") |>
    str_replace_all("et al", "et al.") |>
    str_replace("Wu et", "Wu et al.") |> # error
    str_replace("LoheideNiesmann", "Loheide-Niesmann") |> # special case
    map_chr(\(x) {
      x_w <- str_split(x, " ")[[1]]
      if (length(x_w) == 3) {
        x <- str_replace(x, " ", " & ")
      }
      x <- case_when( # special cases
        x == "Hamlin (unpub)" ~ "Hamlin (unpub a)",
        x == "Hamlin & 2025 (unpub)" ~ "Hamlin (unpub b)",
        .default = x
      )
      return(x)
    })
}

level_lab <- function(x, var) {
  case_when(
    var == "scenario" ~ x |> str_to_sentence() |> str_replace_all("_", "/\n"),
    var == "question" & x == "Who do you like" ~ "Who do\nyou like?",
    var == "question" & x == "No" ~ "No question",
    var == "question" ~ x,
    var %in% c("H_Lab_narrow", "H_Lab_wide") & x == "yes" ~ "Hamlin",
    var %in% c("H_Lab_narrow", "H_Lab_wide") ~ "Non-Hamlin",
    var == "G_Lab" & x == "yes" ~ "Geraci",
    var == "G_Lab" ~ "Non-Geraci",
    var == "preregistered" & x == "yes" ~ "Yes",
    var == "preregistered" ~ "No",
    var == "setting" & x == "in_person" ~ "In-person",
    var == "setting" ~ "Online",
    var == "nationality" ~ x |> str_replace_all("_", "\n") |> str_to_title(),
    .default = x |> str_replace_all("_", " ") |> str_to_sentence() |> str_replace_all("-", " –\n")
  )
}

# This is super customised… primarily because I didn't like the default look of most forest plots.
make_forest_plot <- function(df, ma, diamond_y = -1, diamond_height = 0.4,
                             x_min = -0.6, x_max = 1.5) {
  summary_prop <- d_to_prop(ma$b[1])
  summary_prop_ci.lb <- d_to_prop(ma$ci.lb)
  summary_prop_ci.ub <- d_to_prop(ma$ci.ub)

  df_diamond <- tibble(
    x = c(
      summary_prop_ci.lb, summary_prop,
      summary_prop_ci.ub, summary_prop, summary_prop_ci.lb
    ),
    y = c(
      diamond_y, diamond_y + diamond_height,
      diamond_y, diamond_y - diamond_height, diamond_y
    )
  )

  ggplot(df, aes(y = rev(id))) +
    # reference lines
    annotate("segment",
      x = 0.5, xend = 0.5,
      y = diamond_y - diamond_height - 0.5,
      yend = max(df$id) + 1,
      lty = "dotted", col = "grey27", linewidth = 0.5
    ) +
    annotate("segment",
      x = d_to_prop(ma$b[1]), xend = d_to_prop(ma$b[1]),
      y = diamond_y - diamond_height - 0.5,
      yend = max(df$id) + 1,
      lty = "dashed", col = PLOT_COL, linewidth = 0.5
    ) +
    # forest points
    geom_errorbarh(aes(xmin = prop_ci.lb, xmax = prop_ci.ub), height = 0.5) +
    geom_point(aes(x = prop, size = weight, shape = published),
      col = PLOT_COL
    ) +
    # summary
    geom_hline(
      yintercept = 0,
      lty = "solid", col = "black", linewidth = 0.5
    ) +
    geom_polygon(
      data = df_diamond, aes(x = x, y = y),
      fill = PLOT_COL, col = PLOT_COL, inherit.aes = FALSE
    ) +
    annotate("text",
      x = x_min + 0.02, y = diamond_y,
      label = "Random effects model",
      hjust = 0, size = 3, fontface = "bold"
    ) +
    annotate("text",
      x = x_max - 0.02, y = diamond_y,
      label = sprintf("%.2f [%.2f, %.2f]", summary_prop, summary_prop_ci.lb, summary_prop_ci.ub),
      hjust = 1, size = 3, fontface = "bold"
    ) +
    # text
    geom_text(aes(x = x_min + 0.02, label = study),
      hjust = 0, size = 3
    ) +
    geom_text(
      aes(
        x = x_max - 0.02,
        label = sprintf("%.2f [%.2f, %.2f]", prop, prop_ci.lb, prop_ci.ub)
      ),
      hjust = 1, size = 3
    ) +
    # top labels
    annotate("text",
      x = x_min + 0.02,
      y = max(df$id) + 2,
      label = "Study",
      hjust = 0, size = 3, fontface = "bold"
    ) +
    annotate("text",
      x = x_max - 0.02,
      y = max(df$id) + 2,
      label = "Proportion [95% CI]",
      hjust = 1, size = 3, fontface = "bold"
    ) +
    geom_hline(
      yintercept = max(df$id) + 1,
      lty = "solid", col = "black", linewidth = 0.5
    ) +
    # axes
    annotate("segment",
      x = 0, xend = 1,
      y = diamond_y - diamond_height - 0.5,
      yend = diamond_y - diamond_height - 0.5,
      linewidth = 0.5
    ) +
    scale_x_continuous(
      limits = c(x_min, x_max),
      breaks = seq(0, 1, 0.25),
      expand = c(0.01, 0.01)
    ) +
    scale_y_continuous(
      limits = c(
        diamond_y - diamond_height - 0.5,
        max(df$id) + 4
      ),
      expand = c(0, 0)
    ) +
    scale_size_continuous(range = c(1, 3), guide = "none") +
    scale_shape_manual(
      values = c(0, 15),
      labels = c("Unpublished", "Published"),
      name = "Publication status",
      guide = "none"
    ) +
    coord_cartesian(clip = "off") +
    labs(
      x = "Proportion preferring prosocial agent",
      y = NULL
    ) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      panel.border = element_blank(),
      plot.margin = margin(10, 10, 10, 10),
      axis.title.x = element_text(hjust = .545) # hand-tweaked oops
    )
}

make_moderator_plot <- function(df, ma, term, xlab = NULL) {
  emm <- get_emm(ma, term, tibble = FALSE)
  contrasts <- pairs(emm)

  df |>
    ggplot(aes(x = .data[[term]], y = d_to_prop(g))) +
    geom_quasirandom(
      aes(size = weight, shape = published),
      col = PLOT_COL, alpha = .65, width = .3
    ) +
    geom_boxplot(width = .2, alpha = .8) +
    geom_point(
      data = as_tibble(emm) |>
        mutate(prop = d_to_prop(emmean)),
      aes(x = .data[[term]], y = prop),
      col = PLOT_COL, shape = 18, size = 4
    ) +
    stat_pvalue_manual(
      data = contrasts |>
        separate_contrasts() |>
        filter(p.signif != "ns"),
      y.position = 1.05,
      step.increase = 0.05,
      tip.length = 0
    ) +
    scale_x_discrete(labels = \(x) level_lab(x, term)) +
    labs(
      x = if (is.null(xlab)) str_to_sentence(term) else xlab,
      y = "Proportion preferring prosocial agent"
    ) +
    scale_size_continuous(
      range = c(1, 2.5),
      guide = "none"
    ) +
    PUB_SCALE
}

make_continuous_plot <- function(df, ma, term, xlab = NULL) {
  nd <- seq(min(df[[term]]), max(df[[term]]), length.out = 50)
  mod_names <- setdiff(colnames(ma$X), c("intrcpt", "(Intercept)"))
  newmods <- matrix(
    0,
    nrow = length(nd),
    ncol = length(mod_names),
    dimnames = list(NULL, mod_names)
  )
  # newmods[, "mean_age_days"] <- mean(df$mean_age_days)
  # newmods[, "log_ni"] <- mean(df$log_ni)
  newmods[, term] <- nd
  nd_pred <- tibble(xval = nd) |>
    cbind(predict(ma, newmods = newmods, level = 95) |>
            as_tibble()) |>
    mutate(
      prop_pred = d_to_prop(pred),
      prop_pred_ci.lb = d_to_prop(ci.lb),
      prop_pred_ci.ub = d_to_prop(ci.ub)
    )

  star <- tidy(ma) |>
    filter(term == .env$term) |>
    pull(p.value) |>
    {\(x) x < .05}()

  if (term == "age_sc") {
    xticks_orig <- c(0, 250, 500, 750, 1000)
    xticks_trf <- (xticks_orig - mean(df$mean_age_days)) / sd(df$mean_age_days)
  }
  if (term == "lni_sc") {
    xticks_orig <- c(2, 4, 6)
    xticks_trf <- (xticks_orig - mean(df$log_ni)) / sd(df$log_ni)
  }

  p <- df |>
    ggplot(aes(x = .data[[term]], y = d_to_prop(g))) +
    geom_point(
      aes(size = weight, shape = published),
      col = PLOT_COL, alpha = .65
    ) +
    geom_ribbon(
      data = nd_pred,
      aes(x = xval,
          y = prop_pred,
          ymin = prop_pred_ci.lb,
          ymax = prop_pred_ci.ub),
      fill = PLOT_COL,
      alpha = .2
    ) +
    geom_line(
      data = nd_pred,
      aes(x = xval,
          y = prop_pred),
      col = PLOT_COL,
      linewidth = 1
    ) +
    labs(
      x = if (is.null(xlab)) str_to_sentence(term) else xlab,
      y = "Proportion preferring prosocial agent"
    ) +
    scale_x_continuous(
      breaks = xticks_trf,
      labels = xticks_orig
    ) +
    scale_size_continuous(
      range = c(1, 2.5),
      guide = "none"
    ) +
    PUB_SCALE

  if (star) {
    p <- p +
      annotate("text",
               x = median(nd),
               y = 1.05,
               label = "*")
  }
  p
}










axis_lab <- function(x, var) {
  lab <- level_lab(x, var)
  if (var == "scenario") {
    str_replace(lab, "/", "/\n")
  } else if (var == "stimuli_c") {
    str_replace(lab, ", ", ",\n")
  } else {
    lab
  }
}

plot_cont <- function(var, preds, xlab, star = FALSE) {
  p <- df_plot |>
    ggplot(aes(x = .data[[var]])) +
    geom_point(
      aes(y = prop, size = weight, shape = published),
      col = PLOT_COL, alpha = .7
    ) +
    geom_ribbon(
      data = preds,
      aes(y = prop_pred, ymin = prop_pred_ci.lb, ymax = prop_pred_ci.ub),
      fill = PLOT_COL, alpha = .2
    ) +
    geom_line(data = preds, aes(y = prop_pred), col = PLOT_COL, linewidth = 1) +
    scale_size_continuous(range = c(1, 3), guide = "none") +
    PUB_SCALE +
    labs(x = xlab, y = "Proportion preferring prosocial agent")
  if (star) {
    p <- p + annotate(
      "text",
      x = mean(range(df_plot[[var]], na.rm = TRUE)),
      y = 1.05,
      label = "*"
    )
  }
  p
}

plot_cat <- function(var, preds, pairs, xlab) {
  p <- df_plot |>
    filter(!is.na(.data[[var]])) |>
    ggplot(aes(x = .data[[var]])) +
    geom_quasirandom(
      aes(y = prop, size = weight, shape = published),
      col = PLOT_COL, alpha = .7, width = .3
    ) +
    geom_boxplot(aes(y = prop), width = .2, alpha = .8) +
    geom_point(
      data = preds,
      aes(y = prop_pred),
      col = PLOT_COL, shape = 18, size = 4
    ) +
    scale_x_discrete(labels = \(x) axis_lab(x, var)) +
    scale_size_continuous(range = c(1, 3), guide = "none") +
    PUB_SCALE +
    labs(x = xlab, y = "Proportion preferring prosocial agent")
  sig <- pairs |>
    filter(p.signif != "ns") |>
    select(group1, group2, p.signif)
  if (nrow(sig) > 0) {
    p <- p + stat_pvalue_manual(
      sig,
      y.position = 1.05,
      step.increase = 0.05,
      tip.length = 0
    )
  }
  p
}

plot_int_cat <- function(xvar, facet, preds, xlab) {
  df_plot |>
    ggplot(aes(x = .data[[xvar]])) +
    geom_quasirandom(
      aes(y = prop, size = weight, shape = published),
      col = PLOT_COL, alpha = .7, width = .3
    ) +
    geom_boxplot(aes(y = prop), width = .2, alpha = .8) +
    geom_point(
      data = preds,
      aes(y = prop_pred),
      col = PLOT_COL, shape = 18, size = 4
    ) +
    facet_wrap(reformulate(facet), labeller = as_labeller(\(x) level_lab(x, facet))) +
    scale_x_discrete(labels = \(x) axis_lab(x, xvar)) +
    scale_size_continuous(range = c(1, 3), guide = "none") +
    PUB_SCALE +
    labs(x = xlab, y = "Proportion preferring prosocial agent")
}

plot_int_age <- function(facet, preds) {
  preds <- preds |> arrange(.data[[facet]], mean_age_days)
  df_plot |>
    ggplot(aes(x = mean_age_days)) +
    geom_point(
      aes(y = prop, size = weight, shape = published),
      col = PLOT_COL, alpha = .7
    ) +
    geom_ribbon(
      data = preds,
      aes(y = prop_pred, ymin = prop_pred_ci.lb, ymax = prop_pred_ci.ub),
      fill = PLOT_COL, alpha = .2
    ) +
    geom_line(data = preds, aes(y = prop_pred), col = PLOT_COL, linewidth = 1) +
    facet_wrap(reformulate(facet), labeller = as_labeller(\(x) level_lab(x, facet))) +
    scale_size_continuous(range = c(1, 3), guide = "none") +
    PUB_SCALE +
    labs(x = "Mean age (days)", y = "Proportion preferring prosocial agent")
}

finish <- function(..., ncol = 1) {
  wrap_plots(...) +
    plot_layout(ncol = ncol, guides = "collect") +
    plot_annotation(tag_levels = "A") &
    scale_y_continuous(limits = c(0, 1.1), breaks = seq(0, 1, 0.25)) &
    theme(legend.position = "bottom")
}
