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

make_moderator_plot <- function(df, ma) {
  moderator_name <- all.vars(ma$formula.mods)[1] # assumes only one moderator was run
  emm <- ma |>
    emmprep() |>
    emmeans(as.formula(paste0("~ ", moderator_name)))
  contrasts <- pairs(emm)

  df |>
    ggplot(aes(x = .data[[moderator_name]], y = d_to_prop(d))) +
    geom_quasirandom(
      aes(size = weight, shape = published),
      col = PLOT_COL, alpha = .7, width = .3
    ) +
    geom_boxplot(width = .2, alpha = .8) +
    geom_point(
      data = as_tibble(emm) |>
        mutate(prop = d_to_prop(emmean)),
      aes(x = .data[[moderator_name]], y = prop),
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
    scale_x_discrete(
      labels = \(x)
      if (moderator_name == "scenario") {
        x |>
          str_replace_all("_", "/") |>
          str_to_sentence()
      } else {
        x |>
          str_replace_all("_", " ") |>
          str_to_sentence()
      }
    ) +
    labs(
      x = str_to_sentence(moderator_name),
      y = "Proportion preferring prosocial agent"
    ) +
    scale_size_continuous(
      range = c(1, 3),
      guide = "none"
    ) +
    PUB_SCALE
}
