# Figures for "Example 1/2: Privacy function" slides (Persuasive Privacy).
# Both figures use the same story: a fixed true data value x and three
# candidate Receiver decisions d1, d2, d3:
#   d1  confident and right   -> low privacy
#   d2  confident and wrong   -> high privacy
#   d3  vague                 -> privacy retained (Ex 1) / intermediate (Ex 2)
# Colour encodes privacy: red = privacy lost, blue = privacy retained.

library(ggplot2)

col_lost     <- "#B2182B"
col_retained <- "#2166AC"
col_mid      <- "grey45"
col_ink      <- "grey15"

x0    <- 0   # true (private) data value x
kappa <- 1   # interval-length threshold in Example 1

theme_slide <- function(base_size = 18) {
  theme_void(base_size = base_size) +
    theme(
      plot.margin     = margin(10, 10, 10, 10),
      legend.position = "bottom",
      legend.title    = element_blank(),
      legend.text     = element_text(size = base_size * 0.8)
    )
}

# Example 1: interval privacy function ----------------------------------------
# rho(d, x) = 0 if x in d and |d| < kappa, 1 otherwise.

rho_interval <- function(a, b, x, kappa) {
  as.integer(!(a <= x & x <= b & (b - a) < kappa))
}

intervals <- data.frame(
  id  = c("d[1]", "d[2]", "d[3]"),
  a   = c(-0.35, 0.55, -1.60),
  b   = c( 0.35, 1.25,  1.10),
  y   = c(3, 2, 1),
  why = c("x %in% d[1]*','~~abs(d[1]) < kappa",
          "x %notin% d[2]",
          "abs(d[3]) >= kappa")
)
intervals$rho     <- rho_interval(intervals$a, intervals$b, x0, kappa)
intervals$outcome <- factor(ifelse(intervals$rho == 0, "lost", "retained"),
                            levels = c("lost", "retained"))
intervals$rho_lab <- sprintf("rho(%s, x) == %d", intervals$id, intervals$rho)

line_y <- 0.25  # height of the drawn real line

p_interval <- ggplot(intervals) +
  # real line with the true x marked
  annotate("segment", x = -2.4, xend = 2.0, y = line_y, yend = line_y,
           colour = col_ink, linewidth = 0.6,
           arrow = arrow(ends = "both", length = unit(0.12, "inches"))) +
  annotate("segment", x = x0, xend = x0, y = line_y, yend = 3.45,
           colour = col_ink, linetype = "dashed", linewidth = 0.6) +
  annotate("point", x = x0, y = line_y, size = 3.5, colour = col_ink) +
  annotate("text", x = x0, y = line_y - 0.28, label = "x",
           parse = TRUE, size = 6.5, colour = col_ink) +
  #annotate("text", x = 2.05, y = line_y, label = "\u211D",
           #hjust = 0, size = 7, colour = col_ink) +
  # scale bar of length kappa
  #annotate("errorbarh", xmin = -2.3, xmax = -2.3 + kappa, y = 3.55,
           #height = 0.1, linewidth = 0.8, colour = col_ink) +
  geom_errorbarh(aes(xmin = -2.3, xmax = -2.3 + kappa, y = 3.55, colour = col_ink),
                 height = 0.22, linewidth = 1.6) +
  annotate("text", x = -2.3 + kappa / 2, y = 3.85, label = "kappa",
           parse = TRUE, size = 6.5, colour = col_ink) +
  # candidate decisions (intervals)
  geom_errorbarh(aes(xmin = a, xmax = b, y = y, colour = outcome),
                 height = 0.22, linewidth = 1.6) +
  geom_segment(aes(x = a, xend = b, y = y, yend = y, colour = outcome),
               linewidth = 4, alpha = 0.35) +
  geom_text(aes(x = a - 0.12, y = y, label = id), parse = TRUE,
            hjust = 1, size = 6.5, colour = col_ink) +
  geom_label(aes(x = (a + b) / 2, y = y - 0.34, label = why), parse = TRUE,
             size = 5, colour = "grey35", fill = "white", label.size = 0,
             label.padding = unit(0.08, "lines")) +
  geom_text(aes(x = 2.55, y = y, label = rho_lab, colour = outcome),
            parse = TRUE, hjust = 0, size = 6.5, show.legend = FALSE) +
  scale_colour_manual(
    values = c(lost = col_lost, retained = col_retained),
    labels = c(lost = "privacy lost", retained = "privacy retained")
  ) +
  coord_cartesian(xlim = c(-2.6, 4.0), ylim = c(-0.15, 4.0), clip = "off") +
  theme_slide()

# Example 2: negative log-density privacy function -----------------------------
# rho(d, x) = -log d(x), with d a density on R.

dens <- data.frame(
  id = c("d[1]", "d[2]", "d[3]"),
  mu = c(0.15, 1.40, 0.00),
  sd = c(0.30, 0.40, 1.20)
)
dens$fx      <- dnorm(x0, dens$mu, dens$sd)   # d(x)
dens$rho     <- -log(dens$fx)                 # rho(d, x)
dens$rho_lab <- sprintf('rho(%s, x) == "%s"', dens$id,
                        sub("-", "\u2212",
                            formatC(dens$rho, format = "f", digits = 2)))
dens$colour  <- c(col_lost, col_retained, col_mid)
dens$lab_x   <- c(0.62, 1.40, -2.05)          # where to name each curve
dens$lab_y   <- c(1.30, 1.08, 0.20)
dens$col_y   <- c(1.30, 1.10, 0.90)           # rows of the rho column

grid   <- seq(-3, 3.2, length.out = 800)
curves <- do.call(rbind, lapply(seq_len(nrow(dens)), function(i) {
  data.frame(id = dens$id[i], z = grid,
             f  = dnorm(grid, dens$mu[i], dens$sd[i]))
}))

pal <- setNames(dens$colour, dens$id)

p_logdens <- ggplot() +
  geom_area(data = curves, aes(z, f, fill = id), position = "identity",
            alpha = 0.12, show.legend = FALSE) +
  geom_line(data = curves, aes(z, f, colour = id), linewidth = 1.3,
            show.legend = FALSE) +
  annotate("segment", x = x0, xend = x0, y = 0, yend = 1.42,
           colour = col_ink, linetype = "dashed", linewidth = 0.6) +
  # d(x): the height each density puts on the true x
  geom_point(data = dens, aes(x0, fx, colour = id), size = 3.5,
             show.legend = FALSE) +
  geom_text(data = dens, aes(lab_x, lab_y, label = id, colour = id),
            parse = TRUE, size = 6.5, show.legend = FALSE) +
  geom_text(data = dens, aes(1.85, col_y, label = rho_lab, colour = id),
            parse = TRUE, hjust = 0, size = 6, show.legend = FALSE) +
  annotate("text", x = x0, y = -0.09, label = "x", parse = TRUE,
           size = 6.5, colour = col_ink) +
  annotate("segment", x = -3, xend = 3.2, y = 0, yend = 0,
           colour = col_ink, linewidth = 0.6) +
  scale_colour_manual(values = pal) +
  scale_fill_manual(values = pal) +
  coord_cartesian(xlim = c(-3, 4.5), ylim = c(-0.12, 1.45), clip = "off") +
  theme_slide()
