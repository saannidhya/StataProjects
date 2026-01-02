# Helper: safely build formula terms that exist in the data -----------------
vars_exist <- function(data, vars) vars[vars %in% names(data)]

# Helper: linear combination (estimate + se + p) ----------------------------
lincom <- function(mod, w) {
  # w: named numeric vector of weights for coefficients (names = coef names)
  b  <- coef(mod)
  V  <- vcov(mod)
  wv <- w[names(w) %in% names(b)]
  bv <- b[names(wv)]
  if(length(wv) == 0) return(tibble(estimate = NA_real_, se = NA_real_, p = NA_real_))
  est <- sum(wv * bv)
  se  <- sqrt(as.numeric(t(wv) %*% V[names(wv), names(wv), drop = FALSE] %*% wv))
  # Use t with residual df if present; else normal
  df  <- tryCatch(mod$nobs - mod$nit, error = function(.) NA_real_)
  # p-val (approx)
  p <- 2 * (1 - pnorm(abs(est / se)))
  tibble(estimate = est, se = se, p = p)
}

stars_p <- function(p) ifelse(is.na(p), "",
                       ifelse(p < .01, "***",
                       ifelse(p < .05, "**",
                       ifelse(p < .10, "*", ""))))

# Helper: average windows like Stata `effects` and `effects_first` ----------
avg_window <- function(mod, groups) {
  # groups = list(short = c("D1","D2","D3","D4"), med = c("D5",...))
  bind_rows(lapply(names(groups), function(g) {
    cs <- groups[[g]]
    w  <- setNames(rep(1/length(cs), length(cs)), cs)
    out <- lincom(mod, w) %>% mutate(window = g)
    out
  })) %>%
    select(window, estimate, se, p)
}

# Helper: fetch weighted mean of dependent var on model sample -------------
weighted_mean_on_sample <- function(mod, varname, wvar) {
  df <- model.frame(mod)
  if(!(varname %in% names(df))) return(NA_real_)
  w <- if (wvar %in% names(df)) df[[wvar]] else rep(1, nrow(df))
  sum(df[[varname]] * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
}

# Plot helpers --------------------------------------------------------------
plot_event_stem <- function(df, title, ylab, outpath, level = .90) {
  # df: k, b, se; keep -5:-1,0,1:10 (single-interval panel)
  z <- qnorm(1 - (1 - level)/2)
  gg <- df %>%
    mutate(lo = b - z*se, hi = b + z*se) %>%
    ggplot(aes(k, b)) +
    geom_hline(yintercept = 0, color = "grey70") +
    geom_vline(xintercept = 0, color = "red", alpha = .6) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = ylight, alpha = .7) +
    geom_line(color = yaleblue, linewidth = 1) +
    geom_point(color = yaleblue) +
    scale_x_continuous(breaks = -5:10) +
    labs(x = "Time since bond passage", y = ylab, title = title)
  ggsave(outpath, gg, width = 7, height = 4.2, dpi = 300)
}

plot_event_dual <- function(df_yearly, df_cum, title, ylab1, ylab2, outpath) {
  # df_yearly: k, b, se ; df_cum: k, b, se  (we render same scale; labels note the series)
  z <- qnorm(0.975)
  gg <- ggplot() +
    geom_hline(yintercept = 0, color = "grey70") +
    geom_vline(xintercept = 0, color = "red", alpha = .6) +
    # yearly
    geom_errorbar(data = df_yearly %>% mutate(lo = b - z*se, hi = b + z*se),
                  aes(k, ymin = lo, ymax = hi), width = .15, color = ylb, alpha = .6) +
    geom_line(data = df_yearly, aes(k, b), color = ylb, linewidth = 1) +
    geom_point(data = df_yearly, aes(k, b), color = ylb) +
    # cumulative
    geom_ribbon(data = df_cum %>% mutate(lo = b - z*se, hi = b + z*se),
                aes(k, ymin = lo, ymax = hi), fill = yo, alpha = .20) +
    geom_line(data = df_cum, aes(k, b), color = yo, linewidth = 1) +
    scale_x_continuous(breaks = -5:10) +
    labs(x = "Time since bond passage",
         y = paste0(ylab1, " (yearly)  /  ", ylab2, " (cumulative)"),
         title = title)
  ggsave(outpath, gg, width = 7, height = 4.2, dpi = 300)
}

# Event-time grid and coefficient extraction for -5..-1,0,1..10
get_coef <- function(mod, name) {
co <- tidy(mod, conf.int = TRUE)
co %>% filter(term == name) %>% select(estimate, std.error) %>%
    rename(b = estimate, se = std.error)
}
