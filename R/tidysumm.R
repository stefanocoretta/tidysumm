tidysumm <- function(model, conf_level = 0.95) {
  if (!inherits(model, "brmsfit")) {
    cli::cli_abort("The 'model' object is not a brms fit.")
  }

  m_summ <- summary(model)
  m_form <- utils::capture.output(print(m_summ[['formula']][['formula']], wsp = 9))[1]
  m_fam <- m_summ[['formula']][['family']][['family']]
  m_data <- m_summ[["data_name"]]
  m_chains <- m_summ[["chains"]]
  m_iter <- m_summ[["iter"]]
  m_warm <- m_summ[["warmup"]]

  txt_family <- glue::glue("{crayon::red(cli::symbol$circle_filled)} Family  {crayon::green(cli::symbol$arrow_right)}   {stringr::str_to_sentence(m_fam)}")
  txt_formula <- glue::glue("{crayon::red(cli::symbol$circle_filled)} Formula {crayon::green(cli::symbol$arrow_right)}   {m_form}")
  txt_data <- glue::glue("{crayon::red(cli::symbol$circle_filled)} Data    {crayon::green(cli::symbol$arrow_right)}   {m_data}")
  txt_draws <- glue::glue("{crayon::red(cli::symbol$circle_filled)} Draws   {crayon::green(cli::symbol$arrow_right)}   {crayon::blue(m_chains)} chains, each with {crayon::blue(m_iter)} iterations ({crayon::blue(m_warm)} warmup); {crayon::blue(m_chains * m_iter - m_chains * m_warm)} post-warmup draws")

  cli::cli_h1("BRM summary")
  cli::cli_h2("Model specification")
  cli::cli_verbatim(txt_family)
  cli::cli_verbatim(txt_formula)
  cli::cli_verbatim(txt_data)
  cli::cli_verbatim(txt_draws)

  m_data <- model[["data"]]
  m_preds <- insight::find_predictors(model, flatten = TRUE)
  m_tidy <- broom.mixed::tidy(model, conf.level = conf_level)
  txt_conf_level <- paste0(conf_level * 100, "%")

  cli::cli_h2("Population-level predictors")
  cli::cli_text("{crayon::blue(cli::symbol$circle_filled)} Baseline")
  d <- cli::cli_div(
    class = "example",
    theme = list(.example = list(`margin-left` = 5))
  )
  cli::cli_text("{.emph - Numeric predictors = 0 and categorical predictors = reference level}")
  int_est <- dplyr::filter(m_tidy, term == "(Intercept)") %>% dplyr::pull(estimate) %>% round(., 2)
  int_err <- dplyr::filter(m_tidy, term == "(Intercept)") %>% dplyr::pull(std.error) %>% round(., 2)
  int_lci <- dplyr::filter(m_tidy, term == "(Intercept)") %>% dplyr::pull(conf.low) %>% round(., 2)
  int_uci <- dplyr::filter(m_tidy, term == "(Intercept)") %>% dplyr::pull(conf.high) %>% round(., 2)
  cli::cli_text("- Estimate: {crayon::red(int_est)}, SD: {crayon::red(int_err)}")
  cli::cli_text("- {txt_conf_level} CrI (quantile): [{crayon::red(int_lci)}, {crayon::red(int_uci)}]")
  cli::cli_end(d)
}
