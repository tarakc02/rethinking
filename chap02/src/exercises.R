# vim: set ts=4 sts=0 sw=4 si fenc=utf-8 et:
# vim: set fdm=marker fmr={{{,}}} fdl=0 foldcolumn=4:

# load libs {{{
pacman::p_load(
    argparse,
    dplyr,
    ggplot2,
    purrr
)
# }}}

# helpers {{{

# `measurements` should be T/F or 1/0
binom_grid_approx <- function(measurements, priors) {
    n_succ  <- sum(measurements)
    n_trial <- length(measurements)
    likelihood <- dbinom(n_succ,
                         size = n_trial,
                         prob = priors$p)
    unstd_post <- likelihood * priors$prob
    post <- unstd_post / sum(unstd_post)
    tibble(p = priors$p, prob = post)
}

uniform <- function(grid_size) {
    tibble(
        p = seq(from = 0, to = 1, length.out = grid_size),
        prob = 1
    )
}

binom_plot_estimates <- function(probs) {
    ggplot(probs, aes(x = p, y = prob)) +
        geom_point() + geom_line() +
        theme_bw()
}

# }}}

# M01 {{{

myprior <- uniform(20)

trials <- list(c(T, T, T),
               c(T, T, T, F),
               c(F, T, T, F, T, T, T),
               c(F, T, T, F, T, T, T, T, T, F, T))

map_dfr(trials, binom_grid_approx,
        priors = myprior, .id = "trial") %>%
    binom_plot_estimates + facet_wrap(~trial)

# }}}

# M02 {{{

m02_prior <- tibble(p = seq(0, 1, by=.05)) %>%
    mutate(prob = if_else(p < .5, 0, 2))


m02_estimates <- map_dfr(
    trials, binom_grid_approx,
    priors = m02_prior,
    .id = "trial")

m02_estimates %>% binom_plot_estimates + facet_wrap(~trial)

# }}}

# M03 {{{

# p(land | earth), etc.
p_land_earth <- .3
p_land_mars <- 1
p_earth <- .5
p_mars <- .5

p_land <- p_land_earth * p_earth + p_land_mars * p_mars

p_land_earth * p_earth / p_land

# }}}

# H01 & H02 {{{

# prob that panda = A
update_probs <- function(starting, birth_twins) {
    p_twins_A <- .1
    p_twins_B <- .2

    p_sing_A <- .9
    p_sing_B <- .8

    p_A <- starting
    p_B <- 1 - p_A

    p_twins <- p_A * p_twins_A + p_B * p_twins_B
    p_sing <- p_A * p_sing_A + p_B * p_sing_B

    if (birth_twins) return(p_twins_A * p_A / p_twins)
    return(p_sing_A * p_A / p_sing)
}

update_probs(.5, T)

update_probs(.5, T) %>% update_probs(T) %>% update_probs(F)

# }}}
