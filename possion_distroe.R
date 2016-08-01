
## assume y follows a possion distribution with mean  mu (Y ~ Poisson(µ))
## and the log of mu follows linear model with intercept ß0 and slope ß1, so x is one of our predictors (log µ = ß0 + ß1x)
## assuming values for ß1 and ß0, how do simulate from this data to get poisson model (use rpois)

possion_distro<- function(obs, incep, slop){ ##incep is interecept B0, and slop is slope B1
        linear_summary <- match.fun(linear_summary)
        set.seed(1)
        x <- rnorm(obs)
        log.mu <- incep + slop * x ## generate linear predictor log µ by adding intercept and slope coefficent by x
        y <- rpois(obs, exp(log.mu)) ## in order to get mean from the log mu, we need to exponentiate the log mu, in the rpois
        plot(x,y)
        ysum <- summary(y)
        print(ysum)
}