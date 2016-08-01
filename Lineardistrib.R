linear_summary<- function(obs, noise, ...){ ## noise is first ... is intercept and  second ... is slope
        linear_summary <- match.fun(linear_summary)
        set.seed(20)
        x <- rnorm(obs)
        e <- rnorm(obs, 0, noise)
        j <- as.numeric(...)
        y <- j[1] + j[2] * x + e
        plot(x,y)
        ysum <- summary(y)
        print(ysum)
}