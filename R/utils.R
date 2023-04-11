# add prediction to a dataframe
add_predict <- function(data, fit, ...){
    pr <- predict(fit, newdata = data, ...)
    tibble::tibble(cbind(data, data.frame(pr)))
}

# print model equation with generic or actual values
# thanks to https://stats.stackexchange.com/a/433060
model_equation <- function(model, values = NULL, ..., only_print = FALSE) {
    format_args <- list(...)
    model_coeff <- model$coefficients
    model_coeff <- round(model_coeff, 3)
    format_args$x <- abs(model_coeff)
    model_coeff_sign <- sign(model_coeff)
    model_coeff_prefix <- case_when(model_coeff_sign == -1 ~ " - ",
                                    model_coeff_sign == 1 ~ " + ",
                                    model_coeff_sign == 0 ~ " + ")
    nms <- names(model_coeff[-1])
    # check if there are values to print
    if(!is.null(values)){
        # check if there are too much values to print
        if(length(values[[1]]) > 3){
            # formatting
            values <- lapply(values, function(x) sprintf("[%s ... %s]", x[1], x[length(x)]))
        }
        
        nms <- purrr::reduce2(names(values), 
                              as.character(values), 
                              .init = nms, 
                              stringr::str_replace)
    }
    
    
    y <- strsplit(as.character(model$call$formula), "~")[[2]]
    b0 <- paste0(ifelse(model_coeff[1] < 0, "-", ""), do.call(format, format_args)[1])
    bs <- paste0(model_coeff_prefix[-1],
                 do.call(format, format_args)[-1],
                 "*",
                 cli::col_blue(nms),
                 sep = "", collapse = "")
    
    model_eqn <- sprintf("%s ~ %s%s", cli::col_green(y), b0, bs)
    cat(model_eqn)
    if(!only_print){
        invisible(model_eqn)
    }
}

# predict values and print model equation
epredict <- function(fit, values, ...){
    message(model_equation(fit, values, only_print = TRUE))
    pr <- predict(fit, values, ...)
    as.numeric(sapply(pr, unname))
}

lgt <- function(letter){
    sprintf("$log(\\frac{%s}{1 - %s})$", letter, letter)
}

invlgt <- function(letter){
    sprintf("$\\frac{e^{%s}}{1 + e^{%s}}$", letter, letter)
}

odds <- function(p){
    p / (1 - p)
}

odds_ratio <- function(pn, pd){
    odds(pn) / odds(pd)
}

plot_param <- function(fit, b){
    fits <- broom::tidy(fit, conf.int = TRUE)
    fits <- fits[fits$term == b, ]
    ci <- c(fits$estimate - fits$std.error * abs(qnorm(0.025)),
            fits$estimate + fits$std.error * abs(qnorm(0.025)))
    m <- fits$estimate[1]
    se <- fits$std.error[1]
    title <- sprintf("Sampling distribution of $\\beta$ [%s]", b)
    out <- ggnorm(m, se) +
        stat_function(fun = dnorm,
                      geom = "area",
                      args = list(mean = m, sd = se),
                      fill = "lightblue",
                      alpha = 0.5,
                      xlim = ci) +
        ggtitle(latex2exp::TeX(title)) +
        geom_segment(x = ci, y = c(0, 0),
                     xend = ci, yend = dnorm(ci, m, se)) +
        xlab("x")
    if(!(0 < (m - se*5) | 0 > (m + se*5))){
        out +
            geom_vline(xintercept = 0, color = "firebrick3", linewidth = 1)
    }else{
        out
    }
}

ggnorm <- function(mean, sd){
    range <- c(mean - sd*5, mean + sd*5)
    ggplot(data = data.frame(x = range)) +
        stat_function(fun = dnorm, args = list(mean = mean, sd = sd)) +
        ylab("Density") +
        xlim(range)
}

# convert xtable to df (tibble)
ct_to_df <- function(ct){
    df <- data.frame(ct)
    names(df)[length(names(df))] <- "n"
    tibble::tibble(df)
}

# get the probability at the numerator
# of an odds ratio given the probability
# at the denominator and the desired odds
# ratio

pn_from_or <- function(pd, or){
    (or * pd) / (pd * (or - 1) + 1)
}

pdiff <- function(p1, p2, n1, n2 = NULL){
    if(is.null(n2)){
        n2 <- n1 
    }
    pd <- p1 - p2
    se <- sqrt((p1 * (1 - p1))/n1 + (p2 * (1 - p2))/n2)
    ci <- pd + se * qnorm(c(0.025, 0.975))
    cat(sprintf("p1 - p2 = %.3f (SE = %.3f)\n95%% CI = [%.3f, %.3f])",
                pd, se, ci[1], ci[2]))
}

bin_to_binary <- function(data, nc, nt){
    nt <- substitute(nt)
    nc <- substitute(nc)
    nts <- subset(data, select = eval(nt), drop = TRUE)
    ncs <- subset(data, select = eval(nc), drop = TRUE)
    drep <- data[rep(seq_len(nrow(data)), nts), ]
    y <- lapply(1:nrow(data), function(i){
        rep(c(1, 0), c(ncs[i], nts[i] - ncs[i]))
    })
    drep$y <- unlist(y)
    rownames(drep) <- NULL
    tibble::tibble(drep[, c("y", names(data))])
}

binary_to_bin <- function(data, y, ...){
    y <- rlang::enexpr(y)
    dots <- rlang::enexprs(...)
    data |> 
        group_by(!!!dots) |> 
        summarise(nc = sum(!!y),
                  nf = n() - nc,
                  nt = n())
}
