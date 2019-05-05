d <- read.csv("D:/OneDrive/Data/3E test data.csv")
dts <-ts(d[d$PPE.Count < 27,]$Count, start = 2006, frequency = 26)

R_HoltWinters <- function (x, alpha = NULL, beta = NULL, gamma = NULL,  llamda = NULL,
    seasonal = c("additive", "multiplicative"), start.periods = 2, l.start = NULL, b.start = NULL, ll.start = NULL,
    k = 2, ck = 2.52, 
    s.start = NULL, optim.start = c(alpha = 0.3, beta = 0.1, 
        gamma = 0.1, llamda = 0.1), optim.control = list()) 
{
    x <- as.ts(x)
    seasonal <- match.arg(seasonal)
    f <- frequency(x)
    if (!is.null(alpha) && (alpha == 0)) 
        stop("cannot fit models without level ('alpha' must not be 0 or FALSE)")
    if (!all(is.null(c(alpha, beta, gamma, llamda))) && any(c(alpha, 
        beta, gamma, llamda) < 0 || c(alpha, beta, gamma,llamda) > 1)) 
        stop("'alpha', 'beta', 'gamma' and 'llamda' must be within the unit interval")
    if ((is.null(gamma) || gamma > 0)) {
        if (seasonal == "multiplicative" && any(x == 0)) 
            stop("data must be non-zero for multiplicative Holt-Winters")
        if (start.periods < 2) 
            stop("need at least 2 periods to compute seasonal start values")
    }
    if (!is.null(gamma) && is.logical(gamma) && !gamma) {
        expsmooth <- !is.null(beta) && is.logical(beta) && !beta
        if (is.null(l.start)) 
            l.start <- if (expsmooth) 
                x[1L]
            else x[2L]
        if (is.null(b.start)) 
            if (is.null(beta) || !is.logical(beta) || beta) 
                b.start <- x[2L] - x[1L]
        start.time <- 3 - expsmooth
        s.start <- 0
    }
    else {
        start.time = f + 1
        wind <- start.periods*f
        if(is.null(b.start) && is.null(l.start) && is.null(s.start))
        {
	b.start =  sapply(1:wind, function(y) rme(y,wind,x))
    	Int = sapply(1:wind, function(y) (x[y]-b.start*y))
    	l.start = Int + b.start*wind
    	theta = mean(abs(sapply(1:wind, function(y) (x[y]-(Int + b.start*y )))))
    	s.start = sapply(1:f, function(y) median(sapply(0:(start.periods - 1), function(yy) (x[y+yy*f] - (Int + b.start*(y+yy*f)))  )))
	} else
          {theta = 1
            st <- decompose(ts(x[1L:wind], start = start(x), frequency = f),seasonal)
            dat <- na.omit(st$trend)
            m <- lm(dat ~ seq_along(dat))
            if (is.null(l.start)) 
              l.start <- as.vector(coef(m)[1L])
            if (is.null(b.start)) 
              b.start <- as.vector(coef(m)[2L])
            if (is.null(s.start)) 
              s.start <- st$figure
          }
    }
    lenx <- as.integer(length(x))
    if (is.na(lenx)) 
        stop("invalid length(x)")
    dyn.load('D:/OneDrive/Code/Git/DataScience/DataScience/R/HoltWinters/C_R_HoltWinters.dll')
    len <- lenx - start.time + 1
    x_adj = x #added
    hw <- function(alpha, beta, gamma, llamda) {.C("R_HoltWinters", 
		as.double(x), 
		as.double(x_adj),
    lenx,
		as.double(max(min(alpha, 1), 0)), 
		as.double(max(min(beta,1), 0)),
		as.double(max(min(gamma, 1), 0)), 
		as.double(max(min(llamda, 1), 0)), 
		as.integer(start.time), 
    as.integer(!+(seasonal == "multiplicative")),
		as.integer(f), 
    as.integer(!is.logical(beta) || beta), 
		as.integer(!is.logical(gamma) || gamma),
		a = as.double(l.start),
		b = as.double(b.start), 
    s = as.double(s.start), 
		l = as.double(ll.start),
		k = as.double(k),
		ck = as.double(ck),
		theta = as.double(theta),
		SSE = as.double(0),
		level = double(len + 1L),
		trend = double(len + 1L), 
		seasonal = double(len + f))}
    if (is.null(gamma)) {
        if (is.null(alpha)) {
            if (is.null(beta)) {
                  error <- function(p) hw(p[1L], p[2L], p[3L], p[4L])$SSE
                  sol <- optim(optim.start, error, method = "L-BFGS-B", 
                    lower = c(0, 0, 0, 0), upper = c(1, 1, 1, 1), control = optim.control)
                  if (sol$convergence || any(sol$par < 0 | sol$par > 
                    1)) {
                    if (sol$convergence > 50) {
                      warning(gettextf("optimization difficulties: %s", 
                        sol$message), domain = NA)
                    }
                    else stop("optimization failure")
                  }
                  alpha <- sol$par[1L]
                  beta <- sol$par[2L]
                  gamma <- sol$par[3L]
                  llamda <- sol$par[4L]
               
            }
            else {
                error <- function(p) hw(p[1L], beta, p[2L], p[3L])$SSE
                sol <- optim(c(optim.start["alpha"], optim.start["gamma"], optim.start["llamda"]), 
                  error, method = "L-BFGS-B", lower = c(0, 0, 0), 
                  upper = c(1, 1, 1), control = optim.control)
                if (sol$convergence || any(sol$par < 0 | sol$par > 
                  1)) {
                  if (sol$convergence > 50) {
                    warning(gettextf("optimization difficulties: %s", 
                      sol$message), domain = NA)
                  }
                  else stop("optimization failure")
                }
                alpha <- sol$par[1L]
                gamma <- sol$par[2L]
                llamda <- sol$par[3L]
            }
        }
        else {
            if (is.null(beta)) {
                error <- function(p) hw(alpha, p[1L], p[2L], p[3L])$SSE
                sol <- optim(c(optim.start["beta"], optim.start["gamma"], optim.start["llamda"]), 
                  error, method = "L-BFGS-B", lower = c(0, 0, 0), 
                  upper = c(1, 1, 1), control = optim.control)
                if (sol$convergence || any(sol$par < 0 | sol$par > 
                  1)) {
                  if (sol$convergence > 50) {
                    warning(gettextf("optimization difficulties: %s", 
                      sol$message), domain = NA)
                  }
                  else stop("optimization failure")
                }
                beta <- sol$par[1L]
                gamma <- sol$par[2L]
                llamda <- sol$par[3L]
            }
            else {
                error <- function(p) hw(alpha, beta, p[1L], p[2L])$SSE
                sol <- optim(c(optim.start["gamma"], optim.start["llamda"]), 
                             error, method = "L-BFGS-B", lower = c(0, 0), 
                             upper = c(1, 1), control = optim.control)
                if (sol$convergence || any(sol$par < 0 | sol$par > 
                                           1)) {
                  if (sol$convergence > 50) {
                    warning(gettextf("optimization difficulties: %s", 
                                     sol$message), domain = NA)
                  }
                  else stop("optimization failure")
                }
                gamma <- sol$par[1L]
                llamda <- sol$par[2L]
            }
        }
    }
    else {
        if (is.null(alpha)) {
            if (is.null(beta)) {
                error <- function(p) hw(p[1L], p[2L], gamma, p[3L])$SSE
                sol <- optim(c(optim.start["alpha"], optim.start["beta"], optim.start["llamda"]), 
                  error, method = "L-BFGS-B", lower = c(0, 0, 0), 
                  upper = c(1, 1, 1), control = optim.control)
                if (sol$convergence || any(sol$par < 0 | sol$par > 
                  1)) {
                  if (sol$convergence > 50) {
                    warning(gettextf("optimization difficulties: %s", 
                      sol$message), domain = NA)
                  }
                  else stop("optimization failure")
                }
                alpha <- sol$par[1L]
                beta <- sol$par[2L]
                llamds <- sol$par[3L]
            }
            else {
                error <- function(p) hw(p[1L], beta, gamma, p[2L])$SSE
                sol <- optim(c(optim.start["alpha"], optim.start["llamda"]), 
                             error, method = "L-BFGS-B", lower = c(0, 0), 
                             upper = c(1, 1), control = optim.control)
                if (sol$convergence || any(sol$par < 0 | sol$par > 
                                           1)) {
                  if (sol$convergence > 50) {
                    warning(gettextf("optimization difficulties: %s", 
                                     sol$message), domain = NA)
                  }
                  else stop("optimization failure")
                }
                alpha <- sol$par[1L]
                llamds <- sol$par[2L]
            }
        }
        else {
            if (is.null(beta)) {
                error <- function(p) hw(alpha, p[1L], gamma, p[2L])$SSE
                sol <- optim(c(optim.start["beta"], optim.start["llamda"]), 
                             error, method = "L-BFGS-B", lower = c(0, 0), 
                             upper = c(1, 1), control = optim.control)
                if (sol$convergence || any(sol$par < 0 | sol$par > 
                                           1)) {
                  if (sol$convergence > 50) {
                    warning(gettextf("optimization difficulties: %s", 
                                     sol$message), domain = NA)
                  }
                  else stop("optimization failure")
                }
                beta <- sol$par[1L]
                llamds <- sol$par[2L]
                
                
                
                
            }
        }
    }
    final.fit <- hw(alpha, beta, gamma, llamda)
    fitted <- ts(cbind(xhat = final.fit$level[-len - 1], level = final.fit$level[-len - 
        1], trend = if (!is.logical(beta) || beta) 
        final.fit$trend[-len - 1], season = if (!is.logical(gamma) || 
        gamma) 
        final.fit$seasonal[1L:len]), start = start(lag(x, k = 1 - 
        start.time)), frequency = frequency(x))
    if (!is.logical(beta) || beta) 
        fitted[, 1] <- fitted[, 1] + fitted[, "trend"]
    if (!is.logical(gamma) || gamma) 
        fitted[, 1] <- if (seasonal == "multiplicative") 
            fitted[, 1] * fitted[, "season"]
        else fitted[, 1] + fitted[, "season"]
    structure(list(fitted = fitted, x = x, alpha = alpha, beta = beta, 
        gamma = gamma, llamda = llamda, coefficients = c(a = final.fit$level[len + 
            1], b = if (!is.logical(beta) || beta) final.fit$trend[len + 
            1], s = if (!is.logical(gamma) || gamma) final.fit$seasonal[len + 
            1L:f]), seasonal = seasonal, SSE = final.fit$SSE, 
        call = match.call()), class = "HoltWinters")
}


ed = R_HoltWinters(dts, llamda = 0.6)


rme <- function(ind, rng, x){
      #function for repeated mean estimator
      median(
        sapply(1:rng, function(y) ifelse(ind == y,NA,(x[ind] - x[y])/(ind-y)))
        ,na.rm = T)}

      
  
}
