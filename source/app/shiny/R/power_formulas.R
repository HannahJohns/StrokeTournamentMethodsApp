# These sample sizes are in total. Divide by 2 to get per-group sample size
yu_power_generic <- function(p_win,p_loss,p_tie, k=1, alpha, power){

  # k here is proportion assigned to one group, not ratio
  # For consistency with winp, convert from ratio to proportion
  k <- k/(1+k)

  logwr = log(p_win/p_loss)

  s2 = 4*(1+p_tie)/(3*k*(1-k)*(1-p_tie))

  N = s2 * (qnorm(1-alpha/2) + qnorm(power))^2 / (logwr^2)

  return(N)
}

get_sample_size <- function(p0,p1,w,power,alpha, nStrata){

  winP_power <- function(p0, p1, r=1, power,alpha){

    winP <- outer(p0,p1)
    winP <- 0.5*sum(diag(winP))+sum(winP[lower.tri(winP)])

    # Probability that 1 will win against random control
    p_win_1 <- sapply(1:length(p0),function(i){
      if(i<length(p0))
      {
        return(0.5*p0[i] + sum(p0[(i+1):length(p0)]))
      }
      else
      {
        return(0.5*p0[i])
      }
    })

    # Probability that 0 will win against random treatment
    p_win_0 <- sapply(1:length(p1),function(i){
      if(i<length(p1))
      {
        return(0.5*p1[i] + sum(p1[(i+1):length(p1)]))
      }
      else
      {
        return(0.5*p1[i])
      }
    })

    s0 <- sum(p0*(p_win_0-sum(p0*p_win_0))^2)
    s1 <- sum(p1*(p_win_1-sum(p1*p_win_1))^2)

    return(ceiling((r+1)/r * (qnorm(1-alpha/2) + qnorm(power))^2/(log(winP/(1-winP)))^2 * (s1 + r*s0)/(winP*(1-winP))^2))

  }


  # Sample size formula for a win ratio endpoint
  # 10.1002/sim.9297

  yu_power_formula <- function(p0, p1, k=1, alpha, power){


    # yu's formula only asks for win/loss/tie proportions

    winP <- outer(p0,p1)
    p_win <- sum(winP[lower.tri(winP)])
    p_loss <- sum(winP[upper.tri(winP)])
    p_tie <- sum(diag(winP))

    N <- yu_power_generic(p_win,p_loss,p_tie, k=k, alpha=alpha, power=power)

    return(N)
  }


  wmw_power <- function(p0, p1, alpha, power)
  {

    winP <- outer(p0,p1)
    p_win <- sum(winP[lower.tri(winP)])
    p_loss <- sum(winP[upper.tri(winP)])
    p_tie <- sum(diag(winP))

    N <- (qnorm(1-alpha/2) + qnorm(power))^2 / (6* (p_win + 0.5*p_tie-0.5)^2 )

    return(2*N)
  }

  # SAMPLE SIZE CALCULATION FOR THE VAN ELTEREN TEST ADJUSTING FOR TIES
  # 10.1080/10543400802369020

  # For this formula, p0 and p1 should be matrices where the number
  # of rows equals the number of stratum.

  # w will be supplied as the treatment proportion and needs to be modified
  # to match equation 12 of the paper

  vanelteren_power <- function(p0,p1, w, k=1,alpha = 0.05, power = 0.8)
  {

    # Proportion assigned to treatment
    k <- k/(1+k)

    # Convert to proportions in each strata
    s <- w/sum(w)

    # Convert to approximate van elteren weights
    w <- s * k * (1-k)
    w <- w/sum(w)

    do.call("rbind",lapply(1:length(w),function(i){

      winP <- outer(p0[i,],p1[i,])
      p_win <- sum(winP[lower.tri(winP)])
      p_loss <- sum(winP[upper.tri(winP)])
      p_tie <- sum(diag(winP))

      mu1 <- p_win + 0.5*p_tie

      v <- 1/(12*s[i]*k*(1-k)) * (1 - sum(((1-k)*p0 + k*p1)^3))

      # Calculate sample size for effect in each strata while we're at it
      N <-  (qnorm(1-alpha/2) + qnorm(power))^2 * v / (mu1 - 0.5)^2

      c(mu1,v,N)
    })) -> strataVals

    mu1 <- sum(w*strataVals[,1])
    v <- sum(w^2*strataVals[,2])

    N <- (qnorm(1-alpha/2) + qnorm(power))^2 * v / (mu1 - 0.5)^2

    return(N)
  }



  # Tang Y. Size and power estimation for the Wilcoxon-Mann-Whitney test for ordered categorical data. Statistics in Medicine 2011; 30:3461-3470

  tang_power_formula <- function(p0, p1, k=1, alpha, power){

    # This code is ported from the paper, I've left variable names as-is.

    pi1 <- p0
    pi2 <- p1

    p1 <- k/(1+k)
    p2 <- 1-p1

    r1 = cumsum(pi1)-pi1/2;
    s2 = 1-(cumsum(pi2)-pi2/2);

    theta = sum( pi2 * r1) # the # operator in SAS is elementwise multiplication

    tau1 = sum(pi1*s2*s2)-theta*theta
    tau2 = sum(pi2*r1*r1)-theta*theta
    pibar = p1*pi1 + p2*pi2;
    sigma0 = sqrt((1-sum( pibar^3))/(12*p1*p2)); # the ## operator is elementwise power
    sigma1 = sqrt(tau1/p1 + tau2/p2);

    Sigma0OC = 4*sigma0
    Sigma1OC = sigma1/theta/(1-theta)


    SizeEX = (qnorm(1-power) * sigma1 + qnorm(alpha/2)* sigma0)^2/(theta-0.5)^2

    return(unname(SizeEX))

  }


  if(nStrata==1)
  {

    genodds_split.N <- genodds::genodds.power(p0=c(p0), p1=c(p1), power = power, alpha = alpha,ties = "split")
    genodds_drop.N <- genodds::genodds.power(p0=c(p0), p1=c(p1), power = power, alpha = alpha, ties = "drop")
    winP.N <- winP_power(p0=c(p0), p1=c(p1), power = power, alpha = alpha)
    yu.N <- yu_power_formula(p0=c(p0), p1=c(p1), power = power, alpha = alpha)
    wmw.N <- wmw_power(p0=c(p0),p1=c(p1),alpha = alpha, power = power)
    vanelteren.N <- vanelteren_power(p0=p0,p1=p1,w=1,alpha = alpha, power = power)
    tang.N <- tang_power_formula(p0=p0,p1=p1,k=1,alpha = alpha, power = power)

    out <- data.frame(genodds_split.N=genodds_split.N,
                      genodds_drop.N=genodds_drop.N,
                      winP.N,
                      yu=yu.N,
                      # ologit.N,
                      wmw=wmw.N,
                      vanelteren=vanelteren.N,
                      tang=tang.N
    )


    colnames(out) <- c("Gen OR (split ties)",
                       "Gen OR (drop ties)",
                       "WinP",
                       "Win Ratio",
                       "Wilcoxon-Mann-Whitney",
                       "van Elteren",
                       "Tang")

  } else {

    vanelteren.N <- vanelteren_power(p0=p0,p1=p1,w=w,alpha = alpha, power = power)

    out <- data.frame(
      vanelteren=vanelteren.N
    )

    colnames(out) <- "van Elteren"
  }

  for(i in 1:ncol(out)) out[,i] <-2*ceiling(out[,i]/2)

  out
}
