
## Analysis functions ######################################################

# Analysis code goes here

# Genodds is effectively implemented here a second time so we don't
# have to generate long-form data as required by the package.
# Way more efficient this way.

genodds <- function(xtab,contr_fav=0.5,assume_no_effect=F, alpha=0.05){

  genodds_strata <- function(crosstab,contr_fav,assume_no_effect, alpha)
  {

    # This is for higher score being better, not lower score being better.
    # Reverse mRS order here to work around this

    crosstab <- crosstab[nrow(crosstab):1,]

    crosstab <- as.matrix(crosstab)

    N=sum(crosstab)
    p=crosstab/N

    # print(class(p))

    Rt=p[,2:1]
    Rs=genodds:::get_Rs(p)
    Rd=genodds:::get_Rd(p)
    # Redistribute ties

    # print(crosstab)

    if(!is.na(contr_fav))
    {
      Rs=Rs+(1-contr_fav)*Rt
      Rd=Rd+contr_fav*Rt
    }

    Pc=sum(p*Rs)
    Pd=sum(p*Rd)

    odds=Pc/Pd

    SEodds=2/Pd*(sum(p*(odds*Rd-Rs)^2)/N)^0.5
    SElnodds=SEodds/odds

    # Smooth p across groups and do this again.

    p=outer(apply(p,1,sum),apply(p,2,sum))

    Rt=p[,2:1]
    Rs=genodds:::get_Rs(p)
    Rd=genodds:::get_Rd(p)

    # Redistribute ties

    if(!is.na(contr_fav))
    {
      Rs=Rs+(1-contr_fav)*Rt
      Rd=Rd+contr_fav*Rt
    }

    Pc=sum(p*Rs)
    Pd=sum(p*Rd)

    SEnull=2/Pd*(sum(p*(1*Rd-Rs)^2)/N)^0.5
    SElnnull=SEnull/1

    SE <- ifelse(assume_no_effect,SElnnull,SElnodds)

    conf.int=exp( qnorm(c(alpha/2,1-alpha/2),mean=log(odds),sd=SE) )
    p=pnorm(abs(log(odds)),sd=SE,lower.tail=FALSE)*2

    out=list(odds=odds,conf.int=conf.int,p=p,SEodds=SEodds,SEnull=SEnull,
             xtab=crosstab)

    return(out)
  }

  results=by(xtab[,c("control","treatment")],
             xtab$strata,
             genodds_strata,
             contr_fav=contr_fav,assume_no_effect=assume_no_effect, alpha=alpha
  )

  return(results)
}


# Produce a statement that can be copy-pasted into
# a manuscript
howard_statement <- function(xtab,c_label,t_label){




  out <- by(xtab,xtab$strata, function(x, nStrata,c_label,t_label){

    p0 <- x[,"control"]
    p1 <- x[,"treatment"]

    p0 <- p0/sum(p0)
    p1 <- p1/sum(p1)

    proportions <- outer(p0,p1)

    p_win <- sum(proportions[lower.tri(proportions)])*100
    p_loss <- sum(proportions[upper.tri(proportions)])*100
    p_tie <- sum(diag(proportions))*100

    paste0(
      ifelse(nStrata>1,
             sprintf("Of 100 patients given %s instead of %s in the %s subgroup, ",t_label, c_label, unique(x$strata)),
             sprintf("Of 100 patients given %s instead of %s,",t_label,c_label)
      ),
      sprintf(" %2.2f will have a better outcome with %s ",p_win,t_label),
      sprintf("while %2.2f will have a better outcome with %s. ",p_loss,c_label),
      sprintf("%2.2f appear the same with either treatment.",p_tie)
    )
  },
  nStrata = length(unique(xtab$strata)),
  c_label=c_label,t_label=t_label)

}
