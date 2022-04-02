library(gtools)
library(ggplot2)
library(viridis)
library(ggpubr)
boxplot.custom <- function(dat, value, name, title = "", x_lab=NULL){
  if (is.null(x_lab)){x_lab = name}
  dat %>%
    ggplot( aes_string(x=name, y=value, fill=name)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    geom_jitter(color="black", size=0.01, alpha=0.9) +
    theme(
      legend.position="right",
      plot.title = element_text(size=13)
    ) +
    ggtitle(title) +
    ylab(value) + xlab(x_lab)
}
generate_table <- function(strat_by, constant, dat){
  p <- data.frame()
  p <- dat[constant]
  
  P <- data.frame()
  for (i in strat_by){
    temp <- cbind(p, 
                  dat[i], 
                  rep(i, nrow(dat)))
    colnames(temp) <- c(constant, "Value", "type")
    P <- rbind(P,temp)
  }
  return(P)
}
find_median <- function(var_name = c("log_hsCRPmgL_Comb", "log_Gal3ngmL", "log_CystCmgL")){
  rt <- c()
  for (j in var_name){
    for (i in names(table(dat1['type']))){
      rt <- c(rt, median(dat1[which(dat1['type']==i), j], na.rm = T))
    }
  }
  return(rt)
}

my.render.cont <- function(x) {
  if(median(x, na.rm = T) %in% med){
    with(stats.apply.rounding(stats.default(x), digits = 5, digits.pct=1), 
         c("", "Median [Q1, Q3]"=sprintf("%.2f [%.2f; %.2f]", as.numeric(MEDIAN), as.numeric(Q1), as.numeric(Q3))))
  }else{
    with(stats.apply.rounding(stats.default(x), digits = 5, digits.pct=1), 
         c("", "Mean (SD)"=sprintf("%.2f (&plusmn; %.2f)", as.numeric(MEAN), as.numeric(SD))))}
}
my.render.cat <- function(x) {
  #print(length(stats.default(x)))
  if (length(stats.default(x)) < 3){
    c("", sapply(stats.default(x)[2], function(y) with(y,sprintf("%d (%0.0f %%)", FREQ, PCT))))
  }
  else{
    c("", sapply(stats.default(x), function(y) with(y,sprintf("%d (%0.0f %%)", FREQ, PCT))))
  }
}
pvalue <- function(x, ignore = NULL, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.null(ignore)){
    y <- unlist(y[-which(g%in%ignore)])
    g <- factor(g[-which(g%in%ignore)])
  }
  if (is.numeric(y)) {
    if (length(table(g)) > 2){
      p <- unlist(anova(aov(y ~ g))$`Pr(>F)`)[1]
    }
    else{
      # For numeric variables, perform a standard 2-sample t-test
      p <- t.test(y ~ g)$p.value
      if (median(x[[1]], na.rm = T) %in% med){
        p <- wilcox.test(y ~ g)$p.value
      }
    }
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g), simulate.p.value = TRUE)$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

lm_univariable <- function(outcome, explanatory, dat, caption="No Caption"){
  P <- data.frame()
  p <- data.frame()
  pack_row <- c()
  hdr <- c(" ", rep(2, length(outcome)))
  names(hdr) <- c("", outcome)
  for (i in outcome){
    for (j in explanatory){
      m <- lm(unlist(dat[i]) ~ unlist(dat[j]))
      s <- as.data.frame(coef(summary(m)))
      s <- s[-1,-c(2,3)]
      r_names <- rownames(s)
      r_names <- gsub("unlist(dat[j])", "",  r_names, fixed = T)
      if ("" %in% r_names){
        r_names <- j
      }
      rownames(s) <- r_names
      s <- round(s, 4)
      p <- rbind(p, s)
      temp <- length(r_names)
      names(temp) <- j
      pack_row <- c(pack_row, temp)
    }
    if (which(outcome == i) == 1){
      P <- p
    }
    else{
      P <- cbind(P,p)
    }
    p <- data.frame()
  }
  pack_row <- pack_row[1:length(explanatory)]
  P <- as.matrix(P)
  kbl(P, centering = "c", caption = caption) %>% 
    column_spec(1:ncol(P), width = "1.2in")%>%
    pack_rows(index = pack_row)%>%
    add_header_above(hdr)%>%
    kable_classic("striped", full_width = F, html_font = "Arial")
}

surv_univariable <- function(dat, vars, outcome=list(c("timetoMI_CVdeath","MI_CVdeath"), c("timetoMI_CVdeath_CHF", "MI_CVdeath_CHF")), covariates = NULL, 
                             show = NULL, get_report = FALSE, caption=NULL){
  require(survminer)
  require(survival)
  pack_row <- c()
  hdr <- c(" ", rep(3, length(outcome)))
  names(hdr) <- c("", sapply(outcome, "[[", 2))
  P <- data.frame()
  P1 <- data.frame()
  counter_outcome <- 0
  for (j in outcome){
    for (i in vars){
      fit <- coxph(as.formula(paste(
        paste("Surv(", j[1], ",", j[2], ")", sep=""), 
        paste(c(i, covariates), collapse = "+"), sep = "~")), data = dat)
      
      show_idx <- grepl(i, rownames(coef(summary(fit))))
      if (!is.null(show)){for (x in show){show_idx <- show_idx | grepl(x, rownames(coef(summary(fit))))}}
      n <- which(show_idx)
      #print(n)
      p <- data.frame(coef(summary(fit)))[,-c(1,3,4)]
      conf <- round(exp(data.frame(confint(fit))),2)
      colnames(p) <- c("expcoef","pval"); colnames(conf) <- c("lower", "upper")
      p <- cbind(round(p,2), conf, p['pval'])[n,-2]
      #print(p)
      r <- cbind(p[,1],paste("[",p[,2],",",p[,3],"]",sep = ""), round(p[,4],3))
      colnames(r) <- c("Hazard ratio", "95% Confidence Interval", "P-Value")
      rownames(r) <- rownames(p)
      temp <- length(rownames(r))
      names(temp) <- paste("~", paste(c(i, covariates), collapse = " + "))
      pack_row <- c(pack_row, temp)
      r <- as.matrix(r)
      P <- as.matrix(rbind(P, r))
    }
    if (counter_outcome == 0){P1 <- P} else{P1 <- cbind(P1,P)}
    P <- data.frame()
    counter_outcome <- counter_outcome+1
  }
  P <- P1
  pack_row <- pack_row[1:length(vars)]

  if (get_report == FALSE){return(P)}
  else{
    additional_columnspec <- c()
    for (z in 1:length(outcome)){additional_columnspec <- c(additional_columnspec, paste('column_spec(', (3*z+1):(3*z+1), ',background = ifelse(P[,', 3*z, ']<0.05, "lightgreen",""))', sep = ""), ifelse(0>1, paste('column_spec(', (3*z-1), ',border_left=TRUE)', sep = ""), NA))}
    additional_columnspec <- additional_columnspec[!is.na(additional_columnspec)]
    additional_columnspec <- paste(additional_columnspec, collapse = "%>%")
    if (is.null(caption)){caption = paste("<b>Main vars:</b> ", paste(vars, collapse = ", "), "; <b>Covariates:</b> ", paste(covariates, collapse = " + "), sep="")}
    else {caption = paste(caption, "<br>  <b>Main vars:</b> ", paste(vars, collapse = ", "), "; <b>Covariates:</b> ", paste(covariates, collapse = " + "), sep="")}
    eval(parse(text=paste('kbl(P, centering = "r", caption = caption)%>%
      pack_rows(index = pack_row) %>%
      add_header_above(hdr)%>%
      column_spec(1:ncol(P), width = "1.5in")%>%
      kable_classic("hover",full_width = F, html_font = "Arial")%>%', additional_columnspec)))
  }
}
#surv_univariable(dat=dat1, vars = "type", covariates = c("age", "male", "Black"), show = c("age", "male", "Black"), get_report = T)
#surv_univariable(dat=dat1, vars = c("age", "male", "Black"), covariates = "type", show = "type", get_report = T)

graph_HR_table <- function(P, caption = "", base_ncol = 3, width="1.5in"){
  n <- ncol(P) / base_ncol
  
  series <- rep(1:n,each=2)*3
  plus <- seq(from=1, to=2*n, by=2)+1
  series[plus] <- series[plus]+1
  
  hdr <- c(" ", 3, 3)
  names(hdr) <- c("", 
                  "MI_CVdeath",
                  "MI_CVdeath_CHF")
  
  kbl(P, centering = "r", caption = caption) %>%
    
    column_spec(1:ncol(P), width = width)%>%
    column_spec(4:4, background = ifelse(P[,3]<0.05, "lightgreen",""))%>%
    column_spec(7:7, background = ifelse(P[,6]<0.05, "lightgreen",""))%>%
    add_header_above(hdr)%>%
    kable_classic("striped", full_width = F, html_font = "Arial")
}

chart <- function (R, histogram = TRUE, method = c("pearson", "kendall", "spearman"), ...) 
{
  require(PerformanceAnalytics)
  x = checkData(R, method = "matrix")
  if (missing(method)) 
    method = method[1]
  cormeth <- method
  panel.cor <- function(x, y, digits = 2, prefix = "", use = "pairwise.complete.obs", 
                        method = cormeth, cex.cor, ...) {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y, use = use, method = method)
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (missing(cex.cor)) 
      cex <- 0.8/strwidth(txt)
    test <- cor.test(as.numeric(x), as.numeric(y), method = method)
    Signif <- symnum(test$estimate, corr = FALSE, na = FALSE, 
                     cutpoints = c(0, 0.3, 0.5, 0.7, 1), symbols = c(" ", "*", "**", "***"))
    text(0.5, 0.5, txt, cex = cex * (abs(r) + 0.3)/1.3)
    text(0.8, 0.8, Signif, cex = cex, col = 2)
  }
  f <- function(t) {
    dnorm(t, mean = mean(x), sd = sd.xts(x))
  }
  dotargs <- list(...)
  dotargs$method <- NULL
  rm(method)
  hist.panel = function(x, ... = NULL) {
    par(new = TRUE)
    hist(x, col = "light gray", probability = TRUE, axes = FALSE, 
         main = "", breaks = "FD")
    lines(density(x, na.rm = TRUE), col = "red", lwd = 1)
    rug(x)
  }
  if (histogram) 
    pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor, 
          diag.panel = hist.panel)
  else pairs(x, gap = 0, lower.panel = panel.smooth, upper.panel = panel.cor)
}
