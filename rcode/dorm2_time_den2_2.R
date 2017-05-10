library(data.table)
library(doParallel)
library(minpack.lm)
library(pracma)
library(ggplot2)
library(scales)
library(sfsmisc)
library(VGAM)
source("ptsal.R")

beck_model <- function(tvec, b1, xmax) {
  p <- numeric(length(tvec))
  den <- density(b1, n = 1000, to=xmax)
  b <- den$x
  fb <- den$y
  
  for (i in 1:length(tvec)) {
    p[i] <- trapz(b, fb*b*exp(-b*tvec[i]))
  }
  print(trapz(tvec, p))
  return(p)
}


ses.flow <- function(n, lambda) {
  #n <- round(tsim/lambda)
  time <- rexp(n*lambda, lambda)
  #timeline <- cumsum(time)
  return(time)
}

beck_sim <- function(n, lambda) {
  # Calculate the number of cores
  no_cores <- detectCores() - 1
  
  # Initiate cluster
  cl <- makeCluster(no_cores)
  
  # Calculate session flows
  timeline <- clusterMap(cl, ses.flow, lambda,  MoreArgs=list(n=n))
  
  # Stop cluster
  stopCluster(cl)
  
  timeline <- unlist(timeline)
  #  timeline <- sort(timeline)
  #  time.flow <- diff(timeline)
  return(timeline)
}

ftype <- "D"
xmax <- 20

fdate <- c(20150317, 20150318, 20150416, 20150417, 20150418, 20150419, 20160426, 20160427, 20160428, 20160429)
#fdate <- c(20150317, 20150318)
qf <- c(1.12, 1.14, 1.11, 1.12, 1.11, 1.13, 1.12, 1.14, 1.13, 1.12)
#qf <- rep(1.12, 10)
lf <- c(1.24, 1.31, 1.19, 1.24, 1.22, 1.28, 1.21, 1.29, 1.29, 1.2)
rf <- c(0.75, 0.77, 0.73, 0.73, 0.71, 0.76, 0.75, 0.78, 0.75, 0.74)

for (i in 1:10) {
  fyear <- substr(fdate[i],1,4)
  fmonth <- substr(fdate[i],5,6)
  fday <- substr(fdate[i], 7, 8)
  times <-  gsub(":", "", substr(seq(ISOdatetime(fyear,fmonth,fday,0,0,0), ISOdatetime(fyear,fmonth,fday,23,45,0), length.out=96), 12, 16))
  timestamp <- paste(fdate[i], "_", times, sep="")
  fnum <- 96
  infile <- paste(" zcat ses/", ftype, substr(timestamp, 1, 8) ,"/ses_", ftype, timestamp, ".txt.gz", sep="")
  avtime <- numeric(fnum)
  for (j in 1:fnum) {
    data <- fread(infile[j])
    data1 <-  data[with(data, order(-V4)), ]
    data1 <-  data1[with(data1, order(V1)), ]
    var1 <- diff(data1$V1)
    avtime[j] <- mean(var1)
    if (j == 1) {
      var <- var1
    } else {
      var <- c(var, var1)
    }
  }
  nvar <- var/mean(var)
  p <- density(nvar, from = 0.5, to=xmax, n = 100)
  out <- data.frame(x = p$x, p = p$y)
  
 
  # Fit with q-exponential distribution
  #wf <- (seq(1, 10, length.out=length(p$x)))^9
  #fit <- nlsLM(p ~ dts.qexp(x, qf[i], p2), start = list(p2 = 1),
  #             data = out, lower = 0.01, upper = 100, weights = wf)
  #pf <- dts.qexp(out$x, qf[i], coef(fit)[1])
  pf <- dts.qexp(out$x, qf[i], lf[i])
  
  lam1 <- 1/rrayleigh(10000, rf[i])
  x2 <- seq(0.1, 22, length.out = 100)
  pb <- beck_model(x2,lam1, xmax)
  outb <- data.frame(xb=x2, pb=pb)
  
  #lam2 <- 1/rweibull(96, 2, 1.1)
  lam2 <- mean(avtime)/avtime
  svar <- beck_sim(18750, lam2)
  svar <- svar/mean(svar)
  ps <- density(svar, from=0.5, to=xmax, n=100)
  outs <- data.frame(xs=ps$x, ps=ps$y)
  
  
  out <- cbind(out, pf)
  out <- cbind(out, outb)
  out <- cbind(out, outs)
  
  #qf <- round(coef(fit)[1], 2)
  #lf <- round(coef(fit)[2], 2)
  
  qtext <- paste("italic(q) == ", qf[i])
  ltext <- paste("lambda == ", lf[i])
  
  
  # x<- p$x
  # xs <- ps$x
  # xb<- x2
  # circle <- data.frame(x,p$y)
  # write.table(circle,file = paste0('2/',fdate[i],'_circle_r','.txt'), col.names = F, row.names = F)
  # squ <- data.frame(xs,ps$y)
  # write.table(squ,file = paste0('2/',fdate[i],'_square_g','.txt'), col.names = F, row.names = F)
  # bla <- data.frame(x,pf)
  # write.table(bla,file = paste0('2/',fdate[i],'_black_dash','.txt'), col.names = F, row.names = F)
  # blu <- data.frame(xb,pb)
  # write.table(blu,file = paste0('2/',fdate[i],'_blue_line','.txt'), col.names = F, row.names = F)

  
  pl <- ggplot(out)
  pl <- pl + geom_point(aes(x = x, y = p), shape=1, color = "red", size = 3)+
            geom_line(aes(x = x, y = pf), linetype = 2, color = "black", size=2)+
            geom_line(aes(x = xb, y = pb), color = "blue", size=1)+
            geom_point(aes(x = xs, y = ps), shape=0, color = "green3", size=3)+
            scale_x_continuous(limits = c(0, 20))+
            scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                          labels = trans_format("log10", math_format(10^.x)),
                          limits=c(1e-5, 1))+
            theme_bw()+
            theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
                  legend.position="none", text=element_text(size=20), axis.text=element_text(size=20), 
                  axis.title=element_text(size=25, face="bold"))+
            ggtitle(bquote(paste(.(fday), "/", .(fmonth), "/", .(fyear), sep="")))+

            ylab(bquote(paste(~bar(tau), " ", ~italic(p), "(", ~tau, ")", sep=""))) + 
            xlab(bquote(paste(~tau, "/", ~bar(tau))))+
            geom_text(aes(label = qtext, x = 18, y = .8), size=8, parse = TRUE)+
            geom_text(aes(label = ltext, x = 18, y = .3), size=8, parse = TRUE)
  setEPS()
  #postscript(paste("~/Dropbox/paper_new/epl2-author/fig1_", i, ".eps", sep=""))
  postscript(paste("fig1_", i, ".eps", sep=""))
  print(pl)
  dev.off()
  # out2 <- data.frame(avtime)
  # outfile <- paste("avt/avt_", ftype, fdate[i], "_", times[i], ".txt", sep="")
 # write.table(format(out2, digits = 6), outfile, row.names = FALSE, col.names = FALSE, quote = FALSE)
}




