library(data.table)
library(doParallel)
library(minpack.lm)
library(pracma)
library(ggplot2)
library(scales)
library(sfsmisc)
library(VGAM)
source("ptsal.R")

rmixgamma <- function(n, pi, alpha, beta) {
  k <- sample.int(length(pi), n, replace = TRUE, prob = pi)
  rgamma(n, alpha[k], beta[k])
}

beck_model <- function(tvec, b1, xmax) {
  p <- numeric(length(tvec))
  den <- density(b1, n = 1000, to=xmax)
  b <- den$x
  fb <- den$y
  for (i in 1:length(tvec)) {
    p[i] <- trapz(b, fb*b*exp(-b*tvec[i]))
  }
  #print(trapz(tvec, p))
  return(p)
}

ses.flow <- function(n, lambda) {
  time <- rexp(n*lambda, lambda)
  return(time)
}

beck_sim <- function(n, lambda) {
  no_cores <- detectCores()-5
  no_cores <- 2
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

fdate <- c(20170131, 20170201, 20170202, 20170203, 20170204, 
           20170205, 20170206, 20170207, 20170208, 20170209,
           20170210, 20170211, 20170212, 20170213, 20170214)
xmax <- 20
pi <- data.frame(c(0.5053636, 0.5872484), c(0.6400897,0.4403208),c(0.5296933,0.4703067),
                 c(0.4660081, 0.5339919), c(0.3645594, 0.6354406),c(0.4338857,0.5661143),
                 c(0.6255652, 0.5268955), c(0.5949939, 0.4752822),c(0.7162692,0.2837308),
                 c(0.5181168, 0.5517985), c(0.5181168, 0.5188063),c(0.46, 0.5476987),
                 c(0.4524864, 0.6248192), c(0.6486798, 0.3513202),c(0.689257, 0.3757052)
                 )
alpha <- data.frame(c(8.7, 50.27874),c(10.69317,76.37211),c(8.166674,49.07354),
                    c(17.15648, 58.36627), c(8.046744, 21.87396),c(8.151318,26.85464),
                    c(9.133919, 43.1932), c(4.567585, 73.85163),c(5.817031,266.3495),
                    c(10.61204, 29.91483), c(10.61204, 98.8998),c(10.61204,55.85268),
                    c(10.33908,23.84411), c(8.708184, 74.87254),c(6.906357, 66.75411)
                    )
beta <- data.frame(c(10.77, 32.21564),c(11.75811,47.84192),c(9.721238,28.98812),
                   c(22.43574, 37.72854), c(12.19902, 12.37155),c(11.01089,15.33814),
                   c(10.3209, 27.15929), c(4.9094, 36.83688),c(5.911721,131.9837),
                   c(13.39783, 18.63602), c(13.39783, 64.68041),c(13.39783,34.28915),
                   c(13.80668,15.03347), c(9.682648, 38.18783),c(7.267399, 33.77511)
                    )
for (i in (1:15)) {
  fyear <- substr(fdate[i],1,4)
  fmonth <- substr(fdate[i],5,6)
  fday <- substr(fdate[i], 7, 8)
  times <-  gsub(":", "", substr(seq(ISOdatetime(fyear,fmonth,fday,0,0,0), ISOdatetime(fyear,fmonth,fday,23,50,0), length.out=144), 12, 16))
  timestamp <- paste0(fdate[i],"_",times)
  infile <- paste0("../data_ses/src_ses/D",substr(timestamp, 1, 8) ,"/ses_D", timestamp, ".txt", sep="")
  fnum <- 144
  avtime <- numeric(fnum)
  numex <- numeric(fnum)
  for (j in 1:fnum) {
    data <- fread(infile[j])
    data1 <-  data[with(data, order(-V4)), ]
    data1 <-  data1[with(data1, order(V1)), ]
    var1 <- diff(data1$V1)
    avtime[j] <- mean(var1)
    numex[j] <- length(var1)
    if (j == 1) {
      var <- var1
    } else {
      var <- c(var, var1)
    }
  }
  nvar <- var/mean(var)
  p <- density(nvar, from = 0.2, to=xmax, n = 100)
  out <- data.frame(x = p$x, pe = p$y)
  write.table(out,file = paste0('graph/3/data/',fdate[i],'_1_emp.txt'), col.names = F, row.names = F)
 
  wf <- (seq(1, 10, length.out=length(p$x)))^10
  fit <- nlsLM(pe ~ dts.qexp(x, p1, p2), start = list(p1 = 1, p2 = 1),
               data = out, lower = c(0.01, 0.01), upper = c(100, 100), weights = wf)
  pq <- dts.qexp(out$x, coef(fit)[1], coef(fit)[2])
  qtext <- paste("italic(q) == ", round(coef(fit)[1], 2))
  ltext <- paste("lambda == ", round(coef(fit)[2], 2))
  outq <- data.frame(xq = out$x, pq = pq)
  write.table(outq,file = paste0('graph/3/data/',fdate[i],'_2_eqfit.txt'), col.names = F, row.names = F)
  out <- cbind(out,outq)

  lam1<- rmixgamma(1000, pi[,i], alpha[,i], beta[,i])
  lam1 <- lam1/mean(lam1)
  lami <- 1/lam1
  x2 <- seq(0.1,20, length.out = 100)
  pb <- beck_model(x2,lam1,xmax)
  outb <- data.frame(xb=x2, pb=pb)
  write.table(outb,file = paste0('graph/3/data/',fdate[i],'_3_trapz.txt'), col.names = F, row.names = F)
  out <- cbind(out,outb)
  #l <- data.frame(beta = lam1)
  #write.table(l,file = 'beta.txt', col.names = F, row.names = F)
  #li <-data.frame(tau = lami)
  #write.table(li,file = 'tau.txt', col.names = F, row.names = F)
  
  bss1 <- beck_sim(18000, lam1)
  bss<- bss1/mean(bss1)
  pr <- density(bss, from=0.2, to=xmax, n=100)
  outr <- data.frame(xr=pr$x, pr=pr$y)
  write.table(outr,file = paste0('graph/3/data/',fdate[i],'_4_surflow.txt'), col.names = F, row.names = F)
  out <- cbind(out,outr)

  lam2<- mean(avtime)/avtime
  bse1 <- beck_sim(18000, lam2)
  bse<- bse1/mean(bse1)
  pbse <- density(bse,from=0.2, to=xmax, n=100)
  outbse <- data.frame(xbse=pbse$x, pbse=pbse$y)
  write.table(outbse,file = paste0('graph/3/data/',fdate[i],'_5_empflow.txt'), col.names = F, row.names = F)
  out <- cbind(out,outbse)
  
  pl <- ggplot(out)
  pl <- pl + geom_point(aes(x = x, y = pe), shape=1, color = "red", size = 3)+
    geom_line(aes(x = xq, y = pq), color = "red", size=1)+
    geom_point(aes(x = xbse, y = pbse), color = "green", size=3,shape=5)+                     
    geom_line(aes(x = xb, y = pb), color = "black", size=1)+
    geom_point(aes(x = xr, y = pr), color = "black", size=3,shape=2)+
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
    geom_text(aes(label = qtext, x = 14, y = .8), size=8, parse = TRUE)+
    geom_text(aes(label = ltext, x = 14, y = .3), size=8, parse = TRUE)
  setEPS()
  postscript(paste("fig1_", i, ".eps", sep=""),colormodel="rgb")
  print(pl)
  dev.off()
  print(i)
}

