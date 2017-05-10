library(data.table)
library(doParallel)
library(minpack.lm)
library(pracma)
library(ggplot2)
library(scales)
library(sfsmisc)
library(VGAM)
library(mixtools)
source("ptsal.R")
require(mixtools)

fdate <- c(20170208)

for (i in 1:1) {
  print(paste0('date ',i,': ',fdate[i]))
  fyear <- substr(fdate[i],1,4)
  fmonth <- substr(fdate[i],5,6)
  fday <- substr(fdate[i], 7, 8)
  times <-  gsub(":", "", substr(seq(ISOdatetime(fyear,fmonth,fday,0,0,0), ISOdatetime(fyear,fmonth,fday,23,50,0), length.out=144), 12, 16))
  timestamp <- paste0(fdate[i],"_",times)
  infile <- paste0("../data_ses/src_ses/D",substr(timestamp, 1, 8) ,"/ses_D", timestamp, ".txt", sep="")
  fnum <- 144
  avtime <- numeric(fnum)
  for (j in 1:fnum) {
    if (file.size(infile[j])>0){
      data <- fread(infile[j])
      var1 <- diff(data$V1)
      avtime[j] <- mean(var1)
    } else{
      avtime[j] <- avtime[j-1]
    }
  }
  nvar <- mean(avtime)/avtime
  co <- sqrt(var(nvar))/mean(nvar)
  print(paste0('Coef. of var: ',round(co, 3)))
  p <- density(nvar, n=300)
  p$y <- p$y/trapz(p$x,p$y)
  out <- data.frame(x=p$x,y=p$y)
  write.table(out,file = paste0('graph/mixfit/',fdate[i],'_1_pbeta.txt'), col.names = F, row.names = F)
  
  g3 <- gammamixEM(nvar)
  x <- seq(min(nvar), max(nvar), 0.00001)
  d3 <- function(x) g3$lambda[1]*dgamma(x, g3$gamma.pars[1], 1/g3$gamma.pars[2]) + g3$lambda[2]*dgamma(x, g3$gamma.pars[3], 1/g3$gamma.pars[4])
  print(g3$lambda[1])
  print(g3$gamma.pars[1])
  print(1/g3$gamma.pars[2])
  print(g3$lambda[2])
  print(g3$gamma.pars[3])
  print(1/g3$gamma.pars[4])
  
  if (i==1){
    pl <- ggplot()
    pl <- pl+geom_point(data=out,aes(x=x,y=y),shape=1, color="red",size=3)+ 
          #geom_line(data=outg, aes(x = p$x, y = yg), linetype = 1, color = "green", size=1)+
          scale_x_continuous(limits = c(0.01, 3))+
          scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x, n = 4),
                        labels = trans_format("log10", math_format(10^.x)),
                        limits=c(1e-2, 10))+
          theme_bw()+
          theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
                legend.position="none", text=element_text(size=20), axis.text=element_text(size=20))+ 
          #ylab(bquote(sqrt(paste(~bar(beta), " ", ~italic(p), "(", ~beta, ")", sep="")))) + 
          ylab(bquote(paste(~bar(beta), " ", ~italic(p), "(", ~beta, ")", sep="")))+
          xlab(bquote(paste(~beta, "/", ~bar(beta))))+
        geom_text(aes(label = "", x = 1.5, y = 8), size=8, parse = TRUE)
  } else {
    pl <- pl+geom_point(data=out,aes(x=x,y=y),shape=1, color="red",size=3)
  }
}

pf <- g3$lambda[1]*dgamma(out$x, g3$gamma.pars[1], 1/g3$gamma.pars[2]) + 
      g3$lambda[2]*dgamma(out$x, g3$gamma.pars[3], 1/g3$gamma.pars[4])
l1 <- g3$lambda[1]
a1 <- g3$gamma.pars[1]
b1 <- 1/g3$gamma.pars[2]

l2 <- g3$lambda[2]
a2 <- g3$gamma.pars[3]
b2 <-  1/g3$gamma.pars[4]

outf <- data.frame(xf = out$x, pf = pf)
pl <- pl+geom_line(data=outf, aes(x = xf, y = pf), linetype = 1, color = "green", size=1)
pf1 <- l1*dgamma(out$x, a1, b1)
outf1 <- data.frame(xf1 = out$x, pf1 = pf1)
pl <- pl+geom_line(data=outf1, aes(x = xf1, y = pf1), linetype = 1, color = "black", size=1)
pf2 <- l2*dgamma(out$x,a2,b2)
outf2 <- data.frame(xf2 = out$x, pf2 = pf2)
pl <- pl+geom_line(data=outf2, aes(x = xf2, y = pf2), linetype = 1, color = "black", size=1)

pf3 <- l1*dgamma(out$x, a1, b1) + l2*dgamma(out$x, a2, b2)
outf3 <- data.frame(xf3 = out$x, pf3 = pf3)
pl <- pl+geom_line(data=outf3, aes(x = xf3, y = pf3), linetype = 1, color = "black", size=1)

print(pl)


