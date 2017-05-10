library(data.table)
library(doParallel)
library(minpack.lm)
library(pracma)
library(ggplot2)
library(scales)
library(sfsmisc)
library(VGAM)
source("ptsal.R")


fdate <- c(20170131, 20170201, 20170202, 20170203, 20170204, 
           20170205, 20170206, 20170207, 20170208, 20170209,
           20170210, 20170211, 20170212, 20170213, 20170214)
n <- 15
for (i in (1:n)) {
  fyear <- substr(fdate[i],1,4)
  fmonth <- substr(fdate[i],5,6)
  fday <- substr(fdate[i], 7, 8)
  times <-  gsub(":", "", substr(seq(ISOdatetime(fyear,fmonth,fday,0,0,0), ISOdatetime(fyear,fmonth,fday,23,50,0), length.out=144), 12, 16))
  timestamp <- paste0(fdate[i],"_",times)
  infile <- paste0("../data_ses/src_ses/D",substr(timestamp, 1, 8) ,"/ses_D", timestamp, ".txt", sep="")
  fnum <- 144
  for (j in 1:fnum) {
    data <- fread(infile[j])
    size1 <- data$V5
    if (j == 1) {
      size <- size1
    } else {
      size <- c(size, size1)
    } 
  }
  nsize <-size/mean(size)
  p <- density(nsize, from = 0, to=100, n = 300)
  p$y <- p$y/trapz(p$x,p$y)
  out <- data.frame(x = p$x, pe = p$y)
  write.table(out,file = paste0('graph/4/date/',fdate[i],'_size.txt'), col.names = F, row.names = F)
  pax <- p$x
  if (i==1) {
    pax <- p$x
    pa <- p$y
  } else {
      pa <- cbind(pa,p$y)
  }
  
  if (i==1){
    pl <- ggplot()
    pl <- pl+geom_point(data=out,aes(x=x,y=pe),shape=1, color="red",size=3)+
      scale_x_continuous(limits = c(0, 30))+
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x)),
                    limits=c(1e-3, 30))+
      theme_bw()+
      theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
            legend.position="none", text=element_text(size=20), axis.text=element_text(size=20), 
            axis.title=element_text(size=25, face="bold"))+
      ylab(bquote(paste(~bar(italic(v)), " ", ~italic(p),"(",~italic(v),")", sep=""))) + 
      xlab(bquote(paste(~italic(v), "/", ~bar(v))))+
      geom_text(aes(label = "", x = 20, y = .8), size=8, parse = TRUE)
  } else{
    pl <- pl + geom_point(data=out, aes(x = x, y = pe), shape=1, color = "red", size = 3)
  }
}


pv <- rowSums(pa, na.rm = FALSE, dims = 1)/n
outv <- data.frame(xv = pax, yv=pv)
write.table(outv,file = 'graph/4/av/av_size.txt', col.names = F, row.names = F)
wf <- (seq(1, 100, length.out=length(out$x)))^2.8
fit <- nlsLM(pv ~ dts.qexp(pax, p1,p2), start = list(p1 = 1.5, p2=10),
             data = out, lower = c(0.001,0.001), upper = c(100,100), weights = wf)

q <- coef(fit)[1]
l <- coef(fit)[2]
q <- 1.49
l <- 9.69
pq <- dts.qexp(pax,q,l)
qtext <- paste("italic(q) == ", round(q, 2))
ltext <- paste("lambda == ", round(l, 2))
outq <- data.frame(xq=pax,yq=pq)
write.table(outq,file = 'graph/4/av/qfit_size.txt', col.names = F, row.names = F)
pl <- pl + geom_line(data=outq,aes(x=xq,y=pq),color='black',size=1)+
      #geom_line(data=outv, aes(x=xv,y=yv),color='green',size=1) +
      geom_text(aes(label = qtext, x = 20, y = .8), size=8, parse = TRUE)+
      geom_text(aes(label = ltext, x = 20, y = .3), size=8, parse = TRUE)
print(pl)
