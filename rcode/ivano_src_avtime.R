library(data.table)
library(doParallel)
library(minpack.lm)
library(ggplot2)
library(scales)
library(pracma)
require(mixtools)

fdate <- c(20170131, 20170201, 20170202, 20170203, 20170204, 
           20170205, 20170206, 20170207, 20170208, 20170209,
           20170210, 20170211, 20170212, 20170213, 20170214)
n = 15
for (i in 1:n) {
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
  #co <- sqrt(var(nvar))/mean(nvar)
  #print(paste0('Coef. of var: ',round(co, 3)))
  p <- density(nvar,from = 0, to = 3, n=300)
  p$y <- p$y/trapz(p$x,p$y)
  out <- data.frame(x=p$x,y=p$y)
  write.table(out,file = paste0('graph/2/data/date/',fdate[i],'.txt'), col.names = F, row.names = F)
  if (i==1) {
    nvar1 <- nvar
    pax <- p$x
    pa <- p$y
  } else {
    pa <- cbind(pa,p$y)
    nvar1 <- cbind(nvar1,nvar)
  }
  
  if (i==1){
    pl <- ggplot()
    pl <- pl+geom_point(data=out,aes(x=x,y=y),shape=1, color="red",size=3)+ 
          scale_x_continuous(limits = c(0.01,3))+
          scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x, n = 4),
                        labels = trans_format("log10", math_format(10^.x)),
                        limits=c(1e-2, 2))+
          theme_bw()+
          theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
                legend.position="none", text=element_text(size=20), axis.text=element_text(size=20))+ 
          ylab(bquote(paste(~bar(beta), " ", ~italic(p), "(", ~beta, ")", sep=""))) + 
          #ylab(bquote(paste(~bar(beta), " ", ~italic(p), "(", ~beta, ")", sep="")))+
          xlab(bquote(paste(~beta, "/", ~bar(beta))))+
          geom_text(aes(label = "", x = 1.5, y = 8), size=8, parse = TRUE)
  } else {
    pl <- pl+geom_point(data=out,aes(x=x,y=y),shape=1, color="red",size=3)
  }
}

pv <- rowSums(pa, na.rm = FALSE, dims = 1)/n
outv <- data.frame(xv = pax, yv=pv)
write.table(outv,file = 'graph/2/data/other/ep_av.txt', col.names = F, row.names = F)
pl <- pl+geom_line(data=outv, aes(x=xv,y=yv),color='green',size=1)

# l1 <-  0.545
# a1 <- 5.7814509
# b1 <- 6.7849861
# l2 <- 0.525
# a2 <- 26
# b2 <- 15.45

l1 <-  0.545
a1 <- 5.6
b1 <- 6.9
l2 <- 0.525
a2 <- 26
b2 <- 15.45

pf1 <- l1*dgamma(pax, a1, b1)
outf1 <- data.frame(xf1 = pax, pf1 = pf1)
write.table(outf1,file = 'graph/2/data/other/g1.txt', col.names = F, row.names = F)
pl <- pl+geom_line(data=outf1, aes(x = xf1, y = pf1), linetype = 1, color = "black", size=1)
pf2 <- l2*dgamma(pax,a2,b2)
outf2 <- data.frame(xf2 = pax, pf2 = pf2)
write.table(outf2,file = 'graph/2/data/other/g2.txt', col.names = F, row.names = F)
pl <- pl+geom_line(data=outf2, aes(x = xf2, y = pf2), linetype = 1, color = "black", size=1)

pf <- pf1+pf2
outf <- data.frame(xf=pax,pf=pf)
write.table(outf,file = 'graph/2/data/other/gg.txt', col.names = F, row.names = F)
pl <- pl <- pl+geom_line(data=outf, aes(x = xf, y = pf), linetype = "dashed", color = "blue", size=1)

print(pl)


fit <- nls(pv~(d1*dgamma(pax,p1,p2)+(1-d1)*dgamma(pax,p3,p4)), start=list(d1=0.56,p1=5.8,p2=6.8,p3=26.7,p4=15.45))
print(coef(fit))

