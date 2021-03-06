library(data.table)
library(doParallel)
library(minpack.lm)
library(ggplot2)
library(scales)


fdate <- c(20170131, 20170201, 20170202, 20170203, 20170204, 
           20170205, 20170206, 20170207, 20170208, 20170209,
           20170210, 20170211, 20170212, 20170213, 20170214)

for (i in 1:length(fdate)) {
  print(i)
  fyear <- substr(fdate[i],1,4)
  fmonth <- substr(fdate[i],5,6)
  fday <- substr(fdate[i], 7, 8)
  times <-  gsub(":", "", substr(seq(ISOdatetime(fyear,fmonth,fday,0,0,0), ISOdatetime(fyear,fmonth,fday,23,50,0), length.out=144), 12, 16))
  timestamp <- paste0(fdate[i],"_",times)
  infile <- paste0("../data_ses/src_ses/D",substr(timestamp, 1, 8) ,"/ses_D", timestamp, ".txt", sep="")
  fnum <- 144
  for (j in 1:fnum) {
    if(file.size(infile[j])>0){
      data <- fread(infile[j])
      var <- diff(data$V1)
      nvar <- var/mean(var)
      p <- density(nvar, from = 0.5, to=30, n = 100)
      out <- data.frame(x = p$x, p = p$y)
      if (j==1) {
        pax <- p$x
        pa <- p$y
      } else {
        pa <- cbind(pa,p$y)
      }
      if (j==1){
        pl <- ggplot()
        pl <- pl+geom_point(data=out,aes(x=x,y=p),shape=1, color="red",size=3)+
              scale_x_continuous(limits = c(0, 15))+
              scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                            labels = trans_format("log10", math_format(10^.x)),
                            limits=c(1e-4, 1))+
              theme_bw()+
              theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
                    legend.position="none", text=element_text(size=20), axis.text=element_text(size=20), 
                    axis.title=element_text(size=25, face="bold"))+
              ylab(bquote(paste(~bar(tau), " ", ~italic(p), "(", ~tau, ")", sep=""))) + 
              xlab(bquote(paste(~tau, "/", ~bar(tau))))+
              geom_text(aes(label = "", x = 10, y = .8), size=8, parse = TRUE)
      } else{
              pl <- pl + geom_point(data=out, aes(x = x, y = p), shape=1, color = "red", size = 3)
      }
    }
  }
  pv <- rowSums(pa, na.rm = FALSE, dims = 1)/n
  outv <- data.frame(xv = pax, yv=pv)
  write.table(outv,file = 'graph/1/ep_av.txt', col.names = F, row.names = F)
  pl <- pl+geom_line(data=outv, aes(x=xv,y=yv),color='green',size=1)
  
}

print(pl)



