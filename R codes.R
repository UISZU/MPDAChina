m<-read.csv("E:/work/data/mobility.csv")   # read the mobility data

############ Fig 1 ###########################
windows(width=37,height=30)
par(mfrow = c(3,2),
    mar = c(5,5,0, 3),
    oma = c(0, 0,0, 0))

# plot 1: mobility in 2020 vs cases
library(gridBase)
library(grid)
plot.new()              
vps <- baseViewports()
pushViewport(vps$figure) ##  
vp1 <-plotViewport(c(0,1,0,1)) ## 

library(ggplot2)
vp <- viewport(height = unit(1,"npc"), width=unit(2, "npc"), 
               just = c("left","top"),
               y = 1, x = 0)
m$day<-as.numeric(m$day)
p<-ggplot(m[10:53,]) + 
  geom_rect(aes(xmin=24, xmax=31, ymin=0, ymax=Inf),fill="FloralWhite",alpha=1)+
  # add vertical line
  geom_vline(xintercept = 24, linetype="dashed",  color = "#fc8d59", size=0.6)+  # add vertical line
  geom_vline(xintercept = 31, linetype="dashed",  color = "#fc8d59", size=0.6)+
  geom_vline(xintercept = 33, linetype="dashed",  color = "#fc8d59", size=0.6)+
  geom_vline(xintercept = 40, linetype="dashed",  color = "#fc8d59", size=0.6)+
  #add bar plot
  geom_col(aes(x = day, y = cases*1e05 ), size = 1, color = "white", fill = "#084594", group =2)+
  scale_x_continuous(breaks=c(10,14,18,22,26,30,34,38,42,46,50,54), 
                     labels=c("Jan 10", "Jan 14", "Jan 18","Jan 22","Jan 26","Jan 30","Feb 3","Feb 7","Feb 11","Feb 15","Feb 19","Feb 23"),
                     limits=c(9,55),expand=c(0,0))+
  labs( x="Date", y = "Daily Mobility(millions)")+
  # add mobility line
  geom_line(aes(x = day, y = m2020), size = 1.35, color="#f16913") + 
  geom_point(aes(x = day, y = m2020), size = 3.5, color="#f16913")+
  scale_y_continuous(labels =c("0","10","20","30"),sec.axis = sec_axis(~./1e05, name = "Number of Cases"),limits=c(0,3e07), expand=c(0,0))+
  ggtitle("A. ")+
  theme(legend.key=element_blank(),panel.background = element_blank(), panel.border = element_rect(colour = "grey5", fill = NA, size = 0.5),
        plot.title = element_text(hjust = 0.5, face="bold", size=13))+
  # add text of intervention
  annotate("text", x = 21, y = 2.7e07, label = "Guangdong \nLevel 1 response",color = "grey20", size = 4)+
  annotate("text", x = 36.5, y = 2.68e07, label = "Workplace\n closed",color = "grey20", size = 4)+
  annotate("text", x = 42.5, y = 2.68e07, label = "Resume work\n by steps",color = "grey20", size = 4)+
  annotate("text", x = 27.5, y = 2.7e07, label = "Spring\n Festivel",color = "grey20", size = 4)+
  annotate("text", x = 31.1, y = 2.7e07, label = "Festivel\n extended",color = "grey20", size = 4)

print(p,vp = vp)    # add ggplot to the window

plot.new()
plot.new()
# plot 2:lag
ccf1<-ccf(m$m2020[12:53],m$cases[12:53],lag.max=20,plot=F)
ccf1
ind.max <- which(abs(ccf1$acf[1:41])==max(abs(ccf1$acf[1:41])))
max.cor <- ccf1$acf[ind.max]
2 * (1 - pnorm(abs(max.cor), mean = 0, sd = 1/sqrt(ccf1$n.used)))  # p-value for max correlation
#par(new=TRUE)
plot.new() 
plot(ccf1,xlim=c(-20,0),main="")


ci<- data.frame ("r" = ccf1$acf,"lcl"=rep(0,41),"ucl"=rep(0,41))
ci

require(psychometric)


CIr (ccf1$acf[1], 42)

#plot 3: correlation
require(roll)
result <- roll_cor(x=m$m2020[(18-n):(53-n)],y=m$cases[18:53],width =window)
plot(result,main="",
     xlim=c(19,33),ylim=c(0,1),xlab="Date",ylab=expression(R^2),
     type="o",lwd=2.8,bty="l" , pch=19 , cex=1.6,col="grey50",bty="o",xaxt="n")
axis(1, at = seq(19,34, by = 2), las=1,labels = c("Jan 19","Jan 21","Jan 23","Jan 25","Jan 27","Jan 29","Jan 31","Feb 2"))


########## Fig 2£º Total and sex  ##############################
windows(width=25,height=20)
par(mfrow = c(2,2),
    mar = c(2,4, 0, 1),
    oma = c(3,2,3, 0))

##################### plot 1 mobility volume ################
plot(0:20,m$m2019[24:44],col="grey50",type="o",lwd=2.6,bty="l" , pch=19 , cex=1.5,
     axes=F,xlim = c(0,21), ylim = c(0, 30e06),xlab="", ylab="Mobility volume (millions)",cex.lab=1.2,
     panel.first = rect(0, 0, 6, 30e6,  col=c("FloralWhite"), border=NA)) # add shadow for sprind festival
axis(1,at=0:7*3,labels=c("","","","","","","",""),pos=0)
axis(2,at=0:6*5e06,labels=c(0,5,10,15,20,25,30),las=1)
# Legend
legend(38, 33.5e6, legend=c("2019","2020"),xpd="NA",lty = c(1,1),ncol=2,pch=19 , 
       col=c("grey40","#d94801"), cex=1.1,bty ="n")
mtext("A.", side=3, line=0, at =-1, cex =1.1,font=2)
mtext("Mobile phone data", side=3, line=0.15, at =3, cex =0.9,font=1)
lines(c(0,0),c(0e06,30e06),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(6,6),c(0e06,30e06),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(9,9),c(0e06,30e06),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(16,16),c(0e06,30e06),col=c("#fc8d59"),lty=2,lwd=1.5)
text(3,28e06, "Spring\nFestival",cex = 1)
text(7.5,28e06, "Festival\n extended",cex = 1)
text(12.5,28e06, "Workplace\n closed",cex = 1)
text(17.5,28e06, "Resume\n Work",cex = 1)

lines(0:20,m$m2019[24:44],col="grey50",type="o",lwd=2.6,bty="l" , pch=19 , cex=1.5)
lines(0:20,m$m2020[24:44],col="#f16913",type="o",lwd=2.8,bty="l" , pch=19 , cex=1.5)


################### plot 2 baidu index  ###################
plot(0:20,m$b2019[24:44],col="grey40",type="o",lwd=2.6,bty="l" , pch=19 , cex=1.3,
     axes=F,xlim = c(0,21), ylim = c(0, 5),xlab="", ylab="Baidu Mobility index",cex.lab=1.2,
     panel.first = rect(0, 0, 6, 5,  col=c("FloralWhite"), border=NA)) # add shadow for sprind festival
axis(1,at=0:7*3,labels=c("","","","","","","",""),pos=0)
axis(2,at=0:5*1,labels=c(0,1,2,3,4,5),las=1)
mtext("B.", side=3, line=0, at =-1, cex =1.1,font=2)
mtext("Baidu data", side=3, line=0.15, at =1.6, cex =0.9,font=1)
lines(c(0,0),c(0e06,5),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(6,6),c(0e06,5),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(9,9),c(0e06,5),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(16,16),c(0e06,5),col=c("#fc8d59"),lty=2,lwd=1.5)
text(3,4.7, "Spring\nFestival",cex = 1)
text(7.5,4.7, "Festival\n extended",cex = 1)
text(12.5,4.7, "Workplace\n closed",cex = 1)
text(17.5,4.7, "Resume\n Work",cex = 1)

lines(0:20,m$b2019[24:44],col="grey50",type="o",lwd=2.6,bty="l" , pch=19 , cex=1.3)
lines(0:20,m$b2020[24:44],col="#f16913",type="o",lwd=2.8,bty="l" , pch=19 , cex=1.3)


################### plot 3: male
plot(0:20,m$male2019[24:44],col="grey40",type="o",lwd=2.6,bty="l" , pch=19 , cex=1.3,
     axes=F,xlim = c(0,22), ylim = c(0, 15e06),xlab="", ylab="Mobility volume (millions)",cex.lab=1.2,
     panel.first = rect(0, 0, 6, 15e6,  col=c("FloralWhite"), border=NA)) # add shadow
axis(1,at=0:7*3,labels=c("Jan 24","Jan 27","Jan 30","Feb 2","Feb 5","Feb 8","Feb 11","Feb 14"),cex.axis=1,pos=0)
axis(2,at=0:5*3e06,labels=c(0,3,6,9,12,15),las=1)
mtext("Male (Mobile phone data)", side=3, line=0.15, at =4, cex =0.9,font=1)
mtext("C.", side=3, line=0, at =-1, cex =1.1,font=2)
lines(c(0,0),c(0e06,15e06),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(6,6),c(0e06,15e06),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(9,9),c(0e06,15e06),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(16,16),c(0e06,15e06),col=c("#fc8d59"),lty=2,lwd=1.5)
text(3,14e06, "Spring\nFestival",cex = 1)
text(7.5,14e06, "Festival\n extended",cex = 1)
text(12.5,14e06, "Workplace\n closed",cex = 1)
text(17.5,14e06, "Resume\n Work",cex = 1)

lines(0:20,m$male2019[24:44],col="grey50",type="o",lwd=2.6,bty="l" , pch=19 , cex=1.5)
lines(0:20,m$male2020[24:44],col="#f16913",type="o",lwd=2.8,bty="l" , pch=19 , cex=1.5)


################## plot 4: female
plot(0:20,m$female2019[24:44],col="grey40",type="o",lwd=2.6,bty="l" , pch=19 , cex=1.3,
     axes=F, xlim = c(0,22), ylim = c(0, 15e06),xlab="", ylab="Mobility volume (millions)",cex.lab=1.2,
     panel.first = rect(0, 0, 6, 15e6,  col=c("FloralWhite"), border=NA))
axis(1,at=0:7*3,labels=c("Jan 24","Jan 27","Jan 30","Feb 2","Feb 5","Feb 8","Feb 11","Feb 14"),cex.axis=1,pos=0)
axis(2,at=0:5*3e06,labels=c(0,3,6,9,12,15),las=1)
mtext("Female (Mobile phone data)", side=3, line=0.15, at =4.5, cex =0.9,font=1)
mtext("D.", side=3, line=0, at =-1, cex =1.1,font=2)
lines(c(0,0),c(0e06,15e06),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(6,6),c(0e06,15e06),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(9,9),c(0e06,15e06),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(16,16),c(0e06,15e06),col=c("#fc8d59"),lty=2,lwd=1.5)
text(3,14e06, "Spring\nFestival",cex = 1)
text(7.5,14e06, "Festival\n extended",cex = 1)
text(12.5,14e06, "Workplace\n closed",cex = 1)
text(17.5,14e06, "Resume\n Work",cex = 1)
lines(0:20,m$female2019[24:44],col="grey50",type="o",lwd=2.6,bty="l" , pch=19 , cex=1.5)
lines(0:20,m$female2020[24:44],col="#f16913",type="o",lwd=2.8,bty="l" , pch=19 , cex=1.5)


########### Fig 3: age groups #######################

windows(width=22,height=20)
par(mfrow = c(3,2),
    mar = c(2,2.5, 1, 0),
    oma = c(3,1,3, 0))


######################################
##===== plot for 6 age groups === ####
######################################

plot(0:20,m$age18_2019[24:44],col="",type="o",lwd=2.2,bty="l" , pch=19 , cex=1.35,main="",xlab="", ylab="",axes=F,xlim = c(0,22), 
     ylim = c(0, 9e06), panel.first = rect(0, -10e6, 6, 9e6,  col=c("FloralWhite"), border=NA)) # add shadow
axis(1,at=0:7*3,labels=c("","","","","","","",""))
axis(2,at=0:3*3e06,labels=c(0,3,6,9),las=1)
#vertical lines
lines(c(0,0),c(-0.5e06,9e06),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(6,6),c(-0.5e06,9e06),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(9,9),c(-0.5e06,9e06),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(16,16),c(-0.5e06,9e06),col=c("#fc8d59"),lty=2,lwd=1.5)
# intervention
text(3,8.3e06, "Spring\nFestival",cex = 1)
text(7.5,8.3e06, "Festival\n extended",cex = 1)
text(12.5,8.3e06, "Workplace\n closed",cex = 1)
text(17.5,8.3e06, "Resume\n Work",cex = 1)
mtext("A.", side=3, line=0, at =-1, cex =1.1,font=2)
mtext("Age <18", side=3, line=0.04, at =1.6, cex =0.9,font=1)
mtext("Daily mobility(millions)", side=2, line=2, at =4.5e06, cex =0.8,font=1)

lines(0:20,m$age18_2019[24:44],type="o",lwd=2.2,bty="l" , pch=19 ,col="grey50",cex=1.35)
lines(0:20,m$age18_2020[24:44],type="o",lwd=2.2,bty="l" , pch=19 ,col="#f16913",cex=1.35)

# Legend
legend(38, 10.4e6, legend=c("2019","2020"),xpd="NA",lty = c(1,1),ncol=2,pch=19 , 
       col=c("grey50","#f16913"), cex=1.4,bty ="n")

plot(0:20,m$age29_2019[24:44],col="",type="o",lwd=2.2,bty="l" , pch=19 ,cex=1.35,main="",xlab="", ylab="",axes=F,xlim = c(0,22), ylim = c(0, 9e06),
     panel.first = rect(0, -10e6, 6, 9e6,  col=c("FloralWhite"), border=NA)) # add shadow
axis(1,at=0:7*3,labels=c("","","","","","","",""))
axis(2,at=0:3*3e06,labels=c(0,3,6,9),las=1)
#vertical lines
lines(c(0,0),c(-0.5e06,9e06),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(6,6),c(-0.5e06,9e06),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(9,9),c(-0.5e06,9e06),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(16,16),c(-0.5e06,9e06),col=c("#fc8d59"),lty=2,lwd=1.5)
# intervention
text(3,8.3e06, "Spring\nFestival",cex = 1)
text(7.5,8.3e06, "Festival\n extended",cex = 1)
text(12.5,8.3e06, "Workplace\n closed",cex = 1)
text(17.5,8.3e06, "Resume\n Work",cex = 1)
mtext("B.", side=3, line=0, at =-1, cex =1.1,font=2)
mtext("Age 18-29", side=3, line=0.04, at =1.8, cex =0.9,font=1)

lines(0:20,m$age29_2019[24:44],type="o",lwd=2.2,bty="l" , pch=19 ,col="grey50",cex=1.35)
lines(0:20,m$age29_2020[24:44],type="o",lwd=2.2,bty="l" , pch=19 ,col="#f16913",cex=1.35)


plot(0:20,m$age39_2019[24:44],col="",type="o",lwd=2.2,bty="l" , pch=19 ,cex=1.35,main="",xlab="", ylab="",axes=F,xlim = c(0,22), ylim = c(0, 9e06),
     panel.first = rect(0, -10e6, 6, 9e6,  col=c("FloralWhite"), border=NA))  # add ,i.
axis(1,at=0:7*3,labels=c("","","","","","","",""))
axis(2,at=0:3*3e06,labels=c(0,3,6,9),las=1)
#vertical lines
lines(c(0,0),c(-0.5e06,9e06),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(6,6),c(-0.5e06,9e06),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(9,9),c(-0.5e06,9e06),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(16,16),c(-0.5e06,9e06),col=c("#fc8d59"),lty=2,lwd=1.5)
# intervention
text(3,8.3e06, "Spring\nFestival",cex = 1)
text(7.5,8.3e06, "Festival\n extended",cex = 1)
text(12.5,8.3e06, "Workplace\n closed",cex = 1)
text(17.5,8.3e06, "Resume\n Work",cex = 1)
mtext("C.", side=3, line=0, at =-1, cex =1.1,font=2)
mtext("Age 30-39", side=3, line=0.04, at =1.9, cex =0.9,font=1)
mtext("Daily mobility(millions)", side=2, line=2, at =4.5e06, cex =0.8,font=1)

lines(0:20,m$age39_2019[24:44],type="o",lwd=2.2,bty="l" , pch=19 ,col="grey50",cex=1.35)
lines(0:20,m$age39_2020[24:44],type="o",lwd=2.2,bty="l" , pch=19 ,col="#f16913",cex=1.35)



plot(0:20,m$age49_2019[24:44],col="",type="o",lwd=2.2,bty="l" , pch=19 ,cex=1.35,main="",xlab="", 
     ylab="",axes=F,xlim = c(0,22), ylim = c(0, 9e06),
     panel.first = rect(0, -10e6, 6, 9e6,  col=c("FloralWhite"), border=NA)) # add shadow
axis(1,at=0:7*3,labels=c("","","","","","","",""),cex.axis=1.2)
axis(2,at=0:3*3e06,labels=c(0,3,6,9),las=1)
#vertical lines
lines(c(0,0),c(-0.5e06,9e06),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(6,6),c(-0.5e06,9e06),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(9,9),c(-0.5e06,9e06),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(16,16),c(-0.5e06,9e06),col=c("#fc8d59"),lty=2,lwd=1.5)
# intervention
text(3,8.3e06, "Spring\nFestival",cex = 1)
text(7.5,8.3e06, "Festival\n extended",cex = 1)
text(12.5,8.3e06, "Workplace\n closed",cex = 1)
text(17.5,8.3e06, "Resume\n Work",cex = 1)
mtext("D.", side=3, line=0, at =-1, cex =1.1,font=2)
mtext("Age 40-49", side=3, line=0.04, at =1.9, cex =0.9,font=1)

lines(0:20,m$age49_2019[24:44],type="o",lwd=2.2,bty="l" , pch=19 ,col="grey50",cex=1.35)
lines(0:20,m$age49_2020[24:44],type="o",lwd=2.2,bty="l" , pch=19 ,col="#f16913",cex=1.35)


plot(0:20,m$age59_2019[24:44],col="",type="o",lwd=2.2,bty="l" , pch=19 ,cex=1.35,main="",xlab="", 
     ylab="",axes=F,xlim = c(0,22), ylim = c(0, 9e06),
     panel.first = rect(0, -10e6, 6, 9e6,  col=c("FloralWhite"), border=NA)) # add shadow
axis(1,at=0:7*3,labels=c("Jan 24","Jan 27","Jan 30","Feb 2","Feb 5","Feb 8","Feb 11","Feb 14"),cex.axis=1.2)
axis(2,at=0:3*3e06,labels=c(0,3,6,9),las=1)
#vertical lines
lines(c(0,0),c(-0.5e06,9e06),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(6,6),c(-0.5e06,9e06),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(9,9),c(-0.5e06,9e06),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(16,16),c(-0.5e06,9e06),col=c("#fc8d59"),lty=2,lwd=1.5)
# intervention
text(3,8.3e06, "Spring\nFestival",cex = 1)
text(7.5,8.3e06, "Festival\n extended",cex = 1)
text(12.5,8.3e06, "Workplace\n closed",cex = 1)
text(17.5,8.3e06, "Resume\n Work",cex = 1)
mtext("E.", side=3, line=0, at =-1, cex =1.1,font=2)
mtext("Age 50-59", side=3, line=0.04, at =1.9, cex =0.9,font=1)
mtext("Daily mobility(millions)", side=2, line=2, at =4.5e06, cex =0.8,font=1)

lines(0:20,m$age59_2019[24:44],type="o",lwd=2.2,bty="l" , pch=19 ,col="grey50",cex=1.35)
lines(0:20,m$age59_2020[24:44],type="o",lwd=2.2,bty="l" , pch=19 ,col="#f16913",cex=1.35)


plot(0:20,m$age60_2019[24:44],col="",type="o",lwd=2.2,bty="l" , pch=19 ,cex=1.35,main="",xlab=c("Jan 24","Jan 27","Jan 30","Feb 2","Feb 5","Feb 8","Feb 11","Feb 14"), 
     ylab="",axes=F,xlim = c(0,22), ylim = c(0, 9e06),
     panel.first = rect(0, -10e6, 6, 9e6,  col=c("FloralWhite"), border=NA))  # add shadow
axis(1,at=0:7*3,labels=c("Jan 24","Jan 27","Jan 30","Feb 2","Feb 5","Feb 8","Feb 11","Feb 14"),cex.axis=1.2)
axis(2,at=0:3*3e06,labels=c(0,3,6,9),las=1)
#vertical lines
lines(c(0,0),c(-0.5e06,9e06),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(6,6),c(-0.5e06,9e06),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(9,9),c(-0.5e06,9e06),col=c("#fc8d59"),lty=2,lwd=1.5)
lines(c(16,16),c(-0.5e06,9e06),col=c("#fc8d59"),lty=2,lwd=1.5)
# intervention
text(3,8.3e06, "Spring\nFestival",cex = 1)
text(7.5,8.3e06, "Festival\n extended",cex = 1)
text(12.5,8.3e06, "Workplace\n closed",cex = 1)
text(17.5,8.3e06, "Resume\n Work",cex = 1)
mtext("F.", side=3, line=0, at =-1, cex =1.1,font=2)
mtext("Age >60", side=3, line=0.04, at =1.6, cex =0.9,font=1)

lines(0:20,m$age60_2019[24:44],type="o",lwd=2.2,bty="l" , pch=19 ,col="grey50",cex=1.35)
lines(0:20,m$age60_2020[24:44],type="o",lwd=2.2,bty="l" , pch=19 ,col="#f16913",cex=1.35)


#################################### Fig 4 space change #############
windows(width=37,height=30)
par(mfrow = c(3,1),
    oma = c(3, 1,1, 0))

# construct format for bubble plot
s <- data.frame("day" =c(1:21,1:21), "r" = NA)
s$r[1:21] <-  m[24:44,32]
s$r[22:42] <-  m[24:44,33]
s$scenario <-as.factor( c(rep(2019,21),rep(2020,21)))
s$day<-as.factor(s$day)

col1 =c("grey")
col2 =c("sienna1")
colours = c(col1,col2)


library(gridBase)
library(grid)
plot.new()              ## 
vps <- baseViewports()
pushViewport(vps$figure) ##  
vp1 <-plotViewport(c(0,1,0,1)) ##
require(ggplot2)


p<- ggplot(s, aes(x = day, y = scenario)) + 
  geom_point(aes(fill = scenario, size = r/1000),shape = 21,alpha = 1) +
  scale_fill_manual(values = colours, guide = FALSE) +
  scale_size_continuous(range = c(1, 16.8))+  # Adjust the range of points size
  labs( x="", y = "", size = "Daily mobility radius (km)", fill = "")+
  theme(legend.key=element_blank(), 
        axis.text.x = element_text(colour = "black", size = 10.3, angle = 0, vjust = 0.3, hjust = -0.4), 
        axis.text.y = element_text(colour = "black",  size = 10.6), 
        legend.text = element_text(size = 8, colour ="black"), 
        legend.title = element_text(size = 10), 
        legend.direction = "horizontal",
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "grey30", fill = NA, size = 0.1))+
  guides(fill = guide_legend(nrow = 1))+              # change legend position
  theme(plot.margin=unit(c(2,0,0.2,0),"cm"))+
  theme(legend.position=c(0.93,1.36))+
  scale_x_discrete(breaks=c("1","7","14","21"),  labels=c("     Spring Festival", "First week after Spring Festival", "Second week after Spring Festival",""))

print(p,vp = vp1) 


# plot 2: intracity change

library(shape)

par(mar = c(2,4.2,4,0.6)) 
barplot(m$intracity,col = c("grey","sienna1","grey","sienna1","grey","sienna1"),ylim=c(0,160e+06),ylab="Mobility Frequency(millions)",axes=F,
        main="",space=c(0.2,0.1,0.4,0.1,0.4,0.1))
axis(1,at=0:3*2.5,label=c("","","",""))
axis(2,at=0:4*40e+06,label=c("0","40","80","120","160"),las=1)
mtext("Spring Festival", side=1, line=1, at =1.25, cex = 0.85)
mtext("First week after Spring Festival", side=1, line=1, at =3.75, cex = 0.85)
mtext("Second week after Spring Festival", side=1, line=1, at =6.2, cex = 0.85)


Arrows(d,c,d,e, arr.type="triangle", arr.width=0.2,lty=1,code=2 ,arr.length=0.18, lwd=1.5, col="black")
lines(c(a,d),c(c,c),lty=1) 
lines(c(a,a),c(c,b),lty=1) 
text(1.25,6.5e7,"12%",cex = 1)
text(0.7,4.3e7,"50 million",cex = 1)
text(1.8,3.8e7,"44 million",cex = 1)

Arrows(d,c,d,e, arr.type="triangle", arr.width=0.2,lty=1,code=2 ,arr.length=0.18, lwd=1.5, col="black")
lines(c(a,d),c(c,c),lty=1) 
lines(c(a,a),c(c,b),lty=1) 
text(3.75,11.5e7,"67%",cex = 1)
text(3.2,9.3e7,"99 million",cex = 1)
text(4.3,3.2e7,"38 million",cex = 1)


Arrows(d,c,d,e, arr.type="triangle", arr.width=0.2,lty=1,code=2 , arr.length=0.18,lwd=1.5, col="black")
lines(c(a,d),c(c,c),lty=1) 
lines(c(a,a),c(c,b),lty=1) 
text(6.25,1.52e8,"69%",cex = 1)
text(5.7,1.28e8,"136 million",cex = 1)
text(6.8,0.36e8,"42 million",cex = 1)


#plot 3: intercity

par(mar = c(2,4.2,4,0.6)) 
barplot(m$intercity,col = c("grey","sienna1","grey","sienna1","grey","sienna1"),ylim=c(0,20e+06),ylab="Mobility Frequency(millions)",axes=F,
        main="",space=c(0.2,0.1,0.4,0.1,0.4,0.1))
axis(1,at=0:3*2.5,label=c("","","",""))
axis(2,at=0:4*5e+06,label=c("0","5","10","15","20"),las=1)
mtext("Spring Festival", side=1, line=1, at =1.25, cex = 0.85)
mtext("First week after Spring Festival", side=1, line=1, at =3.75, cex = 0.85)
mtext("Second week after Spring Festival", side=1, line=1, at =6.2, cex = 0.85)


Arrows(d,c,d,e, arr.type="triangle", arr.width=0.2,arr.length=0.18,lty=1,code=2 , lwd=1.5, col="black")
lines(c(a,d),c(c,c),lty=1) 
lines(c(a,a),c(c,b),lty=1) 
text(1.25,9e6,"51%",cex = 1.0)
text(0.70,6e6,"6.8 million",cex = 1.0)
text(1.8,2.3e6,"3.3 million",cex = 1.0)

Arrows(d,c,d,e, arr.type="triangle", arr.width=0.2,arr.length=0.18,lty=1,code=2 , lwd=1.5, col="black")
lines(c(a,d),c(c,c),lty=1) 
lines(c(a,a),c(c,b),lty=1) 
text(3.75,12.5e6,"78%",cex = 1.0)
text(3.2,9.6e6,"10.6 million",cex = 1.0)
text(4.3,1.6e6,"2.3 million",cex = 1.0)


Arrows(d,c,d,e, arr.type="triangle", arr.width=0.2,arr.length=0.18,lty=1,code=2 , lwd=1.5, col="black")
lines(c(a,d),c(c,c),lty=1) 
lines(c(a,a),c(c,b),lty=1) 
text(6.25,13.8e6,"78%",cex = 1.0)
text(5.7,10.7e6,"11.8 million",cex = 1.0)
text(6.8,1.8e6,"2.6 million",cex = 1.0)

mtext(expression(paste(bold("A.  Daily mobility radius under intensive control "))), side=3, line=46, at =1.2, cex = 1)
mtext(expression(paste(bold("B.  Intracity Mobility under intensive control"))), side=3, line=24, at =1.1, cex = 1)
mtext(expression(paste(bold("C.  Intercity Mobility under intensive control"))), side=3, line=1, at =1.1, cex = 1)


#################  Fig 5:  time change ##################
require(ggpubr)
########################################
# ============= heatmap ================
########################################
# construct dataframe as reverted order
for(k in 0:20){
  hours[hours$day==k&hours$year==2019,3]<-rev(hours[hours$day==k&hours$year==2019,3])
  hours[hours$day==k&hours$year==2020,3]<-rev(hours[hours$day==k&hours$year==2020,3])
}
require(ggplot2)

p1 <- ggplot(data = hours, mapping = aes(x = day, y = time, fill = Volume/1000000)) +
  geom_tile() +
  facet_grid(~ year,switch = "x",scales = "free_x", space = "free_x")+
  scale_fill_gradient(name = "Volume(millions)",
                      low = "#fff5f0",
                      high = "#a50f15")+
  theme_bw() +
  theme(strip.placement = "outside",plot.title = element_text(hjust = 0.1),
        panel.background = element_blank(), panel.border = element_rect(colour = "grey", fill = NA, size = 1))+
  labs(x="Times to Spring Festival(days)",y="Time(hours)")+
  scale_y_discrete(limits = c(rev(levels(as.factor(hours$time)))))+ # list the time from 1-24 in y-axis
  guides(fill = guide_legend(nrow = 1))+
  theme(plot.margin=unit(c(0.0,0.2,0,0.5),"cm"))+
  theme(legend.position=c(1.5,0.5))

col=c("#fb6a4a")
p2<-ggplot(wk, aes(x=week, y=m2_2019)) +
  geom_bar(stat="identity",width=0.8, colour="white", fill=col)+
  scale_x_discrete(limits=c("Mon", "Tue","Wed","Thu","Fri","Sat","Sun"))+
  theme_classic()+
  theme(legend.key=element_blank(), 
        axis.text.x = element_text(colour = "black", size = 10, angle = 0, vjust = 0.3, hjust = 0.5), 
        axis.text.y = element_text(colour = "black",  size = 8.6), 
        axis.title.x=element_blank(),
        legend.text = element_text(size = 10, face ="bold", colour ="black"), 
        legend.title = element_text(size = 12, face = "bold"), 
        plot.margin=unit(c(1,0.5,0.5,0.5), "lines"),
        panel.background = element_blank(), panel.border = element_rect(colour = "grey", fill = NA, size = 1), 
        legend.position = "top",legend.direction = "horizontal")+
  coord_cartesian(ylim = c(0,50e06)) +
  scale_y_continuous(labels =c("0","10","20","30","40","50"))+
  ylab("Mobility (millions)")

p3<-ggplot(wk, aes(x=week, y=m2_2020)) +
  geom_bar(stat="identity",width=0.8, colour="white", fill=col)+
  scale_x_discrete(limits=c("Mon", "Tue","Wed","Thu","Fri","Sat","Sun"))+
  theme_classic()+
  theme(legend.key=element_blank(), 
        axis.text.x = element_text(colour = "black", size = 10, angle = 0, vjust = 0.3, hjust = 0.5), 
        axis.text.y = element_blank(), 
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.text = element_text(size = 10, face ="bold", colour ="black"), 
        legend.title = element_text(size = 12, face = "bold"), 
        plot.margin=unit(c(1,0.5,0.5,0.5), "lines"),
        panel.background = element_blank(), panel.border = element_rect(colour = "grey", fill = NA, size = 1), 
        legend.position = "top",legend.direction = "horizontal")+
  coord_cartesian(ylim = c(0,50e06)) 


p12<-ggarrange(p2, p3, ncol = 2, nrow = 1,widths = c(1.065, 1))

windows(width=12,height=10)
library(cowplot)
# add title as a plot
p1_titile<- ggdraw() + 
  draw_label("A. Mobility distribution within a day",x=0.16,y=0.8, fontface = "bold", color = "black", size = 13)  
# add title as a plot
p12_titile<- ggdraw() + 
  draw_label("B. Mobility distribution within a week",x=0.16,y=0.5, fontface = "bold", color = "black", size = 13)

ggarrange(p1_titile,
          p1,                             
          p12_titile,
          p12, # Second row with 2 plots in 2 different columns
          nrow = 4,  align = "hv", 
          heights = c(0.1,2, 0.1,1),
          common.legend = TRUE)








