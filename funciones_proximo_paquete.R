

#### Functions #####################

indiv.kinet <- function(S, vel){
  inv.vel = 1/vel
  inv.S = 1/S
  inv.vmax = as.vector(lm(inv.vel ~ inv.S)$coefficients[1])
  inv.km = as.vector(lm(inv.S ~ inv.vel)$coefficients[1])
  vmax = 1/inv.vmax
  km = 1/-inv.km
  
  v.s = vel/S
  lm(vel~S)
  
  lm.LW = lm(inv.vel ~ inv.S)
  lm.EH = lm(inv.vel ~ inv.S)
  
  LW.equation = paste("Y = ", inv.vmax, " + ", lm.LW$coefficients[2], "× X")
  EH.equation = paste("Y = ", inv.vmax, " + ", lm.EH$coefficients[2], "× X")
  
  res = list("Km" = km,
             "Vmax" = vmax,
             "Lineweaver-Burk equation" = LW.equation,
             "Lineweaver-Burk R squared" = summary(lm.LW)$r.squared,
             "Eadie-Hofstee equation" = EH.equation,
             "Eadie-Hofstee R squared" = summary(lm.EH)$r.squared,
             "1/Vmax" =inv.vmax,
             "-1/Km" = inv.km,
             "Vmax/Km" = vmax/km
             
             )
  
  print(res)
}

LB.plot <- function(S, 
                    vel,
                    ylim = c(0, 1.1*max(1/vel)),
                    xlim = c(1.5*inv.km, 1.1*max(1/S)),
                    xlab="1 / [S]",
                    ylab="1 / Vel",
                    pch=16,
                    col.pch="red",
                    col.lm="orange",
                    grid=TRUE,
                    cex=1.2, 
                    lwd=2.4
                    ){
  inv.vel = 1/vel
  inv.S = 1/S
  inv.vmax = lm(inv.vel ~ inv.S)$coefficients[1]
  inv.km = lm(inv.S ~ inv.vel)$coefficients[1]
  
  plot(x=1/S,
       y=1/vel,
       xlim=xlim,
       ylim=ylim,
       pch=pch,
       col=col.pch,
       ylab=ylab,
       xlab=xlab,
       cex=cex
       )
  abline(lm(inv.vel ~ inv.S),col=col.lm,lwd=lwd)
  if(grid == TRUE){
    grid()
    abline(v=0)
  }
  
}

add.LB.plot <- function(S, 
                       vel,
                       pch=16,
                       col.pch="red",
                       col.lm="orange",
                       grid=TRUE,
                       cex=1.2, 
                       lwd=2.4
){
  inv.vel = 1/vel
  inv.S = 1/S
  inv.vmax = lm(inv.vel ~ inv.S)$coefficients[1]
  inv.km = lm(inv.S ~ inv.vel)$coefficients[1]
  
  points(x=1/S,
       y=1/vel,
       pch=pch,
       col=col.pch,
       cex=cex
  )
  abline(lm(inv.vel ~ inv.S),col=col.lm,lwd=lwd)
  if(grid == TRUE){
    grid()
  }
  abline(v=0)
}


MM.plot <- function(S, 
                    vel,
                    ylim = c(0, 1.1*max(vel)),
                    xlim = c(0, 1.1*max(S)),
                    xlab="[S]",
                    ylab="Vel",
                    pch=16,
                    col="blue",
                    grid=TRUE,
                    cex=1.2, 
                    lwd=2.4
                    ){
  plot(
    x=S,
    y=vel,
    type="b",
    pch=pch,
    ylab=ylab,
    xlab=xlab,
    col=col,
    xlim=xlim,
    ylim=ylim,
    lwd=lwd,
    cex=cex
  )
  
  if(grid == TRUE){
    grid()
  }
  
}


add.MM.plot <- function(S, 
                        vel,
                        pch=16,
                        col="blue",
                        cex=1.2, 
                        lwd=2.4){
  points(
    x=S,
    y=vel,
    type="b",
    pch=pch,
    col=col,
    lwd=lwd,
    cex=cex
  )
  

}

EH.plot <- function(S, 
                    vel,
                    ylim = c(0, vmax),
                    xlim = c(0, vmax/km),
                    xlab="Vel / [S]",
                    ylab="Vel",
                    pch=16,
                    col.pch="deeppink",
                    col.lm="blue",
                    grid=TRUE,
                    cex=1.2,
                    lwd=2.4){
  
  v.s = vel/S
  
  inv.vel = 1/vel
  inv.S = 1/S
  inv.vmax = lm(inv.vel ~ inv.S)$coefficients[1]
  inv.km = lm(inv.S ~ inv.vel)$coefficients[1]
  vmax = 1/inv.vmax
  km = 1/-inv.km
  
  plot(
    x=vel/S,
    y=vel,
    pch=pch,
    ylab=ylab,
    xlab=xlab,
    col=col.pch,
    xlim=xlim,
    ylim=ylim,
    lwd=lwd,
    cex=cex)
  
  abline(lm(vel~v.s), col=col.lm,
         lwd=lwd)
  
  if(grid == TRUE){
    grid()
  }
  
  

}


add.EH.plot <- function(S, 
            vel,
            pch=16,
            col.pch="deeppink",
            col.lm="blue",
            cex=1.2,
            lwd=2.4){
  v.s = vel/S
  
  points(
    x=vel/S,
    y=vel,
    pch=pch,
    col=col.pch,
    lwd=lwd,
    cex=cex)
  
  abline(lm(vel~v.s), col=col.lm,
         lwd=lwd)
  
}


  
Ag.plot <- function(S,
                    vel,
                    xlim = c(1.1*-km, 1.1*max(S)),
                    ylim = c(0, 1.1*max(S/vel)),
                    xlab="[S]",
                    ylab="[S] / Vel",
                    pch=16,
                    col.pch="red4",
                    col.lm="orchid",
                    grid=TRUE,
                    cex=1.2,
                    lwd=2.4){
  s.v = S/vel
  
  inv.vel = 1/vel
  inv.S = 1/S
  inv.vmax = lm(inv.vel ~ inv.S)$coefficients[1]
  inv.km = lm(inv.S ~ inv.vel)$coefficients[1]
  vmax = 1/inv.vmax
  km = 1/-inv.km
  
  plot(x=S,
       y=s.v,
       ylim=ylim,
       xlim=xlim,
       xlab=xlab,
       ylab=ylab,
       col=col.pch,
       cex=cex,
       pch=pch
       )
  
  abline(lm(s.v~S), col=col.lm,
         lwd=lwd)
  
  if(grid == TRUE){
    grid()
    abline(v=0)
  }
  
}


add.Ag.plot <- function(S, 
                        vel,
                        pch=16,
                        col.pch="red4",
                        col.lm="orchid",
                        cex=1.2,
                        lwd=2.4){
  
  
  s.v = S/vel
  
  points(x=S,
       y=s.v,
       col=col.pch,
       cex=cex,
       pch=pch
  )
  
  abline(lm(s.v~S), col=col.lm,
         lwd=lwd)
}

##### Por hacer ####################################################

# - IC50
# - E. activación


#### Data Frames ###################################################



#####################################################################

eA = c(1.05, 1.54, 1.98,2.86,3.78,5,6.67,7.15,8)
eE = c(5, 6.66, 8, 10, 11.67, 13.33, 15, 15.4, 16)


cS = c(1.68e-5, 2.5e-5, 3.33e-5, 5e-05, 7e-5,1e-4, 1.5e-4, 1.67e-4, 2e-4)

ik = indiv.kinet(cS, eA)
ik

LB.plot(S=cS,vel=eE)

#LB.plot(S=cS,vel=eA, xlim = c(2*ik2$`-1/Km`, 1.1*max(1/cS)))
add.LB.plot(S=cS, vel=eE,col.pch = "blue",col.lm = "grey")

MM.plot(S=cS,vel=eE, ylim = c(0,20))
add.MM.plot(S=cS, vel=eA, col = "red")
plot(x=1/cS,y=1/eA,xlim=c(-1000,70000)$Vmax)

EH.plot(cS, eE, ylim = c(0, 25))
add.EH.plot(cS, eA)

indiv.kinet(cS, eA)


Ag.plot(cS,eA)
add.Ag.plot(cS, eE)
