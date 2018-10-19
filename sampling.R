#この関数を実行してみたらわかると思うが, 実験回数$Nsim$が小さいとほぼ知りたい事象は観測できない.
# Simple Monte Carlo Method
install.packages(tidyverse)
install.packages(reshape2)
library(tidyverse)
library(reshape2)
sim.SMC <-function(Nsim, n, p, X){
  x <- rbinom(Nsim, size=n, prob=p)
  out <- sum( x >=X )/Nsim 
  return(out)}
sim.SMC(1000, n=100, p=0.5, X=70)
sim.SMC(100000, n=100, p=0.5, X=70)



## 重点<br>サンンプリング

# Importance Sampling Method
sim.IS <-function(Nsim, n, p, X,theta){
  x <- rbinom(Nsim, size=n,
              prob=p*exp(theta)/(1-p*(1-exp(theta))))
  out <- mean(  (x >=X )*(1-p+p*exp(theta))^n*exp(-theta*x))
  return(out)}
out1 <- sim.IS(100, n=100, p=0.5, X=70, theta=log(7/3))
out1
out2 <- sim.IS(100000, n=100, p=0.5, X=70, theta=log(7/3))
out2

#単純モンテカルロ法では, 1000回の実験から得ることが出来なかった推定値が$`r out1`$と推定できている.
#100,000回行えば, ほぼ正確な値を推定可能.

Nsim <- c(100,1000,2000,3000)
hyou.b <- matrix(0, 2, 4)
dimnames(hyou.b) <- list(c("平均","標準偏差"), Nsim)
k<- 1
for(i in Nsim){
  kekka <-replicate(1000, sim.IS(i, n=100, p=0.5, X=70, theta=log(7/3)))
  hyou.b[,k] <- c(mean(kekka), sd(kekka)) 
  k<- k+1}

hyou.b

hyou.c <- matrix(0, 2, 4)
dimnames(hyou.c) <- list(c("平均","標準偏差"), Nsim)
k<- 1
for(i in Nsim){
  kekka <-replicate(1000, sim.SMC(i, n=100, p=0.5, X=70))
  hyou.c[,k] <- c(mean(kekka), sd(kekka)) 
  k<- k+1}

hyou.c


Nsim <- c(100,1000,2000,3000)
hyou.b2 <- matrix(0, 1000, 4)
k<- 1
for(i in Nsim){
  kekka <-replicate(1000, sim.IS(i, n=100, p=0.5, X=70, theta=log(7/3)))
  hyou.b2[,k] <- kekka
  k<- k+1}

colnames(hyou.b2) <- c(100,1000,2000,3000)
hyou.c2 <- matrix(0, 1000, 4)
k<- 1
for(i in Nsim){
  kekka <-replicate(1000, sim.SMC(i, n=100, p=0.5, X=70))
  hyou.c2[,k] <- kekka
  k<- k+1}

colnames(hyou.c2) <- c(100,1000,2000,3000)


plot_hikaku <- data.frame(IS=t(hyou.b),SMC=t(hyou.c))
plot_up_2d <- plot_hikaku[,c(1,3)] + plot_hikaku[,c(2,4)]*2
plot_douwn_2d <- plot_hikaku[,c(1,3)] - plot_hikaku[,c(2,4)]*2
plot_coin <- cbind(plot_hikaku[,c(1,3)],IS_up=plot_up_2d[,1],
                   SMC_up=plot_up_2d[,2],IS_down=plot_douwn_2d[,1],
                   SMC_down=max(plot_douwn_2d[,2],0))

prob <- 1 - pbinom(69, size = 100, prob = 0.5)
prob
ggplot() +
  geom_errorbar(data=plot_coin,
                aes(x=rownames(plot_coin),ymin=IS_up,ymax=IS_down),color="red")+
  geom_errorbar(data=plot_coin,
                aes(x=rownames(plot_coin),ymin=SMC_up,ymax=SMC_down),color="blue")+xlab("N")+
  theme_bw()+
  theme(axis.title.x = element_text(size=25),axis.title.y = element_text(size=25))+
  theme(axis.text.x = element_text(size=25),axis.text.y = element_text(size=25)) +
  theme(legend.title = element_text(size=25),legend.text = element_text(size=25))+
  geom_hline(yintercept = prob)

hyou.b2 <-  data.frame(hyou.b2) %>% melt() 
hyou.b2$method <- "IS"
hyou.c2 <-  data.frame(hyou.c2) %>% melt() 
hyou.c2$method <- "SMC"
hyou.2 <- rbind(hyou.b2,hyou.c2)


layout <- t(t(c(2,1,1,1,1,1,1,1,1)))
plot_A <- ggplot(data=hyou.2[hyou.2$value < 0.000075,],aes(x=variable,y=value,color=method,fill=method))+
  geom_violin(alpha=0.2,position = "identity")+
  geom_jitter(size=0.01,width = 0.05)+
  geom_hline(yintercept = prob)+theme_bw()+
  theme(legend.position = "bottom")

plot_B <- ggplot(data=hyou.2[hyou.2$value > 0.000075,],aes(x=variable,y=value,color=method))+
  geom_jitter(size=0.01,width = 0.05)+
  guides(fill=FALSE,color=FALSE)+
  scale_color_manual(values=c("#00BFC4", "#00BFC4"))+
  theme_bw()+theme(axis.title.x = element_blank(),axis.text.x = element_blank())+
  scale_y_continuous(breaks=c(0.0003,0.0025,0.0050,0.0075,0.01),limits = c(0.0003,0.01))

grid.arrange(plot_A,plot_B,layout_matrix=layout)
