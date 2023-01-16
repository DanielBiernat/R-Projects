#stworzenie pliku wejsciowego
{
  # 
  # rm(list=ls());gc();dev.off()
  # library(quantmod);library(xts);library(tseries);library(dplyr);library(magrittr)
  # library(zoo);library(lmtest);library(nortest);library(car);library(sur)
  # 
  # as_log_xts=function(ohlc){
  #   returned <- xts(ohlc[,5],order.by = as.Date(ohlc[,1]))
  #   return(returned)
  # }
  # 
  # 
  # 
  # KGH <- read.csv("kgh_d (1).csv") %>% as_log_xts
  # USD <- read.csv("usdpln_d.csv") %>% as_log_xts
  # SLVR <- read.csv("xagusd_d.csv") %>% as_log_xts
  # COPP <- read.csv("copperUSD.csv")%>% as_log_xts
  # W20 <- read.csv("wig20_d.csv") %>% as_log_xts
  # data <- merge.xts(KGH,USD,SLVR,COPP,W20)
  # colnames(data) <-c("KGH","USD","SLVR","COPP","W20") 
  # data <- data["2021-05-12/2022-05-12"]
  # data %<>% as.data.frame 
  # data <- cbind(data,(rownames(data) %>% as.Date() %>% weekdays())=="poniedzia³ek")
  # colnames(data)[6] <- "monday"
  # data.all <- data
  # save(data.all, file = "biernat.RData")
}
rm(list=ls());gc();dev.off()
  # setwd("C:")
  library(quantmod);library(xts);library(tseries);library(dplyr);library(magrittr)
  library(zoo);library(lmtest);library(nortest);library(car);library(sur);library(glmnet);library(plotrix);  library(factoextra)
# load("zbior_danych_KGH.RData")
load("biernat.Rdata")
data <- data.all
#definicja funkcji
{
  #paczka testow normalnosci
  nortests <- function(lm){
    e=lm$residuals
    jarque.bera.test(e) %>% print;  shapiro.test(e) %>% print ; ad.test(e) %>% print
  }
  #porownanie wariancji na srodkowych 60% i ogonach
  tailvartest <- function(lm)
  {
    e=lm$residuals;yhat=lm$fitted.values
    tails <- e[yhat<quantile(yhat,0.2)|yhat>quantile(yhat,0.8)]
    mid <- e[yhat>=quantile(yhat,0.2)&yhat<=quantile(yhat,0.8)]
    var.test(mid,tails) #brak homoskedastycznosci  p-value = 0.003239

  }
  #sprawdzanie liniowej niezaleznosci
    sqrteigen <- function(lm)
    {
      X <- model.matrix(lm)
      ((t(X)%*%X %>% eigen %>% extract2("values") %>% abs %>%max)/
          (t(X)%*%X %>% eigen %>% extract2("values") %>% abs %>%min)) %>% sqrt

    }
    #porownanie wariancji na ponieidzalkach
  montest <- function(lm,data){
    e=lm$residuals
    e1=e[data$monday==T];e2=e[data$monday==F]
    var.test(e1,e2)
  }
}
# 1. Obrobka danych
{
summary(data) #sprawdzenie poprawnosci danych
is.na(data$KGH) %>% rownames(data)[.] %>% as.Date #swieta => brak notowan KGH
data$monday[which(is.na(data$KGH))+1]=T #oflagowanie NA w KGH poprzez zmiane monday na true w dniu nastepujacym
data <- data[is.na(data$KGH)==F,] #usuniecie dni z brakiem notowan KGH
for( i in which(is.na(data$COPP))){ #usrednienie pojedynczych brakujacych cen miedzi
  data$COPP[i]=0.5*(data$COPP[i-1]+data$COPP[i+1])
};rm(i)
which(is.na(data$COPP))[1]->marker #tymczasowa zmienna zaznaczajaca gdzie NA
data$COPP[c(marker+0:1)]<- seq(from=data$COPP[marker-1],to=data$COPP[marker+2],length=4)[2:3] #usrednienie dwoch brakow pod rzad
rm(marker)
data$monday %<>% as.factor
#zamiana cen miedzi i srebra na z³otowki
data$COPP <- data$COPP*data$USD
data$SLVR <- data$SLVR*data$USD
#zamiana na proste stopy zwrotu
# data_log <- data
# data_log[-1,-6] <- apply(data[,-6],2,function(x){x %>% log %>% diff})
# data_log <- data_log[-1,]
data[-1,-6] <-  apply(data[,-6],2,function(x){x %>% log %>% diff %>% exp %>% '-'(.,1)})
data <- data[-1,]

}
# 2. model CAPM
{
lm(KGH~W20,data) %>% summary #model z wyrazem wolnym
lm_capm <- lm(KGH~0+W20,data)# model jednoczynnikowy
summary(lm_capm) #odczytanie podstawowych parametrow modelu
confint(lm_capm)#przedzial ufnosci bety
e_capm <- lm_capm$residuals# wczotanie residuow
# pdf("capm_density.pdf")
# e_capm %>% density %>% plot(xlab="",main="Estymator j¹drowy gestoœci")
# dev.off()
# pdf("capm_qq.pdf")
qqnorm(e_capm);qqline(e_capm)# wykres 1
# dev.off()

nortests(lm_capm)#testy normalnosci
# acf(e_capm)
dwtest(lm_capm,alternative = "two.sided")#durbin watson
raintest(lm_capm) #0.017 #rainbowtest
# pdf("capm_e-fitted.pdf")
plot(e_capm~lm_capm$fitted.values,ylab="residua",xlab="wartoœci dopasowane");abline(0,0)#wykres 2
# dev.off()



tailvartest(lm_capm)
# plot(e_capm~data$W20);abline(0,0)
# plot(data$KGH~data$W20)
# plot(lm_capm)
gqtest(lm_capm)# goldfield-quant
# bptest(lm_capm)
}
# 3. model ze wszystkimi zmiennymi
{
  lm_all <- lm(KGH~.,data)
  summary(lm_all)
  lm_all <- lm(KGH~.-monday,data)
  summary(lm_all)
  lm_all <- lm(KGH~0+USD+SLVR+COPP+W20,data)
  summary(lm_all)

}
# 4. model bez monday i intercept
{
  lm_ac <- lm(KGH~0+USD+SLVR+COPP+W20,data)
  summary(lm_ac) #RSE 1.87 , R2=.52
  e_ac <- lm_ac$residuals
  # pdf("ac_acf.pdf")
  acf(e_ac,main=" ")#wykres 3
  # dev.off()
  # temp <- acf(e_ac)
  dwtest(lm_ac,alternative = "two.sided")
  gqtest(lm_ac)
  # pdf("ac_evsfit.pdf")
  # plot(e_ac~lm_ac$fitted.values,xlab="wartoœci dopasowane",ylab="residua");abline(0,0)
  # dev.off()

  #znaczaca poprawa wzledem capm
tailvartest(lm_ac)
  # plot(e_ac~data$W20)
  nortests(lm_ac)
  # pdf("ac_e.pdf")
  plot(e_ac,ylab="residua",xlab = "nr obserwacji") #wykres 5
  # dev.off()
  gqtest(update(lm_ac,.~.,data=data[101:250,])) #test na ogonie danych
 resettest(lm_ac)
 # Box.test(e_ac^2,lag=1)
 # pdf("ac_qq.pdf")
 qqnorm(e_ac);qqline(e_ac) #wykres 4
 # dev.off()
 raintest(lm_ac) #do opisania
 bptest(lm_ac) # do opisania
 COOK <- cooks.distance(lm_ac)
 # ols_plot_cooksd_bar(lm_ac)
 # pdf("ac_cook.pdf")
 cooks.distance(lm_ac) %>% barplot #wykres 6
 # dev.off()

}
leverage(lm_capm) %>% mean #obliczenie dzwigni
leverage(lm_capm)[200]
#model capm zawezony
{
  data_trim <- tail(data[-200,],150)
  lm(KGH~W20, data_trim) %>% summary #pv incerceptu #V
  lm_capmtrim <- update(lm_capm,data=data_trim)
  lm_capmtrim %>% summary #V
  lm_capmtrim %>% confint %>% t %>% diff %>% '/'(.,2)#V
  lm_capmtrim %>% nortests#V

  e_captrim <- lm_capmtrim %>% residuals
  # pdf("captrim_qq.pdf")
  qqnorm(e_captrim);qqline(e_captrim)#V Wykres 7
  # dev.off()
  acf(e_captrim,plot=F)[1] #V
  # dwtest(lm_capmtrim,alternative="two.sided")
  gqtest(lm_capmtrim)#V
  raintest(lm_capmtrim)#V
  resettest(lm_capmtrim)#V
  # bptest(lm_capmtrim)
  tailvartest(lm_capmtrim) #V
  montest(lm_capmtrim,data_trim)#V
}
#model trim:-wojna, ostatnie 150 obserwacji
{
  data_trim <- tail(data[-200,],150)
  lm_trim <- update(lm_ac,.~.,data=data_trim);e_trim <- lm_trim %>% residuals()
  lm(KGH~.-monday,data_trim) %>% summary #V
  lm(KGH~0+.,data_trim) %>% summary#V
  summary(lm_trim)#V
  confint(lm_trim) %>%t %>%  diff %>% '/'(.,2)#V
  nortests(lm_trim)#V
  # pdf("trim_qq.pdf")
  qqnorm(e_trim);qqline(e_trim)#V #wykres 8
  # dev.off()


  acf(e_trim,plot=F)[1]#V
  # pdf("trim_acf.pdf")
  acf(e_trim,main=NA)#V #wykres 9
  # dev.off()
  dwtest(lm_trim,alternative = "two.sided")#V



  resettest(lm_trim)#V
  bptest(lm_trim)#V
  raintest(lm_trim)#V
  # plot(e_trim)
  # pdf("trim_evsfitted.pdf")
  plot(e_trim~lm_trim$fitted.values,xlab="wartoœci dopasowane",ylab="residua");abline(0,0)#V #wykres 10
  # dev.off()
  tailvartest(lm_trim)#V
  gqtest(lm_trim)#V
  montest(lm_trim,data_trim)#V

  # pdf("trim_cook.pdf")
  cooks.distance(lm_trim) %>% barplot(.,xlab=" ")#V #wykres 11
  # dev.off()
  sqrteigen(lm_trim)
}
#PCR
{
  #budowa modelu
 prcomp(data_trim[2:5])[[2]]
  princomp(data_trim[2:5], cor = FALSE, scores = TRUE)
  res.pca <- prcomp(data_trim[2:5], scale = FALSE)
  # pdf("PCA.pdf")

  # dev.off()
  data_PCR <- as.matrix(data_trim[2:5])%*%res.pca$rotation
  data_PCR <- cbind(data_trim$KGH,data_PCR)
  colnames(data_PCR) <- c("KGH","PC1","PC2","PC3","PC4")
  data_PCR %<>% as.data.frame
  #model
  lm_PCR <- lm(KGH~0+PC1+PC2,data_PCR)
  lm_PCR %>% summary
  lm_PCR %>% coefficients#V
  confint(lm_PCR) %>%t %>%  diff %>% '/'(.,2)#V

  prcomp(data_trim[2:5])[[2]][,1:2]%*%(lm_PCR %>% coefficients())#V
  # lm_trim %>% coefficients()
  e_PCR <- lm_PCR$residuals
  acf(e_PCR,plot = F)[1]#V
  dwtest(lm_PCR,alternative = "two.sided")#V
  nortests(lm_PCR)#V
  fviz_eig(res.pca) #wykres 13
  # raintest(lm_PCR)
  # resettest(lm_PCR)
  # gqtest(lm_PCR,alternative = "two.sided")
  # bptest(lm_PCR,)
  # plot(e_PCR)
  # plot(e_PCR~lm_PCR$fitted.values)
  # plot(e_PCR~data_PCR$PC1)
  # plot(e_PCR~data_PCR$PC2)
  # ols_plot_cooksd_bar(lm_PCR)
  # plot(e_PCR)
# tailvartest(lm_PCR)


}
#crossvalidation modeli
{

crossvalid <- function(lm,data,dontplot=F){
  N=dim(data)[1]
  data_train =head(data,N-30)
  data_test = tail(data,30)
  lm_cross = update(lm,.~.,data=data_train)
  predictions = predict.lm(lm_cross,data_test,interval = "prediction")
  train_mse = sum(((data_test$KGH)-predictions[,1])^2)
  if(dontplot==F){
    plotCI(x=1:(dim(data_test)[1]),y =predictions[,1],li =predictions[,2],ui =predictions[,3],xlab=" ",ylab="KGH")
    points(data_test$KGH,col="red")
  }

  a=which(data_test$KGH>predictions[,3]) %>% length
  b=which(data_test$KGH<predictions[,2])%>% length
  # print(a+b)
  print((binom.test(x=a+b,n=(dim(data_test)[1]),p = 0.05,alternative = "two.sided"))$p.value)
  # print(mean(predictions[,3]-predictions[,2]))
  #
  return(sqrt(train_mse/30))
}
crossvalid(lm_capm,data,dontplot = T) %>% round(4)
crossvalid(lm_ac,data,dontplot = T)%>% round(4)

crossvalid(lm_capmtrim,data_trim,dontplot = T)%>% round(4)
# pdf("trim_cross.pdf")
crossvalid(lm_trim,data_trim) #wykres 14
# dev.off()
crossvalid(lm_PCR,data_PCR,dontplot = T)%>% round(4)
}

#analiza stabilnoœci modelu
{
  stability <- function(lm,data){
  for(j in 1:(length(lm$coefficients)))  {
    CI <- matrix(0,nrow=10,ncol=2);cut_b <- NULL
    for(i in 0:9){
      cut_lm=update(lm,data=data[(15*i+(1:15)),])
      CI[i+1,]=confint(cut_lm)[j,]
      cut_b[i+1]=cut_lm$coefficients[j]
    }
    plotCI(x=1:10,y =cut_b,li =CI[,1],ui =CI[,2],main=((lm %>% coefficients%>% names)[j]),ylab="beta",xlab="")
    lm$coefficients[j] %>% abline(.,0)
    abline(confint(lm)[j,1],0,lty="dashed",lwd=0.5)
    abline(confint(lm)[j,2],0,lty="dashed",lwd=0.5)
  }}

  # pdf("trim_stab.pdf")
  par(mfrow=c(2,2))
  stability(lm_trim,data_trim) #wykres 12
  # dev.off()
  # par(mfrow=c(2,2))
  # stability(lm_PCR,data_PCR)
  # stability(lm_capmtrim,data_trim)
  # stability(lm_ac,data)

}
# -0.0864, 0.1504,  0.6418, 1.085 bety z PCA

  #vartest na monday
#model CO bez 24-02-2022
{
  # data_WAROMIT <- data_CO[-c(205,206,199),]
  #   lm_WAROMIT <- lm(KGH~0+USD+SLVR+COPP+W20,data_WAROMIT)
  #   summary(lm_WAROMIT) # MSE=1.8% R2=.56
  #   e_WAROMIT <- lm_WAROMIT %>% residuals
  #   #normalnosc
  #   e_WAROMIT %>% density %>% plot
  #   jarque.bera.test(e_WAROMIT)
  #   shapiro.test(e_WAROMIT)
  #   ad.test(e_WAROMIT) #nie dziala
  #   qqPlot(e_WAROMIT)  #nie dziala
  #   #autokorelacja
  #   dwtest(lm_WAROMIT,alternative = "two.sided")
  #   acf(e_WAROMIT)
  #   Box.test(e_WAROMIT^2,lag=1)
  #   plot(e_WAROMIT);abline(0,0)
  #   Box.test(e_WAROMIT[1:62],lag=1)
  #   #homoskedastycznoœæ
  #   raintest(lm_WAROMIT)
  #   bptest(lm_WAROMIT)
  #   gqtest(lm_WAROMIT,alternative = "two.sided")
  #   library(olsrr)
  #   ols_plot_cooksd_bar(lm_WAROMIT)
  #
  #   plot(e_WAROMIT~data_WAROMIT$USD);abline(0,0)
  #   plot(e_WAROMIT~data_WAROMIT$W20)
  #   var.test(e_WAROMIT[data_WAROMIT$monday==T],e_WAROMIT[data_WAROMIT$monday==F])
  #   #test ogonowy
  #   tails <- e_WAROMIT[lm_WAROMIT$fitted.values<quantile(lm_WAROMIT$fitted.values,0.2)|lm_WAROMIT$fitted.values>quantile(lm_WAROMIT$fitted.values,0.8)]
  #   mid <- e_WAROMIT[lm_WAROMIT$fitted.values>=quantile(lm_WAROMIT$fitted.values,0.2)&lm_WAROMIT$fitted.values<=quantile(lm_WAROMIT$fitted.values,0.8)]
  #   var.test(mid,tails);rm(tails,mid)
}
# model po poprawce Cochrane–Orcutta
{
  #   lmE <- lm(e_ac[-1]~0+e_ac[-length(e_ac)])
  #   rho=lmE$coefficients[1];rm(lmE) #-0.2?
  #   data_CO <- apply(data[1:5],2,function(x){x[2:length(x)]-rho*x[1:(length(x)-1)]})
  #   data_CO %<>% as.data.frame
  #   data_CO %<>% cbind(.,data$monday[-1])
  #   colnames(data_CO)[6] <- "monday"
  #   lm_CO <- lm(KGH~0+USD+SLVR+COPP+W20,data_CO)
  #   summary(lm_CO) # MSE=1.8% R2=.56
  #   e_CO <- lm_CO %>% residuals
  #   #normalnosc
  #   e_CO %>% density %>% plot
  #   jarque.bera.test(e_CO)
  #   shapiro.test(e_CO)
  #   ad.test(e_CO) #nie dziala p-value = 0.02538
  #   qqPlot(e_CO)  #nie dziala
  #   #autokorelacja
  #   dwtest(lm_CO,alternative = "two.sided")
  #   acf(e_CO)
  #   Box.test(e_CO^2,lag=1)
  #   #homoskedastycznoœæ
  #   raintest(lm_CO)
  #   bptest(lm_CO)
  #   gqtest(lm_CO)
  #   library(olsrr)
  #   # COOK <- ols_plot_cooksd_bar(lm_CO)
  #
  #   # plot( lm_CO)
  #   plot(e_CO~data_CO$USD)
  #   plot(e_CO~data_CO$W20)
  #   var.test(e_CO[data_CO$monday==T],e_CO[data_CO$monday==F])
  #
}
# detachAllPackages <- function() {
# 
#   basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
# 
#   package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
# 
#   package.list <- setdiff(package.list,basic.packages)
# 
#   if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
# 
# }
# 
# detachAllPackages()
