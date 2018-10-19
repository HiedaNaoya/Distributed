応用情報工学演習配布資料

```
install.packages('quantmod')
library(quantmod)
library(tidyverse)
nikkei <- getSymbols('^N225',
                     src = 'yahoo',
                     from = as.Date('2017-01-01'),
                      to = as.Date('2018-10-18'), auto.assign = FALSE)
plot_y <- nikkei$N225.Close
plot_x <- index(nikkei)
plot_d <- data.frame(plot_x,plot_y)

ggplot(
  na.omit(plot_d)
  )+
  geom_path(aes(x=plot_x,y=N225.Close))

plot_d$log <- c(0,diff(log(plot_d$N225.Close)))

ggplot(plot_d)+
  geom_path(aes(x=plot_x,y=log))


```


課題

- 上記のコードで得たデータに対して、基本統計量の計算とヒストグラムのPlotを行う
- functions.Rのコードを参考に、正規分布とFA分布を当てはめて、パラメータ推定を行う。
- 同様に、そのモーメントを求める。
