応用情報工学演習配布資料

```

install.packages(“quantmod”)
library(“quantmod”)
nikkei <- getSymbols(“^N225”, src = “yahoo”, from = as.Date(“”),
                    to = as.Date(“”), auto.assign = FALSE)
```
