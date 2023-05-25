# 5/25

## R code tutorial

```
head(Nile)
```

gives the first 5/6 in the dataset



``````
hist(Nile)
``````

draw a histogram

Whether to exclude some value in terem project



```
nile <- Nile[Nile > 50]
hist(nile)
```

exclude values smaller than 450 and draw a histogram



```
density(nile)
```

histogram is a density estimater



```
plot(density(nile))
```

Draw a smoother graph

You can change bandwidth



```
dgamma()
```

use `lambda` and `r` to draw a gamma

Estimate `lambda` and `r` are hard

