Installation
------------------------
``` r
if(!require(devtools)) install.packages("devtools")
devtools::install_github("masato-ogishi/plotUtility")
```

Usage
------------------------
``` r
library(tidyverse)
library(ggpubr)
library(plotUtility)

data("ToothGrowth")
df <- ToothGrowth

plt <- ggboxplot(df, x = "dose", y = "len",
                 color = "dose", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                 add = "jitter", shape = "dose")
my_comparisons <- list( c("0.5", "1"), c("1", "2"), c("0.5", "2") )
plt <- plt + stat_compare_means(comparisons = my_comparisons) + stat_compare_means(label.y = 50)
plt <- plt + theme_pubr(16)
savePDF(plt, o="Plot.pdf", w=8, h=5)
savePPTX(plt, o="Plot.pptx", w=8, h=5)
```
