```{r prepareAnalysis_2014-by-month}
options(prism.path = "D:/lbooth/tmp")
get_prism_monthlys(type="tmean", years = 2000:2015, mon = 1, keepZip=FALSE)
get_prism_monthlys(type="tmean", years = 2014:2015, mon = 1:12, keepZip=TRUE)
```

switch(tolower(regmatches(p.Table$product_name, regexpr("precipitation|temperature", p.Table$product_name, ignore.case = TRUE))),
       precipitation = ppt,
       temperature = temp)

# This totally works but was deemed unnecessary in the end. 1 hour wasted :(
p.Table$type <- sapply(1:nrow(p.Table), function(x) 
  switch(tolower(
    regmatches(p.Table$product_name, regexpr("precipitation|temperature", p.Table$product_name, ignore.case = TRUE)))[x],
    precipitation = "ppt",
    temperature = "temp"))
```
