# import libraries

library(rsdmx)

 

# agency and id identify the dataset you are interested in

flowref <- 'IMF.STA,CPI'

 

# filter identifies the subset of the dataset you want.

filter <- 'USA...IX.M'

 

dataset <- as.data.frame(readSDMX(providerId = 'IMF_DATA',

                                  resource = 'data',

                                  flowRef = flowref,

                                  key = filter))

                               