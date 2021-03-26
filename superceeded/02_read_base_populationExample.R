#read_base_population

#
require(tidyverse)
require(data.table)

#As an example
#Read a single district 
#(Eden district in cumbria chosen as it has low population and therefore short run time)

popfile <- read_csv("MDOfiles/Eden.csv")

