# make example data from RData file

rm(list=ls())

load("SRPECLI_BZCN_0001.RData")

write.csv(data_storage$reactivity, file = "example_data.csv", row.names = F, quote = F)
