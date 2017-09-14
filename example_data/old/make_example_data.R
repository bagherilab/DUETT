# make example data from RData file

setwd("C:/Users/Xuebert/Dropbox/Albert Xue/Research/Deployment/ShapeSeq_event_detector/example_data/")

rm(list=ls())

load("SRPECLI_BZCN_0001.RData")
write.csv(data_storage$reactivity, file = "../example_data.csv", row.names = F, quote = F)
write.csv(data_storage$reactivity, file = "SRPECLI_BZCN_0001.csv", row.names = F, quote = F)

load("FLUORSW_BZCN_0001.RData")
write.csv(data_storage$reactivity, file = "FLUORSW_BZCN_0001.csv", row.names = F, quote = F)

load("FLUORSW_BZCN_0004.RData")
write.csv(data_storage$reactivity, file = "FLUORSW_BZCN_0004.csv", row.names = F, quote = F)
