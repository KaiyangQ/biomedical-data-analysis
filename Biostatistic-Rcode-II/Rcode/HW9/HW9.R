getwd()
setwd("/Users/kaiyangqian/Documents/Courses/Biostatistic Method II/Rcode/HW9")

load("breastcancer2.rda")
write.csv(breastcancer, "/Users/kaiyangqian/Documents/Courses/Biostatistic Method II/Rcode/HW9/breastcancer2.csv", row.names = FALSE)
