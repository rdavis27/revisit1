# original code
### data(currency)
datapath <- "data" # was system.file("data", package="revisit")                              
curr <- read.table(paste0(datapath, "/EXC.ASC"), header = TRUE)
lmout <- lm(Japn.Yen ~ .,data=curr)
print(summary(lmout))
