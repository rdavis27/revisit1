library(reshape2)
library(ggplot2)

getTrade <- function(overwrite=FALSE) {
   filepath <- "goods.xls"
   if (!file.exists(filepath) | overwrite == TRUE) {
      download.file("https://www.census.gov/foreign-trade/statistics/historical/goods.xls", "goods.xls", mode = "wb")
   }
   xx <- read_excel(filepath, col_names = FALSE, skip = 6, col_types = "numeric")
   dd <- as.data.frame(xx[!is.na(xx[,1]),c(1,10,6,2)])
   colnames(dd) <- c("Year","Imports","Exports","Balance")
   # Convert from $millions to $billions
   dd$Balance <- dd$Balance / 1000
   dd$Exports <- dd$Exports / 1000
   dd$Imports <- dd$Imports / 1000
   trade <- dd
}
gdp <- getGDP(span="year", overwrite=FALSE)
trade <- getTrade(overwrite=FALSE)
minyr <- max(min(gdp$year), min(trade$Year))
maxyr <- min(max(gdp$year), max(trade$Year))
gdp   <- gdp  [gdp$year   >= minyr & gdp$year   <= maxyr,]
trade <- trade[trade$Year >= minyr & trade$Year <= maxyr,]
tradegdp <- trade
tradegdp$Balance <- 100 * tradegdp$Balance / gdp$gdp_curr
tradegdp$Exports <- 100 * tradegdp$Exports / gdp$gdp_curr
tradegdp$Imports <- 100 * tradegdp$Imports / gdp$gdp_curr
print(tradegdp)
#X11() # comment out if using png and readPNG
tradem <- melt(tradegdp, id="Year")
vcolor <- c("blue","green","red")
gg <- ggplot(tradem, aes(x=Year, y=value, group=variable)) +
   geom_line(aes(color = variable), size = 1, alpha = 0.7) +
   geom_point(aes(color=variable, shape=variable), size=3, alpha=0.7) +
   scale_x_continuous(breaks = seq(1960,2020,10)) +
   scale_y_continuous(breaks = seq(-8,16,4)) +
   geom_hline(yintercept = 0) +
   scale_color_manual(values = c("red","green4","blue")) +
   scale_shape_manual(values = c(15,16,17)) +
   ggtitle("U.S. Imports, Exports, and Trade Balance") +
   labs(x = "Source: https://www.census.gov/foreign-trade/statistics/historical/goods.xls") +
   labs(y = "Percent of GDP")
print(gg)
