gdp <- read.csv("GDP_data.csv",sep=",")
pairs(gdp[,1:10])
pairs(gdp[,c(3,11:20)])
pairs(gdp[,c(3,21:30)])
pairs(gdp[,c(3,31:40)])
pairs(gdp[,c(3,41:50)])
pairs(gdp[,c(3,51:60)])
pairs(gdp[,c(3,61:70)])

vis <- gdp[,c("CODE","GR6096")]
vis <- vis[order(vis[,2]),]
plot(vis,las=2,cex.axis=.6)


GR <- vis[,2]
plot(GR,pch=20)
