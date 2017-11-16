library(xlsx)
setwd("C:/Users/user/Downloads")
dat <- read.xlsx("ED Products list short (JIRA).xlsx", sheetName="ED Products list short (JIRA)")
c <- colnames(dat)
c[3] = "Product"
c[4] = "type"
colnames(dat) <- c
dat[, 7] <- as.numeric(as.character( dat[, 7] ))

typef <- function(nn){
  
  k <- length(nn)
  for (n in 1:k){
  x <- nn[n] 
  if(x == "B2C"){nn[n]<- 1}
  else if (x == "B2B"){nn[n]<- 2}
  else if (x == "B2E"){nn[n]<- 3}
  else if (x == "B2T"){nn[n]<- 4}
  else if (x == "DO"){nn[n]<- 5}
  else{nn[n]<- 6}
  }
}


dat["type"] <- lapply(dat["type"], typef)
dat[4] <- lapply(dat[4], as.character)
dat[6] <- lapply(dat[6], as.character)

dat <- subset(dat, select = -c(Issue.key,Issue.id, Assignee) )




treemap(dat,
        index=c("Custom.field..Business.line.", "budget"),
        vSize="budget",
        vColor="Status",
        type="value",
        format.legend = list(scientific = FALSE, big.mark = " "))

treemap(dat,
        index=c("Status", "Budget"),
        vSize="Budget",
        title.legend="number of NACE4 categories",
        type="value")

dat$label <- paste(dat$Product, dat$Budget, sep = "\n")


treemap(dat,
        index=c("label"),
        vSize="Budget",
        vColor="Status",
        type="value")


library(plotly)



p <- plot_ly(dat, x = ~type, y = ~Status, text = ~label, type = 'scatter', 
             mode = 'markers', color = ~type,
             marker = list(size = ~Budget, opacity = 0.1)) %>%
  layout(title = 'Gender Gap in Earnings per University',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE))

p <- plot_ly(dat, x = ~Status, y = ~type, text = ~label, type = 'scatter', 
             mode = 'markers',
             marker = list(size = ~Budget, opacity = 0.5)) %>%
  layout(title = 'Gender Gap in Earnings per University',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE))




chart_link = plotly_POST(p, filename="bubble/simple")
chart_link
