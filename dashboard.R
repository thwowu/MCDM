library(xlsx)

# getwd()
# seted("") # enter the expected directory


dat <- read.xlsx("ED Products list short (JIRA).xlsx", sheetName="ED Products list short (JIRA)")
c <- colnames(dat)
c[1] = "Product" 
c[2] = "Expert"
c[3] = "type"
colnames(dat) <- c
dat[, 1] <- lapply(dat[1], as.character)
dat[, 2] <- lapply(dat[2], as.character)
dat[, 3]    <- lapply(dat[3], as.character)
dat[, 4]  <- lapply(dat[4], as.character)

dat <- na.omit(dat)

dat$Status[dat$Status == "IMAGINE"] <- 0
dat$Status[dat$Status == "QUALIFY"] <- 1
dat$Status[dat$Status == "SHAPE"] <- 2
dat$Status[dat$Status == "MAKE"] <- 3
dat$Status[dat$Status == "PROPULSE"]  <- 4
dat$Status[dat$Status == "MEASURE"] <- 5


ds <- function(list, dadada){
  le <- length(list)
  li = matrix(0, ncol = le, nrow = 1) 
  didi = max(dadada$r)

  Qualify <- length(dadada[dadada$Status == 1,]$r)
  Shape <- length(dadada[dadada$Status == 2,]$r)
  Make <- length(dadada[dadada$Status == 3,]$r)
  Propulse <- length(dadada[dadada$Status == 4,]$r)
  Measure <- length(dadada[dadada$Status == 5,]$r)

  jj = c(Qualify, Shape, Make, Propulse, Measure)
  pos = which.max(jj) 
  middle = -didi * length(dadada[dadada$Status == pos,]$r ) /2
  print(middle)

  if (sum(list) == max(jj)){
    for (k in 1:le ){
      if (k == 1){ li[k] = -sum(list)/2 + didi }
      else       { li[k] = li[k-1] + didi }  
    }
  }
  else{
    lowest_location = didi * le / 2
    for (k in 1:le ){
      if (k == 1){ li[k] = middle - lowest_location }
      else       { li[k] = li[k-1] + didi }  
    }
  }
  return (as.list(li) )
}





dat$r <- sqrt(dat$Budget)

#Imagine <- dat[dat$Status == 1,] 
Qualify <- dat[dat$Status == 1,] 
Shape <- dat[dat$Status == 2,] 
Make <- dat[dat$Status == 3,] 
Propulse <- dat[dat$Status == 4,] 
Measure <- dat[dat$Status == 5,] 

#Imagine$y_cord <- unlist(ds(Imagine$r, 0.2))
Qualify$y_cord <- unlist(ds(Qualify$r, dat))
Shape$y_cord <- unlist(ds(Shape$r, dat))
Make$y_cord <- unlist(ds(Make$r, dat))
Propulse$y_cord <- unlist(ds(Propulse$r, dat))
Measure$y_cord <- unlist(ds(Measure$r, dat))

#data <- rbind(Imagine, Qualify, Shape, Make, Propulse, Measure)
data <- rbind(Qualify, Shape, Make, Propulse, Measure)
data$label <- paste(data$Product,  as.integer(data$Budget), sep = "\n")
data$label <- paste(data$label,  " K€", sep = "")





# with ggplot

library(ggplot2)

company_color = c(rgb(0,120,190, maxColorValue=255), 
                  rgb(0,170,255, maxColorValue=255),
                  rgb(88,88,88, maxColorValue=255), 
                  rgb(150,190,15, maxColorValue=255),
                  rgb(240,125,0, maxColorValue=255),
                  rgb(230,45,135, maxColorValue=255))

prodcut_types = c("B2C", "B2B", "B2E", "B2T", "DO", "NewBiz")

ggplot(data, aes(x = Status, y = y_cord, label=label, colour=type, size = r)) +
  geom_point(show.legend = FALSE) + scale_radius(range=c(0, 25)) +  
  geom_text(check_overlap = TRUE, size = 3, color = "black", nudge_x = +0.28, nudge_y = +7, show.legend = FALSE) + 
  
  scale_x_discrete(limits = c(1, 2,3,4,5), 
                   labels=c("Qualify", "Shape", 
                           "Make", "Propulse", "Measure") ) +

  scale_color_manual(breaks = prodcut_types, values= company_color) +
  scale_fill_manual(breaks = prodcut_types, values = company_color) +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 
  #+ geom_curve(aes(x = 1, y = max(Qualify$y_cord), xend = 6, yend = max(Measure$y_cord)), 
  #  data = data, curvature = -0.2, inherit.aes = FALSE, linetype = 2) +
  #geom_curve(aes(x = 1, y = min(Qualify$y_cord), xend = 6, yend = min(Measure$y_cord)), 
  #  data = data, curvature = 0.2, inherit.aes = FALSE, linetype = 2) +
  #geom_hline(yintercept = 0, arrow)


  scale_x_discrete(limits = c(1, 2,3,4,5,6), 
                   labels=c("", "Qualify", "Shape", 
                           "Make", "Propulse", "Measure") ) +

# with Draw.circle 

color <- function(name){
  if ( name == "B2C" ){ return (rgb(0,120,190, maxColorValue=255))}
  else if ( name == "B2B" ){ return (rgb(0,170,190, maxColorValue=255))}
  else if ( name == "B2E" ){ return (rgb(66,66,66, maxColorValue=255))}
  else if ( name == "B2T" ){ return (rgb(150,190,15, maxColorValue=255))}
  else if ( name == "DO" ){ return (rgb(240,125,0, maxColorValue=255))}
  else if ( name == "NewBiz" ){ return (rgb(230,45,135, maxColorValue=255))}
  else { return("black")}
}

lenP <- function(project){
  return (length(project))
}



plot(c(1, 6), c(coor_min - 20,coor_max + 20 ), type = "n")
ratio = 1.5 / (coor_max - coor_min) 
text_box_border = 0.3

lenP(Qualify)
for (loo in 1:lenP(Qualify)){
  draw.circle(2, Qualify$y_cord[loo], Qualify$r[loo] * ratio , col = color(Qualify$type[loo]))
  textbox(x = c(2 - text_box_border, 2 + text_box_border), y = Qualify$y_cord[loo] + ratio * 0.2 * sum(Qualify$Budget)/length(Qualify) , 
    textlist = Qualify$Product[loo], justify = 'c', cex=0.5, box=FALSE)
}


lenP(Measure)
for (loo in 1:lenP(Measure)){
  draw.circle(6, Measure$y_cord[loo], Measure$r[loo] * ratio , col = color(Measure$type[loo]))
  textbox(x = c(6 - text_box_border, 6 + text_box_border), y = Measure$y_cord[loo] + ratio * 0.2 * sum(Measure$Budget)/length(Measure) , 
    textlist = Measure$Product[loo], justify = 'c', cex=0.5, box=FALSE)
}

