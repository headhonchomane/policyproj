library(dplyr)
library(ggplot2)

energydata = read.csv("/Users/arshhothi/Desktop/peakloadstwo.csv")

privateprov = filter(energydata, LSE == "IOU")
publicutil = filter(energydata, LSE == "POU")
partnerutil = filter(energydata, LSE == "CCA")
cooputil = filter(energydata, LSE == "Co-op")
specialutil = filter(energydata, LSE == "Special")

privateprovnum = as.numeric(privateprov$Peak.Load..MW..1)
privatepeakload = sum(privateprovnum)

publicutilnum = as.numeric(publicutil$Peak.Load..MW..1)
publicpeakload = sum(publicutilnum)

cooputilnum = as.numeric(cooputil$Peak.Load..MW..1)
cooppeakload = sum(cooputilnum)

specialutilnum = as.numeric(specialutil$Peak.Load..MW..1)
specialpeakload = sum(specialutilnum)

partnerutilnum = as.numeric(partnerutil$Peak.Load..MW..1)
partnerpeakload = sum(partnerutilnum)

totalpeakload = privatepeakload + publicpeakload + cooppeakload + specialpeakload + partnerpeakload

privateperc = round((privatepeakload/totalpeakload)*100)
publicperc = round((publicpeakload/totalpeakload)*100)
partnerperc = round((partnerpeakload/totalpeakload)*100)
coopperc = round((cooppeakload/totalpeakload)*100)
specialperc = round((specialpeakload/totalpeakload)*100)

graphdata = data.frame(
  ProviderType=c("PRIVATE","PUBLIC","AGGREGATORS"),
  value=c(privateperc, publicperc, partnerperc)
  )

graphdata$fraction = graphdata$value / sum(graphdata$value)
graphdata$ymax = cumsum(graphdata$fraction)
graphdata$ymin <- c(0, head(graphdata$ymax, n=-1))
graphdata$labelPosition <- (graphdata$ymax + graphdata$ymin) / 2
graphdata$label <- paste0(graphdata$ProviderType, "\n % at Peak: ", graphdata$value)

ggplot(graphdata, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=ProviderType)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=ProviderType), size=3) + 
  scale_fill_brewer(palette="Dark2") +
  scale_color_brewer(palette="Dark2") +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")
