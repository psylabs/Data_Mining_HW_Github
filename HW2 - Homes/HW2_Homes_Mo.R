## HW2 MO's Analysis ##

homes <- read.csv("homes2004.csv")
nrow(homes) #15565
length(homes$NUNITS[homes$NUNITS==1]) #15013 (96% of data is single unit homes)
boxplot(homes$VALUE[homes$NUNITS>0 & homes$NUNITS<20] ~ (homes$NUNITS[homes$NUNITS>0 & homes$NUNITS<20]),
        col=rainbow(20),
        ylab="Home Value", 
        xlab="# of Units",
        ylim=c(0,500000))
#Posit some cyclicality. Consider this while controlling for other stuff. 

plot(homes$VALUE ~ homes$METRO, ylim=c(0,500000)) #Strange would expect rural to be cheaper
plot(homes$ESFD ~ homes$HHGRAD) #less likely to be near single family home if you No HS?
plot(homes$FRSTHO ~ homes$HHGRAD, col=c(1,3)) #educated people buy more homes
plot(homes$FRSTHO ~ homes$BEDRMS, col=c(3,5),
     ylab="First Home?", 
     xlab="# of Bedrooms",) #people buy small homes first
