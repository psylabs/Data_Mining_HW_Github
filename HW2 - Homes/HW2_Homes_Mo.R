## HW2 MO's Analysis ##

# [1] Plot some relationships and tell a story.

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

# [2] Regress log value onto all but mortgage and purchase $.
# How many coefficients are significant at 10% FDR?
# Re-run regression with only the significant covariates,
# and compare R2 to the full model

pricey <- glm(log(VALUE) ~ .-AMMORT-LPRICE, data=homes)
hist(pvals) ## looks like a ton of signal
pvals <- summary(pricey)$coef[-1,4]
names(pvals)[pvals>.1]
source("fdr.R")
print(cutoff <- fdr_cut(pvals, 0.1)) #Others got 0.079, this changes based on the new data you add eg.gt20down
signif <- coef(pricey)[-1][pvals<cutoff] # <= 
length(signif)*0.9 #29 true discoveries
print(coef(pricey)[-1][pvals>cutoff]) # what should we remove

#Run regression with only sig coeffs
pricey_new <- glm(log(VALUE) ~ .-ECOM1-EGREEN-ELOW1-ETRANS-ODORA-PER-ZADULT,
                  data=homes)
#ToDo (Mo): Compare Models,do an OOS test



# [3] Fit a regression for whether the buyer had  20% down.
# Interpret effects for 1st home buyers and # of bathrooms.
# Add + describe interaction for 1st home-buyers and #baths

homes$gt20dwn <- factor(0.2<(homes$LPRICE-homes$AMMORT)/homes$LPRICE)
reg_20pct<-glm(homes$gt20dwn ~.-AMMORT-LPRICE, data=homes, family=binomial) #What if it's a function of price??
summary(reg_20pct)
# IF FHO 37% less likely to put 20%down
plot(homes$FRSTHO ~ homes$gt20dwn, col=c(3,5),
     ylab="First Home?", 
     xlab="20%down") #Corroborated by this graph
# If more baths, more likely to put >20%down
plot(homes$BATHS ~ homes$gt20dwn, col=c(3,5),
     ylab="#Baths", 
     xlab="20%down") #Not as illustrative?
plot(homes$BATHS ~ homes$FRSTHO, col=c(3,5),
     ylab="#Baths", 
     xlab="20%down") #Not as illustrative? Let's try it differently
pctdown=((homes$LPRICE-homes$AMMORT)/homes$LPRICE)*100
plot(pctdown[pctdown>0] ~ as.factor(homes$BATHS[pctdown>0]), 
     main="Percent down Vs Number of baths",
     ylab="percent down", 
     xlab="# BATHS") #why are there negative percent down
reg_20pct_new<-glm(homes$gt20dwn ~.-AMMORT-LPRICE+FRSTHO*BATHS - FRSTHO , data=homes, family=binomial)
summary(reg_20pct_new) 
# FRSTHO insig can take it out
# Given the number of Baths x, if it is your first home 21% less likely to have put 20%down (??)
