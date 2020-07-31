
#exw grapsei ola ta sxoleia se agglika (oxi greeklish) 


library(survey)
attach(x)
#x is the dataset imported and corresponding to 2013
year2013=x



library(dplyr)

#distinct(year2013,q41)

#2013
yr2013weighted <- 
  svydesign( 
    ~ psu, 
    strata = ~ stratum , 
    data = year2013 , 
    weights = ~ weight , 
    nest = TRUE 
  )

dfyr2013weighted <- degf(yr2013weighted)
dfyr2013weighted



#Count the weighted size of the generalizable population, overall and by groups:

svytotal( ~ one , yr2013weighted )


#Calculate the weighted mean (average) of a linear variable, overall and by groups:
# bmipct variable is a formula taking as inputs the students body mass and height

MeanbmipctWeighted=svymean( ~ bmipct , yr2013weighted , na.rm = TRUE )
MeanbmipctWeighted
cimeanbmipctWEighted <- confint(MeanbmipctWeighted, level = 0.95, df = dfyr2013weighted)

cimeanbmipctWEighted




#doing the same for variable q 24 
#q 24 indicates if someone has been bullied during the past 12 months on school
#property ( 1 for no ,2 for yes)

#weighted
q24=as.numeric(q24)

MeanbulliedWeighted=svymean( ~ q24 , yr2013weighted , na.rm = TRUE )
MeanbulliedWeighted
cimeanbulliedWEighted <- confint(MeanbulliedWeighted, level = 0.95, df = dfyr2013weighted)

cimeanbulliedWEighted

totalweightedbullied <- svytotal(~q24, yr2013weighted, deff = "replace")
totalweightedbullied
citottotalweightedbullied <- confint(totalweightedbullied, level = 0.95, df = dfyr2013weighted)
citottotalweightedbullied


#estimate a ratio
notSmoked= as.numeric( q36 == 1 ) 
Smoked = as.numeric( q36 > 1 )

#weighted
svyratio( 
  numerator = ~ Smoked , 
  denominator = ~ notSmoked , 
  yr2013weighted ,
  na.rm = TRUE
)


#sampling in subsets

#q68=1 indicates the students that have not eaten food for a period of 
#24 hours , during the past 30 days ,in order to lose weight
#q68=2 indicates students that have not going through this process


sub_yr2013weighted1 <- subset( yr2013weighted , q68 == 1 )
sub_yr2013weighted2 <- subset( yr2013weighted , q68 == 2 )


#Calculate the mean (average) of this subset:

#weighted  
meansub1Weighted=svymean( ~ bmipct , sub_yr2013weighted1 , na.rm = TRUE )
meansub2Weighted=svymean( ~ bmipct , sub_yr2013weighted2 , na.rm = TRUE )

meansub1Weighted
meansub2Weighted

SE( meansub1Weighted )
confint( meansub1Weighted )

SE( meansub2Weighted )
confint( meansub2Weighted )



##############################################################################
#                              1.3
############################################################################
#Perform a design-based t-test:

#weighted t-test
q69=as.numeric(q69)
svyttest(bmipct~q69,yr2013weighted )

#q69 takes the value 1 if someone tried to lose weight the past 30 days by consuming
#liquids or something else, and the value 2 if not.

#non weighted t-test
yr2013unweighted <- 
  svydesign( 
    ~ psu, 
    strata = ~ stratum , 
    data = year2013 , 
    nest = TRUE 
  )

#assuming equal probability 
svyttest(bmipct~q69,yr2013unweighted )

#the two tests are giving the same results in mean that in both cases the null
#hypothesis is being rejected. Also there are slightly differences in the 
#confidence intervals

#chi square test 
#variable q2 describes the gender

svychisq(~q2+q15,yr2013weighted)

svychisq(~q2+q15,yr2013unweighted)

#doing some plots
counts <- table(year2013$q68)

barplot(counts, main="Distribution of question q68 ",
        xlab="1 coresponds to students that have answered yes to q68")


#describing q69
#  the past 30 days, students that took take any diet pills, powders, or liquids without 
#a doctor's advice to lose weight or to keep from gaining 
library(dplyr)
distinct(year2013,q69)
str(year2013$q36)


counts <- table(year2013$q69)

barplot(counts, main="Distribution of question q69 ",
        xlab="1 coresponds to students that have answered yes to q69")

#the first bar coresponds to students that have not answered

