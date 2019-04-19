####### FLoat value berechnung für tradeupcontracts###
## 20.03.2019 derkonna ##


#Parameter: Vektor von Länge 10 mit floats aller items im contract
#Parameter: Minimaler und Maximaler Float des gewünschten Skins
#Ausgabe: Finale Float Value des gewünschten Skins

floatCalc <- function(skinFloats,  min, max){
  if(length(skinFloats) == 10){
    sF <- mean(as.numeric(skinFloats))
    #Der float ist "komprimiert", um auf die Float range des ausgangsskins zu passen
    fRange <- max - min

    finalFloat <- sF*fRange
    finalFloat <- finalFloat + min

    if(finalFloat < 0.07){
      return(cat(paste("Float: ", finalFloat,  "Condition: Factory New" )))
    }
    if(finalFloat > 0.07 && finalFloat < 0.15){
      return(cat(paste("Float: ", finalFloat, "Condition: Minimal Wear")))
    }
    if(finalFloat > 0.15 && finalFloat < 0.38){
      return(cat(paste("Float: ", finalFloat, "Condition: Field-Tested" )))
    }
    if(finalFloat > 0.38 && finalFloat < 0.45){
      return(cat(paste("Float: ", finalFloat, "Condition: Well-Worn" )))
    }
    if(finalFloat > 0.45 && finalFloat < 1){
      return(cat(paste("Float: ", finalFloat,  "Condition: Battle-Scarred" )))
    }
  }
  else{
    return("The vector of skin floats must contain 10 values")
  }
}

#Welchen average float brauche ich, damit mein Skin eine bestimmte Rarity erreicht
neededFloat <- function(desiredCondition, min, max){
  if(desiredCondition == "Factory New" || desiredCondition == "FN" && min <= 0.07)
  {
    fRange <- max - min
    fLimit <- 0.07 - min
    fFraction <- 1/fRange

    fNeeded <- fLimit * fFraction
    return(paste("Average Condition Needed: ", fNeeded))
  }
  if(desiredCondition == "Minimal Wear" || desiredCondition == "MW" && min <= 0.15)
  {
    fRange <- max - min
    fLimit <- 0.15 - min
    fFraction <- 1/fRange

    fNeeded <- fLimit * fFraction
    return(paste("Average Condition Needed: ", fNeeded))
  }
  if(desiredCondition == "Field-Tested" || desiredCondition == "FT" && min <= 0.38 )
  {
    fRange <- max - min
    fLimit <- 0.38 - min
    fFraction <- 1/fRange

    fNeeded <- fLimit * fFraction
    return(paste("Average Condition Needed: ", fNeeded))
  }
  if(desiredCondition == "Well-Worn" || desiredCondition == "WW" && min <= 0.45)
  {
    fRange <- max - min
    fLimit <- 0.45 - min
    fFraction <- 1/fRange

    fNeeded <- fLimit * fFraction
    return(paste("Average Condition Needed: ", fNeeded))
  }
  if(desiredCondition == "Battle-Scarred" || desiredCondition == "BS")
  {
    fRange <- max - min
    fLimit <- 1 - min
    fFraction <- 1/fRange

    fNeeded <- fLimit * fFraction
    return(paste("Average Condition Needed: ", fNeeded))
  }
}
