bisg_prep_race_predict <- function(df, voterid=NULL, precinct=NULL, surname_char, state,
                                   census.geo="block", census.key, census.data, 
                                   census.surname = TRUE, surname.only = FALSE,
                                   surname.year = 2010, age = FALSE, sex = FALSE, 
                                   party, retry = 0){
  
  if ( !is.null(voterid) | !is.null(precinct) ){
    
    voter.file <- data.frame(voterid = df[, voterid], 
                             precinct = df[, precinct],
                             surname = tolower(df[,surname_char]), 
                             state="NY", 
                             county = as.character(df$county),
                             tract = as.character(df$tract), 
                             block=as.character(df$block), 
                             stringsAsFactors = F); 
    print(str(voter.file))  
    
  } else{
    # Create Data.frame Object to send to predict_race() wru package function #
    voter.file <- data.frame(voterid = 1:nrow(df), 
                             surname = tolower(df[,surname_char]), 
                             state="NY", 
                             county = as.character(df$county),
                             tract = as.character(df$tract), 
                             block=as.character(df$block), 
                             stringsAsFactors = F); 
    print(str(voter.file))
  }
  # Estimate Voter Race #
  # Suppress the warning that a few people are not race predicted
  bisg <- suppressWarnings( wru::predict_race(voter.file, 
                                         census.geo=census.geo, 
                                         census.key = census.key,
                                         census.data = census.data,
                                         census.surname = census.surname, surname.only = surname.only,
                                         surname.year = surname.year, age = age, sex = sex, party, 
                                         retry = retry))
  
  # Print out Number of NA names not matching Census Lists #
  no_name <- as.vector( table(is.na(bisg$pred.whi))["TRUE"] )
  print(paste("Number of Names not matching Census List: ", no_name, sep=""))
  
  ## Convert NA probabilities to 0 ##
  bisg$pred.whi[is.na(bisg$pred.whi)] <- 0 # White
  bisg$pred.bla[is.na(bisg$pred.bla)] <- 0 # Black
  bisg$pred.his[is.na(bisg$pred.his)] <- 0 # Hispanic
  bisg$pred.asi[is.na(bisg$pred.asi)] <- 0 # Asian
  bisg$pred.oth[is.na(bisg$pred.oth)] <- 0 # Race: Other
  
  return(list(voter.file = voter.file, bisg = bisg))
  
}