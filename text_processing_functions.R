# This is from some work I did before I took this class.
# It is located at a gist:
# https://gist.github.com/ClaytonJRamsey/d7e815065aad17042e888bf56be696af

word_data <- function(text){
  # break the text into characters
  rawcharacters <- strsplit(text, "")[[1]]
  # adding a space to the beginning and end of the vector. This is because the code later on 
  # detects the starting letter of a word by seeing what follows a space and also detects when
  # a space follows the character, meaning that letter is the end of the word.
  charvector <- rep(" ", length(rawcharacters)+2)
  charvector[2:(length(rawcharacters)+1)] <- rawcharacters
  # removing all non-space, non-letter characters.
  lettersandwhitespace_location <- which(is.element(charvector, letters) | is.element(charvector, LETTERS) | charvector == " ")
  lettersandwhitespace <- charvector[lettersandwhitespace_location]
  # detecting capital letters and converting them to lowercase.
  capitals_location <- which(is.element(lettersandwhitespace, LETTERS))
  if(length(capitals_location > 0)){
    capitals <- lettersandwhitespace[capitals_location]
    lowercase <- tolower(capitals)
    lettersandwhitespace[capitals_location] <- lowercase
  }
  # This is what the function returns. The entries "beginning" and "ending" contain vectors of all the
  # beginning and ending letters of the words from the text. Each entry named after a letter contains a vector of the
  # characters following that letter, with spaces for incidences where the letter terminates a word.
  following <- list()
  beginlocationsraw <- which(lettersandwhitespace == " ")+1
  endlocationsraw <- which(lettersandwhitespace == " ")-1
  beginlength <- length(beginlocationsraw)
  endlength<- length(endlocationsraw)
  # This chops off the last space since a word doesn't start after it.
  beginlocations <- beginlocationsraw[1:(beginlength-1)]
  # and this chops off the first space.
  endlocations <- endlocationsraw[2:(beginlength)]
  following[["beginning"]] <- lettersandwhitespace[beginlocations]
  following[["ending"]] <- lettersandwhitespace[endlocations]
  for(i in letters){
    following[[i]] <- lettersandwhitespace[1+which(lettersandwhitespace == i)]
  }
  # checking where spaces are lets us calculate word lengths.
  spacelocations <- beginlocationsraw - 1
  spaceloop <- length(spacelocations)-1
  wordlengths <- vector("numeric", spaceloop)
  for(i in 1:spaceloop){
    wordlengths[i] <- spacelocations[i+1] - spacelocations[i] - 1
  }
  following[["wordlengths"]] <- wordlengths
  return(following)
}

# Function to generate English-like nonsense by sampling from a report
# generated from the above function.
fenglish = function(longness, tx_report){
  generated <- vector("character", length = longness)
  generated[1] <- sample(tx_report$beginning, 1)
  for(j in 2:longness){
    priorchar <- generated[j-1]
    if(priorchar != " "){
      generated[j] <- sample(tx_report[[priorchar]], 1)
    }
    if(priorchar == " "){
      generated[j] <- sample(tx_report$beginning, 1)
    }
  }
  return(paste(generated, collapse = ""))
}


capitalize <- function(s){
  c <- strsplit(s, "")
  fl <- toupper(c[[1]][1])
  o <- paste0(fl, paste(c[[1]][-1], collapse = ""))
  return(o)
}

poke_following_freq <- word_data(paste0(poke_data$name, collapse = " "))

random_pokemon <- function(n = 0){
  # need to remove those spaces...
  base <- capitalize(fenglish(1+rnorm(1, mean = 7, sd = 2), poke_following_freq))
  c <- strsplit(base, "")
  c <- c[[1]][which(c[[1]] != " ")]
  return(paste0(c, collapse = ""))
}
v_poke <- Vectorize(random_pokemon)

# to make up random stats to use the random forest on.
stat_sampler <- function(n){
  return(sample(poke_data_model[[n]], 1))
}
  
  