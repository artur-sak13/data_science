# Question 1
# Write down a general regular expression to match the following:

# (a) Words with @ symbols in them, e.g., h@te or v|c0din
test = c("h@te", "v|c0din", "@rt", "g@@dsh|z", "sameTBH", "CDT")
print(grep("\\@", test))

# (b) An IP address (Four sets of 1 to 3 digits separated by periods, e.g., 100.12.162.0)
test = c("100.12.162.0", "127.0.0.1", "192.168.1.1", "192.168.1.255", "0.0.0.0", "1.1.1.01", "30.168.1.255.1", "127.1","192.168.1.256","-1.2.3.4", "3...3")
print(grep("^(?:\\d{1,3}\\.|\\d{1,3}){4}$", test))

# (c) An email address that ends with .com, .edu, .net, .org, or .gov
test = c("dude@sbcglobal.net", "bro@gmail.com", "JACK_DANIELS@yahoo.com", "qrwa@121@.gov", "com.blah.www/mahHObbis", "huehuehuehue", "wizardSLEEVE@gmail.pl")
print(grep("^\\w+@{1}\\w+\\.{1}\\w+{1,3}", test))

# Question 2

# a. Use readLines() to read in the 
# speeches (available as a text file 
# in moodle) where the return value is: 
# character vector with one element/character 
# string per line in the file

file = readLines("stateoftheunion1790-2012.txt")

# b. Use regular expressions to find ***
re = "[*]{3}"
speeches = grep(re,file)

# c. Use *** to identify the date of the speech
dates = file[speeches + 4]
dates = dates[!is.na(dates)]
head(dates,12)

# d. Use regular expressions to extract the year
re = "[[:digit:]]{4}"
yearPositions = as.numeric(gregexpr(re, dates))
years = substr(dates, start=yearPositions, stop=yearPositions + 4)
head(years, 12)

# e. Use regular expressions to extract the month
re = "[0-9, ]"
months = gsub(pattern=re, replacement="", dates)
head(months, 10)

# f. Use *** to extract the name of the president 
# State of the union speeches
presidents = file[speeches + 3]
presidents = presidents[!is.na(presidents)]
head(presidents,10)

# g. Use regular expressions and R to return the 
# number of speeches in the dataset, and the number 
# of presidents that gave speeches

# Assume *** are nodes and speeches are edges, therefore document has n-1 speeches
length(file[speeches]) - 1

length(unique(presidents))
unique(presidents)

# h. Chop the speeches up into a list there is one 
# element for each speech. Each element is a character 
# vector. Check: does your number of list elements match 
# your answer above?

speechList = list()
for(i in 2:length(speeches)-1){
	speechList[[i]] = paste(file[(speeches[i] + 6):(speeches[i+1] - 1)], sep="", collapse=" ")
}
length(speechList)

# i. Eliminate apostrophes, numbers, and the phrase: (Applause.)
cleanSpeech = function(speech) {
	modSpeech = gsub(pattern="[[:punct:]]?[0-9'][[:punct:]]?", replacement="", x=speech)
	modSpeech = gsub(pattern="[[:punct:]]?Applause[[:punct:]]{0,2}", replacement="", x=modSpeech, ignore.case=T)
	return(modSpeech)
}

speechList = lapply(speechList, cleanSpeech)

# j. Make all the characters lower case.
speechList = tolower(speechList)

# k. Split the sentences up where there are 
# blanks and punctuation to create “words”
speechList = strsplit(speechList, split="([[:blank:]]|[[:punct:]])")

# l. Drop any empty words that resulted from this split.
speechList[] = lapply(speechList[], function(x) x[x!=""])

# m. Create a word vector for each speech.
words = list()
uniqWords = numeric()
for(i in 1:length(speechList)) {
	uniqWords = c(uniqWords, length(unique(speechList[[i]])))
	words[[i]] = as.vector(table(speechList[[i]]))
}

# n. Normalize the word vectors to get term frequencies.
normWords = lapply(words[], function(x) x/sum(x))

# o. Carry out some exploratory analysis of the data and term frequencies. 
# For example, find the number of sentences, extract the long words, and the 
# political party. Plot and interpret the term frequencies. What are your observations?

png("Rplots.png",width=800,height=400)
plot(years, uniqWords, type="l", 
	main="Number of Unique Words in SOTU Over Time", 
	xlab="January 8, 1790 - January 24, 2012",
	ylab="Number of Unique Words")
abline(h=mean(uniqWords), col="blue", lwd=2)
legend('topleft', c("Mean of Unique Words"), lty=1, lwd=2, col="blue")

# The number of unique words used in SOTU speeches seems to have increased over time
