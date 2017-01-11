# @author: Artur Sak

### This assignment will use Google Earth for data display. 
### The .rda file is uploaded to Moodle.

### Load hw8.rda and attach the XML library

### Part 1.  Create the data frame
### Look at the instructions in HW8.pdf.
### Functions you'll want to use: xmlParse(), xmlRoot(), xpathSApply(), xmlGetAttr().
### It also might make it easier to use: xmlToList(), merge().

### Load the data frame called LatLon from HW8.rda.  
load("HW8.rda")
require("XML")

### Download the gzipped XML factbook document from
### http://jmatchparser.sourceforge.net/factbook/
### and create an XML "tree" in R 
Factbook = "factbook.xml"
Parsed_Factbook = xmlParse(Factbook)

### Use XPath to extract the infant mortality and the CIA country codes from the XML tree
root = xmlRoot(Parsed_Factbook)
XPATH = '//field[@name="Infant mortality rate"]//rank'

Infant_Mort = as.numeric(xpathSApply(root, XPATH, xmlGetAttr,"number"))
CIA_Codes = toupper(xpathSApply(root, XPATH, xmlGetAttr, "country"))

### Create a data frame called IM using this XML file.
### The data frame should have 2 columns: for Infant Mortality and CIA.Codes.
IM = data.frame("Infant Mortality" = Infant_Mort, "CIA.Codes" = CIA_Codes)
head(IM)

### Extract the country populations from the same XML document
### Create a data frame called Pop using these data.
### This data frame should also have 2 columns, for Population and CIA.Codes.
XPATH = '//field[@name="Population"]//rank'

Population = as.numeric(xpathSApply(root, XPATH, xmlGetAttr,"number"))
CIA_Codes = toupper(xpathSApply(root, XPATH, xmlGetAttr, "country"))

Pop = data.frame("Population" = Population, "CIA.Codes" = CIA_Codes)
head(Pop)

### Merge the two data frames to create a data frame called IMPop with 3 columns:
### IM, Pop, and CIA.Codes
IMPop = merge(IM, Pop, by = intersect(names(IM), names(Pop)))
head(IMPop)

### Now merge IMPop with LatLon (from HW8.rda) to create a data frame called AllData that has 6 columns
### for Latitude, Longitude, CIA.Codes, Country Name, Population, and Infant Mortality
### (please check lat,long are not reversed in the file)
AllData = merge(IMPop, LatLon, by=intersect(names(IMPop), names(LatLon)))
head(AllData)

### Part 2.  Create a KML document
### Make the KML document described in HW8.pdf.  It should have the basic
### structure shown in that document.  You can use the addPlacemark function below to make
### the Placemark nodes, you just need to complete the line for the Point node and
### figure out how to use the function.
makeBaseDocument = function(){
	# ### This code creates the template KML document
	doc 		= newXMLDoc()
	root_ptr 	= newXMLNode("kml", doc = doc)
	root 		= newXMLNode("Document", parent = root_ptr)
	
	newXMLNode(name = "name", "Country Facts", parent = root)
	newXMLNode(name = "description", "Infant Mortality", parent = root)
	
	look_at 	= newXMLNode("LookAt", parent =  root)

	newXMLNode('longitude', -121, parent = look_at)
	newXMLNode('latitude', 43, parent = look_at)
	newXMLNode('altitude', 4100000, parent = look_at)

	newXMLNode('tilt', 0, parent = look_at)
	newXMLNode('heading', 0, parent = look_at)
	newXMLNode('altitudeMode', 'absolute', parent = look_at)
	
	
	folder = newXMLNode(name = "Folder", parent = root)
	newXMLNode(name = "name", "CIA Fact Book", parent= folder)

	return(doc)
}

kml = makeBaseDocument()
document = xmlChildren(kml)[[1]][[1]] 
folder = document[[4]]

addPlacemark = function(lat, lon, ctryCode, ctryName, pop, infM, parent, inf1, pop1, style = FALSE) {
	pm = newXMLNode("Placemark", newXMLNode("name", ctryName), attrs = c(id = ctryCode), parent = parent)
	newXMLNode("description", paste(ctryName, "\n Population: ", pop, "\n Infant Mortality: ", infM, sep =""), parent = pm)
	newXMLNode("Point", newXMLNode("coordinates", paste(lon, ", ", lat, ", ", 0, sep="")), parent = pm)
	### You need to fill in the code for making the Point node above, including coordinates.
	### The line below won't work until you've run the code for the next section to set up
	### the styles.
	if(style) newXMLNode("styleUrl", paste("#YOR", inf1, "-", pop1, sep = ''), parent = pm)
}

invisible(lapply(1:nrow(AllData), function(x) {
	addPlacemark(
		lat 		= AllData$Latitude[[x]],
		lon 		= AllData$Longitude[[x]],
		ctryCode 	= AllData$CIA.Codes[[x]],
		ctryName 	= AllData$Country.Name[[x]],
		pop 		= AllData$Population[[x]],
		infM 		= AllData$Infant.Mortality[[x]],
		parent 		= folder
		)
	}
))

### Save your KML document here, call it Part2.kml, and open it in Google Earth.
### (You will need to install Google Earth.)  
### It should have pushpins for all the countries.
saveXML(doc = kml, file = "Part2.kml")

### Part 3.  Add Style to your KML
### Now you are going to make the visualizatiion a bit fancier.  Pretty much all the code is given to you
### below to create style elements that are to be placed near the top of the document.
### These, you just need to figure out what it all does.

### Start fresh with a new KML document, by calling makeBaseDocument()
doc2 = makeBaseDocument()
DocNode = xmlChildren(doc2)[[1]][[1]]
folder2 = DocNode[[4]]

### The following code is an example of how to create cut points for 
### different categories of infant mortality and population size.
### Figure out what cut points you want to use and modify the code to create these 
### categories.
infCut = cut(AllData$Infant.Mortality, breaks = c(0, 10, 25, 50, 75, 200))
infCut = as.numeric(infCut)
popCut = cut(log(AllData$Population), breaks = 5)
popCut = as.numeric(popCut)

### Now figure out how to add styles and placemarks to doc2
### You'll want to use the addPlacemark function with style = TRUE

### Below is code to make style nodes. 
### You should not need to do much to it.

### You do want to figure out what scales to you for the sizes of your circles
scales = c(0.5, 1, 3, 5, 10)

addStyle = function(j, k, parent, urlBase, scales = scales) {
	colors = c("blue", "green", "orange", "red", "yellow")
	st = newXMLNode("Style", attrs = c("id" = paste("YOR", j, "-", k, sep="")), parent = parent)
	is = newXMLNode("IconStyle", parent = st)
	newXMLNode("scale", scales[k], parent = is)
	newXMLNode("Icon", paste(urlBase, "color_label_circle_", colors[j], ".png", sep =""), parent = is)
}

### You will need to figure out what order to call addStyle() and addPlacemark()
### so that the tree is built properly. You may need to adjust the code to call the png files
for(k in 1:5) {
  for(j in 1:5) {
  	addStyle(j, k, folder2, 'http://www.stanford.edu/~vcs/StatData/circles/', scales)
  }
}

invisible(lapply(1:nrow(AllData), function(x) {
	addPlacemark(
		lat 		= AllData$Latitude[[x]],
		lon 		= AllData$Longitude[[x]],
		ctryCode 	= AllData$CIA.Codes[[x]],
		ctryName 	= AllData$Country.Name[[x]],
		pop 		= AllData$Population[[x]],
		infM 		= AllData$Infant.Mortality[[x]],
		parent 		= folder2,
		inf1		= infCut[[x]],
		pop1 		= popCut[[x]],
		style 		= TRUE
		)
	}
))

### Finally, save your KML document, call it Part3.kml and open it in Google Earth to 
### verify that it works.  For this assignment, you only need to submit your code, 
### nothing else.  You can assume that the grader has already loaded HW8.rda.
saveXML(doc = doc2, file = "Part3.kml")
