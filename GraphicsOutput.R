##############################################################################################################################
#
#  Program to visualize N2O Rose Simulation summary by Dr. Debasish Saha.
#
#  Felipe Montes,  2016/06/08
#
##############################################################################################################################






###############################################################################################################
#                          Loading Packages and setting up working directory                        
###############################################################################################################



#  Tell the program where the package libraries are  #####################


.libPaths("C:/Felipe/R_Library/library")

#  Set Working directory

setwd("C:\\Felipe\\OrganicTransitions_N2OROSE\\CyclesSimulation\\SahaN2ORoseSimulations\\SimulationFolders");


#  Inlcude the necesary packages

library(lattice); 

options(java.parameters = "-Xmx4g" ); 

#  options(java.parameters = "-Xmx1024m" );

library(XLConnect);



# Create a directoty to collect the grphs sumarizing the simulations 

dir.create("..\\OutputSummary\\Graphics");  



###############################################################################################################
#                          Code to extract excell columns by letter name                                  
###############################################################################################################


# Line of codes to convert exell column names to couln numbers to easy extraction in R

Excel.Columns<-c(LETTERS, paste0("A",LETTERS),paste0("B",LETTERS),paste0("C",LETTERS));


###############################################################################################################
#                        Code to plot the roation system used in the simulations
###############################################################################################################


## Read the  crop rotation files

Crop_Rotaion_Header<-readWorksheetFromFile("ManureasbyWade_Setting.xls", sheet = "Planting Order", startRow = 4,endRow = 5,header=F) ;

Crop_Rotaion_ColNames<-paste(Crop_Rotaion_Header[1,],Crop_Rotaion_Header[2,],sep="_");

Crop_Rotaion<-readWorksheetFromFile("ManureasbyWade_Setting.xls", sheet = "Planting Order", startRow = 6,header=F) ;

names(Crop_Rotaion)<-Crop_Rotaion_ColNames[1:3] ;




##  Add the 4 years of rotation with alfalfa

fix(Crop_Rotaion) ;
#    Rotation_Year Calendar_Day    Crop_Name
# 1              1          141        Maize
# 2              1          310   Winter Rye
# 3              2          146      Soybean
# 4              2          306 Winter Wheat
# 5              3          249   Red Clover
# 6              4          141        Maize
# 7              4          310   Winter Rye
# 8              5          146      Soybean
# 9              5          306 Winter Wheat
# 10             6          243      Alfalfa
# 11            10          365      Alfalfa
# 12             1            1      Alfalfa

Crop_Rotaion<-Crop_Rotaion[order(Crop_Rotaion$Rotation_Year,Crop_Rotaion$Calendar_Day),] ;




## Convert the Rotation year and Calendar Year to a one variable to be plotted in the staked horizontal bars


Crop_Rotaion$DaysPlanted<-((Crop_Rotaion$Rotation_Year-1)*365)+Crop_Rotaion$Calendar_Day ;

Crop_Rotaion$DaysAfterPlanting<-c(0,Crop_Rotaion$DaysPlanted[2:12]-Crop_Rotaion$DaysPlanted[1:11]) ;


##  Combine the Roations into a thought experiment in which each year rotation changes by a year

Crop_Rotaion$

barplot(Crop_Rotaion$DaysAfterPlanting, names.arg = Crop_Rotaion$Crop_Name )  ;




###############################################################################################################
#              Code to plot the relationship between cover crop Biomass before planting and Transpiration
###############################################################################################################


### Select the spreadsheet columns to be read from the Spreadsheet. 


ColumnsSeason_Output<-which(Excel.Columns %in% c("A", "B" , "E" , "F" ,"G", "H" , "I", "K" ));



### Read the data from the Season_Output spreadsheet in the summary Excell workbook


Season_Output<-readWorksheetFromFile("..\\OutputSummary\\CyclesOutputSummary.xlsx", sheet = "Season_Output", startRow = 1, header=T, keep=ColumnsSeason_Output);


###  Select the years with Red Clover


RedClover.Season_Output<-Season_Output[Season_Output$NA_NA_Crop == "Red Clover",] ;


### plot Time Series of Above ground residue


plot(RedClover.Season_Output$NA_NA_Date,RedClover.Season_Output$Aboveground_Residue_Mg.ha,pch=19,col="RED") ;


### Plot Transpiration vs Above ground biomass


plot(RedClover.Season_Output$Potential_Transpiration_mm,RedClover.Season_Output$Aboveground_Residue_Mg.ha, pch=19, col="RED") 
# points(RedClover.Season_Output$Actual_Transpiration_mm,RedClover.Season_Output$Aboveground_Residue_Mg.ha, pch=19, col="BLUE") ;  For Red Clover have Zero water deficit and therefore Potential and Actual traspiration are the same


###  Select the years with Alfalfa


Alfalfa.Season_Output<-Season_Output[Season_Output$NA_NA_Crop == "Alfalfa",] ;

## adding a year colum to the data set

Alfalfa.Season_Output$Year<-format(Alfalfa.Season_Output$NA_NA_Date,format="%Y") ;


### plot Time Series of Above ground residue


plot(Alfalfa.Season_Output$NA_NA_Date,Alfalfa.Season_Output$Aboveground_Residue_Mg.ha,pch=19,col="RED") ;


### Plot Transpiration vs Above ground biomass


plot(Alfalfa.Season_Output$Potential_Transpiration_mm,Alfalfa.Season_Output$Aboveground_Residue_Mg.ha, pch=19, col="RED") ;
points(Alfalfa.Season_Output$Actual_Transpiration_mm,Alfalfa.Season_Output$Aboveground_Residue_Mg.ha, pch=19, col="BLUE") ;  


### Order the Output by year

Alfalfa.Season_Output<-Alfalfa.Season_Output[order(Alfalfa.Season_Output$NA_NA_Date),];


###  Combine the Red Clover and Alfalfa Residue plots into a 2 by two series of graphs;



pdf(file="..\\OutputSummary\\Graphics\\RodaleVisit.pdf",onefile=T, width=17, height=11) ;


par(mfrow = c(2,2)) ;


# Residue Time Series


plot(RedClover.Season_Output$NA_NA_Date,RedClover.Season_Output$Aboveground_Residue_Mg.ha, pch = 21, col = "BLACK",bg = "RED" , ylim = c(0,4), ylab = "Fresh biomass  Mg/Ha", main = "Cover crop biomass before Corn", xlab="",cex=1.5);
points(Alfalfa.Season_Output$NA_NA_Date,Alfalfa.Season_Output$Aboveground_Residue_Mg.ha, pch=21 ,col = "BLACK" , bg = "GREEN", cex = 1.5) ;
legend("topleft" , c("Red Clover" , "Alfalfa"),fill=c("RED","GREEN"));


# Residue and Evapotranspiration

plot(RedClover.Season_Output$Actual_Transpiration_mm,RedClover.Season_Output$Aboveground_Residue_Mg.ha, pch = 21, col = "BLACK" , bg = "RED", cex =  1.5 ,ylim = c(0,4), xlim = c(10,170),ylab = "Cover Crop biomass Mg/Ha", xlab = "Plant Transpiration mm", main = "Cover crop Biomass vs Transpiration");
points(Alfalfa.Season_Output$Actual_Transpiration_mm,Alfalfa.Season_Output$Aboveground_Residue_Mg.ha, pch = 21 , col="BLACK" , bg = "GREEN" , cex = 1.5 ) ; 
legend("topleft" , c("Red Clover" , "Alfalfa"),fill=c("RED","GREEN"));



barplot(t(as.matrix(Alfalfa.Season_Output[,c("Root_Nitrogen_Mg.ha","Forage_Nitrogen_Mg.ha")])*1000),names.arg=Alfalfa.Season_Output$Year,las= 2,col=c("chocolate4","darkgreen"),ylab = c("Nitrogen Kg/Ha"),main = "Alfalfa before Corn") ;
legend("topleft",c("Root","Forage"), fill=c("chocolate4","darkgreen"));



dev.off()


# ##########################################################################################################
# 
# 
# Create cumulative distribution plots of the plant biomass of alfalfa and red clover  before planting Corn
# 
# 
# ##########################################################################################################

Redclover_ecdf<-ecdf(RedClover.Season_Output$Aboveground_Residue_Mg.ha) ;

Alfalfa_ecdf<-ecdf(Alfalfa.Season_Output$Aboveground_Residue_Mg.ha) ;

summary(Redclover_ecdf) ;


plot(Alfalfa_ecdf, verticals= T , col.points="green" , do.points=F, col.hor = "green", col.vert="green",lwd =4, ylab="Cumulative distribution Cover crop Biomass" ,xlab="Biomass Mg/Ha", main="Cover crop Biomass before Corn Planting") ;

plot(Redclover_ecdf, verticals= T , col.points="red" , do.points=F, col.hor = "red", col.vert="red", lwd =4, add = T ) ;

abline(h=0.5, col="blue", lwd=2);

legend("topleft",c("Red Clover" , "Alfalfa"), col= c("red" , "green"), fill=c("red" , "green"), bty = "n" )

# ##########################################################################################################
# 
#
#Plotting the 1-cumulative distribution of the plant biomass of alfalfa and red clover  before planting Corn
# 
# 
# ##########################################################################################################



#Ordering the data
Alfalfa.order<-sort(Alfalfa.Season_Output$Aboveground_Residue_Mg.ha) ;

Alfalfa.length<-length(Alfalfa.Season_Output$Aboveground_Residue_Mg.ha) ;

RedClover.order<-sort(RedClover.Season_Output$Aboveground_Residue_Mg.ha) ;

RedClover.length<-length(RedClover.Season_Output$Aboveground_Residue_Mg.ha) ;



plot(Alfalfa.order, 1-((1:Alfalfa.length)/Alfalfa.length), type = "s", col= "green" , ylim = c(0,1), xlim = c(0,4), lwd = 4, ylab="Cumulative Distribution" ,xlab="Biomass Mg/Ha", main="Biomass before Corn Planting") ;



points(RedClover.order, 1-((1:RedClover.length)/RedClover.length), type = "s", col= "red" ,lwd=4)  ;

abline(h=0.5, col="blue", lwd=2);

legend("topright",c("Red Clover" , "Alfalfa"), col= c("red" , "green"), fill=c("red" , "green"), bty = "n" )  ;






