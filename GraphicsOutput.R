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
#  Code to plot the relationship between cover crop Biomass before planting and Transpiration
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

## adding a year columd to the data set

Alfalfa.Season_Output$Year<-format(Alfalfa.Season_Output$NA_NA_Date,format="%Y") ;


### plot Time Series of Above ground residue


plot(Alfalfa.Season_Output$NA_NA_Date,Alfalfa.Season_Output$Aboveground_Residue_Mg.ha,pch=19,col="RED") ;


### Plot Transpiration vs Above ground biomass


plot(Alfalfa.Season_Output$Potential_Transpiration_mm,Alfalfa.Season_Output$Aboveground_Residue_Mg.ha, pch=19, col="RED") ;
points(Alfalfa.Season_Output$Actual_Transpiration_mm,Alfalfa.Season_Output$Aboveground_Residue_Mg.ha, pch=19, col="BLUE") ;  


### Order the Output by year

Alfalfa.Season_Output<-Alfalfa.Season_Output[order(Alfalfa.Season_Output$NA_NA_Date),];


### Plot Time series of nitrogen

barplot(t(as.matrix(Alfalfa.Season_Output[,c("Total_Nitrogen_Mg.ha","Root_Nitrogen_Mg.ha","Root_Nitrogen_Mg.ha")])),names.arg=Alfalfa.Season_Output$Year,legend.text = T) ;


