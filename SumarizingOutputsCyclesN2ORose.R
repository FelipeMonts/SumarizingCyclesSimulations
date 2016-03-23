
#####     Extracting Nitrogen Daily outputs for Corn#  Felipe Montes
#  2016 02 15
#  Program to extract and average outputs from N2O Rose Simulations by Dr. Saha.

#  2016 03 21
#  Added samll program to convert column names in excell (A,B,C,D....AD, AE...) into column numbers for selection in R (1,2,3,4, ....)
#  Continue working on sumarizing the necesary daily outputs 
#  2016 03 22 Added the selection of transitions rows between crops and fallow

#  2016 03 23
#  Abandon the fallow row selection approach as it was getting too complicated and not getting the desired results
#  The approach using now is to select all the daily aoutput rows for Maize, Red Clover and Alfalfa, and work with them






########### Tell the program where the package libraries are  #####################


.libPaths("C:/Felipe/R_Library/library")

#  Set Working directory

setwd("C:\\Felipe\\OrganicTransitions_N2OROSE\\CyclesSimulation\\SahaN2ORoseSimulations\\SimulationFolders");


#  Inlcude the necesary packages

library(lattice); 
options(java.parameters = "-Xmx4g" );
library(XLConnect);


# Get the names of the files in the working directory

FileNames<-list.files() ;


# Name of the file from when data is going to be extracted


#Simulation.File<-FileNames[1] ; 

# Line of codes to convert exell column names to couln numbers to easy extraction in R

Excel.Columns<-c(LETTERS, paste0("A",LETTERS),paste0("B",LETTERS),paste0("C",LETTERS));

# Column numbers from the season soutput that are to be kept for data extraction

ColumnsSeason<-which(Excel.Columns %in% c("B", "C" , "F" , "M" , "N" , "O" , "Q"))-1;


#  Reading Column headers 


SeasonOutput.header<-readWorksheetFromFile(FileNames[1], sheet = "Season Output", startRow = 3, endRow=5, header=F,keep=ColumnsSeason);

# Composing and saving the season ouptput column names


SeasonColNames<-paste(SeasonOutput.header[1,],SeasonOutput.header[2,],SeasonOutput.header[3,],sep="_") ;

#  Reading data columns in each file of contained in the simulations directory

for (i in FileNames) {

     
     #  Read data columns of in the season output of the simulation ouput spreadsheet
     
     
     SeasonOutput<-readWorksheetFromFile(i, sheet = "Season Output", startRow = 6,header=F,keep=ColumnsSeason); 

     # Naming the data columns read


     names(SeasonOutput)<-SeasonColNames



     #   Extracting Maize Rows and the row above

     MaizeRows<-which(SeasonOutput$NA_NA_Crop ==c("Maize")) ;


     Corn<-SeasonOutput[c(MaizeRows,MaizeRows-1),]  ;
     
     Corn$File<-i

     Corn<-Corn[do.call(order,Corn),] ;
     


     write.table(Corn, file="..\\CornYieldSeasonSummary.csv",append=T, sep= ",",row.names = F) ;
     
    
     #   Remove objects to open memory space
     
     rm(Corn,SeasonOutput.header,SeasonOutput) ;
     
}




#### Starting Daily outputs data query


# Column numbers from the daily output that are to be kept for data extraction


     ColumnsDaily<-which(Excel.Columns %in% c("A","B","G","H","N","AT","AU","AX","BA","BC","BD","BG"));



     #  Reading Column headers 


     DailyOutput.header<-readWorksheetFromFile(FileNames[1], sheet = "Daily Outputs", startRow = 3, endRow=7, header=F, keep=ColumnsDaily);


     # Composing and saving the daily ouptput column names


DailyColNames<-c(paste(DailyOutput.header[2,],DailyOutput.header[3,],DailyOutput.header[4,],DailyOutput.header[5,],sep="_"))   ;



for (j in FileNames) {


     #  Read data columns of in the daily output of the simulation ouput spreadsheet
     
     
     DailyOutput<-readWorksheetFromFile(j, sheet = "Daily Outputs", startRow= 8, header=F, keep=ColumnsDaily); 


     # Naming the data columns read


     names(DailyOutput)<-DailyColNames  ;


     DailyOutput$Year<-as.factor(format(DailyOutput$NA_NA_NA_Date,format="%Y"));


     #   Extracting Nitrogen available in  Red Clover and Alfalfa cover crops before Maize; 

     #   Selecting Maize rows in the outputs

     Maize.Rows<-DailyOutput[DailyOutput$`NA_Rotation_Stage_Crop Name` == c("Maize"),];

     #   Selecting Mineral N at cron planting

     Maize.Planting<-Maize.Rows[Maize.Rows$NA_Crop_Growth_Stage == c("Planting"), ];

     Maize.Planting$MineralN<-Maize.Planting$"Profile_Soil_Nitrate_kg N/ha" + Maize.Planting$"Profile_Soil_Ammonium_kg N/ha";

     #   Calculating Net Mineralization in Corn

     Maize.NMineralization<-tapply(Maize.Rows$"Nitrogen_Net_Mineralization_kg N/ha",Maize.Rows$Year,sum);


     #   Nitrogen leaching during corn planting

     Maize.NLeaching<-tapply(Maize.Rows$"NA_Nitrate_Leaching_kg N/ha",Maize.Rows$Year,sum);

     Maize.NH4Volatilization<-tapply(Maize.Rows$"NA_Ammonia_Volatilization_kg N/ha",Maize.Rows$Year,sum);

     Maize.N2OEmissions<-tapply(Maize.Rows$"Nitrous Oxide_from_Denitrification_kg N/ha",Maize.Rows$Year,sum);

     Maize.NGaseousLosses<-Maize.NH4Volatilization + Maize.N2OEmissions;

     #  Soil Health During Corn

     Maize.SoilOC.planting<-tapply(Maize.Rows$`Soil_Organic_Carbon_Mg/ha`,Maize.Rows$Year,max);

     # Maize rows at Maturity

     Maize.Rows.Maturity<-Maize.Rows[Maize.Rows$NA_Crop_Growth_Stage == c("Maturity"),];  

     # Selectimg the minimum Soil Organic carbon at corn maturity by year

     Maize.SoilOC.Maturiy<-tapply(Maize.Rows.Maturity$`Soil_Organic_Carbon_Mg/ha`,Maize.Rows.Maturity$Year,min);


     #   Selecting Red Clover rows in the outputs

     RedClover.Rows<-DailyOutput[DailyOutput$`NA_Rotation_Stage_Crop Name` == c("Red Clover"),];


     #   Selecting the maximum total crop N of red clover cover by year

     RedClover.TotalCropN<-tapply(RedClover.Rows$`Total_Crop_Nitrogen_kg N/ha`,RedClover.Rows$Year,max);


     #   Selecting Alfalfa rows in the outputs

     Alfalfa.Rows<-DailyOutput[DailyOutput$`NA_Rotation_Stage_Crop Name`== c("Alfalfa"),];


     #   Selecting the maximum total crop N of alfalfa by year

     Alfalfa.TotalCropN<-tapply(Alfalfa.Rows$`Total_Crop_Nitrogen_kg N/ha`,Alfalfa.Rows$Year,max);




     # Sumarizing the results:

     DailyOutput.Summary<-data.frame(Maize.NMineralization,Maize.NLeaching,Maize.NH4Volatilization,Maize.N2OEmissions,Maize.NGaseousLosses,Maize.SoilOC.planting,Maize.SoilOC.Maturiy,RedClover.TotalCropN,Alfalfa.TotalCropN);

     #writting table with the summary

     DailyOutput.Summary$File<-j;

     write.table(DailyOutput.Summary, file="..\\DailyOutput.Summary.csv",append=T, sep= ",",row.names = F) ;
     
     
     rm(DailyOutput.Summary,DailyOutput.header,DailyOutput) ;
}






















