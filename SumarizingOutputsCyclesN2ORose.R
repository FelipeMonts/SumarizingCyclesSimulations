
#####     Extracting Nitrogen Daily outputs for Corn#  Felipe Montes
#  2016 02 15
#  Program to extract and average outputs from N2O Rose Simulations by Dr. Saha.


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


Simulation.File<-FileNames[1] ; 

# Column numbers from the season soutput that are to be kept for data extraction

ColumnsSeason<-c(1,2,5,16)  ;

#  Reading Column headers 


SeasonOutput.header<-readWorksheetFromFile(Simulation.File, sheet = "Season Output", startRow = 3, endRow=5, header=F,keep=ColumnsSeason);

# Composing and saving the season ouptput column names


SeasonColNames<-c(SeasonOutput.header[3,1],SeasonOutput.header[3,2],paste(SeasonOutput.header[1,3], SeasonOutput.header[2,3],SeasonOutput.header[3,3],sep="_"),paste(SeasonOutput.header[1,4], SeasonOutput.header[2,4],SeasonOutput.header[3,4],sep="_")) ;

#  Reading data columns in each file of contained in the simulations directory

for (i in FileNames) {


     
     #  Read data columns of in the season output of the simulation ouput spreadsheet
     
     
     SeasonOutput<-readWorksheetFromFile(Simulation.File, sheet = "Season Output", startRow = 6,header=F,keep=ColumnsSeason); 

     # Naming the data columns read


     names(SeasonOutput)<-SeasonColNames



     #   Extracting Maize Rows and the row above

     MaizeRows<-which(SeasonOutput$Crop==c("Maize")) ;


     Corn<-SeasonOutput[c(MaizeRows,MaizeRows-1),]  ;
     
     Corn$File<-i

     Corn<-Corn[do.call(order,Corn),] ;
     




     write.table(Corn, file="..\\CornYieldSeasonSummary.csv",append=T, sep= ",",row.names = F) ;
     
}


##### Remove objects to open memory space

rm(Corn,SeasonOutput.header,SeasonOutput) ;





# Column numbers from the daily output that are to be kept for data extraction


ColumnsDaily<-c(1,7:22) ;


#  Reading Column headers 


DailyOutput.header<-readWorksheetFromFile(Simulation.File, sheet = "Daily Outputs", startRow = 3, endRow=7, header=F, keep=ColumnsDaily);


# Composing and saving the daily ouptput column names


DailyColNames<-c(paste(DailyOutput.header[2,],DailyOutput.header[3,],DailyOutput.header[4,],DailyOutput.header[5,],sep="_"))   ;

DailyColNames[2]<-c("Rotation_Stage_Crop") ;


#  Read data columns of in the season output of the simulation ouput spreadsheet
     
     
     DailyOutput<-readWorksheetFromFile(Simulation.File, sheet = "Daily Outputs", startRow= 8, header=F, keep=ColumnsDaily); 

     # Naming the data columns read


     names(DailyOutput)<-DailyColNames  ;




     #   Extracting Maize Rows 


     MaizeRows<-which(DailyOutput[,2] ==c("Maize")) ;
     
     

     CornDaily<-DailyOutput[MaizeRows,] ;

     # Extracting the year from the date column and creating a factor with it


     
      CornDaily$Year<-as.factor(format(CornDaily$NA_NA_NA_Date,"%Y"));










     


     
     rm(DailyOutput)

























