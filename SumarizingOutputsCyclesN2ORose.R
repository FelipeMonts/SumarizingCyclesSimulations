
#####     Extracting Nitrogen Daily outputs for Corn#  Felipe Montes
#  2016 02 15
#  Program to extract and average outputs from N2O Rose Simulations by Dr. Saha.

#  2016 03 21
#  Added samll program to convert column names in excell (A,B,C,D....AD, AE...) into column numbers for selection in R (1,2,3,4, ....)
#  Continue working on sumarizing the necesary daily outputs 
#  2016 03 22 Added the selection of transitions rows between crops and fallow




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

# Line of codes to convert exell column names to couln numbers to easy extraction in R

Excel.Columns<-c(LETTERS, paste0("A",LETTERS),paste0("B",LETTERS),paste0("C",LETTERS));

# Column numbers from the season soutput that are to be kept for data extraction

ColumnsSeason<-which(Excel.Columns %in% c("B", "C","F","Q"))-1;


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


ColumnsDaily<-which(Excel.Columns %in% c("A","B","G","H","N","AT","AU","AX","BA","BC","BD","BG"));



#  Reading Column headers 


DailyOutput.header<-readWorksheetFromFile(Simulation.File, sheet = "Daily Outputs", startRow = 3, endRow=7, header=F, keep=ColumnsDaily);


# Composing and saving the daily ouptput column names


DailyColNames<-c(paste(DailyOutput.header[2,],DailyOutput.header[3,],DailyOutput.header[4,],DailyOutput.header[5,],sep="_"))   ;


#  Read data columns of in the season output of the simulation ouput spreadsheet
     
     
DailyOutput<-readWorksheetFromFile(Simulation.File, sheet = "Daily Outputs", startRow= 8, header=F, keep=ColumnsDaily); 


# Naming the data columns read


names(DailyOutput)<-DailyColNames  ;


#   Extracting Nitrogen available in  Red Clover and Alfalfa cover crops before Maize; 

#   Selecting fallow rows in the outputs

Fallow.Rows<-data.frame(which(DailyOutput[,2] == c("Fallow"))); names(Fallow.Rows)<-c("Fallow");

#   Selecting one row before any Fallow row

Fallow.Rows$Before<-Fallow.Rows$Fallow-1;

#   Selecting one row after any Fallow row#

Fallow.Rows$After<-Fallow.Rows$Fallow+1;


#   Select Rows$Before not in Fallow Rows and Rows$After not in Fallow Rows except the first row

     
Fallow.DayliOutput<-Fallow.Rows[!1 & !Fallow.Rows$Before %in% Fallow.Rows$Fallow | !Fallow.Rows$After %in% Fallow.Rows$Fallow ,] ;


#   Disolve the data frame Fallow.DayliOutput into a vector of row numbers and order it to obtain the rows in DailyOutput that are in transition from fallow to crop or rom crop to fallow  

Fallow.CropsRows<-DailyOutput[sort(unlist(Fallow.DayliOutput,use.names = F)),];


#   Select total crop Nitrogen from Red Clover


RedClover<-Fallow.CropsRows[Fallow.CropsRows$'NA_Rotation_Stage_Crop Name'==c("Red Clover"),];

RedClover.N<-DailyOutput[as.numeric(row.names(RedClover))-1,];

which(RedClover$`NA_Rotation_Stage_Crop Name` %in% Fallow.CropsRows$'NA_Rotation_Stage_Crop Name')



#    Select the rows with the last occurence before fallow, of alafalfa and red clover

     RedClover<-which(DailyOutput[Fallow.Rows-1,3]==c("Maize")) ; 




DailyOutput$`NA_Rotation_Stage_Crop Name`==c("Maize"),] ;

     
     

     <-DailyOutput[MaizeCloverAlfalfa.Rows,];




     # Extracting the year from the date column and creating a factor with it

     
      CornDaily$Year<-as.factor(format(CornDaily$NA_NA_NA_Date,"%Y"));
















     


     
     rm(DailyOutput)

























