
#####     Extracting Nitrogen Daily outputs for Corn#  Felipe Montes
#  2016 02 15
#  Program to extract and average outputs from N2O Rose Simulations by Dr. Saha.

#  2016 03 21
#  Added small program to convert column names in excell (A,B,C,D....AD, AE...) into column numbers for selection in R (1,2,3,4, ....)
#  Continue working on sumarizing the necesary daily outputs 
#  2016 03 22 Added the selection of transitions rows between crops and fallow

#  2016 03 23
#  Abandon the fallow row selection approach as it was getting too complicated and not getting the desired results
#  The approach using now is to select all the daily aoutput rows for Maize, Red Clover and Alfalfa, and work with them

#  2016 03 30 
#  Working on adding year to the season output
#  Add N2 to the gasous losses
#  Nitrogen synchrony from cover crop to the crop /or Manure application
#  Adding Outputs in pounds per acre and Bushels per acre
#  Temperature, drought and precipitation indices also need to be added


#  2016 04 12

# Added water deficit based on seasonal output for potential and realized evapotranspiration
# Added Potential Transpiration (Column J) and actual Transpiration (cloumn K) from the Season Outputs to get at the water stress
# Added Forage yield column
# Changeds the name of the variables back to the defoult original to make it easier to add an change variables


#2016 04 20

# Improved code ouptut and comments  structure
# Created a new directory to summarize outputs
# Added a collection file to store all the results and then process it into an excell work book





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
library(XLConnect);


# Get the names of the files in the working directory

FileNames<-list.files() ;


# Create a directoty to summarize the outputs

dir.create("..\\OutputSummary");  






###############################################################################################################
#                          Code to extract excell columns by letter name                                  
###############################################################################################################


# Line of codes to convert exell column names to couln numbers to easy extraction in R

Excel.Columns<-c(LETTERS, paste0("A",LETTERS),paste0("B",LETTERS),paste0("C",LETTERS));


###############################################################################################################
#                          Starting Season Outputs data query                                   
###############################################################################################################

# Column numbers from the season soutput that are to be kept for data extraction

ColumnsSeason<-which(Excel.Columns %in% c("B", "C" , "F" , "G" ,"H", "J" , "K", "M" , "N" , "O" , "P", "Q"))-1;


#  Reading Column headers 


SeasonOutput.header<-readWorksheetFromFile(FileNames[1], sheet = "Season Output", startRow = 3, endRow=5, header=F,keep=ColumnsSeason);

# Composing and saving the season ouptput column names


SeasonColNames<-paste(SeasonOutput.header[1,],SeasonOutput.header[2,],SeasonOutput.header[3,],sep="_") ;

#  Reading data columns in each file of contained in the simulations directory


#  Create a storing file to accumulate the data of the files after processing

SeasonOutput.summary<-data.frame()  ;



for (i in FileNames) {
     # i=FileNames[1]
     
     #  Read data columns of in the season output of the simulation ouput spreadsheet
     
     
     SeasonOutput<-readWorksheetFromFile(i, sheet = "Season Output", startRow = 6,header=F,keep=ColumnsSeason); 

     # Naming the data columns read


     names(SeasonOutput)<-SeasonColNames



     #   Extracting Maize Rows and the row above

     MaizeRows<-which(SeasonOutput$NA_NA_Crop ==c("Maize")) ;


     Corn<-SeasonOutput[c(MaizeRows,MaizeRows-1),]  ;
     
     
     # # Adding the values in pounds per acre and bushels per acre
     # # Based on the conversion tool in the Iowa State University Extension and Outreach Ag Decision Maker website
     # # http://www.extension.iastate.edu/agdm/wholefarm/html/c6-80.html
     # 
     # Mg_ha_to_Bushels_ac=1000*2.205/(56*2.471)
     # Mg_ha_to_lb_ac=1000*2.205/2.471
     # 
     # 
     # Corn$`Grain_Yield_Bushel/ac`<-Corn$`Grain_Yield_Mg/ha` * Mg_ha_to_Bushels_ac ;
     # 
     # Corn$`Forage_Yield_Bushel/ac`<-Corn$`Forage_Yield_Mg/ha` * Mg_ha_to_lb_ac ;
     # 
     # Corn$`Total_Nitrogen_lb/ac`<-Corn$`Total_Nitrogen_Mg/ha` * Mg_ha_to_lb_ac  ;
     # 
     # Corn$`Root_Nitrogen_lb/ac`<-Corn$`Root_Nitrogen_Mg/ha` * Mg_ha_to_lb_ac  ;
     # 
     # Corn$`Grain_Nitrogen_lb/ac`<-Corn$`Grain_Nitrogen_Mg/ha` *  Mg_ha_to_lb_ac  ;
     
     # Adding the Water strees indicator based in potential and realized evapotranspiration
     # The Water Stress or water deficit indicator will be calculated as: 1-[Actual_Transpiration_mm / Potential_Transpiration_mm ]
     
     Corn$WaterDeficit<-1 - (Corn$Actual_Transpiration_mm / Corn$Potential_Transpiration_mm)    ;

     
    # Adding the file name to the column File
     
     Corn$File<-i
     

    #  Sorting the data according to Harvest date

     Corn<-Corn[do.call(order,Corn),] ;
     
    # Write the results into the storing file

    SeasonOutput.summary<-rbind(SeasonOutput.summary,Corn)    ;   

    # Remove the data to create space in memory
     
     
     rm(SeasonOutput)
     
}

#  Remove objects to free memory space

rm(list=ls()[!ls() %in%  c("Excel.Columns","FileNames","SeasonOutput.summary") ])




###############################################################################################################
#                          Starting Daily outputs data query                                   
###############################################################################################################

# Column numbers from the daily output that are to be kept for data extraction


ColumnsDaily<-which(Excel.Columns %in% c("A","B","G","H","N","U","V","AT","AU","AX","AZ","BA","BC","BD","BG","BI","BJ"));



#  Reading Column headers 

DailyOutput.header<-readWorksheetFromFile(FileNames[1], sheet = "Daily Outputs", startRow = 3, endRow=7, header=F, keep=ColumnsDaily);


# Composing and saving the daily ouptput column names


DailyColNames<-c(paste(DailyOutput.header[2,],DailyOutput.header[3,],DailyOutput.header[4,],DailyOutput.header[5,],sep="_"))   ;


#  Create a storing file to accumulate the data of the files after processing

Daily.summary<-data.frame()  ;



for (j in FileNames) {

     # j=FileNames[1]
     #  Read data columns of in the daily output of the simulation ouput spreadsheet
     
     
     DailyOutput<-readWorksheetFromFile(j, sheet = "Daily Outputs", startRow= 8, header=F, keep=ColumnsDaily); 


     # Naming the data columns read


     names(DailyOutput)<-DailyColNames  ;


     DailyOutput$Year<-as.factor(format(DailyOutput$NA_NA_NA_Date,format="%Y"));

     
     #   Selecting Maize rows in the outputs

     Maize.Rows<-DailyOutput[DailyOutput$`NA_Rotation_Stage_Crop Name` == c("Maize"),];

     #   Selecting Mineral N at corn planting

     Maize.Planting<-Maize.Rows[Maize.Rows$NA_Crop_Growth_Stage == c("Planting"), ];

     Maize.Planting$MineralN<-Maize.Planting$"Profile_Soil_Nitrate_kg N/ha" + Maize.Planting$"Profile_Soil_Ammonium_kg N/ha";

     #   Calculating Net Mineralization in Corn

     `Maize.NMineralization_kg_ha`<-tapply(Maize.Rows$"Nitrogen_Net_Mineralization_kg N/ha",Maize.Rows$Year,sum);


     #   Nitrogen leaching during corn planting

    `Maize.N_Leaching_kg_ha`<-tapply(Maize.Rows$"NA_Nitrate_Leaching_kg N/ha",Maize.Rows$Year,sum);

    `Maize.NH4_N_Volatilization_kg_ha`<-tapply(Maize.Rows$"NA_Ammonia_Volatilization_kg N/ha",Maize.Rows$Year,sum);

    `Maize.N2O_N_Denitification_kg_ha`<-tapply(Maize.Rows$"Nitrous Oxide_from_Denitrification_kg N/ha",Maize.Rows$Year,sum);
     
    `Maize.N2O_Nitrification_kg_ha`<-tapply(Maize.Rows$"Nitrous Oxide_from_Nitrification_kg N/ha",Maize.Rows$Year,sum);

    `Maize.N_GaseousLosses_kg_ha`<-`Maize.NH4_N_Volatilization_kg_ha` + `Maize.NH4_N_Volatilization_kg_ha` + `Maize.N2O_Nitrification_kg_ha`;

     #  Soil Health During Corn

    `Maize.SoilOC.planting_Mg_ha`<-tapply(Maize.Rows$`Soil_Organic_Carbon_Mg/ha`,Maize.Rows$Year,max);
     
    `Maize.Residue.C_Resp_Mg_ha`<-tapply(Maize.Rows$"Residue_Respired_Carbon_Mg/ha",Maize.Rows$Year,sum);
     
    `Maize.SOM.C_Resp_Mg_ha`<-tapply(Maize.Rows$"SOM_Respired_Carbon_Mg/ha",Maize.Rows$Year,sum);
     
     # Grouping the data
    
     
     DailyOutput.Maize.1<-data.frame(`Maize.NMineralization_kg_ha` , `Maize.N_Leaching_kg_ha` , `Maize.NH4_N_Volatilization_kg_ha` , `Maize.N2O_N_Denitification_kg_ha`, `Maize.N2O_Nitrification_kg_ha` , `Maize.N_GaseousLosses_kg_ha` , `Maize.SoilOC.planting_Mg_ha`, `Maize.Residue.C_Resp_Mg_ha`, `Maize.SOM.C_Resp_Mg_ha`)   ;
     
     # Adding Year to the grouping ot merge the rest of the data
     
     DailyOutput.Maize.1$Year<-as.factor(row.names(DailyOutput.Maize.1))  ;   
     
     

     # Maize rows at the last day corn was grown in each year
     
     Maize.Rows.LastDay<-tapply(Maize.Rows$NA_NA_NA_Day,Maize.Rows$Year,max) ;
     
     Year<-as.factor(row.names(Maize.Rows.LastDay)) ;
     
     Maize.Data<-data.frame(Maize.Rows.LastDay, Year) ;
     
     names(Maize.Data)<-c("NA_NA_NA_Day","Year")   ;
     
     Maize.LastDay.year<-merge(Maize.Data, Maize.Rows) ;


     # Selectimg the Soil Organic carbon during the last day corn is growing in each year

     `Maize.SoilOC.Maturiy_Mg_ha`<-Maize.LastDay.year[,c("Year","Soil_Organic_Carbon_Mg/ha")]; 
     
     names(`Maize.SoilOC.Maturiy_Mg_ha`)<-c("Year","Maize.Soil_Organic_Carbon_Mg_ha")   ;
     
     # Merge Maize.SoilOC.Maturiy with the rest of the data
     
     DailyOutput.Maize<-merge(DailyOutput.Maize.1,`Maize.SoilOC.Maturiy_Mg_ha`, by="Year",all=T);
     

     
     #   Extracting Nitrogen available in  Red Clover and Alfalfa cover crops before Maize; 

     #   Selecting Red Clover rows in the outputs

     RedClover.Rows<-DailyOutput[DailyOutput$`NA_Rotation_Stage_Crop Name` == c("Red Clover"),];


     # Selecting the last day of the year that RecClover was grown 
     
     RedClover.LastDay<-tapply(RedClover.Rows$NA_NA_NA_Day,RedClover.Rows$Year,max) ;
     
     Year.RedClover<-as.factor(row.names(RedClover.LastDay))  ;
     
     RedClover.Data<-data.frame(RedClover.LastDay, Year.RedClover,row.names = NULL) ;
     
     names(RedClover.Data)<-c("NA_NA_NA_Day","Year")   ;
     
     #select the rows of RedClover with the last day of RedClover in each year
     
     RedClover.LastDay.year<-merge(RedClover.Data, RedClover.Rows) ;
     
     # Selecting the Total Crop N for the last day of RedClover before corn

     `RedClover.TotalCropN_kg/ha`<-RedClover.LastDay.year[,c("Year","NA_NA_NA_Day","Total_Crop_Nitrogen_kg N/ha")] ;
     
     names(`RedClover.TotalCropN_kg/ha`)<-c("Year","NA_NA_NA_Day","RedClover_Total_Crop_Nitrogen_kg_ha")  ;
     
     
      #   Selecting the maximum total crop N of red clover cover by year

     RedClover.TotalCropN.1<-tapply(RedClover.Rows$`Total_Crop_Nitrogen_kg N/ha`,RedClover.Rows$Year,max);
     
     
      #   Selecting Alfalfa rows in the outputs

     Alfalfa.Rows<-DailyOutput[DailyOutput$`NA_Rotation_Stage_Crop Name`== c("Alfalfa"),];


     #   Selecting the maximum total crop N of alfalfa by year 
     
     # Selecting the las day of the year that afalfa was grown 
     
     Alfalfa.LastDay<-tapply(Alfalfa.Rows$NA_NA_NA_Day,Alfalfa.Rows$Year,max) ;
     
     Year<-as.factor(row.names(Alfalfa.LastDay))  ;
     
     Alfalfa.Data<-data.frame(Alfalfa.LastDay, Year,row.names = NULL) ;
     
     names(Alfalfa.Data)<-c("NA_NA_NA_Day","Year")   ;
     

     #select the rows of alfalfa with the last day of alafalfa in each year
     
     Alfalfa.LastDay.year<-merge(Alfalfa.Data, Alfalfa.Rows) ;
     
     
     # Selecting the Total Crop N for the last day of alfalfa before corn

     `Alfalfa.TotalCropN_kg/ha`<-Alfalfa.LastDay.year[,c("Year","NA_NA_NA_Day","Total_Crop_Nitrogen_kg N/ha")] ;
     
     names(`Alfalfa.TotalCropN_kg/ha`)<-c("Year","NA_NA_NA_Day","Alfalfa_Total_Crop_Nitrogen_kg_ha")

     # Merge data from Alfalfa and RedClover
     
     Alfalfa.RedClover.summary<-merge(`Alfalfa.TotalCropN_kg/ha`,`RedClover.TotalCropN_kg/ha`, by="Year", all=T) ;
     
     # Merge data from Corn, Alfalfa and RedClover
     
     DailyOutput.Summary<-merge(DailyOutput.Maize,Alflafa.RedClover.summary, by="Year", all=T) ;
     
     
     #   Adding the file name to keep track

     Original.File<-j ;

     DailyOutput.Summary$File<-Original.File ;

     # Sumarizing the results:

         # Write the results into the storing file

    Daily.summary<-rbind(Daily.summary,DailyOutput.Summary)    ;   

    # Remove the data to create space in memory
     
     
     rm(DailyOutput) ;
     
}


#  Remove objects to free memory space

rm(list=ls()[!ls() %in%  c("Excel.Columns","FileNames","SeasonOutput.summary","Daily.summary") ])



###############################################################################################################
#                          Starting Annual Soil Profile and Annual Outputs data  query                                   
###############################################################################################################



#  Reading Column headers for the Annual Soil Profile Data 


AnnualSoilProfile.1<-readWorksheetFromFile(FileNames[1], sheet = "Annual Soil Profile", startRow = 3, endRow=3, header=F, simplify=T);

#  Creating Coulmn header for classes

AnnualSoilProfile.classes<-as.factor(AnnualSoilProfile.1[!is.na(AnnualSoilProfile.1)] )  ;


#  Create a storing file to accumulate the data of the files after processing

SoilProfile.summary<-data.frame()  ;


#  Reading Column headers for the Annual Soil Outputs Data


AnnualSoilOutputs.header<-readWorksheetFromFile(FileNames[1], sheet = "Annual Soil Outputs", startRow = 5, endRow=8, header=F);

# Composing and saving the daily ouptput column names


AnnualSoilOutputsNames<-c(paste(AnnualSoilOutputs.header[1,],AnnualSoilOutputs.header[2,],AnnualSoilOutputs.header[3,],AnnualSoilOutputs.header[4,],sep="_"))   ;


#  Create a storing file to accumulate the data of the files after processing

AnnualSoilOutput.summary<-data.frame()  ;



for (k in FileNames) {
     
     # k=FileNames[1]
     
     ###############################################################################################################
     #                                 Annual Soil Profile                                
     ###############################################################################################################

     #  Creating Coulmn header for layers

     AnnualSoilProfile.2<-readWorksheetFromFile(k, sheet = "Annual Soil Profile", startRow = 4, endRow=5, header=F);


     AnnualSoilProfile.Layers<-AnnualSoilProfile.2[,!is.na(AnnualSoilProfile.2[2,])] ;
     
     #Pasting the colum layers with the data classes to make the full column headers, excluding the "Year" coulm header, the first column in the data      set

     AnnualSoilProfile.Names<-paste(rep(AnnualSoilProfile.classes,each=(length(AnnualSoilProfile.Layers)-1)/length(levels(AnnualSoilProfile.classes))),AnnualSoilProfile.Layers[1,-1],AnnualSoilProfile.Layers[2,-1],sep="_")  ;

     # Reading the data from the spreadsheet

     AnnualSoilProfile.3<-readWorksheetFromFile(k, sheet = "Annual Soil Profile", startRow = 6, header=F);

     # Eliminting blank columns form the data set
     AnnualSoilProfile.Data<-AnnualSoilProfile.3[,!is.na(AnnualSoilProfile.3[1,])];
     
     #Adding the full column names to the data set

     names(AnnualSoilProfile.Data)<-c("Year",AnnualSoilProfile.Names);
     
     # Calculating the Humidified carbon per year on the top four layers
     
     AnnualSoilProfile.Data$HumidifiedC_25<-AnnualSoilProfile.Data[,c("Humified Carbon Mass_Layer 1_Mg C/ha")] + AnnualSoilProfile.Data[,c("Humified Carbon Mass_Layer 2_Mg C/ha")] + AnnualSoilProfile.Data[,c("Humified Carbon Mass_Layer 3_Mg C/ha")] + AnnualSoilProfile.Data[,c("Humified Carbon Mass_Layer 4_Mg C/ha")] ;
     
     
     # Calculating the Initial soil carbon carbon per year on the top four layers
     
     AnnualSoilProfile.Data$InitialC_25<-AnnualSoilProfile.Data[,c("Initial Carbon Mass_Layer 1_Mg C/ha")] + AnnualSoilProfile.Data[,c("Initial Carbon Mass_Layer 2_Mg C/ha")] + AnnualSoilProfile.Data[,c("Initial Carbon Mass_Layer 3_Mg C/ha")] + AnnualSoilProfile.Data[,c("Initial Carbon Mass_Layer 4_Mg C/ha")] ;
     
     
     #  Calculating percent humidification per year in th top 4 layers
     
     AnnualSoilProfile.Data$HumidifiedC_25_percent<-AnnualSoilProfile.Data$HumidifiedC_25/AnnualSoilProfile.Data$InitialC_25  ;
     
     #  Adding the File Name 
     
     Original.File<-k ;
     
     AnnualSoilProfile.Data$File<-Original.File   ;
     
     
     # Write the results into the storing file

      SoilProfile.summary<-rbind(SoilProfile.summary,AnnualSoilProfile.Data)  ;   

     
     
     ###############################################################################################################
     #                                 Annual Soil ouputs                              
     ###############################################################################################################
     
     # Reading the data from the spreadsheet and adding column names
     
     AnnualSoilOutputs<-readWorksheetFromFile(k, sheet = "Annual Soil Outputs", startRow = 9, header=F);
     

     names(AnnualSoilOutputs)<-AnnualSoilOutputsNames ;
     
     
     # Selecting the rows that do not have NA data in the Layer Thickness column
     
     AnnualSoilOutputs.1<-AnnualSoilOutputs[! is.na(AnnualSoilOutputs$NA_Layer_Thickness_m ), ]    ;
     
     # Selecting the Layer number and transforming it into a factor
     
     AnnualSoilOutputs.Layers<-as.factor(AnnualSoilOutputs$`NA_NA_Year &_Layer #`[! is.na(AnnualSoilOutputs$NA_Layer_Thickness_m ) ])  ;
     
     AnnualSoilOutputs.1$Layer<-AnnualSoilOutputs.Layers  ;
     
     # Selecting the Years and making a Year by selecting rows that have NA in the column NA_Layer_Thickness_m c
     
     AnnualSoilOutputs.Years<-AnnualSoilOutputs$`NA_NA_Year &_Layer #`[is.na(AnnualSoilOutputs$NA_Layer_Thickness_m ) ]  ;
     
     # Add years as a factor
     
    
     AnnualSoilOutputs.1$Year<-as.factor(rep(AnnualSoilOutputs.Years, each=length(levels(AnnualSoilOutputs.Layers)))) ;
     
     # Computing the layer thickness for each layer
     
     Layer.Depth<-AnnualSoilOutputs.1[1:length(levels(AnnualSoilOutputs.Layers)),c("Layer","NA_Layer_Thickness_m")];
     
     Zeros<-matrix(0,length(levels(AnnualSoilOutputs.Layers)),length(levels(AnnualSoilOutputs.Layers)))   ; #creating a zero square matrix of dimensions equal to the number of layers
     
     Zeros[lower.tri(Zeros, diag=T)]<-1  ; # filling the lower trinagular Zeros matrix with 1, including the diagaonal
     
     # Computing the layer depth by mutiplying the Zeros Matrix with the layer thickness values
     
     LayerDepth<-Zeros %*% AnnualSoilOutputs.1[1:length(levels(AnnualSoilOutputs.Layers)),c("NA_Layer_Thickness_m")] ;
     
     
     #  Adding the File Name 
     
     Original.File<-k ;
     
     AnnualSoilProfile.Data$File<-Original.File   ;
     
     AnnualSoilOutputs<-cbind(AnnualSoilOutputs.1,LayerDepth,Original.File) ;
     

    # Write the results into the storing file

     AnnualSoilOutput.summary<-rbind(AnnualSoilOutput.summary,AnnualSoilOutputs)  ; 
     
   # Remove the data to create space in memory
     
     rm(AnnualSoilOutputs) ;
     
     rm(AnnualSoilProfile.Data)    ;


}

#  Remove objects to free memory space

rm(list=ls()[!ls() %in%  c("Excel.Columns","FileNames","SeasonOutput.summary","Daily.summary","AnnualSoilOutput.summary") ])



###############################################################################################################
#                          Organizing and producing the ouputs in an Excell Workbook                              
###############################################################################################################


# Writting the results to their correponding spreadsheets



writeWorksheetToFile("..\\OutputSummary\\CyclesOutputSummary.xlsx", SeasonOutput.summary, sheet="Season_Output")  ;

writeWorksheetToFile("..\\OutputSummary\\CyclesOutputSummary.xlsx", Daily.summary , sheet="Daily_Output")  ;

writeWorksheetToFile("..\\OutputSummary\\CyclesOutputSummary.xlsx", AnnualSoilOutput.summary, sheet="Soil_Output")  ;


# Converting outputs to pounds per acre and bushels per acre


# Based on the conversion tool in the Iowa State University Extension and Outreach Ag Decision Maker website
# http://www.extension.iastate.edu/agdm/wholefarm/html/c6-80.html
     
     Mg_ha_to_Bushels_ac=1000*2.205/(56*2.471)
     Mg_ha_to_lb_ac=1000*2.205/2.471
     kg_ha_to_lb_ac=2.205/2.471

     
###########################     
# Season Output summary
     
     
# Selecting the columns that need to be converted to pounds per acre 
     
Col.LbAc<-grepl("Mg/ha",names(SeasonOutput.summary)) & ! grepl("Grain_Yield",names(SeasonOutput.summary)); 

# Converting the data from Mg/ha to lb,Ac

SeasonOutput.summary.LbAc<-SeasonOutput.summary[,Col.LbAc]*Mg_ha_to_lb_ac ;

# Changing the names of the columns

names(SeasonOutput.summary.LbAc)<-sub("Mg/ha","lb\/\Ac",names(SeasonOutput.summary.LbAc))  ;
     

     
# Selecting the columns that need to be converted to bushels per acre   

Col.BuAc<-grepl("Grain_Yield",names(SeasonOutput.summary))  ; 

# Converting the data from Mg/ha to Bu,Ac

SeasonOutput.summary.BuAc<-data.frame(SeasonOutput.summary[,Col.BuAc]*Mg_ha_to_Bushels_ac)  ; 

# Changing the names of the columns

names(SeasonOutput.summary.BuAc)<-"Grain_Yield_Bushels/Ac" ;


# Putting all the columns back together


SeasonOutput.summary.LbBuAc<-data.frame(SeasonOutput.summary[,!(Col.LbAc | Col.BuAc)],SeasonOutput.summary.BuAc,SeasonOutput.summary.LbAc);

# Writing the  data to the spreadsheet

writeWorksheetToFile("..\\OutputSummary\\CyclesOutputSummary.xlsx", SeasonOutput.summary.LbBuAc, sheet="Season_OutputLbBuAc")  ;

#######################

#  Daily.summary


# Selecting the columns that need to be converted from Mg/ha to pounds per acre 
     
ColMgha.LbAc<-grepl("Mg_ha",names(Daily.summary)) ; 

# Converting the data from Mg/ha to lb,Ac

Daily.summary.MghaLbAc<-data.frame(Daily.summary[,ColMgha.LbAc]*Mg_ha_to_lb_ac);

# Changing the names of the columns

names(Daily.summary.MghaLbAc)<-sub("Mg_ha","lb_Ac",names(Daily.summary.LbAc))  ;



# Selecting the columns that need to be converted from Kg/ha to pounds per acre 
     
Colkgha.LbAc<-grepl("kg_ha",names(Daily.summary)) ;

# Converting the data from Kg/ha to lb,Ac

Daily.summary.kghaLbAc<-data.frame(Daily.summary[,Colkgha.LbAc]*kg_ha_to_lb_ac);

# Changing the names of the columns

names(Daily.summary.kghaLbAc)<-sub("kg_ha","lb_Ac",names(Daily.summary.kghaLbAc))  ;

# Putting all the columns back together

Daily.summary.LbBuAc<-data.frame(Daily.summary[,!(ColMgha.LbAc | Colkgha.LbAc)],Daily.summary.MghaLbAc,Daily.summary.kghaLbAc);
    
     
