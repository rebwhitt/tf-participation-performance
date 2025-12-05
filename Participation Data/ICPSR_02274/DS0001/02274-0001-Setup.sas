/*              SAS DATA DEFINITION STATEMENTS FOR ICPSR 2274           
             COMMON CORE OF DATA: LOCAL EDUCATION AGENCIES (LEA)        
                      NONFISCAL DATA, 1983-1984                         
                          1ST ICPSR VERSION                             
                             AUGUST, 1999                               
                                                                        
 DATA:  begins a SAS data step and names an output SAS data set.        
                                                                        
 INFILE:  identifies the input file to be read with the input statement.
 Users must replace the "physical-filename" with host computer specific 
 input file specifications.                                             
                                                                        
 INPUT:  assigns the name, type, decimal specification (if any), and    
 specifies the beginning and ending column locations for each variable  
 in the data file.                                                      
                                                                        
 These data definition statements have been tested for compatability    
 with SAS Release 6.11 for UNIX and/or SAS Release 6.11 for Windows. */ 
                                                                        
data;                                                                   
infile "physical-filename" lrecl=193 missover pad;                      
input                                                                   
  LEAID    $ 001-007                                                    
  NAME     $ 008-037                                                    
  A01        038-049                                                    
  B01        050-061                                                    
  B02        062-073                                                    
  B03        074-085                                                    
  B04        086-097                                                    
  C01        098-109                                                    
  C02        110-121                                                    
  C03        122-133                                                    
  C04        134-145                                                    
  C05        146-157                                                    
  D01        158-169                                                    
  D02        170-181                                                    
  IA01       182-182                                                    
  IB01       183-183                                                    
  IB02       184-184                                                    
  IB03       185-185                                                    
  IB04       186-186                                                    
  IC01       187-187                                                    
  IC02       188-188                                                    
  IC03       189-189                                                    
  IC04       190-190                                                    
  IC05       191-191                                                    
  ID01       192-192                                                    
  ID02       193-193                                                    
 ;                                                                      
