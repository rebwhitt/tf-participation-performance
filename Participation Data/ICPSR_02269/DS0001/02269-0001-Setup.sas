/*              SAS DATA DEFINITION STATEMENTS FOR ICPSR 2269           
             COMMON CORE OF DATA: LOCAL EDUCATION AGENCIES (LEA)        
                      NONFISCAL DATA, 1981-1982                         
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
infile "physical-filename" lrecl=540 missover pad;                      
input                                                                   
  NCESID       1 -  7                                                   
  NAME     $   8 - 37                                                   
  ADDRESS  $  38 - 62                                                   
  CITY     $  63 - 75                                                   
  STABBR   $  76 - 77                                                   
  ZIP         78 - 82                                                   
  CONAME   $  83 - 98                                                   
  FIPST       99 -100                                                   
  FIPCO      101 -103                                                   
  GSLO     $ 104 -105                                                   
  GSHI81   $ 111 -112                                                   
  OPCODE81   108 -108                                                   
  SUPERID    109 -120 .2                                                
  ADMOTH     121 -132 .2                                                
  ADMTOT     133 -144 .2                                                
  B1C        145 -156 .2                                                
  B2C        157 -168 .2                                                
  B3C        169 -180 .2                                                
  B4C        181 -192 .2                                                
  C1C        193 -204 .2                                                
  C2C        205 -216 .2                                                
  C3C        217 -228 .2                                                
  C4C        229 -240 .2                                                
  DC         241 -252 .2                                                
  EC         253 -264 .2                                                
  FC         265 -276 .2                                                
  GC         277 -288 .2                                                
  H1C        289 -300 .2                                                
  H2C        301 -312 .2                                                
  H3C        313 -324 .2                                                
  H4C        325 -336 .2                                                
  H5C        337 -348 .2                                                
  H6C        349 -360 .2                                                
  IC         361 -372 .2                                                
  J1C        373 -384 .2                                                
  J2C        385 -396 .2                                                
  J3C        397 -408 .2                                                
  J4C        409 -420 .2                                                
  KC         421 -432 .2                                                
  TOTPROF    433 -444 .2                                                
  L1C        445 -456 .2                                                
  L2C        457 -468 .2                                                
  L3C        469 -480 .2                                                
  ELEM       481 -492 .2                                                
  SEC        493 -504 .2                                                
  TOTENR     505 -516 .2                                                
  HSGRAD     517 -528 .2                                                
  SCNUM      529 -540 .2                                                
 ;                                                                      
                                                                        
