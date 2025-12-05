/*              SAS DATA DEFINITION STATEMENTS FOR ICPSR 2248                 
       COMMON CORE OF DATA: PUBLIC SCHOOL UNIVERSE DATA, 1984-1985            
                          1ST ICPSR VERSION                                   
                            OCTOBER, 1999                                     
                                                                              
 DATA:  begins a SAS data step and names an output SAS data set.              
                                                                              
 INFILE:  identifies the input file to be read with the input statement.      
 Users must replace the "physical-filename" with host computer specific       
 input file specifications.                                                   
                                                                              
 INPUT:  assigns the name, type, decimal specification (if any), and          
 specifies the beginning and ending column locations for each variable        
 in the data file.                                                            
                                                                              
 LABEL:  assigns descriptive labels to all variables. Variable labels         
 and variable names may be identical for some variables.                      
                                                                              
 These data definition statements have been tested for compatability          
 with SAS Release 6.11 for UNIX and/or SAS Release 6.11 for Windows. */       
                                                                              
data;                                                                         
infile "physical-filename" lrecl=175 missover pad;                            
input                                                                         
  STATECD     001-002                                                         
  DISTRNO     003-007                                                         
  SCHNO84     008-012                                                         
  STID84    $ 013-026                                                         
  SCHID84   $ 027-046                                                         
  SCHNAM84  $ 047-076                                                         
  STREET84  $ 077-101                                                         
  CITY84    $ 102-114                                                         
  ST84      $ 115-116                                                         
  ZIP84     $ 117-121                                                         
  FILLER    $ 122-125                                                         
  TYPE84      126-126                                                         
  OPCODE84    127-127                                                         
  GSLO84    $ 128-129                                                         
  GSHI84    $ 130-131                                                         
  FTE84       132-136                                                         
  MEMBER84    137-140                                                         
  STATUS84    141-141                                                         
  SYNAME84  $ 142-171                                                         
  OEST84      172-173                                                         
  IMPTCH84    174-174                                                         
  IMPSTD84    175-175                                                         
 ;                                                                            
                                                                              
LABEL                                                                         
  STATECD  = "FIPS STATE CODE FOR LOCATION OF SCHOOL"                         
  DISTRNO  = "NCES DISTRICT NUMBER"                                           
  SCHNO84  = "UNIQUE SCHOOL ID (NCES ASSIGNED)"                               
  STID84   = "STATE SYSTEM ID"                                                
  SCHID84  = "STATE SCHOOL ID"                                                
  SCHNAM84 = "SCHOOL NAME"                                                    
  STREET84 = "MAILING ADDRESS OF SCHOOL"                                      
  CITY84   = "CITY NAME"                                                      
  ST84     = "STATE ABBREVIATION (POSTAL SERVICE)"                            
  ZIP84    = "5-DIGIT ZIP CODE"                                               
  FILLER   = "BLANK"                                                          
  TYPE84   = "SCHOOL TYPE CODE"                                               
  OPCODE84 = "NCES OPERATION CODE"                                            
  GSLO84   = "SCHOOL LOW GRADE (FROM GRADE ENROLLMENT)"                       
  GSHI84   = "SCHOOL HIGH GRADE (FROM GR. ENROLLMENT)"                        
  FTE84    = "CLASSROOM TEACHERS (FULL-TIME EQUIV)"                           
  MEMBER84 = "NUMBER OF STUDENTS ATTENDING"                                   
  STATUS84 = "NCES CODE FOR STATUS OF SCHOOL"                                 
  SYNAME84 = "NAME OF SCHOOL DISTRICT"                                        
  OEST84   = "OE STATE CODE"                                                  
  IMPTCH84 = "IMPUTATION FLAG TEACHERS"                                       
  IMPSTD84 = "IMPUTATION FLAG MEMBERSHIP"                                     
 ;                                                                            
