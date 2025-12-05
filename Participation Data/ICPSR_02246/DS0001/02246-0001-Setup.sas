/*              SAS DATA DEFINITION STATEMENTS FOR ICPSR 2246                 
       COMMON CORE OF DATA: PUBLIC SCHOOL UNIVERSE DATA, 1982-1983            
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
  SCHNO82     008-012                                                         
  STID82    $ 013-026                                                         
  SCHID82   $ 027-046                                                         
  SCHNAM82  $ 047-076                                                         
  STREET82  $ 077-101                                                         
  CITY82    $ 102-114                                                         
  ST82      $ 115-116                                                         
  ZIP82     $ 117-121                                                         
  FILLER    $ 122-125                                                         
  TYPE82      126-126                                                         
  OPCODE82    127-127                                                         
  GSLO82    $ 128-129                                                         
  GSHI82    $ 130-131                                                         
  FTE82       132-136                                                         
  MEMBER82    137-140                                                         
  STATUS82    141-141                                                         
  SYNAME82  $ 142-171                                                         
  OEST82      172-173                                                         
  IMPTCH82    174-174                                                         
  IMPSTD82    175-175                                                         
 ;                                                                            
                                                                              
LABEL                                                                         
  STATECD  = "FIPS STATE CODE FOR LOCATION OF SCHOOL"                         
  DISTRNO  = "NCES DISTRICT NUMBER"                                           
  SCHNO82  = "UNIQUE SCHOOL ID (NCES ASSIGNED)"                               
  STID82   = "STATE SYSTEM ID"                                                
  SCHID82  = "STATE SCHOOL ID"                                                
  SCHNAM82 = "SCHOOL NAME"                                                    
  STREET82 = "MAILING ADDRESS OF SCHOOL"                                      
  CITY82   = "CITY NAME"                                                      
  ST82     = "STATE ABBREVIATION (POSTAL SERVICE)"                            
  ZIP82    = "5-DIGIT ZIP CODE"                                               
  FILLER   = "BLANK"                                                          
  TYPE82   = "SCHOOL TYPE CODE"                                               
  OPCODE82 = "NCES OPERATION CODE"                                            
  GSLO82   = "SCHOOL LOW GRADE (FROM GRADE ENROLLMENT)"                       
  GSHI82   = "SCHOOL HIGH GRADE (FROM GR. ENROLLMENT)"                        
  FTE82    = "CLASSROOM TEACHERS (FULL-TIME EQUIV)"                           
  MEMBER82 = "NUMBER OF STUDENTS ATTENDING"                                   
  STATUS82 = "NCES CODE FOR STATUS OF SCHOOL"                                 
  SYNAME82 = "NAME OF SCHOOL DISTRICT"                                        
  OEST82   = "OE STATE CODE"                                                  
  IMPTCH82 = "IMPUTATION FLAG TEACHERS"                                       
  IMPSTD82 = "IMPUTATION FLAG MEMBERSHIP"                                     
 ;                                                                            
