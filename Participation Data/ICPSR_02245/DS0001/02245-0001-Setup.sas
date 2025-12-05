/*              SAS DATA DEFINITION STATEMENTS FOR ICPSR 2245              
       COMMON CORE OF DATA: PUBLIC SCHOOL UNIVERSE DATA, 1980-1981         
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
infile "physical-filename" lrecl=183 missover pad;                         
input                                                                      
  STATECD     001-002                                                      
  DISTRNO     003-007                                                      
  SCHNO80     008-012                                                      
  STID80    $ 013-032                                                      
  SYSNAM80  $ 033-062                                                      
  SCHID80   $ 063-082                                                      
  SCHNAM80  $ 083-112                                                      
  STREET80  $ 113-138                                                      
  CITY80    $ 139-152                                                      
  FILLER    $ 153-154                                                      
  ST80      $ 155-156                                                      
  FILLER2   $ 157-159                                                      
  ZIP80       160-164                                                      
  FILLER3   $ 165-168                                                      
  TYPE80      169-169                                                      
  GSLO80    $ 170-171                                                      
  GSHI80    $ 172-173                                                      
  FILLER4   $ 174-174                                                      
  FTE80       175-179                                                      
  MEMBER80    180-183                                                      
 ;                                                                         
                                                                           
LABEL                                                                      
  STATECD  = "FIPS STATE CODE FOR LOCATION OF SCHOOL"                      
  DISTRNO  = "NCES DISTRICT NUMBER"                                        
  SCHNO80  = "UNIQUE SCHOOL ID (NCES ASSIGNED)"                            
  STID80   = "STATE SYSTEM ID"                                             
  SYSNAM80 = "SYSTEM NAME"                                                 
  SCHID80  = "STATE SCHOOL ID"                                             
  SCHNAM80 = "SCHOOL NAME"                                                 
  STREET80 = "MAILING ADDRESS OF SCHOOL"                                   
  CITY80   = "CITY NAME"                                                   
  FILLER   = "BLANK"                                                       
  ST80     = "STATE ABBREVIATION (POSTAL SERVICE)"                         
  FILLER2  = "BLANK"                                                       
  ZIP80    = "5-DIGIT ZIP CODE"                                            
  FILLER3  = "BLANK"                                                       
  TYPE80   = "SCHOOL TYPE CODE"                                            
  GSLO80   = "SCHOOL LOW GRADE (FROM GRADE ENROLLMENT)"                    
  GSHI80   = "SCHOOL HIGH GRADE (FROM GR. ENROLLMENT)"                     
  FILLER4  = "BLANK"                                                       
  FTE80    = "CLASSROOM TEACHERS (FULL-TIME EQUIV)"                        
  MEMBER80 = "NUMBER OF STUDENTS ATTENDING"                                
 ;                                                                         
