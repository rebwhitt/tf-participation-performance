/*              SAS DATA DEFINITION STATEMENTS FOR ICPSR 2244              
       COMMON CORE OF DATA: PUBLIC SCHOOL UNIVERSE DATA, 1978-1979         
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
infile "physical-filename" lrecl=188 missover pad;                         
input                                                                      
  STATECD     001-002                                                      
  DISTRNO     003-007                                                      
  SCHNO78     008-012                                                      
  STID78    $ 013-032                                                      
  SYSNAM78  $ 033-062                                                      
  SCHID78   $ 063-082                                                      
  SCHNAM78  $ 083-112                                                      
  FILLER    $ 113-113                                                      
  STREET78  $ 114-138                                                      
  FILLER2   $ 139-139                                                      
  CITY78    $ 140-152                                                      
  FILLER3   $ 153-154                                                      
  ST78      $ 155-156                                                      
  FILLER4   $ 157-159                                                      
  ZIP78     $ 160-164                                                      
  FILLER5   $ 165-168                                                      
  TYPE78      169-169                                                      
  GSLO78    $ 170-171                                                      
  GSHI78    $ 172-173                                                      
  FILLER6   $ 174-174                                                      
  FTE78       175-178 .1                                                   
  FILLER7   $ 179-179                                                      
  MEMBER78    180-183                                                      
  STATUS78  $ 184-184                                                      
  SCTYPE78    185-185                                                      
  FILLER8   $ 186-188                                                      
 ;                                                                         
                                                                           
LABEL                                                                      
  STATECD  = "FIPS STATE CODE FOR LOCATION OF SCHOOL"                      
  DISTRNO  = "NCES DISTRICT NUMBER"                                        
  SCHNO78  = "UNIQUE SCHOOL ID (NCES ASSIGNED)"                            
  STID78   = "STATE SYSTEM ID"                                             
  SYSNAM78 = "SYSTEM NAME"                                                 
  SCHID78  = "STATE SCHOOL ID"                                             
  SCHNAM78 = "SCHOOL NAME"                                                 
  FILLER   = "BLANK"                                                       
  STREET78 = "MAILING ADDRESS OF SCHOOL"                                   
  FILLER2  = "BLANK"                                                       
  CITY78   = "CITY NAME"                                                   
  FILLER3  = "BLANK"                                                       
  ST78     = "STATE ABBREVIATION (POSTAL SERVICE)"                         
  FILLER4  = "BLANK"                                                       
  ZIP78    = "5-DIGIT ZIP CODE"                                            
  FILLER5  = "BLANK"                                                       
  TYPE78   = "SCHOOL TYPE CODE"                                            
  GSLO78   = "SCHOOL LOW GRADE (FROM GRADE ENROLLMENT)"                    
  GSHI78   = "SCHOOL HIGH GRADE (FROM GR. ENROLLMENT)"                     
  FILLER6  = "BLANK"                                                       
  FTE78    = "CLASSROOM TEACHERS (FULL-TIME EQUIV)"                        
  FILLER7  = "BLANK"                                                       
  MEMBER78 = "NUMBER OF STUDENTS ATTENDING"                                
  STATUS78 = "SCHOOL STATUS"                                               
  SCTYPE78 = "REPORTED TYPE"                                               
  FILLER8  = "BLANK"                                                       
 ;                                                                         
