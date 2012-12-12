unit UPalabrasRes;

interface

uses Classes;

const
  ctnPalRes = 625;
  ArrPalRes: array[0..ctnPalRes-1] of string = (
    'ABSTRACT',
    'ACCEPT',
    'ACCESS',
    'ACQUIRE',
    'ACTUAL',
    'ADD',
    'ADDRESS',
    'ADVANCING',
    'AFTER',
    'ALL',
    'ALLOW',
    'ALPHABET',
    'ALPHABETIC',
    'ALPHABETIC-LOWER',
    'ALPHABETIC-UPPER',
    'ALPHANUMERIC',
    'ALPHANUMERIC-EDITED',
    'ALSO',
    'ALTER',
    'ALTERNATE',
    'AND',
    'ANY',
    'APPLY',
    'ARE',
    'AREA',
    'AREAS',
    'AS',
    'ASCENDING',
    'ASSIGN',
    'AT',
    'AUTHOR',
    'AUTO',
    'AUTO-HYPHEN-SKIP',
    'AUTOMATIC',
    'AUTO-SKIP',
    'BACKGROUND-COLOUR',
    'BACKWARD',
    'B-AND',
    'BASIS',                    
    'BEEP',                     
    'BEFORE',
    'BEGINNING',                
    'BELL',                     
    'B-EXOR',                   
    'BINARY',
    'BLANK',
    'B-LEFT',                   
    'BLINK',                    
    'BLOCK',                    
    'B-NOT',                    
    'B-OR',                     
    'BOTTOM',                   
    'B-RIGHT',                  
    'BROWSING',                 
    'B-XOR',                    
    'BY',                       
    'C01',                      
    'C02',                      
    'C03',                      
    'C04',                      
    'C05',                      
    'C06',                      
    'C07',                      
    'C08',                      
    'C09',                      
    'C10',                      
    'C11',                      
    'C12',                      
    'CALL',                     
    'CANCEL',                   
    'CBL',                      
    'CD',                       
    'CF',                       
    'CH',                       
    'CHAIN',                    
    'CHAINING',                 
    'CHANGED',                  
    'CHARACTER',                
    'CHARACTERS',               
    'CLASS',                    
    'CLASS-CONTROL',            
    'CLASS-ID',
    'CLASS-OBJECT',             
    'CLOCK-UNITS',              
    'CLOSE',                    
    'COBOL',
    'CODE',                     
    'CODE-SET',                 
    'COERCION',                 
    'COL',
    'COLLATING',
    'COLUMN',                   
    'COMMA',                    
    'COMMIT',                   
    'COMMON',                   
    'COMMUNICATION',            
    'COMP',                     
    'COMP-0',                   
    'COMP-1',                   
    'COMP-2',                   
    'COMP-3',                   
    'COMP-4',                   
    'COMP-5',                   
    'COMP-6',                   
    'COMPUTATIONAL',            
    'COMPUTATIONAL-0',          
    'COMPUTATIONAL-1',          
    'COMPUTATIONAL-2',          
    'COMPUTATIONAL-3',          
    'COMPUTATIONAL-4',          
    'COMPUTATIONAL-5',          
    'COMPUTATIONAL-6',          
    'COMPUTATIONAL-X',          
    'COMPUTE',                  
    'COMP-X',                   
    'COM-REG',                  
    'CONFIGURATION',            
    'CONSOLE',                  
    'CONTAINS',                 
    'CONTENT',                  
    'CONTINUE',                 
    'CONTROL',                  
    'CONTROL-AREA',             
    'CONTROLS',                 
    'CONVERT',                  
    'CONVERTING',               
    'COPY',
    'CORE-INDEX',               
    'CORR',                     
    'CORRESPONDING',            
    'COUNT',
    'CRT',                      
    'CRT-UNDER',                
    'CSP',                      
    'CURRENCY',
    'CURRENT-DATE',
    'CURSOR',                   
    'CYCLE',                    
    'CYL-INDEX',                
    'CYL-OVERFLOW',             
    'DATA',                     
    'DATE',                     
    'DATE-COMPILED',            
    'DATE-WRITTEN',             
    'DAY',                      
    'DAY-OF-WEEK',              
    'DBCS',                     
    'DE',                       
    'DEBUG',                    
    'DEBUG-CONTENTS',           
    'DEBUGGING',                
    'DEBUG-ITEM',               
    'DEBUG-LINE',               
    'DEBUG-NAME',               
    'DEBUG-SUB-1',              
    'DEBUG-SUB-2',              
    'DEBUG-SUB-3',              
    'DECIMAL-POINT',            
    'DECLARATIVES',             
    'DELETE',                   
    'DELIMITED',                
    'DELIMITER',                
    'DEPENDING',                
    'DESCENDING',               
    'DESTINATION',              
    'DETAIL',                   
    'DISABLE',                  
    'DISK',                     
    'DISP',                     
    'DISPLAY',                  
    'DISPLAY-1',                
    'DISPLAY-ST',
    'DIVIDE',                   
    'DIVISION',                 
    'DOWN',                     
    'DROP',
    'DUPLICATES',               
    'DYNAMIC',                  
    'ECHO',                     
    'EGCS',
    'EGI',
    'EJECT',                    
    'ELSE',                     
    'EMI',                      
    'EMPTY-CHECK',              
    'ENABLE',                   
    'END',                      
    'END-ACCEPT',               
    'END-ADD',                  
    'END-CALL',                 
    'END-CHAIN',                
    'END-COMPUTE',              
    'END-DELETE',               
    'END-DISPLAY',              
    'END-DIVIDE',               
    'END-EVALUATE',             
    'END-IF',                   
    'ENDING',                   
    'END-INVOKE',               
    'END-MULTIPLY',             
    'END-OF-PAGE',              
    'END-PERFORM',              
    'END-READ',                 
    'END-RECEIVE',              
    'END-RETURN',               
    'END-REWRITE',              
    'END-SEARCH',               
    'END-START',                
    'END-STRING',               
    'END-SUBTRACT',             
    'END-UNSTRING',             
    'END-WRITE',                
    'ENTER',                    
    'ENTRY',                    
    'ENVIRONMENT',              
    'EOL',                      
    'EOP',
    'EOS',                      
    'EQUAL',                    
    'EQUALS',                   
    'ERASE',
    'ERROR',                    
    'ESCAPE',                   
    'ESI',                      
    'EVALUATE',
    'EVENT-POINTER',
    'EVERY',                    
    'EXAMINE',                  
    'EXCEEDS',                  
    'EXCEPTION',                
    'EXCESS-3',                 
    'EXCLUSIVE',                
    'EXEC',                     
    'EXECUTE',                  
    'EXHIBIT',                  
    'EXIT',                     
    'EXTEND',                   
    'EXTENDED-SEARCH',          
    'EXTERNAL',                 
    'EXTERNALLY-DESCRIBED-KEY', 
    'FACTORY',                  
    'FALSE',                    
    'FD',                       
    'FILE',                     
    'FILE-CONTROL',             
    'FILE-ID',                  
    'FILE-LIMIT',               
    'FILE-LIMITS',              
    'FILLER',                   
    'FINAL',                    
    'FIRST',                    
    'FIXED',                    
    'FOOTING',                  
    'FOR',                      
    'FOREGROUND-COLOR',         
    'FOREGROUND-COLOUR',        
    'FORMAT',                   
    'FROM',                     
    'FULL',                     
    'FUNCTION',                 
    'GENERATE',                 
    'GIVING',
    'GLOBAL',                   
    'GO',                       
    'GOBACK',                   
    'GREATER',
    'GRID',                     
    'GROUP',                    
    'HEADING',                  
    'HIGH',
    'HIGHLIGHT',
    'HIGH-VALUE',               
    'HIGH-VALUES',              
    'ID',                       
    'IDENTIFICATION',           
    'IDENTIFIED',               
    'IF',                       
    'IGNORE',                   
    'IN',                       
    'INDEX',                    
    'INDEXED',                  
    'INDIC',                    
    'INDICATE',                 
    'INDICATOR',                
    'INDICATORS',               
    'INHERITING',               
    'INHERITS',                 
    'INITIAL',                  
    'INITIALIZE',               
    'INITIATE',                 
    'INPUT',                    
    'INPUT-OUTPUT',             
    'INSERT',                   
    'INSPECT',                  
    'INSTALLATION',             
    'INTO',                     
    'INVALID',                  
    'INVOKE',                   
    'INVOKED',                  
    'I-O',                      
    'I-O-CONTROL',              
    'JAPANESE',                 
    'JUST',                     
    'JUSTIFIED',                
    'KANJI',                    
    'KEPT',                     
    'KEY',
    'KEYBOARD',                 
    'LABEL',                    
    'LAST',                     
    'LEADING',
    'LEAVE',                    
    'LEFT',                     
    'LEFT-JUSTIFY',             
    'LEFTLINE',
    'LENGTH',
    'LENGTH-CHECK',             
    'LESS',                     
    'LIMIT',                    
    'LIMITS',                   
    'LIN',                      
    'LINAGE',                   
    'LINAGE-COUNTER',           
    'LINE',                     
    'LINE-COUNTER',             
    'LINES',                    
    'LINKAGE',                  
    'LOCAL-STORAGE',            
    'LOCK',                     
    'LOCKING',                  
    'LOW',                      
    'LOWER',                    
    'LOWLIGHT',                 
    'LOW-VALUE',                
    'LOW-VALUES',               
    'MANUAL',                   
    'MASTER-INDEX',             
    'MEMORY',                   
    'MERGE',                    
    'MESSAGE',                  
    'METHOD',                   
    'METHOD-ID',                
    'MODE',                     
    'MODIFIED',                 
    'MODULES',                  
    'MONITOR-POINTER',          
    'MORE-LABELS',              
    'MOVE',                     
    'MULTIPLE',                 
    'MULTIPLY',                 
    'MUTEX-POINTER',            
    'NAME',
    'NAMED',                    
    'NATIONAL',                 
    'NATIONAL-EDITED',          
    'NATIVE',
    'NCHAR',                    
    'NEGATIVE',                 
    'NEXT',                     
    'NO',
    'NO-ECHO',
    'NOMINAL',                  
    'NOT',                      
    'NOTE',                     
    'NSTD-REELS',               
    'NULL',                     
    'NULLS',                    
    'NUMBER',                   
    'NUMERIC',                  
    'NUMERIC-EDITED',           
    'OBJECT',                   
    'OBJECT-COMPUTER',          
    'OBJECT-ID',                
    'OBJECT-STORAGE',           
    'OCCURS',                   
    'OF',                       
    'OFF',                      
    'O-FILL',                   
    'OMITTED',                  
    'ON',                       
    'OOSTACKPTR',               
    'OPEN',                     
    'OPTIONAL',                 
    'OR',                       
    'ORDER',                    
    'ORGANIZATION',             
    'OTHER',                    
    'OTHERWISE',                
    'OUTPUT',                   
    'OVERFLOW',                 
    'OVERLINE',                 
    'PACKED-DECIMAL',           
    'PADDING',                  
    'PAGE',                     
    'PAGE-COUNTER',             
    'PARAGRAPH',                
    'PASSWORD',
    'PERFORM',                  
    'PF',                       
    'PH',                       
    'PIC',
    'PICTURE',                  
    'PLUS',                     
    'POINTER',                  
    'POS',
    'POSITION',
    'POSITIONING',              
    'POSITIVE',                 
    'PREVIOUS',                 
    'PRINT',                    
    'PRINTER',                  
    'PRINTER-1',                
    'PRINTING',                 
    'PRINT-SWITCH',             
    'PRIVATE',                  
    'PROCEDURE',                
    'PROCEDURE-POINTER',        
    'PROCEDURES',               
    'PROCEED',                  
    'PROCESSING',               
    'PROGRAM',                  
    'PROGRAM-ID',               
    'PROMPT',                   
    'PROTECTED',                
    'PUBLIC',                   
    'PURGE',                    
    'QUEUE',                    
    'QUOTE',                    
    'QUOTES',                   
    'RANDOM',                   
    'RANGE',                    
    'RD',                       
    'READ',                     
    'READING',                  
    'READY',                    
    'RECEIVE',                  
    'RECORD',                   
    'RECORDING',                
    'RECORD-OVERFLOW',          
    'RECORDS',                  
    'REDEFINES',                
    'REEL',
    'REFERENCE',                
    'REFERENCES',               
    'RELATIVE',                 
    'RELEASE',
    'RELOAD',                   
    'REMAINDER',                
    'REMARKS',                  
    'REMOVAL',
    'RENAMES',
    'REORG-CRITERIA',           
    'REPEATED',                 
    'REPLACE',                  
    'REPLACING',                
    'REPORT',                   
    'REPORTING',                
    'REPORTS',                  
    'REQUIRED',                 
    'REREAD',                   
    'RERUN',                    
    'RESERVE',                  
    'RESET',                    
    'RESTRICTED',               
    'RETURN',                   
    'RETURN-CODE',              
    'RETURNING',                
    'REVERSE',                  
    'REVERSED',                 
    'REVERSE-VIDEO',            
    'REWIND',                   
    'REWRITE',                  
    'RF',                       
    'RH',                       
    'RIGHT',                    
    'RIGHT-JUSTIFY',            
    'ROLLBACK',                 
    'ROUNDED',                  
    'RUN',                      
    'S01',                      
    'S02',                      
    'S03',                      
    'S04',                      
    'S05',                      
    'SAME',                     
    'SCREEN',                   
    'SD',
    'SEARCH',                   
    'SECTION',                  
    'SECURE',                   
    'SECURITY',
    'SEEK',                     
    'SEGMENT',                  
    'SEGMENT-LIMIT',            
    'SELECT',
    'SELECTIVE',
    'SELF',                     
    'SELFCLASS',                
    'SEMAPHORE-POINTER',        
    'SEND',                     
    'SENTENCE',                 
    'SEPARATE',                 
    'SEQUENCE',                 
    'SEQUENTIAL',               
    'SERVICE',                  
    'SET',                      
    'SHIFT-IN',                 
    'SHIFT-OUT',                
    'SIGN',                     
    'SIZE',                     
    'SKIP1',                    
    'SKIP2',                    
    'SKIP3',                    
    'SORT',                     
    'SORT-CONTROL',             
    'SORT-CORE-SIZE',           
    'SORT-FILE-SIZE',           
    'SORT-MERGE',               
    'SORT-MESSAGE',             
    'SORT-MODE-SIZE',           
    'SORT-OPTION',              
    'SORT-RETURN',              
    'SOURCE',                   
    'SOURCE-COMPUTER',          
    'SPACE',                    
    'SPACE-FILL',               
    'SPACES',                   
    'SPECIAL-NAMES',            
    'STANDARD',                 
    'STANDARD-1',               
    'STANDARD-2',               
    'START',
    'STATUS',                   
    'STOP',                     
    'STORE',                    
    'STRING',
    'SUB-FILE',                 
    'SUB-QUEUE-1',              
    'SUB-QUEUE-2',              
    'SUB-QUEUE-3',
    'SUBTRACT',
    'SUM',                      
    'SUPER',                    
    'SUPPRESS',                 
    'SYMBOLIC',                 
    'SYNC',                     
    'SYNCHRONIZED',             
    'SYSIN',                    
    'SYSIPT',                   
    'SYSLST',                   
    'SYSOUT',                   
    'SYSPCH',                   
    'SYSPUNCH',                 
    'TALLYING',                 
    'TAPE',                     
    'TERMINAL',                 
    'TERMINATE',                
    'TEST',                     
    'TEXT',                     
    'THAN',                     
    'THEN',                     
    'THREAD-LOCAL',             
    'THREAD-POINTER',           
    'THROUGH',                  
    'THRU',                     
    'TIME',                     
    'TIME-OF-DAY',              
    'TIMEOUT',                  
    'TIME-OUT',                 
    'TIMES',                    
    'TITLE',                    
    'TO',                       
    'TOP',                      
    'TOTALED',                  
    'TOTALING',                 
    'TRACE',                    
    'TRACK-AREA',
    'TRACK-LIMIT',              
    'TRACKS',                   
    'TRAILING',                 
    'TRAILING-SIGN',
    'TRANSFORM',                
    'TRUE',                     
    'TYPE',                     
    'TYPEDEF',
    'UNDERLINE',
    'UNEQUAL',                  
    'UNIT',                     
    'UNLOCK',                   
    'UNSTRING',                 
    'UNTIL',                    
    'UP',                       
    'UPDATE',                   
    'UPON',                     
    'UPPER',                    
    'UPSI-0',                   
    'UPSI-1',                   
    'UPSI-2',                   
    'UPSI-3',                   
    'UPSI-4',                   
    'UPSI-5',                   
    'UPSI-6',                   
    'UPSI-7',                   
    'USAGE',                    
    'USE',                      
    'USER',                     
    'USING',                    
    'VALUE',                    
    'VALUES',                   
    'VARIABLE',                 
    'VARYING',                  
    'WAIT',                     
    'WHEN',                     
    'WHEN-COMPILED',            
    'WITH',                     
    'WORDS',                    
    'WORKING-STORAGE',          
    'WRITE',                    
    'WRITE-ONLY',               
    'WRITE-VERIFY',             
    'WRITING',                  
    'ZERO',
    'ZEROES',                   
    'ZERO-FILL',                
    'ZEROS');

type

TPalResId =(
  prABSTRACT                 ,
  prACCEPT                   ,
  prACCESS                   ,
  prACQUIRE                  ,
  prACTUAL                   ,
  prADD                      ,
  prADDRESS                  ,
  prADVANCING                ,
  prAFTER                    ,
  prALL                      ,
  prALLOW                    ,
  prALPHABET                 ,
  prALPHABETIC               ,
  prALPHABETIC_LOWER         ,
  prALPHABETIC_UPPER         ,
  prALPHANUMERIC             ,
  prALPHANUMERIC_EDITED      ,
  prALSO                     ,
  prALTER                    ,
  prALTERNATE                ,
  prAND                      ,
  prANY                      ,
  prAPPLY                    ,
  prARE                      ,
  prAREA                     ,
  prAREAS                    ,
  prAS                       ,
  prASCENDING                ,
  prASSIGN                   ,
  prAT                       ,
  prAUTHOR                   ,
  prAUTO                     ,
  prAUTO_HYPHEN_SKIP         ,
  prAUTOMATIC                ,
  prAUTO_SKIP                ,
  prBACKGROUND_COLOUR        ,
  prBACKWARD                 ,
  prB_AND                    ,
  prBASIS                    ,
  prBEEP                     ,
  prBEFORE                   ,
  prBEGINNING                ,
  prBELL                     ,
  prB_EXOR                   ,
  prBINARY                   ,
  prBLANK                    ,
  prB_LEFT                   ,
  prBLINK                    ,
  prBLOCK                    ,
  prB_NOT                    ,
  prB_OR                     ,
  prBOTTOM                   ,
  prB_RIGHT                  ,
  prBROWSING                 ,
  prB_XOR                    ,
  prBY                       ,
  prC01                      ,
  prC02                      ,
  prC03                      ,
  prC04                      ,
  prC05                      ,
  prC06                      ,
  prC07                      ,
  prC08                      ,
  prC09                      ,
  prC10                      ,
  prC11                      ,
  prC12                      ,
  prCALL                     ,
  prCANCEL                   ,
  prCBL                      ,
  prCD                       ,
  prCF                       ,
  prCH                       ,
  prCHAIN                    ,
  prCHAINING                 ,
  prCHANGED                  ,
  prCHARACTER                ,
  prCHARACTERS               ,
  prCLASS                    ,
  prCLASS_CONTROL            ,
  prCLASS_ID                 ,
  prCLASS_OBJECT             ,
  prCLOCK_UNITS              ,
  prCLOSE                    ,
  prCOBOL                    ,
  prCODE                     ,
  prCODE_SET                 ,
  prCOERCION                 ,
  prCOL                      ,
  prCOLLATING                ,
  prCOLUMN                   ,
  prCOMMA                    ,
  prCOMMIT                   ,
  prCOMMON                   ,
  prCOMMUNICATION            ,
  prCOMP                     ,
  prCOMP_0                   ,
  prCOMP_1                   ,
  prCOMP_2                   ,
  prCOMP_3                   ,
  prCOMP_4                   ,
  prCOMP_5                   ,
  prCOMP_6                   ,
  prCOMPUTATIONAL            ,
  prCOMPUTATIONAL_0          ,
  prCOMPUTATIONAL_1          ,
  prCOMPUTATIONAL_2          ,
  prCOMPUTATIONAL_3          ,
  prCOMPUTATIONAL_4          ,
  prCOMPUTATIONAL_5          ,
  prCOMPUTATIONAL_6          ,
  prCOMPUTATIONAL_X          ,
  prCOMPUTE                  ,
  prCOMP_X                   ,
  prCOM_REG                  ,
  prCONFIGURATION            ,
  prCONSOLE                  ,
  prCONTAINS                 ,
  prCONTENT                  ,
  prCONTINUE                 ,
  prCONTROL                  ,
  prCONTROL_AREA             ,
  prCONTROLS                 ,
  prCONVERT                  ,
  prCONVERTING               ,
  prCOPY                     ,
  prCORE_INDEX               ,
  prCORR                     ,
  prCORRESPONDING            ,
  prCOUNT                    ,
  prCRT                      ,
  prCRT_UNDER                ,
  prCSP                      ,
  prCURRENCY                 ,
  prCURRENT_DATE             ,
  prCURSOR                   ,
  prCYCLE                    ,
  prCYL_INDEX                ,
  prCYL_OVERFLOW             ,
  prDATA                     ,
  prDATE                     ,
  prDATE_COMPILED            ,
  prDATE_WRITTEN             ,
  prDAY                      ,
  prDAY_OF_WEEK              ,
  prDBCS                     ,
  prDE                       ,
  prDEBUG                    ,
  prDEBUG_CONTENTS           ,
  prDEBUGGING                ,
  prDEBUG_ITEM               ,
  prDEBUG_LINE               ,
  prDEBUG_NAME               ,
  prDEBUG_SUB_1              ,
  prDEBUG_SUB_2              ,
  prDEBUG_SUB_3              ,
  prDECIMAL_POINT            ,
  prDECLARATIVES             ,
  prDELETE                   ,
  prDELIMITED                ,
  prDELIMITER                ,
  prDEPENDING                ,
  prDESCENDING               ,
  prDESTINATION              ,
  prDETAIL                   ,
  prDISABLE                  ,
  prDISK                     ,
  prDISP                     ,
  prDISPLAY                  ,
  prDISPLAY_1                ,
  prDISPLAY_ST               ,
  prDIVIDE                   ,
  prDIVISION                 ,
  prDOWN                     ,
  prDROP                     ,
  prDUPLICATES               ,
  prDYNAMIC                  ,
  prECHO                     ,
  prEGCS                     ,
  prEGI                      ,
  prEJECT                    ,
  prELSE                     ,
  prEMI                      ,
  prEMPTY_CHECK              ,
  prENABLE                   ,
  prEND                      ,
  prEND_ACCEPT               ,
  prEND_ADD                  ,
  prEND_CALL                 ,
  prEND_CHAIN                ,
  prEND_COMPUTE              ,
  prEND_DELETE               ,
  prEND_DISPLAY              ,
  prEND_DIVIDE               ,
  prEND_EVALUATE             ,
  prEND_IF                   ,
  prENDING                   ,
  prEND_INVOKE               ,
  prEND_MULTIPLY             ,
  prEND_OF_PAGE              ,
  prEND_PERFORM              ,
  prEND_READ                 ,
  prEND_RECEIVE              ,
  prEND_RETURN               ,
  prEND_REWRITE              ,
  prEND_SEARCH               ,
  prEND_START                ,
  prEND_STRING               ,
  prEND_SUBTRACT             ,
  prEND_UNSTRING             ,
  prEND_WRITE                ,
  prENTER                    ,
  prENTRY                    ,
  prENVIRONMENT              ,
  prEOL                      ,
  prEOP                      ,
  prEOS                      ,
  prEQUAL                    ,
  prEQUALS                   ,
  prERASE                    ,
  prERROR                    ,
  prESCAPE                   ,
  prESI                      ,
  prEVALUATE                 ,
  prEVENT_POINTER            ,
  prEVERY                    ,
  prEXAMINE                  ,
  prEXCEEDS                  ,
  prEXCEPTION                ,
  prEXCESS_3                 ,
  prEXCLUSIVE                ,
  prEXEC                     ,
  prEXECUTE                  ,
  prEXHIBIT                  ,
  prEXIT                     ,
  prEXTEND                   ,
  prEXTENDED_SEARCH          ,
  prEXTERNAL                 ,
  prEXTERNALLY_DESCRIBED_KEY ,
  prFACTORY                  ,
  prFALSE                    ,
  prFD                       ,
  prFILE                     ,
  prFILE_CONTROL             ,
  prFILE_ID                  ,
  prFILE_LIMIT               ,
  prFILE_LIMITS              ,
  prFILLER                   ,
  prFINAL                    ,
  prFIRST                    ,
  prFIXED                    ,
  prFOOTING                  ,
  prFOR                      ,
  prFOREGROUND_COLOR         ,
  prFOREGROUND_COLOUR        ,
  prFORMAT                   ,
  prFROM                     ,
  prFULL                     ,
  prFUNCTION                 ,
  prGENERATE                 ,
  prGIVING                   ,
  prGLOBAL                   ,
  prGO                       ,
  prGOBACK                   ,
  prGREATER                  ,
  prGRID                     ,
  prGROUP                    ,
  prHEADING                  ,
  prHIGH                     ,
  prHIGHLIGHT                ,
  prHIGH_VALUE               ,
  prHIGH_VALUES              ,
  prID                       ,
  prIDENTIFICATION           ,
  prIDENTIFIED               ,
  prIF                       ,
  prIGNORE                   ,
  prIN                       ,
  prINDEX                    ,
  prINDEXED                  ,
  prINDIC                    ,
  prINDICATE                 ,
  prINDICATOR                ,
  prINDICATORS               ,
  prINHERITING               ,
  prINHERITS                 ,
  prINITIAL                  ,
  prINITIALIZE               ,
  prINITIATE                 ,
  prINPUT                    ,
  prINPUT_OUTPUT             ,
  prINSERT                   ,
  prINSPECT                  ,
  prINSTALLATION             ,
  prINTO                     ,
  prINVALID                  ,
  prINVOKE                   ,
  prINVOKED                  ,
  prI_O                      ,
  prI_O_CONTROL              ,
  prJAPANESE                 ,
  prJUST                     ,
  prJUSTIFIED                ,
  prKANJI                    ,
  prKEPT                     ,
  prKEY                      ,
  prKEYBOARD                 ,
  prLABEL                    ,
  prLAST                     ,
  prLEADING                  ,
  prLEAVE                    ,
  prLEFT                     ,
  prLEFT_JUSTIFY             ,
  prLEFTLINE                 ,
  prLENGTH                   ,
  prLENGTH_CHECK             ,
  prLESS                     ,
  prLIMIT                    ,
  prLIMITS                   ,
  prLIN                      ,
  prLINAGE                   ,
  prLINAGE_COUNTER           ,
  prLINE                     ,
  prLINE_COUNTER             ,
  prLINES                    ,
  prLINKAGE                  ,
  prLOCAL_STORAGE            ,
  prLOCK                     ,
  prLOCKING                  ,
  prLOW                      ,
  prLOWER                    ,
  prLOWLIGHT                 ,
  prLOW_VALUE                ,
  prLOW_VALUES               ,
  prMANUAL                   ,
  prMASTER_INDEX             ,
  prMEMORY                   ,
  prMERGE                    ,
  prMESSAGE                  ,
  prMETHOD                   ,
  prMETHOD_ID                ,
  prMODE                     ,
  prMODIFIED                 ,
  prMODULES                  ,
  prMONITOR_POINTER          ,
  prMORE_LABELS              ,
  prMOVE                     ,
  prMULTIPLE                 ,
  prMULTIPLY                 ,
  prMUTEX_POINTER            ,
  prNAME                     ,
  prNAMED                    ,
  prNATIONAL                 ,
  prNATIONAL_EDITED          ,
  prNATIVE                   ,
  prNCHAR                    ,
  prNEGATIVE                 ,
  prNEXT                     ,
  prNO                       ,
  prNO_ECHO                  ,
  prNOMINAL                  ,
  prNOT                      ,
  prNOTE                     ,
  prNSTD_REELS               ,
  prNULL                     ,
  prNULLS                    ,
  prNUMBER                   ,
  prNUMERIC                  ,
  prNUMERIC_EDITED           ,
  prOBJECT                   ,
  prOBJECT_COMPUTER          ,
  prOBJECT_ID                ,
  prOBJECT_STORAGE           ,
  prOCCURS                   ,
  prOF                       ,
  prOFF                      ,
  prO_FILL                   ,
  prOMITTED                  ,
  prON                       ,
  prOOSTACKPTR               ,
  prOPEN                     ,
  prOPTIONAL                 ,
  prOR                       ,
  prORDER                    ,
  prORGANIZATION             ,
  prOTHER                    ,
  prOTHERWISE                ,
  prOUTPUT                   ,
  prOVERFLOW                 ,
  prOVERLINE                 ,
  prPACKED_DECIMAL           ,
  prPADDING                  ,
  prPAGE                     ,
  prPAGE_COUNTER             ,
  prPARAGRAPH                ,
  prPASSWORD                 ,
  prPERFORM                  ,
  prPF                       ,
  prPH                       ,
  prPIC                      ,
  prPICTURE                  ,
  prPLUS                     ,
  prPOINTER                  ,
  prPOS                      ,
  prPOSITION                 ,
  prPOSITIONING              ,
  prPOSITIVE                 ,
  prPREVIOUS                 ,
  prPRINT                    ,
  prPRINTER                  ,
  prPRINTER_1                ,
  prPRINTING                 ,
  prPRINT_SWITCH             ,
  prPRIVATE                  ,
  prPROCEDURE                ,
  prPROCEDURE_POINTER        ,
  prPROCEDURES               ,
  prPROCEED                  ,
  prPROCESSING               ,
  prPROGRAM                  ,
  prPROGRAM_ID               ,
  prPROMPT                   ,
  prPROTECTED                ,
  prPUBLIC                   ,
  prPURGE                    ,
  prQUEUE                    ,
  prQUOTE                    ,
  prQUOTES                   ,
  prRANDOM                   ,
  prRANGE                    ,
  prRD                       ,
  prREAD                     ,
  prREADING                  ,
  prREADY                    ,
  prRECEIVE                  ,
  prRECORD                   ,
  prRECORDING                ,
  prRECORD_OVERFLOW          ,
  prRECORDS                  ,
  prREDEFINES                ,
  prREEL                     ,
  prREFERENCE                ,
  prREFERENCES               ,
  prRELATIVE                 ,
  prRELEASE                  ,
  prRELOAD                   ,
  prREMAINDER                ,
  prREMARKS                  ,
  prREMOVAL                  ,
  prRENAMES                  ,
  prREORG_CRITERIA           ,
  prREPEATED                 ,
  prREPLACE                  ,
  prREPLACING                ,
  prREPORT                   ,
  prREPORTING                ,
  prREPORTS                  ,
  prREQUIRED                 ,
  prREREAD                   ,
  prRERUN                    ,
  prRESERVE                  ,
  prRESET                    ,
  prRESTRICTED               ,
  prRETURN                   ,
  prRETURN_CODE              ,
  prRETURNING                ,
  prREVERSE                  ,
  prREVERSED                 ,
  prREVERSE_VIDEO            ,
  prREWIND                   ,
  prREWRITE                  ,
  prRF                       ,
  prRH                       ,
  prRIGHT                    ,
  prRIGHT_JUSTIFY            ,
  prROLLBACK                 ,
  prROUNDED                  ,
  prRUN                      ,
  prS01                      ,
  prS02                      ,
  prS03                      ,
  prS04                      ,
  prS05                      ,
  prSAME                     ,
  prSCREEN                   ,
  prSD                       ,
  prSEARCH                   ,
  prSECTION                  ,
  prSECURE                   ,
  prSECURITY                 ,
  prSEEK                     ,
  prSEGMENT                  ,
  prSEGMENT_LIMIT            ,
  prSELECT                   ,
  prSELECTIVE                ,
  prSELF                     ,
  prSELFCLASS                ,
  prSEMAPHORE_POINTER        ,
  prSEND                     ,
  prSENTENCE                 ,
  prSEPARATE                 ,
  prSEQUENCE                 ,
  prSEQUENTIAL               ,
  prSERVICE                  ,
  prSET                      ,
  prSHIFT_IN                 ,
  prSHIFT_OUT                ,
  prSIGN                     ,
  prSIZE                     ,
  prSKIP1                    ,
  prSKIP2                    ,
  prSKIP3                    ,
  prSORT                     ,
  prSORT_CONTROL             ,
  prSORT_CORE_SIZE           ,
  prSORT_FILE_SIZE           ,
  prSORT_MERGE               ,
  prSORT_MESSAGE             ,
  prSORT_MODE_SIZE           ,
  prSORT_OPTION              ,
  prSORT_RETURN              ,
  prSOURCE                   ,
  prSOURCE_COMPUTER          ,
  prSPACE                    ,
  prSPACE_FILL               ,
  prSPACES                   ,
  prSPECIAL_NAMES            ,
  prSTANDARD                 ,
  prSTANDARD_1               ,
  prSTANDARD_2               ,
  prSTART                    ,
  prSTATUS                   ,
  prSTOP                     ,
  prSTORE                    ,
  prSTRING                   ,
  prSUB_FILE                 ,
  prSUB_QUEUE_1              ,
  prSUB_QUEUE_2              ,
  prSUB_QUEUE_3              ,
  prSUBTRACT                 ,
  prSUM                      ,
  prSUPER                    ,
  prSUPPRESS                 ,
  prSYMBOLIC                 ,
  prSYNC                     ,
  prSYNCHRONIZED             ,
  prSYSIN                    ,
  prSYSIPT                   ,
  prSYSLST                   ,
  prSYSOUT                   ,
  prSYSPCH                   ,
  prSYSPUNCH                 ,
  prTALLYING                 ,
  prTAPE                     ,
  prTERMINAL                 ,
  prTERMINATE                ,
  prTEST                     ,
  prTEXT                     ,
  prTHAN                     ,
  prTHEN                     ,
  prTHREAD_LOCAL             ,
  prTHREAD_POINTER           ,
  prTHROUGH                  ,
  prTHRU                     ,
  prTIME                     ,
  prTIME_OF_DAY              ,
  prTIMEOUT                  ,
  prTIME_OUT                 ,
  prTIMES                    ,
  prTITLE                    ,
  prTO                       ,
  prTOP                      ,
  prTOTALED                  ,
  prTOTALING                 ,
  prTRACE                    ,
  prTRACK_AREA               ,
  prTRACK_LIMIT              ,
  prTRACKS                   ,
  prTRAILING                 ,
  prTRAILING_SIGN            ,
  prTRANSFORM                ,
  prTRUE                     ,
  prTYPE                     ,
  prTYPEDEF                  ,
  prUNDERLINE                ,
  prUNEQUAL                  ,
  prUNIT                     ,
  prUNLOCK                   ,
  prUNSTRING                 ,
  prUNTIL                    ,
  prUP                       ,
  prUPDATE                   ,
  prUPON                     ,
  prUPPER                    ,
  prUPSI_0                   ,
  prUPSI_1                   ,
  prUPSI_2                   ,
  prUPSI_3                   ,
  prUPSI_4                   ,
  prUPSI_5                   ,
  prUPSI_6                   ,
  prUPSI_7                   ,
  prUSAGE                    ,
  prUSE                      ,
  prUSER                     ,
  prUSING                    ,
  prVALUE                    ,
  prVALUES                   ,
  prVARIABLE                 ,
  prVARYING                  ,
  prWAIT                     ,
  prWHEN                     ,
  prWHEN_COMPILED            ,
  prWITH                     ,
  prWORDS                    ,
  prWORKING_STORAGE          ,
  prWRITE                    ,
  prWRITE_ONLY               ,
  prWRITE_VERIFY             ,
  prWRITING                  ,
  prZERO                     ,
  prZEROES                   ,
  prZERO_FILL                ,
  prZEROS                    ,
  prVacio);

  TPalRes = class
    public
      function    IndexOf(sword: string): integer;
  end;

implementation

uses SysUtils;

{ ListPClaves }

function TPalRes.IndexOf(sword: string): integer;
var
  left,right: integer;
  medio: integer;
  RCmp: integer;
begin
  left := 0;
  right := ctnPalRes - 1;
  Result := -1;
  while (Left <= Right) and (Result = -1) do
  begin
    medio := (left + right) div 2;
    RCmp := CompareStr(sword, ArrPalRes[medio]);
    if RCmp = 0 then Result := medio
    else if RCmp > 0 then left := medio + 1
    else right := medio - 1;
  end;
end;

end.


