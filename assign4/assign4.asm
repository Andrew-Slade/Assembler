//KC03H99A JOB ,'ANDREW SLADE',MSGCLASS=H                               00010000
//JSTEP01 EXEC PGM=ASSIST                                               00020000
//STEPLIB DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR                        00030000
//SYSPRINT DD SYSOUT=*                                                  00040000
//SYSIN    DD *                                                         00050000
***************************************************                     00060000
* ASSIGNMENT 3 - BASIC ASSEMBLY PROGRAM           *                     00070000
* ANDREW SLADE                                    *                     00080001
* DATE DUE: 09/29/2017                            *                     00090001
***************************************************                     00100000
*PROCEDURE                                        *                     00110000
*   *                                             *                     00120000
*1 CLEAR OUT RECORDS                              *                     00121091
*2 READ RECORDS INTO BUFFER                       *                     00122091
*3 CONVERT RECORDSINTO DECIMAL FORM               *                     00123091
*4 ADD ALL RECORDS TOGETHER                       *                     00124091
*5 STORE SUM INTO TWO SEPARATE STORAGES           *                     00125091
*6 ONE FOR GRAND SUM, ONE FOR LINE TOTAL          *                     00126091
*7 COUNT LINES                                    *                     00127091
*8 STORE LINE COUNT                               *                     00127191
*9 PRINT ALL VALUES                               *                     00127291
***************************************************                     00128000
ASSIGN4  CSECT                                                          00129001
*                                                                       00130000
         USING ASSIGN4,15        ESTABLISH ADDRESSABILITY               00140001
*                                                                       00150000
         SR    10,10             CLEAR R10                              00150153
         SR    12,12             CLEAR R12                              00150287
         XREAD BUFFER,80         READ INTO BUFFER                       00151033
*                                                                       00152076
READF    BC    B'0100',ENDREADF  BREAK IF COND CODE 1                   00160034
*                                                                       00160135
         L     10,TEMP           LOAD R10 WITH ITS VAL                  00160260
         L     11,INCR           LOAD R11 WITH INCR VAL                 00160360
         L     12,GNTOT          LOAD TOTAL INTO A R12                  00160491
         AR    10,11             INCREMENT 10                           00160587
         ST    10,TEMP           STORE R10 INTO TEMP                    00160687
*                                                                       00160776
         XDECI 2,BUFFER          CONVERT CHAR TO DEC STORE IN R2        00160890
         XDECI 3,0(0,1)          CONVERT 2ND STORE IN R3                00160990
         XDECI 4,0(0,1)          CONVERT 3RD STORE IN R4                00161090
         XDECO 2,VAL1            CONVERT RECORD TO CHAR FORM            00161190
         XDECO 3,VAL2            CONVERT SECOND RECORD LIKEWISE         00161290
         XDECO 4,VAL3            CONVERT THIRD RECORDS LIKEWISE         00161390
*                                                                       00161476
         SR    8,8               ZERO OUT R8                            00161648
         AR    8,2               ADD R2 TO R8                           00161748
         AR    8,3               ADD R3 TO R8                           00161848
         AR    8,4               ADD R4 TO R8                           00161948
         AR    12,8              ADD TO TOT                             00162060
         ST    12,GNTOT          STORE TOTAL                            00162191
         XDECO 8,SUM             STORE R8 AS CHARS                      00162378
*                                                                       00162844
         XPRNT CCV,133        PRINT READ RECORD                         00162995
         XREAD BUFFER,80         READ INTO BUFFER                       00163001
*                                                                       00180001
         BC    B'1111',READF     LOOP BACK TO READF FOR ANY CODE        00188337
*                                                                       00188476
ENDREADF XDECO 10,LINES          STORE NUMBER OF LINES                  00190053
*                                                                       00190176
         XDECO 12,TOTAL          STORE TOTAL AS CHAR                    00190287
         XPRNT CCC,133           PRNT RECORD NUMBER                     00191097
         XPRNT CV,133            PRNT GRAND TOTAL                       00192097
         BR    14                BREAK TO OS                            00200090
*                                                                       00210053
         LTORG                                                          00221000
BUFFER   DC    80C'%'            CREATES BUFFER AND FILLS               00222001
*                                                                       00223176
CCV      DC    C'0'              CARRIAGE CONTROL                       00223290
         DC     5C' '            SPACES                                 00223390
VAL1     DS    CL12              STORAGE FOR RECORD 1                   00223490
         DC     5C' '            SPACES                                 00223590
VAL2     DS    CL12              STORAGE FOR RECORD 2                   00223690
         DC     5C' '            SPACES                                 00223790
VAL3     DS    CL12              STORAGE FOR RECORD 3                   00223890
         DC     5C' '            SPACES                                 00223990
*                                                                       00224076
SUM      DS    CL12              STORAGE FOR LINE TOTAL                 00224190
         DC    64C' '            SPACES                                 00224290
*                                                                       00224392
CV       DC    C'0'              CARRIAGE CONTROL                       00224492
         DC    56C' '            SPACES                                 00224594
TOTAL    DS    12C               STORAGE FOR GRAN TOTAL AS CHAR         00224694
         DC    64C' '            SPACES                                 00224794
*                                                                       00224894
GNTOT    DC    F'0'              STORAGE FOR LINE TOTAL                 00224990
*                                                                       00225076
INCR     DC    F'1'              STORES INCREMENT VALUE                 00225173
*                                                                       00225296
CCC      DC    C'0'                                                     00225395
         DC    56C' '                                                   00225595
LINES    DC    12C'@'            STORES TOTAL NUMBER OF LINES           00225695
         DC    64C' '                                                   00225795
*                                                                       00225895
TEMP     DC    F'0'              STORES TEMPORARY HOLDER FOR VAL        00225973
*********************************************************               00226090
* AS A NOTE, THE PROFESSOR ASKED ME TO                  *               00226190
* POINT OUT THAT LINUX'S X3270 DOES NOT LIKE            *               00226290
* READING NUMBERS THAT ARE IN THE FIRST COLUMN          *               00226390
* THUS ALL NUMBERS IN THE RECORD START IN COLUMN 2      *               00226490
*********************************************************               00226590
         END   ASSIGN4                                                  00226601
 12  34  56                                                             00226775
 45  98  23                                                             00226875
 55  88  99                                                             00226975
 10  09  32                                                             00227075
 66  22  07                                                             00227175
 98  76  54                                                             00227275
 01  01  01                                                             00227375
/*                                                                      00227400
//                                                                      00228000
