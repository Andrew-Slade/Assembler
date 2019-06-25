//KC03H99A JOB ,'ANDREW SLADE',MSGCLASS=H                               00010000
//JSTEP01 EXEC PGM=ASSIST                                               00020000
//STEPLIB DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR                        00030000
//SYSPRINT DD SYSOUT=*                                                  00040000
//SYSIN    DD *                                                         00050000
***************************************************                     00060000
* ASSIGNMENT 5B- BASIC ASSEMBLY PROGRAM           *                     00070000
* ANDREW SLADE                                    *                     00080000
* DATE DUE: 10/13/2017                            *                     00090000
***************************************************                     00100000
*PROCEDURE                                        *                     00110000
*   *                                             *                     00120000
*1 CLEAR OUT RELEVANT REGISTERS                   *                     00130000
*2 LOAD ANY USED CONSTANTS                        *                     00140000
*3 READ RECORDS                                   *                     00150000
*4 CALCULATE GROSS PAY, BONUSES, EMPLOYEE COUNT   *                     00160000
*5 CALCULATE AVERAGE GROSS PAY                    *                     00170000
*6 OUTPUT ALL RECORDS                             *                     00180000
*7 OUTPUT GROSS PAY, BONUSES, EMPLOYEE COUNT      *                     00190000
*8 OUTPUT AVERAGE GROSS PAY                       *                     00200000
*   *                                             *                     00210000
*FUNCTION:                                        *                     00220000
*   *     TO READ ALL OF THE RECORDS, PRINT EACH  *                     00230000
*   *     RECORD ON A LINE, AND THEN CALCULATE    *                     00240000
*   *     TOTAL GROSS PAY, EMPLOYEE COUNT, TOTAL  *                     00250000
*   *     BONUSES, AND AVERAGE GROSS. WITH A      *                     00260086
*         LEGIBLE FORMAT.                         *                     00270086
*   *                                             *                     00280000
*INPUT: RECORDS REGARDING VARIOUS INFORMATION     *                     00290000
*       ABOUT EMPLOYEES AND THEIR EARNINGS        *                     00300000
*  *                                              *                     00310000
*ENTRY CONDS: NONE                                *                     00320000
*  *                                              *                     00330000
*EXIT CONDS: NONE                                 *                     00340000
*  *                                              *                     00350000
*NOTES: NONE                                      *                     00360000
*  *                                              *                     00370000
*OUTPUT: A REPORT SHOWING DATA FOR EVERY EMPLOYEE *                     00380000
*   *                                             *                     00390000
*REGISTER USAGE: R2: STORAGE FOR EMPLOYEE ID      *                     00400000
*                    ALSO USED TO CALC AVG GROSS  *                     00410000
*                R3: STORAGE FOR HOURLY PAY       *                     00420000
*                    ALSO USED TO CALC AVG GROSS  *                     00430000
*                R4: STORAGE FOR NUMBER OF HOURS  *                     00440000
*                R5: STORAGE FOR DEDUCTIONS       *                     00450000
*                    ALSO USED TO CALC AVG GROSS  *                     00460000
*                R6: STORAGE FOR BONUS AMOUNT     *                     00470000
*                R7: DIVISION                     *                     00480000
*                R8: MULTIPLICATION TO CALC GROSS *                     00490000
*                R9: MULTIPLICATION TO CALC GROSS *                     00500000
*                R10: HOLDS EMPLOYEE COUNT        *                     00510000
*                R12: HOLDS TOTAL TEMPORARILY     *                     00520000
***************************************************                     00530000
ASSIGN5B CSECT                                                          00540001
         USING ASSIGN5B,15       ESTABLISH ADDRESSABILITY               00550001
         SR    10,10             PREPARE A SPACE TO HOLD INCREMENT      00560000
         SR    12,12             PREPARE A SPACE FOR GROSS TOTAL        00570000
*                                                                       00580006
         XPRNT CCP,133           PRNT OVERHEAD                          00590080
         XPRNT CCW,133           PRNT RECORD HEADER                     00600006
         XPRNT CCE,133           PRNT DIVIDER                           00610081
*                                                                       00620006
         XREAD BUFFER,80         READ INTO BUFFER                       00630000
*                                                                       00640000
READF    BC    B'0100',ENDREADF  BREAK ALL RECORDS READ                 00650000
         LA    10,1(,10)         INCREMENT EMPLOYEE COUNT               00660000
         ST    10,TEMP           STORE EMPLOYEE COUNT                   00670000
*  READ RECORDS                                                         00680000
         XDECI 2,BUFFER          CONVERT EMPLOYEE ID                    00690000
         MVC   NAME(25),19(1)    MOVE NAME TO STORAGE                   00700000
         XDECI 3,0(0,1)          CONVERT HOURLY PAY                     00710000
         XDECI 4,0(0,1)          CONVERT NUMBER OF HOURS                00720000
         XDECI 5,0(0,1)          CONVERT DEDUCTION AMOUNT               00730000
         XDECI 6,0(0,1)          CONVERT BONUS AMOUNT                   00740000
*                                                                       00750000
         XDECO 2,VAL1            CONVERT ITEM TO CHAR FORM              00760000
         XDECO 3,VAL2            CONVERT SECOND ITEM LIKEWISE           00770000
         XDECO 4,VAL3            CONVERT THIRD ITEM LIKEWISE            00780000
         XDECO 5,VAL4            CONVERT FOURTH ITEM LIKEWISE           00790000
         XDECO 6,VAL5            CONVERT FIFTH ITEM LIKEWISE            00800000
* TOTALGROSS PAY AND EMPLOYEE COUNT CALCULATIONS                        00810000
         SR    7,7               PREPARE A SPACE FOR PAY                00820000
         SR    8,8               PREPARE A SPACE FOR HOURS              00830000
         SR    9,9               PREPARE A SPACE FOR MULTIPLICATION     00840000
*                                                                       00850000
         LR    7,3               LOAD PAY                               00860000
         LR    9,4               LOAD HOURS                             00870000
         MR    8,7               MULTIPLY PAY BY HOURS                  00880000
         SR    9,5               SUBTRACT DEDUCTION AMOUNT              00890000
         AR    9,6               ADD BONUS AMOUNT                       00900000
         ST    9,GNTOT           STORE LINE COUNT                       00910000
         XDECO 9,GROS            STORE GROSS PER PERSON                 00920000
*                                                                       00930000
         AR    12,9              PUSH GROSS TOTAL TO ANOTHER LOCATION   00940000
         ST    12,GTOTAL         STORES GROSS TOTAL                     00950005
         L     12,GTOTAL         LOAD TOTAL GROSS                       00960005
         XPRNT CCM,133           PRINT BLANK LINE                       00970086
         XPRNT CCV,133           PRINT READ RECORD                      00980000
* TOTAL BONUS CALCULATION                                               00990000
         L     7,BNS             LOAD TOTAL BONUS                       01000000
         SR    3,3               PREPARE A SPACE FOR DIVISION           01010000
         LR    3,6               LOAD BONUS AMOUNT                      01020000
         AR    7,3               ADD  BONUS WITH PREVIOUS BONUS         01030000
         ST    7,BNS             STORE BONUS AMOUNT                     01040000
         XDECO 7,TOBNS           STORE BONUS AMOUNT IN PRINTABLE FORM   01050000
* READ AND PRINT RECORDS                                                01060000
         XREAD BUFFER,80         READ INTO BUFFER                       01070000
         BC    B'1111',READF     LOOP BACK TO READF FOR ANY CODE        01080000
ENDREADF XDECO 10,LINES          STORE NUMBER OF LINES                  01090000
         XDECO 12,TOTAL          STORE TOTAL AS CHAR                    01100000
* CALCULATE AVERAGE GROSS                                               01110000
         SR    3,3               PREPARE A SPACE FOR TOTAL GROSS        01120000
         SR    2,2               PREPARE A SPACE FOR DIVISION           01130000
         SR    5,5               PREPARE A SPACE FOR EMPLOYEE COUNT     01140000
*                                                                       01150000
         LR    3,12              LOADS TOTAL GROSS                      01160000
         LR    5,10              LOADS EMPLOYEE COUNT                   01170000
         DR    2,5               DIVIDE GROSS BY EMPLOYEES              01180000
         XDECO 3,AVGGS           STORE AVERAGE GROSS                    01190000
* PRINT ALL VALUES THEN BRANCH TO OS                                    01200000
         XPRNT CCZ,133           PRNT A DIVIDER                         01210081
         XPRNT CCC,133           PRNT RECORD NUMBER                     01220000
         XPRNT CCM,133           PRNT A BLANK LINE                      01230062
         XPRNT CCT,133           PRNT AVERAGE GROSS                     01240000
*                                                                       01250000
         BR    14                BRANCH TO OS                           01260000
*                                                                       01270000
         LTORG                                                          01280000
BUFFER   DC    80C'%'            CREATES BUFFER AND FILLS               01290000
*                                                                       01300002
CCP      DC    C'1'              CARRIAGE CONTROL TO TOP OF PAGE        01310021
         DC    55C' '            SPACES                                 01320020
HEAD     DC    C'EMPLOYEE PAYROLL REPORT'                               01330022
         DC    54C' '            SPACES                                 01340020
*                                                                       01350023
CCW      DC    C'0'              CARRIAGE CONTROL TO TOP OF PAGE        01360020
         DC    4C' '             SPACES                                 01370087
ENAME    DC    C'EMPLOYEE NAME'                                         01380002
         DC    19C' '            SPACES                                 01390087
EID      DC    C'EMPLOYEE ID'                                           01400002
         DC    8C' '             SPACES                                 01410076
ERATE    DC    C'PAY RATE'                                              01420002
         DC    4C' '             SPACES                                 01430020
EHOURS   DC    C'HOURS WORKED'                                          01440002
         DC    8C' '             SPACES                                 01450056
EDEDUC   DC    C'DEDUCTIONS'                                            01460002
         DC    11C' '            SPACES                                 01470020
EBONUS   DC    C'BONUS'                                                 01480002
         DC    5C' '             SPACES                                 01490057
EGROSS   DC    C'GROSS PAY'                                             01500002
         DC     9C' '            SPACES                                 01510076
*                                                                       01520000
CCM      DC    C'0'              BLANK LINE                             01530062
BLANK    DC    132C' '           BLANK LINE                             01540062
*                                                                       01550060
CCZ      DC    C'0'              CARRIAGE CONTROL TO TOP OF PAGE        01560061
         DC    101C' '           SPACES                                 01570084
EBONUS2  DC    C'------------'                                          01580080
         DC    2C' '             SPACES                                 01590085
EGROSS2  DC    C'------------'                                          01600080
         DC    7C' '             SPACES                                 01610085
*                                                                       01620060
*                                                                       01630080
CCE      DC    C'0'              CARRIAGE CONTROL TO TOP OF PAGE        01640080
         DC    4C' '             SPACES                                 01650080
ENAME1   DC    25C'-'            DIVIDER                                01660090
         DC    7C' '             SPACES                                 01670080
EID1     DC    11C'-'            DIVIDER                                01680093
         DC    4C' '             SPACES                                 01690080
SRATE1   DC    12C'-'            DIVIDER                                01700090
         DC    4C' '             SPACES                                 01710091
EHOURS1  DC    12C'-'            DIVIDER                                01720092
         DC    6C' '             SPACES                                 01730080
EDEDUC1  DC    12C'-'            DIVIDER                                01740092
         DC     4C' '            SPACES                                 01750080
EBONUS1  DC    12C'-'            DIVIDER                                01760092
         DC    2C' '             SPACES                                 01770080
EGROSS1  DC    12C'-'            DIVIDER                                01780092
         DC    5C' '             SPACES                                 01790080
*                                                                       01800080
CCV      DC    C'0'              CARRIAGE CONTROL                       01810000
         DC     4C' '            SPACES                                 01820000
NAME     DS    CL25              STORAGE FOR NAME                       01830000
         DC     6C' '            SPACES                                 01840073
VAL1     DS    CL12              STORAGE FOR ID                         01850000
         DC     4C' '            SPACES                                 01860073
VAL2     DS    CL12              STORAGE FOR PAY                        01870066
         DC     4C' '            SPACES                                 01880000
VAL3     DS    CL12              STORAGE FOR HOURS                      01890000
         DC     6C' '            SPACES                                 01900058
VAL4     DS    CL12              STORAGE FOR DEDUCTION                  01910000
         DC     4C' '            SPACES                                 01920000
VAL5     DS    CL12              STORAGE FOR BONUS                      01930000
         DC     2C' '            SPACES                                 01940058
GROS     DS    CL12              STORAGE FOR THE GROSS                  01950000
         DC     5C' '            SPACES                                 01960073
*                                                                       01970000
*                                                                       01980000
GNTOT    DC    F'0'              STORAGE FOR LINE TOTAL                 01990000
*                                                                       02000000
BNS      DC    F'0'              STORES BONUS AMOUNT                    02010000
*                                                                       02020000
TEMP     DC    F'0'              STORES TEMPORARY HOLDER FOR VAL        02030000
*                                                                       02040005
GTOTAL   DC    F'0'              STORES GROSS TOTAL TEMPORARILY         02050005
*                                                                       02060000
CCC      DC    C'0'              CARRIAGE CONTROL                       02070000
         DC    4C' '             SPACES                                 02080033
LINECNT  DC    C'TOTAL EMPLOYEE COUNT:'                                 02090000
LINES    DC    12C'@'            STORES TOTAL NUMBER OF LINES           02100000
         DC    40C' '            SPACES                                 02110077
BONUS    DC    C'TOTAL BONUSES/GROSS PAY:'                              02120024
TOBNS    DC    12C'@'            STORAGE FOR TOTAL BONUSES              02130055
         DC     2C' '            SPACES                                 02140079
TOTAL    DC    12C'@'            STORAGE FOR GROSS PAY                  02150024
         DC     7C' '            SPACES                                 02160079
*                                                                       02170000
CCT      DC    C'0'                                                     02180000
         DC    82C' '             SPACES                                02190041
AVGGR    DC    C'AVERAGE GROSS PAY:'                                    02200000
         DC    15C' '            SPACES                                 02210074
AVGGS    DC    12C'@'            STORES TOTAL BONUSES                   02220000
         DC     5C' '            SPACES                                 02230074
*                                                                       02240000
         END   ASSIGN5B                                                 02250001
/*                                                                      02260000
//*                                                                     02270000
//FT05F001 DD *                                                         02280000
12345 15 80 25 500      JOAN TOWER                                      02290000
23456 19 80 0  10       WOLFGANG AMADEUS MOZART                         02300000
22132 10 80 31 200      RICHARD STRAUSS                                 02310000
65465 22 80 15 900      AMY BEACH                                       02320000
44560 23 28 34 70       DAME ETHEL SMYTHE                               02330000
99870 22 80 21 0        PETER ILYICH TCHAIKOVSKY                        02340000
14966 20 78 0  210      ANTON BRUCKNER                                  02350000
/*                                                                      02360000
//*                                                                     02370000
//FT06F001 DD SYSOUT=*                                                  02380000
//*                                                                     02390000
//SYSPRINT DD SYSOUT=*                                                  02400000
//                                                                      02410000
