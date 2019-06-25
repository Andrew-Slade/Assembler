//KC03H99A JOB ,'ANDREW SLADE',MSGCLASS=H                               00010001
//JSTEP01 EXEC PGM=ASSIST                                               00020001
//STEPLIB DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR                        00030001
//SYSPRINT DD SYSOUT=*                                                  00040001
//SYSIN    DD *                                                         00050001
***************************************************                     00060001
* ASSIGNMENT 3 - BASIC ASSEMBLY PROGRAM           *                     00070001
* ANDREW SLADE                                    *                     00080001
* DATE DUE: 10/13/2017                            *                     00090001
***************************************************                     00100001
*PROCEDURE                                        *                     00110001
*   *                                             *                     00120001
*1 CLEAR OUT RELEVANT REGISTERS                   *                     00130059
*2 LOAD ANY USED CONSTANTS                        *                     00140059
*3 READ RECORDS                                   *                     00150059
*4 CALCULATE GROSS PAY, BONUSES, EMPLOYEE COUNT   *                     00160059
*5 CALCULATE AVERAGE GROSS PAY                    *                     00170059
*6 OUTPUT ALL RECORDS                             *                     00180059
*7 OUTPUT GROSS PAY, BONUSES, EMPLOYEE COUNT      *                     00190059
*8 OUTPUT AVERAGE GROSS PAY                       *                     00200059
*   *                                             *                     00201099
*FUNCTION:                                        *                     00202099
*   *     TO READ ALL OF THE RECORDS, PRINT EACH  *                     00203099
*   *     RECORD ON A LINE, AND THEN CALCULATE    *                     00204099
*   *     TOTAL GROSS PAY, EMPLOYEE COUNT, TOTAL  *                     00205099
*   *     BONUSES, AND AVERAGE GROSS.             *                     00206099
*   *                                             *                     00208099
*INPUT: RECORDS REGARDING VARIOUS INFORMATION     *                     00209099
*       ABOUT EMPLOYEES AND THEIR EARNINGS        *                     00209199
*  *                                              *                     00209299
*ENTRY CONDS: NONE                                *                     00209399
*  *                                              *                     00209699
*EXIT CONDS: NONE                                 *                     00209799
*  *                                              *                     00209899
*NOTES: NONE                                      *                     00210099
*  *                                              *                     00210199
*OUTPUT: A REPORT SHOWING DATA FOR EVERY EMPLOYEE *                     00210399
*   *                                             *                     00210499
*REGISTER USAGE: R2: STORAGE FOR EMPLOYEE ID      *                     00210599
*                    ALSO USED TO CALC AVG GROSS  *                     00210699
*                R3: STORAGE FOR HOURLY PAY       *                     00210799
*                    ALSO USED TO CALC AVG GROSS  *                     00210899
*                R4: STORAGE FOR NUMBER OF HOURS  *                     00210999
*                R5: STORAGE FOR DEDUCTIONS       *                     00211099
*                    ALSO USED TO CALC AVG GROSS  *                     00211199
*                R6: STORAGE FOR BONUS AMOUNT     *                     00211299
*                R7: DIVISION                     *                     00211399
*                R8: MULTIPLICATION TO CALC GROSS *                     00211499
*                R9: MULTIPLICATION TO CALC GROSS *                     00211599
*                R10: HOLDS EMPLOYEE COUNT        *                     00211699
*                R12: HOLDS TOTAL TEMPORARILY     *                     00211799
***************************************************                     00212001
ASSIGN5  CSECT                                                          00220001
         USING ASSIGN5,15        ESTABLISH ADDRESSABILITY               00240001
         SR    10,10             PREPARE A SPACE TO HOLD INCREMENT      00260099
         SR    12,12             PREPARE A SPACE FOR GROSS TOTAL        00270099
         XREAD BUFFER,80         READ INTO BUFFER                       00280001
*                                                                       00290099
READF    BC    B'0100',ENDREADF  BREAK ALL RECORDS READ                 00300099
         LA    10,1(,10)         INCREMENT EMPLOYEE COUNT               00320099
         ST    10,TEMP           STORE EMPLOYEE COUNT                   00330099
*  READ RECORDS                                                         00340047
         XDECI 2,BUFFER          CONVERT EMPLOYEE ID                    00350006
         MVC   NAME(25),19(1)    MOVE NAME TO STORAGE                   00360019
         XDECI 3,0(0,1)          CONVERT HOURLY PAY                     00370009
         XDECI 4,0(0,1)          CONVERT NUMBER OF HOURS                00380006
         XDECI 5,0(0,1)          CONVERT DEDUCTION AMOUNT               00390006
         XDECI 6,0(0,1)          CONVERT BONUS AMOUNT                   00400006
*                                                                       00401099
         XDECO 2,VAL1            CONVERT ITEM TO CHAR FORM              00410019
         XDECO 3,VAL2            CONVERT SECOND ITEM LIKEWISE           00420019
         XDECO 4,VAL3            CONVERT THIRD ITEM LIKEWISE            00430019
         XDECO 5,VAL4            CONVERT FOURTH ITEM LIKEWISE           00440019
         XDECO 6,VAL5            CONVERT FIFTH ITEM LIKEWISE            00450019
* TOTALGROSS PAY AND EMPLOYEE COUNT CALCULATIONS                        00470059
         SR    7,7               PREPARE A SPACE FOR PAY                00480099
         SR    8,8               PREPARE A SPACE FOR HOURS              00490099
         SR    9,9               PREPARE A SPACE FOR MULTIPLICATION     00500099
*                                                                       00501099
         LR    7,3               LOAD PAY                               00510099
         LR    9,4               LOAD HOURS                             00520099
         MR    8,7               MULTIPLY PAY BY HOURS                  00530036
         SR    9,5               SUBTRACT DEDUCTION AMOUNT              00540041
         AR    9,6               ADD BONUS AMOUNT                       00550041
         ST    9,GNTOT           STORE LINE COUNT                       00560041
         XDECO 9,GROS            STORE GROSS PER PERSON                 00570041
*                                                                       00571099
         AR    12,9              PUSH GROSS TOTAL TO ANOTHER LOCATION   00580099
         ST    12,TOTAL          STORES GROSS TOTAL                     00590099
         L     12,TOTAL          LOAD TOTAL GROSS                       00600099
         XPRNT CCV,133           PRINT READ RECORD                      00610092
* TOTAL BONUS CALCULATION                                               00630059
         L     7,BNS             LOAD TOTAL BONUS                       00640099
         SR    3,3               PREPARE A SPACE FOR DIVISION           00650099
         LR    3,6               LOAD BONUS AMOUNT                      00660099
         AR    7,3               ADD  BONUS WITH PREVIOUS BONUS         00670047
         ST    7,BNS             STORE BONUS AMOUNT                     00680099
         XDECO 7,TOBNS           STORE BONUS AMOUNT IN PRINTABLE FORM   00690059
* READ AND PRINT RECORDS                                                00710065
         XREAD BUFFER,80         READ INTO BUFFER                       00720001
         BC    B'1111',READF     LOOP BACK TO READF FOR ANY CODE        00740001
ENDREADF XDECO 10,LINES          STORE NUMBER OF LINES                  00760001
         XDECO 12,TOTAL          STORE TOTAL AS CHAR                    00780001
* CALCULATE AVERAGE GROSS                                               00790060
         SR    3,3               PREPARE A SPACE FOR TOTAL GROSS        00800099
         SR    2,2               PREPARE A SPACE FOR DIVISION           00810099
         SR    5,5               PREPARE A SPACE FOR EMPLOYEE COUNT     00820099
*                                                                       00821099
         LR    3,12              LOADS TOTAL GROSS                      00830099
         LR    5,10              LOADS EMPLOYEE COUNT                   00840099
         DR    2,5               DIVIDE GROSS BY EMPLOYEES              00850062
         XDECO 3,AVGGS           STORE AVERAGE GROSS                    00860063
* PRINT ALL VALUES THEN BRANCH TO OS                                    00870065
         XPRNT CCC,133           PRNT RECORD NUMBER                     00960001
         XPRNT CCL,133           PRNT TOTAL BONUSES                     00970063
         XPRNT CCQ,133           PRNT GROSS PAY                         00980063
         XPRNT CCT,133           PRNT AVERAGE GROSS                     00990063
*                                                                       00991099
         BR    14                BRANCH TO OS                           01000019
*                                                                       01010099
         LTORG                                                          01020001
BUFFER   DC    80C'%'            CREATES BUFFER AND FILLS               01040001
*                                                                       01050001
CCV      DC    C'0'              CARRIAGE CONTROL                       01060001
         DC     4C' '            SPACES                                 01070099
NAME     DS    CL25              STORAGE FOR NAME                       01080018
         DC     5C' '            SPACES                                 01090099
VAL1     DS    CL12              STORAGE FOR ID                         01100018
         DC     4C' '            SPACES                                 01110099
VAL2     DS    CL12              STORAGE FOR PAY                        01120018
         DC     4C' '            SPACES                                 01130093
VAL3     DS    CL12              STORAGE FOR HOURS                      01140018
         DC     4C' '            SPACES                                 01150093
VAL4     DS    CL12              STORAGE FOR DEDUCTION                  01160018
         DC     4C' '            SPACES                                 01170093
VAL5     DS    CL12              STORAGE FOR BONUS                      01180018
         DC     4C' '            SPACES                                 01190093
GROS     DS    CL12              STORAGE FOR THE GROSS                  01200093
         DC     6C' '            SPACES                                 01210099
*                                                                       01220099
CCQ      DC    C'0'              CARRIAGE CONTROL                       01230086
         DC     9C' '             SPACES                                01240099
GROSS    DC    C'TOTAL GROSS PAY:'                                      01250099
         DC     9C' '            SPACES                                 01260099
TOTAL    DC    12C'@'            STORAGE FOR GROSS PAY                  01270057
         DC    86C' '            SPACES                                 01280099
*                                                                       01290099
GNTOT    DC    F'0'              STORAGE FOR LINE TOTAL                 01300001
*                                                                       01310099
BNS      DC    F'0'              STORES BONUS AMOUNT                    01320047
*                                                                       01330099
TEMP     DC    F'0'              STORES TEMPORARY HOLDER FOR VAL        01340072
*                                                                       01350099
CCC      DC    C'0'              CARRIAGE CONTROL                       01360077
         DC    4C' '             SPACES                                 01370094
LINECNT  DC    C'TOTAL EMPLOYEE COUNT:'                                 01380018
         DC    9C' '             SPACES                                 01390099
LINES    DC    12C'@'            STORES TOTAL NUMBER OF LINES           01400001
         DC    86C' '            SPACES                                 01410099
*                                                                       01420099
CCL      DC    C'0'              CARRIAGE CONTROL                       01430088
         DC    11C' '             SPACES                                01440099
BONUS    DC    C'TOTAL BONUSES:'                                        01450072
         DC     9C' '            SPACES                                 01460099
TOBNS    DC    12C'@'            STORES TOTAL BONUSES                   01470047
         DC    86C' '            SPACES                                 01480099
*                                                                       01490099
CCT      DC    C'0'                                                     01500077
         DC     7C' '             SPACES                                01510099
AVGGR    DC    C'AVERAGE GROSS PAY:'                                    01520063
         DC     9C' '            SPACES                                 01530099
AVGGS    DC    12C'@'            STORES TOTAL BONUSES                   01540063
         DC    86C' '            SPACES                                 01550099
*                                                                       01560099
         END   ASSIGN5                                                  01570003
/*                                                                      01580004
//*                                                                     01590004
//FT05F001 DD *                                                         01600005
12345 15 80 25 500      JOAN TOWER                                      01610014
23456 19 80 0  10       WOLFGANG AMADEUS MOZART                         01620014
22132 10 80 31 200      RICHARD STRAUSS                                 01630014
65465 22 80 15 900      AMY BEACH                                       01640014
44560 23 28 34 70       DAME ETHEL SMYTHE                               01650014
99870 22 80 21 0        PETER ILYICH TCHAIKOVSKY                        01660017
14966 20 78 0  210      ANTON BRUCKNER                                  01670017
/*                                                                      01680002
//*                                                                     01690003
//FT06F001 DD SYSOUT=*                                                  01700003
//*                                                                     01710003
//SYSPRINT DD SYSOUT=*                                                  01720003
//                                                                      01730002
