//KC03H99A JOB ,'ANDREW SLADE',MSGCLASS=H                               00010000
//JSTEP01 EXEC PGM=ASSIST                                               00020000
//STEPLIB DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR                        00030000
//SYSPRINT DD SYSOUT=*                                                  00040000
//FT05F001 DD DSN=KC02322.CSCI360.DATAFA17(DATA7),DISP=SHR              00050001
//SYSIN    DD *                                                         00060000
***************************************************                     00070000
* ASSIGNMENT 7- CONDITIONALS AND EXT MNEMONICS    *                     00080000
* ANDREW SLADE                                    *                     00090000
* DATE DUE: 11/01/2017                            *                     00100000
***************************************************                     00110000
*PROCEDURE                                        *                     00120000
*   *                                             *                     00130000
*1 PRINT HEADER                                   *                     00140099
*   *                                             *                     00150099
*2 CLEAR REGISTERS                                *                     00160099
*   *                                             *                     00170099
*3 READ RECORDS                                   *                     00180099
*   *                                             *                     00190099
*4 COMPUTE TOTAL                                  *                     00200099
*   *                                             *                     00210099
*5 COUNT RECORDS                                  *                     00220099
*   *                                             *                     00230099
*6 COMPARE RECORDS WITH ZERO                      *                     00240099
*   *                                             *                     00250099
*7 STORE HIGHEST OR LOWEST VALUE                  *                     00260099
*   *                                             *                     00270099
*8 COUNT NUMBER OF POSITIVE VALUES, NEGATIVE VAL- *                     00280099
*  UES, AND ZERO VALUES                           *                     00290099
*   *                                             *                     00300099
*9 CALCULATE AVERGAGE VALUE                       *                     00310099
*   *                                             *                     00320099
*10 STORE VALUES                                  *                     00330099
*   *                                             *                     00340099
*11 PRINT REPORTS                                 *                     00350099
*   *                                             *                     00360000
*FUNCTION: READ DATA SET, PROCESS DATA, OUTPUT    *                     00370087
*   *      META DATA AS WELL AS DATA SET WITH LAB-*                     00380087
*   *      ELS. COUNT NEGATIVE, POSITIVE, AND     *                     00390087
*   *      ZERO VALUES AS WELL AS STORING HIGHEST,*                     00400087
*   *      LOWEST AND AVERAGE VALUES.             *                     00410087
*INPUT: A DATAFILE FROM ANOTHER USER              *                     00420087
*  *                                              *                     00430000
*ENTRY CONDS: NONE                                *                     00440000
*  *                                              *                     00450000
*EXIT CONDS: NONE                                 *                     00460000
*  *                                              *                     00470000
*NOTES: NONE                                      *                     00480000
*  *                                              *                     00490000
*OUTPUT: A REPORT SHOWING META DATA FOR THE DATA  *                     00500000
*  *     SET.                                     *                     00510000
*  *                                              *                     00520000
*REGISTER USAGE: R2: READS IN RECORDS             *                     00530087
*  *             R4: USED IN DIVISION AS THE EVEN *                     00540099
*  *                 COMPONENT OF THE PAIR        *                     00550099
*  *             R5: HOLDS TOTAL IN DIVISION, IS  *                     00560099
*  *                 THE ODD COMPONENT OF THE PAIR*                     00570099
*  *             R6: HOLDS THE RECORD COUNT IN THE*                     00580099
*  *                 DIVISION PROCESS             *                     00590099
*  *             R10: USED TO COUNT RECORDS       *                     00600099
*  *             R11: USED TO COUNT NEGATIVE, POS-*                     00610099
*  *                  ITIVE, AND ZERO VALUES      *                     00620099
*  *             R12: HOLDS TOTAL OF ALL VALUES   *                     00630099
*  *             R14: HOLDS SYSTEM ADDRESS        *                     00640087
*  *             R15: HOLDS PROGRAM FOR ADDRESSAB-*                     00650087
*  *                   ILITY                      *                     00660087
***************************************************                     00670000
ASSIGN7  CSECT                                                          00680000
         USING ASSIGN7,15        ESTABLISH ADDRESSABILITY               00690000
*                                                                       00700000
         XPRNT CCA,133           PRINT HEADER #1                        00710019
*                                                                       00720099
         SR    12,12             CLEAR AN AREA FOR TOTAL                00730099
         SR    10,10             CLEAR AN AREA FOR INCREMENT            00740099
         SR    4,4               CLEAR AN AREA FOR DIVISION             00750099
         SR    5,5               CLEAR AN AREA FOR TOTAL                00760099
         SR    6,6               CLEAR AN AREA FOR RECCOUNT             00770099
*                                                                       00780099
         XREAD BUFFER,80         READ INTO BUFFER                       00790000
*                                                                       00800000
READF    BL    ENDREADF          BREAK ALL RECORDS READ                 00810053
* READ RECORDS                                                          00820099
         XDECI 2,BUFFER          CONVERT VALUE 1                        00830007
         AR    12,2              ADD VALUES TOGETHER                    00840099
         ST    12,TOTAL          STORE TOTAL                            00850099
* COUNT RECORDS                                                         00860099
         LA    10,1(0,10)        INCREMENT RECORD COUNT                 00870099
* COMPARE VALUES                                                        00880099
         LTR   2,2               TEST VALUES                            00890099
         BH    POS               BRANCH TO POSITIVE IF POS              00900099
         BL    NEG               BRANCH TO NEGATIVE IF NEG              00910099
         BZ    ZERO              BRANCH TO ZERO IF ZERO                 00920099
* POSITIVE BRANCH                                                       00930079
POS      MVC   STATUS(8),PO      MOVE 'POSITIVE' TO OUT                 00940099
         L     11,PI             LOAD POSITIVE COUNT                    00950086
         LA    11,1(0,11)        INCREMENT                              00960075
         ST    11,PI             STORE                                  00970075
         C     2,CURRP           COMPARE POSITIVE VALS                  00980099
         BH    HIGHER            BRANCH ON GREATER THAN                 00990099
         BNH   ENDP              BRANCH OTHERWISE                       01000099
HIGHER   XDECO 2,HIGH            STORE                                  01010099
         ST    2,CURRP           STORE TEMP VALUE                       01020099
ENDP     B     OVER              END PROCESS                            01040099
* NEGATIVE BRANCH                                                       01050079
NEG      MVC   STATUS(8),NE      STORE 'NEGATIVE'                       01060058
         L     11,NI             LOAD NEGATIVE COUNT                    01070086
         LA    11,1(0,11)        INCREMENT                              01080075
         ST    11,NI             STORE                                  01090075
         C     2,CURRN           COMPARE NEGATIVE VALS                  01100099
         BL    LOWER             BRANCH ON LESS THAN                    01110099
         BNL   ENDN              BRANCH ON NOT LESS THAN                01120099
LOWER    XDECO 2,LOW             STORE LOWEST VAL                       01130099
         ST    2,CURRN           STORE TEMP VALUE                       01140099
ENDN     B     OVER              END PROCESS                            01160099
* ZERO BRANCH                                                           01170079
ZERO     MVC   STATUS(8),ZE      MOVE 'ZERO' TO OUTPUT                  01180099
         L     11,ZI             LOAD ZERO COUNT                        01190085
         LA    11,1(0,11)        INCREMENT                              01200086
         ST    11,ZI             STORE                                  01210075
ENDZ     B     OVER              STORE 'ZERO'                           01220080
* RETURN TO PROCESS                                                     01230079
OVER     XDECO 2,VAL1            CONVERT VAL 1 TO CHAR FORM             01240054
         C     10,=F'1'          SEE IF THIS IS RECORD ONE              01241099
         BZ    ISONE             IF IT IS, DO RECORD ONE                01242099
         BNZ   ISNT              ELSE, DO RECORDS                       01242199
* IS FIRST RECORD                                                       01242299
ISONE    MVC   CCB(1),=C'0'      STORE DOUBLE SPACE                     01243099
ENDO     B     DONE              FINISH                                 01244099
* IS NOT FIRST RECORD                                                   01244199
ISNT     MVC   CCB(1),=C' '      STORE SINGLE SPACE                     01245099
DONE     XPRNT CCB,133           PRINT READ RECORD                      01250099
*                                                                       01251099
         XREAD BUFFER,80         READ INTO BUFFER                       01260000
         B     READF             LOOP BACK TO READF FOR ANY CODE        01270029
ENDREADF XPRNT CCC,133           PRINT HEADER2                          01280077
* CALC AVERAGE                                                          01290099
         SR    4,4               CLEAR FOR DIVISION                     01300099
         SR    5,5               CLEAR FOR DIVISION                     01310099
         SR    6,6               CLEAR FOR DIVISION                     01320099
*                                                                       01321099
         LR    5,12              LOAD TOTAL                             01330099
         LR    6,10              LOAD RECORD COUNT                      01340099
         DR    4,6               DIVIDE TOTAL BY COUNT                  01350099
         XDECO 5,AVG             STORE AVERAGE                          01360099
* END CALC AVERAGE                                                      01370099
         L     11,ZI             LOAD NUMBER OF ZEROS                   01380089
         XDECO 11,ZEROS          STORE NUMBER OF ZEROS AS CHAR          01390089
*                                                                       01400099
         L     11,PI             LOAD NUMBER OF POSITIVES               01410095
         XDECO 11,POSTS          STORE NUMBER OF POS AS CHAR            01420094
*                                                                       01430099
         L     11,NI             LOAD NUMBER OF NEGATIVES               01440099
         XDECO 11,NEGS           STORE NUMBER OF NEGS AS CHAR           01450099
* PRINT VALUES                                                          01460099
         XPRNT CCD,133           PRINT ZERO NUMBER COUNT                01470094
         XPRNT CCE,133           PRINT POSITIVE NUMBER COUNT            01480094
         XPRNT CCF,133           PRINT NEGATIVE NUMBER COUNT            01490099
         XPRNT CCG,133           PRINT AVERAGE VALUE                    01500099
         XPRNT CCH,133           PRINT HIGHEST VALUE                    01510099
         XPRNT CCI,133           PRINT LOWEST VALUE                     01520099
*                                                                       01530099
         BR    14                                                       01540099
*                                                                       01550000
         LTORG                                                          01560000
BUFFER   DC    80C'%'            CREATES BUFFER AND FILLS               01570000
*                                                                       01580000
PI       DC    F'0'              COUNT OF POSITIVE NUMBERS              01590085
NI       DC    F'0'              COUNT OF NEGATIVE NUMBERS              01600085
ZI       DC    F'0'              COUNT OF ZEROS                         01610085
CURRP    DC    F'0'              CURRENT POSITIVE VALUE                 01620099
CURRN    DC    F'0'              CURRENT NEGATIVE VALUE                 01630099
TOTAL    DC    F'0'              TOTAL                                  01640099
*                                                                       01650065
INC      DC    F'0'              A PLACE TO HOLD RECORD COUNT           01660009
*                                                                       01670009
CCB      DC    C' '              CARRIAGE CONTROL                       01680099
         DC     6C' '            SPACES                                 01690022
         DC    C'NUMBER PROCESSED:'                                     01700020
VAL1     DS    CL12              STORAGE FOR VAL 2                      01710005
         DC    C' '              SPACE                                  01720035
STATUS   DS    8C' '             VALUE OF COMPARISON                    01730044
         DC     99C' '           SPACES                                 01740044
*                                                                       01750000
PO       DC    C'POSITIVE'                                              01760027
NE       DC    C'NEGATIVE'                                              01770027
ZE       DC    C'ZERO    '                                              01780064
*                                                                       01790025
CCA      DC    C'0'              CARRIAGE CONTROL                       01800076
         DC    4C' '             SPACES                                 01810076
HEADER1  DC    C'**** NUMBER REPORT ****'                               01820019
         DC    106C' '           SPACES                                 01830076
*                                                                       01840076
CCC      DC    C'0'              CARRIAGE CONTROL                       01850076
         DC    4C' '             SPACES                                 01860076
HEADER2  DC    C'**** TOTALS ****'                                      01870076
         DC    113C' '           SPACES                                 01880076
*                                CURRENT ISSUE, CALCULATING COUNTS OF   01890078
CCD      DC    C'0'              NEGATIVE,POSITIVE AND ZEROS            01900099
         DC    6C' '             SPACES                                 01910092
         DC    C'NUMBER OF ZEROS:'                                      01920089
         DC    3C' '             SPACES                                 01930098
ZEROS    DC    12C' '            TOTAL NUMBER OF ZEROS                  01940089
         DC    94C' '            SPACES                                 01950098
*                                CURRENT ISSUE, CALCULATING COUNTS OF   01960094
CCE      DC    C' '              NEGATIVE,POSITIVE AND ZEROS            01970094
         DC    6C' '             SPACES                                 01980094
         DC    C'NUMBER OF POSITIVE:'                                   01990094
POSTS    DC    12C' '            TOTAL NUMBER OF ZEROS                  02000094
         DC    95C' '            SPACES                                 02010097
*                                                                       02020078
CCF      DC    C' '              NEGATIVE,POSITIVE AND ZEROS            02030099
         DC    6C' '             SPACES                                 02040099
         DC    C'NUMBER OF NEGATIVE:'                                   02050099
NEGS     DC    12C' '            TOTAL NUMBER OF ZEROS                  02060099
         DC    95C' '            SPACES                                 02070099
*                                                                       02080099
CCG      DC    C' '              CARRIAGE CONTROL                       02090099
         DC    17C' '            SPACES                                 02100099
         DC    C'AVERAGE:'                                              02110099
AVG      DC    12C' '            AVERAGE VALUE                          02120099
         DC    95C' '            SPACES                                 02130099
*                                                                       02140099
CCH      DC    C' '              CARRIAGE CONTROL                       02150099
         DC    17C' '            SPACES                                 02160099
         DC    C'HIGHEST:'                                              02170099
HIGH     DC    12C' '            HIGHEST VALUE                          02180099
         DC    95C' '            SPACES                                 02190099
*                                                                       02200099
CCI      DC    C' '              CARRIAGE CONTROL                       02210099
         DC    17C' '            SPACES                                 02220099
         DC    C'LOWEST:'                                               02230099
         DC    C' '                                                     02240099
LOW      DC    12C' '            LOWEST VALUE                           02250099
         DC    95C' '            SPACES                                 02260099
*                                                                       02270099
         END   ASSIGN7                                                  02280000
/*                                                                      02290000
//                                                                      02300000
