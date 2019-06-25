//KC03H99A JOB ,'ANDREW SLADE',MSGCLASS=H                               00010023
//JSTEP01 EXEC PGM=ASSIST                                               00020023
//STEPLIB DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR                        00030023
//SYSPRINT DD SYSOUT=*                                                  00040023
//FT05F001 DD DSN=KC02322.CSCI360.DATAFA17(DATA7),DISP=SHR              00050023
//SYSIN    DD *                                                         00060023
***************************************************                     00070023
* ASSIGNMENT 8- INTERNAL SUBROUTINES              *                     00080023
* ANDREW SLADE                                    *                     00090023
* DATE DUE: 11/29/2017                            *                     00100023
***************************************************                     00110023
*PROCEDURE                                        *                     00120023
*   *                                             *                     00130023
*1 PRINT HEADER                                   *                     00140023
*   *                                             *                     00150023
*2 CLEAR REGISTERS                                *                     00160023
*   *                                             *                     00170023
*3 READ RECORDS                                   *                     00180023
*   *                                             *                     00190023
*4 COMPUTE TOTAL                                  *                     00200023
*   *                                             *                     00210023
*5 COUNT RECORDS                                  *                     00220023
*   *                                             *                     00230023
*6 COMPARE RECORDS WITH ZERO                      *                     00240023
*   *                                             *                     00250023
*7 STORE HIGHEST OR LOWEST VALUE                  *                     00260023
*   *                                             *                     00270023
*8 COUNT NUMBER OF POSITIVE VALUES, NEGATIVE VAL- *                     00280023
*  UES, AND ZERO VALUES                           *                     00290023
*   *                                             *                     00300023
*9 CALCULATE AVERGAGE VALUE                       *                     00310023
*   *                                             *                     00320023
*10 STORE VALUES                                  *                     00330023
*   *                                             *                     00340023
*11 PRINT REPORTS                                 *                     00350023
*   *                                             *                     00360023
*FUNCTION: READ DATA SET, PROCESS DATA, OUTPUT    *                     00370023
*   *      META DATA AS WELL AS DATA SET WITH LAB-*                     00380023
*   *      ELS. COUNT NEGATIVE, POSITIVE, AND     *                     00390023
*   *      ZERO VALUES AS WELL AS STORING HIGHEST,*                     00400023
*   *      LOWEST AND AVERAGE VALUES.             *                     00410023
*INPUT: A DATAFILE FROM ANOTHER USER              *                     00420023
*  *                                              *                     00430023
*ENTRY CONDS: NONE                                *                     00440023
*  *                                              *                     00450023
*EXIT CONDS: NONE                                 *                     00460023
*  *                                              *                     00470023
*NOTES: NONE                                      *                     00480023
*  *                                              *                     00490023
*OUTPUT: A REPORT SHOWING META DATA FOR THE DATA  *                     00500023
*  *     SET.                                     *                     00510023
*  *                                              *                     00520023
*REGISTER USAGE: R2: READS IN RECORDS             *                     00530023
*  *             R3: HOLDS HIGHEST                *                     00531033
*  *             R4: USED IN DIVISION AS THE EVEN *                     00540023
*  *                 COMPONENT OF THE PAIR        *                     00550023
*  *             R5: HOLDS TOTAL IN DIVISION, IS  *                     00560023
*  *                 THE ODD COMPONENT OF THE PAIR*                     00570023
*  *             R6: HOLDS THE RECORD COUNT IN THE*                     00580023
*  *                 DIVISION PROCESS             *                     00590023
*  *             R8: USED FOR HOLDING BUFFER VAL  *                     00591033
*  *             R10: USED TO COUNT RECORDS       *                     00600023
*  *             R11: USED TO COUNT NEGATIVE, POS-*                     00610023
*  *                  ITIVE, AND ZERO VALUES      *                     00620023
*  *             R12: HOLDS TOTAL OF ALL VALUES   *                     00630023
*  *             R13: HOLDS LOWEST                *                     00631033
*  *             R14: HOLDS SYSTEM ADDRESS        *                     00640023
*  *             R15: HOLDS PROGRAM FOR ADDRESSAB-*                     00650023
*  *                   ILITY                      *                     00660023
***************************************************                     00670023
ASSIGN8  CSECT                                                          00680023
         USING ASSIGN8,15        ESTABLISH ADDRESSABILITY               00690023
*                                                                       00700023
         XPRNT CCA,133           PRINT HEADER #1                        00710023
*                                                                       00720023
         SR    12,12             CLEAR AN AREA FOR TOTAL                00730023
         SR    10,10             CLEAR AN AREA FOR INCREMENT            00740023
         SR    5,5               CLEAR AN AREA FOR TOTAL                00750023
         SR    6,6               CLEAR AN AREA FOR RECCOUNT             00760023
*                                                                       00770023
         XREAD BUFFER,80         READ INTO BUFFER                       00780023
READF    BL    ENDREADF                                                 00790023
* COUNT RECORDS                                                         00800023
         LA    10,1(0,10)        INCREMENT RECORD COUNT                 00810023
         ST    10,COUNT          STORE COUNT                            00820028
         C     10,=F'1'           COMPARE TO SEE IF FIRST REC           00821031
         BE    ISONE             SEE IF IS FIRST                        00822026
         BNE   ISNT              IF ISNT, SKIP                          00823026
ISONE    MVC   CCB(1),=C'0'      DOUBLE SPACE                           00824031
         B     CHANGED           BRANCH WHEN DONE                       00825032
ISNT     MVC   CCB(1),=C' '      SINGLE SPACE                           00826032
         B     CHANGED           BRANCH WHEN DONE                       00827032
CHANGED  XDECI 2,BUFFER          CONVERT                                00830032
         SR    11,11             CLEAR COUNTER                          00840023
         LTR   2,2               TEST VALUES                            00850023
         BH    POSC              BRANCH TO POSITIVE IF POS              00860023
         BL    NEGC              BRANCH TO NEGATIVE IF NEG              00870023
         BZ    ZEROC             BRANCH TO ZERO IF ZERO                 00880023
* POSITIVE BRANCH                                                       00890023
POSC     L     11,PI             LOAD POSITIVE COUNT                    00900023
         LA    11,1(0,11)        INCREMENT                              00910023
         ST    11,PI             STORE                                  00920023
         B     OVERC             END PROCESS                            00930023
* NEGATIVE BRANCH                                                       00940023
NEGC     L     11,NI             LOAD NEGATIVE COUNT                    00950023
         LA    11,1(0,11)        INCREMENT                              00960023
         ST    11,NI             STORE                                  00970023
         B     OVERC             END PROCESS                            00980023
* ZERO BRANCH                                                           00990023
ZEROC    L     11,ZI             LOAD ZERO COUNT                        01000023
         LA    11,1(0,11)        INCREMENT                              01010023
         ST    11,ZI             STORE                                  01020023
OVERC    SR    11,11             CLEAR OUT A SPACE FOR USE              01030023
* SUBROUTINE                                                            01040023
         LA    1,PARLIST         POINT TO PARAMETER LIST                01050023
         BAL   11,PROCNUM        BRANCH TO SUBROUTINE PROCNUM           01060023
         L     3,HIGHEST         LOAD HIGHEST                           01070023
         L     13,LOWEST         LOAD LOWEST                            01080023
         XDECO 3,HIGH            STORE HIGH AS CHAR                     01090023
         XDECO 13,LOW            STORE LOW AS CHAR                      01100023
* END OF SUBROUTINE                                                     01110023
         XPRNT CCB,133           PRINT READ RECORD                      01120023
         B     READF             LOOP BACK TO READF FOR ANY CODE        01130023
ENDREADF XPRNT CCC,133           PRINT HEADER 2                         01140023
*                                                                       01150023
* CALC AVERAGE                                                          01160023
         SR    4,4               CLEAR FOR DIVISION                     01170023
         SR    5,5               CLEAR FOR DIVISION                     01180023
         SR    6,6               CLEAR FOR DIVISION                     01190023
*                                                                       01200023
         L     5,TOTAL           LOAD TOTAL                             01210023
         L     6,COUNT           LOAD RECORD COUNT                      01220023
         DR    4,6               DIVIDE TOTAL BY COUNT                  01230023
         XDECO 5,AVG             STORE AVERAGE                          01240023
* END CALC AVERAGE                                                      01250023
         L     11,ZI             LOAD NUMBER OF ZEROS                   01260023
         XDECO 11,ZEROS          STORE NUMBER OF ZEROS AS CHAR          01270023
*                                                                       01280023
         L     11,PI             LOAD NUMBER OF POSITIVES               01290023
         XDECO 11,POSTS          STORE NUMBER OF POS AS CHAR            01300023
*                                                                       01310023
         L     11,NI             LOAD NUMBER OF NEGATIVES               01320023
         XDECO 11,NEGS           STORE NUMBER OF NEGS AS CHAR           01330023
* PRINT VALUES                                                          01340023
         XPRNT CCD,133           PRINT ZERO NUMBER COUNT                01350023
         XPRNT CCE,133           PRINT POSITIVE NUMBER COUNT            01360023
         XPRNT CCF,133           PRINT NEGATIVE NUMBER COUNT            01370023
         XPRNT CCG,133           PRINT AVERAGE VALUE                    01380023
         XPRNT CCH,133           PRINT HIGHEST VALUE                    01390023
         XPRNT CCI,133           PRINT LOWEST VALUE                     01400023
*                                                                       01410023
         BR    14                                                       01420023
*                                                                       01430023
         LTORG                                                          01440023
BUFFER   DC    80C'%'            CREATES BUFFER AND FILLS               01450023
*                                                                       01460023
PI       DC    F'0'              COUNT OF POSITIVE NUMBERS              01470023
NI       DC    F'0'              COUNT OF NEGATIVE NUMBERS              01480023
ZI       DC    F'0'              COUNT OF ZEROS                         01490023
HIGHEST  DC    F'0'              CURRENT HIGH                           01500023
LOWEST   DC    F'0'              CURRENT LOW                            01510023
TOTAL    DC    F'0'              TOTAL                                  01520023
COUNT    DC    F'0'              RECORD COUNT                           01530023
*                                                                       01540023
INC      DC    F'0'              A PLACE TO HOLD RECORD COUNT           01550023
*                                                                       01560023
CCB      DC    C' '              CARRIAGE CONTROL                       01570023
         DC     6C' '            SPACES                                 01580023
         DC    C'NUMBER PROCESSED:'                                     01590023
VAL1     DS    CL12              STORAGE FOR VAL 2                      01600023
         DC    C' '              SPACE                                  01610023
STATUS   DS    8C' '             VALUE OF COMPARISON                    01620023
         DC     99C' '           SPACES                                 01630023
*                                                                       01640023
PO       DC    C'POSITIVE'                                              01650023
NE       DC    C'NEGATIVE'                                              01660023
ZE       DC    C'ZERO    '                                              01670023
*                                                                       01680023
CCA      DC    C'1'              CARRIAGE CONTROL                       01690023
         DC    4C' '             SPACES                                 01700023
HEADER1  DC    C'**** NUMBER REPORT ****'                               01710023
         DC    106C' '           SPACES                                 01720023
*                                                                       01730023
CCC      DC    C'0'              CARRIAGE CONTROL                       01740023
         DC    4C' '             SPACES                                 01750023
HEADER2  DC    C'**** TOTALS ****'                                      01760023
         DC    113C' '           SPACES                                 01770023
*                                CURRENT ISSUE, CALCULATING COUNTS OF   01780023
CCD      DC    C'0'              NEGATIVE,POSITIVE AND ZEROS            01790023
         DC    6C' '             SPACES                                 01800023
         DC    C'NUMBER OF ZEROS:'                                      01810023
         DC    3C' '             SPACES                                 01820023
ZEROS    DC    12C' '            TOTAL NUMBER OF ZEROS                  01830023
         DC    94C' '            SPACES                                 01840023
*                                                                       01850023
CCE      DC    C' '              CC                                     01860023
         DC    6C' '             SPACES                                 01870023
         DC    C'NUMBER OF POSITIVE:'                                   01880023
POSTS    DC    12C' '            TOTAL NUMBER OF ZEROS                  01890023
         DC    95C' '            SPACES                                 01900023
*                                                                       01910023
CCF      DC    C' '              CC                                     01920023
         DC    6C' '             SPACES                                 01930023
         DC    C'NUMBER OF NEGATIVE:'                                   01940023
NEGS     DC    12C' '            TOTAL NUMBER OF ZEROS                  01950023
         DC    95C' '            SPACES                                 01960023
*                                                                       01970023
CCG      DC    C' '              CARRIAGE CONTROL                       01980023
         DC    17C' '            SPACES                                 01990023
         DC    C'AVERAGE:'                                              02000023
AVG      DC    12C' '            AVERAGE VALUE                          02010023
         DC    95C' '            SPACES                                 02020023
*                                                                       02030023
CCH      DC    C' '              CARRIAGE CONTROL                       02040023
         DC    17C' '            SPACES                                 02050023
         DC    C'HIGHEST:'                                              02060023
HIGH     DC    12C' '            HIGHEST VALUE                          02070023
         DC    95C' '            SPACES                                 02080023
*                                                                       02090023
CCI      DC    C' '              CARRIAGE CONTROL                       02100023
         DC    17C' '            SPACES                                 02110023
         DC    C'LOWEST:'                                               02120023
         DC    C' '                                                     02130023
LOW      DC    12C' '            LOWEST VALUE                           02140023
         DC    95C' '            SPACES                                 02150023
*                                                                       02160023
PARLIST  DC    A(BUFFER)                                                02170023
         DC    A(VAL1)                                                  02180023
         DC    A(STATUS)                                                02190023
         DC    A(HIGHEST)                                               02200023
         DC    A(LOWEST)                                                02210023
         DC    A(TOTAL)                                                 02220023
*                                                                       02230023
* INTERNAL SUBROUTINE                                                   02240023
PROCNUM  STM   0,15,SAVE         SAVE CALLER'S REGS                     02250023
         LM    2,7,0(1)          R2 <-  BUFFER                          02260023
*                                R3 <-  VAL1(DECO AREA)                 02270023
*                                R4 <-  STATUS (NEG,POS,Z)              02280023
*                                R5 <-  HIGH (HIGHEST)                  02290023
*                                R6 <-  LOW (LOWEST)                    02300023
*                                R7 <-  TOTAL                           02310023
* READ RECORDS                                                          02320023
         L     12,0(,7)          LOAD TOTAL                             02330023
         SR    11,11             CLEAR FOR COUNTING                     02340023
         XDECI 8,0(,2)           CONVERT TO USABLE FORM                 02350023
         AR    12,8              ADD VALUES TOGETHER                    02360023
         ST    12,TOTAL          STORE TOTAL                            02370023
* COMPARE VALUES                                                        02380023
         LTR   8,8               TEST VALUES                            02390023
         BH    POS               BRANCH TO POSITIVE IF POS              02400023
         BL    NEG               BRANCH TO NEGATIVE IF NEG              02410023
         BZ    ZERO              BRANCH TO ZERO IF ZERO                 02420023
* POSITIVE BRANCH                                                       02430023
POS      MVC   0(8,4),=C'POSITIVE'                                      02440023
         C     8,0(,5)           COMPARE POSITIVE VALS                  02450023
         BH    HIGHER            BRANCH ON GREATER THAN                 02460023
         BNH   ENDP              BRANCH OTHERWISE                       02470023
HIGHER   ST    8,0(,5)           STORE HIGHER VALUE                     02480023
ENDP     B     OVER              END PROCESS                            02490023
* NEGATIVE BRANCH                                                       02500023
NEG      MVC   0(8,4),=C'NEGATIVE'                                      02510023
         C     8,0(,6)           COMPARE NEGATIVE VALS                  02520023
         BL    LOWER             BRANCH ON LESS THAN                    02530023
         BNL   ENDN              BRANCH ON NOT LESS THAN                02540023
LOWER    ST    8,0(,6)           STORE LOWER VALUE                      02550023
ENDN     B     OVER              END PROCESS                            02560023
* ZERO BRANCH                                                           02570023
ZERO     MVC   0(8,4),=C'ZERO    '                                      02580023
OVER     XDECO 8,0(,3)           STORE CHAR FORM OF REC                 02590023
         XREAD 0(,2),80          READ RECORD                            02600023
*                                                                       02610023
         LM    0,15,SAVE         RESTORE REGISTERS                      02620023
         BR    11                                                       02630023
*                                                                       02640023
         LTORG                                                          02650023
SAVE     DS    16F               STORAGE FOR REGISTERS                  02660023
*                                                                       02670023
         END   ASSIGN8                                                  02680023
/*                                                                      02690023
//                                                                      02700023
