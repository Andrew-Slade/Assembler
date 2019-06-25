//KC03H99A JOB ,'ANDREW SLADE',MSGCLASS=H                               00010031
//JSTEP01 EXEC PGM=ASSIST                                               00020031
//STEPLIB DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR                        00030031
//SYSPRINT DD SYSOUT=*                                                  00040031
//FT05F001 DD DSN=KC02322.CSCI360.DATAFA17(DATA7),DISP=SHR              00050031
//SYSIN    DD *                                                         00060031
***************************************************                     00070031
* ASSIGNMENT 9- EXTERNAL SUBROUTINES              *                     00080031
* ANDREW SLADE                                    *                     00090031
* DATE DUE: 12/04/2017                            *                     00100031
***************************************************                     00110031
*PROCEDURE                                        *                     00120031
*   *                                             *                     00130031
*1 PRINT HEADER                                   *                     00140031
*   *                                             *                     00150031
*2 CLEAR REGISTERS                                *                     00160031
*   *                                             *                     00170031
*3 READ RECORDS                                   *                     00180031
*   *                                             *                     00190031
*4 COMPUTE TOTAL                                  *                     00200031
*   *                                             *                     00210031
*5 COUNT RECORDS                                  *                     00220031
*   *                                             *                     00230031
*6 COMPARE RECORDS WITH ZERO                      *                     00240031
*   *                                             *                     00250031
*7 STORE HIGHEST OR LOWEST VALUE                  *                     00260031
*   *                                             *                     00270031
*8 COUNT NUMBER OF POSITIVE VALUES, NEGATIVE VAL- *                     00280031
*  UES, AND ZERO VALUES                           *                     00290031
*   *                                             *                     00300031
*9 CALCULATE AVERGAGE VALUE                       *                     00310031
*   *                                             *                     00320031
*10 STORE VALUES                                  *                     00330031
*   *                                             *                     00340031
*11 PRINT REPORTS                                 *                     00350031
*   *                                             *                     00360031
*FUNCTION: READ DATA SET, PROCESS DATA, OUTPUT    *                     00370031
*   *      META DATA AS WELL AS DATA SET WITH LAB-*                     00380031
*   *      ELS. COUNT NEGATIVE, POSITIVE, AND     *                     00390031
*   *      ZERO VALUES AS WELL AS STORING HIGHEST,*                     00400031
*   *      LOWEST AND AVERAGE VALUES.             *                     00410031
*INPUT: A DATAFILE FROM ANOTHER USER              *                     00420031
*  *                                              *                     00430031
*ENTRY CONDS: NONE                                *                     00440031
*  *                                              *                     00450031
*EXIT CONDS: NONE                                 *                     00460031
*  *                                              *                     00470031
*NOTES: NONE                                      *                     00480031
*  *                                              *                     00490031
*OUTPUT: A REPORT SHOWING META DATA FOR THE DATA  *                     00500031
*  *     SET.                                     *                     00510031
*  *                                              *                     00520031
*REGISTER USAGE: R2: READS IN RECORDS             *                     00530031
*  *             R3: HOLDS HIGHEST                *                     00540031
*  *             R4: USED IN DIVISION AS THE EVEN *                     00550031
*  *                 COMPONENT OF THE PAIR        *                     00560031
*  *             R5: HOLDS TOTAL IN DIVISION, IS  *                     00570031
*  *                 THE ODD COMPONENT OF THE PAIR*                     00580031
*  *             R6: HOLDS THE RECORD COUNT IN THE*                     00590031
*  *                 DIVISION PROCESS             *                     00600031
*  *             R8: USED FOR HOLDING BUFFER VAL  *                     00610031
*  *             R9:  HOLDS LOWEST, RECORDS       *                     00611032
*  *             R10: USED TO COUNT RECORDS       *                     00620031
*  *             R11: USED TO COUNT NEGATIVE, POS-*                     00630031
*  *                  ITIVE, AND ZERO VALUES      *                     00640031
*  *             R12: STD LINKAGE                 *                     00650032
*  *             R13: HOLDS LOWEST                *                     00660031
*  *             R14: HOLDS SYSTEM ADDRESS        *                     00670031
*  *             R15: HOLDS PROGRAM FOR ADDRESSAB-*                     00680031
*  *                   ILITY                      *                     00690031
***************************************************                     00700031
ASSIGN9  CSECT                                                          00710032
         STM   14,12,12(13)      STORE REGISTERS                        00711046
         LR    12,15             ADDRESS OF ASSIGN9                     00712046
         USING ASSIGN9,12        ESTABLISH ADDRESSABILITY               00720043
         LA    14,SAVER          POINT TO SAVE AREA                     00721046
         ST    13,4(,14)         BACKWARD POINTR                        00722046
         ST    14,8(,13)         FORWARD POINTER                        00723046
         LR    13,14             POINT TO SAVE AREA                     00724046
*                                                                       00730031
         XPRNT CCA,133           PRINT HEADER #1                        00740031
*                                                                       00750031
         SR    12,12             CLEAR AN AREA FOR TOTAL                00760031
         SR    10,10             CLEAR AN AREA FOR INCREMENT            00770031
         SR    5,5               CLEAR AN AREA FOR TOTAL                00780031
         SR    6,6               CLEAR AN AREA FOR RECCOUNT             00790031
*                                                                       00800031
         XREAD BUFFER,80         READ INTO BUFFER                       00810031
READF    BL    ENDREADF          BRANCH WHEN DONE READING               00820042
* COUNT RECORDS                                                         00830031
         LA    10,1(0,10)        INCREMENT RECORD COUNT                 00840031
         ST    10,COUNT          STORE COUNT                            00850031
         C     10,=F'1'           COMPARE TO SEE IF FIRST REC           00860031
         BE    ISONE             SEE IF IS FIRST                        00870031
         BNE   ISNT              IF ISNT, SKIP                          00880031
ISONE    MVC   CCB(1),=C'0'      DOUBLE SPACE                           00890031
         B     CHANGED           BRANCH WHEN DONE                       00900031
ISNT     MVC   CCB(1),=C' '      SINGLE SPACE                           00910031
         B     CHANGED           BRANCH WHEN DONE                       00920031
CHANGED  XDECI 2,BUFFER          CONVERT                                00930031
         SR    11,11             CLEAR COUNTER                          00940031
         LTR   2,2               TEST VALUES                            00950031
         BH    POSC              BRANCH TO POSITIVE IF POS              00960031
         BL    NEGC              BRANCH TO NEGATIVE IF NEG              00970031
         BZ    ZEROC             BRANCH TO ZERO IF ZERO                 00980031
* POSITIVE BRANCH                                                       00990031
POSC     L     11,PI             LOAD POSITIVE COUNT                    01000031
         LA    11,1(0,11)        INCREMENT                              01010031
         ST    11,PI             STORE                                  01020031
         B     OVERC             END PROCESS                            01030031
* NEGATIVE BRANCH                                                       01040031
NEGC     L     11,NI             LOAD NEGATIVE COUNT                    01050031
         LA    11,1(0,11)        INCREMENT                              01060031
         ST    11,NI             STORE                                  01070031
         B     OVERC             END PROCESS                            01080031
* ZERO BRANCH                                                           01090031
ZEROC    L     11,ZI             LOAD ZERO COUNT                        01100031
         LA    11,1(0,11)        INCREMENT                              01110031
         ST    11,ZI             STORE                                  01120031
OVERC    SR    11,11             CLEAR OUT A SPACE FOR USE              01130031
* SUBROUTINE                                                            01140031
         LA    1,PARLIST         POINT TO PARAMETER LIST                01150031
         L     15,=V(PROCNUM)    LOAD PROCNUM                           01151037
         BALR  14,15             BRANCH TO PROCNUM                      01152037
         L     3,HIGHEST         LOAD HIGHEST                           01170045
         L     7,LOWEST          LOAD LOWEST                            01180045
         XDECO 3,HIGH            STORE HIGH AS CHAR                     01190031
         XDECO 7,LOW             STORE LOW AS CHAR                      01200034
* END OF SUBROUTINE                                                     01210031
         XPRNT CCB,133           PRINT READ RECORD                      01220031
         B     READF             LOOP BACK TO READF FOR ANY CODE        01230031
ENDREADF XPRNT CCC,133           PRINT HEADER 2                         01240031
*                                                                       01250031
* CALC AVERAGE                                                          01260031
         SR    4,4               CLEAR FOR DIVISION                     01270031
         SR    5,5               CLEAR FOR DIVISION                     01280031
         SR    6,6               CLEAR FOR DIVISION                     01290031
*                                                                       01300031
         L     5,TOTAL           LOAD TOTAL                             01310031
         L     6,COUNT           LOAD RECORD COUNT                      01320031
         DR    4,6               DIVIDE TOTAL BY COUNT                  01330031
         XDECO 5,AVG             STORE AVERAGE                          01340031
* END CALC AVERAGE                                                      01350031
         L     11,ZI             LOAD NUMBER OF ZEROS                   01360031
         XDECO 11,ZEROS          STORE NUMBER OF ZEROS AS CHAR          01370031
*                                                                       01380031
         L     11,PI             LOAD NUMBER OF POSITIVES               01390031
         XDECO 11,POSTS          STORE NUMBER OF POS AS CHAR            01400031
*                                                                       01410031
         L     11,NI             LOAD NUMBER OF NEGATIVES               01420031
         XDECO 11,NEGS           STORE NUMBER OF NEGS AS CHAR           01430031
* PRINT VALUES                                                          01440031
         XPRNT CCD,133           PRINT ZERO NUMBER COUNT                01450031
         XPRNT CCE,133           PRINT POSITIVE NUMBER COUNT            01460031
         XPRNT CCF,133           PRINT NEGATIVE NUMBER COUNT            01470031
         XPRNT CCG,133           PRINT AVERAGE VALUE                    01480031
         XPRNT CCH,133           PRINT HIGHEST VALUE                    01490031
         XPRNT CCI,133           PRINT LOWEST VALUE                     01500031
*                                                                       01510031
         L     13,4(,13)         LOAD SAVE AREA                         01511046
         LM    14,12,12(13)      LOAD ALL REGISTERS BACK                01512046
         BR    14                BREAK TO OS                            01520046
*                                                                       01530031
         LTORG                                                          01540031
SAVER    DS    18F               SAVE AREA FOR ALL REGS                 01541046
BUFFER   DC    80C'%'            CREATES BUFFER AND FILLS               01550031
*                                                                       01560031
PI       DC    F'0'              COUNT OF POSITIVE NUMBERS              01570031
NI       DC    F'0'              COUNT OF NEGATIVE NUMBERS              01580031
ZI       DC    F'0'              COUNT OF ZEROS                         01590031
HIGHEST  DC    F'0'              CURRENT HIGH                           01600031
LOWEST   DC    F'0'              CURRENT LOW                            01610031
TOTAL    DC    F'0'              TOTAL                                  01620031
COUNT    DC    F'0'              RECORD COUNT                           01630031
*                                                                       01640031
INC      DC    F'0'              A PLACE TO HOLD RECORD COUNT           01650031
*                                                                       01660031
CCB      DC    C' '              CARRIAGE CONTROL                       01670031
         DC     6C' '            SPACES                                 01680031
         DC    C'NUMBER PROCESSED:'                                     01690031
VAL1     DS    CL12              STORAGE FOR VAL 2                      01700031
         DC    C' '              SPACE                                  01710031
STATUS   DS    8C' '             VALUE OF COMPARISON                    01720031
         DC    99C' '            SPACES                                 01730046
*                                                                       01780031
CCA      DC    C'1'              CARRIAGE CONTROL                       01790031
         DC    4C' '             SPACES                                 01800031
HEADER1  DC    C'**** NUMBER REPORT ****'                               01810031
         DC    106C' '           SPACES                                 01820031
*                                                                       01830031
CCC      DC    C'0'              CARRIAGE CONTROL                       01840031
         DC    4C' '             SPACES                                 01850031
HEADER2  DC    C'**** TOTALS ****'                                      01860031
         DC    113C' '           SPACES                                 01870031
*                                CURRENT ISSUE, CALCULATING COUNTS OF   01880031
CCD      DC    C'0'              NEGATIVE,POSITIVE AND ZEROS            01890031
         DC    6C' '             SPACES                                 01900031
         DC    C'NUMBER OF ZEROS:'                                      01910031
         DC    3C' '             SPACES                                 01920031
ZEROS    DC    12C' '            TOTAL NUMBER OF ZEROS                  01930031
         DC    94C' '            SPACES                                 01940031
*                                                                       01950031
CCE      DC    C' '              CC                                     01960031
         DC    6C' '             SPACES                                 01970031
         DC    C'NUMBER OF POSITIVE:'                                   01980031
POSTS    DC    12C' '            TOTAL NUMBER OF ZEROS                  01990031
         DC    95C' '            SPACES                                 02000031
*                                                                       02010031
CCF      DC    C' '              CC                                     02020031
         DC    6C' '             SPACES                                 02030031
         DC    C'NUMBER OF NEGATIVE:'                                   02040031
NEGS     DC    12C' '            TOTAL NUMBER OF ZEROS                  02050031
         DC    95C' '            SPACES                                 02060031
*                                                                       02070031
CCG      DC    C' '              CARRIAGE CONTROL                       02080031
         DC    17C' '            SPACES                                 02090031
         DC    C'AVERAGE:'                                              02100031
AVG      DC    12C' '            AVERAGE VALUE                          02110031
         DC    95C' '            SPACES                                 02120031
*                                                                       02130031
CCH      DC    C' '              CARRIAGE CONTROL                       02140031
         DC    17C' '            SPACES                                 02150031
         DC    C'HIGHEST:'                                              02160031
HIGH     DC    12C' '            HIGHEST VALUE                          02170031
         DC    95C' '            SPACES                                 02180031
*                                                                       02190031
CCI      DC    C' '              CARRIAGE CONTROL                       02200031
         DC    17C' '            SPACES                                 02210031
         DC    C'LOWEST:'                                               02220031
         DC    C' '                                                     02230031
LOW      DC    12C' '            LOWEST VALUE                           02240031
         DC    95C' '            SPACES                                 02250031
*                                                                       02260031
PARLIST  DC    A(BUFFER)                                                02270031
         DC    A(VAL1)                                                  02280031
         DC    A(STATUS)                                                02290031
         DC    A(HIGHEST)                                               02300031
         DC    A(LOWEST)                                                02310031
         DC    A(TOTAL)                                                 02320031
*                                                                       02330031
* EXTERNAL SUBROUTINE                                                   02340040
PROCNUM  CSECT                                                          02350036
         STM   14,12,12(13)      SAVE ALL REGS                          02351036
         LR    12,15             ADDRESS OF PROCNUM                     02352046
         USING PROCNUM,12        ADDRESSABILITY                         02353046
         LA    14,SAVEAR         STORAGE POINTER                        02354046
         ST    13,4(,14)         BACKWARD POINTER                       02355046
         ST    14,8(,13)         FORWARD POINTER                        02356046
         LR    13,14             POINT TO SAVE AREA                     02357046
         LM    2,7,0(1)          R2 <-  BUFFER                          02360031
*                                R3 <-  VAL1(DECO AREA)                 02370031
*                                R4 <-  STATUS (NEG,POS,Z)              02380031
*                                R5 <-  HIGH (HIGHEST)                  02390031
*                                R6 <-  LOW (LOWEST)                    02400031
*                                R7 <-  TOTAL                           02410031
* READ RECORDS                                                          02420031
         L     9,0(,7)           LOAD TOTAL                             02430034
         XDECI 8,0(,2)           CONVERT TO USABLE FORM                 02440031
         AR    9,8              ADD VALUES TOGETHER                     02450034
         ST    9,0(,7)           STORE TOTAL                            02460034
* COMPARE VALUES                                                        02470031
         LTR   8,8               TEST VALUES                            02480031
         BH    POS               BRANCH TO POSITIVE IF POS              02490031
         BL    NEG               BRANCH TO NEGATIVE IF NEG              02500031
         BZ    ZERO              BRANCH TO ZERO IF ZERO                 02510031
* POSITIVE BRANCH                                                       02520031
POS      MVC   0(8,4),=C'POSITIVE'                                      02530031
         C     8,0(,5)           COMPARE POSITIVE VALS                  02540031
         BH    HIGHER            BRANCH ON GREATER THAN                 02550031
         BNH   ENDP              BRANCH OTHERWISE                       02560031
HIGHER   ST    8,0(,5)           STORE HIGHER VALUE                     02570031
ENDP     B     OVER              END PROCESS                            02580031
* NEGATIVE BRANCH                                                       02590031
NEG      MVC   0(8,4),=C'NEGATIVE'                                      02600031
         C     8,0(,6)           COMPARE NEGATIVE VALS                  02610031
         BL    LOWER             BRANCH ON LESS THAN                    02620031
         BNL   ENDN              BRANCH ON NOT LESS THAN                02630031
LOWER    ST    8,0(,6)           STORE LOWER VALUE                      02640031
ENDN     B     OVER              END PROCESS                            02650031
* ZERO BRANCH                                                           02660031
ZERO     MVC   0(8,4),=C'ZERO    '                                      02670031
OVER     XDECO 8,0(,3)           STORE CHAR FORM OF REC                 02680031
         XREAD 0(,2),80          READ RECORD                            02690031
*                                                                       02700031
         L     13,4(,13)         ADDRESS OF CALL ROUTINE                02701046
         L     14,12(,13)        RESTORE CALLER REGISTER                02702046
         LM    0,12,20(13)       RESTORES CALLERS REGISTERS             02703046
         BR    14                GO BACK TO CALLER ROUTINE              02720046
*                                                                       02730031
         LTORG                                                          02740031
SAVEAR   DS    18F               SAVE AREA FOR REGISTERS                02741046
*                                                                       02760031
         END   ASSIGN9                                                  02770032
/*                                                                      02780031
//                                                                      02790031
