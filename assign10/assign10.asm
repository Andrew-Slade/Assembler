//KC03H99A JOB ,'ANDREW SLADE',MSGCLASS=H                               00010000
//JSTEP01 EXEC PGM=ASSIST                                               00020000
//STEPLIB DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR                        00030000
//SYSPRINT DD SYSOUT=*                                                  00040000
//FT05F001 DD DSN=KC02322.CSCI360.DATAFA17(DATA10),DISP=SHR             00050000
//SYSIN    DD *                                                         00060000
***************************************************                     00070000
* ASSIGNMENT 10- PACKED DECIMAL                   *                     00080000
* ANDREW SLADE                                    *                     00090000
* DATE DUE: 12/13/2017                            *                     00100000
***************************************************                     00110000
*1 PRINT HEADER                                   *                     00140000
*   *                                             *                     00150000
*2 READ RECORD                                    *                     00160099
*   *                                             *                     00170000
*3 INCREMENT RECORD COUNT                         *                     00180099
*   *                                             *                     00190000
*4 ENTER SUBROUTINE                               *                     00200099
*   *                                             *                     00210000
*5 GET ALL INFORMATION                            *                     00220099
*   *                                             *                     00230000
*6 DO VARIOUS CALCULATIONS                        *                     00240099
*   *                                             *                     00250000
*7 FORMAT OUTPUT FOR EACH RECORD                  *                     00260099
*   *                                             *                     00270000
*8 CALCULATE RECORD COUNT AND FORMAT OUTPUT       *                     00280099
*   *                                             *                     00290000
*9 CALCULATE AVERGAGE VALUE AND FORMAT OUTPUT     *                     00300099
*   *                                             *                     00310000
*10 CALCULATE TOTAL  VALUE AND FORMAT OUTPUT      *                     00320099
*   *                                             *                     00330000
*11 PRINT REPORTS                                 *                     00340000
*   *                                             *                     00350000
*FUNCTION: READS RECORDS AND GETS VARIOUS METADATA*                     00360099
*   * FOR EACH RECORD THAT DETAILS ACCOUNT INFO.  *                     00370099
*   *                                             *                     00400000
*INPUT: A DATAFILE FROM ANOTHER USER              *                     00410000
*  *                                              *                     00420000
*ENTRY CONDS: NONE                                *                     00430000
*  *                                              *                     00440000
*EXIT CONDS: NONE                                 *                     00450000
*  *                                              *                     00460000
*NOTES: NONE                                      *                     00470000
*  *                                              *                     00480000
*OUTPUT: A REPORT SHOWING META DATA FOR THE DATA  *                     00490000
*  *     SET.                                     *                     00500000
*  *                                              *                     00510000
*REGISTER USAGE: R2: BUFFER                       *                     00530099
*  *             R3: TOTAL BALANCE                *                     00540037
*  *             R11: RECORD COUNT                *                     00620001
*  *             R12: STD LINKAGE                 *                     00630000
*  *             R13: STD LINKAGE                 *                     00640000
*  *             R14: STD LINKAGE                 *                     00650000
*  *             R15: STD LINKAGE                 *                     00660000
***************************************************                     00670000
ASSIGN10 CSECT                                                          00680000
* STD LINKAGE                                                           00690000
         STM   14,12,12(13)      STORE REGISTERS                        00700000
         LR    12,15             ADDRESS OF ASSIGN9                     00710000
         USING ASSIGN10,12       ESTABLISH ADDRESSABILITY               00720003
         LA    14,SAVER          POINT TO SAVE AREA                     00730000
         ST    13,4(,14)         BACKWARD POINTR                        00740000
         ST    14,8(,13)         FORWARD POINTER                        00750000
         LR    13,14             POINT TO SAVE AREA                     00760000
* END STD LINKAGE                                                       00770000
*                                                                       00780000
         XPRNT CCH0,133          PRINT OVERHEADER                       00790099
         XPRNT CCH1,133          PRINT HEADER                           00800099
         XPRNT CCH5,133          PRINT DIVIDER                          00810099
         XREAD BUFFER,80         READ INTO BUFFER                       00820099
READF    BL    ENDREADF          BRANCH WHEN DONE READING               00830000
*                                                                       00840001
         AP    COUNT(2),=PL1'1'  INCREMENT RECORD COUNT                 00850099
*                                                                       00860001
         LA    1,PARMLIST        POINT TO PARAMETER LIST                00870001
         L     15,=V(PROCACCT)   LOAD ADDRESS OF SUBROUTINE             00880004
         BALR  14,15             BRANCH AND LINK TO SUBROUTINE          00890029
*                                                                       00900001
         XREAD BUFFER,80         READ INTO BUFFER                       00910099
*                                                                       00920099
         B     READF             LOOP BACK TO READF FOR ANY CODE        00930000
*                                                                       00940001
ENDREADF LA    1,RECCNT                                                 00951099
         MVC   RECCNT(4),=X'40202120'                                   00952099
         ED    RECCNT(4),COUNT   TRANSFER RECORD COUNT                  00960099
         XPRNT CCH2,133          PRINT RECORD COUNT                     00970099
* AVERAGE BALANCE                                                       00980099
         SRP   SUMBAL(9),3,5       MULT BY 10000                        00990099
         ZAP   AVGBAL(9),SUMBAL(9) PUT SUM INTO CALC AREA               01000099
         DP    AVGBAL(9),COUNT(2)  DIVIDE TOTAL                         01010099
         LA    1,AVG+9             MARK DOLLAR SIGN PLACE               01020099
         MVC   AVG(15),=X'404020206B2020206B2020204B2020'               01030099
         EDMK  AVG(15),AVGBAL      MOVE AND MARK                        01040099
         BCTR  1,0                 GET READY FOR DOLLAR SIGN            01050099
         MVI   0(1),C'$'           DOLLAR SIGN                          01060099
* TOTAL BALANCE                                                         01070099
         SRP   SUMBAL(9),3,5     PRINT SECOND HEADER                    01080099
         LA    1,TOTAL+12        MAKE SURE ZEROS ARE OKAY               01090099
         MVC   TOTAL(16),=X'40402020206B2020206B2021204B2020'           01100099
         EDMK  TOTAL(16),SUMBAL  MOVE AND MARK                          01110099
         BCTR  1,0                DECREMENT TO DOLLAR SIGN AREA         01120099
         MVI   0(1),C'$'          MOVE DOLLAR SIGN                      01130099
         XPRNT CCH3,133           PRINT TOTAL                           01140099
         XPRNT CCH4,133           PRINT AVERAGE                         01150099
*                                                                       01160000
* STD LINKAGE                                                           01170000
         L     13,4(,13)         LOAD SAVE AREA                         01180007
         LM    14,12,12(13)      LOAD ALL REGISTERS BACK                01190000
         BR    14                BREAK TO OS                            01200000
* END STD LINKAGE                                                       01210000
         LTORG                                                          01220000
SAVER    DS    18F               SAVE AREA FOR REGISTERS                01230099
BUFFER   DC    80C' '            BUFFER FOR READING                     01240099
COUNT    DC    PL2'0'            RECORD COUNT                           01250099
NUMOUT   DC    CL13'%'           CHARACTER ACTBAL                       01260081
SUMBAL   DC    PL9'0'            SUM OF ALL BALANCE                     01270099
AVGBAL   DC    PL9'0'            AVERAGE BALANCE                        01280099
*                                                                       01290018
*****                                                                   01300099
CCH0     DC    C'1'              HEADER                                 01310099
         DC    51C' '                                                   01320099
         DC    C'ACCOUNT BALANCES DAILY REPORT'                         01330099
         DC    52C' '                                                   01340099
*****                                                                   01350099
CCH5     DC    C'0'              CARRIAGE CONTROL                       01360099
         DC    C' '                                                     01370099
         DC    C'----------------------'                                01380099
         DC     6C' '                                                   01390099
         DC    C'----------------' ORIGINAL BALANCE                     01400099
         DC    3C' '                                                    01410099
         DC    C'---------------'  INTEREST AMOUNT                      01420099
         DC    3C' '                                                    01430099
         DC    C'----------'     FEES                                   01440099
         DC    3C' '                                                    01450099
         DC    C'-------------'  FINAL BALANCE                          01460099
         DC    45C' '                                                   01470099
*****                                                                   01480099
CCH1     DC    C'0'              CARRIAGE CONTROL                       01490099
         DC    C' '                                                     01500099
NAME     DC    C'NAME'           ACCOUNT NAME                           01510099
         DC    24C' '                                                   01520099
ORIG     DC    C'ACCOUNT BALANCE'  ORIGINAL BALANCE                     01530099
         DC    3C' '                                                    01540099
INTEREST DC    C'INTEREST AMOUNT'  INTEREST AMOUNT                      01550099
         DC    7C' '                                                    01560099
FEES     DC    C'FEE'            FEES                                   01570099
         DC    5C' '                                                    01580099
OBALANCE DC    C'NEW ACCOUNT BALANCE'  BALANCE                          01590099
         DC    39C' '                                                   01600099
*****                                                                   01610099
CCH4     DC    C' '           HEADER 4                                  01620099
         DC    28C' '                                                   01630099
         DC    C'AVERAGE AMOUNT:'                                       01640099
         DC    15C' '                                                   01650099
AVG      DC    15C' '                                                   01660099
         DC    60C' '                                                   01670099
*****                                                                   01680099
CCH3     DC    C' '           HEADER 2                                  01690099
         DC    28C' '                                                   01700099
         DC    C'NEW BALANCE SUM:'                                      01710099
         DC    13C' '                                                   01720099
TOTAL    DC    16C' '         SUM OF ALL VALUES                         01730099
         DC    60C' '                                                   01740099
*****                                                                   01750099
CCH2     DC    C'0'              HEADER 3                               01760099
         DC    28C' '                                                   01770099
         DC    C'NUMBER OF ACCOUNTS:'                                   01780099
         DC    20C' '                                                   01790099
RECCNT   DC    4C' '           RECORD COUNT                             01800099
         DC    62C' '                                                   01810099
*****                                                                   01820099
ACTBAL   DS    CL13              ACCOUNT AMOUNT                         01830018
*                                                                       01840018
PARMLIST DC    A(BUFFER)         PARAMETER LIST                         01850001
         DC    A(SUMBAL)         PARM LIST                              01860099
*                                                                       01870001
* EXTERNAL SUBROUTINE                                                   01880000
PROCACCT CSECT                                                          01890001
* STD LINKAGE                                                           01900000
         STM   14,12,12(13)      SAVE ALL REGS                          01910000
         LR    12,15             ADDRESS OF PROCNUM                     01920000
         USING PROCACCT,12       ADDRESSABILITY                         01930003
         LA    14,SAVEAR         STORAGE POINTER                        01940000
         ST    13,4(,14)         BACKWARD POINTER                       01950000
         ST    14,8(,13)         FORWARD POINTER                        01960000
         LR    13,14             POINT TO SAVE AREA                     01970000
* END STD LINKAGE                                                       01980000
         LM    2,3,0(1)          R2 <-  BUFFER                          01990009
*                                R3 <-  SUMBAL(SUM OF BALANCE)          02000099
*                                                                       02010009
         ZAP   FINBAL(9),=PL9'0'  CLEAR OUT FINAL BALANCE               02020099
         MVC   ACTNAME(25),0(2)   COPY NAME INTO OUTPUT FIELD           02030099
* NAME SAVED                                                            02040099
         PACK  PACTBAL(5),25(9,2) PACK ACCOUNT BALANCE                  02050099
*                                                                       02060099
         LA    1,OUTOBAL+10        MAKE SURE ZEROS ARE OKAY             02070099
         MVC   OUTOBAL(14),=X'4040206B2020206B2021204B2020'             02080099
         EDMK  OUTOBAL(14),PACTBAL MOVE AND MARK                        02090099
         BCTR  1,0                DECREMENT TO DOLLAR SIGN AREA         02100099
         MVI   0(1),C'$'          MOVE DOLLAR SIGN                      02110099
*                                                                       02120099
         CP    PACTBAL(5),=PL1'0'       COMPARE ACCOUNT BALANCE WITH 0  02130099
         BNL   POSBAL                   BRANCH TO POSITIVE BALANCE CALC 02140099
         MVC   FLAGF(1),=C'Y'           IS NEGATIVE                     02150099
         BCTR  1,0                      MARK AREA FOR NEGATIVE SIGN     02160099
         MVI   0(1),C'-'                IS NEGATIVE                     02170099
         ZAP   PFEE(3),=PL3'-35.00'     FEE FOR NEGATIVE BALANCE:35     02180099
         ZAP   PCINT(8),=PL1'0'         INTRATE IS ZERO ON NEGATIVE BAL 02190099
         B     PASTPOS                  BRANCH PAST POSITIVE STATEMENT  02200099
*                                                                       02210021
POSBAL   ZAP   PFEE(3),=PL1'0'     NO FEE ON A POSITIVE BALANCE         02220099
         MVC   FLAGF(1),=C'N'      IS NOT NEGATIVE                      02230099
         PACK  PINRATE(3),34(4,2)  PACK INTEREST RATE                   02240099
         ZAP   PCINT(8),PACTBAL(5) ADD ACT BAL TO CALC AREA             02250099
         MP    PCINT(8),PINRATE(3) MULTIPLY ACT BAL BY INTEREST         02260099
         SRP   PCINT(8),64-4,5     MULTIPLY BY 100 ROUND OFF            02270099
*                                                                       02280099
* NOW FOR THE FINAL BALANCE                                             02290099
PASTPOS  ZAP   FINBAL(9),PACTBAL(5) ADD ACCOUNT BALANCE                 02300099
         AP    FINBAL(9),PCINT(8)   ADD INTEREST                        02310099
         AP    FINBAL(9),PFEE(3)    ADD FEES                            02320099
         AP    0(9,3),FINBAL(9)     ADD TO TOTAL                        02330099
         SRP   FINBAL(9),7,5       ROUND FOR PRINTING                   02340099
         LA    1,BALANCE+11        MAKE SURE ZEROS ARE OKAY             02350099
         MVC   BALANCE(15),=X'404020206B2020206B2021204B2020'           02360099
         EDMK  BALANCE(15),FINBAL  MOVE AND MARK                        02370099
*                                                                       02380099
         BCTR  1,0                DECREMENT TO DOLLAR SIGN AREA         02390099
         MVI   0(1),C'$'          MOVE DOLLAR SIGN                      02400099
         CLI   FLAGF,C'Y'         CHECK FOR NEGATIVE                    02410099
         BL    FINISH             IF NOT NEGATIVE BRANCH                02420099
         BCTR  1,0                MARK NEGATIVE AREA                    02430099
         MVI   0(1),C'-'          PUSH SIGN                             02440099
         B     FINISH             BRANCH TO END                         02450099
* PRINT FEE AMOUNT                                                      02460099
FINISH   SRP   PFEE(3),1,5         ROUND UP FEES FOR PRINTING           02470099
         LA    1,FEEAMT+2          MARK SIG FIG                         02480099
         MVC   FEEAMT(6),=X'4021204B2020'                               02490099
         EDMK  FEEAMT(6),PFEE      PUSH FEE INTO PRINTLINE              02500099
         BCTR  1,0                 MARK DOLLAR SIGN AREA                02510099
         MVI   0(1),C'$'           MOVE DOLLAR SIGN                     02520099
* PRINT INTEREST AMOUNT                                                 02530099
         SRP   PCINT(8),6,5        ROUND UP INTEREST FOR PRINTING       02540099
         LA    1,OINTAMT+10        MAKE SURE ZEROS ARE OKAY             02550099
         MVC   OINTAMT(14),=X'4040206B2020206B2021204B202020'           02560099
         EDMK  OINTAMT(14),PCINT   MOVE AND MARK                        02570099
         BCTR  1,0                 MARK DOLLAR SIGN AREA                02580099
         MVI   0(1),C'$'           MOVE DOLLAR SIGN                     02590099
ENDPOS   XPRNT CC,133              PRINT INFO LINE                      02600099
*                                                                       02610021
*                                                                       02620009
* STD LINKAGE                                                           02630000
         L     13,4(,13)         ADDRESS OF CALL ROUTINE                02640000
         L     14,12(,13)        RESTORE CALLER REGISTER                02650000
         LM    0,12,20(13)       RESTORES CALLERS REGISTERS             02660000
         BR    14                GO BACK TO CALLER ROUTINE              02670000
* END STD LINKAGE                                                       02680000
         LTORG                                                          02690000
SAVEAR   DS    18F               SAVE AREA FOR REGISTERS                02700099
*                                                                       02710099
CC       DC    C'0'              CARRIAGE CONTROL                       02720099
         DC    C' '                                                     02730099
ACTNAME  DS    CL25              ACCOUNT NAME                           02740099
         DC    3C' '                                                    02750099
OUTOBAL  DS    CL14              ORIGINAL BALANCE                       02760099
         DC    5C' '                                                    02770099
OINTAMT  DS    CL14              INTEREST AMOUNT                        02780099
         DC    5C' '                                                    02790099
FEEAMT   DS    CL6               FEES                                   02800099
         DC    5C' '                                                    02810099
BALANCE  DS    CL15              FINAL BALANCE                          02820099
         DC    39C' '                                                   02830099
*                                                                       02840099
FLAGF    DC    C' '              FLAG FOR NEGATIVE                      02850099
PFEE     DC    PL3'0'            FEE AMOUNT                             02860099
PACTBAL  DC    PL5'0'            ACCOUNT BALANCE                        02870099
PINRATE  DC    PL3'0'            INTEREST RATE                          02880099
PCINT    DC    PL8'0'            CALC INTEREST                          02890099
PINAMT   DC    PL8'0'            FINAL INTEREST RATE                    02900099
FINBAL   DC    PL9'0'                                                   02910099
*                                                                       02920000
         END   ASSIGN10                                                 02930000
/*                                                                      02940000
//                                                                      02950000
