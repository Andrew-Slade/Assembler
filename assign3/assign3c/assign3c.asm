//KC03H99A JOB ,'ANDREW SLADE',MSGCLASS=H                               00010004
//JSTEP01 EXEC PGM=ASSIST                                               00020004
//STEPLIB DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR                        00030004
//SYSPRINT DD SYSOUT=*                                                  00040004
//SYSIN    DD *                                                         00050004
***************************************************                     00060004
* ASSIGNMENT 3 - BASIC ASSEMBLY PROGRAM           *                     00070004
*                                                 *                     00080004
* DATE DUE: 09/15/2017                            *                     00090033
***************************************************                     00100044
* PROCEDURE                                       *                     00110044
*  *                                              *                     00120033
*1 LOAD VALUE 15 FROM MEMORY INTO R2              *                     00121033
*2 LOAD VALUE 07 FROM MEMORY INTO R3              *                     00122033
*3 SUBTRACT R3 FROM R2                            *                     00123033
*4 STORE VALUE INTO A FULLWORD IN MEMORY          *                     00124033
*5 DUMP REGISTERS                                 *                     00125033
*6 DUMP STORAGE                                   *                     00126033
*7 DEFINE A FULLWORD OF 15                        *                     00128034
*8 DEFINE A FULLWORD OF 07                        *                     00129034
*9 DEFINE A FULLWORD OF UNUSED STORAGE IN MEMORY  *                     00129134
*10 BREAK TO OS                                   *                     00129234
***************************************************                     00130004
ASSIGN3C CSECT                                                          00140023
         USING ASSIGN3C,15      ESTABLISH ADDRESSABILITY                00150023
*                                                                       00160044
         L     2,32(0,15)       LOAD 15 INTO R2                         00180044
         L     3,36(0,15)       LOAD 07 INTO R3                         00190044
         SR    2,3              SUBTRACT R3 FROM R2                     00200011
         ST    2,40(0,15)       STORE RESULT                            00201041
*                                                                       00202034
         XDUMP                                                          00210011
         XDUMP ANSWER,4                                                 00211044
*                                                                       00212044
         BR    14               RETURN TO CALLER                        00220011
*                                                                       00221034
         LTORG                                                          00222035
NUM1     DC    F'15'            DEFINE FULLWORD OF 15                   00230022
NUM2     DC    F'7'             DEFINE FULLWORD OF 7                    00240011
ANSWER   DS    F                DEFINE FULLWORD                         00241019
*                                                                       00242035
         END   ASSIGN3C                                                 00250034
/*                                                                      00260004
//                                                                      00270004
