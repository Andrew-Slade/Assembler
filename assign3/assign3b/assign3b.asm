//KC03H99A JOB ,'ANDREW SLADE',MSGCLASS=H                               00010004
//JSTEP01 EXEC PGM=ASSIST                                               00020004
//STEPLIB DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR                        00030004
//SYSPRINT DD SYSOUT=*                                                  00040004
//SYSIN    DD *                                                         00050004
***************************************************                     00060004
* ASSIGNMENT 3 - BASIC ASSEMBLY PROGRAM           *                     00070004
*                                                 *                     00080004
* DATE DUE: 09/15/2017                            *                     00090024
*                                                 *                     00100024
*PROCEDURE                                       *                      00110028
*   *                                             *                     00111024
*1 LOAD 15 INTO REGISTER 2                        *                     00112024
*2 LOAD 7  INTO REGISTER 3                        *                     00113024
*3 SUBTRACT REGISTER 3 FROM R2                    *                     00114024
*4 STORE CONTENTS OF R2 INTO DEFINED MEMORY       *                     00115024
*5 DUMP REGISTERS                                 *                     00116024
*6 DUMP MEMORY                                    *                     00117024
*7 DEFINE A FULLWORD OF 15                        *                     00119025
*8 DEFINE A FULLWORD OF 7                         *                     00119125
*9 DEFINE A FULLWORD OF UNUSED MEMORY             *                     00120025
*10 BREAK TO OS                                   *                     00121025
***************************************************                     00130004
ASSIGN3B CSECT                                                          00140023
         USING ASSIGN3B,15      ESTABLISH ADDRESSABILITY                00150023
*                                                                       00160028
         L     2,NUM1           LOAD 15 INTO R2                         00180028
         L     3,NUM2           LOAD 07 INTO R3                         00190028
         SR    2,3              SUBTRACT R3 FROM R2                     00200011
         ST    2,ANSWER         STORE RESULT                            00201020
*                                                                       00202026
         XDUMP                                                          00210011
         XDUMP ANSWER,4                                                 00211028
*                                                                       00212025
         BR    14               RETURN TO CALLER                        00220011
*                                                                       00221026
         LTORG                                                          00222026
NUM1     DC    F'15'            DEFINE FULLWORD OF 15                   00230022
NUM2     DC    F'7'             DEFINE FULLWORD OF 7                    00240011
ANSWER   DS    F                DEFINE FULLWORD                         00241019
*                                                                       00242026
         END   ASSIGN3B                                                 00250025
/*                                                                      00260004
//                                                                      00270004
