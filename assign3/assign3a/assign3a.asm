//KC03H99A JOB ,'ANDREW SLADE',MSGCLASS=H                               00010004
//JSTEP01 EXEC PGM=ASSIST                                               00020004
//STEPLIB DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR                        00030004
//SYSPRINT DD SYSOUT=*                                                  00040004
//SYSIN    DD *                                                         00050004
***************************************************                     00060004
* ASSIGNMENT 3 - BASIC ASSEMBLY PROGRAM           *                     00070004
*                                                 *                     00080004
* DATE DUE: 09/15/2017                            *                     00090013
***************************************************                     00100013
*PROCEDURE                                        *                     00110016
*   *                                             *                     00120013
*1  LOAD 15 INTO REGISTER 2                       *                     00121013
*2  LOAD 7  INTO REGISTER 3                       *                     00122013
*3  SUBTRACT REGISTER 3 FROM R2                   *                     00123013
*4  DUMP REGISTERS                                *                     00124013
*5  DEFINE NUM1 AS A FULLWORD OF 15               *                     00126014
*6  DEFINE NUM2 AS A FULLWORD OF 7                *                     00127014
*7  BREAK TO OS                                   *                     00128014
***************************************************                     00130004
ASSIGN3A CSECT                                                          00140012
*                                                                       00141016
         USING ASSIGN3A,15      ESTABLISH ADDRESSABILITY                00150012
*                                                                       00160016
         L     2,NUM1           LOAD R2 WITH 15                         00180018
         L     3,NUM2           LOAD R3 WITH 7                          00190018
         SR    2,3              SUBTRACT R3 FROM R2                     00200011
         XDUMP                                                          00210020
*                                                                       00211016
         BR    14               RETURN TO CALLER                        00220011
*                                                                       00221014
         LTORG                                                          00222015
NUM1     DC    F'15'            DEFINE FULLWORD OF 15                   00230011
NUM2     DC    F'7'             DEFINE FULLWORD OF 7                    00240011
*                                                                       00241016
         END   ASSIGN3A                                                 00250012
/*                                                                      00260004
//                                                                      00270004
