//KC03nnnA JOB ,'your name here',MSGCLASS=H
//JSTEP01  EXEC PGM=ASSIST
//STEPLIB  DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
******************************************************************
*                                                                *
*  ASSIGNMENT 1 - YOUR FIRST MAINFRAME PROGRAM                   *
*                                                                *
*  DATE DUE:  09/01/2017                                         *
*                                                                *
*  Replace KC03nnn above with your KC-ID assigned to you by      *
*  your instructor.  DO NOT LEAVE OUT THE CAPITAL LETTER A AT    *    
*  THE END OF YOUR KC-ID!  Also, put your name in all capital    *
*  letters in the first line where it says 'your name here'.     *
*                                                                *
*  Note that this documentation box does not go past column 65!  *
*                                                                *
******************************************************************
MAIN     CSECT
         USING MAIN,15     ESTABLISH ADDRESSABILITY ON REGISTER 15
         SR    3,3         ZERO REGISTER 3 FOR TOTAL
         SR    4,4         ZERO REGISTER 4 FOR COUNTER
         XDUMP
         BR    14          RETURN TO CALLER
         END   MAIN
/*
//
