//EXECPGM  JOB ,'FATURA NOTURNA',NOTIFY=&SYSUID,
//             CLASS=A,MSGCLASS=X
//*--------------------------------------------------------------------
//* AULAS 5 E 6 - EXECUTA UM PROGRAMA SEM ARQUIVOS (SO DISPLAY)
//* A SAIDA DO DISPLAY APARECE NO DD SYSOUT DO SPOOL.
//*--------------------------------------------------------------------
//         SET PGM=FATJUROS
//RUN      EXEC PGM=&PGM
//STEPLIB  DD DSN=&SYSUID..FATURA.LOAD,DISP=SHR
//SYSOUT   DD SYSOUT=*
//CEEDUMP  DD SYSOUT=*
