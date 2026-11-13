//CICCOMP  JOB ,'CICLO DO CARTAO',NOTIFY=&SYSUID,
//             CLASS=A,MSGCLASS=X
//*--------------------------------------------------------------------
//* COMPILA E LIGA UM PROGRAMA DO CICLO DO CARTAO.
//* TROCAR O VALOR DE PGM NO SET ABAIXO PELO NOME DO MEMBRO.
//*--------------------------------------------------------------------
//         SET PGM=CICBIN
//*
//CL       EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CICLO.COBOL(&PGM),DISP=SHR
//COBOL.SYSLIB DD DSN=&SYSUID..CICLO.COPY,DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..CICLO.LOAD(&PGM),DISP=SHR
