//DEFGDG   JOB ,'FATURA NOTURNA',NOTIFY=&SYSUID,
//             CLASS=A,MSGCLASS=X
//*--------------------------------------------------------------------
//* AULA 4 - DEFINICAO DAS BASES GDG DO PROJETO (EXECUTAR UMA VEZ)
//* LIMIT(7): GUARDA AS 7 GERACOES MAIS RECENTES (UMA SEMANA).
//* SCRATCH: A GERACAO MAIS ANTIGA E APAGADA QUANDO O LIMITE ESTOURA.
//*--------------------------------------------------------------------
//STEP010  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *,SYMBOLS=JCLONLY
  DEFINE GDG (NAME(&SYSUID..FATURA.TRANS.ORDEN) -
              LIMIT(7) SCRATCH NOEMPTY)
  DEFINE GDG (NAME(&SYSUID..FATURA.RELAT) -
              LIMIT(7) SCRATCH NOEMPTY)
/*
//*--------------------------------------------------------------------
//* ATENCAO: SIMBOLOS COMO &SYSUID DENTRO DE DADOS IN-STREAM (DD *) SO
//* SAO RESOLVIDOS SE A PLATAFORMA PERMITIR SYMBOLS=JCLONLY. SE O
//* IDCAMS ACUSAR NOME INVALIDO, TROCAR &SYSUID PELO SEU ID.
//*--------------------------------------------------------------------
