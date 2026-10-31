//CARGAMCC JOB ,'TABELA MCC',NOTIFY=&SYSUID,
//             CLASS=A,MSGCLASS=X
//*--------------------------------------------------------------------
//* AULA 8: CARREGA A TABELA DE CATEGORIAS (MCC) USADA PELO FATRELAT.
//* RODAR UMA VEZ. A TABELA PRECISA FICAR EM ORDEM CRESCENTE DE MCC.
//*--------------------------------------------------------------------
//STEP010  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT2   DD DSN=&SYSUID..FATURA.MCC.TABELA,DISP=OLD
//SYSUT1   DD *
4121TAXI E TRANSPORTE POR APP     
5411SUPERMERCADOS                 
5541POSTOS DE COMBUSTIVEL         
5812RESTAURANTES                  
5814LANCHONETES E FAST FOOD       
5912FARMACIAS                     
5942LIVRARIAS                     
5999VAREJO DIVERSO                
7011HOTEIS E HOSPEDAGEM           
7832CINEMAS                       
/*
