      *================================================================*
      * CPBIN    - TABELA DE BINS RECEBIDA DAS BANDEIRAS               *
      * LRECL 80, RECFM FB. ORDENADA POR BIN (8 DIGITOS).              *
      * PROJETO CICLO DO CARTAO - DADOS 100% FICTICIOS                 *
      *================================================================*
       01  REG-BIN.
           05  BN-BIN                 PIC 9(08).
           05  BN-BANDEIRA            PIC X(01).
               88  BN-BANDEIRA-ALFA             VALUE 'A'.
               88  BN-BANDEIRA-BETA             VALUE 'B'.
           05  BN-NOME-BANDEIRA       PIC X(10).
           05  BN-COD-EMISSOR         PIC 9(03).
           05  BN-NOME-EMISSOR        PIC X(20).
           05  BN-PRODUTO             PIC X(01).
               88  BN-CREDITO                   VALUE 'C'.
               88  BN-DEBITO                    VALUE 'D'.
               88  BN-PRE-PAGO                  VALUE 'P'.
           05  BN-DESC-PRODUTO        PIC X(15).
           05  FILLER                 PIC X(22).
