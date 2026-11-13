      *================================================================*
      * CPIRD    - TABELA DE TAXAS DE INTERCAMBIO POR IRD, RECEBIDA    *
      *            DAS BANDEIRAS                                       *
      * LRECL 80, RECFM FB. ORDENADA POR BANDEIRA E IRD.               *
      * TAXAS EM PERCENTUAL COM 4 DECIMAIS (14000 = 1,4000%).          *
      *================================================================*
       01  REG-IRD.
           05  IR-BANDEIRA            PIC X(01).
           05  IR-CODIGO              PIC X(02).
           05  IR-PRODUTO             PIC X(01).
           05  IR-DESCRICAO           PIC X(30).
           05  IR-TAXA-INTERCAMBIO    PIC 9(01)V9(04).
           05  IR-TARIFA-BANDEIRA     PIC 9(01)V9(04).
           05  FILLER                 PIC X(36).
