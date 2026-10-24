      *================================================================*
      * CPPAGTO  - PAGAMENTO RECEBIDO NO DIA                           *
      * LRECL 80, RECFM FB. ORDENADO POR NUMERO DO CARTAO.             *
      *================================================================*
       01  REG-PAGAMENTO.
           05  PG-NUM-CARTAO          PIC X(16).
           05  PG-DATA-PAGAMENTO      PIC 9(08).
           05  PG-VALOR               PIC 9(09)V99.
           05  FILLER                 PIC X(45).
