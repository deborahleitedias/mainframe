      *================================================================*
      * CPFATUR  - FATURA EM ABERTO                                    *
      * LRECL 80, RECFM FB. ORDENADO POR NUMERO DO CARTAO.             *
      *================================================================*
       01  REG-FATURA.
           05  FT-NUM-CARTAO          PIC X(16).
           05  FT-DATA-VENCIMENTO     PIC 9(08).
           05  FT-VALOR               PIC 9(09)V99.
           05  FT-NOME-TITULAR        PIC X(30).
           05  FILLER                 PIC X(15).
