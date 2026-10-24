      *================================================================*
      * CPDIVER  - DIVERGENCIA DA CONCILIACAO DE PAGAMENTOS            *
      * LRECL 80, RECFM FB.                                            *
      *================================================================*
       01  REG-DIVERGENCIA.
           05  DV-NUM-CARTAO-MASC     PIC X(16).
           05  DV-TIPO                PIC X(02).
               88  DV-PAGTO-SEM-FATURA          VALUE 'PS'.
               88  DV-FATURA-SEM-PAGTO          VALUE 'FS'.
               88  DV-PAGTO-A-MENOR             VALUE 'PM'.
           05  DV-VALOR-FATURA        PIC 9(09)V99.
           05  DV-VALOR-PAGO          PIC 9(09)V99.
           05  FILLER                 PIC X(40).
