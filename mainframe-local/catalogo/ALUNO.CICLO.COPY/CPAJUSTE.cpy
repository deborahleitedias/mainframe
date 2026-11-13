      *================================================================*
      * CPAJUSTE - AJUSTE NA CONTA DO PORTADOR (CREDITO PROVISORIO     *
      *            ENQUANTO A DISPUTA NAO TERMINA)                     *
      * LRECL 80, RECFM FB. GRAVADO PELO CICDISPU.                     *
      *================================================================*
       01  REG-AJUSTE.
           05  AJ-NUM-CARTAO          PIC X(16).
           05  AJ-TIPO                PIC X(02).
               88  AJ-CREDITO-PROVISORIO        VALUE 'CP'.
           05  AJ-DATA                PIC 9(08).
           05  AJ-VALOR               PIC 9(09)V99.
           05  AJ-RRN                 PIC X(12).
           05  AJ-PROTOCOLO           PIC X(10).
           05  FILLER                 PIC X(21).
