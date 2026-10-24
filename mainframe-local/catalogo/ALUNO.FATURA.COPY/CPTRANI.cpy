      *================================================================*
      * CPTRANI  - TRANSACAO INTERNA (APOS VALIDACAO)                  *
      * LRECL 80, RECFM FB. VALOR EM COMP-3 (PACKED DECIMAL).          *
      * ESTE ARQUIVO E BINARIO: NAO TRANSFERIR COMO TEXTO.             *
      *================================================================*
       01  REG-TRANS-INTERNA.
           05  TI-NUM-CARTAO          PIC X(16).
           05  TI-DATA-TRANSACAO      PIC 9(08).
           05  TI-HORA-TRANSACAO      PIC 9(06).
           05  TI-VALOR               PIC S9(09)V99 COMP-3.
           05  TI-MCC                 PIC 9(04).
           05  TI-PAIS                PIC X(03).
           05  TI-COD-AUTORIZACAO     PIC X(06).
           05  TI-CLASSIFICACAO       PIC X(01).
               88  TI-NORMAL                    VALUE 'N'.
               88  TI-SUSPEITA                  VALUE 'S'.
           05  TI-ESTABELECIMENTO     PIC X(20).
           05  FILLER                 PIC X(10).
