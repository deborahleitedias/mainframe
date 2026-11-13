      *================================================================*
      * CPTRANS  - TRANSACAO RECEBIDA DA BANDEIRA (ARQUIVO EXTERNO)    *
      * LRECL 80, RECFM FB. CAMPOS NUMERICOS EM FORMATO DISPLAY PARA   *
      * QUE O ARQUIVO POSSA SER TRANSFERIDO COMO TEXTO.                *
      * PROJETO-GUIA FATURA NOTURNA - DADOS 100% FICTICIOS             *
      *================================================================*
       01  REG-TRANSACAO.
           05  TR-NUM-CARTAO          PIC X(16).
           05  TR-DATA-TRANSACAO      PIC 9(08).
           05  TR-HORA-TRANSACAO      PIC 9(06).
           05  TR-VALOR               PIC 9(09)V99.
           05  TR-VALOR-X REDEFINES TR-VALOR
                                      PIC X(11).
           05  TR-MCC                 PIC 9(04).
           05  TR-PAIS                PIC X(03).
               88  TR-NACIONAL                  VALUE 'BRA'.
           05  TR-COD-AUTORIZACAO     PIC X(06).
           05  TR-STATUS              PIC X(01).
               88  TR-APROVADA                  VALUE 'A'.
               88  TR-NEGADA                    VALUE 'N'.
           05  TR-ESTABELECIMENTO     PIC X(20).
           05  FILLER                 PIC X(05).
