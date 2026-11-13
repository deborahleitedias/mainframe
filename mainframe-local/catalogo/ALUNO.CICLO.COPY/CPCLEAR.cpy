      *================================================================*
      * CPCLEAR  - ARQUIVO DE COMPENSACAO (CLEARING) RECEBIDO DE UMA   *
      *            BANDEIRA: HEADER (0), DETALHES (1) E TRAILER (9)    *
      * LRECL 80, RECFM FB. UM ARQUIVO POR BANDEIRA.                   *
      *================================================================*
       01  REG-CLEARING.
           05  CL-TIPO-REGISTRO       PIC X(01).
               88  CL-HEADER                    VALUE '0'.
               88  CL-DETALHE                   VALUE '1'.
               88  CL-TRAILER                   VALUE '9'.
           05  CL-CORPO               PIC X(79).
      *--- DETALHE: UMA APRESENTACAO (TRANSACAO CAPTURADA) -------------*
           05  CL-DETALHE-R REDEFINES CL-CORPO.
               10  CL-NUM-CARTAO      PIC X(16).
      *        DE 38: BRANCOS QUANDO A COMPRA NAO TEVE AUTORIZACAO
               10  CL-COD-AUTORIZACAO PIC X(06).
      *        DE 37: REFERENCIA UNICA DA APRESENTACAO
               10  CL-RRN             PIC X(12).
               10  CL-DATA-COMPRA     PIC 9(08).
               10  CL-VALOR           PIC 9(09)V99.
               10  CL-MCC             PIC 9(04).
               10  CL-PAIS            PIC X(03).
               10  CL-MODO-ENTRADA    PIC X(02).
               10  CL-ECI             PIC X(02).
               10  CL-RECORRENTE      PIC X(01).
               10  CL-PRODUTO         PIC X(01).
                   88  CL-CREDITO               VALUE 'C'.
      *        INDICADOR DA TAXA DE INTERCAMBIO (IRD)
               10  CL-IRD             PIC X(02).
               10  CL-BANDEIRA        PIC X(01).
               10  CL-ESTABELECIMENTO PIC X(10).
      *--- HEADER ------------------------------------------------------*
           05  CL-HEADER-R REDEFINES CL-CORPO.
               10  CL-H-BANDEIRA      PIC X(01).
               10  CL-H-DATA-PROC     PIC 9(08).
               10  CL-H-ARQUIVO       PIC X(10).
               10  FILLER             PIC X(60).
      *--- TRAILER: TOTAIS DE CONTROLE DOS DETALHES --------------------*
           05  CL-TRAILER-R REDEFINES CL-CORPO.
               10  CL-T-BANDEIRA      PIC X(01).
               10  CL-T-QTD           PIC 9(07).
               10  CL-T-VALOR         PIC 9(11)V99.
               10  FILLER             PIC X(58).
