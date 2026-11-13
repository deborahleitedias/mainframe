      *================================================================*
      * CPAUTLOG - LOG DE AUTORIZACOES (UM REGISTRO POR MENSAGEM)      *
      * LRECL 120, RECFM FB. GRAVADO PELO CICAUTOR, LIDO PELO CICCLEAR *
      * ORDENADO POR CARTAO E CODIGO DE AUTORIZACAO (POSICOES 15 E 74) *
      *================================================================*
       01  REG-LOG-AUTORIZACAO.
           05  LG-MTI-PEDIDO          PIC X(04).
           05  LG-MTI-RESPOSTA        PIC X(04).
           05  LG-COD-PROCESSAMENTO   PIC X(06).
           05  LG-NUM-CARTAO          PIC X(16).
           05  LG-VALOR               PIC 9(09)V99.
           05  LG-DATA                PIC 9(08).
           05  LG-HORA                PIC 9(06).
           05  LG-STAN                PIC 9(06).
           05  LG-MCC                 PIC 9(04).
           05  LG-MODO-ENTRADA        PIC X(02).
           05  LG-PAIS                PIC X(03).
           05  LG-ECI                 PIC X(02).
           05  LG-RECORRENTE          PIC X(01).
      *    DE 38 E DE 39
           05  LG-COD-AUTORIZACAO     PIC X(06).
           05  LG-RESPOSTA            PIC X(02).
               88  LG-APROVADA                  VALUE '00'.
      *    QUEM RESPONDEU: E = EMISSOR, S = BANDEIRA EM STAND-IN
           05  LG-ORIGEM              PIC X(01).
               88  LG-PELO-EMISSOR              VALUE 'E'.
               88  LG-EM-STAND-IN               VALUE 'S'.
      *    SITUACAO FINAL: BRANCO = VALE, R = REVERTIDA DEPOIS
           05  LG-SITUACAO            PIC X(01).
               88  LG-REVERTIDA                 VALUE 'R'.
           05  LG-DESC-RESPOSTA       PIC X(30).
           05  FILLER                 PIC X(07).
