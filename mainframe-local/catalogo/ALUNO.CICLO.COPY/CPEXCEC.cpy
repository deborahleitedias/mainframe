      *================================================================*
      * CPEXCEC  - EXCECAO DA COMPENSACAO, PARA ANALISE                *
      * LRECL 120, RECFM FB. GRAVADO PELO CICCLEAR.                    *
      * GUARDA O NUMERO COMPLETO DO CARTAO PARA A ANALISE: O ACESSO A  *
      * ESTE ARQUIVO E RESTRITO.                                       *
      *================================================================*
       01  REG-EXCECAO.
           05  EX-TIPO                PIC X(02).
               88  EX-DIFERENCA-FORA            VALUE 'DF'.
               88  EX-SEM-AUTORIZACAO           VALUE 'SA'.
               88  EX-DUPLICADA                 VALUE 'DP'.
               88  EX-TARDIA                    VALUE 'AT'.
               88  EX-AUT-SEM-APRESENTACAO      VALUE 'AS'.
           05  EX-DESCRICAO           PIC X(40).
           05  EX-NUM-CARTAO          PIC X(16).
           05  EX-COD-AUTORIZACAO     PIC X(06).
           05  EX-RRN                 PIC X(12).
           05  EX-DATA-COMPRA         PIC 9(08).
           05  EX-VALOR-APRESENTADO   PIC 9(09)V99.
           05  EX-VALOR-AUTORIZADO    PIC 9(09)V99.
           05  EX-DIAS                PIC 9(03).
           05  EX-BANDEIRA            PIC X(01).
           05  FILLER                 PIC X(10).
