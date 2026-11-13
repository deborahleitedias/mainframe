      *================================================================*
      * CPCONTA  - CADASTRO DE CONTAS DE CARTAO DOS EMISSORES          *
      * LRECL 80, RECFM FB. ORDENADO POR NUMERO DO CARTAO.             *
      * NO DEBITO, O DISPONIVEL E O SALDO DA CONTA; NO PRE-PAGO, O     *
      * SALDO CARREGADO. O DISPONIVEL TEM SINAL PORQUE O STAND-IN PODE *
      * APROVAR ACIMA DO LIMITE.                                       *
      *================================================================*
       01  REG-CONTA.
           05  CT-NUM-CARTAO          PIC X(16).
           05  CT-COD-EMISSOR         PIC 9(03).
           05  CT-STATUS              PIC X(01).
               88  CT-ATIVO                     VALUE 'A'.
               88  CT-BLOQUEADO                 VALUE 'B'.
               88  CT-CANCELADO                 VALUE 'C'.
      *    VALIDADE NO FORMATO AAAAMM: O CARTAO VALE ATE O FIM DO MES
           05  CT-VALIDADE            PIC 9(06).
           05  CT-LIMITE-TOTAL        PIC 9(09)V99.
           05  CT-LIMITE-DISP         PIC S9(09)V99
                                      SIGN IS LEADING SEPARATE.
           05  CT-INTERNACIONAL       PIC X(01).
               88  CT-HABILITADO-EXTERIOR       VALUE 'S'.
           05  CT-LIMITE-SAQUE        PIC 9(07)V99.
           05  CT-NOME-TITULAR        PIC X(20).
           05  FILLER                 PIC X(01).
