      *================================================================*
      * CPCHARG  - CHARGEBACK ENVIADO PELO EMISSOR A BANDEIRA          *
      * LRECL 120, RECFM FB. GRAVADO PELO CICDISPU.                    *
      * O NUMERO COMPLETO DO CARTAO E EXIGIDO PELA BANDEIRA: O ARQUIVO *
      * TEM ACESSO RESTRITO E NAO E IMPRESSO.                          *
      *================================================================*
       01  REG-CHARGEBACK.
           05  CB-NUM-CARTAO          PIC X(16).
           05  CB-RRN                 PIC X(12).
           05  CB-COD-AUTORIZACAO     PIC X(06).
           05  CB-BANDEIRA            PIC X(01).
           05  CB-CATEGORIA           PIC X(02).
           05  CB-REASON-CODE         PIC X(06).
           05  CB-VALOR               PIC 9(09)V99.
           05  CB-DATA-COMPRA         PIC 9(08).
           05  CB-DATA-CHARGEBACK     PIC 9(08).
           05  CB-PROTOCOLO           PIC X(10).
           05  CB-DESCRICAO           PIC X(30).
           05  FILLER                 PIC X(10).
