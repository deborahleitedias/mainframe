      *================================================================*
      * CPLANC   - LANCAMENTO: APRESENTACAO JA CASADA COM A            *
      *            AUTORIZACAO E LANCADA NA CONTA DO PORTADOR          *
      * LRECL 120, RECFM FB. GRAVADO PELO CICCLEAR.                    *
      *================================================================*
       01  REG-LANCAMENTO.
           05  LC-NUM-CARTAO          PIC X(16).
           05  LC-COD-AUTORIZACAO     PIC X(06).
           05  LC-RRN                 PIC X(12).
           05  LC-DATA-COMPRA         PIC 9(08).
           05  LC-VALOR               PIC 9(09)V99.
           05  LC-VALOR-AUTORIZADO    PIC 9(09)V99.
           05  LC-MCC                 PIC 9(04).
           05  LC-PAIS                PIC X(03).
           05  LC-MODO-ENTRADA        PIC X(02).
           05  LC-ECI                 PIC X(02).
           05  LC-RECORRENTE          PIC X(01).
           05  LC-PRODUTO             PIC X(01).
           05  LC-IRD                 PIC X(02).
           05  LC-BANDEIRA            PIC X(01).
           05  LC-ESTABELECIMENTO     PIC X(10).
           05  LC-DATA-PROCESSAMENTO  PIC 9(08).
      *    RESULTADO DO CASAMENTO COM A AUTORIZACAO
           05  LC-SITUACAO            PIC X(01).
               88  LC-CASADA                    VALUE ' '.
               88  LC-DIFERENCA-ACEITA          VALUE 'D'.
               88  LC-SEM-AUTORIZACAO           VALUE 'S'.
               88  LC-DUPLICADA                 VALUE 'P'.
           05  LC-TARDIA              PIC X(01).
               88  LC-APRESENTACAO-TARDIA       VALUE 'S'.
           05  FILLER                 PIC X(20).
