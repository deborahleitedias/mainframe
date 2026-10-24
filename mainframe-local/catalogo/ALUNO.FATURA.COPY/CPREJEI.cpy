      *================================================================*
      * CPREJEI  - TRANSACAO REJEITADA NA VALIDACAO                    *
      * LRECL 120, RECFM FB.                                           *
      *================================================================*
       01  REG-REJEITADA.
           05  RJ-REGISTRO-ORIGINAL   PIC X(80).
           05  RJ-COD-MOTIVO          PIC X(02).
               88  RJ-CARTAO-INVALIDO           VALUE '01'.
               88  RJ-VALOR-INVALIDO            VALUE '02'.
               88  RJ-DATA-INVALIDA             VALUE '03'.
               88  RJ-TRANSACAO-NEGADA          VALUE '04'.
           05  RJ-DESC-MOTIVO         PIC X(30).
           05  FILLER                 PIC X(08).
