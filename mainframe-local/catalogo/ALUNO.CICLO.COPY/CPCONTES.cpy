      *================================================================*
      * CPCONTES - CONTESTACAO REGISTRADA PELO PORTADOR NO EMISSOR     *
      * LRECL 80, RECFM FB. ORDENADO PELA REFERENCIA (RRN).            *
      *================================================================*
       01  REG-CONTESTACAO.
           05  CN-NUM-CARTAO          PIC X(16).
           05  CN-RRN                 PIC X(12).
           05  CN-DATA-CONTESTACAO    PIC 9(08).
           05  CN-MOTIVO              PIC X(02).
               88  CN-NAO-RECONHECE             VALUE 'NR'.
               88  CN-RECORRENTE-CANCELADA      VALUE 'CR'.
               88  CN-COBRANCA-DUPLICADA        VALUE 'DU'.
               88  CN-NAO-ENTREGUE              VALUE 'NE'.
               88  CN-VALOR-DIFERENTE           VALUE 'VD'.
           05  CN-VALOR               PIC 9(09)V99.
           05  CN-PROTOCOLO           PIC X(10).
           05  FILLER                 PIC X(21).
