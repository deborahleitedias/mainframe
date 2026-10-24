      *================================================================*
      * CPLUHN   - AREA DE COMUNICACAO COM O SUBPROGRAMA LUHNVALD      *
      *================================================================*
       01  LK-AREA-LUHN.
           05  LK-NUM-CARTAO          PIC X(16).
           05  LK-RESULTADO           PIC X(01).
               88  LK-CARTAO-VALIDO             VALUE 'S'.
               88  LK-CARTAO-INVALIDO           VALUE 'N'.
