      *================================================================*
      * PROGRAMA : LUHNVALD (SUBPROGRAMA)                              *
      * AULA     : 9 - MODULARIZACAO                                   *
      * OBJETIVO : VALIDAR O DIGITO VERIFICADOR DO NUMERO DO CARTAO    *
      *            PELO ALGORITMO DE LUHN. EXTRAIDO DO FATCLASS.       *
      * CHAMADA  : CALL 'LUHNVALD' USING LK-AREA-LUHN (COPY CPLUHN)    *
      * RETORNO  : LK-RESULTADO = 'S' (VALIDO) OU 'N' (INVALIDO)       *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LUHNVALD.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-LUHN-CARTAO             PIC X(16).
       01  WS-LUHN-DIGITOS REDEFINES WS-LUHN-CARTAO.
           05  WS-LUHN-DIGITO         PIC 9(01) OCCURS 16 TIMES.
       01  WS-LUHN-POS                PIC 9(02).
       01  WS-LUHN-PARCELA            PIC 9(02).
       01  WS-LUHN-SOMA               PIC 9(03).
       01  WS-LUHN-DOBRAR             PIC X(01).
           88  WS-LUHN-DOBRA                    VALUE 'S'.
           88  WS-LUHN-NAO-DOBRA                VALUE 'N'.

       LINKAGE SECTION.
           COPY CPLUHN.

       PROCEDURE DIVISION USING LK-AREA-LUHN.
       0000-PRINCIPAL.
           SET LK-CARTAO-INVALIDO TO TRUE
           MOVE LK-NUM-CARTAO     TO WS-LUHN-CARTAO
           IF WS-LUHN-CARTAO IS NOT NUMERIC
              GOBACK
           END-IF
           MOVE ZERO              TO WS-LUHN-SOMA
           SET WS-LUHN-NAO-DOBRA  TO TRUE
           PERFORM VARYING WS-LUHN-POS FROM 16 BY -1
                   UNTIL WS-LUHN-POS < 1
              MOVE WS-LUHN-DIGITO(WS-LUHN-POS) TO WS-LUHN-PARCELA
              IF WS-LUHN-DOBRA
                 COMPUTE WS-LUHN-PARCELA = WS-LUHN-PARCELA * 2
                 IF WS-LUHN-PARCELA > 9
                    SUBTRACT 9 FROM WS-LUHN-PARCELA
                 END-IF
                 SET WS-LUHN-NAO-DOBRA TO TRUE
              ELSE
                 SET WS-LUHN-DOBRA     TO TRUE
              END-IF
              ADD WS-LUHN-PARCELA TO WS-LUHN-SOMA
           END-PERFORM
           IF FUNCTION MOD(WS-LUHN-SOMA, 10) = ZERO
              SET LK-CARTAO-VALIDO TO TRUE
           END-IF
           GOBACK.
