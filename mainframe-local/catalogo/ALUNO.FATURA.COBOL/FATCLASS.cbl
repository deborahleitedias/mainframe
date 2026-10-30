
      *================================================================*
      * PROGRAMA : FATCLASS (ESQUELETO PARA A TURMA)                   *
      * AULA     : 6 - COBOL II, DECISAO E REPETICAO                   *
      * OBJETIVO : CLASSIFICAR TRANSACOES EM APROVADA, NEGADA OU       *
      *            SUSPEITA E VALIDAR O NUMERO DO CARTAO (LUHN).       *
      * O QUE VOCE ESCREVE:                                            *
      *   1. OS NIVEIS 88 MARCADOS NA TABELA DE TESTE;                 *
      *   2. O EVALUATE TRUE DO PARAGRAFO 2100-CLASSIFICAR-TRANSACAO.  *
      * O PARAGRAFO 9100-VALIDAR-LUHN JA ESTA PRONTO: LEIA E COMPARE   *
      * COM O PSEUDOCODIGO DE LUHN DA AULA 2.                          *
      * REGRAS   : CARTAO INVALIDO = NUMERO REPROVADO NO LUHN          *
      *            NEGADA   = STATUS 'N' DA AUTORIZACAO                *
      *            SUSPEITA = APROVADA, FORA DO PAIS E VALOR ACIMA     *
      *                       DE 5.000,00                              *
      *            APROVADA = DEMAIS CASOS                             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FATCLASS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-LIMITE-SUSPEITA         PIC 9(09)V99 VALUE 5000.00.

      *--- MASSA DE TESTE ---------------------------------------------*
       01  WS-MASSA-TESTE.
           05  FILLER PIC X(31) VALUE
               '990000073702838300000015000BRAA'.
           05  FILLER PIC X(31) VALUE
               '990000073702838300000750000USAA'.
           05  FILLER PIC X(31) VALUE
               '990000079819331600000020000BRAN'.
           05  FILLER PIC X(31) VALUE
               '990000079819331700000020000BRAA'.
           05  FILLER PIC X(31) VALUE
               '990000079819331600000450000ARGA'.
           05  FILLER PIC X(31) VALUE
               '990000073702838300000500000USAA'.

       01  WS-TABELA-TESTE REDEFINES WS-MASSA-TESTE.
           05  WS-TRANS-TESTE OCCURS 6 TIMES.
               10  WS-T-CARTAO        PIC X(16).
               10  WS-T-VALOR         PIC 9(09)V99.
               10  WS-T-PAIS          PIC X(03).
                   88  WS-T-NACIONAL VALUE 'BRA'.
               10  WS-T-STATUS        PIC X(01).
                   88  WS-T-APROVADA VALUE 'A'.
                   88  WS-T-NEGADA   VALUE 'N'.

       01  WS-IND                     PIC 9(02).
       01  WS-CLASSIFICACAO           PIC X(20).

      *--- AREAS DO ALGORITMO DE LUHN ---------------------------------*
       01  WS-LUHN-CARTAO             PIC X(16).
       01  WS-LUHN-DIGITOS REDEFINES WS-LUHN-CARTAO.
           05  WS-LUHN-DIGITO         PIC 9(01) OCCURS 16 TIMES.
       01  WS-LUHN-POS                PIC 9(02).
       01  WS-LUHN-PARCELA            PIC 9(02).
       01  WS-LUHN-SOMA               PIC 9(03).
       01  WS-LUHN-DOBRAR             PIC X(01).
           88  WS-LUHN-DOBRA          VALUE 'S'.
           88  WS-LUHN-NAO-DOBRA      VALUE 'N'.
       01  WS-LUHN-RESULTADO          PIC X(01).
           88  WS-LUHN-VALIDO         VALUE 'S'.
           88  WS-LUHN-INVALIDO       VALUE 'N'.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 2100-CLASSIFICAR-TRANSACAO
              VARYING WS-IND FROM 1 BY 1
              UNTIL WS-IND > 6
           GOBACK.

       2100-CLASSIFICAR-TRANSACAO.
           MOVE SPACES TO WS-CLASSIFICACAO
           MOVE WS-T-CARTAO(WS-IND) TO WS-LUHN-CARTAO
           PERFORM 9100-VALIDAR-LUHN

           EVALUATE TRUE
               WHEN WS-LUHN-INVALIDO
                   MOVE 'CARTAO INVALIDO'
                     TO WS-CLASSIFICACAO

               WHEN WS-T-NEGADA(WS-IND)
                   MOVE 'NEGADA'
                     TO WS-CLASSIFICACAO

               WHEN WS-T-APROVADA(WS-IND)
                AND NOT WS-T-NACIONAL(WS-IND)
                AND WS-T-VALOR(WS-IND) > WS-LIMITE-SUSPEITA
                   MOVE 'SUSPEITA'
                     TO WS-CLASSIFICACAO

               WHEN OTHER
                   MOVE 'APROVADA'
                     TO WS-CLASSIFICACAO
           END-EVALUATE

           DISPLAY 'TRANSACAO ' WS-IND ': ' WS-CLASSIFICACAO
           .

      *----------------------------------------------------------------*
      * ALGORITMO DE LUHN
      *----------------------------------------------------------------*
       9100-VALIDAR-LUHN.
           SET WS-LUHN-INVALIDO TO TRUE
           IF WS-LUHN-CARTAO IS NUMERIC
              MOVE ZERO TO WS-LUHN-SOMA
              SET WS-LUHN-NAO-DOBRA TO TRUE
              PERFORM VARYING WS-LUHN-POS FROM 16 BY -1
                      UNTIL WS-LUHN-POS < 1
                 MOVE WS-LUHN-DIGITO(WS-LUHN-POS)
                   TO WS-LUHN-PARCELA
                 IF WS-LUHN-DOBRA
                    COMPUTE WS-LUHN-PARCELA =
                       WS-LUHN-PARCELA * 2
                    IF WS-LUHN-PARCELA > 9
                       SUBTRACT 9 FROM WS-LUHN-PARCELA
                    END-IF
                    SET WS-LUHN-NAO-DOBRA TO TRUE
                 ELSE
                    SET WS-LUHN-DOBRA TO TRUE
                 END-IF
                 ADD WS-LUHN-PARCELA TO WS-LUHN-SOMA
              END-PERFORM
              IF FUNCTION MOD(WS-LUHN-SOMA, 10) = ZERO
                 SET WS-LUHN-VALIDO TO TRUE
              END-IF
           END-IF
           .
