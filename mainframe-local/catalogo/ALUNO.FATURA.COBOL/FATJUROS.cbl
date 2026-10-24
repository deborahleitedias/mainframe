      *================================================================*
      * PROGRAMA : FATJUROS                                            *
      * AULA     : 5 - COBOL I, ANATOMIA DE UM PROGRAMA                *
      * OBJETIVO : CALCULAR O TOTAL DA FATURA DE UM CLIENTE, COM MULTA *
      *            E JUROS SOBRE O SALDO ANTERIOR EM ATRASO.           *
      *            TRADUCAO DIRETA DO PSEUDOCODIGO DA AULA 1.          *
      * REGRAS   : MULTA DE 2% SOBRE O SALDO EM ATRASO.                *
      *            JUROS DE 1% AO MES, PROPORCIONAIS AOS DIAS DE       *
      *            ATRASO (1% / 30 POR DIA).                           *
      * PROJETO  : FATURA NOTURNA - DADOS FICTICIOS                    *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FATJUROS.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *--- ENTRADAS (VALORES FIXOS NESTA AULA; ARQUIVOS NA AULA 7) ----*
       01  WS-COMPRAS.
           05  WS-COMPRA-1            PIC 9(07)V99 VALUE 152.30.
           05  WS-COMPRA-2            PIC 9(07)V99 VALUE 89.90.
           05  WS-COMPRA-3            PIC 9(07)V99 VALUE 1200.00.
           05  WS-COMPRA-4            PIC 9(07)V99 VALUE 45.50.
           05  WS-COMPRA-5            PIC 9(07)V99 VALUE 310.00.
       01  WS-SALDO-ANTERIOR          PIC 9(07)V99 VALUE 500.00.
       01  WS-DIAS-ATRASO             PIC 9(03)    VALUE 10.
       01  WS-LIMITE                  PIC 9(07)V99 VALUE 1000.00.

      *--- CONSTANTES DE NEGOCIO --------------------------------------*
       01  WS-TAXA-MULTA              PIC V9(04)   VALUE .0200.
       01  WS-TAXA-JUROS-MES          PIC V9(04)   VALUE .0100.

      *--- AREAS DE CALCULO -------------------------------------------*
       01  WS-TOTAL-COMPRAS           PIC 9(09)V99 VALUE ZERO.
       01  WS-MULTA                   PIC 9(07)V99 VALUE ZERO.
       01  WS-JUROS                   PIC 9(07)V99 VALUE ZERO.
       01  WS-TOTAL-FATURA            PIC 9(09)V99 VALUE ZERO.
       01  WS-QTD-ACIMA-LIMITE        PIC 9(02)    VALUE ZERO.

      *--- CAMPOS EDITADOS PARA EXIBICAO ------------------------------*
       01  WS-VALOR-EDITADO           PIC ZZZ,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           ADD WS-COMPRA-1 WS-COMPRA-2 WS-COMPRA-3
               WS-COMPRA-4 WS-COMPRA-5
               GIVING WS-TOTAL-COMPRAS

           IF WS-COMPRA-1 > WS-LIMITE
              ADD 1 TO WS-QTD-ACIMA-LIMITE
           END-IF
           IF WS-COMPRA-2 > WS-LIMITE
              ADD 1 TO WS-QTD-ACIMA-LIMITE
           END-IF
           IF WS-COMPRA-3 > WS-LIMITE
              ADD 1 TO WS-QTD-ACIMA-LIMITE
           END-IF
           IF WS-COMPRA-4 > WS-LIMITE
              ADD 1 TO WS-QTD-ACIMA-LIMITE
           END-IF
           IF WS-COMPRA-5 > WS-LIMITE
              ADD 1 TO WS-QTD-ACIMA-LIMITE
           END-IF

           IF WS-DIAS-ATRASO > ZERO
              COMPUTE WS-MULTA ROUNDED =
                      WS-SALDO-ANTERIOR * WS-TAXA-MULTA
              COMPUTE WS-JUROS ROUNDED =
                      WS-SALDO-ANTERIOR * WS-TAXA-JUROS-MES
                      * WS-DIAS-ATRASO / 30
              END-COMPUTE
           END-IF

           COMPUTE WS-TOTAL-FATURA =
                   WS-TOTAL-COMPRAS + WS-SALDO-ANTERIOR
                   + WS-MULTA + WS-JUROS
              ON SIZE ERROR
                 DISPLAY 'FATJUROS - ESTOURO NO TOTAL DA FATURA'
                 MOVE 16 TO RETURN-CODE
                 GOBACK
           END-COMPUTE

           DISPLAY '=== FATURA DO CLIENTE (DADOS FICTICIOS) ==='
           MOVE WS-TOTAL-COMPRAS  TO WS-VALOR-EDITADO
           DISPLAY 'TOTAL DE COMPRAS ......: ' WS-VALOR-EDITADO
           MOVE WS-SALDO-ANTERIOR TO WS-VALOR-EDITADO
           DISPLAY 'SALDO ANTERIOR ........: ' WS-VALOR-EDITADO
           MOVE WS-MULTA          TO WS-VALOR-EDITADO
           DISPLAY 'MULTA (2%) ............: ' WS-VALOR-EDITADO
           MOVE WS-JUROS          TO WS-VALOR-EDITADO
           DISPLAY 'JUROS PRO RATA ........: ' WS-VALOR-EDITADO
           MOVE WS-TOTAL-FATURA   TO WS-VALOR-EDITADO
           DISPLAY 'TOTAL DA FATURA .......: ' WS-VALOR-EDITADO
           DISPLAY 'COMPRAS ACIMA DO LIMITE: ' WS-QTD-ACIMA-LIMITE

           GOBACK.
