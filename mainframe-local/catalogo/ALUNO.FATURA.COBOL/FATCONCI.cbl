      *================================================================*
      * PROGRAMA : FATCONCI                                            *
      * AULA     : 9 - BALANCE LINE                                    *
      * OBJETIVO : CRUZAR FATURAS EM ABERTO COM PAGAMENTOS DO DIA,     *
      *            AMBOS ORDENADOS POR CARTAO, BAIXANDO AS FATURAS     *
      *            PAGAS E GRAVANDO AS DIVERGENCIAS:                   *
      *            PS = PAGAMENTO SEM FATURA                           *
      *            FS = FATURA SEM PAGAMENTO                           *
      *            PM = PAGAMENTO A MENOR                              *
      * ENTRADAS : DD FATIN   - CPFATUR, FB 80                         *
      *            DD PAGIN   - CPPAGTO, FB 80                         *
      * SAIDA    : DD DIVEROUT - CPDIVER, FB 80                        *
      * PREMISSA : NO MAXIMO UM PAGAMENTO E UMA FATURA POR CARTAO.     *
      * RC       : 0 = OK, 16 = ERRO DE ARQUIVO OU CONTAS NAO FECHAM   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FATCONCI.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FATURAS     ASSIGN TO FATIN
                  FILE STATUS IS WS-FS-FATURA.
           SELECT PAGAMENTOS  ASSIGN TO PAGIN
                  FILE STATUS IS WS-FS-PAGTO.
           SELECT DIVERGENCIAS ASSIGN TO DIVEROUT
                  FILE STATUS IS WS-FS-DIVER.

       DATA DIVISION.
       FILE SECTION.
       FD  FATURAS
           RECORDING MODE IS F.
           COPY CPFATUR.
       FD  PAGAMENTOS
           RECORDING MODE IS F.
           COPY CPPAGTO.
       FD  DIVERGENCIAS
           RECORDING MODE IS F.
           COPY CPDIVER.

       WORKING-STORAGE SECTION.
       01  WS-FS-FATURA               PIC X(02).
           88  WS-FS-FAT-OK                     VALUE '00'.
           88  WS-FS-FAT-FIM                    VALUE '10'.
       01  WS-FS-PAGTO                PIC X(02).
           88  WS-FS-PAG-OK                     VALUE '00'.
           88  WS-FS-PAG-FIM                    VALUE '10'.
       01  WS-FS-DIVER                PIC X(02).
           88  WS-FS-DIV-OK                     VALUE '00'.

      *--- CHAVES DO BALANCE LINE: HIGH-VALUES QUANDO O ARQUIVO ACABA -*
       01  WS-CHAVE-FATURA            PIC X(16).
       01  WS-CHAVE-PAGTO             PIC X(16).

       01  WS-CONTADORES.
           05  WS-QTD-FAT-LIDAS       PIC 9(07) VALUE ZERO.
           05  WS-QTD-PAG-LIDOS       PIC 9(07) VALUE ZERO.
           05  WS-QTD-BAIXADAS        PIC 9(07) VALUE ZERO.
           05  WS-QTD-PS              PIC 9(07) VALUE ZERO.
           05  WS-QTD-FS              PIC 9(07) VALUE ZERO.
           05  WS-QTD-PM              PIC 9(07) VALUE ZERO.

       01  WS-CARTAO-ABERTO           PIC X(16).
       01  WS-CARTAO-MASC             PIC X(16).

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INICIALIZAR THRU 1000-FIM-INICIALIZAR
           PERFORM 2000-CONCILIAR   THRU 2000-FIM-CONCILIAR
              UNTIL WS-CHAVE-FATURA = HIGH-VALUES
                AND WS-CHAVE-PAGTO  = HIGH-VALUES
           PERFORM 3000-FINALIZAR   THRU 3000-FIM-FINALIZAR
           STOP RUN.
       0000-FIM-PRINCIPAL.
           EXIT.

       1000-INICIALIZAR.

           OPEN INPUT FATURAS
           IF NOT WS-FS-FAT-OK
              DISPLAY 'FATCONCI - ERRO DE OPEN FATURAS (FATIN)'
              DISPLAY 'FS: ' WS-FS-FATURA
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR-COM-ERRO
           END-IF

           OPEN INPUT PAGAMENTOS
           IF NOT WS-FS-PAG-OK
              DISPLAY 'FATCONCI - ERRO DE OPEN PAGAMENTOS (PAGIN)'
              DISPLAY 'FS: ' WS-FS-PAGTO
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR-COM-ERRO
           END-IF

           OPEN OUTPUT DIVERGENCIAS
           IF NOT WS-FS-DIV-OK
              DISPLAY 'FATCONCI - ERRO DE OPEN DIVERGENCIAS (DIVEROUT)'
              DISPLAY 'FS: ' WS-FS-DIVER
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR-COM-ERRO
           END-IF

           PERFORM 8100-LER-FATURA    THRU 8100-FIM-LER-FATURA
           PERFORM 8200-LER-PAGAMENTO THRU 8200-FIM-LER-PAGAMENTO.

       1000-FIM-INICIALIZAR.
           EXIT.

       2000-CONCILIAR.

           EVALUATE TRUE
              WHEN WS-CHAVE-FATURA = WS-CHAVE-PAGTO
                 IF PG-VALOR < FT-VALOR
                    MOVE SPACES TO REG-DIVERGENCIA
                    SET DV-PAGTO-A-MENOR TO TRUE
                    PERFORM 7000-GRAVAR-DIVERGENCIA
                       THRU 7000-FIM-GRAVAR-DIVERGENCIA
                    ADD 1 TO WS-QTD-PM
                 ELSE
                    ADD 1 TO WS-QTD-BAIXADAS
                 END-IF
                 PERFORM 8100-LER-FATURA    THRU 8100-FIM-LER-FATURA
                 PERFORM 8200-LER-PAGAMENTO THRU 8200-FIM-LER-PAGAMENTO
              WHEN WS-CHAVE-FATURA < WS-CHAVE-PAGTO
                 MOVE SPACES TO REG-DIVERGENCIA
                 SET DV-FATURA-SEM-PAGTO TO TRUE
                 PERFORM 7000-GRAVAR-DIVERGENCIA
                    THRU 7000-FIM-GRAVAR-DIVERGENCIA
                 ADD 1 TO WS-QTD-FS
                 PERFORM 8100-LER-FATURA    THRU 8100-FIM-LER-FATURA
              WHEN OTHER
                 MOVE SPACES TO REG-DIVERGENCIA
                 SET DV-PAGTO-SEM-FATURA TO TRUE
                 PERFORM 7000-GRAVAR-DIVERGENCIA
                    THRU 7000-FIM-GRAVAR-DIVERGENCIA
                 ADD 1 TO WS-QTD-PS
                 PERFORM 8200-LER-PAGAMENTO THRU 8200-FIM-LER-PAGAMENTO
           END-EVALUATE.

       2000-FIM-CONCILIAR.
           EXIT.

       3000-FINALIZAR.

           CLOSE FATURAS
           IF NOT WS-FS-FAT-OK
              DISPLAY 'FATCONCI - ERRO DE CLOSE FATURAS (FATIN)'
              DISPLAY 'FS: ' WS-FS-FATURA
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR-COM-ERRO
           END-IF

           CLOSE PAGAMENTOS
           IF NOT WS-FS-PAG-OK
              DISPLAY 'FATCONCI - ERRO DE CLOSE PAGAMENTOS (PAGIN)'
              DISPLAY 'FS: ' WS-FS-PAGTO
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR-COM-ERRO
           END-IF

           CLOSE DIVERGENCIAS
           IF NOT WS-FS-DIV-OK
              DISPLAY 'FATCONCI - ERRO DE CLOSE DIVERGENCIAS (DIVEROUT)'
              DISPLAY 'FS: ' WS-FS-DIVER
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR-COM-ERRO
           END-IF

           DISPLAY 'FATCONCI - FATURAS LIDAS .........: '
                   WS-QTD-FAT-LIDAS
           DISPLAY 'FATCONCI - PAGAMENTOS LIDOS ......: '
                   WS-QTD-PAG-LIDOS
           DISPLAY 'FATCONCI - FATURAS BAIXADAS ......: '
                   WS-QTD-BAIXADAS
           DISPLAY 'FATCONCI - PAGAMENTO SEM FATURA ...: ' WS-QTD-PS
           DISPLAY 'FATCONCI - FATURA SEM PAGAMENTO ...: ' WS-QTD-FS
           DISPLAY 'FATCONCI - PAGAMENTO A MENOR ......: ' WS-QTD-PM

      *    CONFERENCIA PELOS DOIS LADOS:
      *    FATURAS    = BAIXADAS + A MENOR + SEM PAGAMENTO
      *    PAGAMENTOS = BAIXADAS + A MENOR + SEM FATURA
           IF WS-QTD-FAT-LIDAS NOT =
                 WS-QTD-BAIXADAS + WS-QTD-PM + WS-QTD-FS
           OR WS-QTD-PAG-LIDOS NOT =
                 WS-QTD-BAIXADAS + WS-QTD-PM + WS-QTD-PS
              DISPLAY 'FATCONCI - CONTADORES NAO FECHAM'
              MOVE 16 TO RETURN-CODE
           END-IF.

       3000-FIM-FINALIZAR.
           EXIT.

       7000-GRAVAR-DIVERGENCIA.
      *    O REGISTRO FOI LIMPO (MOVE SPACES, INCLUSIVE O FILLER) ANTES
      *    DO SET DO TIPO, NO PARAGRAFO QUE CHAMOU ESTE.

           IF DV-PAGTO-SEM-FATURA
              MOVE PG-NUM-CARTAO TO WS-CARTAO-ABERTO
              MOVE ZERO          TO DV-VALOR-FATURA
              MOVE PG-VALOR      TO DV-VALOR-PAGO
           ELSE
              MOVE FT-NUM-CARTAO TO WS-CARTAO-ABERTO
              MOVE FT-VALOR      TO DV-VALOR-FATURA
              IF DV-PAGTO-A-MENOR
                 MOVE PG-VALOR   TO DV-VALOR-PAGO
              ELSE
                 MOVE ZERO       TO DV-VALOR-PAGO
              END-IF
           END-IF
           PERFORM 9800-MASCARAR-CARTAO
           MOVE WS-CARTAO-MASC TO DV-NUM-CARTAO-MASC

           WRITE REG-DIVERGENCIA
           IF NOT WS-FS-DIV-OK
              DISPLAY 'FATCONCI - ERRO DE WRITE DIVERGENCIAS (DIVEROUT)'
              DISPLAY 'FS: ' WS-FS-DIVER
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR-COM-ERRO
           END-IF.

       7000-FIM-GRAVAR-DIVERGENCIA.
           EXIT.

       8100-LER-FATURA.

           READ FATURAS
                AT END
                   MOVE HIGH-VALUES   TO WS-CHAVE-FATURA
                NOT AT END
                   ADD 1              TO WS-QTD-FAT-LIDAS
                   MOVE FT-NUM-CARTAO TO WS-CHAVE-FATURA
           END-READ

           IF NOT WS-FS-FAT-OK AND NOT WS-FS-FAT-FIM
              DISPLAY 'FATCONCI - ERRO DE READ FATURAS (FATIN)'
              DISPLAY 'FS: ' WS-FS-FATURA
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR-COM-ERRO
           END-IF.

       8100-FIM-LER-FATURA.
           EXIT.

       8200-LER-PAGAMENTO.

           READ PAGAMENTOS
                AT END
                   MOVE HIGH-VALUES   TO WS-CHAVE-PAGTO
                NOT AT END
                   ADD 1              TO WS-QTD-PAG-LIDOS
                   MOVE PG-NUM-CARTAO TO WS-CHAVE-PAGTO
           END-READ

           IF NOT WS-FS-PAG-OK AND NOT WS-FS-PAG-FIM
              DISPLAY 'FATCONCI - ERRO DE READ PAGAMENTOS (PAGIN)'
              DISPLAY 'FS: ' WS-FS-PAGTO
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR-COM-ERRO
           END-IF.

       8200-FIM-LER-PAGAMENTO.
           EXIT.

           COPY CPMASCAR.

       9900-ENCERRAR-COM-ERRO.
      *    ENCERRA COM RC 16 PARA IMPEDIR OS STEPS SEGUINTES.
           MOVE 16 TO RETURN-CODE
           STOP RUN.
       9900-FIM-ENCERRAR-COM-ERRO.
           EXIT.

