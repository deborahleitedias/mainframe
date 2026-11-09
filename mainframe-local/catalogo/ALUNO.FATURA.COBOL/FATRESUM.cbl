       IDENTIFICATION DIVISION.
       PROGRAM-ID. FATRESUM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DIVERGENCIA
               ASSIGN TO DIVERIN
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FS.

       DATA DIVISION.
       FILE SECTION.

       FD  DIVERGENCIA
           RECORD CONTAINS 80 CHARACTERS.
       01  FD-REGISTRO              PIC X(80).

       WORKING-STORAGE SECTION.

       COPY CPDIVER.

       01  WS-FS                    PIC XX.

       01  WS-QUANTIDADES.
           05 WS-QT-PS              PIC 9(09) VALUE ZERO.
           05 WS-QT-FS              PIC 9(09) VALUE ZERO.
           05 WS-QT-PM              PIC 9(09) VALUE ZERO.
           05 WS-QT-TOTAL           PIC 9(09) VALUE ZERO.

       01  WS-VALORES.
           05 WS-FAT-PS             PIC 9(11)V99 VALUE ZERO.
           05 WS-PAG-PS             PIC 9(11)V99 VALUE ZERO.
           05 WS-FAT-FS             PIC 9(11)V99 VALUE ZERO.
           05 WS-PAG-FS             PIC 9(11)V99 VALUE ZERO.
           05 WS-FAT-PM             PIC 9(11)V99 VALUE ZERO.
           05 WS-PAG-PM             PIC 9(11)V99 VALUE ZERO.

           05 WS-FAT-TOTAL          PIC 9(11)V99 VALUE ZERO.
           05 WS-PAG-TOTAL          PIC 9(11)V99 VALUE ZERO.

       PROCEDURE DIVISION.

       0000-PRINCIPAL.
           PERFORM 1000-INICIALIZAR THRU 1000-FIM-INICIALIZAR
           PERFORM 2000-PROCESSAR THRU 2000-FIM-PROCESSAR
           PERFORM 3000-FINALIZAR THRU 3000-FIM-FINALIZAR
           STOP RUN.

       1000-INICIALIZAR.
           OPEN INPUT DIVERGENCIA

           IF WS-FS NOT = '00'
               DISPLAY 'ERRO NO OPEN: ' WS-FS
               STOP RUN
           END-IF.

       1000-FIM-INICIALIZAR.
           EXIT.

       2000-PROCESSAR.
           PERFORM UNTIL WS-FS = '10'

               READ DIVERGENCIA
                   AT END
                       CONTINUE
                   NOT AT END
                       MOVE FD-REGISTRO TO REG-DIVERGENCIA

                       PERFORM 2100-CLASSIFICAR
                           THRU 2100-FIM
               END-READ

               IF WS-FS NOT = '00' AND WS-FS NOT = '10'
                   DISPLAY 'ERRO NO READ: ' WS-FS
                   STOP RUN
               END-IF

           END-PERFORM.

       2000-FIM-PROCESSAR.
           EXIT.

       2100-CLASSIFICAR.
           EVALUATE TRUE

               WHEN DV-PAGTO-SEM-FATURA
                   ADD 1 TO WS-QT-PS
                   ADD DV-VALOR-FATURA TO WS-FAT-PS
                   ADD DV-VALOR-PAGO TO WS-PAG-PS

               WHEN DV-FATURA-SEM-PAGTO
                   ADD 1 TO WS-QT-FS
                   ADD DV-VALOR-FATURA TO WS-FAT-FS
                   ADD DV-VALOR-PAGO TO WS-PAG-FS

               WHEN DV-PAGTO-A-MENOR
                   ADD 1 TO WS-QT-PM
                   ADD DV-VALOR-FATURA TO WS-FAT-PM
                   ADD DV-VALOR-PAGO TO WS-PAG-PM

               WHEN OTHER
                   DISPLAY 'TIPO DE DIVERGENCIA INVALIDO: '
                       DV-TIPO

           END-EVALUATE.

       2100-FIM.
           EXIT.

       3000-FINALIZAR.

           COMPUTE WS-QT-TOTAL =
               WS-QT-PS + WS-QT-FS + WS-QT-PM

           COMPUTE WS-FAT-TOTAL =
               WS-FAT-PS + WS-FAT-FS + WS-FAT-PM

           COMPUTE WS-PAG-TOTAL =
               WS-PAG-PS + WS-PAG-FS + WS-PAG-PM

           DISPLAY '========================================'
           DISPLAY '       RESUMO DE DIVERGENCIAS'
           DISPLAY '========================================'

           DISPLAY 'TIPO: PS - PAGAMENTO SEM FATURA'
           DISPLAY 'QUANTIDADE: ' WS-QT-PS
           DISPLAY 'VALOR FATURA: ' WS-FAT-PS
           DISPLAY 'VALOR PAGO:   ' WS-PAG-PS

           DISPLAY '----------------------------------------'

           DISPLAY 'TIPO: FS - FATURA SEM PAGAMENTO'
           DISPLAY 'QUANTIDADE: ' WS-QT-FS
           DISPLAY 'VALOR FATURA: ' WS-FAT-FS
           DISPLAY 'VALOR PAGO:   ' WS-PAG-FS

           DISPLAY '----------------------------------------'

           DISPLAY 'TIPO: PM - PAGAMENTO A MENOR'
           DISPLAY 'QUANTIDADE: ' WS-QT-PM
           DISPLAY 'VALOR FATURA: ' WS-FAT-PM
           DISPLAY 'VALOR PAGO:   ' WS-PAG-PM

           DISPLAY '========================================'
           DISPLAY 'TOTAL DE REGISTROS: ' WS-QT-TOTAL
           DISPLAY 'TOTAL VALOR FATURA: ' WS-FAT-TOTAL
           DISPLAY 'TOTAL VALOR PAGO:   ' WS-PAG-TOTAL
           DISPLAY '========================================'

           CLOSE DIVERGENCIA

           IF WS-FS NOT = '00'
               DISPLAY 'ERRO NO CLOSE: ' WS-FS
           END-IF.

       3000-FIM-FINALIZAR.
           EXIT.