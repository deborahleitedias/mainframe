      *================================================================*
      * PROGRAMA : FATFILTR (ESQUELETO PARA A TURMA)                   *
      * AULA     : 7 - COBOL III, ARQUIVOS SEQUENCIAIS                 *
      * OBJETIVO : LER AS TRANSACOES DO DIA (JA ORDENADAS PELO SORT),  *
      *            VALIDAR, CLASSIFICAR E SEPARAR EM DOIS ARQUIVOS:    *
      *            APROVADAS (FORMATO INTERNO, VALOR EM COMP-3) E      *
      *            REJEITADAS (COM MOTIVO).                            *
      * ENTRADA  : DD TRANSIN  - CPTRANS, FB 80                        *
      * SAIDAS   : DD APROVOUT - CPTRANI, FB 80                        *
      *            DD REJEIOUT - CPREJEI, FB 120                       *
      * RETORNO  : 0 = OK, 4 = HOUVE REJEITADAS, 16 = ERRO DE ARQUIVO  *
      *            OU CONTADORES QUE NAO FECHAM                        *
      * O QUE VOCE ESCREVE: A LOGICA DE CADA PARAGRAFO DA PROCEDURE    *
      * DIVISION, NO LUGAR DE CADA CONTINUE. AS DIVISOES DE            *
      * IDENTIFICACAO, AMBIENTE E DADOS JA ESTAO PRONTAS.              *
      * NESTA AULA NAO HA VALIDACAO DO NUMERO DO CARTAO: ELA ENTRA NA  *
      * AULA 9, COM O SUBPROGRAMA LUHNVALD.                            *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FATFILTR.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANS-ENTRADA  ASSIGN TO TRANSIN
                  FILE STATUS IS WS-FS-ENTRADA.
           SELECT TRANS-APROVADA ASSIGN TO APROVOUT
                  FILE STATUS IS WS-FS-APROVADA.
           SELECT TRANS-REJEITADA ASSIGN TO REJEIOUT
                  FILE STATUS IS WS-FS-REJEITADA.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANS-ENTRADA
           RECORDING MODE IS F.
           COPY CPTRANS.
       FD  TRANS-APROVADA
           RECORDING MODE IS F.
           COPY CPTRANI.
       FD  TRANS-REJEITADA
           RECORDING MODE IS F.
           COPY CPREJEI.

       WORKING-STORAGE SECTION.
       01  WS-FS-ENTRADA              PIC X(02).
           88  WS-FS-ENT-OK                     VALUE '00'.
           88  WS-FS-ENT-FIM                    VALUE '10'.
       01  WS-FS-APROVADA             PIC X(02).
           88  WS-FS-APR-OK                     VALUE '00'.
       01  WS-FS-REJEITADA            PIC X(02).
           88  WS-FS-REJ-OK                     VALUE '00'.

       01  WS-CONTADORES.
           05  WS-QTD-LIDOS           PIC 9(07) VALUE ZERO.
           05  WS-QTD-APROVADAS       PIC 9(07) VALUE ZERO.
           05  WS-QTD-SUSPEITAS       PIC 9(07) VALUE ZERO.
           05  WS-QTD-REJEITADAS      PIC 9(07) VALUE ZERO.

       01  WS-LIMITE-SUSPEITA         PIC 9(09)V99 VALUE 5000.00.

       01  WS-VALIDACAO               PIC X(01).
           88  WS-REGISTRO-VALIDO               VALUE 'S'.
           88  WS-REGISTRO-INVALIDO             VALUE 'N'.

       01  WS-DATA-VALIDAR.
           05  WS-DV-ANO              PIC 9(04).
           05  WS-DV-MES              PIC 9(02).
               88  WS-MES-VALIDO                VALUE 01 THRU 12.
           05  WS-DV-DIA              PIC 9(02).
               88  WS-DIA-VALIDO                VALUE 01 THRU 31.
      
       01  WS-PGM-LUHN                PIC X(08) VALUE 'LUHNVALD'.
           COPY CPLUHN.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.

           PERFORM 1000-INICIALIZAR
              THRU 1000-FIM-INICIALIZAR

           PERFORM 2000-PROCESSAR 
              THRU 2000-FIM-PROCESSAR
              UNTIL WS-FS-ENT-FIM

           PERFORM 3000-FINALIZAR
              THRU 3000-FIM-FINALIZAR

           STOP RUN.

       1000-INICIALIZAR.

           OPEN INPUT TRANS-ENTRADA
           IF NOT WS-FS-ENT-OK
              DISPLAY 'FATFILTR - ERRO DE OPEN TRANSIN'
              DISPLAY 'FS: ' WS-FS-ENTRADA
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF

           OPEN OUTPUT TRANS-APROVADA
           IF NOT WS-FS-APR-OK
              DISPLAY 'FATFILTR - ERRO DE OPEN APROVOUT'
              DISPLAY 'FS: ' WS-FS-APROVADA
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF

           OPEN OUTPUT TRANS-REJEITADA
           IF NOT WS-FS-REJ-OK
              DISPLAY 'FATFILTR - ERRO DE OPEN REJEIOUT'
                      'FS: ' WS-FS-REJEITADA
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF

           PERFORM 8000-LER-TRANSACAO THRU 8000-FIM-LER-TRANSACAO.
        
       1000-FIM-INICIALIZAR. EXIT.   

       2000-PROCESSAR.
           
           PERFORM 2100-VALIDAR-TRANSACAO
           IF WS-REGISTRO-VALIDO
              PERFORM 2200-GRAVAR-APROVADA
           ELSE
              PERFORM 2300-GRAVAR-REJEITADA 
           END-IF

           PERFORM 8000-LER-TRANSACAO THRU 8000-FIM-LER-TRANSACAO.
           
       2000-FIM-PROCESSAR. EXIT.

       2100-VALIDAR-TRANSACAO.

           MOVE SPACES TO REG-REJEITADA
           SET WS-REGISTRO-VALIDO TO TRUE
             
           MOVE TR-NUM-CARTAO     TO LK-NUM-CARTAO
           CALL WS-PGM-LUHN USING LK-AREA-LUHN

           EVALUATE TRUE

           WHEN LK-CARTAO-INVALIDO
                SET WS-REGISTRO-INVALIDO TO TRUE
                SET RJ-CARTAO-INVALIDO   TO TRUE
                 MOVE 'CARTAO COM DIGITO INVALIDO'
                                         TO RJ-DESC-MOTIVO   

              WHEN TR-VALOR IS NOT NUMERIC
                   SET WS-REGISTRO-INVALIDO TO TRUE
                   SET RJ-VALOR-INVALIDO TO TRUE
                   MOVE 'VALOR NAO NUMERICO'
                       TO RJ-DESC-MOTIVO

              WHEN TR-VALOR = ZERO
                   SET WS-REGISTRO-INVALIDO TO TRUE
                   SET RJ-VALOR-INVALIDO TO TRUE
                   MOVE 'VALOR ZERADO'
                       TO RJ-DESC-MOTIVO

              WHEN TR-DATA-TRANSACAO IS NOT NUMERIC
                   SET WS-REGISTRO-INVALIDO TO TRUE
                   SET RJ-DATA-INVALIDA TO TRUE
                   MOVE 'DATA INVALIDA'
                       TO RJ-DESC-MOTIVO

              WHEN OTHER
                   IF FUNCTION TEST-DATE-YYYYMMDD(
                       TR-DATA-TRANSACAO) NOT = ZERO
                      SET WS-REGISTRO-INVALIDO TO TRUE
                      SET RJ-DATA-INVALIDA TO TRUE
                      MOVE 'DATA INVALIDA'
                          TO RJ-DESC-MOTIVO
                   ELSE
                      IF TR-NEGADA
                         SET WS-REGISTRO-INVALIDO TO TRUE
                         SET RJ-TRANSACAO-NEGADA TO TRUE
                         MOVE 'TRANSACAO NEGADA'
                             TO RJ-DESC-MOTIVO
                      END-IF
                   END-IF

           END-EVALUATE
           .

       2100-FIM-VALIDAR-TRANSACAO. EXIT.     

       2200-GRAVAR-APROVADA.

           MOVE SPACES TO REG-TRANS-INTERNA
           INITIALIZE REG-TRANS-INTERNA

           MOVE TR-NUM-CARTAO
               TO TI-NUM-CARTAO
           MOVE TR-DATA-TRANSACAO
               TO TI-DATA-TRANSACAO
           MOVE TR-HORA-TRANSACAO
               TO TI-HORA-TRANSACAO
           MOVE TR-VALOR
               TO TI-VALOR
           MOVE TR-MCC
               TO TI-MCC
           MOVE TR-PAIS
               TO TI-PAIS
           MOVE TR-COD-AUTORIZACAO
               TO TI-COD-AUTORIZACAO
           MOVE TR-ESTABELECIMENTO
               TO TI-ESTABELECIMENTO

           IF NOT TR-NACIONAL
              AND TR-VALOR > WS-LIMITE-SUSPEITA
              SET TI-SUSPEITA TO TRUE
              ADD 1 TO WS-QTD-SUSPEITAS
           ELSE
              SET TI-NORMAL TO TRUE
           END-IF

           WRITE REG-TRANS-INTERNA

           IF NOT WS-FS-APR-OK
              DISPLAY 'FATFILTR - ERRO DE WRITE APROVOUT'
              DISPLAY 'FS: ' WS-FS-APROVADA
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF

           ADD 1 TO WS-QTD-APROVADAS
           .

       2200-FIM-GRAVAR-APROVADA. EXIT.

       2300-GRAVAR-REJEITADA.

           MOVE REG-TRANSACAO
               TO RJ-REGISTRO-ORIGINAL

           WRITE REG-REJEITADA

           IF NOT WS-FS-REJ-OK
              DISPLAY 'FATFILTR - ERRO DE WRITE REJEIOUT'
              DISPLAY 'FS: ' WS-FS-REJEITADA
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF

           ADD 1 TO WS-QTD-REJEITADAS
           .

       2300-FIM-GRAVAR-REJEITADA. EXIT.  

       3000-FINALIZAR.

           CLOSE TRANS-ENTRADA
           CLOSE TRANS-APROVADA
           CLOSE TRANS-REJEITADA

           DISPLAY '--------------------------------'
           DISPLAY 'FATFILTR - RESUMO DO PROCESSAMENTO'
           DISPLAY '--------------------------------'
           DISPLAY 'REGISTROS LIDOS:      ' WS-QTD-LIDOS
           DISPLAY 'REGISTROS APROVADOS:  ' WS-QTD-APROVADAS
           DISPLAY 'REGISTROS SUSPEITOS:  ' WS-QTD-SUSPEITAS
           DISPLAY 'REGISTROS REJEITADOS: ' WS-QTD-REJEITADAS

           IF WS-QTD-LIDOS NOT =
              WS-QTD-APROVADAS + WS-QTD-REJEITADAS
              DISPLAY 'ERRO: CONTADORES NAO CONFEREM'
              MOVE 16 TO RETURN-CODE
           ELSE
              IF WS-QTD-REJEITADAS > ZERO
                 MOVE 4 TO RETURN-CODE
              ELSE
                 MOVE 0 TO RETURN-CODE
              END-IF
           END-IF
           .

       3000-FIM-FINALIZAR. EXIT.

       8000-LER-TRANSACAO.

           READ TRANS-ENTRADA
                AT END 
                   CONTINUE
                NOT AT END
                   ADD 1 TO WS-QTD-LIDOS
           END-READ

           IF NOT WS-FS-ENT-OK AND NOT WS-FS-ENT-FIM 
              DISPLAY 'FATFILTR - ERRO DE READ TRANS-ENTRADA (TRANSIN)'
                      'FS: ' WS-FS-ENTRADA 
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           .
       8000-FIM-LER-TRANSACAO. EXIT.

       9900-ENCERRAR-COM-ERRO.
           MOVE 16 TO RETURN-CODE
           STOP RUN
           .
