      *================================================================*
      * PROGRAMA : PEDEXTRT                                            *
      * SISTEMA  : PEDAGIO NOTURNO (DADOS 100% FICTICIOS)              *
      * OBJETIVO : EMITIR O EXTRATO DIARIO DE PASSAGENS: DETALHE POR   *
      *            PASSAGEM, TOTAL POR TAG (QUEBRA DE CONTROLE) E      *
      *            TOTAL GERAL. A TAG E SEMPRE EXIBIDA MASCARADA       *
      *            (4 PRIMEIROS E 4 ULTIMOS DIGITOS).                  *
      * ENTRADA  : DD APROVIN  - APROVADAS DO PEDVALID, FB 80,         *
      *                          ORDENADAS POR TAG, DATA E HORA        *
      * SAIDA    : DD EXTROUT  - EXTRATO, FBA 133 (LINHA DE 132 +      *
      *                          CARACTERE DE CONTROLE DO ADVANCING)   *
      * RETORNO  : 0 = OK, 16 = ERRO DE ARQUIVO                        *
      * HISTORICO: 01/10/2026 VERSAO INICIAL                           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEDEXTRT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT APROVADA       ASSIGN TO APROVIN
                  FILE STATUS IS WS-FS-APROVADA.
           SELECT EXTRATO        ASSIGN TO EXTROUT
                  FILE STATUS IS WS-FS-EXTRATO.

       DATA DIVISION.
       FILE SECTION.
      *--- PASSAGEM APROVADA (LAYOUT IGUAL AO DO PEDVALID) ------------*
       FD  APROVADA
           RECORDING MODE IS F.
       01  REG-APROVADA.
           05  PA-TAG                 PIC X(12).
           05  PA-PRACA               PIC X(03).
           05  PA-DATA                PIC 9(08).
           05  PA-HORA                PIC 9(06).
           05  PA-CATEGORIA           PIC 9(01).
           05  PA-VALOR               PIC S9(05)V99 COMP-3.
           05  PA-TIPO                PIC X(01).
               88  PA-PAGA                      VALUE 'P'.
               88  PA-ISENTA                    VALUE 'I'.
           05  PA-NOME-PRACA          PIC X(20).
           05  PA-SENTIDO             PIC X(01).
           05  FILLER                 PIC X(24).
       FD  EXTRATO
           RECORDING MODE IS F.
       01  REG-EXTRATO                PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FS-APROVADA             PIC X(02).
           88  WS-FS-APR-OK                     VALUE '00'.
           88  WS-FS-APR-FIM                    VALUE '10'.
       01  WS-FS-EXTRATO              PIC X(02).
           88  WS-FS-EXT-OK                     VALUE '00'.

      *--- CONTROLE DA QUEBRA -----------------------------------------*
       01  WS-TAG-ANTERIOR            PIC X(12).
       01  WS-TOTAL-TAG               PIC S9(07)V99 COMP-3.
       01  WS-PASSAGENS-TAG           PIC 9(05).
       01  WS-TOTAL-GERAL             PIC S9(09)V99 COMP-3 VALUE ZERO.
       01  WS-QTD-TAGS                PIC 9(07) VALUE ZERO.
       01  WS-QTD-PASSAGENS           PIC 9(07) VALUE ZERO.
       01  WS-VALOR-EDITADO           PIC ZZZ,ZZZ,ZZ9.99.

      *--- PAGINACAO --------------------------------------------------*
       01  WS-LINHAS-POR-PAGINA       PIC 9(03) VALUE 50.
       01  WS-CONTA-LINHAS            PIC 9(03) VALUE 99.
       01  WS-PAGINA                  PIC 9(04) VALUE ZERO.

      *--- MASCARAMENTO -----------------------------------------------*
       01  WS-TAG-ABERTA              PIC X(12).
       01  WS-TAG-MASC                PIC X(12).

       01  WS-HORA-AUX.
           05  WS-HH                  PIC X(02).
           05  WS-MM                  PIC X(02).
           05  WS-SS                  PIC X(02).

      *--- LINHAS DO EXTRATO ------------------------------------------*
       01  WS-CAB-1.
           05  FILLER                 PIC X(40) VALUE
               'PEDAGIO NOTURNO - EXTRATO DE PASSAGENS'.
           05  FILLER                 PIC X(20) VALUE
               '(DADOS FICTICIOS)'.
           05  FILLER                 PIC X(60) VALUE SPACES.
           05  FILLER                 PIC X(07) VALUE 'PAGINA '.
           05  CB1-PAGINA             PIC ZZZ9.
           05  FILLER                 PIC X(01) VALUE SPACE.
       01  WS-CAB-2.
           05  FILLER                 PIC X(14) VALUE 'TAG'.
           05  FILLER                 PIC X(12) VALUE 'DATA'.
           05  FILLER                 PIC X(10) VALUE 'HORA'.
           05  FILLER                 PIC X(22) VALUE 'PRACA'.
           05  FILLER                 PIC X(05) VALUE 'CAT'.
           05  FILLER                 PIC X(03) VALUE 'SEN'.
           05  FILLER                 PIC X(09) VALUE '    VALOR'.
           05  FILLER                 PIC X(03) VALUE SPACES.
           05  FILLER                 PIC X(10) VALUE 'OBS'.
       01  WS-DETALHE.
           05  DT-TAG                 PIC X(12).
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  DT-DATA                PIC 9999/99/99.
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  DT-HH                  PIC X(02).
           05  FILLER                 PIC X(01) VALUE ':'.
           05  DT-MM                  PIC X(02).
           05  FILLER                 PIC X(01) VALUE ':'.
           05  DT-SS                  PIC X(02).
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  DT-PRACA               PIC X(20).
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  DT-CATEGORIA           PIC 9(01).
           05  FILLER                 PIC X(04) VALUE SPACES.
           05  DT-SENTIDO             PIC X(01).
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  DT-VALOR               PIC ZZ,ZZ9.99.
           05  FILLER                 PIC X(03) VALUE SPACES.
           05  DT-OBS                 PIC X(10).
       01  WS-LINHA-TOTAL-TAG.
           05  FILLER                 PIC X(14) VALUE SPACES.
           05  FILLER                 PIC X(14) VALUE 'TOTAL DA TAG'.
           05  TT-TAG                 PIC X(12).
           05  FILLER                 PIC X(04) VALUE SPACES.
           05  FILLER                 PIC X(11) VALUE 'PASSAGENS: '.
           05  TT-PASSAGENS           PIC ZZZZ9.
           05  FILLER                 PIC X(13) VALUE SPACES.
           05  FILLER                 PIC X(07) VALUE 'VALOR: '.
           05  TT-VALOR               PIC Z,ZZZ,ZZ9.99.
       01  WS-LINHA-GERAL.
           05  FILLER                 PIC X(14) VALUE 'TOTAL GERAL'.
           05  FILLER                 PIC X(06) VALUE 'TAGS: '.
           05  LG-TAGS                PIC Z,ZZZ,ZZ9.
           05  FILLER                 PIC X(04) VALUE SPACES.
           05  FILLER                 PIC X(11) VALUE 'PASSAGENS: '.
           05  LG-PASSAGENS           PIC Z,ZZZ,ZZ9.
           05  FILLER                 PIC X(04) VALUE SPACES.
           05  FILLER                 PIC X(07) VALUE 'VALOR: '.
           05  LG-VALOR               PIC ZZZ,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INICIALIZAR THRU 1000-FIM-INICIALIZAR
           PERFORM 2000-PROCESSAR-TAG THRU 2000-FIM-PROCESSAR-TAG
              UNTIL WS-FS-APR-FIM
           PERFORM 3000-FINALIZAR THRU 3000-FIM-FINALIZAR
           STOP RUN.
       0000-FIM-PRINCIPAL.
           EXIT.

       1000-INICIALIZAR.
           OPEN INPUT  APROVADA
           IF NOT WS-FS-APR-OK
              DISPLAY 'PEDEXTRT - ERRO NO OPEN DE APROVIN. FS='
                      WS-FS-APROVADA
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR-COM-ERRO
           END-IF
           OPEN OUTPUT EXTRATO
           IF NOT WS-FS-EXT-OK
              DISPLAY 'PEDEXTRT - ERRO NO OPEN DE EXTROUT. FS='
                      WS-FS-EXTRATO
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR-COM-ERRO
           END-IF
      *    LEITURA ANTECIPADA: A PRIMEIRA PASSAGEM E LIDA AQUI
           PERFORM 8000-LER-APROVADA THRU 8000-FIM-LER-APROVADA.
       1000-FIM-INICIALIZAR.
           EXIT.

      *----------------------------------------------------------------*
      * QUEBRA DE CONTROLE: UM CICLO DESTE PARAGRAFO = UMA TAG         *
      *----------------------------------------------------------------*
       2000-PROCESSAR-TAG.
           MOVE PA-TAG TO WS-TAG-ANTERIOR
           MOVE ZERO   TO WS-TOTAL-TAG
           MOVE ZERO   TO WS-PASSAGENS-TAG
           ADD 1       TO WS-QTD-TAGS
           PERFORM 2100-PROCESSAR-PASSAGEM
              THRU 2100-FIM-PROCESSAR-PASSAGEM
              UNTIL WS-FS-APR-FIM
                 OR PA-TAG NOT = WS-TAG-ANTERIOR
           PERFORM 2200-IMPRIMIR-TOTAL-TAG
              THRU 2200-FIM-IMPRIMIR-TOTAL-TAG.
       2000-FIM-PROCESSAR-TAG.
           EXIT.

       2100-PROCESSAR-PASSAGEM.
           ADD PA-VALOR TO WS-TOTAL-TAG WS-TOTAL-GERAL
           ADD 1        TO WS-PASSAGENS-TAG WS-QTD-PASSAGENS

           MOVE PA-TAG             TO WS-TAG-ABERTA
           PERFORM 9800-MASCARAR-TAG THRU 9800-FIM-MASCARAR-TAG
           MOVE WS-TAG-MASC        TO DT-TAG
           MOVE PA-DATA            TO DT-DATA
           MOVE PA-HORA            TO WS-HORA-AUX
           MOVE WS-HH              TO DT-HH
           MOVE WS-MM              TO DT-MM
           MOVE WS-SS              TO DT-SS
           MOVE PA-NOME-PRACA      TO DT-PRACA
           MOVE PA-CATEGORIA       TO DT-CATEGORIA
           MOVE PA-SENTIDO         TO DT-SENTIDO
           MOVE PA-VALOR           TO DT-VALOR
           IF PA-ISENTA
              MOVE 'ISENTA'        TO DT-OBS
           ELSE
              MOVE SPACES          TO DT-OBS
           END-IF
           PERFORM 7000-IMPRIMIR-DETALHE THRU 7000-FIM-IMPRIMIR-DETALHE
           PERFORM 8000-LER-APROVADA THRU 8000-FIM-LER-APROVADA.
       2100-FIM-PROCESSAR-PASSAGEM.
           EXIT.

       2200-IMPRIMIR-TOTAL-TAG.
           MOVE WS-TAG-ANTERIOR    TO WS-TAG-ABERTA
           PERFORM 9800-MASCARAR-TAG THRU 9800-FIM-MASCARAR-TAG
           MOVE WS-TAG-MASC        TO TT-TAG
           MOVE WS-PASSAGENS-TAG   TO TT-PASSAGENS
           MOVE WS-TOTAL-TAG       TO TT-VALOR
           WRITE REG-EXTRATO FROM WS-LINHA-TOTAL-TAG
                 AFTER ADVANCING 1 LINE
           MOVE SPACES TO REG-EXTRATO
           WRITE REG-EXTRATO AFTER ADVANCING 1 LINE
           ADD 2 TO WS-CONTA-LINHAS.
       2200-FIM-IMPRIMIR-TOTAL-TAG.
           EXIT.

       3000-FINALIZAR.
           MOVE WS-QTD-TAGS        TO LG-TAGS
           MOVE WS-QTD-PASSAGENS   TO LG-PASSAGENS
           MOVE WS-TOTAL-GERAL     TO LG-VALOR
           WRITE REG-EXTRATO FROM WS-LINHA-GERAL
                 AFTER ADVANCING 2 LINES
           CLOSE APROVADA
           IF NOT WS-FS-APR-OK
              DISPLAY 'PEDEXTRT - ERRO NO CLOSE DE APROVIN. FS='
                      WS-FS-APROVADA
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR-COM-ERRO
           END-IF
           CLOSE EXTRATO
           IF NOT WS-FS-EXT-OK
              DISPLAY 'PEDEXTRT - ERRO NO CLOSE DE EXTROUT. FS='
                      WS-FS-EXTRATO
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR-COM-ERRO
           END-IF
           MOVE WS-TOTAL-GERAL     TO WS-VALOR-EDITADO
           DISPLAY 'PEDEXTRT - TAGS NO EXTRATO ...: ' WS-QTD-TAGS
           DISPLAY 'PEDEXTRT - PASSAGENS .........: ' WS-QTD-PASSAGENS
           DISPLAY 'PEDEXTRT - VALOR TOTAL .......: ' WS-VALOR-EDITADO
           MOVE 0 TO RETURN-CODE.
       3000-FIM-FINALIZAR.
           EXIT.

       7000-IMPRIMIR-DETALHE.
           IF WS-CONTA-LINHAS >= WS-LINHAS-POR-PAGINA
              PERFORM 7100-IMPRIMIR-CABECALHO
                 THRU 7100-FIM-IMPRIMIR-CABECALHO
           END-IF
           WRITE REG-EXTRATO FROM WS-DETALHE
                 AFTER ADVANCING 1 LINE
           ADD 1 TO WS-CONTA-LINHAS.
       7000-FIM-IMPRIMIR-DETALHE.
           EXIT.

       7100-IMPRIMIR-CABECALHO.
           ADD 1 TO WS-PAGINA
           MOVE WS-PAGINA TO CB1-PAGINA
           WRITE REG-EXTRATO FROM WS-CAB-1 AFTER ADVANCING PAGE
           WRITE REG-EXTRATO FROM WS-CAB-2 AFTER ADVANCING 2 LINES
           MOVE ALL '-' TO REG-EXTRATO
           WRITE REG-EXTRATO AFTER ADVANCING 1 LINE
           MOVE 4 TO WS-CONTA-LINHAS.
       7100-FIM-IMPRIMIR-CABECALHO.
           EXIT.

       8000-LER-APROVADA.
           READ APROVADA
              AT END CONTINUE
           END-READ
           IF NOT WS-FS-APR-OK AND NOT WS-FS-APR-FIM
              DISPLAY 'PEDEXTRT - ERRO NO READ DE APROVIN. FS='
                      WS-FS-APROVADA
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR-COM-ERRO
           END-IF.
       8000-FIM-LER-APROVADA.
           EXIT.

      *    MASCARAMENTO: SO OS 4 PRIMEIROS E OS 4 ULTIMOS DIGITOS
       9800-MASCARAR-TAG.
           MOVE WS-TAG-ABERTA      TO WS-TAG-MASC
           MOVE '****'             TO WS-TAG-MASC(5:4).
       9800-FIM-MASCARAR-TAG.
           EXIT.

       9900-ENCERRAR-COM-ERRO.
           MOVE 16 TO RETURN-CODE
           STOP RUN.
       9900-FIM-ENCERRAR-COM-ERRO.
           EXIT.
