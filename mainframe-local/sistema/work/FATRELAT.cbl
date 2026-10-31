      *================================================================*
      * PROGRAMA : FATRELAT                                            *
      * AULA     : 8 - COBOL IV, TABELAS INTERNAS E RELATORIOS         *
      * OBJETIVO : EMITIR O RELATORIO DE FATURAS: DETALHE POR          *
      *            TRANSACAO, TOTAL POR CARTAO (QUEBRA DE CONTROLE) E  *
      *            TOTAL GERAL POR CATEGORIA (MCC).                    *
      *            O NUMERO DO CARTAO E SEMPRE EXIBIDO MASCARADO.      *
      * ENTRADAS : DD APROVIN  - CPTRANI, FB 80, ORDENADO POR CARTAO   *
      *            DD MCCTAB   - TABELA MCC, FB 80, ORDENADA POR MCC   *
      * SAIDA    : DD RELATOUT - RELATORIO, FBA 133 (LINHA DE 132 +    *
      *            CARACTERE DE CONTROLE GERADO PELO ADVANCING)        *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FATRELAT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANS-APROVADA ASSIGN TO APROVIN
                  FILE STATUS IS WS-FS-APROVADA.
           SELECT TABELA-MCC     ASSIGN TO MCCTAB
                  FILE STATUS IS WS-FS-MCC.
           SELECT RELATORIO      ASSIGN TO RELATOUT
                  FILE STATUS IS WS-FS-RELATORIO.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANS-APROVADA
           RECORDING MODE IS F.
           COPY CPTRANI.
       FD  TABELA-MCC
           RECORDING MODE IS F.
       01  REG-MCC.
           05  MC-CODIGO              PIC 9(04).
           05  MC-DESCRICAO           PIC X(30).
           05  FILLER                 PIC X(46).
       FD  RELATORIO
           RECORDING MODE IS F.
       01  REG-RELATORIO              PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FS-APROVADA             PIC X(02).
           88  WS-FS-APR-OK                     VALUE '00'.
           88  WS-FS-APR-FIM                    VALUE '10'.
       01  WS-FS-MCC                  PIC X(02).
           88  WS-FS-MCC-OK                     VALUE '00'.
           88  WS-FS-MCC-FIM                    VALUE '10'.
       01  WS-FS-RELATORIO            PIC X(02).
           88  WS-FS-REL-OK                     VALUE '00'.

      *--- TABELA DE MCC EM MEMORIA (CARREGADA DO ARQUIVO MCCTAB) -----*
      *    AS POSICOES NAO USADAS FICAM COM CODIGO 9999 PARA QUE A
      *    TABELA CONTINUE EM ORDEM CRESCENTE (EXIGENCIA DO SEARCH ALL)
       01  WS-MAX-MCC                 PIC 9(03) VALUE 100.
       01  WS-QTD-MCC                 PIC 9(03) VALUE ZERO.
       01  WS-TABELA-MCC.
           05  WS-MCC-ITEM OCCURS 100 TIMES
                           ASCENDING KEY IS WS-MCC-CODIGO
                           INDEXED BY IX-MCC.
               10  WS-MCC-CODIGO      PIC 9(04).
               10  WS-MCC-DESCRICAO   PIC X(30).
               10  WS-MCC-TOTAL       PIC S9(11)V99 COMP-3.
               10  WS-MCC-QTD         PIC 9(07).

      *--- CONTROLE DA QUEBRA -----------------------------------------*
       01  WS-CARTAO-ANTERIOR         PIC X(16).
       01  WS-TOTAL-CARTAO            PIC S9(11)V99 COMP-3.
       01  WS-TOTAL-GERAL             PIC S9(13)V99 COMP-3 VALUE ZERO.
       01  WS-QTD-CARTOES             PIC 9(07) VALUE ZERO.
       01  WS-DESC-CATEGORIA          PIC X(30).

      *--- PAGINACAO --------------------------------------------------*
       01  WS-LINHAS-POR-PAGINA       PIC 9(03) VALUE 50.
       01  WS-CONTA-LINHAS            PIC 9(03) VALUE 99.
       01  WS-PAGINA                  PIC 9(04) VALUE ZERO.

      *--- MASCARAMENTO -----------------------------------------------*
       01  WS-CARTAO-ABERTO           PIC X(16).
       01  WS-CARTAO-MASC             PIC X(16).

      *--- LINHAS DO RELATORIO ----------------------------------------*
       01  WS-CAB-1.
           05  FILLER                 PIC X(40) VALUE
               'FATURA NOTURNA - RELATORIO DE FATURAS'.
           05  FILLER                 PIC X(20) VALUE
               '(DADOS FICTICIOS)'.
           05  FILLER                 PIC X(60) VALUE SPACES.
           05  FILLER                 PIC X(07) VALUE 'PAGINA '.
           05  CB1-PAGINA             PIC ZZZ9.
           05  FILLER                 PIC X(01) VALUE SPACE.
       01  WS-CAB-2.
           05  FILLER                 PIC X(18) VALUE 'CARTAO'.
           05  FILLER                 PIC X(12) VALUE 'DATA'.
           05  FILLER                 PIC X(06) VALUE 'MCC'.
           05  FILLER                 PIC X(32) VALUE 'CATEGORIA'.
           05  FILLER                 PIC X(22) VALUE 'ESTABELECIMENTO'.
           05  FILLER                 PIC X(06) VALUE 'PAIS'.
           05  FILLER                 PIC X(16) VALUE
               '           VALOR'.
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  FILLER                 PIC X(18) VALUE 'OBS'.
       01  WS-DETALHE.
           05  DT-CARTAO              PIC X(16).
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  DT-DATA                PIC 9999/99/99.
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  DT-MCC                 PIC 9(04).
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  DT-CATEGORIA           PIC X(30).
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  DT-ESTABELECIMENTO     PIC X(20).
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  DT-PAIS                PIC X(03).
           05  FILLER                 PIC X(03) VALUE SPACES.
           05  DT-VALOR               PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                 PIC X(04) VALUE SPACES.
           05  DT-OBS                 PIC X(18).
       01  WS-LINHA-TOTAL-CARTAO.
           05  FILLER                 PIC X(18) VALUE SPACES.
           05  FILLER                 PIC X(20) VALUE
               'TOTAL DO CARTAO'.
           05  TC-CARTAO              PIC X(16).
           05  FILLER                 PIC X(43) VALUE SPACES.
           05  TC-VALOR               PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-LINHA-MCC.
           05  FILLER                 PIC X(04) VALUE SPACES.
           05  LM-MCC                 PIC 9(04).
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  LM-DESCRICAO           PIC X(30).
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  LM-QTD                 PIC Z,ZZZ,ZZ9.
           05  FILLER                 PIC X(04) VALUE SPACES.
           05  LM-VALOR               PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-LINHA-GERAL.
           05  FILLER                 PIC X(20) VALUE
               'TOTAL GERAL'.
           05  FILLER                 PIC X(10) VALUE 'CARTOES: '.
           05  LG-QTD-CARTOES         PIC Z,ZZZ,ZZ9.
           05  FILLER                 PIC X(10) VALUE SPACES.
           05  FILLER                 PIC X(08) VALUE 'VALOR: '.
           05  LG-VALOR               PIC ZZZ,ZZZ,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INICIALIZAR
           PERFORM 2000-PROCESSAR-CARTAO UNTIL WS-FS-APR-FIM
           PERFORM 3000-FINALIZAR
           GOBACK.

       1000-INICIALIZAR.
           OPEN INPUT TABELA-MCC
           IF NOT WS-FS-MCC-OK
              DISPLAY 'FATRELAT - ERRO NO OPEN DE MCCTAB. FS='
                      WS-FS-MCC
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           INITIALIZE WS-TABELA-MCC
           PERFORM VARYING IX-MCC FROM 1 BY 1
                   UNTIL IX-MCC > WS-MAX-MCC
              MOVE 9999 TO WS-MCC-CODIGO(IX-MCC)
           END-PERFORM
           PERFORM 1100-CARREGAR-MCC UNTIL WS-FS-MCC-FIM
           CLOSE TABELA-MCC

           OPEN INPUT  TRANS-APROVADA
           IF NOT WS-FS-APR-OK
              DISPLAY 'FATRELAT - ERRO NO OPEN DE APROVIN. FS='
                      WS-FS-APROVADA
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           OPEN OUTPUT RELATORIO
           IF NOT WS-FS-REL-OK
              DISPLAY 'FATRELAT - ERRO NO OPEN DE RELATOUT. FS='
                      WS-FS-RELATORIO
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           PERFORM 8000-LER-APROVADA
           .

       1100-CARREGAR-MCC.
           READ TABELA-MCC
              AT END
                 CONTINUE
              NOT AT END
                 IF WS-QTD-MCC >= WS-MAX-MCC
                    DISPLAY 'FATRELAT - TABELA MCC CHEIA'
                    PERFORM 9900-ENCERRAR-COM-ERRO
                 END-IF
                 ADD 1 TO WS-QTD-MCC
                 MOVE MC-CODIGO    TO WS-MCC-CODIGO(WS-QTD-MCC)
                 MOVE MC-DESCRICAO TO WS-MCC-DESCRICAO(WS-QTD-MCC)
           END-READ
           .

      *----------------------------------------------------------------*
      * QUEBRA DE CONTROLE: UM CICLO DESTE PARAGRAFO = UM CARTAO       *
      *----------------------------------------------------------------*
       2000-PROCESSAR-CARTAO.
           MOVE TI-NUM-CARTAO TO WS-CARTAO-ANTERIOR
           MOVE ZERO          TO WS-TOTAL-CARTAO
           ADD 1              TO WS-QTD-CARTOES
           PERFORM 2100-PROCESSAR-TRANSACAO
              UNTIL WS-FS-APR-FIM
                 OR TI-NUM-CARTAO NOT = WS-CARTAO-ANTERIOR
           PERFORM 2200-IMPRIMIR-TOTAL-CARTAO
           .

       2100-PROCESSAR-TRANSACAO.
           SEARCH ALL WS-MCC-ITEM
              AT END
                 MOVE 'CATEGORIA NAO CADASTRADA' TO WS-DESC-CATEGORIA
              WHEN WS-MCC-CODIGO(IX-MCC) = TI-MCC
                 MOVE WS-MCC-DESCRICAO(IX-MCC)   TO WS-DESC-CATEGORIA
                 ADD TI-VALOR TO WS-MCC-TOTAL(IX-MCC)
                 ADD 1        TO WS-MCC-QTD(IX-MCC)
           END-SEARCH

           MOVE TI-NUM-CARTAO      TO WS-CARTAO-ABERTO
           PERFORM 9800-MASCARAR-CARTAO
           MOVE WS-CARTAO-MASC     TO DT-CARTAO
           MOVE TI-DATA-TRANSACAO  TO DT-DATA
           MOVE TI-MCC             TO DT-MCC
           MOVE WS-DESC-CATEGORIA  TO DT-CATEGORIA
           MOVE TI-ESTABELECIMENTO TO DT-ESTABELECIMENTO
           MOVE TI-PAIS            TO DT-PAIS
           MOVE TI-VALOR           TO DT-VALOR
           IF TI-SUSPEITA
              MOVE 'SUSPEITA'      TO DT-OBS
           ELSE
              MOVE SPACES          TO DT-OBS
           END-IF
           PERFORM 7000-IMPRIMIR-DETALHE

           ADD TI-VALOR TO WS-TOTAL-CARTAO WS-TOTAL-GERAL
           PERFORM 8000-LER-APROVADA
           .

       2200-IMPRIMIR-TOTAL-CARTAO.
           MOVE WS-CARTAO-ANTERIOR TO WS-CARTAO-ABERTO
           PERFORM 9800-MASCARAR-CARTAO
           MOVE WS-CARTAO-MASC     TO TC-CARTAO
           MOVE WS-TOTAL-CARTAO    TO TC-VALOR
           WRITE REG-RELATORIO FROM WS-LINHA-TOTAL-CARTAO
                 AFTER ADVANCING 1 LINE
           MOVE SPACES TO REG-RELATORIO
           WRITE REG-RELATORIO AFTER ADVANCING 1 LINE
           ADD 2 TO WS-CONTA-LINHAS
           .

       3000-FINALIZAR.
           PERFORM 7100-IMPRIMIR-CABECALHO
           MOVE 'TOTAL POR CATEGORIA (MCC)' TO REG-RELATORIO
           WRITE REG-RELATORIO AFTER ADVANCING 2 LINES
           PERFORM VARYING IX-MCC FROM 1 BY 1
                   UNTIL IX-MCC > WS-QTD-MCC
              MOVE WS-MCC-CODIGO(IX-MCC)    TO LM-MCC
              MOVE WS-MCC-DESCRICAO(IX-MCC) TO LM-DESCRICAO
              MOVE WS-MCC-QTD(IX-MCC)       TO LM-QTD
              MOVE WS-MCC-TOTAL(IX-MCC)     TO LM-VALOR
              WRITE REG-RELATORIO FROM WS-LINHA-MCC
                    AFTER ADVANCING 1 LINE
           END-PERFORM
           MOVE WS-QTD-CARTOES TO LG-QTD-CARTOES
           MOVE WS-TOTAL-GERAL TO LG-VALOR
           WRITE REG-RELATORIO FROM WS-LINHA-GERAL
                 AFTER ADVANCING 2 LINES
           CLOSE TRANS-APROVADA RELATORIO
           DISPLAY 'FATRELAT - CARTOES NO RELATORIO: ' WS-QTD-CARTOES
           .

       7000-IMPRIMIR-DETALHE.
           IF WS-CONTA-LINHAS >= WS-LINHAS-POR-PAGINA
              PERFORM 7100-IMPRIMIR-CABECALHO
           END-IF
           WRITE REG-RELATORIO FROM WS-DETALHE
                 AFTER ADVANCING 1 LINE
           ADD 1 TO WS-CONTA-LINHAS
           .

       7100-IMPRIMIR-CABECALHO.
           ADD 1 TO WS-PAGINA
           MOVE WS-PAGINA TO CB1-PAGINA
           WRITE REG-RELATORIO FROM WS-CAB-1 AFTER ADVANCING PAGE
           WRITE REG-RELATORIO FROM WS-CAB-2 AFTER ADVANCING 2 LINES
           MOVE ALL '-' TO REG-RELATORIO
           WRITE REG-RELATORIO AFTER ADVANCING 1 LINE
           MOVE 4 TO WS-CONTA-LINHAS
           .

       8000-LER-APROVADA.
           READ TRANS-APROVADA
              AT END CONTINUE
           END-READ
           IF NOT WS-FS-APR-OK AND NOT WS-FS-APR-FIM
              DISPLAY 'FATRELAT - ERRO NO READ DE APROVIN. FS='
                      WS-FS-APROVADA
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           .

           COPY CPMASCAR.

       9900-ENCERRAR-COM-ERRO.
           MOVE 16 TO RETURN-CODE
           STOP RUN
           .
