      *================================================================*
      * PROGRAMA : CICLIQUI (ESQUELETO DA TURMA)                       *
      * AULA     : 3 - CLEARING E SETTLEMENT (DESAFIO)                 *
      * OBJETIVO : CALCULAR A LIQUIDACAO DO EMISSOR COM CADA BANDEIRA: *
      *            - INTERCAMBIO DE CADA TRANSACAO PELA TAXA DO IRD    *
      *              (RECEITA DO EMISSOR);                             *
      *            - TARIFA DE BANDEIRA (CUSTO DO EMISSOR);            *
      *            - POSICAO A PAGAR = VALOR BRUTO - INTERCAMBIO       *
      *              + TARIFA, POR BANDEIRA (QUEBRA DE CONTROLE).      *
      *            CONFERE O TETO REGULATORIO DO INTERCAMBIO DO DEBITO *
      *            (0,50%) E DO PRE-PAGO (0,70%): TAXA ACIMA DO TETO E *
      *            LIMITADA AO TETO E GERA ALERTA.                     *
      * ENTRADAS : DD LANCIN   - CPLANC, FB 120, ORDENADO POR BANDEIRA *
      *                          E IRD                                 *
      *            DD IRDTAB   - CPIRD, FB 80, ORDENADA POR BANDEIRA E *
      *                          IRD                                   *
      * SAIDA    : DD LIQUIOUT - RELATORIO, FBA 133                    *
      * RETORNO  : 0 = OK, 4 = HOUVE ALERTA DE TETO,                   *
      *            8 = IRD NAO CADASTRADO, 16 = ERRO DE ARQUIVO        *
      * A ESCREVER NA AULA 3:                                          *
      * DESAFIO DO FIM DE SEMANA: 2000-PROCESSAR (QUEBRA),             *
      * 2100-CALCULAR E 2200-CONFERIR-TETO                             *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICLIQUI.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LANCAMENTOS ASSIGN TO LANCIN
                  FILE STATUS IS WS-FS-LANCAMENTO.
           SELECT TABELA-IRD  ASSIGN TO IRDTAB
                  FILE STATUS IS WS-FS-IRD.
           SELECT RELATORIO   ASSIGN TO LIQUIOUT
                  FILE STATUS IS WS-FS-RELATORIO.

       DATA DIVISION.
       FILE SECTION.
       FD  LANCAMENTOS
           RECORDING MODE IS F.
           COPY CPLANC.
       FD  TABELA-IRD
           RECORDING MODE IS F.
           COPY CPIRD.
       FD  RELATORIO
           RECORDING MODE IS F.
       01  REG-RELATORIO              PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FS-LANCAMENTO           PIC X(02).
           88  WS-FS-LAN-OK                     VALUE '00'.
           88  WS-FS-LAN-FIM                    VALUE '10'.
       01  WS-FS-IRD                  PIC X(02).
           88  WS-FS-IRD-OK                     VALUE '00'.
           88  WS-FS-IRD-FIM                    VALUE '10'.
       01  WS-FS-RELATORIO            PIC X(02).
           88  WS-FS-REL-OK                     VALUE '00'.

      *--- TABELA DE IRD EM MEMORIA (SEARCH ALL PELA CHAVE) -----------*
       01  WS-MAX-IRD                 PIC 9(03) VALUE 100.
       01  WS-QTD-IRD                 PIC 9(03) VALUE ZERO.
       01  WS-TABELA-IRD.
           05  WS-IRD-ITEM OCCURS 100 TIMES
                           ASCENDING KEY IS WS-IRD-CHAVE
                           INDEXED BY IX-IRD.
               10  WS-IRD-CHAVE.
                   15  WS-IRD-BANDEIRA PIC X(01).
                   15  WS-IRD-CODIGO   PIC X(02).
               10  WS-IRD-PRODUTO     PIC X(01).
               10  WS-IRD-DESCRICAO   PIC X(30).
               10  WS-IRD-TAXA        PIC 9(01)V9(04).
               10  WS-IRD-TARIFA      PIC 9(01)V9(04).
       01  WS-CHAVE-PROCURADA.
           05  WS-CP-BANDEIRA         PIC X(01).
           05  WS-CP-CODIGO           PIC X(02).
       01  WS-ACHOU-IRD               PIC X(01).
           88  WS-IRD-ENCONTRADO                VALUE 'S'.
           88  WS-IRD-NAO-ENCONTRADO            VALUE 'N'.

      *--- TETOS REGULATORIOS DO INTERCAMBIO (RESOLUCAO BCB 246/2022) -*
       01  WS-TETO-DEBITO             PIC 9(01)V9(04) VALUE 0.5000.
       01  WS-TETO-PRE-PAGO           PIC 9(01)V9(04) VALUE 0.7000.
       01  WS-TAXA-APLICADA           PIC 9(01)V9(04).
       01  WS-ACIMA-TETO              PIC X(01).
           88  WS-TAXA-ACIMA-DO-TETO            VALUE 'S'.

      *--- VALORES DE UMA TRANSACAO ----------------------------------*
       01  WS-VL-INTERCAMBIO          PIC 9(09)V99 VALUE ZERO.
       01  WS-VL-TARIFA               PIC 9(09)V99 VALUE ZERO.

      *--- QUEBRA DE CONTROLE: BANDEIRA (MAIOR) E IRD (MENOR) ---------*
       01  WS-BANDEIRA-ANTERIOR       PIC X(01).
       01  WS-IRD-ANTERIOR            PIC X(02).
       01  WS-ACUM-IRD.
           05  WS-AI-QTD              PIC 9(07).
           05  WS-AI-BRUTO            PIC 9(11)V99.
           05  WS-AI-INTERCAMBIO      PIC 9(11)V99.
           05  WS-AI-TARIFA           PIC 9(11)V99.
           05  WS-AI-ALERTA           PIC X(01).
           05  WS-AI-DESCRICAO        PIC X(30).
           05  WS-AI-TAXA             PIC 9(01)V9(04).
       01  WS-ACUM-BANDEIRA.
           05  WS-AB-QTD              PIC 9(07).
           05  WS-AB-BRUTO            PIC 9(11)V99.
           05  WS-AB-INTERCAMBIO      PIC 9(11)V99.
           05  WS-AB-TARIFA           PIC 9(11)V99.
       01  WS-ACUM-GERAL.
           05  WS-AG-QTD              PIC 9(07)    VALUE ZERO.
           05  WS-AG-BRUTO            PIC 9(11)V99 VALUE ZERO.
           05  WS-AG-INTERCAMBIO      PIC 9(11)V99 VALUE ZERO.
           05  WS-AG-TARIFA           PIC 9(11)V99 VALUE ZERO.
       01  WS-POSICAO                 PIC S9(11)V99.

       01  WS-CONTADORES.
           05  WS-QTD-LIDOS           PIC 9(07) VALUE ZERO.
           05  WS-QTD-SEM-IRD         PIC 9(07) VALUE ZERO.
           05  WS-QTD-ACIMA-TETO      PIC 9(07) VALUE ZERO.

      *--- LINHAS DO RELATORIO ----------------------------------------*
       01  WS-CAB-1.
           05  FILLER                 PIC X(60) VALUE
               'CICLO DO CARTAO - LIQUIDACAO DO EMISSOR POR BANDEIRA'.
           05  FILLER                 PIC X(20) VALUE
               '(DADOS FICTICIOS)'.
       01  WS-CAB-2.
           05  FILLER                 PIC X(05) VALUE 'IRD'.
           05  FILLER                 PIC X(32) VALUE 'DESCRICAO'.
           05  FILLER                 PIC X(09) VALUE '  TAXA %'.
           05  FILLER                 PIC X(07) VALUE '    QTD'.
           05  FILLER                 PIC X(18) VALUE
               '      VALOR BRUTO'.
           05  FILLER                 PIC X(16) VALUE
               '     INTERCAMBIO'.
           05  FILLER                 PIC X(16) VALUE
               '    TARIFA BAND.'.
           05  FILLER                 PIC X(20) VALUE '  OBS'.
       01  WS-CAB-BANDEIRA.
           05  FILLER                 PIC X(10) VALUE 'BANDEIRA '.
           05  CBB-BANDEIRA           PIC X(01).
       01  WS-LINHA-IRD.
           05  LI-IRD                 PIC X(02).
           05  FILLER                 PIC X(03) VALUE SPACES.
           05  LI-DESCRICAO           PIC X(30).
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  LI-TAXA                PIC 9.9999.
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  LI-QTD                 PIC ZZ,ZZ9.
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  LI-BRUTO               PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  LI-INTERCAMBIO         PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  LI-TARIFA              PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  LI-OBS                 PIC X(20).
       01  WS-LINHA-TOTAL.
           05  LT-DESCRICAO           PIC X(46).
           05  LT-QTD                 PIC ZZ,ZZ9.
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  LT-BRUTO               PIC ZZ,ZZZ,ZZZ,ZZ9.99.
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  LT-INTERCAMBIO         PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  LT-TARIFA              PIC ZZZ,ZZZ,ZZ9.99.
       01  WS-LINHA-POSICAO.
           05  LP-DESCRICAO           PIC X(46).
           05  FILLER                 PIC X(08) VALUE SPACES.
           05  LP-VALOR               PIC ZZ,ZZZ,ZZZ,ZZ9.99-.
       01  WS-LINHA-ERRO.
           05  FILLER                 PIC X(30) VALUE
               'IRD NAO CADASTRADO: BANDEIRA '.
           05  LE-BANDEIRA            PIC X(01).
           05  FILLER                 PIC X(06) VALUE ' IRD '.
           05  LE-IRD                 PIC X(02).
           05  FILLER                 PIC X(08) VALUE ' RRN '.
           05  LE-RRN                 PIC X(12).

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INICIALIZAR THRU 1000-FIM-INICIALIZAR
           PERFORM 2000-PROCESSAR   THRU 2000-FIM-PROCESSAR
              UNTIL WS-FS-LAN-FIM
           PERFORM 3000-FINALIZAR   THRU 3000-FIM-FINALIZAR
           STOP RUN.

       1000-INICIALIZAR.

           OPEN INPUT TABELA-IRD
           IF NOT WS-FS-IRD-OK
              DISPLAY 'CICLIQUI - ERRO DE OPEN IRDTAB FS: ' WS-FS-IRD
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           OPEN INPUT LANCAMENTOS
           IF NOT WS-FS-LAN-OK
              DISPLAY 'CICLIQUI - ERRO DE OPEN LANCIN FS: '
                      WS-FS-LANCAMENTO
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           OPEN OUTPUT RELATORIO
           IF NOT WS-FS-REL-OK
              DISPLAY 'CICLIQUI - ERRO DE OPEN LIQUIOUT FS: '
                      WS-FS-RELATORIO
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF

           PERFORM 1100-CARREGAR-IRD THRU 1100-FIM-CARREGAR-IRD

           WRITE REG-RELATORIO FROM WS-CAB-1 AFTER ADVANCING PAGE
           WRITE REG-RELATORIO FROM WS-CAB-2 AFTER ADVANCING 2 LINES

           PERFORM 8000-LER-LANCAMENTO THRU 8000-FIM-LER-LANCAMENTO
           IF NOT WS-FS-LAN-FIM
              PERFORM 2800-INICIAR-BANDEIRA
                 THRU 2800-FIM-INICIAR-BANDEIRA
           END-IF
           .
       1000-FIM-INICIALIZAR.
           EXIT.

       1100-CARREGAR-IRD.
           PERFORM VARYING IX-IRD FROM 1 BY 1 UNTIL IX-IRD > WS-MAX-IRD
              MOVE HIGH-VALUES TO WS-IRD-CHAVE(IX-IRD)
           END-PERFORM
           READ TABELA-IRD
           PERFORM UNTIL WS-FS-IRD-FIM
              ADD 1 TO WS-QTD-IRD
              IF WS-QTD-IRD > WS-MAX-IRD
                 DISPLAY 'CICLIQUI - TABELA DE IRD MAIOR QUE O LIMITE'
                 PERFORM 9900-ENCERRAR-COM-ERRO
              END-IF
              MOVE IR-BANDEIRA         TO WS-IRD-BANDEIRA(WS-QTD-IRD)
              MOVE IR-CODIGO           TO WS-IRD-CODIGO(WS-QTD-IRD)
              MOVE IR-PRODUTO          TO WS-IRD-PRODUTO(WS-QTD-IRD)
              MOVE IR-DESCRICAO        TO WS-IRD-DESCRICAO(WS-QTD-IRD)
              MOVE IR-TAXA-INTERCAMBIO TO WS-IRD-TAXA(WS-QTD-IRD)
              MOVE IR-TARIFA-BANDEIRA  TO WS-IRD-TARIFA(WS-QTD-IRD)
              READ TABELA-IRD
           END-PERFORM
           CLOSE TABELA-IRD
           DISPLAY 'CICLIQUI - IRDS CARREGADOS ....: ' WS-QTD-IRD
           .
       1100-FIM-CARREGAR-IRD.
           EXIT.

       2000-PROCESSAR.
      *    A ESCREVER (DESAFIO): A QUEBRA DE CONTROLE. MUDOU A BANDEIRA:
      *    FECHAR O IRD (2600), FECHAR A BANDEIRA (2700) E INICIAR A NOVA
      *    (2800). MUDOU SO O IRD: FECHAR O IRD (2600) E INICIAR O NOVO
      *    (2900).
           PERFORM 2100-CALCULAR THRU 2100-FIM-CALCULAR
           PERFORM 8000-LER-LANCAMENTO THRU 8000-FIM-LER-LANCAMENTO
           .
       2000-FIM-PROCESSAR.
           EXIT.

       2100-CALCULAR.
      *    A ESCREVER (DESAFIO): PROCURAR O IRD DO LANCAMENTO NA TABELA
      *    (SEARCH ALL PELA CHAVE BANDEIRA + IRD). NAO ACHOU: CONTAR E
      *    IMPRIMIR WS-LINHA-ERRO. ACHOU: 2200-CONFERIR-TETO, CALCULAR O
      *    INTERCAMBIO (TAXA APLICADA) E A TARIFA COM ROUNDED E SOMAR NOS
      *    ACUMULADORES DO IRD (WS-AI-...).
           CONTINUE
           .
       2100-FIM-CALCULAR.
           EXIT.

       2200-CONFERIR-TETO.
      *    A ESCREVER (DESAFIO): NO DEBITO (PRODUTO D) O TETO E 0,50% E NO
      *    PRE-PAGO (P), 0,70%. TAXA DA TABELA ACIMA DO TETO: APLICAR O
      *    TETO, CONTAR (WS-QTD-ACIMA-TETO) E MARCAR WS-AI-ALERTA = 'S'.
           MOVE WS-IRD-TAXA(IX-IRD) TO WS-TAXA-APLICADA
           .
       2200-FIM-CONFERIR-TETO.
           EXIT.

       2600-FECHAR-IRD.
           IF WS-AI-QTD > ZERO
              MOVE WS-IRD-ANTERIOR      TO LI-IRD
              MOVE WS-AI-DESCRICAO      TO LI-DESCRICAO
              MOVE WS-AI-TAXA           TO LI-TAXA
              MOVE WS-AI-QTD            TO LI-QTD
              MOVE WS-AI-BRUTO          TO LI-BRUTO
              MOVE WS-AI-INTERCAMBIO    TO LI-INTERCAMBIO
              MOVE WS-AI-TARIFA         TO LI-TARIFA
              IF WS-AI-ALERTA = 'S'
                 MOVE 'LIMITADA AO TETO'      TO LI-OBS
              ELSE
                 MOVE SPACES                  TO LI-OBS
              END-IF
              WRITE REG-RELATORIO FROM WS-LINHA-IRD
                    AFTER ADVANCING 1 LINE
              ADD WS-AI-QTD          TO WS-AB-QTD
              ADD WS-AI-BRUTO        TO WS-AB-BRUTO
              ADD WS-AI-INTERCAMBIO  TO WS-AB-INTERCAMBIO
              ADD WS-AI-TARIFA       TO WS-AB-TARIFA
           END-IF
           .
       2600-FIM-FECHAR-IRD.
           EXIT.

       2700-FECHAR-BANDEIRA.
           MOVE 'TOTAL DA BANDEIRA'   TO LT-DESCRICAO
           MOVE WS-AB-QTD             TO LT-QTD
           MOVE WS-AB-BRUTO           TO LT-BRUTO
           MOVE WS-AB-INTERCAMBIO     TO LT-INTERCAMBIO
           MOVE WS-AB-TARIFA          TO LT-TARIFA
           WRITE REG-RELATORIO FROM WS-LINHA-TOTAL
                 AFTER ADVANCING 2 LINES
      *    O EMISSOR PAGA A BANDEIRA O BRUTO MENOS O INTERCAMBIO QUE
      *    ELE RECEBE, MAIS A TARIFA QUE ELE DEVE
           COMPUTE WS-POSICAO = WS-AB-BRUTO - WS-AB-INTERCAMBIO
                              + WS-AB-TARIFA
           MOVE 'POSICAO DO EMISSOR A PAGAR A BANDEIRA' TO LP-DESCRICAO
           MOVE WS-POSICAO            TO LP-VALOR
           WRITE REG-RELATORIO FROM WS-LINHA-POSICAO
                 AFTER ADVANCING 1 LINE
           DISPLAY 'CICLIQUI - BANDEIRA ' WS-BANDEIRA-ANTERIOR
                   ' A PAGAR: ' LP-VALOR
           ADD WS-AB-QTD          TO WS-AG-QTD
           ADD WS-AB-BRUTO        TO WS-AG-BRUTO
           ADD WS-AB-INTERCAMBIO  TO WS-AG-INTERCAMBIO
           ADD WS-AB-TARIFA       TO WS-AG-TARIFA
           .
       2700-FIM-FECHAR-BANDEIRA.
           EXIT.

       2800-INICIAR-BANDEIRA.
           MOVE LC-BANDEIRA TO WS-BANDEIRA-ANTERIOR CBB-BANDEIRA
           INITIALIZE WS-ACUM-BANDEIRA
           WRITE REG-RELATORIO FROM WS-CAB-BANDEIRA
                 AFTER ADVANCING 2 LINES
           PERFORM 2900-INICIAR-IRD THRU 2900-FIM-INICIAR-IRD
           .
       2800-FIM-INICIAR-BANDEIRA.
           EXIT.

       2900-INICIAR-IRD.
           MOVE LC-IRD TO WS-IRD-ANTERIOR
           INITIALIZE WS-ACUM-IRD
           MOVE 'N'    TO WS-AI-ALERTA
           .
       2900-FIM-INICIAR-IRD.
           EXIT.

       3000-FINALIZAR.
           IF WS-QTD-LIDOS > ZERO
              PERFORM 2600-FECHAR-IRD      THRU 2600-FIM-FECHAR-IRD
              PERFORM 2700-FECHAR-BANDEIRA THRU 2700-FIM-FECHAR-BANDEIRA
           END-IF
           MOVE 'TOTAL GERAL'         TO LT-DESCRICAO
           MOVE WS-AG-QTD             TO LT-QTD
           MOVE WS-AG-BRUTO           TO LT-BRUTO
           MOVE WS-AG-INTERCAMBIO     TO LT-INTERCAMBIO
           MOVE WS-AG-TARIFA          TO LT-TARIFA
           WRITE REG-RELATORIO FROM WS-LINHA-TOTAL
                 AFTER ADVANCING 3 LINES
           COMPUTE WS-POSICAO = WS-AG-BRUTO - WS-AG-INTERCAMBIO
                              + WS-AG-TARIFA
           MOVE 'POSICAO TOTAL DO EMISSOR A PAGAR'   TO LP-DESCRICAO
           MOVE WS-POSICAO            TO LP-VALOR
           WRITE REG-RELATORIO FROM WS-LINHA-POSICAO
                 AFTER ADVANCING 1 LINE
           CLOSE LANCAMENTOS RELATORIO

           DISPLAY 'CICLIQUI - LANCAMENTOS LIDOS ..: ' WS-QTD-LIDOS
           DISPLAY 'CICLIQUI - LIQUIDADOS .........: ' WS-AG-QTD
           DISPLAY 'CICLIQUI - IRD NAO CADASTRADO .: ' WS-QTD-SEM-IRD
           DISPLAY 'CICLIQUI - ACIMA DO TETO ......: ' WS-QTD-ACIMA-TETO
           DISPLAY 'CICLIQUI - POSICAO TOTAL ......: ' LP-VALOR
      *    CONFERENCIA: LIDOS = LIQUIDADOS + SEM IRD
           EVALUATE TRUE
              WHEN WS-QTD-LIDOS NOT = WS-AG-QTD + WS-QTD-SEM-IRD
                 DISPLAY 'CICLIQUI - CONTADORES NAO FECHAM'
                 MOVE 16 TO RETURN-CODE
              WHEN WS-QTD-SEM-IRD > ZERO
                 MOVE 8 TO RETURN-CODE
              WHEN WS-QTD-ACIMA-TETO > ZERO
                 MOVE 4 TO RETURN-CODE
              WHEN OTHER
                 MOVE 0 TO RETURN-CODE
           END-EVALUATE
           .
       3000-FIM-FINALIZAR.
           EXIT.

       8000-LER-LANCAMENTO.
           READ LANCAMENTOS
                AT END
                   CONTINUE
                NOT AT END
                   ADD 1 TO WS-QTD-LIDOS
           END-READ
           IF NOT WS-FS-LAN-OK AND NOT WS-FS-LAN-FIM
              DISPLAY 'CICLIQUI - ERRO DE READ LANCIN FS: '
                      WS-FS-LANCAMENTO
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           .
       8000-FIM-LER-LANCAMENTO.
           EXIT.

       9900-ENCERRAR-COM-ERRO.
      *    ENCERRA COM RC 16 PARA IMPEDIR OS STEPS SEGUINTES
           MOVE 16 TO RETURN-CODE
           STOP RUN
           .
