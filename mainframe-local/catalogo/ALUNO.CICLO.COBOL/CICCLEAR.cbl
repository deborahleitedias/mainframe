      *================================================================*
      * PROGRAMA : CICCLEAR (ESQUELETO DA TURMA)                       *
      * AULA     : 3 - CLEARING E SETTLEMENT                           *
      * OBJETIVO : PROCESSAR O CLEARING RECEBIDO DAS BANDEIRAS:        *
      *            1. CONFERIR OS TOTAIS DE CONTROLE (TRAILER) ANTES   *
      *               DE PROCESSAR. SE NAO FECHAM, NADA E PROCESSADO.  *
      *            2. CASAR CADA APRESENTACAO COM A AUTORIZACAO        *
      *               (BALANCE LINE POR CARTAO + CODIGO DE AUTORIZ.):  *
      *               CASADA, DIFERENCA ACEITA, DIFERENCA FORA DA      *
      *               TOLERANCIA, SEM AUTORIZACAO, DUPLICADA E         *
      *               AUTORIZACAO SEM APRESENTACAO.                    *
      *            3. LANCAR NA CONTA (LANCAMENTOS) E GERAR AS         *
      *               TRANSACOES DE CREDITO PARA A FATURA NOTURNA.     *
      * ENTRADAS : DD AUTLOGIN - CPAUTLOG, FB 120, ORDENADO POR CARTAO *
      *                          E CODIGO DE AUTORIZACAO               *
      *            DD CLEARIN  - CPCLEAR, FB 80, HEADERS, DETALHES     *
      *                          ORDENADOS COMO O LOG, E TRAILERS      *
      * SAIDAS   : DD LANCOUT  - CPLANC, FB 120                        *
      *            DD FATOUT   - CPTRANS, FB 80 (SO CREDITO)           *
      *            DD EXCECOUT - CPEXCEC, FB 120                       *
      * RETORNO  : 0 = SEM EXCECOES, 4 = HOUVE EXCECOES,               *
      *            8 = TOTAIS DO TRAILER NAO CONFEREM, 16 = ERRO       *
      * A ESCREVER NA AULA 3:                                          *
      * 2000-CASAR, O BALANCE LINE (AO VIVO, COM A TURMA)              *
      * 2200-CONFERIR-VALOR, A TOLERANCIA (MAO NA MASSA)               *
      * CONFERENCIA DO TRAILER NO 1100 (MAO NA MASSA)                  *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICCLEAR.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOG-AUTORIZ  ASSIGN TO AUTLOGIN
                  FILE STATUS IS WS-FS-LOG.
           SELECT CLEARING     ASSIGN TO CLEARIN
                  FILE STATUS IS WS-FS-CLEARING.
           SELECT LANCAMENTOS  ASSIGN TO LANCOUT
                  FILE STATUS IS WS-FS-LANCAMENTO.
           SELECT TRANS-FATURA ASSIGN TO FATOUT
                  FILE STATUS IS WS-FS-FATURA.
           SELECT EXCECOES     ASSIGN TO EXCECOUT
                  FILE STATUS IS WS-FS-EXCECAO.

       DATA DIVISION.
       FILE SECTION.
       FD  LOG-AUTORIZ
           RECORDING MODE IS F.
           COPY CPAUTLOG.
       FD  CLEARING
           RECORDING MODE IS F.
           COPY CPCLEAR.
       FD  LANCAMENTOS
           RECORDING MODE IS F.
           COPY CPLANC.
       FD  TRANS-FATURA
           RECORDING MODE IS F.
           COPY CPTRANS.
       FD  EXCECOES
           RECORDING MODE IS F.
           COPY CPEXCEC.

       WORKING-STORAGE SECTION.
       01  WS-FS-LOG                  PIC X(02).
           88  WS-FS-LOG-OK                     VALUE '00'.
           88  WS-FS-LOG-FIM                    VALUE '10'.
       01  WS-FS-CLEARING             PIC X(02).
           88  WS-FS-CLR-OK                     VALUE '00'.
           88  WS-FS-CLR-FIM                    VALUE '10'.
       01  WS-FS-LANCAMENTO           PIC X(02).
           88  WS-FS-LAN-OK                     VALUE '00'.
       01  WS-FS-FATURA               PIC X(02).
           88  WS-FS-FAT-OK                     VALUE '00'.
       01  WS-FS-EXCECAO              PIC X(02).
           88  WS-FS-EXC-OK                     VALUE '00'.

      *--- CHAVES DO BALANCE LINE (HIGH-VALUES NO FIM DO ARQUIVO) -----*
       01  WS-CHAVE-AUT.
           05  WS-CA-CARTAO           PIC X(16).
           05  WS-CA-CODIGO           PIC X(06).
       01  WS-CHAVE-CLR.
           05  WS-CC-CARTAO           PIC X(16).
           05  WS-CC-CODIGO           PIC X(06).
      *    A AUTORIZACAO ATUAL JA FOI CASADA COM UMA APRESENTACAO?
       01  WS-AUT-CASADA              PIC X(01).
           88  WS-AUTORIZACAO-CASADA            VALUE 'S'.
           88  WS-AUTORIZACAO-LIVRE             VALUE 'N'.

      *--- TOTAIS DE CONTROLE POR BANDEIRA (1 = ALFA, 2 = BETA) -------*
       01  WS-CONTROLE.
           05  WS-CTL-ITEM OCCURS 2 TIMES INDEXED BY IX-CTL.
               10  WS-CTL-BANDEIRA    PIC X(01).
               10  WS-CTL-QTD-DET     PIC 9(07).
               10  WS-CTL-VALOR-DET   PIC 9(11)V99.
               10  WS-CTL-QTD-TRL     PIC 9(07).
               10  WS-CTL-VALOR-TRL   PIC 9(11)V99.
               10  WS-CTL-TEM-TRAILER PIC X(01).
       01  WS-TOTAIS-CONFEREM         PIC X(01).
           88  WS-TOTAIS-OK                     VALUE 'S'.
           88  WS-TOTAIS-ERRADOS                VALUE 'N'.

       01  WS-DATA-PROCESSAMENTO      PIC 9(08) VALUE ZERO.
       01  WS-DIAS                    PIC S9(05).
       01  WS-PRAZO-APRESENTACAO      PIC 9(03) VALUE 10.
       01  WS-PRAZO-EXPIRACAO         PIC 9(03) VALUE 7.
       01  WS-LIMITE-TOLERANCIA       PIC 9(09)V99.

       01  WS-RESULTADO-VALOR         PIC X(01).
           88  WS-VALOR-IGUAL                   VALUE 'I'.
           88  WS-VALOR-ACEITO                  VALUE 'A'.
           88  WS-VALOR-FORA                    VALUE 'F'.

       01  WS-CONTADORES.
           05  WS-QTD-LOG-LIDOS       PIC 9(07) VALUE ZERO.
           05  WS-QTD-LOG-USADOS      PIC 9(07) VALUE ZERO.
           05  WS-QTD-DETALHES        PIC 9(07) VALUE ZERO.
           05  WS-QTD-CASADAS         PIC 9(07) VALUE ZERO.
           05  WS-QTD-DIF-ACEITA      PIC 9(07) VALUE ZERO.
           05  WS-QTD-DIF-FORA        PIC 9(07) VALUE ZERO.
           05  WS-QTD-SEM-AUT         PIC 9(07) VALUE ZERO.
           05  WS-QTD-DUPLICADAS      PIC 9(07) VALUE ZERO.
           05  WS-QTD-TARDIAS         PIC 9(07) VALUE ZERO.
           05  WS-QTD-AUT-PENDENTE    PIC 9(07) VALUE ZERO.
           05  WS-QTD-AUT-EXPIRADA    PIC 9(07) VALUE ZERO.
           05  WS-QTD-LANCAMENTOS     PIC 9(07) VALUE ZERO.
           05  WS-QTD-FATURA          PIC 9(07) VALUE ZERO.
           05  WS-QTD-EXCECOES        PIC 9(07) VALUE ZERO.
           05  WS-QTD-ESPERADA        PIC 9(07) VALUE ZERO.
       01  WS-VALOR-LANCADO           PIC 9(11)V99 VALUE ZERO.
       01  WS-VALOR-EDITADO           PIC Z,ZZZ,ZZZ,ZZ9.99.

       01  WS-CARTAO-ABERTO           PIC X(16).
       01  WS-CARTAO-MASC             PIC X(16).

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INICIALIZAR THRU 1000-FIM-INICIALIZAR
           PERFORM 2000-CASAR       THRU 2000-FIM-CASAR
              UNTIL WS-CHAVE-AUT = HIGH-VALUES
                AND WS-CHAVE-CLR = HIGH-VALUES
           PERFORM 3000-FINALIZAR   THRU 3000-FIM-FINALIZAR
           STOP RUN.

       1000-INICIALIZAR.
      *    PRIMEIRA PASSADA: SO CONFERE OS TOTAIS DE CONTROLE
           PERFORM 1100-CONFERIR-TRAILERS
              THRU 1100-FIM-CONFERIR-TRAILERS
           IF WS-TOTAIS-ERRADOS
              DISPLAY 'CICCLEAR - TOTAIS DO TRAILER NAO CONFEREM: '
                      'ARQUIVO NAO PROCESSADO'
              MOVE 8 TO RETURN-CODE
              STOP RUN
           END-IF

      *    SEGUNDA PASSADA: O PROCESSAMENTO
           OPEN INPUT CLEARING
           IF NOT WS-FS-CLR-OK
              DISPLAY 'CICCLEAR - ERRO DE OPEN CLEARIN FS: '
                      WS-FS-CLEARING
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           OPEN INPUT LOG-AUTORIZ
           IF NOT WS-FS-LOG-OK
              DISPLAY 'CICCLEAR - ERRO DE OPEN AUTLOGIN FS: ' WS-FS-LOG
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           OPEN OUTPUT LANCAMENTOS
           IF NOT WS-FS-LAN-OK
              DISPLAY 'CICCLEAR - ERRO DE OPEN LANCOUT FS: '
                      WS-FS-LANCAMENTO
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           OPEN OUTPUT TRANS-FATURA
           IF NOT WS-FS-FAT-OK
              DISPLAY 'CICCLEAR - ERRO DE OPEN FATOUT FS: '
                      WS-FS-FATURA
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           OPEN OUTPUT EXCECOES
           IF NOT WS-FS-EXC-OK
              DISPLAY 'CICCLEAR - ERRO DE OPEN EXCECOUT FS: '
                      WS-FS-EXCECAO
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF

           SET WS-AUTORIZACAO-LIVRE TO TRUE
           PERFORM 8100-LER-AUTORIZACAO THRU 8100-FIM-LER-AUTORIZACAO
           PERFORM 8200-LER-APRESENTACAO
              THRU 8200-FIM-LER-APRESENTACAO
           .
       1000-FIM-INICIALIZAR.
           EXIT.

       1100-CONFERIR-TRAILERS.
           INITIALIZE WS-CONTROLE
           MOVE 'A' TO WS-CTL-BANDEIRA(1)
           MOVE 'B' TO WS-CTL-BANDEIRA(2)
           MOVE 'N' TO WS-CTL-TEM-TRAILER(1) WS-CTL-TEM-TRAILER(2)
           SET WS-TOTAIS-OK TO TRUE
           OPEN INPUT CLEARING
           IF NOT WS-FS-CLR-OK
              DISPLAY 'CICCLEAR - ERRO DE OPEN CLEARIN FS: '
                      WS-FS-CLEARING
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           READ CLEARING
           PERFORM UNTIL WS-FS-CLR-FIM
              EVALUATE TRUE
                 WHEN CL-HEADER
                    MOVE CL-H-DATA-PROC TO WS-DATA-PROCESSAMENTO
                 WHEN CL-DETALHE
                    SET IX-CTL TO 1
                    SEARCH WS-CTL-ITEM
                       AT END
                          DISPLAY 'CICCLEAR - BANDEIRA DESCONHECIDA: '
                                  CL-BANDEIRA
                          SET WS-TOTAIS-ERRADOS TO TRUE
                       WHEN WS-CTL-BANDEIRA(IX-CTL) = CL-BANDEIRA
                          ADD 1        TO WS-CTL-QTD-DET(IX-CTL)
                          ADD CL-VALOR TO WS-CTL-VALOR-DET(IX-CTL)
                    END-SEARCH
                 WHEN CL-TRAILER
                    SET IX-CTL TO 1
                    SEARCH WS-CTL-ITEM
                       AT END
                          SET WS-TOTAIS-ERRADOS TO TRUE
                       WHEN WS-CTL-BANDEIRA(IX-CTL) = CL-T-BANDEIRA
                          MOVE CL-T-QTD   TO WS-CTL-QTD-TRL(IX-CTL)
                          MOVE CL-T-VALOR TO WS-CTL-VALOR-TRL(IX-CTL)
                          MOVE 'S'        TO WS-CTL-TEM-TRAILER(IX-CTL)
                    END-SEARCH
              END-EVALUATE
              READ CLEARING
           END-PERFORM
           CLOSE CLEARING

      *    A ESCREVER (MAO NA MASSA): PARA CADA BANDEIRA (IX-CTL DE 1 A 2),
      *    MOSTRAR A QUANTIDADE DOS DETALHES E A DO TRAILER E MARCAR
      *    WS-TOTAIS-ERRADOS SE NAO HOUVE TRAILER OU SE A QUANTIDADE OU O
      *    VALOR NAO BATEM. COM ISSO, UM ARQUIVO ERRADO NAO E PROCESSADO.
           DISPLAY 'CICCLEAR - DATA DE PROCESSAMENTO: '
                   WS-DATA-PROCESSAMENTO
           .
       1100-FIM-CONFERIR-TRAILERS.
           EXIT.

       2000-CASAR.
      *    A ESCREVER (AO VIVO): O BALANCE LINE ENTRE WS-CHAVE-AUT E
      *    WS-CHAVE-CLR. IGUAIS: SE A AUTORIZACAO JA FOI CASADA, 2400-
      *    DUPLICADA; SENAO, MARCAR CASADA E 2100-CASADA; LER SO A
      *    APRESENTACAO. AUT MENOR: SE LIVRE, 2500-SEM-APRESENTACAO; LIBERAR
      *    E LER A AUTORIZACAO. CLR MENOR: 2300-SEM-AUTORIZACAO E LER A
      *    APRESENTACAO. ENQUANTO ISSO NAO EXISTE, O PROGRAMA TERMINA NA
      *    PRIMEIRA VOLTA, COM RC 16 (CLASSES NAO FECHAM).
           MOVE HIGH-VALUES TO WS-CHAVE-AUT WS-CHAVE-CLR
           .
       2000-FIM-CASAR.
           EXIT.

       2100-CASADA.
           PERFORM 2200-CONFERIR-VALOR THRU 2200-FIM-CONFERIR-VALOR
           EVALUATE TRUE
              WHEN WS-VALOR-IGUAL
                 ADD 1 TO WS-QTD-CASADAS
                 PERFORM 7000-MONTAR-LANCAMENTO
                    THRU 7000-FIM-MONTAR-LANCAMENTO
                 PERFORM 7050-GRAVAR-LANCAMENTO
                    THRU 7050-FIM-GRAVAR-LANCAMENTO
              WHEN WS-VALOR-ACEITO
                 ADD 1 TO WS-QTD-DIF-ACEITA
                 PERFORM 7000-MONTAR-LANCAMENTO
                    THRU 7000-FIM-MONTAR-LANCAMENTO
                 SET LC-DIFERENCA-ACEITA TO TRUE
                 PERFORM 7050-GRAVAR-LANCAMENTO
                    THRU 7050-FIM-GRAVAR-LANCAMENTO
              WHEN WS-VALOR-FORA
      *          NAO LANCA: A DIFERENCA VAI PARA ANALISE
                 ADD 1 TO WS-QTD-DIF-FORA
                 MOVE SPACES TO REG-EXCECAO
                 SET EX-DIFERENCA-FORA TO TRUE
                 MOVE 'VALOR APRESENTADO FORA DA TOLERANCIA'
                                       TO EX-DESCRICAO
                 PERFORM 7100-GRAVAR-EXCECAO-CLR
                    THRU 7100-FIM-GRAVAR-EXCECAO-CLR
           END-EVALUATE
           .
       2100-FIM-CASADA.
           EXIT.

       2200-CONFERIR-VALOR.
      *    A ESCREVER (MAO NA MASSA): A REGRA DE TOLERANCIA DO PROJETO.
      *    MENOR QUE O AUTORIZADO: ACEITO. MAIOR: SO NO HOTEL (MCC 7011, ATE
      *    15%) E NO RESTAURANTE (MCC 5812, ATE 20%). HOJE SO O VALOR IGUAL
      *    PASSA: QUALQUER DIFERENCA VAI PARA A EXCECAO.
           IF CL-VALOR = LG-VALOR
              SET WS-VALOR-IGUAL TO TRUE
           ELSE
              SET WS-VALOR-FORA  TO TRUE
           END-IF
           .
       2200-FIM-CONFERIR-VALOR.
           EXIT.

       2300-SEM-AUTORIZACAO.
      *    A BANDEIRA COBRA DO EMISSOR MESMO ASSIM: LANCA E REGISTRA
      *    PARA UMA POSSIVEL DISPUTA (SEM AUTORIZACAO)
           ADD 1 TO WS-QTD-SEM-AUT
           PERFORM 7000-MONTAR-LANCAMENTO
              THRU 7000-FIM-MONTAR-LANCAMENTO
           SET LC-SEM-AUTORIZACAO TO TRUE
           PERFORM 7050-GRAVAR-LANCAMENTO
              THRU 7050-FIM-GRAVAR-LANCAMENTO
           MOVE SPACES TO REG-EXCECAO
           SET EX-SEM-AUTORIZACAO TO TRUE
           MOVE 'APRESENTACAO SEM AUTORIZACAO' TO EX-DESCRICAO
           PERFORM 7100-GRAVAR-EXCECAO-CLR
              THRU 7100-FIM-GRAVAR-EXCECAO-CLR
           .
       2300-FIM-SEM-AUTORIZACAO.
           EXIT.

       2400-DUPLICADA.
      *    TAMBEM E LANCADA (A BANDEIRA LIQUIDA AS DUAS) E REGISTRADA
           ADD 1 TO WS-QTD-DUPLICADAS
           PERFORM 7000-MONTAR-LANCAMENTO
              THRU 7000-FIM-MONTAR-LANCAMENTO
           SET LC-DUPLICADA TO TRUE
           PERFORM 7050-GRAVAR-LANCAMENTO
              THRU 7050-FIM-GRAVAR-LANCAMENTO
           MOVE SPACES TO REG-EXCECAO
           SET EX-DUPLICADA TO TRUE
           MOVE 'APRESENTACAO DUPLICADA DA MESMA AUTORIZ.'
                                       TO EX-DESCRICAO
           PERFORM 7100-GRAVAR-EXCECAO-CLR
              THRU 7100-FIM-GRAVAR-EXCECAO-CLR
           .
       2400-FIM-DUPLICADA.
           EXIT.

       2500-SEM-APRESENTACAO.
      *    AUTORIZADA E NAO APRESENTADA. DEPOIS DE 7 DIAS (REGRA
      *    FICTICIA) A AUTORIZACAO EXPIRA E O LIMITE DEVE SER LIBERADO
           COMPUTE WS-DIAS = FUNCTION INTEGER-OF-DATE
                             (WS-DATA-PROCESSAMENTO)
                           - FUNCTION INTEGER-OF-DATE(LG-DATA)
           MOVE SPACES TO REG-EXCECAO
           SET EX-AUT-SEM-APRESENTACAO TO TRUE
           IF WS-DIAS > WS-PRAZO-EXPIRACAO
              ADD 1 TO WS-QTD-AUT-EXPIRADA
              MOVE 'AUTORIZACAO EXPIRADA: LIBERAR LIMITE'
                                       TO EX-DESCRICAO
           ELSE
              ADD 1 TO WS-QTD-AUT-PENDENTE
              MOVE 'AUTORIZACAO AINDA SEM APRESENTACAO'
                                       TO EX-DESCRICAO
           END-IF
           MOVE LG-NUM-CARTAO          TO EX-NUM-CARTAO
           MOVE LG-COD-AUTORIZACAO     TO EX-COD-AUTORIZACAO
           MOVE LG-DATA                TO EX-DATA-COMPRA
           MOVE ZERO                   TO EX-VALOR-APRESENTADO
           MOVE LG-VALOR               TO EX-VALOR-AUTORIZADO
           MOVE WS-DIAS                TO EX-DIAS
           PERFORM 7200-GRAVAR-EXCECAO THRU 7200-FIM-GRAVAR-EXCECAO
           .
       2500-FIM-SEM-APRESENTACAO.
           EXIT.

       3000-FINALIZAR.
           CLOSE LOG-AUTORIZ CLEARING LANCAMENTOS TRANS-FATURA EXCECOES
           DISPLAY 'CICCLEAR - AUTORIZACOES LIDAS .: ' WS-QTD-LOG-LIDOS
           DISPLAY 'CICCLEAR -   USADAS NO CASAMENTO: '
                   WS-QTD-LOG-USADOS
           DISPLAY 'CICCLEAR - APRESENTACOES LIDAS : ' WS-QTD-DETALHES
           DISPLAY 'CICCLEAR -   CASADAS ..........: ' WS-QTD-CASADAS
           DISPLAY 'CICCLEAR -   DIFERENCA ACEITA .: ' WS-QTD-DIF-ACEITA
           DISPLAY 'CICCLEAR -   DIFERENCA FORA ...: ' WS-QTD-DIF-FORA
           DISPLAY 'CICCLEAR -   SEM AUTORIZACAO ..: ' WS-QTD-SEM-AUT
           DISPLAY 'CICCLEAR -   DUPLICADAS .......: ' WS-QTD-DUPLICADAS
           DISPLAY 'CICCLEAR -   (TARDIAS) ........: ' WS-QTD-TARDIAS
           DISPLAY 'CICCLEAR - AUTORIZ. PENDENTES .: '
                   WS-QTD-AUT-PENDENTE
           DISPLAY 'CICCLEAR - AUTORIZ. EXPIRADAS .: '
                   WS-QTD-AUT-EXPIRADA
           DISPLAY 'CICCLEAR - LANCAMENTOS ........: '
                   WS-QTD-LANCAMENTOS
           MOVE WS-VALOR-LANCADO TO WS-VALOR-EDITADO
           DISPLAY 'CICCLEAR -   VALOR LANCADO ....: ' WS-VALOR-EDITADO
           DISPLAY 'CICCLEAR -   PARA A FATURA ....: ' WS-QTD-FATURA
           DISPLAY 'CICCLEAR - EXCECOES GRAVADAS ..: ' WS-QTD-EXCECOES
      *    CONFERENCIA 1: CADA APRESENTACAO CAIU EM UMA CLASSE
           COMPUTE WS-QTD-ESPERADA = WS-QTD-CASADAS + WS-QTD-DIF-ACEITA
                   + WS-QTD-DIF-FORA + WS-QTD-SEM-AUT
                   + WS-QTD-DUPLICADAS
           IF WS-QTD-ESPERADA NOT = WS-QTD-DETALHES
              DISPLAY 'CICCLEAR - CLASSES NAO FECHAM COM AS LIDAS'
              MOVE 16 TO RETURN-CODE
           ELSE
      *       CONFERENCIA 2: SO A DIFERENCA FORA DA TOLERANCIA NAO LANCA
              IF WS-QTD-LANCAMENTOS NOT =
                 WS-QTD-DETALHES - WS-QTD-DIF-FORA
                 DISPLAY 'CICCLEAR - LANCAMENTOS NAO FECHAM'
                 MOVE 16 TO RETURN-CODE
              ELSE
                 IF WS-QTD-EXCECOES > ZERO
                    MOVE 4 TO RETURN-CODE
                 ELSE
                    MOVE 0 TO RETURN-CODE
                 END-IF
              END-IF
           END-IF
           .
       3000-FIM-FINALIZAR.
           EXIT.

       7000-MONTAR-LANCAMENTO.
           MOVE SPACES                 TO REG-LANCAMENTO
           MOVE CL-NUM-CARTAO          TO LC-NUM-CARTAO
           MOVE CL-COD-AUTORIZACAO     TO LC-COD-AUTORIZACAO
           MOVE CL-RRN                 TO LC-RRN
           MOVE CL-DATA-COMPRA         TO LC-DATA-COMPRA
           MOVE CL-VALOR               TO LC-VALOR
           IF WS-CHAVE-AUT = WS-CHAVE-CLR
              MOVE LG-VALOR            TO LC-VALOR-AUTORIZADO
           ELSE
              MOVE ZERO                TO LC-VALOR-AUTORIZADO
           END-IF
           MOVE CL-MCC                 TO LC-MCC
           MOVE CL-PAIS                TO LC-PAIS
           MOVE CL-MODO-ENTRADA        TO LC-MODO-ENTRADA
           MOVE CL-ECI                 TO LC-ECI
           MOVE CL-RECORRENTE          TO LC-RECORRENTE
           MOVE CL-PRODUTO             TO LC-PRODUTO
           MOVE CL-IRD                 TO LC-IRD
           MOVE CL-BANDEIRA            TO LC-BANDEIRA
           MOVE CL-ESTABELECIMENTO     TO LC-ESTABELECIMENTO
           MOVE WS-DATA-PROCESSAMENTO  TO LC-DATA-PROCESSAMENTO
           SET LC-CASADA               TO TRUE
           MOVE 'N'                    TO LC-TARDIA
      *    APRESENTACAO TARDIA: MAIS DE 10 DIAS ENTRE A COMPRA E O
      *    PROCESSAMENTO (REGRA FICTICIA). LANCA E REGISTRA
           COMPUTE WS-DIAS = FUNCTION INTEGER-OF-DATE
                             (WS-DATA-PROCESSAMENTO)
                           - FUNCTION INTEGER-OF-DATE(CL-DATA-COMPRA)
           IF WS-DIAS > WS-PRAZO-APRESENTACAO
              SET LC-APRESENTACAO-TARDIA TO TRUE
           END-IF
           ADD 1                       TO WS-QTD-LANCAMENTOS
           ADD CL-VALOR                TO WS-VALOR-LANCADO
      *    O LANCAMENTO SO E GRAVADO NO 7050, DEPOIS QUE O PARAGRAFO
      *    QUE CHAMOU ESTE DEFINE A SITUACAO
           .
       7000-FIM-MONTAR-LANCAMENTO.
           EXIT.

       7050-GRAVAR-LANCAMENTO.
      *    GRAVA O LANCAMENTO (JA COM A SITUACAO), A TRANSACAO PARA A
      *    FATURA (SO PRODUTO CREDITO) E, SE TARDIA, A EXCECAO
           WRITE REG-LANCAMENTO
           IF NOT WS-FS-LAN-OK
              DISPLAY 'CICCLEAR - ERRO DE WRITE NO LANCOUT FS: '
                      WS-FS-LANCAMENTO
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           IF CL-CREDITO
              PERFORM 7300-GRAVAR-FATURA THRU 7300-FIM-GRAVAR-FATURA
           END-IF
           IF LC-APRESENTACAO-TARDIA
              ADD 1 TO WS-QTD-TARDIAS
              MOVE SPACES TO REG-EXCECAO
              SET EX-TARDIA TO TRUE
              MOVE 'APRESENTACAO TARDIA (MAIS DE 10 DIAS)'
                                       TO EX-DESCRICAO
              PERFORM 7100-GRAVAR-EXCECAO-CLR
                 THRU 7100-FIM-GRAVAR-EXCECAO-CLR
           END-IF
           .
       7050-FIM-GRAVAR-LANCAMENTO.
           EXIT.

       7100-GRAVAR-EXCECAO-CLR.
      *    EXCECAO A PARTIR DA APRESENTACAO ATUAL
           MOVE CL-NUM-CARTAO          TO EX-NUM-CARTAO
           MOVE CL-COD-AUTORIZACAO     TO EX-COD-AUTORIZACAO
           MOVE CL-RRN                 TO EX-RRN
           MOVE CL-DATA-COMPRA         TO EX-DATA-COMPRA
           MOVE CL-VALOR               TO EX-VALOR-APRESENTADO
           IF WS-CHAVE-AUT = WS-CHAVE-CLR
              MOVE LG-VALOR            TO EX-VALOR-AUTORIZADO
           ELSE
              MOVE ZERO                TO EX-VALOR-AUTORIZADO
           END-IF
           COMPUTE WS-DIAS = FUNCTION INTEGER-OF-DATE
                             (WS-DATA-PROCESSAMENTO)
                           - FUNCTION INTEGER-OF-DATE(CL-DATA-COMPRA)
           MOVE WS-DIAS                TO EX-DIAS
           MOVE CL-BANDEIRA            TO EX-BANDEIRA
           PERFORM 7200-GRAVAR-EXCECAO THRU 7200-FIM-GRAVAR-EXCECAO
           .
       7100-FIM-GRAVAR-EXCECAO-CLR.
           EXIT.

       7200-GRAVAR-EXCECAO.
           WRITE REG-EXCECAO
           IF NOT WS-FS-EXC-OK
              DISPLAY 'CICCLEAR - ERRO DE WRITE NO EXCECOUT FS: '
                      WS-FS-EXCECAO
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           ADD 1 TO WS-QTD-EXCECOES
      *    NO SYSOUT, O CARTAO APARECE SEMPRE MASCARADO
           MOVE EX-NUM-CARTAO TO WS-CARTAO-ABERTO
           PERFORM 9800-MASCARAR-CARTAO
           DISPLAY 'CICCLEAR - EXCECAO ' EX-TIPO ' ' WS-CARTAO-MASC
                   ' ' EX-DESCRICAO
           .
       7200-FIM-GRAVAR-EXCECAO.
           EXIT.

       7300-GRAVAR-FATURA.
      *    A TRANSACAO VAI PARA A FATURA NOTURNA NO LAYOUT QUE ELA JA
      *    CONHECE (CPTRANS): O ARQUIVO "RECEBIDO DA BANDEIRA" NASCE AQUI
           MOVE SPACES                 TO REG-TRANSACAO
           MOVE CL-NUM-CARTAO          TO TR-NUM-CARTAO
           MOVE CL-DATA-COMPRA         TO TR-DATA-TRANSACAO
           MOVE ZERO                   TO TR-HORA-TRANSACAO
           MOVE CL-VALOR               TO TR-VALOR
           MOVE CL-MCC                 TO TR-MCC
           MOVE CL-PAIS                TO TR-PAIS
           MOVE CL-COD-AUTORIZACAO     TO TR-COD-AUTORIZACAO
           SET TR-APROVADA             TO TRUE
           MOVE CL-ESTABELECIMENTO     TO TR-ESTABELECIMENTO
           WRITE REG-TRANSACAO
           IF NOT WS-FS-FAT-OK
              DISPLAY 'CICCLEAR - ERRO DE WRITE NO FATOUT FS: '
                      WS-FS-FATURA
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           ADD 1 TO WS-QTD-FATURA
           .
       7300-FIM-GRAVAR-FATURA.
           EXIT.

       8100-LER-AUTORIZACAO.
      *    SO ENTRAM NO CASAMENTO AS AUTORIZACOES DE MENSAGEM DUPLA
      *    (0100 E O AVISO 0120) APROVADAS E NAO REVERTIDAS. O 0200 E
      *    MENSAGEM UNICA: NAO TEM CLEARING
           MOVE SPACES TO WS-CHAVE-AUT
           PERFORM UNTIL WS-CHAVE-AUT NOT = SPACES
              READ LOG-AUTORIZ
                 AT END
                    MOVE HIGH-VALUES TO WS-CHAVE-AUT
                 NOT AT END
                    ADD 1 TO WS-QTD-LOG-LIDOS
                    IF LG-APROVADA AND NOT LG-REVERTIDA
                       AND (LG-MTI-PEDIDO = '0100' OR '0120')
                       ADD 1 TO WS-QTD-LOG-USADOS
                       MOVE LG-NUM-CARTAO      TO WS-CA-CARTAO
                       MOVE LG-COD-AUTORIZACAO TO WS-CA-CODIGO
                    END-IF
              END-READ
              IF NOT WS-FS-LOG-OK AND NOT WS-FS-LOG-FIM
                 DISPLAY 'CICCLEAR - ERRO DE READ AUTLOGIN FS: '
                         WS-FS-LOG
                 PERFORM 9900-ENCERRAR-COM-ERRO
              END-IF
           END-PERFORM
           .
       8100-FIM-LER-AUTORIZACAO.
           EXIT.

       8200-LER-APRESENTACAO.
      *    PULA HEADERS E TRAILERS: SO OS DETALHES ENTRAM NO CASAMENTO
           MOVE SPACES TO WS-CHAVE-CLR
           PERFORM UNTIL WS-CHAVE-CLR NOT = SPACES
              READ CLEARING
                 AT END
                    MOVE HIGH-VALUES TO WS-CHAVE-CLR
                 NOT AT END
                    IF CL-DETALHE
                       ADD 1 TO WS-QTD-DETALHES
                       MOVE CL-NUM-CARTAO      TO WS-CC-CARTAO
                       MOVE CL-COD-AUTORIZACAO TO WS-CC-CODIGO
                    END-IF
              END-READ
              IF NOT WS-FS-CLR-OK AND NOT WS-FS-CLR-FIM
                 DISPLAY 'CICCLEAR - ERRO DE READ CLEARIN FS: '
                         WS-FS-CLEARING
                 PERFORM 9900-ENCERRAR-COM-ERRO
              END-IF
           END-PERFORM
           .
       8200-FIM-LER-APRESENTACAO.
           EXIT.

           COPY CPMASCAR.

       9900-ENCERRAR-COM-ERRO.
      *    ENCERRA COM RC 16 PARA IMPEDIR OS STEPS SEGUINTES
           MOVE 16 TO RETURN-CODE
           STOP RUN
           .
