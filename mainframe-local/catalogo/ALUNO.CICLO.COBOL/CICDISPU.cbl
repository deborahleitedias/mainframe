      *================================================================*
      * PROGRAMA : CICDISPU (ESQUELETO DA TURMA)                       *
      * AULA     : 4 - CONCILIACAO E DISPUTAS                          *
      * OBJETIVO : ANALISAR AS CONTESTACOES DOS PORTADORES:            *
      *            1. ENCONTRAR A TRANSACAO ORIGINAL NOS LANCAMENTOS   *
      *               (BALANCE LINE PELA REFERENCIA, O RRN);           *
      *            2. CONFERIR O PRAZO (120 DIAS DESDE O PROCESSAMENTO,*
      *               REGRA SIMPLIFICADA DO PROJETO);                  *
      *            3. DECIDIR SE CABE CHARGEBACK E COM QUAL REASON     *
      *               CODE, PELOS DADOS QUE O SISTEMA REGISTROU        *
      *               (MODO DE ENTRADA, 3-D SECURE, AUTORIZACAO,       *
      *               DUPLICIDADE, RECORRENCIA);                       *
      *            4. GRAVAR O CHARGEBACK PARA A BANDEIRA E O CREDITO  *
      *               PROVISORIO NA CONTA DO PORTADOR.                 *
      *            REASON CODES: A BANDEIRA ALFA USA COMO REFERENCIA   *
      *            OS CODIGOS PUBLICOS DA VISA; A BETA, OS DA          *
      *            MASTERCARD. AS REGRAS SAO SIMPLIFICADAS.            *
      * ENTRADAS : DD CONTIN   - CPCONTES, FB 80, ORDENADO POR RRN     *
      *            DD LANCIN   - CPLANC, FB 120, ORDENADO POR RRN      *
      *            DD DATAIN   - DATA DE MOVIMENTO (AAAAMMDD), FB 80   *
      * SAIDAS   : DD CHARGOUT - CPCHARG, FB 120                       *
      *            DD AJUSOUT  - CPAJUSTE, FB 80                       *
      *            DD RELDOUT  - RELATORIO, FBA 133                    *
      * RETORNO  : 0 = OK, 4 = HOUVE CONTESTACAO SEM CHARGEBACK,       *
      *            16 = ERRO DE ARQUIVO                                *
      * A ESCREVER NA AULA 4:                                          *
      * AS REGRAS DO 2100-ANALISAR (AO VIVO, COM A TURMA)              *
      * AS PENDENCIAS POR IDADE (MAO NA MASSA)                         *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICDISPU.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONTESTACOES ASSIGN TO CONTIN
                  FILE STATUS IS WS-FS-CONTEST.
           SELECT LANCAMENTOS  ASSIGN TO LANCIN
                  FILE STATUS IS WS-FS-LANCAMENTO.
           SELECT DATA-MOVTO   ASSIGN TO DATAIN
                  FILE STATUS IS WS-FS-DATA.
           SELECT CHARGEBACKS  ASSIGN TO CHARGOUT
                  FILE STATUS IS WS-FS-CHARGEBACK.
           SELECT AJUSTES      ASSIGN TO AJUSOUT
                  FILE STATUS IS WS-FS-AJUSTE.
           SELECT RELATORIO    ASSIGN TO RELDOUT
                  FILE STATUS IS WS-FS-RELATORIO.

       DATA DIVISION.
       FILE SECTION.
       FD  CONTESTACOES
           RECORDING MODE IS F.
           COPY CPCONTES.
       FD  LANCAMENTOS
           RECORDING MODE IS F.
           COPY CPLANC.
       FD  DATA-MOVTO
           RECORDING MODE IS F.
       01  REG-DATA-MOVTO.
           05  DM-DATA                PIC 9(08).
           05  FILLER                 PIC X(72).
       FD  CHARGEBACKS
           RECORDING MODE IS F.
           COPY CPCHARG.
       FD  AJUSTES
           RECORDING MODE IS F.
           COPY CPAJUSTE.
       FD  RELATORIO
           RECORDING MODE IS F.
       01  REG-RELATORIO              PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FS-CONTEST              PIC X(02).
           88  WS-FS-CON-OK                     VALUE '00'.
           88  WS-FS-CON-FIM                    VALUE '10'.
       01  WS-FS-LANCAMENTO           PIC X(02).
           88  WS-FS-LAN-OK                     VALUE '00'.
           88  WS-FS-LAN-FIM                    VALUE '10'.
       01  WS-FS-DATA                 PIC X(02).
           88  WS-FS-DAT-OK                     VALUE '00'.
       01  WS-FS-CHARGEBACK           PIC X(02).
           88  WS-FS-CHB-OK                     VALUE '00'.
       01  WS-FS-AJUSTE               PIC X(02).
           88  WS-FS-AJU-OK                     VALUE '00'.
       01  WS-FS-RELATORIO            PIC X(02).
           88  WS-FS-REL-OK                     VALUE '00'.

      *--- CHAVES DO BALANCE LINE (HIGH-VALUES NO FIM) ----------------*
       01  WS-CHAVE-CONTEST           PIC X(12).
       01  WS-CHAVE-LANC              PIC X(12).

       01  WS-DATA-MOVIMENTO          PIC 9(08).
       01  WS-PRAZO-DISPUTA           PIC 9(03) VALUE 120.
       01  WS-DIAS-PROCESSAMENTO      PIC S9(05).
       01  WS-DIAS-COMPRA             PIC S9(05).

      *--- RESULTADO DA ANALISE ---------------------------------------*
       01  WS-DECISAO                 PIC X(30).
       01  WS-CATEGORIA               PIC X(02).
       01  WS-REASON-CODE             PIC X(06).
       01  WS-CABE-CHARGEBACK         PIC X(01).
           88  WS-COM-CHARGEBACK                VALUE 'S'.
           88  WS-SEM-CHARGEBACK                VALUE 'N'.
      *    TIPO DO CHARGEBACK, USADO PARA ESCOLHER O REASON CODE
       01  WS-TIPO-CHARGEBACK         PIC X(02).
           88  WS-FRAUDE-NAO-PRESENCIAL         VALUE 'FN'.
           88  WS-FRAUDE-TARJA                  VALUE 'FT'.
           88  WS-SEM-AUTORIZACAO               VALUE 'SA'.
           88  WS-DUPLICIDADE                   VALUE 'DU'.
           88  WS-VALOR-INCORRETO               VALUE 'VI'.
           88  WS-RECORRENTE-CANCELADA          VALUE 'RC'.
           88  WS-NAO-RECEBIDO                  VALUE 'NR'.

      *--- PENDENCIAS POR IDADE (DIAS DESDE A COMPRA) -----------------*
       01  WS-AGING.
           05  WS-AG-ITEM OCCURS 4 TIMES INDEXED BY IX-AG.
               10  WS-AG-QTD          PIC 9(05).
               10  WS-AG-VALOR        PIC 9(11)V99.
       01  WS-FAIXAS-TEXTO.
           05  FILLER                 PIC X(20) VALUE 'ATE 30 DIAS'.
           05  FILLER                 PIC X(20) VALUE
               'DE 31 A 60 DIAS'.
           05  FILLER                 PIC X(20) VALUE
               'DE 61 A 90 DIAS'.
           05  FILLER                 PIC X(20) VALUE
               'ACIMA DE 90 DIAS'.
       01  WS-FAIXAS-R REDEFINES WS-FAIXAS-TEXTO.
           05  WS-FAIXA-NOME OCCURS 4 TIMES PIC X(20).

       01  WS-CONTADORES.
           05  WS-QTD-CONTESTACOES    PIC 9(05) VALUE ZERO.
           05  WS-QTD-CHARGEBACKS     PIC 9(05) VALUE ZERO.
           05  WS-QTD-SEM-DIREITO     PIC 9(05) VALUE ZERO.
           05  WS-QTD-NAO-ENCONTRADAS PIC 9(05) VALUE ZERO.
       01  WS-VALOR-CHARGEBACKS       PIC 9(11)V99 VALUE ZERO.

       01  WS-CARTAO-ABERTO           PIC X(16).
       01  WS-CARTAO-MASC             PIC X(16).

      *--- LINHAS DO RELATORIO ----------------------------------------*
       01  WS-CAB-1.
           05  FILLER                 PIC X(50) VALUE
               'CICLO DO CARTAO - ANALISE DE CONTESTACOES'.
           05  FILLER                 PIC X(18) VALUE
               'DATA DE MOVIMENTO '.
           05  CB1-DATA               PIC 9999/99/99.
           05  FILLER                 PIC X(20) VALUE
               '  (DADOS FICTICIOS)'.
       01  WS-CAB-2.
           05  FILLER                 PIC X(12) VALUE 'PROTOCOLO'.
           05  FILLER                 PIC X(18) VALUE 'CARTAO'.
           05  FILLER                 PIC X(14) VALUE 'RRN'.
           05  FILLER                 PIC X(04) VALUE 'MOT'.
           05  FILLER                 PIC X(14) VALUE '         VALOR'.
           05  FILLER                 PIC X(06) VALUE '  DIAS'.
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  FILLER                 PIC X(32) VALUE 'DECISAO'.
           05  FILLER                 PIC X(04) VALUE 'CAT'.
           05  FILLER                 PIC X(06) VALUE 'REASON'.
       01  WS-DETALHE.
           05  DT-PROTOCOLO           PIC X(10).
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  DT-CARTAO              PIC X(16).
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  DT-RRN                 PIC X(12).
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  DT-MOTIVO              PIC X(02).
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  DT-VALOR               PIC ZZZ,ZZZ,ZZ9.99.
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  DT-DIAS                PIC ZZZ9.
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  DT-DECISAO             PIC X(30).
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  DT-CATEGORIA           PIC X(02).
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  DT-REASON              PIC X(06).
       01  WS-LINHA-AGING.
           05  FILLER                 PIC X(04) VALUE SPACES.
           05  LA-FAIXA               PIC X(20).
           05  LA-QTD                 PIC ZZ,ZZ9.
           05  FILLER                 PIC X(04) VALUE SPACES.
           05  LA-VALOR               PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-LINHA-TOTAL.
           05  LT-DESCRICAO           PIC X(40).
           05  LT-QTD                 PIC ZZ,ZZ9.
           05  FILLER                 PIC X(04) VALUE SPACES.
           05  LT-VALOR               PIC ZZ,ZZZ,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INICIALIZAR THRU 1000-FIM-INICIALIZAR
           PERFORM 2000-CRUZAR      THRU 2000-FIM-CRUZAR
              UNTIL WS-CHAVE-CONTEST = HIGH-VALUES
           PERFORM 3000-FINALIZAR   THRU 3000-FIM-FINALIZAR
           STOP RUN.

       1000-INICIALIZAR.

           OPEN INPUT DATA-MOVTO
           IF NOT WS-FS-DAT-OK
              DISPLAY 'CICDISPU - ERRO DE OPEN DATAIN FS: ' WS-FS-DATA
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           READ DATA-MOVTO
           IF NOT WS-FS-DAT-OK OR DM-DATA NOT NUMERIC
              DISPLAY 'CICDISPU - DATA DE MOVIMENTO INVALIDA'
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           MOVE DM-DATA TO WS-DATA-MOVIMENTO
           CLOSE DATA-MOVTO

           OPEN INPUT CONTESTACOES
           IF NOT WS-FS-CON-OK
              DISPLAY 'CICDISPU - ERRO DE OPEN CONTIN FS: '
                      WS-FS-CONTEST
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           OPEN INPUT LANCAMENTOS
           IF NOT WS-FS-LAN-OK
              DISPLAY 'CICDISPU - ERRO DE OPEN LANCIN FS: '
                      WS-FS-LANCAMENTO
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           OPEN OUTPUT CHARGEBACKS
           IF NOT WS-FS-CHB-OK
              DISPLAY 'CICDISPU - ERRO DE OPEN CHARGOUT FS: '
                      WS-FS-CHARGEBACK
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           OPEN OUTPUT AJUSTES
           IF NOT WS-FS-AJU-OK
              DISPLAY 'CICDISPU - ERRO DE OPEN AJUSOUT FS: '
                      WS-FS-AJUSTE
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           OPEN OUTPUT RELATORIO
           IF NOT WS-FS-REL-OK
              DISPLAY 'CICDISPU - ERRO DE OPEN RELDOUT FS: '
                      WS-FS-RELATORIO
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF

           INITIALIZE WS-AGING
           MOVE WS-DATA-MOVIMENTO TO CB1-DATA
           WRITE REG-RELATORIO FROM WS-CAB-1 AFTER ADVANCING PAGE
           WRITE REG-RELATORIO FROM WS-CAB-2 AFTER ADVANCING 2 LINES
           MOVE SPACES TO REG-RELATORIO
           WRITE REG-RELATORIO AFTER ADVANCING 1 LINE

           PERFORM 8100-LER-CONTESTACAO THRU 8100-FIM-LER-CONTESTACAO
           PERFORM 8200-LER-LANCAMENTO  THRU 8200-FIM-LER-LANCAMENTO
           .
       1000-FIM-INICIALIZAR.
           EXIT.

       2000-CRUZAR.
      *    UMA TRANSACAO PODE TER MAIS DE UMA CONTESTACAO: QUANDO AS
      *    CHAVES SAO IGUAIS, SO A CONTESTACAO AVANCA
           EVALUATE TRUE
              WHEN WS-CHAVE-CONTEST = WS-CHAVE-LANC
                 PERFORM 2100-ANALISAR THRU 2100-FIM-ANALISAR
                 PERFORM 8100-LER-CONTESTACAO
                    THRU 8100-FIM-LER-CONTESTACAO
              WHEN WS-CHAVE-CONTEST < WS-CHAVE-LANC
                 PERFORM 2500-NAO-ENCONTRADA
                    THRU 2500-FIM-NAO-ENCONTRADA
                 PERFORM 8100-LER-CONTESTACAO
                    THRU 8100-FIM-LER-CONTESTACAO
              WHEN OTHER
      *          LANCAMENTO SEM CONTESTACAO: SEGUE
                 PERFORM 8200-LER-LANCAMENTO
                    THRU 8200-FIM-LER-LANCAMENTO
           END-EVALUATE
           .
       2000-FIM-CRUZAR.
           EXIT.

       2100-ANALISAR.
           MOVE SPACES TO WS-DECISAO WS-CATEGORIA WS-REASON-CODE
                          WS-TIPO-CHARGEBACK
           SET WS-SEM-CHARGEBACK TO TRUE
           COMPUTE WS-DIAS-PROCESSAMENTO =
                   FUNCTION INTEGER-OF-DATE(WS-DATA-MOVIMENTO)
                 - FUNCTION INTEGER-OF-DATE(LC-DATA-PROCESSAMENTO)
           COMPUTE WS-DIAS-COMPRA =
                   FUNCTION INTEGER-OF-DATE(WS-DATA-MOVIMENTO)
                 - FUNCTION INTEGER-OF-DATE(LC-DATA-COMPRA)

      *    A ESCREVER (AO VIVO): EVALUATE TRUE COM AS REGRAS, NA ORDEM:
      *    FORA DO PRAZO (MAIS DE WS-PRAZO-DISPUTA DIAS DO PROCESSAMENTO);
      *    NR SEM AUTORIZACAO (LC-SEM-AUTORIZACAO) -> SET WS-SEM-AUTORIZACAO;
      *    NR COM CHIP OU APROXIMACAO (05, 07) -> SEM DIREITO: CHIP;
      *    NR AUTENTICADA (LC-ECI = '05') -> SEM DIREITO: 3-D SECURE;
      *    NR DIGITADA OU ONLINE (01, 81) -> FRAUDE NAO PRESENCIAL;
      *    NR COM TARJA (90) -> FRAUDE TARJA;
      *    DU: DUPLICADA (LC-DUPLICADA) OU IMPROCEDENTE; CR: RECORRENTE OU
      *    IMPROCEDENTE; NE: NAO RECEBIDO; VD: VALOR INCORRETO.
      *    QUEM TEM DIREITO A CHARGEBACK RECEBE UM WS-TIPO-CHARGEBACK; QUEM
      *    NAO TEM RECEBE O TEXTO EM WS-DECISAO.
           MOVE 'REGRAS AINDA NAO ESCRITAS' TO WS-DECISAO

           IF WS-TIPO-CHARGEBACK NOT = SPACES
              SET WS-COM-CHARGEBACK TO TRUE
              MOVE 'CHARGEBACK ENVIADO' TO WS-DECISAO
              PERFORM 2200-ESCOLHER-REASON THRU 2200-FIM-ESCOLHER-REASON
              PERFORM 2300-GRAVAR-CHARGEBACK
                 THRU 2300-FIM-GRAVAR-CHARGEBACK
           ELSE
              ADD 1 TO WS-QTD-SEM-DIREITO
           END-IF
           PERFORM 2400-IMPRIMIR THRU 2400-FIM-IMPRIMIR
           .
       2100-FIM-ANALISAR.
           EXIT.

       2200-ESCOLHER-REASON.
      *    ALFA: CATEGORIAS E CODIGOS DA VISA (10 FRAUDE, 11 AUTORIZACAO,
      *    12 ERRO DE PROCESSAMENTO, 13 DISPUTA DO CONSUMIDOR)
      *    BETA: CODIGOS DA MASTERCARD
           EVALUATE TRUE
              WHEN WS-FRAUDE-NAO-PRESENCIAL
                 MOVE '10' TO WS-CATEGORIA
                 IF LC-BANDEIRA = 'A'
                    MOVE '10.4'   TO WS-REASON-CODE
                 ELSE
                    MOVE '4837'   TO WS-REASON-CODE
                 END-IF
              WHEN WS-FRAUDE-TARJA
                 MOVE '10' TO WS-CATEGORIA
                 IF LC-BANDEIRA = 'A'
                    MOVE '10.1'   TO WS-REASON-CODE
                 ELSE
                    MOVE '4870'   TO WS-REASON-CODE
                 END-IF
              WHEN WS-SEM-AUTORIZACAO
                 MOVE '11' TO WS-CATEGORIA
                 IF LC-BANDEIRA = 'A'
                    MOVE '11.3'   TO WS-REASON-CODE
                 ELSE
                    MOVE '4808'   TO WS-REASON-CODE
                 END-IF
              WHEN WS-DUPLICIDADE
                 MOVE '12' TO WS-CATEGORIA
                 IF LC-BANDEIRA = 'A'
                    MOVE '12.6.1' TO WS-REASON-CODE
                 ELSE
                    MOVE '4834'   TO WS-REASON-CODE
                 END-IF
              WHEN WS-VALOR-INCORRETO
                 MOVE '12' TO WS-CATEGORIA
                 IF LC-BANDEIRA = 'A'
                    MOVE '12.5'   TO WS-REASON-CODE
                 ELSE
                    MOVE '4834'   TO WS-REASON-CODE
                 END-IF
              WHEN WS-RECORRENTE-CANCELADA
                 MOVE '13' TO WS-CATEGORIA
                 IF LC-BANDEIRA = 'A'
                    MOVE '13.2'   TO WS-REASON-CODE
                 ELSE
                    MOVE '4853'   TO WS-REASON-CODE
                 END-IF
              WHEN WS-NAO-RECEBIDO
                 MOVE '13' TO WS-CATEGORIA
                 IF LC-BANDEIRA = 'A'
                    MOVE '13.1'   TO WS-REASON-CODE
                 ELSE
                    MOVE '4853'   TO WS-REASON-CODE
                 END-IF
           END-EVALUATE
           .
       2200-FIM-ESCOLHER-REASON.
           EXIT.

       2300-GRAVAR-CHARGEBACK.
           MOVE SPACES              TO REG-CHARGEBACK
           MOVE LC-NUM-CARTAO       TO CB-NUM-CARTAO
           MOVE LC-RRN              TO CB-RRN
           MOVE LC-COD-AUTORIZACAO  TO CB-COD-AUTORIZACAO
           MOVE LC-BANDEIRA         TO CB-BANDEIRA
           MOVE WS-CATEGORIA        TO CB-CATEGORIA
           MOVE WS-REASON-CODE      TO CB-REASON-CODE
           MOVE CN-VALOR            TO CB-VALOR
           MOVE LC-DATA-COMPRA      TO CB-DATA-COMPRA
           MOVE WS-DATA-MOVIMENTO   TO CB-DATA-CHARGEBACK
           MOVE CN-PROTOCOLO        TO CB-PROTOCOLO
           MOVE LC-ESTABELECIMENTO  TO CB-DESCRICAO
           WRITE REG-CHARGEBACK
           IF NOT WS-FS-CHB-OK
              DISPLAY 'CICDISPU - ERRO DE WRITE NO CHARGOUT FS: '
                      WS-FS-CHARGEBACK
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
      *    CREDITO PROVISORIO NA CONTA ENQUANTO A DISPUTA NAO TERMINA
           MOVE SPACES              TO REG-AJUSTE
           MOVE LC-NUM-CARTAO       TO AJ-NUM-CARTAO
           SET AJ-CREDITO-PROVISORIO TO TRUE
           MOVE WS-DATA-MOVIMENTO   TO AJ-DATA
           MOVE CN-VALOR            TO AJ-VALOR
           MOVE LC-RRN              TO AJ-RRN
           MOVE CN-PROTOCOLO        TO AJ-PROTOCOLO
           WRITE REG-AJUSTE
           IF NOT WS-FS-AJU-OK
              DISPLAY 'CICDISPU - ERRO DE WRITE NO AJUSOUT FS: '
                      WS-FS-AJUSTE
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           ADD 1        TO WS-QTD-CHARGEBACKS
           ADD CN-VALOR TO WS-VALOR-CHARGEBACKS
      *    A ESCREVER (MAO NA MASSA): CLASSIFICAR O CHARGEBACK PELA IDADE
      *    DA COMPRA (WS-DIAS-COMPRA): ATE 30, DE 31 A 60, DE 61 A 90 E
      *    ACIMA DE 90 DIAS, SOMANDO QUANTIDADE E VALOR EM WS-AG-ITEM.
           .
       2300-FIM-GRAVAR-CHARGEBACK.
           EXIT.

       2400-IMPRIMIR.
           MOVE CN-PROTOCOLO        TO DT-PROTOCOLO
           MOVE CN-NUM-CARTAO       TO WS-CARTAO-ABERTO
           PERFORM 9800-MASCARAR-CARTAO
           MOVE WS-CARTAO-MASC      TO DT-CARTAO
           MOVE CN-RRN              TO DT-RRN
           MOVE CN-MOTIVO           TO DT-MOTIVO
           MOVE CN-VALOR            TO DT-VALOR
           IF WS-CHAVE-CONTEST = WS-CHAVE-LANC
              MOVE WS-DIAS-COMPRA   TO DT-DIAS
           ELSE
              MOVE ZERO             TO DT-DIAS
           END-IF
           MOVE WS-DECISAO          TO DT-DECISAO
           MOVE WS-CATEGORIA        TO DT-CATEGORIA
           MOVE WS-REASON-CODE      TO DT-REASON
           WRITE REG-RELATORIO FROM WS-DETALHE AFTER ADVANCING 1 LINE
           IF NOT WS-FS-REL-OK
              DISPLAY 'CICDISPU - ERRO DE WRITE NO RELDOUT FS: '
                      WS-FS-RELATORIO
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           .
       2400-FIM-IMPRIMIR.
           EXIT.

       2500-NAO-ENCONTRADA.
           ADD 1 TO WS-QTD-NAO-ENCONTRADAS
           MOVE 'TRANSACAO NAO ENCONTRADA' TO WS-DECISAO
           MOVE SPACES TO WS-CATEGORIA WS-REASON-CODE
           PERFORM 2400-IMPRIMIR THRU 2400-FIM-IMPRIMIR
           .
       2500-FIM-NAO-ENCONTRADA.
           EXIT.

       3000-FINALIZAR.
      *    RESUMO E PENDENCIAS POR IDADE (MAO NA MASSA DA AULA 4)
           MOVE 'CONTESTACOES ANALISADAS'   TO LT-DESCRICAO
           MOVE WS-QTD-CONTESTACOES         TO LT-QTD
           MOVE ZERO                        TO LT-VALOR
           WRITE REG-RELATORIO FROM WS-LINHA-TOTAL
                 AFTER ADVANCING 3 LINES
           MOVE 'CHARGEBACKS ENVIADOS'      TO LT-DESCRICAO
           MOVE WS-QTD-CHARGEBACKS          TO LT-QTD
           MOVE WS-VALOR-CHARGEBACKS        TO LT-VALOR
           WRITE REG-RELATORIO FROM WS-LINHA-TOTAL
                 AFTER ADVANCING 1 LINE
           MOVE 'CHARGEBACKS POR IDADE DA COMPRA' TO REG-RELATORIO
           WRITE REG-RELATORIO AFTER ADVANCING 2 LINES
           PERFORM VARYING IX-AG FROM 1 BY 1 UNTIL IX-AG > 4
              MOVE WS-FAIXA-NOME(IX-AG) TO LA-FAIXA
              MOVE WS-AG-QTD(IX-AG)     TO LA-QTD
              MOVE WS-AG-VALOR(IX-AG)   TO LA-VALOR
              WRITE REG-RELATORIO FROM WS-LINHA-AGING
                    AFTER ADVANCING 1 LINE
           END-PERFORM

           CLOSE CONTESTACOES LANCAMENTOS CHARGEBACKS AJUSTES RELATORIO
           DISPLAY 'CICDISPU - DATA DE MOVIMENTO ..: ' WS-DATA-MOVIMENTO
           DISPLAY 'CICDISPU - CONTESTACOES .......: '
                   WS-QTD-CONTESTACOES
           DISPLAY 'CICDISPU - CHARGEBACKS ........: '
                   WS-QTD-CHARGEBACKS
           DISPLAY 'CICDISPU - SEM CHARGEBACK .....: '
                   WS-QTD-SEM-DIREITO
           DISPLAY 'CICDISPU - NAO ENCONTRADAS ....: '
                   WS-QTD-NAO-ENCONTRADAS
      *    CONFERENCIA: CADA CONTESTACAO TEVE UM DESTINO
           IF WS-QTD-CONTESTACOES NOT = WS-QTD-CHARGEBACKS
                 + WS-QTD-SEM-DIREITO + WS-QTD-NAO-ENCONTRADAS
              DISPLAY 'CICDISPU - CONTADORES NAO FECHAM'
              MOVE 16 TO RETURN-CODE
           ELSE
              IF WS-QTD-SEM-DIREITO + WS-QTD-NAO-ENCONTRADAS > ZERO
                 MOVE 4 TO RETURN-CODE
              ELSE
                 MOVE 0 TO RETURN-CODE
              END-IF
           END-IF
           .
       3000-FIM-FINALIZAR.
           EXIT.

       8100-LER-CONTESTACAO.
           READ CONTESTACOES
              AT END
                 MOVE HIGH-VALUES TO WS-CHAVE-CONTEST
              NOT AT END
                 ADD 1 TO WS-QTD-CONTESTACOES
                 MOVE CN-RRN      TO WS-CHAVE-CONTEST
           END-READ
           IF NOT WS-FS-CON-OK AND NOT WS-FS-CON-FIM
              DISPLAY 'CICDISPU - ERRO DE READ CONTIN FS: '
                      WS-FS-CONTEST
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           .
       8100-FIM-LER-CONTESTACAO.
           EXIT.

       8200-LER-LANCAMENTO.
           READ LANCAMENTOS
              AT END
                 MOVE HIGH-VALUES TO WS-CHAVE-LANC
              NOT AT END
                 MOVE LC-RRN      TO WS-CHAVE-LANC
           END-READ
           IF NOT WS-FS-LAN-OK AND NOT WS-FS-LAN-FIM
              DISPLAY 'CICDISPU - ERRO DE READ LANCIN FS: '
                      WS-FS-LANCAMENTO
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           .
       8200-FIM-LER-LANCAMENTO.
           EXIT.

           COPY CPMASCAR.

       9900-ENCERRAR-COM-ERRO.
      *    ENCERRA COM RC 16 PARA IMPEDIR OS STEPS SEGUINTES
           MOVE 16 TO RETURN-CODE
           STOP RUN
           .
