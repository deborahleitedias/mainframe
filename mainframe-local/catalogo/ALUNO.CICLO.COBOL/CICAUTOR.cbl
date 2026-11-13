      *================================================================*
      * PROGRAMA : CICAUTOR (ESQUELETO DA TURMA)                       *
      * AULA     : 2 - AUTORIZACAO E CAPTURA                           *
      * OBJETIVO : SIMULAR EM BATCH A AUTORIZACAO DO EMISSOR. PARA     *
      *            CADA PEDIDO, NA ORDEM EM QUE CHEGOU:                *
      *            0100/0200: APLICA AS REGRAS DO EMISSOR NA ORDEM E   *
      *                       RESPONDE (0110/0210) COM O DE 39;        *
      *            0120     : REGISTRA A DECISAO TOMADA PELA BANDEIRA  *
      *                       EM STAND-IN E CONFIRMA (0130);           *
      *            0400/0420: REVERTE A AUTORIZACAO ORIGINAL E         *
      *                       DEVOLVE O LIMITE (0410/0430).            *
      *            NA VIDA REAL A AUTORIZACAO E ONLINE. AQUI O LOG E   *
      *            GRAVADO NO FIM, COM A SITUACAO FINAL DE CADA        *
      *            AUTORIZACAO (REVERTIDA OU NAO).                     *
      * ENTRADAS : DD CONTAS   - CPCONTA, FB 80, ORDENADO POR CARTAO   *
      *            DD PEDIDOS  - CPAUTPED, FB 80                       *
      * SAIDAS   : DD LOGOUT   - CPAUTLOG, FB 120                      *
      *            DD CONTOUT  - CPCONTA, FB 80 (LIMITES ATUALIZADOS)  *
      * RETORNO  : 0 = OK, 16 = ERRO DE ARQUIVO OU DE TABELA           *
      * A ESCREVER NA AULA 2:                                          *
      * 2200-DECIDIR (AO VIVO, COM A TURMA)                            *
      * 2500-REVERTER (MAO NA MASSA)                                   *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICAUTOR.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONTAS-ENTRADA ASSIGN TO CONTAS
                  FILE STATUS IS WS-FS-CONTAS.
           SELECT PEDIDOS        ASSIGN TO PEDIDOS
                  FILE STATUS IS WS-FS-PEDIDO.
           SELECT LOG-AUTORIZ    ASSIGN TO LOGOUT
                  FILE STATUS IS WS-FS-LOG.
           SELECT CONTAS-SAIDA   ASSIGN TO CONTOUT
                  FILE STATUS IS WS-FS-CONTOUT.

       DATA DIVISION.
       FILE SECTION.
       FD  CONTAS-ENTRADA
           RECORDING MODE IS F.
           COPY CPCONTA.
       FD  PEDIDOS
           RECORDING MODE IS F.
           COPY CPAUTPED.
       FD  LOG-AUTORIZ
           RECORDING MODE IS F.
           COPY CPAUTLOG.
       FD  CONTAS-SAIDA
           RECORDING MODE IS F.
       01  REG-CONTA-SAIDA            PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-FS-CONTAS               PIC X(02).
           88  WS-FS-CON-OK                     VALUE '00'.
           88  WS-FS-CON-FIM                    VALUE '10'.
       01  WS-FS-PEDIDO               PIC X(02).
           88  WS-FS-PED-OK                     VALUE '00'.
           88  WS-FS-PED-FIM                    VALUE '10'.
       01  WS-FS-LOG                  PIC X(02).
           88  WS-FS-LOG-OK                     VALUE '00'.
       01  WS-FS-CONTOUT              PIC X(02).
           88  WS-FS-COU-OK                     VALUE '00'.

      *--- CONTAS EM MEMORIA (SEARCH ALL PELO CARTAO) -----------------*
      *    CADA POSICAO GUARDA O REGISTRO INTEIRO; O LIMITE DISPONIVEL
      *    E ATUALIZADO NA PROPRIA TABELA E GRAVADO NO FIM
       01  WS-MAX-CONTAS              PIC 9(03) VALUE 200.
       01  WS-QTD-CONTAS              PIC 9(03) VALUE ZERO.
       01  WS-TABELA-CONTAS.
           05  WS-CT-ITEM OCCURS 200 TIMES
                          ASCENDING KEY IS WS-CT-CARTAO
                          INDEXED BY IX-CT.
               10  WS-CT-CARTAO       PIC X(16).
               10  WS-CT-STATUS       PIC X(01).
               10  WS-CT-VALIDADE     PIC 9(06).
               10  WS-CT-DISP         PIC S9(09)V99.
               10  WS-CT-EXTERIOR     PIC X(01).
               10  WS-CT-SAQUE        PIC 9(07)V99.
               10  WS-CT-REGISTRO     PIC X(80).

       01  WS-DISP-SAIDA              PIC S9(09)V99
                                      SIGN IS LEADING SEPARATE.
       01  WS-DISP-SAIDA-X REDEFINES WS-DISP-SAIDA
                                      PIC X(12).
       01  WS-ACHOU-CONTA             PIC X(01).
           88  WS-CONTA-ENCONTRADA              VALUE 'S'.
           88  WS-CONTA-NAO-ENCONTRADA          VALUE 'N'.

      *--- AUTORIZACOES APROVADAS (PARA ACHAR A ORIGINAL NA REVERSAO) -*
       01  WS-MAX-LOG                 PIC 9(04) VALUE 2000.
       01  WS-QTD-LOG                 PIC 9(04) VALUE ZERO.
       01  WS-TABELA-LOG.
           05  WS-LOG-ITEM OCCURS 2000 TIMES INDEXED BY IX-LOG.
               10  WS-LG-REGISTRO     PIC X(120).
       01  WS-TABELA-APROVADAS.
           05  WS-AP-ITEM OCCURS 2000 TIMES INDEXED BY IX-AP.
               10  WS-AP-CARTAO       PIC X(16).
               10  WS-AP-CODIGO       PIC X(06).
               10  WS-AP-VALOR        PIC 9(09)V99.
               10  WS-AP-POSICAO-LOG  PIC 9(04).
               10  WS-AP-REVERTIDA    PIC X(01).
       01  WS-QTD-AP                  PIC 9(04) VALUE ZERO.
       01  WS-ACHOU-ORIGINAL          PIC X(01).
           88  WS-ORIGINAL-ENCONTRADA           VALUE 'S'.
           88  WS-ORIGINAL-NAO-ENCONTRADA       VALUE 'N'.

      *--- GERACAO DO CODIGO DE AUTORIZACAO (DE 38) -------------------*
       01  WS-SEQ-AUTORIZACAO         PIC 9(05) VALUE ZERO.
       01  WS-COD-GERADO.
           05  FILLER                 PIC X(01) VALUE 'P'.
           05  WS-COD-SEQ             PIC 9(05).

       01  WS-CONTADORES.
           05  WS-QTD-LIDOS           PIC 9(07) VALUE ZERO.
           05  WS-QTD-APROVADAS       PIC 9(07) VALUE ZERO.
           05  WS-QTD-NEGADAS         PIC 9(07) VALUE ZERO.
           05  WS-QTD-STANDIN         PIC 9(07) VALUE ZERO.
           05  WS-QTD-REVERTIDAS      PIC 9(07) VALUE ZERO.
           05  WS-QTD-REV-SEM-ORIG    PIC 9(07) VALUE ZERO.
           05  WS-QTD-OUTROS          PIC 9(07) VALUE ZERO.
       01  WS-QTD-POR-CODIGO.
           05  WS-QTD-05              PIC 9(07) VALUE ZERO.
           05  WS-QTD-14              PIC 9(07) VALUE ZERO.
           05  WS-QTD-51              PIC 9(07) VALUE ZERO.
           05  WS-QTD-54              PIC 9(07) VALUE ZERO.
           05  WS-QTD-55              PIC 9(07) VALUE ZERO.
           05  WS-QTD-57              PIC 9(07) VALUE ZERO.
           05  WS-QTD-61              PIC 9(07) VALUE ZERO.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INICIALIZAR THRU 1000-FIM-INICIALIZAR
           PERFORM 2000-PROCESSAR   THRU 2000-FIM-PROCESSAR
              UNTIL WS-FS-PED-FIM
           PERFORM 3000-FINALIZAR   THRU 3000-FIM-FINALIZAR
           STOP RUN.

       1000-INICIALIZAR.

           OPEN INPUT CONTAS-ENTRADA
           IF NOT WS-FS-CON-OK
              DISPLAY 'CICAUTOR - ERRO DE OPEN CONTAS FS: ' WS-FS-CONTAS
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           OPEN INPUT PEDIDOS
           IF NOT WS-FS-PED-OK
              DISPLAY 'CICAUTOR - ERRO DE OPEN PEDIDOS FS: '
                      WS-FS-PEDIDO
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           OPEN OUTPUT LOG-AUTORIZ
           IF NOT WS-FS-LOG-OK
              DISPLAY 'CICAUTOR - ERRO DE OPEN LOGOUT FS: ' WS-FS-LOG
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           OPEN OUTPUT CONTAS-SAIDA
           IF NOT WS-FS-COU-OK
              DISPLAY 'CICAUTOR - ERRO DE OPEN CONTOUT FS: '
                      WS-FS-CONTOUT
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF

           PERFORM 1100-CARREGAR-CONTAS THRU 1100-FIM-CARREGAR-CONTAS
           PERFORM 8000-LER-PEDIDO      THRU 8000-FIM-LER-PEDIDO
           .
       1000-FIM-INICIALIZAR.
           EXIT.

       1100-CARREGAR-CONTAS.
           PERFORM VARYING IX-CT FROM 1 BY 1 UNTIL IX-CT > WS-MAX-CONTAS
              MOVE HIGH-VALUES TO WS-CT-CARTAO(IX-CT)
           END-PERFORM
           READ CONTAS-ENTRADA
           PERFORM UNTIL WS-FS-CON-FIM
              ADD 1 TO WS-QTD-CONTAS
              IF WS-QTD-CONTAS > WS-MAX-CONTAS
                 DISPLAY 'CICAUTOR - MAIS CONTAS QUE O LIMITE DA TABELA'
                 PERFORM 9900-ENCERRAR-COM-ERRO
              END-IF
              MOVE CT-NUM-CARTAO    TO WS-CT-CARTAO(WS-QTD-CONTAS)
              MOVE CT-STATUS        TO WS-CT-STATUS(WS-QTD-CONTAS)
              MOVE CT-VALIDADE      TO WS-CT-VALIDADE(WS-QTD-CONTAS)
              MOVE CT-LIMITE-DISP   TO WS-CT-DISP(WS-QTD-CONTAS)
              MOVE CT-INTERNACIONAL TO WS-CT-EXTERIOR(WS-QTD-CONTAS)
              MOVE CT-LIMITE-SAQUE  TO WS-CT-SAQUE(WS-QTD-CONTAS)
              MOVE REG-CONTA        TO WS-CT-REGISTRO(WS-QTD-CONTAS)
              READ CONTAS-ENTRADA
           END-PERFORM
           CLOSE CONTAS-ENTRADA
           DISPLAY 'CICAUTOR - CONTAS CARREGADAS ..: ' WS-QTD-CONTAS
           .
       1100-FIM-CARREGAR-CONTAS.
           EXIT.

       2000-PROCESSAR.
      *    PREPARA O REGISTRO DE LOG COM OS DADOS DO PEDIDO
           MOVE SPACES               TO REG-LOG-AUTORIZACAO
           MOVE PD-MTI               TO LG-MTI-PEDIDO
           MOVE PD-COD-PROCESSAMENTO TO LG-COD-PROCESSAMENTO
           MOVE PD-NUM-CARTAO        TO LG-NUM-CARTAO
           MOVE PD-VALOR             TO LG-VALOR
           MOVE PD-DATA              TO LG-DATA
           MOVE PD-HORA              TO LG-HORA
           MOVE PD-STAN              TO LG-STAN
           MOVE PD-MCC               TO LG-MCC
           MOVE PD-MODO-ENTRADA      TO LG-MODO-ENTRADA
           MOVE PD-PAIS              TO LG-PAIS
           MOVE PD-ECI               TO LG-ECI
           MOVE PD-RECORRENTE        TO LG-RECORRENTE
           SET LG-PELO-EMISSOR       TO TRUE

           PERFORM 2100-LOCALIZAR-CONTA THRU 2100-FIM-LOCALIZAR-CONTA

           EVALUATE TRUE
              WHEN PD-PEDIDO-DUPLA OR PD-PEDIDO-UNICA
                 PERFORM 2200-DECIDIR      THRU 2200-FIM-DECIDIR
              WHEN PD-AVISO-STANDIN
                 PERFORM 2400-AVISO-STANDIN THRU 2400-FIM-AVISO-STANDIN
              WHEN PD-REVERSAO OR PD-AVISO-REVERSAO
                 PERFORM 2500-REVERTER     THRU 2500-FIM-REVERTER
              WHEN OTHER
                 ADD 1 TO WS-QTD-OUTROS
                 MOVE '12'                 TO LG-RESPOSTA
                 MOVE 'MENSAGEM NAO TRATADA' TO LG-DESC-RESPOSTA
           END-EVALUATE

           PERFORM 7000-GUARDAR-LOG THRU 7000-FIM-GUARDAR-LOG
           PERFORM 8000-LER-PEDIDO  THRU 8000-FIM-LER-PEDIDO
           .
       2000-FIM-PROCESSAR.
           EXIT.

       2100-LOCALIZAR-CONTA.
           SEARCH ALL WS-CT-ITEM
              AT END
                 SET WS-CONTA-NAO-ENCONTRADA TO TRUE
              WHEN WS-CT-CARTAO(IX-CT) = PD-NUM-CARTAO
                 SET WS-CONTA-ENCONTRADA     TO TRUE
           END-SEARCH
           .
       2100-FIM-LOCALIZAR-CONTA.
           EXIT.

       2200-DECIDIR.
      *    RESPOSTA: 0110 PARA O 0100 E 0210 PARA O 0200
           IF PD-PEDIDO-DUPLA
              MOVE '0110' TO LG-MTI-RESPOSTA
           ELSE
              MOVE '0210' TO LG-MTI-RESPOSTA
           END-IF
      *    A ESCREVER (AO VIVO): AS REGRAS DO EMISSOR, NA ORDEM, NUM
      *    EVALUATE TRUE. A PRIMEIRA QUE FALHA DECIDE (LG-RESPOSTA E
      *    LG-DESC-RESPOSTA, E O CONTADOR WS-QTD-NN):
      *    14 CONTA NAO ENCONTRADA        05 STATUS DIFERENTE DE 'A'
      *    54 VALIDADE < PD-ANO-MES       05 CODIGO DE SEGURANCA NAO CONFERE
      *    55 SENHA NAO CONFERE           57 EXTERIOR SEM HABILITACAO
      *    61 SAQUE ACIMA DO LIMITE DE SAQUE
      *    51 VALOR ACIMA DO DISPONIVEL   OTHER: PERFORM 2300-APROVAR
      *    POR ENQUANTO, SO A PRIMEIRA REGRA ESTA PRONTA.
           IF WS-CONTA-NAO-ENCONTRADA
              MOVE '14' TO LG-RESPOSTA
              MOVE 'CARTAO INEXISTENTE' TO LG-DESC-RESPOSTA
              ADD 1 TO WS-QTD-14
           ELSE
              PERFORM 2300-APROVAR THRU 2300-FIM-APROVAR
           END-IF
           IF NOT LG-APROVADA
              ADD 1 TO WS-QTD-NEGADAS
           END-IF
           .
       2200-FIM-DECIDIR.
           EXIT.

       2300-APROVAR.
           ADD 1                     TO WS-SEQ-AUTORIZACAO
           MOVE WS-SEQ-AUTORIZACAO   TO WS-COD-SEQ
           MOVE WS-COD-GERADO        TO LG-COD-AUTORIZACAO
           MOVE '00'                 TO LG-RESPOSTA
           MOVE 'APROVADA'           TO LG-DESC-RESPOSTA
           SUBTRACT PD-VALOR       FROM WS-CT-DISP(IX-CT)
           ADD 1                     TO WS-QTD-APROVADAS
           PERFORM 2350-REGISTRAR-APROVADA
              THRU 2350-FIM-REGISTRAR-APROVADA
           .
       2300-FIM-APROVAR.
           EXIT.

       2350-REGISTRAR-APROVADA.
      *    GUARDA A APROVADA PARA UMA EVENTUAL REVERSAO. A POSICAO NO
      *    LOG E A PROXIMA, PORQUE O LOG DESTE PEDIDO AINDA VAI SER
      *    GUARDADO NO 7000
           ADD 1 TO WS-QTD-AP
           IF WS-QTD-AP > WS-MAX-LOG
              DISPLAY 'CICAUTOR - MAIS APROVADAS QUE O LIMITE'
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           MOVE PD-NUM-CARTAO      TO WS-AP-CARTAO(WS-QTD-AP)
           MOVE LG-COD-AUTORIZACAO TO WS-AP-CODIGO(WS-QTD-AP)
           MOVE PD-VALOR           TO WS-AP-VALOR(WS-QTD-AP)
           COMPUTE WS-AP-POSICAO-LOG(WS-QTD-AP) = WS-QTD-LOG + 1
           MOVE 'N'                TO WS-AP-REVERTIDA(WS-QTD-AP)
           .
       2350-FIM-REGISTRAR-APROVADA.
           EXIT.

       2400-AVISO-STANDIN.
      *    O EMISSOR NAO RESPONDEU A TEMPO: A BANDEIRA DECIDIU COM OS
      *    PARAMETROS DE STAND-IN E AVISA AGORA. O EMISSOR NAO DECIDE
      *    DE NOVO: REGISTRA A DECISAO, ATUALIZA O LIMITE SE FOI
      *    APROVADA E CONFIRMA O RECEBIMENTO (0130)
           ADD 1 TO WS-QTD-STANDIN
           MOVE '0130'               TO LG-MTI-RESPOSTA
           SET LG-EM-STAND-IN        TO TRUE
           MOVE PD-RESPOSTA-STANDIN  TO LG-RESPOSTA
           MOVE PD-COD-AUT-ORIGINAL  TO LG-COD-AUTORIZACAO
           IF LG-APROVADA
              MOVE 'APROVADA EM STAND-IN' TO LG-DESC-RESPOSTA
              IF WS-CONTA-ENCONTRADA
                 SUBTRACT PD-VALOR FROM WS-CT-DISP(IX-CT)
              END-IF
              PERFORM 2350-REGISTRAR-APROVADA
                 THRU 2350-FIM-REGISTRAR-APROVADA
           ELSE
              MOVE 'NEGADA EM STAND-IN'   TO LG-DESC-RESPOSTA
           END-IF
           .
       2400-FIM-AVISO-STANDIN.
           EXIT.

       2500-REVERTER.
           IF PD-REVERSAO
              MOVE '0410' TO LG-MTI-RESPOSTA
           ELSE
              MOVE '0430' TO LG-MTI-RESPOSTA
           END-IF
           MOVE PD-COD-AUT-ORIGINAL TO LG-COD-AUTORIZACAO
      *    A ESCREVER (MAO NA MASSA): PROCURAR NA WS-TABELA-APROVADAS A
      *    AUTORIZACAO ORIGINAL (MESMO CARTAO, MESMO CODIGO, AINDA NAO
      *    REVERTIDA). SE ACHOU: MARCAR WS-AP-REVERTIDA, DEVOLVER O VALOR
      *    AO DISPONIVEL DA CONTA, MARCAR 'R' NA POSICAO 83 DO LOG GUARDADO
      *    (WS-LG-REGISTRO(IX-LOG)(83:1)) E RESPONDER '00'.
      *    SE NAO ACHOU: RESPONDER '25' (ORIGINAL NAO ENCONTRADA).
           MOVE '25' TO LG-RESPOSTA
           MOVE 'ORIGINAL NAO ENCONTRADA' TO LG-DESC-RESPOSTA
           ADD 1 TO WS-QTD-REV-SEM-ORIG
           .
       2500-FIM-REVERTER.
           EXIT.

       3000-FINALIZAR.
      *    GRAVA O LOG COMPLETO E AS CONTAS COM OS LIMITES ATUALIZADOS
           PERFORM VARYING IX-LOG FROM 1 BY 1 UNTIL IX-LOG > WS-QTD-LOG
              WRITE REG-LOG-AUTORIZACAO FROM WS-LG-REGISTRO(IX-LOG)
              IF NOT WS-FS-LOG-OK
                 DISPLAY 'CICAUTOR - ERRO DE WRITE NO LOGOUT FS: '
                         WS-FS-LOG
                 PERFORM 9900-ENCERRAR-COM-ERRO
              END-IF
           END-PERFORM
           PERFORM VARYING IX-CT FROM 1 BY 1
                   UNTIL IX-CT > WS-QTD-CONTAS
      *       O DISPONIVEL FICA NAS POSICOES 38 A 49 DO CPCONTA
              MOVE WS-CT-REGISTRO(IX-CT) TO REG-CONTA-SAIDA
              MOVE WS-CT-DISP(IX-CT)     TO WS-DISP-SAIDA
              MOVE WS-DISP-SAIDA-X       TO REG-CONTA-SAIDA(38:12)
              WRITE REG-CONTA-SAIDA
              IF NOT WS-FS-COU-OK
                 DISPLAY 'CICAUTOR - ERRO DE WRITE NO CONTOUT FS: '
                         WS-FS-CONTOUT
                 PERFORM 9900-ENCERRAR-COM-ERRO
              END-IF
           END-PERFORM
           CLOSE PEDIDOS LOG-AUTORIZ CONTAS-SAIDA

           DISPLAY 'CICAUTOR - PEDIDOS LIDOS ......: ' WS-QTD-LIDOS
           DISPLAY 'CICAUTOR - APROVADAS (00) .....: ' WS-QTD-APROVADAS
           DISPLAY 'CICAUTOR - NEGADAS ............: ' WS-QTD-NEGADAS
           DISPLAY 'CICAUTOR -   05 NAO AUTORIZADA : ' WS-QTD-05
           DISPLAY 'CICAUTOR -   14 CARTAO INVALIDO: ' WS-QTD-14
           DISPLAY 'CICAUTOR -   51 SEM LIMITE ....: ' WS-QTD-51
           DISPLAY 'CICAUTOR -   54 VENCIDO .......: ' WS-QTD-54
           DISPLAY 'CICAUTOR -   55 SENHA .........: ' WS-QTD-55
           DISPLAY 'CICAUTOR -   57 NAO PERMITIDA .: ' WS-QTD-57
           DISPLAY 'CICAUTOR -   61 LIMITE SAQUE ..: ' WS-QTD-61
           DISPLAY 'CICAUTOR - AVISOS DE STAND-IN .: ' WS-QTD-STANDIN
           DISPLAY 'CICAUTOR - REVERSOES EFETUADAS : ' WS-QTD-REVERTIDAS
           DISPLAY 'CICAUTOR - REVERSOES SEM ORIG. : '
                   WS-QTD-REV-SEM-ORIG
           DISPLAY 'CICAUTOR - REGISTROS NO LOG ...: ' WS-QTD-LOG
      *    CONFERENCIA: CADA PEDIDO GERA UM REGISTRO NO LOG
           IF WS-QTD-LIDOS NOT = WS-QTD-LOG
              DISPLAY 'CICAUTOR - CONTADORES NAO FECHAM'
              MOVE 16 TO RETURN-CODE
           ELSE
              MOVE 0 TO RETURN-CODE
           END-IF
           .
       3000-FIM-FINALIZAR.
           EXIT.

       7000-GUARDAR-LOG.
           ADD 1 TO WS-QTD-LOG
           IF WS-QTD-LOG > WS-MAX-LOG
              DISPLAY 'CICAUTOR - MAIS PEDIDOS QUE O LIMITE DO LOG'
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           MOVE REG-LOG-AUTORIZACAO TO WS-LG-REGISTRO(WS-QTD-LOG)
           .
       7000-FIM-GUARDAR-LOG.
           EXIT.

       8000-LER-PEDIDO.
           READ PEDIDOS
                AT END
                   CONTINUE
                NOT AT END
                   ADD 1 TO WS-QTD-LIDOS
           END-READ
           IF NOT WS-FS-PED-OK AND NOT WS-FS-PED-FIM
              DISPLAY 'CICAUTOR - ERRO DE READ PEDIDOS FS: '
                      WS-FS-PEDIDO
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           .
       8000-FIM-LER-PEDIDO.
           EXIT.

       9900-ENCERRAR-COM-ERRO.
      *    ENCERRA COM RC 16 PARA IMPEDIR OS STEPS SEGUINTES
           MOVE 16 TO RETURN-CODE
           STOP RUN
           .
