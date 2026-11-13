      *================================================================*
      * PROGRAMA : CICBIN (ESQUELETO DA TURMA)                         *
      * AULA     : 1 - ECOSSISTEMA E PAPEIS                            *
      * OBJETIVO : IDENTIFICAR, PELO BIN DE 8 DIGITOS, A BANDEIRA, O   *
      *            EMISSOR E O PRODUTO DE CADA COMPRA E SIMULAR A      *
      *            DIVISAO DO VALOR ENTRE OS PARTICIPANTES:            *
      *            MDR (TAXA DE DESCONTO) = INTERCAMBIO (EMISSOR)      *
      *              + TARIFA DE BANDEIRA + RECEITA DO CREDENCIADOR    *
      *            LIQUIDO DO ESTABELECIMENTO = VALOR - MDR            *
      *            AS TAXAS SAO FICTICIAS, UMA POR PRODUTO.            *
      * ENTRADAS : DD BINTAB   - CPBIN, FB 80, ORDENADA POR BIN        *
      *            DD PEDIDOS  - CPAUTPED, FB 80                       *
      * SAIDA    : DD RELBIN   - RELATORIO, FBA 133                    *
      * RETORNO  : 0 = OK, 4 = HOUVE BIN NAO CADASTRADO,               *
      *            16 = ERRO DE ARQUIVO                                *
      * A ESCREVER NA AULA 1:                                          *
      * 2200-DIVIDIR-VALOR (AO VIVO, COM A TURMA)                      *
      * 3100-RESUMO-PARTICIPANTES (DESAFIO DO FIM DE SEMANA)           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICBIN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TABELA-BIN  ASSIGN TO BINTAB
                  FILE STATUS IS WS-FS-BIN.
           SELECT PEDIDOS     ASSIGN TO PEDIDOS
                  FILE STATUS IS WS-FS-PEDIDO.
           SELECT RELATORIO   ASSIGN TO RELBIN
                  FILE STATUS IS WS-FS-RELATORIO.

       DATA DIVISION.
       FILE SECTION.
       FD  TABELA-BIN
           RECORDING MODE IS F.
           COPY CPBIN.
       FD  PEDIDOS
           RECORDING MODE IS F.
           COPY CPAUTPED.
       FD  RELATORIO
           RECORDING MODE IS F.
       01  REG-RELATORIO              PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-FS-BIN                  PIC X(02).
           88  WS-FS-BIN-OK                     VALUE '00'.
           88  WS-FS-BIN-FIM                    VALUE '10'.
       01  WS-FS-PEDIDO               PIC X(02).
           88  WS-FS-PED-OK                     VALUE '00'.
           88  WS-FS-PED-FIM                    VALUE '10'.
       01  WS-FS-RELATORIO            PIC X(02).
           88  WS-FS-REL-OK                     VALUE '00'.

      *--- TABELA DE BINS EM MEMORIA (SEARCH ALL) ---------------------*
      *    AS POSICOES NAO USADAS FICAM COM 99999999 PARA A TABELA
      *    CONTINUAR EM ORDEM CRESCENTE
       01  WS-MAX-BIN                 PIC 9(03) VALUE 50.
       01  WS-QTD-BIN                 PIC 9(03) VALUE ZERO.
       01  WS-TABELA-BIN.
           05  WS-BIN-ITEM OCCURS 50 TIMES
                           ASCENDING KEY IS WS-BIN-CODIGO
                           INDEXED BY IX-BIN.
               10  WS-BIN-CODIGO      PIC 9(08).
               10  WS-BIN-BANDEIRA    PIC X(10).
               10  WS-BIN-EMISSOR     PIC X(20).
               10  WS-BIN-PRODUTO     PIC X(01).

      *--- TAXAS FICTICIAS POR PRODUTO (PERCENTUAIS) ------------------*
      *    CREDITO: MDR 2,20  INTERCAMBIO 1,50  BANDEIRA 0,10
      *    DEBITO : MDR 1,10  INTERCAMBIO 0,50  BANDEIRA 0,05
      *    PRE-PAG: MDR 1,40  INTERCAMBIO 0,70  BANDEIRA 0,05
       01  WS-TAXAS.
           05  WS-TX-MDR              PIC 9(01)V9(04).
           05  WS-TX-INTERCAMBIO      PIC 9(01)V9(04).
           05  WS-TX-BANDEIRA         PIC 9(01)V9(04).

       01  WS-BIN-PEDIDO              PIC 9(08).
       01  WS-ACHOU-BIN               PIC X(01).
           88  WS-BIN-ENCONTRADO                VALUE 'S'.
           88  WS-BIN-NAO-ENCONTRADO            VALUE 'N'.

      *--- VALORES DE UMA COMPRA --------------------------------------*
       01  WS-VALORES.
           05  WS-VL-MDR              PIC 9(09)V99 VALUE ZERO.
           05  WS-VL-INTERCAMBIO      PIC 9(09)V99 VALUE ZERO.
           05  WS-VL-BANDEIRA         PIC 9(09)V99 VALUE ZERO.
           05  WS-VL-CREDENCIADOR     PIC 9(09)V99 VALUE ZERO.
           05  WS-VL-LIQUIDO          PIC 9(09)V99 VALUE ZERO.

      *--- TOTAIS POR PARTICIPANTE ------------------------------------*
       01  WS-TOTAIS.
           05  WS-TT-VALOR            PIC 9(11)V99 VALUE ZERO.
           05  WS-TT-MDR              PIC 9(11)V99 VALUE ZERO.
           05  WS-TT-INTERCAMBIO      PIC 9(11)V99 VALUE ZERO.
           05  WS-TT-BANDEIRA         PIC 9(11)V99 VALUE ZERO.
           05  WS-TT-CREDENCIADOR     PIC 9(11)V99 VALUE ZERO.
           05  WS-TT-LIQUIDO          PIC 9(11)V99 VALUE ZERO.

       01  WS-CONTADORES.
           05  WS-QTD-LIDOS           PIC 9(07) VALUE ZERO.
           05  WS-QTD-COMPRAS         PIC 9(07) VALUE ZERO.
           05  WS-QTD-SEM-BIN         PIC 9(07) VALUE ZERO.
           05  WS-QTD-IGNORADOS       PIC 9(07) VALUE ZERO.

      *--- MASCARAMENTO -----------------------------------------------*
       01  WS-CONFERENCIA             PIC X(01) VALUE 'S'.
           88  WS-CONTAS-FECHAM                 VALUE 'S'.
           88  WS-CONTAS-NAO-FECHAM             VALUE 'N'.

       01  WS-CARTAO-ABERTO           PIC X(16).
       01  WS-CARTAO-MASC             PIC X(16).

      *--- LINHAS DO RELATORIO ----------------------------------------*
       01  WS-CAB-1.
           05  FILLER                 PIC X(60) VALUE
               'CICLO DO CARTAO - IDENTIFICACAO E DIVISAO DO VALOR'.
           05  FILLER                 PIC X(20) VALUE
               '(DADOS FICTICIOS)'.
       01  WS-CAB-2.
           05  FILLER                 PIC X(18) VALUE 'CARTAO'.
           05  FILLER                 PIC X(12) VALUE 'BANDEIRA'.
           05  FILLER                 PIC X(22) VALUE 'EMISSOR'.
           05  FILLER                 PIC X(04) VALUE 'PRD'.
           05  FILLER                 PIC X(14) VALUE '         VALOR'.
           05  FILLER                 PIC X(12) VALUE '         MDR'.
           05  FILLER                 PIC X(12) VALUE ' INTERCAMBIO'.
           05  FILLER                 PIC X(12) VALUE '    BANDEIRA'.
           05  FILLER                 PIC X(12) VALUE ' CREDENCIAD.'.
           05  FILLER                 PIC X(14) VALUE '       LIQUIDO'.
       01  WS-DETALHE.
           05  DT-CARTAO              PIC X(16).
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  DT-BANDEIRA            PIC X(10).
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  DT-EMISSOR             PIC X(20).
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  DT-PRODUTO             PIC X(01).
           05  FILLER                 PIC X(01) VALUE SPACES.
           05  DT-VALOR               PIC ZZZ,ZZZ,ZZ9.99.
           05  DT-MDR                 PIC ZZZ,ZZ9.99.
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  DT-INTERCAMBIO         PIC ZZZ,ZZ9.99.
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  DT-TARIFA              PIC ZZZ,ZZ9.99.
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  DT-CREDENCIADOR        PIC ZZZ,ZZ9.99.
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  DT-LIQUIDO             PIC ZZZ,ZZZ,ZZ9.99.
       01  WS-LINHA-SEM-BIN.
           05  SB-CARTAO              PIC X(16).
           05  FILLER                 PIC X(02) VALUE SPACES.
           05  FILLER                 PIC X(40) VALUE
               'BIN NAO CADASTRADO: COMPRA NAO ROTEADA'.
       01  WS-LINHA-TOTAL.
           05  LT-DESCRICAO           PIC X(50).
           05  LT-VALOR               PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  WS-LINHA-CONFERE.
           05  CF-TEXTO               PIC X(60).
           05  CF-RESULTADO           PIC X(20).

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INICIALIZAR THRU 1000-FIM-INICIALIZAR
           PERFORM 2000-PROCESSAR   THRU 2000-FIM-PROCESSAR
              UNTIL WS-FS-PED-FIM
           PERFORM 3000-FINALIZAR   THRU 3000-FIM-FINALIZAR
           STOP RUN.

       1000-INICIALIZAR.

           OPEN INPUT TABELA-BIN
           IF NOT WS-FS-BIN-OK
              DISPLAY 'CICBIN - ERRO DE OPEN TABELA-BIN (BINTAB) FS: '
                      WS-FS-BIN
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           OPEN INPUT PEDIDOS
           IF NOT WS-FS-PED-OK
              DISPLAY 'CICBIN - ERRO DE OPEN PEDIDOS (PEDIDOS) FS: '
                      WS-FS-PEDIDO
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           OPEN OUTPUT RELATORIO
           IF NOT WS-FS-REL-OK
              DISPLAY 'CICBIN - ERRO DE OPEN RELATORIO (RELBIN) FS: '
                      WS-FS-RELATORIO
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF

           PERFORM 1100-CARREGAR-BINS THRU 1100-FIM-CARREGAR-BINS

           WRITE REG-RELATORIO FROM WS-CAB-1 AFTER ADVANCING PAGE
           WRITE REG-RELATORIO FROM WS-CAB-2 AFTER ADVANCING 2 LINES
           MOVE SPACES TO REG-RELATORIO
           WRITE REG-RELATORIO AFTER ADVANCING 1 LINE

           PERFORM 8000-LER-PEDIDO THRU 8000-FIM-LER-PEDIDO
           .
       1000-FIM-INICIALIZAR.
           EXIT.

       1100-CARREGAR-BINS.
      *    POSICOES VAZIAS COM O MAIOR BIN POSSIVEL (ORDEM CRESCENTE)
           PERFORM VARYING IX-BIN FROM 1 BY 1 UNTIL IX-BIN > WS-MAX-BIN
              MOVE 99999999 TO WS-BIN-CODIGO(IX-BIN)
           END-PERFORM
           READ TABELA-BIN
           PERFORM UNTIL WS-FS-BIN-FIM
              ADD 1 TO WS-QTD-BIN
              IF WS-QTD-BIN > WS-MAX-BIN
                 DISPLAY 'CICBIN - TABELA DE BINS MAIOR QUE O LIMITE'
                 PERFORM 9900-ENCERRAR-COM-ERRO
              END-IF
              MOVE BN-BIN           TO WS-BIN-CODIGO(WS-QTD-BIN)
              MOVE BN-NOME-BANDEIRA TO WS-BIN-BANDEIRA(WS-QTD-BIN)
              MOVE BN-NOME-EMISSOR  TO WS-BIN-EMISSOR(WS-QTD-BIN)
              MOVE BN-PRODUTO       TO WS-BIN-PRODUTO(WS-QTD-BIN)
              READ TABELA-BIN
           END-PERFORM
           CLOSE TABELA-BIN
           DISPLAY 'CICBIN - BINS CARREGADOS ......: ' WS-QTD-BIN
           .
       1100-FIM-CARREGAR-BINS.
           EXIT.

       2000-PROCESSAR.
      *    SO AS COMPRAS (0100, 0200 E O AVISO 0120) ENTRAM NA DIVISAO;
      *    SAQUES E REVERSOES NAO SAO VENDAS DE UM ESTABELECIMENTO
           IF (PD-PEDIDO-DUPLA OR PD-PEDIDO-UNICA OR PD-AVISO-STANDIN)
              AND PD-COMPRA
              ADD 1 TO WS-QTD-COMPRAS
              PERFORM 2100-IDENTIFICAR THRU 2100-FIM-IDENTIFICAR
              IF WS-BIN-ENCONTRADO
                 PERFORM 2200-DIVIDIR-VALOR THRU 2200-FIM-DIVIDIR-VALOR
                 PERFORM 2300-IMPRIMIR      THRU 2300-FIM-IMPRIMIR
              ELSE
                 ADD 1 TO WS-QTD-SEM-BIN
                 MOVE PD-NUM-CARTAO TO WS-CARTAO-ABERTO
                 PERFORM 9800-MASCARAR-CARTAO
                 MOVE WS-CARTAO-MASC TO SB-CARTAO
                 WRITE REG-RELATORIO FROM WS-LINHA-SEM-BIN
                       AFTER ADVANCING 1 LINE
              END-IF
           ELSE
              ADD 1 TO WS-QTD-IGNORADOS
           END-IF
           PERFORM 8000-LER-PEDIDO THRU 8000-FIM-LER-PEDIDO
           .
       2000-FIM-PROCESSAR.
           EXIT.

       2100-IDENTIFICAR.
      *    O BIN SAO OS 8 PRIMEIROS DIGITOS DO NUMERO DO CARTAO
           MOVE PD-NUM-CARTAO(1:8) TO WS-BIN-PEDIDO
           SEARCH ALL WS-BIN-ITEM
              AT END
                 SET WS-BIN-NAO-ENCONTRADO TO TRUE
              WHEN WS-BIN-CODIGO(IX-BIN) = WS-BIN-PEDIDO
                 SET WS-BIN-ENCONTRADO     TO TRUE
           END-SEARCH
           .
       2100-FIM-IDENTIFICAR.
           EXIT.

       2200-DIVIDIR-VALOR.
      *    A ESCREVER (AO VIVO): ESCOLHER AS TAXAS PELO PRODUTO DO BIN
      *    (WS-BIN-PRODUTO(IX-BIN): C, D OU P) E CALCULAR, COM ROUNDED:
      *    WS-VL-MDR, WS-VL-INTERCAMBIO E WS-VL-BANDEIRA. DEPOIS:
      *    CREDENCIADOR = MDR - INTERCAMBIO - BANDEIRA
      *    LIQUIDO = VALOR - MDR
      *    E SOMAR TUDO NOS TOTAIS (WS-TT-...).
           CONTINUE
           .
       2200-FIM-DIVIDIR-VALOR.
           EXIT.

       2300-IMPRIMIR.
           MOVE PD-NUM-CARTAO            TO WS-CARTAO-ABERTO
           PERFORM 9800-MASCARAR-CARTAO
           MOVE WS-CARTAO-MASC           TO DT-CARTAO
           MOVE WS-BIN-BANDEIRA(IX-BIN)  TO DT-BANDEIRA
           MOVE WS-BIN-EMISSOR(IX-BIN)   TO DT-EMISSOR
           MOVE WS-BIN-PRODUTO(IX-BIN)   TO DT-PRODUTO
           MOVE PD-VALOR                 TO DT-VALOR
           MOVE WS-VL-MDR                TO DT-MDR
           MOVE WS-VL-INTERCAMBIO        TO DT-INTERCAMBIO
           MOVE WS-VL-BANDEIRA           TO DT-TARIFA
           MOVE WS-VL-CREDENCIADOR       TO DT-CREDENCIADOR
           MOVE WS-VL-LIQUIDO            TO DT-LIQUIDO
           WRITE REG-RELATORIO FROM WS-DETALHE AFTER ADVANCING 1 LINE
           IF NOT WS-FS-REL-OK
              DISPLAY 'CICBIN - ERRO DE WRITE NO RELBIN FS: '
                      WS-FS-RELATORIO
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           .
       2300-FIM-IMPRIMIR.
           EXIT.

       3000-FINALIZAR.
           PERFORM 3100-RESUMO-PARTICIPANTES
              THRU 3100-FIM-RESUMO-PARTICIPANTES
           CLOSE PEDIDOS RELATORIO
           DISPLAY 'CICBIN - PEDIDOS LIDOS ........: ' WS-QTD-LIDOS
           DISPLAY 'CICBIN - COMPRAS ..............: ' WS-QTD-COMPRAS
           DISPLAY 'CICBIN -   COM BIN NAO CADASTR.: ' WS-QTD-SEM-BIN
           DISPLAY 'CICBIN - IGNORADOS (SAQUE/REV.): ' WS-QTD-IGNORADOS
           EVALUATE TRUE
              WHEN WS-CONTAS-NAO-FECHAM
                 MOVE 8 TO RETURN-CODE
              WHEN WS-QTD-SEM-BIN > ZERO
                 MOVE 4 TO RETURN-CODE
              WHEN OTHER
                 MOVE 0 TO RETURN-CODE
           END-EVALUATE
           .
       3000-FIM-FINALIZAR.
           EXIT.

       3100-RESUMO-PARTICIPANTES.
      *    A ESCREVER (DESAFIO): IMPRIMIR NO RELATORIO O RESUMO POR
      *    PARTICIPANTE (ESTABELECIMENTOS, CREDENCIADOR, EMISSORES,
      *    BANDEIRAS) COM AS LINHAS WS-LINHA-TOTAL E PROVAR QUE:
      *    1. VALOR = LIQUIDO + MDR
      *    2. MDR = INTERCAMBIO + BANDEIRA + CREDENCIADOR
      *    SE ALGUMA CONTA NAO FECHAR: SET WS-CONTAS-NAO-FECHAM TO TRUE.
           CONTINUE
           .
       3100-FIM-RESUMO-PARTICIPANTES.
           EXIT.

       8000-LER-PEDIDO.
           READ PEDIDOS
                AT END
                   CONTINUE
                NOT AT END
                   ADD 1 TO WS-QTD-LIDOS
           END-READ
           IF NOT WS-FS-PED-OK AND NOT WS-FS-PED-FIM
              DISPLAY 'CICBIN - ERRO DE READ PEDIDOS FS: ' WS-FS-PEDIDO
              PERFORM 9900-ENCERRAR-COM-ERRO
           END-IF
           .
       8000-FIM-LER-PEDIDO.
           EXIT.

           COPY CPMASCAR.

       9900-ENCERRAR-COM-ERRO.
      *    ENCERRA COM RC 16 PARA IMPEDIR OS STEPS SEGUINTES
           MOVE 16 TO RETURN-CODE
           STOP RUN
           .
