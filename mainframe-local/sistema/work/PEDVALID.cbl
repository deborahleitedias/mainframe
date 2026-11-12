      *================================================================*
      * PROGRAMA : PEDVALID                                            *
      * SISTEMA  : PEDAGIO NOTURNO (DADOS 100% FICTICIOS)              *
      * OBJETIVO : LER AS PASSAGENS DO DIA (JA ORDENADAS PELO SORT),   *
      *            VALIDAR CADA UMA CONTRA A TABELA DE TARIFAS E       *
      *            SEPARAR EM DOIS ARQUIVOS: APROVADAS (FORMATO        *
      *            INTERNO, VALOR EM COMP-3) E REJEITADAS (REGISTRO    *
      *            ORIGINAL + MOTIVO).                                 *
      * ENTRADAS : DD PASSIN   - PASSAGENS, FB 80                      *
      *            DD TARIFA   - TABELA DE TARIFAS, FB 80, ORDENADA    *
      *                          POR PRACA E CATEGORIA                 *
      * SAIDAS   : DD APROVOUT - APROVADAS, FB 80                      *
      *            DD REJEIOUT - REJEITADAS, FB 120                    *
      * RETORNO  : 0 = OK, 4 = HOUVE REJEITADAS, 16 = ERRO DE ARQUIVO  *
      * HISTORICO: 01/10/2026 VERSAO INICIAL                           *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PEDVALID.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PASSAGEM       ASSIGN TO PASSIN
                  FILE STATUS IS WS-FS-PASSAGEM.
           SELECT TABELA-TARIFA  ASSIGN TO TARIFA
                  FILE STATUS IS WS-FS-TARIFA.
           SELECT APROVADA       ASSIGN TO APROVOUT
                  FILE STATUS IS WS-FS-APROVADA.
           SELECT REJEITADA      ASSIGN TO REJEIOUT
                  FILE STATUS IS WS-FS-REJEITADA.

       DATA DIVISION.
       FILE SECTION.
      *--- PASSAGEM RECEBIDA DAS PRACAS (TEXTO, 80 COLUNAS) -----------*
       FD  PASSAGEM
           RECORDING MODE IS F.
       01  REG-PASSAGEM.
           05  PS-TAG                 PIC X(12).
           05  PS-PRACA               PIC X(03).
           05  PS-DATA                PIC 9(08).
           05  PS-HORA                PIC 9(06).
           05  PS-CATEGORIA           PIC 9(01).
               88  PS-ISENTA                    VALUE 9.
           05  PS-VALOR               PIC 9(05)V99.
           05  PS-SENTIDO             PIC X(01).
           05  FILLER                 PIC X(42).
      *--- TABELA DE TARIFAS (TEXTO, 80 COLUNAS) ----------------------*
       FD  TABELA-TARIFA
           RECORDING MODE IS F.
       01  REG-TARIFA.
           05  TF-CHAVE.
               10  TF-PRACA           PIC X(03).
               10  TF-CATEGORIA       PIC 9(01).
           05  TF-VALOR               PIC 9(03)V99.
           05  TF-NOME-PRACA          PIC X(20).
           05  FILLER                 PIC X(51).
      *--- PASSAGEM APROVADA (LAYOUT IGUAL AO DO PEDEXTRT) ------------*
      *    ESTE ARQUIVO E BINARIO (COMP-3): NAO TRANSFERIR COMO TEXTO.
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
      *--- PASSAGEM REJEITADA: REGISTRO ORIGINAL + MOTIVO -------------*
      *    INSUMO DE REPROCESSAMENTO, DE ACESSO RESTRITO.
       FD  REJEITADA
           RECORDING MODE IS F.
       01  REG-REJEITADA.
           05  RJ-REGISTRO-ORIGINAL   PIC X(80).
           05  RJ-COD-MOTIVO          PIC 9(02).
           05  RJ-DESC-MOTIVO         PIC X(30).
           05  FILLER                 PIC X(08).

       WORKING-STORAGE SECTION.
       01  WS-FS-PASSAGEM             PIC X(02).
           88  WS-FS-PAS-OK                     VALUE '00'.
           88  WS-FS-PAS-FIM                    VALUE '10'.
       01  WS-FS-TARIFA               PIC X(02).
           88  WS-FS-TAR-OK                     VALUE '00'.
           88  WS-FS-TAR-FIM                    VALUE '10'.
       01  WS-FS-APROVADA             PIC X(02).
           88  WS-FS-APR-OK                     VALUE '00'.
       01  WS-FS-REJEITADA            PIC X(02).
           88  WS-FS-REJ-OK                     VALUE '00'.

       01  WS-CONTADORES.
           05  WS-QTD-LIDAS           PIC 9(07) VALUE ZERO.
           05  WS-QTD-APROVADAS       PIC 9(07) VALUE ZERO.
           05  WS-QTD-ISENTAS         PIC 9(07) VALUE ZERO.
           05  WS-QTD-REJEITADAS      PIC 9(07) VALUE ZERO.
           05  WS-QTD-POR-MOTIVO      PIC 9(07) OCCURS 6 TIMES.
       01  WS-VALOR-APROVADO          PIC S9(09)V99 COMP-3 VALUE ZERO.
       01  WS-VALOR-EDITADO           PIC ZZZ,ZZZ,ZZ9.99.
       01  WS-MOT                     PIC 9(02).

      *--- MOTIVOS DE REJEICAO ----------------------------------------*
       01  WS-MOTIVOS-TEXTO.
           05  FILLER  PIC X(30) VALUE 'TAG INVALIDA'.
           05  FILLER  PIC X(30) VALUE 'DATA DA PASSAGEM INVALIDA'.
           05  FILLER  PIC X(30) VALUE 'VALOR NAO NUMERICO'.
           05  FILLER  PIC X(30) VALUE 'PRACA/CATEGORIA SEM TARIFA'.
           05  FILLER  PIC X(30) VALUE 'VALOR DIFERENTE DA TARIFA'.
           05  FILLER  PIC X(30) VALUE 'PASSAGEM ISENTA, SEM COBRANCA'.
       01  WS-MOTIVOS REDEFINES WS-MOTIVOS-TEXTO.
           05  WS-DESC-MOTIVO         PIC X(30) OCCURS 6 TIMES.

       01  WS-VALIDACAO               PIC X(01).
           88  WS-REGISTRO-VALIDO               VALUE 'S'.
           88  WS-REGISTRO-INVALIDO             VALUE 'N'.

       01  WS-DATA-VALIDAR.
           05  WS-DV-ANO              PIC 9(04).
           05  WS-DV-MES              PIC 9(02).
               88  WS-MES-VALIDO                VALUE 01 THRU 12.
           05  WS-DV-DIA              PIC 9(02).
               88  WS-DIA-VALIDO                VALUE 01 THRU 31.

      *--- TABELA DE TARIFAS EM MEMORIA (CARREGADA DO DD TARIFA) ------*
      *    AS POSICOES NAO USADAS FICAM COM HIGH-VALUES NA CHAVE PARA
      *    QUE A TABELA CONTINUE EM ORDEM CRESCENTE (EXIGENCIA DO
      *    SEARCH ALL). O ARQUIVO PRECISA VIR ORDENADO POR CHAVE.
       01  WS-MAX-TARIFA              PIC 9(03) VALUE 80.
       01  WS-QTD-TARIFA              PIC 9(03) VALUE ZERO.
       01  WS-TABELA-TARIFA.
           05  WS-TF-ITEM OCCURS 80 TIMES
                          ASCENDING KEY IS WS-TF-CHAVE
                          INDEXED BY IX-TF.
               10  WS-TF-CHAVE        PIC X(04).
               10  WS-TF-VALOR        PIC 9(03)V99.
               10  WS-TF-NOME         PIC X(20).

       01  WS-CHAVE-BUSCA.
           05  WS-CB-PRACA            PIC X(03).
           05  WS-CB-CATEGORIA        PIC 9(01).
       01  WS-TARIFA-ACHADA           PIC X(01).
           88  WS-ACHOU-TARIFA                  VALUE 'S'.
           88  WS-SEM-TARIFA                    VALUE 'N'.
       01  WS-VALOR-TARIFA            PIC 9(03)V99.
       01  WS-NOME-PRACA              PIC X(20).

       PROCEDURE DIVISION.
       0000-PRINCIPAL.
           PERFORM 1000-INICIALIZAR THRU 1000-FIM-INICIALIZAR
           PERFORM 2000-PROCESSAR THRU 2000-FIM-PROCESSAR
              UNTIL WS-FS-PAS-FIM
           PERFORM 3000-FINALIZAR THRU 3000-FIM-FINALIZAR
           STOP RUN.
       0000-FIM-PRINCIPAL.
           EXIT.

       1000-INICIALIZAR.
           INITIALIZE WS-CONTADORES
           OPEN INPUT TABELA-TARIFA
           IF NOT WS-FS-TAR-OK
              DISPLAY 'PEDVALID - ERRO NO OPEN DE TARIFA. FS='
                      WS-FS-TARIFA
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR-COM-ERRO
           END-IF
           PERFORM VARYING IX-TF FROM 1 BY 1
                   UNTIL IX-TF > WS-MAX-TARIFA
              MOVE HIGH-VALUES TO WS-TF-CHAVE(IX-TF)
              MOVE ZERO        TO WS-TF-VALOR(IX-TF)
              MOVE SPACES      TO WS-TF-NOME(IX-TF)
           END-PERFORM
           PERFORM 1100-CARREGAR-TARIFA THRU 1100-FIM-CARREGAR-TARIFA
              UNTIL WS-FS-TAR-FIM
           CLOSE TABELA-TARIFA
           IF NOT WS-FS-TAR-OK
              DISPLAY 'PEDVALID - ERRO NO CLOSE DE TARIFA. FS='
                      WS-FS-TARIFA
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR-COM-ERRO
           END-IF

           OPEN INPUT  PASSAGEM
           IF NOT WS-FS-PAS-OK
              DISPLAY 'PEDVALID - ERRO NO OPEN DE PASSIN. FS='
                      WS-FS-PASSAGEM
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR-COM-ERRO
           END-IF
           OPEN OUTPUT APROVADA
           IF NOT WS-FS-APR-OK
              DISPLAY 'PEDVALID - ERRO NO OPEN DE APROVOUT. FS='
                      WS-FS-APROVADA
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR-COM-ERRO
           END-IF
           OPEN OUTPUT REJEITADA
           IF NOT WS-FS-REJ-OK
              DISPLAY 'PEDVALID - ERRO NO OPEN DE REJEIOUT. FS='
                      WS-FS-REJEITADA
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR-COM-ERRO
           END-IF
      *    LEITURA ANTECIPADA: A PRIMEIRA PASSAGEM E LIDA AQUI
           PERFORM 8000-LER-PASSAGEM THRU 8000-FIM-LER-PASSAGEM.
       1000-FIM-INICIALIZAR.
           EXIT.

       1100-CARREGAR-TARIFA.
           READ TABELA-TARIFA
              AT END
                 CONTINUE
              NOT AT END
                 IF WS-QTD-TARIFA >= WS-MAX-TARIFA
                    DISPLAY 'PEDVALID - TABELA DE TARIFAS CHEIA'
                    PERFORM 9900-ENCERRAR-COM-ERRO
                       THRU 9900-FIM-ENCERRAR-COM-ERRO
                 END-IF
                 ADD 1 TO WS-QTD-TARIFA
                 MOVE TF-CHAVE      TO WS-TF-CHAVE(WS-QTD-TARIFA)
                 MOVE TF-VALOR      TO WS-TF-VALOR(WS-QTD-TARIFA)
                 MOVE TF-NOME-PRACA TO WS-TF-NOME(WS-QTD-TARIFA)
           END-READ
           IF NOT WS-FS-TAR-OK AND NOT WS-FS-TAR-FIM
              DISPLAY 'PEDVALID - ERRO NO READ DE TARIFA. FS='
                      WS-FS-TARIFA
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR-COM-ERRO
           END-IF.
       1100-FIM-CARREGAR-TARIFA.
           EXIT.

       2000-PROCESSAR.
           PERFORM 2100-VALIDAR-PASSAGEM THRU 2100-FIM-VALIDAR-PASSAGEM
           IF WS-REGISTRO-VALIDO
              PERFORM 2200-GRAVAR-APROVADA THRU 2200-FIM-GRAVAR-APROVADA
           ELSE
              PERFORM 2300-GRAVAR-REJEITADA
                 THRU 2300-FIM-GRAVAR-REJEITADA
           END-IF
           PERFORM 8000-LER-PASSAGEM THRU 8000-FIM-LER-PASSAGEM.
       2000-FIM-PROCESSAR.
           EXIT.

       2100-VALIDAR-PASSAGEM.
           SET WS-REGISTRO-VALIDO TO TRUE
           MOVE PS-DATA TO WS-DATA-VALIDAR
           EVALUATE TRUE
              WHEN PS-TAG IS NOT NUMERIC
                 MOVE 1 TO WS-MOT
              WHEN PS-DATA IS NOT NUMERIC
                OR NOT WS-MES-VALIDO
                OR NOT WS-DIA-VALIDO
                 MOVE 2 TO WS-MOT
              WHEN PS-VALOR IS NOT NUMERIC
                 MOVE 3 TO WS-MOT
              WHEN PS-ISENTA
                 MOVE 6 TO WS-MOT
              WHEN OTHER
                 PERFORM 2110-BUSCAR-TARIFA THRU 2110-FIM-BUSCAR-TARIFA
                 EVALUATE TRUE
                    WHEN WS-SEM-TARIFA
                       MOVE 4 TO WS-MOT
                    WHEN PS-VALOR NOT = WS-VALOR-TARIFA
                       MOVE 5 TO WS-MOT
                    WHEN OTHER
                       MOVE 0 TO WS-MOT
                 END-EVALUATE
           END-EVALUATE
           IF WS-MOT NOT = ZERO
              SET WS-REGISTRO-INVALIDO TO TRUE
           END-IF.
       2100-FIM-VALIDAR-PASSAGEM.
           EXIT.

       2110-BUSCAR-TARIFA.
           MOVE PS-PRACA     TO WS-CB-PRACA
           MOVE PS-CATEGORIA TO WS-CB-CATEGORIA
           SEARCH ALL WS-TF-ITEM
              AT END
                 SET WS-SEM-TARIFA  TO TRUE
              WHEN WS-TF-CHAVE(IX-TF) = WS-CHAVE-BUSCA
                 SET WS-ACHOU-TARIFA TO TRUE
                 MOVE WS-TF-VALOR(IX-TF) TO WS-VALOR-TARIFA
                 MOVE WS-TF-NOME(IX-TF)  TO WS-NOME-PRACA
           END-SEARCH.
       2110-FIM-BUSCAR-TARIFA.
           EXIT.

       2200-GRAVAR-APROVADA.
      *    LIMPA O REGISTRO, INCLUSIVE O FILLER, E MONTA CAMPO A CAMPO
           MOVE SPACES             TO REG-APROVADA
           MOVE PS-TAG             TO PA-TAG
           MOVE PS-PRACA           TO PA-PRACA
           MOVE PS-DATA            TO PA-DATA
           MOVE PS-HORA            TO PA-HORA
           MOVE PS-CATEGORIA       TO PA-CATEGORIA
           MOVE PS-VALOR           TO PA-VALOR
           SET PA-PAGA             TO TRUE
           MOVE WS-NOME-PRACA      TO PA-NOME-PRACA
           MOVE PS-SENTIDO         TO PA-SENTIDO
           WRITE REG-APROVADA
           IF NOT WS-FS-APR-OK
              DISPLAY 'PEDVALID - ERRO NO WRITE DE APROVOUT. FS='
                      WS-FS-APROVADA
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR-COM-ERRO
           END-IF
           ADD 1        TO WS-QTD-APROVADAS
           ADD PS-VALOR TO WS-VALOR-APROVADO.
       2200-FIM-GRAVAR-APROVADA.
           EXIT.

       2300-GRAVAR-REJEITADA.
           MOVE SPACES                TO REG-REJEITADA
           MOVE REG-PASSAGEM          TO RJ-REGISTRO-ORIGINAL
           MOVE WS-MOT                TO RJ-COD-MOTIVO
           MOVE WS-DESC-MOTIVO(WS-MOT) TO RJ-DESC-MOTIVO
           WRITE REG-REJEITADA
           IF NOT WS-FS-REJ-OK
              DISPLAY 'PEDVALID - ERRO NO WRITE DE REJEIOUT. FS='
                      WS-FS-REJEITADA
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR-COM-ERRO
           END-IF
           ADD 1 TO WS-QTD-REJEITADAS
           ADD 1 TO WS-QTD-POR-MOTIVO(WS-MOT).
       2300-FIM-GRAVAR-REJEITADA.
           EXIT.

       3000-FINALIZAR.
           CLOSE PASSAGEM
           IF NOT WS-FS-PAS-OK
              DISPLAY 'PEDVALID - ERRO NO CLOSE DE PASSIN. FS='
                      WS-FS-PASSAGEM
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR-COM-ERRO
           END-IF
           CLOSE APROVADA
           IF NOT WS-FS-APR-OK
              DISPLAY 'PEDVALID - ERRO NO CLOSE DE APROVOUT. FS='
                      WS-FS-APROVADA
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR-COM-ERRO
           END-IF
           CLOSE REJEITADA
           IF NOT WS-FS-REJ-OK
              DISPLAY 'PEDVALID - ERRO NO CLOSE DE REJEIOUT. FS='
                      WS-FS-REJEITADA
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR-COM-ERRO
           END-IF
           DISPLAY 'PEDVALID - TARIFAS CARREGADAS : ' WS-QTD-TARIFA
           DISPLAY 'PEDVALID - PASSAGENS LIDAS ...: ' WS-QTD-LIDAS
           DISPLAY 'PEDVALID - GRAVADAS APROVADAS : ' WS-QTD-APROVADAS
           DISPLAY 'PEDVALID -   DAS QUAIS ISENTAS: ' WS-QTD-ISENTAS
           DISPLAY 'PEDVALID - GRAVADAS REJEITADAS: ' WS-QTD-REJEITADAS
           PERFORM VARYING WS-MOT FROM 1 BY 1 UNTIL WS-MOT > 6
              DISPLAY 'PEDVALID -   MOTIVO ' WS-MOT ' '
                      WS-DESC-MOTIVO(WS-MOT) ': '
                      WS-QTD-POR-MOTIVO(WS-MOT)
           END-PERFORM
           MOVE WS-VALOR-APROVADO TO WS-VALOR-EDITADO
           DISPLAY 'PEDVALID - VALOR APROVADO ....: ' WS-VALOR-EDITADO
      *    CONFERENCIA: LIDAS = APROVADAS + REJEITADAS
           IF WS-QTD-LIDAS NOT = WS-QTD-APROVADAS + WS-QTD-REJEITADAS
              DISPLAY 'PEDVALID - CONTADORES NAO FECHAM'
              MOVE 16 TO RETURN-CODE
           ELSE
              IF WS-QTD-REJEITADAS > ZERO
                 MOVE 4 TO RETURN-CODE
              ELSE
                 MOVE 0 TO RETURN-CODE
              END-IF
           END-IF.
       3000-FIM-FINALIZAR.
           EXIT.

       8000-LER-PASSAGEM.
           READ PASSAGEM
              AT END
                 CONTINUE
              NOT AT END
                 ADD 1 TO WS-QTD-LIDAS
           END-READ
           IF NOT WS-FS-PAS-OK AND NOT WS-FS-PAS-FIM
              DISPLAY 'PEDVALID - ERRO NO READ DE PASSIN. FS='
                      WS-FS-PASSAGEM
              PERFORM 9900-ENCERRAR-COM-ERRO
                 THRU 9900-FIM-ENCERRAR-COM-ERRO
           END-IF.
       8000-FIM-LER-PASSAGEM.
           EXIT.

       9900-ENCERRAR-COM-ERRO.
           MOVE 16 TO RETURN-CODE
           STOP RUN.
       9900-FIM-ENCERRAR-COM-ERRO.
           EXIT.
