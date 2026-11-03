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

       PROCEDURE DIVISION.
       0000-PRINCIPAL.

           PERFORM 1000-INICIALIZAR
           PERFORM 2000-PROCESSAR UNTIL WS-FS-ENT-FIM
           PERFORM 3000-FINALIZAR
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
    
       2000-PROCESSAR.
           
           PERFORM 2100-VALIDAR-TRANSACAO
           IF WS-REGISTRO-VALIDO
              PERFORM 2200-GRAVAR-APROVADA
           ELSE
              PERFORM 2300-GRAVAR-REJEITADA 
           END-IF

           PERFORM 8000-LER-TRANSACAO THRU 8000-FIM-LER-TRANSACAO.
           
       2100-VALIDAR-TRANSACAO.

           MOVE SPACES TO REG-REJEITADA
           SET WS-REGISTRO-VALIDO TO TRUE

           EVALUATE TRUE
              WHEN TR-VALOR IS NOT NUMERIC
                   SET WS-REGISTRO-INVALIDO   TO TRUE 
                   SET RJ-VALOR-INVALIDO      TO TRUE
                   MOVE 'VALOR NAO NUMERICO'
                                            TO RJ-DESC-MOTIVO
              WHEN TR-VALOR = ZERO
                   SET WS-REGISTRO-INVALIDO   TO TRUE 
                   SET RJ-VALOR-INVALIDO      TO TRUE
                   MOVE 'VALOR ZERADO'
                                            TO RJ-DESC-MOTIVO
            END-EVALUATE.

       2200-GRAVAR-APROVADA.
      *    LIMPE O REGISTRO INTERNO (MOVE SPACES E DEPOIS INITIALIZE;
      *    O INITIALIZE SOZINHO NAO LIMPA OS FILLER) E MOVA CADA CAMPO
      *    TR- PARA O CAMPO TI- CORRESPONDENTE (O MOVE DO VALOR CONVERTE
      *    PARA COMP-3). MARQUE TI-SUSPEITA SE A TRANSACAO FOR FORA DO
      *    PAIS E ACIMA DO LIMITE (CONTE AS SUSPEITAS); SENAO,
      *    TI-NORMAL. GRAVE, TESTE O FILE STATUS E CONTE A APROVADA.
           CONTINUE
           .

       2300-GRAVAR-REJEITADA.
      *    COPIE O REGISTRO ORIGINAL PARA RJ-REGISTRO-ORIGINAL, GRAVE,
      *    TESTE O FILE STATUS E CONTE A REJEITADA.
           CONTINUE
           .

       3000-FINALIZAR.
      *    FECHE OS TRES ARQUIVOS E EXIBA OS CONTADORES. CONFIRA SE
      *    LIDOS = APROVADAS + REJEITADAS. CODIGO DE RETORNO: 16 SE NAO
      *    FECHAR; 4 SE HOUVE REJEITADAS; 0 NOS DEMAIS CASOS.
           CONTINUE
           .

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
      *    PRONTO: ENCERRA COM RC 16 PARA IMPEDIR OS STEPS SEGUINTES.
           MOVE 16 TO RETURN-CODE
           STOP RUN
           .
