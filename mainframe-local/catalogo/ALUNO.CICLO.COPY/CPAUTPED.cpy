      *================================================================*
      * CPAUTPED - PEDIDO DE AUTORIZACAO (LAYOUT SIMPLIFICADO,         *
      *            INSPIRADO NOS ELEMENTOS DE DADOS DA ISO 8583)       *
      * LRECL 80, RECFM FB. NA VIDA REAL O PEDIDO E UMA MENSAGEM       *
      * ONLINE; AQUI ELE CHEGA EM ARQUIVO PARA A SIMULACAO EM BATCH.   *
      *================================================================*
       01  REG-PEDIDO.
      *    MTI: VERSAO 0 (1987), CLASSE, FUNCAO E ORIGEM
           05  PD-MTI                 PIC X(04).
               88  PD-PEDIDO-DUPLA              VALUE '0100'.
               88  PD-PEDIDO-UNICA              VALUE '0200'.
               88  PD-AVISO-STANDIN             VALUE '0120'.
               88  PD-REVERSAO                  VALUE '0400'.
               88  PD-AVISO-REVERSAO            VALUE '0420'.
      *    DE 3 - CODIGO DE PROCESSAMENTO (00 COMPRA, 01 SAQUE)
           05  PD-COD-PROCESSAMENTO.
               10  PD-TIPO-TRANSACAO  PIC X(02).
                   88  PD-COMPRA                VALUE '00'.
                   88  PD-SAQUE                 VALUE '01'.
               10  FILLER             PIC X(04).
      *    DE 2 - NUMERO DO CARTAO (PAN)
           05  PD-NUM-CARTAO          PIC X(16).
      *    DE 4 - VALOR DA TRANSACAO
           05  PD-VALOR               PIC 9(09)V99.
      *    DATA E HORA LOCAIS DA TRANSACAO (DE 13 E DE 12)
           05  PD-DATA.
               10  PD-ANO-MES         PIC 9(06).
               10  PD-DIA             PIC 9(02).
           05  PD-HORA                PIC 9(06).
      *    DE 11 - NUMERO DE RASTREIO (STAN)
           05  PD-STAN                PIC 9(06).
      *    DE 18 - MCC
           05  PD-MCC                 PIC 9(04).
      *    DE 22 - MODO DE ENTRADA (SIMPLIFICADO EM 2 POSICOES)
           05  PD-MODO-ENTRADA        PIC X(02).
               88  PD-DIGITADO                  VALUE '01'.
               88  PD-CHIP                      VALUE '05'.
               88  PD-APROXIMACAO               VALUE '07'.
               88  PD-COMERCIO-ELETRONICO       VALUE '81'.
               88  PD-TARJA                     VALUE '90'.
           05  PD-PAIS                PIC X(03).
               88  PD-NACIONAL                  VALUE 'BRA'.
      *    RESULTADO DA VERIFICACAO DO CODIGO DE SEGURANCA OU DO
      *    CRIPTOGRAMA DO CHIP (M = CONFERE, N = NAO CONFERE). O CODIGO
      *    EM SI NUNCA E GUARDADO (PCI DSS)
           05  PD-VERIF-CODIGO        PIC X(01).
               88  PD-CODIGO-NAO-CONFERE        VALUE 'N'.
      *    RESULTADO DA VERIFICACAO DA SENHA (M, N OU BRANCO = SEM SENHA)
           05  PD-VERIF-SENHA         PIC X(01).
               88  PD-SENHA-NAO-CONFERE         VALUE 'N'.
      *    INDICADOR DE COMERCIO ELETRONICO: 05 = AUTENTICADA (3-D
      *    SECURE), 07 = NAO AUTENTICADA, BRANCO = PRESENCIAL
           05  PD-ECI                 PIC X(02).
           05  PD-RECORRENTE          PIC X(01).
      *    DE 38 - NO 0120, O CODIGO DADO PELA BANDEIRA EM STAND-IN;
      *    NO 0400 E NO 0420, O CODIGO DA AUTORIZACAO ORIGINAL
           05  PD-COD-AUT-ORIGINAL    PIC X(06).
      *    DE 39 - NO 0120, A RESPOSTA DADA PELA BANDEIRA
           05  PD-RESPOSTA-STANDIN    PIC X(02).
           05  FILLER                 PIC X(01).
