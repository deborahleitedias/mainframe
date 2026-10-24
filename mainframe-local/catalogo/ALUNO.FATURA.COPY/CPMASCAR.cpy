      *================================================================*
      * CPMASCAR - PARAGRAFO DE MASCARAMENTO DO NUMERO DO CARTAO       *
      * REGRA DO MODULO: EXIBIR SOMENTE OS 6 PRIMEIROS E OS 4 ULTIMOS  *
      * DIGITOS. O PROGRAMA PRECISA DECLARAR WS-CARTAO-ABERTO E        *
      * WS-CARTAO-MASC (AMBOS PIC X(16)). USO: MOVE O NUMERO PARA      *
      * WS-CARTAO-ABERTO E PERFORM 9800-MASCARAR-CARTAO.               *
      *================================================================*
       9800-MASCARAR-CARTAO.
           MOVE WS-CARTAO-ABERTO          TO WS-CARTAO-MASC
           MOVE '******'                  TO WS-CARTAO-MASC(7:6)
           .
