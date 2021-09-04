       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CLIENTES.
      *********************************
      * OBJETIVO:  SISTEMA DE GESTAO DE CLIENTES
      * AUTHOR  :  RAFAEL
      *********************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENTES ASSIGN      TO
                           'C:\Users\rafap\cobol\CLIENTES.DAT'
                  ORGANIZATION IS INDEXED
                  ACCESS MODE IS DYNAMIC
                  FILE STATUS IS CLIENTES-STATUS
                  RECORD KEY IS  CLIENTES-CHAVE.

           SELECT RELATORIO ASSIGN     TO
                           'C:\Users\rafap\cobol\RELATORIO.TXT'
             ORGANIZATION IS LINE SEQUENTIAL
             FILE STATUS IS RELATORIO-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD CLIENTES.
       01 CLIENTES-REG.
            05 CLIENTES-CHAVE.
                10 CLIENTES-FONE       PIC 9(09).
            05 CLIENTES-NOME           PIC X(30).
            05 CLIENTES-EMAIL          PIC X(40).

       FD RELATORIO.
       01 RELATORIO-REG.
            05 REL-CLIENTES-FONE       PIC 9(09).
            05 FILLER                  PIC X(01) VALUE ';'.
            05 REL-CLIENTES-NOME       PIC X(30).
            05 FILLER                  PIC X(01) VALUE ';'.
            05 REL-CLIENTES-EMAIL      PIC X(40).


       WORKING-STORAGE SECTION.
       01 WRK-CLIENTES.
           05 WRK-CLIENTES-FONE        PIC 9(09).
           05 FILLER                   PIC X(01) VALUE ';'.
           05 WRK-CLIENTES-NOME        PIC X(30).
           05 FILLER                   PIC X(01) VALUE ';'.
           05 WRK-CLIENTES-EMAIL       PIC X(40).
       77 WRK-OPCAO                    PIC X(1).
       77 WRK-OPCAO-INVALIDA           PIC X(23).
       77 WRK-MODULO                   PIC X(30).
       77 WRK-TECLA                    PIC X(1).
       77 CLIENTES-STATUS              PIC 9(02).
       77 RELATORIO-STATUS             PIC 9(02).
          88 STATUS-OK                 VALUE 0.
       77 WRK-MSG                      PIC X(31).
       77 WRK-MSG-CONFIRMA             PIC X(25).

       SCREEN SECTION.
       01 TELA.
            05 LIMPA-TELA.
                10 BLANK SCREEN.
                10 LINE 01 COLUMN 01   PIC X(20) ERASE EOL
                   BACKGROUND-COLOR 3.
                10 LINE 01 COLUMN 25   PIC X(20)
                   BACKGROUND-COLOR 3  FOREGROUND-COLOR 0
                                       FROM 'SISTEMA DE CLIENTES '.
                10 LINE 02 COLUMN 01   PIC X(30) ERASE EOL
                   BACKGROUND-COLOR 1  FROM WRK-MODULO.
       01 MENU.
            05 LINE 07 COLUMN 15 VALUE '1 - INCLUIR'.
            05 LINE 08 COLUMN 15 VALUE '2 - CONSULTAR'.
            05 LINE 09 COLUMN 15 VALUE '3 - ALTERAR'.
            05 LINE 10 COLUMN 15 VALUE '4 - EXCLUIR'.
            05 LINE 11 COLUMN 15 VALUE '5 - RELATORIO EM TELA'.
            05 LINE 12 COLUMN 15 VALUE '6 - RELATORIO EM DISCO'.
            05 LINE 13 COLUMN 15 VALUE 'X - SAIDA'.
            05 LINE 14 COLUMN 15 VALUE 'OPCAO......: ' .
            05 LINE 14 COLUMN 28 USING WRK-OPCAO.
            05 COLUMN PLUS 02          PIC X(23)
                              FOREGROUND-COLOR 4
                              FROM WRK-OPCAO-INVALIDA.

       01 TELA-REGISTRO.
            05 CAMPO-CHAVE FOREGROUND-COLOR 2.
               10 LINE 10 COLUMN 10 VALUE 'TELEFONE '.
               10 COLUMN PLUS 2        PIC 9(09) USING CLIENTES-FONE
                   BLANK WHEN ZEROS.
            05 OUTROS-CAMPOS.
               10 LINE 11 COLUMN 10 VALUE 'NOME.... '.
               10 COLUMN PLUS 2        PIC X(30) USING CLIENTES-NOME.
               10 LINE 12 COLUMN 10 VALUE 'EMAIL... '.
               10 COLUMN PLUS 2        PIC X(40) USING CLIENTES-EMAIL.
       01 MENSAGEM.
           05 MENSAGEM-ERRO.
               10 LINE 16 COLUMN 01 ERASE EOL.
               10 LINE 16 COLUMN 10    PIC X(31)
                             FOREGROUND-COLOR 4
                             FROM WRK-MSG.
               10 COLUMN PLUS 01       PIC X(25)
                             FROM WRK-MSG-CONFIRMA.
               10 COLUMN PLUS 2        PIC X(01)
                             USING WRK-TECLA.
           05 MENSAGEM-SUCESSO.
               10 LINE 16 COLUMN 01 ERASE EOL.
               10 LINE 16 COLUMN 10    PIC X(31)
                             FOREGROUND-COLOR 2
                             FROM WRK-MSG.
               10 COLUMN PLUS 01       PIC X(25)
                             FROM WRK-MSG-CONFIRMA.
               10 COLUMN PLUS 2        PIC X(01)
                             USING WRK-TECLA.

       PROCEDURE DIVISION.
       0001-PRINCIPAL SECTION.
            PERFORM 1000-INICIAR THRU 1100-MONTATELA.
            PERFORM 2000-PROCESSAR UNTIL WRK-OPCAO = 'X'.
            PERFORM 3000-FINALIZAR.
            STOP RUN.

       1000-INICIAR.
            OPEN I-O CLIENTES
            IF CLIENTES-STATUS = 35 THEN
               OPEN OUTPUT CLIENTES
               CLOSE CLIENTES
               OPEN I-O CLIENTES
            END-IF.

       1100-MONTATELA.
            DISPLAY TELA.
            ACCEPT MENU.

       2000-PROCESSAR.
            MOVE SPACES TO CLIENTES-REG WRK-OPCAO-INVALIDA.
            EVALUATE WRK-OPCAO
                     WHEN 1
                       PERFORM 2100-INCLUIR
                     WHEN 2
                       PERFORM 2200-PESQUISAR
                     WHEN 3
                       PERFORM 2300-ALTERAR
                     WHEN 4
                       PERFORM 2400-EXCLUIR
                     WHEN 5
                       PERFORM 2500-RELATORIOTELA
                     WHEN 6
                       PERFORM 2600-RELATORIODISCO
                     WHEN OTHER
                       IF WRK-OPCAO NOT EQUAL 'X'
                          MOVE 'ENTRE COM OPCAO CORRETA'
                                       TO WRK-OPCAO-INVALIDA
                       END-IF
            END-EVALUATE.
            MOVE SPACES                TO WRK-MODULO.
            PERFORM 1100-MONTATELA.

       3000-FINALIZAR.
            CLOSE CLIENTES
                  RELATORIO.

       2100-INCLUIR.
            MOVE SPACES               TO MENSAGEM.
            MOVE 'MODULO - INCLUSAO ' TO WRK-MODULO.
            MOVE 'VOLTAR AO MENU? (S/N)'
                                      TO WRK-MSG-CONFIRMA.
            DISPLAY TELA.
            ACCEPT TELA-REGISTRO.
            WRITE CLIENTES-REG
                  INVALID KEY
                      MOVE 'REGISTRO JA EXISTE!'
                                      TO WRK-MSG
                      ACCEPT MENSAGEM-ERRO
                  NOT INVALID KEY
                      MOVE 'REGISTRO INCLUIDO COM SUCESSO!'
                                      TO WRK-MSG
                      ACCEPT MENSAGEM-SUCESSO
                      MOVE SPACES     TO CLIENTES-REG
           END-WRITE.
           IF WRK-TECLA EQUAL 'N'
              PERFORM 2100-INCLUIR
           END-IF.
       2100-END-PERFORM.

       2200-PESQUISAR.
            MOVE SPACES               TO MENSAGEM.
            MOVE 'MODULO - CONSULTAR' TO WRK-MODULO.
            DISPLAY TELA.
            DISPLAY TELA-REGISTRO.
            ACCEPT CAMPO-CHAVE.
            MOVE 'VOLTAR AO MENU? (S/N)'
                                      TO WRK-MSG-CONFIRMA
            READ CLIENTES
                INVALID KEY
                    MOVE 'REGISTRO NAO ENCONTRADO!'
                                      TO WRK-MSG
                    ACCEPT MENSAGEM-ERRO
                NOT INVALID KEY
                    MOVE 'REGISTRO ENCONTRADO!'
                                      TO WRK-MSG
                    DISPLAY OUTROS-CAMPOS
                    ACCEPT MENSAGEM-SUCESSO
           END-READ.
           IF WRK-TECLA EQUAL 'N'
              MOVE SPACES              TO CLIENTES-REG
              PERFORM 2200-PESQUISAR
           END-IF.
       2200-END-PERFORM.

       2300-ALTERAR.
            MOVE SPACES                TO MENSAGEM.
            MOVE 'MODULO - ALTERAR'    TO WRK-MODULO.
            DISPLAY TELA.
            DISPLAY TELA-REGISTRO.
            ACCEPT CAMPO-CHAVE.
            READ CLIENTES.
            IF CLIENTES-STATUS EQUAL 0
               DISPLAY OUTROS-CAMPOS
               MOVE 'REGISTRO ENCONTRADO!'
                                       TO WRK-MSG
               MOVE 'DESEJA ALTERAR? (S/N)'
                                       TO WRK-MSG-CONFIRMA
               ACCEPT MENSAGEM-SUCESSO
               IF WRK-TECLA EQUAL 'S'
                  ACCEPT OUTROS-CAMPOS
                  REWRITE CLIENTES-REG
                  MOVE SPACES          TO WRK-MSG-CONFIRMA WRK-TECLA
                  MOVE 'REGISTRO ALTERADO'
                                       TO WRK-MSG
                  MOVE 'PRESSIONE ENTER PARA SAIR'
                                       TO WRK-MSG-CONFIRMA
                  MOVE SPACES          TO CLIENTES-REG
                  ACCEPT MENSAGEM-SUCESSO
                END-IF
           ELSE
                IF CLIENTES-STATUS EQUAL 23
                   MOVE 'REGISTRO NAO ENCONTRADO!'
                                       TO WRK-MSG
                   MOVE 'VOLTAR AO MENU? (S/N)'
                                       TO WRK-MSG-CONFIRMA
                   ACCEPT MENSAGEM-ERRO
                 IF WRK-TECLA EQUAL 'N'
                    PERFORM 2300-ALTERAR
                 END-IF
              END-IF
           END-IF.
       2300-END-PERFORM.

       2400-EXCLUIR.
            MOVE SPACES                TO WRK-MSG-CONFIRMA.
            MOVE 'MODULO - EXCLUIR'    TO WRK-MODULO.
            DISPLAY TELA.
            DISPLAY TELA-REGISTRO.
            ACCEPT CAMPO-CHAVE.
            READ CLIENTES
                 INVALID KEY
                    MOVE 'REGISTRO NAO ENCONTRADO!'
                                       TO WRK-MSG
                    MOVE 'VOLTAR AO MENU? (S/N)'
                                       TO WRK-MSG-CONFIRMA
                    ACCEPT MENSAGEM-ERRO
                    IF WRK-TECLA EQUAL 'N'
                       PERFORM 2400-EXCLUIR
                    END-IF
                 NOT INVALID KEY
                     MOVE 'REGISTRO ENCONTRADO!'
                                       TO WRK-MSG
                     MOVE 'DESEJA EXCLUIR? (S/N)'
                                       TO WRK-MSG-CONFIRMA
                     DISPLAY OUTROS-CAMPOS
                     ACCEPT MENSAGEM-SUCESSO
                     IF WRK-TECLA EQUAL 'S'
                        DELETE CLIENTES
                        MOVE 'REGISTRO EXCLUIDO'
                                       TO WRK-MSG
                        MOVE SPACES    TO CLIENTES-REG WRK-TECLA
                        MOVE 'PRESSIONE ENTER PARA SAIR'
                                       TO WRK-MSG-CONFIRMA
                        ACCEPT MENSAGEM-SUCESSO
                     END-IF
           END-READ.
       2400-END-PERFORM.

       2500-RELATORIOTELA.
            MOVE SPACES                TO WRK-MSG-CONFIRMA
                                          MENSAGEM WRK-TECLA.
            MOVE 'MODULO - RELATORIO EM TELA'
                                       TO WRK-MODULO.
            DISPLAY TELA.
            ACCEPT CAMPO-CHAVE.
            START CLIENTES KEY EQUAL CLIENTES-FONE.
            READ CLIENTES
                INVALID KEY
                    MOVE 'REGISTRO NAO ENCONTRADO'
                                       TO WRK-MSG
                    MOVE 'VOLTAR AO MENU? (S/N)'
                                       TO WRK-MSG-CONFIRMA
                    ACCEPT MENSAGEM-ERRO
                    IF WRK-TECLA EQUAL 'N'
                       PERFORM 2500-RELATORIOTELA
                    END-IF
                NOT INVALID KEY
                DISPLAY TELA
                DISPLAY ' '
                DISPLAY '*-----------------------------------------*'
                DISPLAY '*       RELATORIO DE CLIENTES             *'
                DISPLAY '*-----------------------------------------*'
                PERFORM UNTIL CLIENTES-STATUS EQUAL 10
                        DISPLAY CLIENTES-FONE ' '
                                CLIENTES-NOME ' '
                                CLIENTES-EMAIL
                        READ CLIENTES NEXT
                END-PERFORM
                MOVE 'PRESSIONE ENTER PARA SAIR'
                                       TO WRK-MSG
                ACCEPT MENSAGEM-SUCESSO
           END-READ.
       2500-END-PERFORM.

       2600-RELATORIODISCO.
            MOVE SPACES                TO WRK-MSG-CONFIRMA
                                          MENSAGEM WRK-TECLA.
            MOVE 'MODULO - RELATORIO EM DISCO'
                                       TO WRK-MODULO.
            DISPLAY TELA.
            ACCEPT CAMPO-CHAVE.
            START CLIENTES KEY EQUAL CLIENTES-FONE.
            MOVE 'GERANDO ARQUIVO RELATORIO'
                                       TO WRK-MSG
            READ CLIENTES
                INVALID KEY
                   MOVE 'REGISTRO NAO ENCONTRADO'
                                       TO WRK-MSG
                   MOVE 'VOLTAR AO MENU? (S/N)'
                                       TO WRK-MSG-CONFIRMA
                   ACCEPT MENSAGEM-ERRO
                   IF WRK-TECLA EQUAL 'N'
                      PERFORM 2600-RELATORIODISCO
                   END-IF
                NOT INVALID KEY
                   OPEN OUTPUT RELATORIO
                   IF NOT STATUS-OK
                      MOVE 'OCORREU UM ERRO AO ABRIR'
                                       TO WRK-MSG
                      MOVE 'PRESSIONE QUALQUER TECLA'
                                       TO WRK-MSG-CONFIRMA
                      ACCEPT MENSAGEM-ERRO
                      STOP RUN
                   END-IF
                   PERFORM UNTIL CLIENTES-STATUS EQUAL 10
                           MOVE SPACES TO WRK-CLIENTES-NOME
                                          WRK-CLIENTES-EMAIL
                           MOVE ZEROS  TO WRK-CLIENTES-FONE
                           MOVE CLIENTES-FONE
                                       TO WRK-CLIENTES-FONE
                           MOVE CLIENTES-NOME
                                       TO WRK-CLIENTES-NOME
                           MOVE CLIENTES-EMAIL
                                       TO WRK-CLIENTES-EMAIL
                           WRITE RELATORIO-REG
                                       FROM WRK-CLIENTES
                           READ CLIENTES NEXT
                   END-PERFORM
           END-READ.
           MOVE SPACES                 TO MENSAGEM.
           MOVE 'ARQUIVO GERADO COM SUCESSO'
                                       TO WRK-MSG.
           MOVE 'PRESSIONE QUALQUER TECLA '
                                       TO WRK-MSG-CONFIRMA.
           ACCEPT MENSAGEM-SUCESSO.
       2600-END-PERFORM.
