       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CFP001.
       AUTHOR.        ANGELO LOTIERSO FILHO.
       DATE-WRITTEN.  17/07/2001.
      ***************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.             DECIMAL-POINT IS COMMA.

       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
       01 V-APELIDO       PIC X(12) VALUE SPACES.
       01 V-NOME          PIC X(30) VALUE SPACES.
       01 V-ENDERECO      PIC X(30) VALUE SPACES.
       01 V-BAIRRO        PIC X(20) VALUE SPACES.
       01 V-CIDADE        PIC X(20) VALUE SPACES.
       01 V-ESTADO        PIC X(02) VALUE SPACES.
       01 V-FONE          PIC X(20) VALUE SPACES.
       01 V-CEP           PIC 9(08) VALUE ZEROS.
       01 V-EMAIL         PIC X(30) VALUE SPACES.
       01 V-IDADE         PIC 9(02) VALUE ZEROS.
       01 OPCAO           PIC X(01) VALUE SPACES.
      *
       SCREEN SECTION.
       01  CFT001.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01
               VALUE  "浜様様様様様様様様様様様様様様様様様様様".
           05  LINE 01  COLUMN 41
               VALUE  "様様様様様様様様様様様様様様様様様様様融".
           05  LINE 02  COLUMN 01
               VALUE  "�                        *** CADASTRO DE".
           05  LINE 02  COLUMN 41
               VALUE  " AMIGOS ***                            �".
           05  LINE 03  COLUMN 01
               VALUE  "麺様様様様様様様様様様様様様様様様様様様".
           05  LINE 03  COLUMN 41
               VALUE  "様様様様様様様様様様様様様様様様様様様郵".
           05  LINE 04  COLUMN 01
               VALUE  "�".
           05  LINE 04  COLUMN 41
               VALUE  "                                       �".
           05  LINE 05  COLUMN 01
               VALUE  "�".
           05  LINE 05  COLUMN 41
               VALUE  "                                       �".
           05  LINE 06  COLUMN 01
               VALUE  "� �APELIDO陳陳陳朕 �NOME陳陳陳陳陳陳陳陳".
           05  LINE 06  COLUMN 41
               VALUE  "陳陳陳陳陳陳�   �IDADE朕               �".
           05  LINE 07  COLUMN 01
               VALUE  "� �              � �".
           05  LINE 07  COLUMN 41
               VALUE  "            �   �      �               �".
           05  LINE 08  COLUMN 01
               VALUE  "� 青陳陳陳陳陳陳潰 青陳陳陳陳陳陳陳陳陳�".
           05  LINE 08  COLUMN 41
               VALUE  "陳陳陳陳陳陳�   青陳陳潰               �".
           05  LINE 09  COLUMN 01
               VALUE  "� �ENDERECO陳陳陳陳陳陳陳陳陳陳陳陳� �BA".
           05  LINE 09  COLUMN 41
               VALUE  "IRRO陳陳陳陳陳陳陳陳�                  �".
           05  LINE 10  COLUMN 01
               VALUE  "� �                                � �".
           05  LINE 10  COLUMN 41
               VALUE  "                    �                  �".
           05  LINE 11  COLUMN 01
               VALUE  "� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 青�".
           05  LINE 11  COLUMN 41
               VALUE  "陳陳陳陳陳陳陳陳陳陳�                  �".
           05  LINE 12  COLUMN 01
               VALUE  "� �CIDADE陳陳陳陳陳陳陳陳朕 �UF陳� �TELE".
           05  LINE 12  COLUMN 41
               VALUE  "FONE陳陳陳陳陳陳陳� �CEP陳陳陳陳�      �".
           05  LINE 13  COLUMN 01
               VALUE  "� �                       � �    � �".
           05  LINE 13  COLUMN 41
               VALUE  "                  � �           �      �".
           05  LINE 14  COLUMN 01
               VALUE  "� 青陳陳陳陳陳陳陳陳陳陳陳� 青陳潰 青陳�".
           05  LINE 14  COLUMN 41
               VALUE  "陳陳陳陳陳陳陳陳陳� 青陳陳陳陳陳�      �".
           05  LINE 15  COLUMN 01
               VALUE  "� �EMAIL陳陳陳陳陳陳陳陳陳陳陳陳陳朕".
           05  LINE 15  COLUMN 41
               VALUE  "                                       �".
           05  LINE 16  COLUMN 01
               VALUE  "� �                                �".
           05  LINE 16  COLUMN 41
               VALUE  "                                       �".
           05  LINE 17  COLUMN 01
               VALUE  "� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰".
           05  LINE 17  COLUMN 41
               VALUE  "                                       �".
           05  LINE 18  COLUMN 01
               VALUE  "�".
           05  LINE 18  COLUMN 41
               VALUE  "                                       �".
           05  LINE 19  COLUMN 01
               VALUE  "�".
           05  LINE 19  COLUMN 41
               VALUE  "                                       �".
           05  LINE 20  COLUMN 01
               VALUE  "�".
           05  LINE 20  COLUMN 41
               VALUE  "                                       �".
           05  LINE 21  COLUMN 01
               VALUE  "�".
           05  LINE 21  COLUMN 41
               VALUE  "                                       �".
           05  LINE 22  COLUMN 01
               VALUE  "麺様様様様様様様様様様様様様様様様様様様".
           05  LINE 22  COLUMN 41
               VALUE  "様様様様様様様様様様様様様様様僕様様様郵".
           05  LINE 23  COLUMN 01
               VALUE  "�MENSAGEM :".
           05  LINE 23  COLUMN 41
               VALUE  "                              � CFP001 �".
           05  LINE 24  COLUMN 01
               VALUE  "藩様様様様様様様様様様様様様様様様様様様".
           05  LINE 24  COLUMN 41
               VALUE  "様様様様様様様様様様様様様様様瞥様様様夕".
           05  T-APELIDO
               LINE 07  COLUMN 05  PIC X(12)
               USING  V-APELIDO
               HIGHLIGHT.
           05  T-NOME
               LINE 07  COLUMN 22  PIC X(30)
               USING  V-NOME
               HIGHLIGHT.
           05  T-IDADE
               LINE 07  COLUMN 60  PIC 9(02)
               USING  V-IDADE
               HIGHLIGHT.
           05  T-ENDERECO
               LINE 10  COLUMN 05  PIC X(30)
               USING  V-ENDERECO
               HIGHLIGHT.
           05  T-BAIRRO
               LINE 10  COLUMN 40  PIC X(20)
               USING  V-BAIRRO
               HIGHLIGHT.
           05  T-CIDADE
               LINE 13  COLUMN 05  PIC X(20)
               USING  V-CIDADE
               HIGHLIGHT.
           05  T-ESTADO
               LINE 13  COLUMN 31  PIC X(02)
               USING  V-ESTADO
               HIGHLIGHT.
           05  T-FONE
               LINE 13  COLUMN 38  PIC X(20)
               USING  V-FONE
               HIGHLIGHT.
           05  T-CEP
               LINE 13  COLUMN 63  PIC 99999.999
               USING  V-CEP
               HIGHLIGHT.
           05  T-EMAIL
               LINE 16  COLUMN 05  PIC X(30)
               USING  V-EMAIL
               HIGHLIGHT.
      *
       PROCEDURE DIVISION.
       INICIO.
            MOVE ZEROS  TO V-IDADE V-CEP.
            MOVE SPACES TO V-APELIDO V-NOME V-FONE V-ENDERECO V-BAIRRO
                           V-CIDADE V-ESTADO V-EMAIL OPCAO.
       TELA.
            DISPLAY CFT001.
       APELIDO.
            ACCEPT  T-APELIDO.
            IF V-APELIDO = SPACES
                    GO TO APELIDO.
       NOME.
            ACCEPT  T-NOME.
            IF V-NOME = SPACES
                    GO TO NOME.
       IDADE.
            ACCEPT  T-IDADE.
            IF V-IDADE = 0
                    GO TO IDADE.
       ENDERECO.
            ACCEPT  T-ENDERECO.
            IF V-ENDERECO = SPACES
                    GO TO ENDERECO.
       BAIRRO.
            ACCEPT  T-BAIRRO.
            IF V-BAIRRO = SPACES
                    GO TO BAIRRO.
       CIDADE.
            ACCEPT  T-CIDADE.
            IF V-CIDADE = SPACES
                    GO TO CIDADE.
       ESTADO.
            ACCEPT  T-ESTADO.
            IF V-ESTADO = SPACES
                    GO TO ESTADO.
       FONE.
            ACCEPT  T-FONE.
            IF V-FONE = SPACES
                    GO TO FONE.
       CEP.
            ACCEPT  T-CEP.
            IF V-CEP = 0
                    GO TO CEP.
       EMAIL.
            ACCEPT  T-EMAIL.
            IF V-EMAIL = SPACES
                    GO TO EMAIL.
       DADOSOK.
           DISPLAY  (20, 40) "DADOS OK (S/N) :"
           ACCEPT (20, 57) OPCAO
           IF OPCAO = "S"
                 DISPLAY (23, 13) "DADOS GRAVADOS"
                 STOP "  "
                 GO TO TELA.
           IF OPCAO = "N"
                 DISPLAY (23, 13) "DADOS RECUSADOS"
                 STOP "  "
                 GO TO TELA.
           DISPLAY (23, 13) "DIGITE APENAS S=SIM   N=N�O "
           STOP "  "
           GO TO DADOSOK.
