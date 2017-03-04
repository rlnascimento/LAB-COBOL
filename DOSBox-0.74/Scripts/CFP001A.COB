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
       PROCEDURE DIVISION.
       INICIO.
            MOVE ZEROS  TO V-IDADE V-CEP.
            MOVE SPACES TO V-APELIDO V-NOME V-FONE V-ENDERECO V-BAIRRO
                           V-CIDADE V-ESTADO V-EMAIL OPCAO.
       TELA.
            DISPLAY (01, 01) ERASE
            DISPLAY (02, 25) "*** CADASTRO DE AMIGOS ***"
            DISPLAY (06, 10) "APELIDO  : ".
            DISPLAY (08, 10) "NOME     : ".
            DISPLAY (10, 10) "ENDERE€O : ".
            DISPLAY (12, 10) "BAIRRO   : ".
            DISPLAY (14, 10) "CIDADE   : ".
            DISPLAY (14, 50) "ESTADO   : ".
            DISPLAY (16, 10) "FONE     : ".
            DISPLAY (16, 50) "CEP      : ".
            DISPLAY (18, 10) "EMAIL    : ".
            DISPLAY (20, 10) "IDADE    : ".
            DISPLAY (23, 01) "MENSAGEM : ".
       APELIDO.
            ACCEPT  (06, 21) V-APELIDO.
            IF V-APELIDO = SPACES
                    GO TO APELIDO.
       NOME.
            ACCEPT  (08, 21) V-NOME.
            IF V-NOME = SPACES
                    GO TO NOME.
       ENDERECO.
            ACCEPT  (10, 21) V-ENDERECO.
            IF V-ENDERECO = SPACES
                    GO TO ENDERECO.
       BAIRRO.
            ACCEPT  (12, 21) V-BAIRRO.
            IF V-BAIRRO = SPACES
                    GO TO BAIRRO.
       CIDADE.
            ACCEPT  (14, 21) V-CIDADE.
            IF V-CIDADE = SPACES
                    GO TO CIDADE.
       ESTADO.
            ACCEPT  (14, 61) V-ESTADO.
            IF V-ESTADO = SPACES
                    GO TO ESTADO.
       FONE.
            ACCEPT  (16, 21) V-FONE.
            IF V-FONE = SPACES
                    GO TO FONE.
       CEP.
            ACCEPT  (16, 61) V-CEP.
            IF V-CEP = 0
                    GO TO CEP.
       EMAIL.
            ACCEPT  (18, 21) V-EMAIL.
            IF V-EMAIL = SPACES
                    GO TO EMAIL.
       IDADE.
            ACCEPT  (20, 21) V-IDADE.
            IF V-IDADE = 0
                    GO TO IDADE.
       DADOSOK.
           DISPLAY  (22, 40) "DADOS OK (S/N) :"
           ACCEPT (22, 57) OPCAO
           IF OPCAO = "S"
                 DISPLAY (23, 13) "DADOS GRAVADOS"
                 STOP "  "
                 GO TO TELA.
           IF OPCAO = "N"
                 DISPLAY (23, 13) "DADOS RECUSADOS"
                 STOP "  "
                 GO TO TELA.
           DISPLAY (23, 13) "DIGITE APENAS S=SIM   N=NÇO "
           STOP "  "
           GO TO DADOSOK.
