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
       01 V-FONE          PIC X(20) VALUE SPACES.
       01 OPCAO           PIC X(01) VALUE SPACES.
      *
       PROCEDURE DIVISION.
       INICIO.
            MOVE SPACES TO V-APELIDO V-NOME V-FONE OPCAO.
       TELA.
            DISPLAY (01, 01) ERASE
            DISPLAY (02, 25) "*** CADASTRO DE AMIGOS ***"
            DISPLAY (10, 10) "APELIDO  : ".
            DISPLAY (12, 10) "NOME     : ".
            DISPLAY (14, 10) "TELEFONE : ".
            DISPLAY (23, 01) "MENSAGEM : ".
       APELIDO.
            ACCEPT  (10, 21) V-APELIDO.
            IF V-APELIDO = SPACES
                    GO TO APELIDO.
       NOME.
            ACCEPT  (12, 21) V-NOME.
            IF V-NOME = SPACES
                    GO TO NOME.
       TELEFONE.
            ACCEPT  (14, 21) V-FONE.
            IF V-FONE = SPACES
                    GO TO TELEFONE.
       DADOSOK.
           DISPLAY  (16, 40) "DADOS OK (S/N) :"
           ACCEPT (16, 57) OPCAO
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