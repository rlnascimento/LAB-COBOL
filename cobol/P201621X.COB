       IDENTIFICATION DIVISION.
       PROGRAM-ID. P201621A.
      **************************************
      * MANUTENCAO DO CADASTRO DE ALUNO  *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ALUNO ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODALU
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS CHAVE2 = NOME
                                                      WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD ALUNO
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ALUNO.DAT".
       01 REGALU.
                03 CODALU          PIC 9(05).
                03 NOME            PIC X(30).
                03 DISCIPLINA      PIC X(20).
                03 NOTAS.
				    05 NOTA1	   PIC 99V9.
				    05 NOTA2	   PIC 99V9.
                03	FALTAS.
				    05 FALTA1      PIC 9(2).
					05 FALTA2      PIC 9(2).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.
       77 ST-ERRO      PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.
       77 MEDIA        PIC 99V9 VALUE ZEROS.
       77 TFALTAS      PIC 9(3) VALUE ZEROS.
       01 MASC1        PIC Z9,9.
       01 MASC2        PIC ZZ9.
       SCREEN SECTION.
        01  TELALUNO.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  "ษอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 01  COLUMN 41 
               VALUE  "อออออออออออออออออออออออออออออออออออออออป".
           05  LINE 02  COLUMN 01 
               VALUE  "บ                           CADASTRO DE".
           05  LINE 02  COLUMN 41 
               VALUE  "ALUNOS                                 บ".
           05  LINE 03  COLUMN 01 
               VALUE  "บฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ".
           05  LINE 03  COLUMN 41 
               VALUE  "ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ".
           05  LINE 04  COLUMN 01 
               VALUE  "บ   CODIGO DO ALUNO  :".
           05  LINE 04  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 05  COLUMN 01 
               VALUE  "บ   NOME             :".
           05  LINE 05  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 06  COLUMN 01 
               VALUE  "บ   DISCIPLINA       :".
           05  LINE 06  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 07  COLUMN 01 
               VALUE  "บ   NOTA 1           :".
           05  LINE 07  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 08  COLUMN 01 
               VALUE  "บ   NOTA 2           :".
           05  LINE 08  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 09  COLUMN 01 
               VALUE  "บ   MEDIA            :".
           05  LINE 09  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 10  COLUMN 01 
               VALUE  "บ   FALTA 1          :".
           05  LINE 10  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 11  COLUMN 01 
               VALUE  "บ   FALTA 2          :".
           05  LINE 11  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 12  COLUMN 01 
               VALUE  "บ   TOTAL FALTA      :".
           05  LINE 12  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 13  COLUMN 01 
               VALUE  "บ".
           05  LINE 13  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 14  COLUMN 01 
               VALUE  "บ".
           05  LINE 14  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 15  COLUMN 01 
               VALUE  "บ".
           05  LINE 15  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 16  COLUMN 01 
               VALUE  "บ".
           05  LINE 16  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 17  COLUMN 01 
               VALUE  "บ".
           05  LINE 17  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 18  COLUMN 01 
               VALUE  "บ".
           05  LINE 18  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 19  COLUMN 01 
               VALUE  "บ".
           05  LINE 19  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 20  COLUMN 01 
               VALUE  "บ".
           05  LINE 20  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 21  COLUMN 01 
               VALUE  "บ".
           05  LINE 21  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 22  COLUMN 01 
               VALUE  "บฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ".
           05  LINE 22  COLUMN 41 
               VALUE  "ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤบ".
           05  LINE 23  COLUMN 01 
               VALUE  "บ MENSAGEM".
           05  LINE 23  COLUMN 41 
               VALUE  "                                       บ".
           05  LINE 24  COLUMN 01 
               VALUE  "ศอออออออออออออออออออออออออออออออออออออออ".
           05  LINE 24  COLUMN 41 
               VALUE  "อออออออออออออออออออออออออออออออออออออออผ".
           05  TCODIGO
               LINE 04  COLUMN 24  PIC 99999
               USING  CODALU
               HIGHLIGHT.
           05  TNOME
               LINE 05  COLUMN 24  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  TDISCIPLINA
               LINE 06  COLUMN 24  PIC X(20)
               USING  DISCIPLINA
               HIGHLIGHT.
           05  TNOTA1
               LINE 07  COLUMN 24  PIC Z9,9
               USING  NOTA1
			   HIGHLIGHT.
           05  TNOTA2
               LINE 08  COLUMN 24  PIC Z9,9
               USING  NOTA2
               HIGHLIGHT.
           05  TMEDIA
               LINE 09  COLUMN 24  PIC Z9,9
               USING  MEDIA
               HIGHLIGHT.
           05  TFALTA1
               LINE 10  COLUMN 24  PIC Z9
               USING  FALTA1
               HIGHLIGHT.
           05  TFALTA2
               LINE 11  COLUMN 24  PIC Z9
               USING  FALTA2
               HIGHLIGHT.
           05  TTFALTAS
               LINE 12  COLUMN 24  PIC ZZ9
               USING  TFALTAS
               HIGHLIGHT.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O ALUNO
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT ALUNO
                      CLOSE ALUNO
                      MOVE "*** ARQUIVO ALUNO SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO ALUNO" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE ZEROS  TO CODALU NOTA1 NOTA2 FALTA1 FALTA2
				MOVE ZERO   TO TFALTAS MEDIA
                MOVE SPACES TO NOME DISCIPLINA.
                DISPLAY TELALUNO.
       INC-002.
                ACCEPT  TCODIGO
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE ALUNO
                   GO TO ROT-FIM.
                IF CODALU  = 0
                   MOVE "*** ALUNO INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
       LER-ALUNO01.
                MOVE 0 TO W-SEL
                READ ALUNO
				PERFORM INC-008
				PERFORM INC-011
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY TELALUNO
                      MOVE "*** ALUNO JA CADASTRAD0 ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO ALUNO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-004.
                ACCEPT TNOME
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
       INC-005.
                ACCEPT TDISCIPLINA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
       INC-006.
                ACCEPT TNOTA1
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
                IF NOTA1 <0 OR > 10
                   MOVE  "NOTA INVALIDA" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-006.
       INC-007.
                ACCEPT TNOTA2
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
				IF NOTA2 <0 OR > 10
                   MOVE  "NOTA INVALIDA" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-006.
       INC-008.
                COMPUTE MEDIA = NOTA1 + NOTA2
                COMPUTE MEDIA = MEDIA / 2
                DISPLAY TMEDIA.
       INC-009.
                ACCEPT TFALTA1
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT= 02 GO TO INC-007.
       INC-010.
                ACCEPT TFALTA2
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT= 02 GO TO INC-009.
       INC-011.
                COMPUTE TFALTAS = FALTA1 + FALTA2
                DISPLAY TTFALTAS.
      *
                IF W-SEL = 1
                              GO TO ALT-OPC.

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGALU
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** ALUNO JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE ALUNO"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 12)
                     "F1=NOVO REGISTRO   F2=ALTERAR   F3=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT NOT = 02 AND W-ACT NOT = 03 AND W-ACT NOT = 04
                   GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-ACT = 02
                   MOVE 02 TO W-SEL
                   GO TO INC-001.
                IF W-ACT = 03
                   GO TO INC-004.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE ALUNO RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***           " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGALU
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO ALUNO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
                DISPLAY (01, 01) ERASE
                EXIT PROGRAM.
       ROT-FIMP.
                EXIT PROGRAM.

       ROT-FIMS.
                STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.
    