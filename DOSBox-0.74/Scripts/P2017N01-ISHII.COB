       IDENTIFICATION DIVISION.
       PROGRAM-ID. P2017N01.
      **************************************
      *          CADASTRO DE AMIGO         *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CODAMIGO ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS APELIDO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS CHAVE2 = NOME
                                                      WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CODAMIGO
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "AMIGO.DAT".
       01 REGAMIGO.
                03 APELIDO             PIC X(12).
                03 NOME                PIC X(35).
                03 EMAIL               PIC X(25).
                03 TELEFONE.
                    05 DDD             PIC 9(03).
                    05 NUMERO          PIC 9(09).
                03 TIPOAMIGO           PIC 9(01).
                03 DATANASCIMENTO.
                    05 DIA             PIC 9(02).
                    05 MES             PIC 9(02).
                    05 ANO             PIC 9(04).
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
       77 RESULTADO    PIC 9(02) VALUE ZERO.
       77 RESTO        PIC 9(02) VALUE ZERO.
       77 TEXTOAMIGO   PIC X(10) VALUE SPACES.
       77 MASC1        PIC 9.9999.9999.
       
      *VECTOR
       01 TABAMIGO.
            03  TABAMIGO0 PIC X(12) VALUE "FACULDADE".
            03  TABAMIGO1 PIC X(12) VALUE "IGREJA".
            03  TABAMIGO2 PIC X(12) VALUE "BAIRRO".
            03  TABAMIGO3 PIC X(12) VALUE "CLUBE".
            03  TABAMIGO4 PIC X(12) VALUE "BALADA".
            03  TABAMIGO5 PIC X(12) VALUE "ACADEMIA".
            03  TABAMIGO6 PIC X(12) VALUE "BOTECO".
            03  TABAMIGO7 PIC X(12) VALUE "PRAIA".
            03  TABAMIGO8 PIC X(12) VALUE "VIAGEM".
            03  TABAMIGO9 PIC X(12) VALUE "INIMIGO".
       01 TABELAAMIGO REDEFINES TABAMIGO.
            03  TABELAAMIGOV PIC X(12) OCCURS 10 TIMES. 
            
       01 TABDIAMES.
            03 TABJAN PIC 9(02) VALUE 31.
            03 TABFEV PIC 9(02) VALUE 28.
            03 TABMAR PIC 9(02) VALUE 31.
            03 TABABR PIC 9(02) VALUE 30.
            03 TABMAI PIC 9(02) VALUE 31.
            03 TABJUN PIC 9(02) VALUE 30.
            03 TABJUL PIC 9(02) VALUE 31.
            03 TABAGO PIC 9(02) VALUE 31.
            03 TABSET PIC 9(02) VALUE 30.
            03 TABOUT PIC 9(02) VALUE 31.
            03 TABNOV PIC 9(02) VALUE 30.
            03 TABDEZ PIC 9(02) VALUE 31.
	   01 TBDIAMES REDEFINES TABDIAMES.
            03 TBDIAMESV PIC 9(02) OCCURS 12 TIMES.
      *
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O CODAMIGO
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CODAMIGO
                      CLOSE CODAMIGO
                      MOVE "*** ARQUIVO AMIGO SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO AMIGO" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE ZEROS  TO DDD NUMERO TIPOAMIGO
                MOVE ZEROS  TO DIA MES ANO
                MOVE SPACES TO APELIDO NOME EMAIL.
                DISPLAY  (01, 01) ERASE
                DISPLAY  (01, 20) "CADASTRO DE AMIGO"
                DISPLAY  (04, 01) "APELIDO         : "
                DISPLAY  (05, 01) "NOME            : "
                DISPLAY  (06, 01) "EMAIL           : "
                DISPLAY  (07, 01) "TELEFONE        : "
                DISPLAY  (08, 01) "TIPO AMIGO      : "
                DISPLAY  (09, 01) "DATA NASCIMENTO : ".
       INC-002.
                ACCEPT  (04, 12) APELIDO
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE CODAMIGO
                   GO TO ROT-FIM.
                IF APELIDO  = 0
                   MOVE "*** AMIGO INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
       LER-AMIGO01.
                MOVE 0 TO W-SEL
                READ CODAMIGO
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY (05, 18) NOME
                      DISPLAY (06, 18) EMAIL
                      DISPLAY (07, 18) DDD
                      MOVE NUMERO TO MASC1
                      DISPLAY (07, 22) MASC1
                      DISPLAY (08, 18) TIPOAMIGO
                      DISPLAY (09, 18) DIA
                      DISPLAY (09, 21) MES
                      DISPLAY (09, 24) ANO
                      MOVE "*** AMIGO JA CADASTRAD0 ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO AMIGO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
                ACCEPT (05, 18) NOME
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
       INC-004.
                ACCEPT (06, 18) EMAIL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
       INC-005.
                ACCEPT (07, 18) DDD
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
                IF DDD < 1 OR DDD > 999
                    MOVE "*** DDD AMIGO INVALIDO ***" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-005.
       INC-006.
                ACCEPT (07, 22) NUMERO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
                IF NUMERO < 100000000 OR NUMERO > 999999999
                    MOVE "*** TELEFONE AMIGO INVALIDO ***" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-006.
                MOVE NUMERO TO MASC1
                DISPLAY (07, 22) MASC1.
       INC-007.
                ACCEPT (08, 18) TIPOAMIGO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
                IF TIPOAMIGO < 0 OR TIPOAMIGO > 9
                    MOVE "*** TIPO AMIGO INVALIDO ***" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-007.
       INC-008.
                MOVE TABELAAMIGOV(TIPOAMIGO) TO TEXTOAMIGO.
                DISPLAY (08, 20) TEXTOAMIGO.
       INC-DIA.
                ACCEPT (09, 18) DIA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-007.
                IF DIA < 1 OR DIA > 31
                    MOVE "*** DIA INVALIDO ***" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-DIA.
       INC-MES.
                ACCEPT (09, 21) MES
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-DIA.
                IF MES < 1 OR MES > 12
                    MOVE "*** MES INVALIDO ***" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-MES.
       INC-ANO.
                ACCEPT (09, 18) ANO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-007.
                IF ANO < 1800 OR ANO > 2999
                    MOVE "*** DIA INVALIDO ***" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-ANO.
      *
      ****************************************************
      * VALIDA MES COM A PARTICULARIDADE DO ANO BISSEXTO *
      ****************************************************
      * 
       INC-009.
                DIVIDE ANO BY 4 GIVING RESULTADO REMAINDER RESTO.
                IF MES = 2
                    IF RESTO = 0
                        IF DIA > 29
                            MOVE "*** DIA INVALIDO! ***" TO MENS
                            PERFORM ROT-MENS THRU ROT-MENS-FIM
                            GO TO INC-DIA
                        ELSE
                            GO TO INC-010.
       INC-010.
                IF DIA > TABELAAMIGOV(MES) AND MES NOT = 2
                    MOVE "*** DIA INVALIDO! ***" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-DIA.
                
      *
                IF W-SEL = 1 GO TO ALT-OPC.

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
                WRITE REGAMIGO
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** CLIENTE JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE CLIENTE"
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
                   GO TO INC-003.
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
                DELETE CODAMIGO RECORD
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
                REWRITE REGAMIGO
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO CLIENTE"   TO MENS
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
