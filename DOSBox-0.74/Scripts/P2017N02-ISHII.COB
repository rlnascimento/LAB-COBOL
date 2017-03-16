       IDENTIFICATION DIVISION.
       PROGRAM-ID. P2017N02.
      **************************************
      *        CADASTRO DE VEICULOS        *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADVEIC ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS PLACA
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS CHAVE2 = PROPRIETARIO
                                                      WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADVEIC
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "VEICULO.DAT".
       01 REGVEIC.
                03 PLACA.
                    05 LETRA           PIC X(03).
                    05 NUM             PIC 9(04).
                03 PROPRIETARIO        PIC X(35).
                03 MARCA               PIC 9(01).
                03 DATACOMPRA.
                    05 DIA             PIC 9(02).
                    05 MES             PIC 9(02).
                    05 ANO             PIC 9(04).
                03 VALORVEIC           PIC 9(06)v99.
                03 IPVA                PIC 99V99.
                03 CALCIPVA            PIC 9(06).
                03 SITUACAO            PIC X(01).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL          PIC 9(01)      VALUE ZEROS.
       77 W-CONT         PIC 9(06)      VALUE ZEROS.
       77 W-OPCAO        PIC X(01)      VALUE SPACES.
       77 ST-ERRO        PIC X(02)      VALUE "00".
       77 W-ACT          PIC 9(02)      VALUE ZEROS.
       77 MENS           PIC X(50)      VALUE SPACES.
       77 LIMPA          PIC X(50)      VALUE SPACES.
       77 RESULTADO      PIC 9(02)      VALUE ZERO.
       77 RESTO          PIC 9(02)      VALUE ZERO.
       77 TEXTOMARCA     PIC X(10)      VALUE SPACES.
       77 TEXTOSITUACAO  PIC X(10)      VALUE SPACES.
       77 CONTA          PIC 9(06)V99   VALUE ZEROS.
       77 MASC1          PIC 999999.99.
       77 MASC2          PIC 99.99.
       77 MASC3          PIC 999999.
       
      *VECTOR
       01 TABMARCA.
            03  TABMARCA0 PIC X(12) VALUE "FORD".
            03  TABMARCA1 PIC X(12) VALUE "FIAT".
            03  TABMARCA2 PIC X(12) VALUE "VOLKSWAGEN".
            03  TABMARCA3 PIC X(12) VALUE "CHEVROLET".
            03  TABMARCA4 PIC X(12) VALUE "MERCEDES".
            03  TABMARCA5 PIC X(12) VALUE "TOYOTA".
            03  TABMARCA6 PIC X(12) VALUE "HONDA".
            03  TABMARCA7 PIC X(12) VALUE "HYUUNDAI".
            03  TABMARCA8 PIC X(12) VALUE "RENALT".
            03  TABMARCA9 PIC X(12) VALUE "CITROEN".
       01 TABELAMARCA REDEFINES TABMARCA.
            03  VETORMARCA PIC X(12) OCCURS 10 TIMES.
            
       01 TABSITUACAO.
            03  TABSITUACAO0 PIC X(06) VALUE "NOVO".
            03  TABSITUACAO1 PIC X(06) VALUE "USADO".
            03  TABSITUACAO2 PIC X(06) VALUE "SUCATA".
       01 TABELASITUACAO REDEFINES TABSITUACAO.
            03  VETORSITUACAO PIC X(06) OCCURS 3 TIMES.
            
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
           OPEN I-O CADVEIC
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CADVEIC
                      CLOSE CADVEIC
                      MOVE "** ARQUIVO VEÍCULO SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO VEICULO.DAT" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE ZEROS  TO NUM MARCA CALCIPVA
                MOVE ZEROS  TO DIA MES ANO
                MOVE ZEROS  TO VALORVEIC IPVA
                MOVE SPACES TO LETRA PROPRIETARIO
                MOVE SPACES TO SITUACAO.
                DISPLAY  (01, 01) ERASE
                DISPLAY  (01, 20) "CADASTRO DE VEICULO"
                DISPLAY  (04, 01) "PLACA           : "
                DISPLAY  (05, 01) "PROPRIETARIO    : "
                DISPLAY  (06, 01) "MARCA           : "
                DISPLAY  (07, 01) "DATA COMPRA     :__/__/____"
                DISPLAY  (08, 01) "VALOR DO VEICULO: "
                DISPLAY  (09, 01) "% IPVA          : "
                DISPLAY  (10, 01) "VALOR DO IPVA   : "
                DISPLAY  (11, 01) "SITUACAO        : ".
       INC-002.
                ACCEPT  (04, 18) LETRA
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE CADVEIC
                   GO TO ROT-FIM.
                IF LETRA  = 0
                   MOVE "*** VEICULO INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
       INC-003.
                ACCEPT  (04, 22) NUM
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE CADVEIC
                   GO TO ROT-FIM.
                IF NUM  < 1000 OR NUM > 9999
                   MOVE "*** VEICULO INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
       LER-VEICULO01.
                MOVE 0 TO W-SEL
                READ CADVEIC
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY (05, 18) PROPRIETARIO
                      DISPLAY (06, 18) MARCA
                      MOVE VETORMARCA(MARCA + 1) TO TEXTOMARCA
                      DISPLAY (06, 20) TEXTOMARCA
                      DISPLAY (07, 18) DIA
                      DISPLAY (07, 21) MES
                      DISPLAY (07, 24) ANO
                      MOVE VALORVEIC TO MASC1
                      DISPLAY (08, 18) MASC1
                      MOVE IPVA TO MASC2
                      DISPLAY (09, 18) MASC2
                      MOVE CALCIPVA TO MASC3
                      DISPLAY (10, 18) MASC3
                      DISPLAY (11, 18) SITUACAO
                      IF SITUACAO = "N" OR "n"
                          MOVE VETORSITUACAO(1) TO TEXTOSITUACAO
                          DISPLAY (11, 20) TEXTOSITUACAO
                      ELSE
                          IF SITUACAO = "U" OR "u"
                              MOVE VETORSITUACAO(2) TO TEXTOSITUACAO
                              DISPLAY (11, 20) TEXTOSITUACAO
                          ELSE
                              IF SITUACAO = "S" OR "s"
                                MOVE VETORSITUACAO(3) TO TEXTOSITUACAO
                                DISPLAY (11, 20) TEXTOSITUACAO
                      MOVE "*** VEICULO JA CADASTRAD0 ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO VEICULO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-004.
                ACCEPT (05, 18) PROPRIETARIO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
                IF PROPRIETARIO = SPACES
                    MOVE "*** PROPRIETARIO INVALIDO ***" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-004.
       INC-005.
                ACCEPT (06, 18) MARCA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
                IF MARCA < 0 OR MARCA > 9
                    MOVE "*** MARCA INVALIDO ***" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-004.
       INC-006.
                MOVE VETORMARCA(MARCA + 1) TO TEXTOMARCA.
                DISPLAY (06, 20) TEXTOMARCA.
       INC-DIA.
                ACCEPT (07, 18) DIA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-007.
                IF DIA < 1 OR DIA > 31
                    MOVE "*** DIA INVALIDO ***" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-DIA.
       INC-MES.
                ACCEPT (07, 21) MES
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-DIA.
                IF MES < 1 OR MES > 12
                    MOVE "*** MES INVALIDO ***" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-MES.
       INC-ANO.
                ACCEPT (07, 24) ANO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-007.
                IF ANO < 1800 OR ANO > 2999
                    MOVE "*** ANO INVALIDO ***" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-ANO.
      *
      ****************************************************
      * VALIDA MES COM A PARTICULARIDADE DO ANO BISSEXTO *
      ****************************************************
      * 
       INC-007.
                DIVIDE ANO BY 4 GIVING RESULTADO REMAINDER RESTO.
                IF MES = 2
                    IF RESTO = 0
                        IF DIA > 29
                            MOVE "*** DIA INVALIDO! ***" TO MENS
                            PERFORM ROT-MENS THRU ROT-MENS-FIM
                            GO TO INC-DIA
                        ELSE
                            GO TO INC-008.
       INC-008.
                IF DIA > TBDIAMESV(MES) AND MES NOT = 2
                    MOVE "*** DIA INVALIDO! ***" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-DIA.
      ***************************************************************
       INC-009.
                ACCEPT (08, 18) VALORVEIC
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-007.
                IF VALORVEIC < 0 OR ANO > 99999999
                    MOVE "*** VALOR INVALIDO ***" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-ANO.
       INC-010.
                ACCEPT (09, 18) IPVA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-007.
                IF IPVA < 0 OR IPVA > 100
                    MOVE "*** IPVA INVALIDO ***" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-ANO.
       INC-011.
                DIVIDE IPVA BY 100 GIVING CONTA
                MULTIPLY CONTA BY VALORVEIC GIVING CALCIPVA
                DISPLAY (10, 18) CALCIPVA.
       INC-012.
                ACCEPT (11, 18) SITUACAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-011.
                IF SITUACAO = SPACES
                    MOVE "*** SITUACAO INVALIDO ***" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-012.
       INC-013.
                IF SITUACAO = "N" OR "n"
                    MOVE VETORSITUACAO(1) TO TEXTOSITUACAO
                    DISPLAY (11, 20) TEXTOSITUACAO
                ELSE
                    IF SITUACAO = "U" OR "u"
                        MOVE VETORSITUACAO(2) TO TEXTOSITUACAO
                        DISPLAY (11, 20) TEXTOSITUACAO
                    ELSE
                        IF SITUACAO = "S" OR "s"
                            MOVE VETORSITUACAO(3) TO TEXTOSITUACAO
                            DISPLAY (11, 20) TEXTOSITUACAO
                        ELSE
                            MOVE "*** SITUAÇÃO INVALIDO ***" TO MENS
                            PERFORM ROT-MENS THRU ROT-MENS-FIM
                            GO TO INC-013.
                            
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
                WRITE REGVEIC
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
                DELETE CADVEIC RECORD
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
                REWRITE REGVEIC
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
