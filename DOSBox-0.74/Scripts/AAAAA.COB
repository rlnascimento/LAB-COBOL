       IDENTIFICATION DIVISION.
       PROGRAM-ID. FP16201P.
      **************************************
      * MANUTENCAO DO CADASTRO DE VEICULOS  *
      **************************************
      *AUTOR: YURIAN ARATA MARGINI
      *RA: 162305-0
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       * SELECT CADVEIC ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS PLACA
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS SECKEY = PROPRIETARIO
                                                      WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADVEIC
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADVEIC.DAT".
       01 REGVEIC.
                03 PLACA.
                    05 LETRA            PIC X(3).
                    05 NUMERO           PIC 9(4).
                03 PROPRIETARIO         PIC X(35).
                03 MARCA                PIC 9(1).
                03 TIPOVEICULO          PIC 9.
                03 VALORDOVEICULO       PIC 9(6)V99.
                03 DATACOMPRA.
                    05 DIA              PIC 9(2).
                    05 MES              PIC 9(2).
                    05 ANO              PIC 9(4).
                03 IPVA                 PIC 99V99.
                03 SITUACAO             PIC X(1).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL            PIC 9(01) VALUE ZEROS.
       77 W-CONT           PIC 9(06) VALUE ZEROS.
       77 W-OPCAO          PIC X(01) VALUE SPACES.
       77 ST-ERRO          PIC X(02) VALUE "00".
       77 W-ACT            PIC 9(02) VALUE ZEROS.
       77 MENS             PIC X(50) VALUE SPACES.
       77 LIMPA            PIC X(50) VALUE SPACES.
       77 TEXTLOCAL        PIC X(20) VALUE SPACES.
       77 TEXTCALC         PIC X(20) VALUE SPACES.
      *
       01 TABELAM.
               03 MARCA1 PIC X(12) VALUE "FORD".  
               03 MARCA2 PIC X(12) VALUE "FIAT".  
               03 MARCA3 PIC X(12) VALUE "HONDA".  
               03 MARCA4 PIC X(12) VALUE "CHEVROLET".  
               03 MARCA5 PIC X(12) VALUE "HYUNDAI".  
               03 MARCA6 PIC X(12) VALUE "NISSAN".  
               03 MARCA7 PIC X(12) VALUE "PEUGEOT".  
               03 MARCA8 PIC X(12) VALUE "RENAULT".  
               03 MARCA9 PIC X(12) VALUE "SUZUKI".  
                
       01 TABELAS.
               03 SITUACAO01 PIC X(12) VALUE "NOVO".
               03 SITUACAO02 PIC X(12) VALUE "USADO".
               03 SITUACAOS03PIC X(12) VALUE "SUCATA".
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
                      MOVE "*** ARQUIVO VEICULOS SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO VEICULOS" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE ZEROS TO NUMERO MARCA TIPOVEICULO DATACOMPRA
                MOVE ZEROS TO VALORVEICULO IPVA
                MOVE SPACES TO LETRA PROPRIETARIO SITUACAO
                DISPLAY (01, 01) ERASE
                DISPLAY  (01, 20) "CADASTRO DE VEICULO"
                DISPLAY  (04, 01) "PLACA                : "
                DISPLAY  (05, 01) "VALOR DO VEICULO     : "
                DISPLAY  (06, 01) "PROPRIETARIO         : "
                DISPLAY  (07, 01) "PLACA                : "
                DISPLAY  (08, 01) "TIPO DE VEICULO      : "
                DISPLAY  (09, 01) "DATA DE COMPRA       : "
                DISPLAY  (10, 01) "IPVA                 : "
                DISPLAY  (10, 01) "SITUAÇÃO             : ".
       INC-002.
                ACCEPT  (04, 23) PLACA
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE CADVEIC
                   GO TO ROT-FIM.
                IF PLACA  = NULL
                   MOVE "*** VEICULO INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
       LER-VEIC01.
                MOVE 0 TO W-SEL
                READ CADVEIC
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      PERFORM INC-010
                      DISPLAY (04, 01) PLACA
                      DISPLAY (05, 01) VALORVEICULO
                      DISPLAY (06, 01) PROPRIETARIO
                      DISPLAY (07, 01) IPVA
                      DISPLAY (08, 01) TIPOVEICULO
                      DISPLAY (09, 01) DATACOMPRA
                      DISPLAY (10, 01) IPVA
                      DISPLAY (08, 01) SITUACAO
                      MOVE "VEICULO JA CADASTRAD0!" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO VEICULO!" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-004.
                ACCEPT  (05, 23) VALORVEICULO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
                IF VALORVEICULO = SPACES
                 MOVE "VALOR INVALIDO!" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-004.
       INC-005.
                ACCEPT  (06, 23) PROPRIETARIO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
                IF BAIRRO = ZEROS
                 MOVE "PROPRIETARIO INVALIDO!" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-005.
       INC-006.
                COMPUTE IPVA = VALORVEICULO * (IPVA/100)
                DISPLAY (07, 23)IPVA
       INC-007.
                ACCEPT MARCA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-007.
                MOVE 1 TO IND.
       INC-008.
                IF MARCA NOT = TBUFV(IND)
                 IF IND = 27
                  MOVE "UF INVALIDA!" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO INC-007
                 ELSE
                   ADD 1 TO IND
                   GO TO INC-008.
       INC-009.
                ACCEPT TTEXTLOCAL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-008.
                IF NLOCAL < 1 OR NLOCAL > 9
                 MOVE "LOCAL INVALIDO!" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-009.
       INC-010.         
                MOVE TBLOCV(NLOCAL) TO TEXTLOCAL.
                DISPLAY TFP16201P.
       INC-011.
                ACCEPT TCALC
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-009.
                IF NCALC < 1 OR NCALC > 8
                 MOVE "CALCAMENTO INVALIDO!" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-011.
       INC-012.         
                MOVE TBCALCV(NCALC) TO TEXTCALC.
                DISPLAY TFP16201P.
       INC-013.
                ACCEPT  TCM
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-011.
                IF CM  < 10 OR CM > 50000
                   MOVE "COMPRIMENTO INVALIDO!" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-013.
       INC-014.
                ACCEPT  TDATEANO
                MOVE DATEANO TO DATA1TRAT
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-013.
                IF ANO  < 2000 OR ANO > 2020
                   MOVE "DATA  INVALIDA!" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-014.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
                IF W-OPCAO = "N" OR "n"
                   MOVE "DADOS RECUSADOS PELO OPERADOR!" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "DIGITE APENAS S=SIM e N=NAO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCEP
                IF ST-ERRO = "00" OR "02"
                      MOVE "DADOS GRAVADOS!" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "CEP JA EXISTENTE!" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE CEP!"
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
                DISPLAY (23, 40) "EXCLUIR (S/N):".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "REGISTRO NAO EXCLUIDO!" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "DIGITE APENAS S=SIM OU N=NAO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADCEP RECORD
                IF ST-ERRO = "00"
                   MOVE "REGISTRO EXCLUIDO!" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO!" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR (S/N):".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
                IF W-OPCAO = "N" OR "n"
                   MOVE "INFORMACOES NAO ALTERADAS!" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "DIGITE APENAS S=SIM OU N=NAO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCEP
                IF ST-ERRO = "00" OR "02"
                   MOVE "REGISTRO ALTERADO!" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO CEP!"   TO MENS
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