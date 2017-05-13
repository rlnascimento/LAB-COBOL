       IDENTIFICATION DIVISION.
       PROGRAM-ID. FP16201P.
      **************************************
      * MANUTENCAO DO CADASTRO DE CEP  *
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
       SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS SECKEY = STREET
                                                      WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT".
       01 REGCEP.
                03 CEP             PIC 9(08).
                03 STREET          PIC X(35).
                03 BAIRRO          PIC X(20).
                03 CITY            PIC X(20).
                03 UF              PIC X(2).
                03 NLOCAL          PIC 9(01).
                03 NCALC           PIC 9(01).
                03 CM              PIC 9(05).
                03 DATEANO         PIC 9(08).
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
       01 TABUF.
           03 TABUF1       PIC X(56) VALUE 
        "ACALAMAPBACEDFESGOMAMGMSMTPAPBPEPIPRRJRNRORRRSSCSESPTO".
       01 TBUF REDEFINES TABUF.
           03 TBUFV        PIC X(02) OCCURS 27 TIMES.
       01 IND              PIC 9(02) VALUE ZEROS.
      *
       01 DATA1TRAT.
           03 DIA          PIC 9(02).
           03 MES          PIC 9(02).
           03 ANO          PIC 9(04).
      *
       01 TABLOC.
           03 TABLOC1      PIC X(12) VALUE "CENTRAL".
           03 TABLOC2      PIC X(12) VALUE "ZONA LESTE 1".
           03 TABLOC3      PIC X(12) VALUE "ZONA LESTE 2".
           03 TABLOC4      PIC X(12) VALUE "ZONA OESTE 1".
           03 TABLOC5      PIC X(12) VALUE "ZONA OESTE 2".
           03 TABLOC6      PIC X(12) VALUE "ZONA NORTE 1".
           03 TABLOC7      PIC X(12) VALUE "ZONA NORTE 2".
           03 TABLOC8      PIC X(12) VALUE "ZONA SUL 1".
           03 TABLOC9      PIC X(12) VALUE "ZONA SUL 2".
       01 TBLOC REDEFINES TABLOC.
           03 TBLOCV       PIC X(12) OCCURS 9 TIMES.
      *
       01 TABCALC.
           03 TABCALC1     PIC X(13) VALUE "ASFALTO".
           03 TABCALC2     PIC X(13) VALUE "PARALELPIPEDO".
           03 TABCALC3     PIC X(13) VALUE "PEDRA".
           03 TABCALC4     PIC X(13) VALUE "CASCALHO".
           03 TABCALC5     PIC X(13) VALUE "TERRA BATIDA".
           03 TABCALC6     PIC X(13) VALUE "AREIA".
           03 TABCALC7     PIC X(13) VALUE "NAO DEFINIDO".
           03 TABCALC8     PIC X(13) VALUE "OUTROS".
       01 TBCALC REDEFINES TABCALC.
           03 TBCALCV  PIC X(13) OCCURS 8 TIMES.
      *
       SCREEN SECTION.
       
       01  TFP16201P.
           05  LINE 03  COLUMN 01 
               VALUE  "      CEP:".
           05  LINE 06  COLUMN 01 
               VALUE  "      RUA:".
           05  LINE 09  COLUMN 01 
               VALUE  "      BAIRRO:".
           05  LINE 09  COLUMN 41 
               VALUE  "COMPRIMENTO:".
           05  LINE 12  COLUMN 01 
               VALUE  "      CIDADE:".
           05  LINE 12  COLUMN 41 
               VALUE  "DATA:".
           05  LINE 15  COLUMN 01 
               VALUE  "      UF:".
           05  LINE 18  COLUMN 01 
               VALUE  "      LOCAL:    -".
           05  LINE 21  COLUMN 01 
               VALUE  "      CALCAMENTO:    -".
           05  TCEP
               LINE 03  COLUMN 13  PIC 99999.999
               USING  CEP
               HIGHLIGHT.
           05  TSTREET
               LINE 06  COLUMN 20  PIC X(35)
               USING  STREET
               HIGHLIGHT.
           05  TBAIRRO
               LINE 09  COLUMN 16  PIC X(20)
               USING  BAIRRO
               HIGHLIGHT.
           05  TCM
               LINE 09  COLUMN 55  PIC 9(05)
               USING  CM
               HIGHLIGHT.
           05  TCITY
               LINE 12  COLUMN 16  PIC X(20)
               USING  CITY
               HIGHLIGHT.
           05  TDATEANO
               LINE 12  COLUMN 48  PIC 99.99.9999
               USING  DATEANO
               HIGHLIGHT.
           05  TUF
               LINE 15  COLUMN 12  PIC X(02)
               USING  UF
               HIGHLIGHT.
           05  TLOCAL
               LINE 18  COLUMN 20  PIC 9(01)
               USING  NLOCAL
               HIGHLIGHT.
           05  TTEXTLOCAL
               LINE 18  COLUMN 24  PIC X(20)
               USING  TEXTLOCAL
               HIGHLIGHT.
           05  TCALC
               LINE 21  COLUMN 20  PIC 9(01)
               USING  NCALC
               HIGHLIGHT.
           05  TTEXTCALC
               LINE 21  COLUMN 24  PIC X(20)
               USING  TEXTCALC
               HIGHLIGHT.
			   
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O CADCEP
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CADCEP
                      CLOSE CADCEP
                      MOVE "*** ARQUIVO CEP SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CEP" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE ZEROS TO CEP NLOCAL NCALC
                MOVE ZEROS TO DATEANO CM
                MOVE SPACES TO STREET BAIRRO CITY UF
                MOVE ZEROS TO DATA1TRAT DIA MES ANO
                MOVE SPACES TO TEXTLOCAL TEXTCALC.
                DISPLAY TFP16201P.
       INC-002.
                ACCEPT TCEP
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE CADCEP
                   GO TO ROT-FIM.
                IF CEP  < 1000000 OR CEP > 99999999
                   MOVE "CEP INVALIDO!" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
       LER-CEP01.
                MOVE 0 TO W-SEL
                READ CADCEP
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      PERFORM INC-010
                      DISPLAY TFP16201P
                      MOVE "CEP JA CADASTRAD0!" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO CEP!" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-004.
                ACCEPT TSTREET
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
                IF STREET = SPACES
                 MOVE "RUA INVALIDA!" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-004.
       INC-005.
                ACCEPT TBAIRRO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
                IF BAIRRO = SPACES
                 MOVE "BAIRRO INVALIDO!" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-005.
       INC-006.
                ACCEPT TCITY
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
                IF CITY = SPACES
                 MOVE "CIDADE INVALIDA!" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-006.
       INC-007.
                ACCEPT TUF
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
                MOVE 1 TO IND.
       INC-008.
                IF UF NOT = TBUFV(IND)
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