       IDENTIFICATION DIVISION.
       PROGRAM-ID. FP16201.
      **************************************
      *          CADASTRO DE CEP           *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CODCEP
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS CHAVE2 = LOGRADOURO
                                                      WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CEP.DAT".
       01 REGCEP.
                03 CODCEP          PIC 9(08).
                03 LOGRADOURO      PIC X(35).
                03 BAIRRO          PIC X(20).
                03 CIDADE          PIC X(20).
                03 UF              PIC X(02).
                03 LOCAL           PIC 9(01).
                    05 LOCALIDADE  PIC X(20).
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
      *
       SCREEN SECTION.

       01  TFP201601.
           05  LINE 01  COLUMN 01 
               VALUE  "ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05  LINE 01  COLUMN 41 
               VALUE  "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»".
           05  LINE 02  COLUMN 01 
               VALUE  "º".
           05  LINE 02  COLUMN 41 
               VALUE  "                                       º".
           05  LINE 03  COLUMN 01 
               VALUE  "º                               CADASTRO".
           05  LINE 03  COLUMN 41 
               VALUE  " DE CEP                                º".
           05  LINE 04  COLUMN 01 
               VALUE  "º".
           05  LINE 04  COLUMN 41 
               VALUE  "                                       º".
           05  LINE 05  COLUMN 01 
               VALUE  "ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05  LINE 05  COLUMN 41 
               VALUE  "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹".
           05  LINE 06  COLUMN 01 
               VALUE  "º".
           05  LINE 06  COLUMN 41 
               VALUE  "                                       º".
           05  LINE 07  COLUMN 01 
               VALUE  "º  ÚÄ[ NUMERO ]ÄÂÄ[ LOGRADOURO ]ÄÄÄÄÄÄÄÄ".
           05  LINE 07  COLUMN 41 
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿                       º".
           05  LINE 08  COLUMN 01 
               VALUE  "º  ³            ³".
           05  LINE 08  COLUMN 41 
               VALUE  "               ³                       º".
           05  LINE 09  COLUMN 01 
               VALUE  "º  ÀÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 09  COLUMN 41 
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ                       º".
           05  LINE 10  COLUMN 01 
               VALUE  "º  ÚÄ[ BAIRRO ]ÄÄÄÄÄÄÄÄÄÄÄÂÄ[ CIDADE ]ÄÄ".
           05  LINE 10  COLUMN 41 
               VALUE  "ÄÄÄÄÄÄÄÄÄÂÄ[ UF ]Ä¿                    º".
           05  LINE 11  COLUMN 01 
               VALUE  "º  ³                      ³".
           05  LINE 11  COLUMN 41 
               VALUE  "         ³        ³                    º".
           05  LINE 12  COLUMN 01 
               VALUE  "º  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 12  COLUMN 41 
               VALUE  "ÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÙ                    º".
           05  LINE 13  COLUMN 01 
               VALUE  "º  ÚÄ[ TIPO LOCALIDADE ]ÄÄÄÄ¿".
           05  LINE 13  COLUMN 41 
               VALUE  "                                       º".
           05  LINE 14  COLUMN 01 
               VALUE  "º  ³                        ³".
           05  LINE 14  COLUMN 41 
               VALUE  "                                       º".
           05  LINE 15  COLUMN 01 
               VALUE  "º  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
           05  LINE 15  COLUMN 41 
               VALUE  "                                       º".
           05  LINE 16  COLUMN 01 
               VALUE  "º".
           05  LINE 16  COLUMN 41 
               VALUE  "                                       º".
           05  LINE 17  COLUMN 01 
               VALUE  "º".
           05  LINE 17  COLUMN 41 
               VALUE  "                                       º".
           05  LINE 18  COLUMN 01 
               VALUE  "º".
           05  LINE 18  COLUMN 41 
               VALUE  "                                       º".
           05  LINE 19  COLUMN 01 
               VALUE  "º".
           05  LINE 19  COLUMN 41 
               VALUE  "                                       º".
           05  LINE 20  COLUMN 01 
               VALUE  "º".
           05  LINE 20  COLUMN 41 
               VALUE  "                                       º".
           05  LINE 21  COLUMN 01 
               VALUE  "º".
           05  LINE 21  COLUMN 41 
               VALUE  "                                       º".
           05  LINE 22  COLUMN 01 
               VALUE  "ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05  LINE 22  COLUMN 41 
               VALUE  "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍ¹".
           05  LINE 23  COLUMN 01 
               VALUE  "ºMENSAGEM:".
           05  LINE 23  COLUMN 41 
               VALUE  "                                 º CPF º".
           05  LINE 24  COLUMN 01 
               VALUE  "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05  LINE 24  COLUMN 41 
               VALUE  "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍ¼".
           05  TNUMCEP
               LINE 08  COLUMN 06  PIC 99999.999
               USING  CODCEP
               HIGHLIGHT.
           05  TLOGRA
               LINE 08  COLUMN 19  PIC X(35)
               USING  LOGRADOURO
               HIGHLIGHT.
           05  TBAIRRO
               LINE 11  COLUMN 06  PIC X(20)
               USING  BAIRRO
               HIGHLIGHT.
           05  TCIDADE
               LINE 11  COLUMN 29  PIC X(20)
               USING  CIDADE
               HIGHLIGHT.
           05  TUF
               LINE 11  COLUMN 54  PIC X(02)
               USING  UF
               HIGHLIGHT.
           05  TLOCAL
               LINE 14  COLUMN 06  PIC 9(01)
               USING  LOCAL
               HIGHLIGHT.
           05  TTLOCAL
               LINE 14  COLUMN 08  PIC X(20)
               USING  LOCALIDADE
               HIGHLIGHT.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O CEP
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CEP
                      CLOSE CEP
                      MOVE "*** ARQUIVO CEP SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CEP" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       TELA.
           DISPLAY TFP201601.
       INC-001.
                MOVE ZERO   TO CODCEP LOCAL
                MOVE SPACES TO LOGRADOURO BAIRRO CIDADE UF LOCALIDADE.
                DISPLAY TFP201601.
       INC-002.
                ACCEPT  CODCEP
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE CEP
                   GO TO ROT-FIM.
                IF CODCEP  = 0
                   MOVE "*** CEP INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
       LER-CEP01.
                MOVE 0 TO W-SEL
                READ CEP
                PERFORM INC-008
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY TFP201601
                      MOVE "*** CEP JA CADASTRAD0 ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO CEP" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-004.
                ACCEPT TLOGRA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
       INC-005.
                ACCEPT TBAIRRO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
       INC-006.
                ACCEPT TCIDADE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
       INC-007.
                ACCEPT TUF
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
       INC-008.
                ACCEPT TLOCAL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-007.
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
                WRITE REGCEP
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** CEP JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE CEP"
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
                DELETE CEP RECORD
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
                REWRITE REGCEP
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO CEP"   TO MENS
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
    