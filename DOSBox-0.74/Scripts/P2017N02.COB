       IDENTIFICATION DIVISION.
       PROGRAM-ID. CEP001.
      **************************************
      * MANUTENCAO DO CADASTRO DE VEICULO  *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT VEICULO ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS PLACA
      * NOTA: Não sei se eu posso marcar como chave um registro que vai ser quebrado depois  
                    FILE STATUS  IS ST-ERRO.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD VEICULO
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "VEICULO.DAT".
       01 REGVEICULO.
                03 PLACA.
                    05 LETRA            PIC X(3).
                    05 NUMERO           PIC 9(4).
                03 PROPIETARIO          PIC X(35).
                03 MARCA                PIC 9(1).
                03 DATACOMPRA.
                    05 DIA              PIC 99.
                    05 MES              PIC 99.
                    05 ANO              PIC 9(4).
                03 VALORVEICULO         PIC 9(6)V99.
                03 IPVA                 PIC 99V99.
                03 SITUACAO             PIC X(1).
   
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
      *77 MASC1        PIC 9.999.9999  NOTA: ainda não consegui fazer esse bang funcionar
       
      * VETOR

      * Você tem que criar uma tabela
      * Você vai criar uma variavel que vai receber os valores da tabela
       
      * TODO: concertar essa tabela com as marcas certas 
       01 TABMARCAS.
               03 AMIGO1 PIC X(12) VALUE "FORD".  
               03 AMIGO2 PIC X(12) VALUE "FIAT".  
               03 AMIGO3 PIC X(12) VALUE "BAIRRO".  
               03 AMIGO4 PIC X(12) VALUE "CLUBE".  
               03 AMIGO5 PIC X(12) VALUE "BALADA".  
               03 AMIGO6 PIC X(12) VALUE "ACADEMIA".  
               03 AMIGO7 PIC X(12) VALUE "BOTECO".  
               03 AMIGO8 PIC X(12) VALUE "PRAIA".  
               03 AMIGO9 PIC X(12) VALUE "VIAGEM".  
               03 AMIGO10 PIC X(12) VALUE "INIMIGO".
       
       01 INDICE REDEFINES TABMARCAS.
               03 INDICE PIC X(12) OCCURS 10 TIMES.    
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O VEICULO
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT VEICULO
                      CLOSE VEICULO
                      MOVE "*** ARQUIVO DE VEICULOS SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
               END-IF.
                      MOVE "ERRO NA ABERTURA DO ARQUIVO DE VEICULOS" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           END-IF.
                 NEXT SENTENCE.
                    
       INC-001.
                MOVE ZEROS  TO DATACOMPRA MARCA
                MOVE SPACES TO PROPRIETARIO PLACA
                DISPLAY (01, 01) ERASE
                DISPLAY  (01, 20) "CADASTRO DE VEICULO"
                DISPLAY  (04, 01) "PLACA                : "
                DISPLAY  (05, 01) "PROPRIETARIO         : "
                DISPLAY  (06, 01) "MARCA                : "
                DISPLAY  (07, 01) "DATA COMPRA          : "
                DISPLAY  (08, 01) "VALOR DO VEICULO     : "
                DISPLAY  (09, 01) "IPVA                 : "
                DISPLAY  (10, 01) "VALOR IPVA           : "
                DISPLAY  (11, 01) "SITUAÇÃO             : "

                
       INC-002.
                ACCEPT  (04, 23) PLACA
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE VEICULO
                   GO TO ROT-FIM.
                IF PLACA  = 0
                   MOVE "*** VEICULO INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                   
       LER-VEICULO01.
                MOVE 0 TO W-SEL
                READ VEICULO
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY (04, 23) PLACA
                      DISPLAY (05, 23) PROPRIETARIO
                      DISPLAY (06, 23) MARCA
                      DISPLAY (07, 23) DATACOMPRA
                      DISPLAY (08, 23) VALORVEICULO
                      DISPLAY (09, 23) IPVA            
                      DISPLAY (10, 23) VALORIPVA      
                      DISPLAY (11, 23) SITUACAO        

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
                   
       INC-003.
                ACCEPT (05, 23) PROPRIETARIO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
       INC-004.
                ACCEPT (06, 23) MARCA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
       INC-005.
                ACCEPT (07, 23) DATACOMPRA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
       INC-006.
                ACCEPT (08, 23) VALORVEICULO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
       INC-007.
                ACCEPT (09, 23) IPVA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
       INC-008.
                ACCEPT (10, 23) VALORIPVA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-007.
       INC-009.
                ACCEPT (10, 23) SITUACAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-008.

                IF W-SEL = 1
                              GO TO ALT-OPC.

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-009.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
                   
       INC-WR1.
                WRITE VEICULO
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** VEICULO GRAVADO *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** VEICULO JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE VEICULOS"
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
                DELETE VEICULO RECORD
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
                REWRITE VEICULO
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO VEICULO"   TO MENS
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
                IF W-CONT < 2000
                    GO TO ROT-MENS2
                ELSE
                   DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.
