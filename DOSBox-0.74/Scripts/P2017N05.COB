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
      * NOTA: Não sei se eu posso marcar como chave um registro que vai 
      * ser quebrado depois  
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
                03 PROPRIETARIO          PIC X(35).
                03 MARCA                PIC 9(1).
                03 DATACOMPRA.
                    05 DIA              PIC 99.
                    05 MES              PIC 99.
                    05 ANO              PIC 9(4).
                03 VALORVEICULO         PIC 9(6)V99.
                03 IPVA                 PIC 99V99.
                03 VALORIPVA            PIC 9(7).
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
       77 TEXTOSITUACAO  PIC X(10)      VALUE SPACES.
       77 CONTA          PIC 9(06)V99   VALUE ZEROS.
       
      * VETOR

      * Você tem que criar uma tabela
      * Você vai criar uma variavel que vai receber os valores da tabela
       
      * TODO: concertar essa tabela com as marcas certas 
       01 TABMARCAS.
               03 VEICULO1 PIC X(12) VALUE "FORD".  
               03 VEICULO2 PIC X(12) VALUE "FIAT".  
               03 VEICULO3 PIC X(12) VALUE "BAIRRO".  
               03 VEICULO4 PIC X(12) VALUE "CLUBE".  
               03 VEICULO5 PIC X(12) VALUE "BALADA".  
               03 VEICULO6 PIC X(12) VALUE "ACADEMIA".  
               03 VEICULO7 PIC X(12) VALUE "BOTECO".  
               03 VEICULO8 PIC X(12) VALUE "PRAIA".  
               03 VEICULO9 PIC X(12) VALUE "VIAGEM".  
               03 VEICULO10 PIC X(12) VALUE "INIMIGO".
       
       01 TABELAMARCAS REDEFINES TABMARCAS.
               03 INDEXTABMARCAS PIC X(12) OCCURS 10 TIMES.

       01 TABSITUACAO.
               03 SITUACAO1 PIC X(12) VALUE "USADO".  
               03 SITUACAO2 PIC X(12) VALUE "NOVO".  
               03 SITUACAO3 PIC X(12) VALUE "SUCATA".

       01 TABELASITUACAO REDEFINES TABSITUACAO.
               03 INDEXTABSITUACAO PIC X(12) OCCURS 3 TIMES.

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
                MOVE "ARQUIVO DE VEICULOS SENDO CRIADO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO INC-OP0

             ELSE
                MOVE "ERRO NA ABERTURA DO ARQUIVO DE VEICULOS" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                   NEXT SENTENCE.
                    
       START-TELA.
             MOVE ZEROS  TO DATACOMPRA MARCA DIA MES ANO VALORVEICULO
             MOVE ZEROS  TO VALORIPVA IPVA
             MOVE SPACES TO PROPRIETARIO SITUACAO

             DISPLAY (01, 01) ERASE.
             DISPLAY  (01, 20) "CADASTRO DE VEICULO"
             DISPLAY  (04, 01) "PLACA                : "
             DISPLAY  (05, 01) "PROPRIETARIO         : "
             DISPLAY  (06, 01) "MARCA                : "
             DISPLAY  (07, 01) "DATA COMPRA          : "
             DISPLAY  (08, 01) "VALOR DO VEICULO     : "
             DISPLAY  (09, 01) "IPVA                 : "
             DISPLAY  (10, 01) "SITUACAO             : ".
                
       LER-LETRA.
                ACCEPT  (04, 23) LETRA 
                ACCEPT W-ACT FROM ESCAPE KEY
                 IF W-ACT = 02
                   CLOSE VEICULO
                   GO TO ROT-FIM.
                IF LETRA  = 0   
                   MOVE "*** PLACA INVALIDA ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO START-TELA.
       LER-NUMERO.
                ACCEPT (04, 26) NUMERO
                ACCEPT W-ACT FROM ESCAPE KEY 

                IF W-ACT = 02
                        CLOSE VEICULO
                        GO TO ROT-FIM.

                IF NUMERO < 1000 OR NUMERO > 9999
                   MOVE "*** PLACA INVALIDA ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO START-TELA.

                   
       LER-VEICULO.
                MOVE 0 TO W-SEL
                READ VEICULO
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
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
                ELSE.
                   
       LER-PROPIETARIO.
                ACCEPT (05, 23) PROPRIETARIO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO LER-LETRA.
       LER-MARCA.
                ACCEPT (06, 23) MARCA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO LER-PROPIETARIO.
       LER-DIA.
                ACCEPT (07, 23) DIA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO LER-MARCA.
                IF DIA < 1 OR DIA > 31
                        MOVE "DIA INVALIDO" TO MENS
                        PERFORM ROT-MENS THRU ROT-MENS-FIM
                        GO TO LER-DIA.
       LER-MES.
                ACCEPT (07, 26) MES
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO LER-DIA.
                IF MES < 1 OR MES > 12
                        MOVE "*Mês Invalido" TO MENS
                        PERFORM ROT-MENS THRU ROT-MENS-FIM
                        GO TO LER-MES.
          
       LER-ANO.
                ACCEPT (07, 29) ANO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO LER-MARCA.
                IF ANO < 1900 OR ANO > 2999
                        MOVE "*Ano inválido" TO MENS
                        PERFORM ROT-MENS THRU ROT-MENS-FIM
                        GO TO LER-ANO.
      * NOTA: Colocar o resto dos testes de dias depois

       LER-VALORVEICULO.
                ACCEPT (08, 23) VALORVEICULO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO LER-DIA.
                IF VALORVEICULO < 0 OR VALORVEICULO > 99999999
                        MOVE "*VALOR INVALIDO*" TO MENS
                        PERFORM ROT-MENS THRU ROT-MENS-FIM
                        GO TO LER-VALORVEICULO.
       LER-IPVA.
                ACCEPT (09, 23) IPVA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO LER-VALORVEICULO.
                IF IPVA < 0 OR IPVA > 100
                        MOVE "*IPVA INVALIDO*" TO MENS
                        PERFORM ROT-MENS THRU ROT-MENS-FIM
                        GO TO LER-IPVA.
       CALC-IPVA.
                DIVIDE IPVA BY 100 GIVING CONTA
                MULTIPLY CONTA BY VALORVEICULO GIVING VALORIPVA
                DISPLAY (9, 40) VALORIPVA.

       LER-SITUACAO.
                ACCEPT (10, 23) SITUACAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO LER-IPVA.

                IF SITUACAO = SPACES
                        MOVE "*Situação invalida*" to MENS
                        PERFORM ROT-MENS THRU ROT-MENS-FIM
                        GO TO LER-SITUACAO.
       SHOW-TEXTOSITUACAO.
                IF SITUACAO = "N" OR "n"
                   MOVE INDEXTABSITUACAO(1) TO TEXTOSITUACAO
                   DISPLAY (11, 20) TEXTOSITUACAO
                ELSE
                   IF SITUACAO = "U" OR "u"
                      MOVE INDEXTABSITUACAO(2) TO TEXTOSITUACAO
                      DISPLAY (11, 20) TEXTOSITUACAO
                   ELSE
                      IF SITUACAO = "S" OR "s"
                         MOVE INDEXTABSITUACAO(3) TO TEXTOSITUACAO
                         DISPLAY (11, 20) TEXTOSITUACAO
                      ELSE
                         MOVE "*** SITUAÇÃO INVALIDO ***" TO MENS
                         PERFORM ROT-MENS THRU ROT-MENS-FIM
                         GO TO LER-SITUACAO.
                IF W-SEL = 1 GO TO ALT-OPC.

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO LER-SITUACAO.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO START-TELA.

                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
                
       INC-WR1.
                WRITE REGVEICULO
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** VEICULO GRAVADO *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO START-TELA.
                IF ST-ERRO = "22"
                      MOVE "*** VEICULO JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO START-TELA
                ELSE
                  MOVE "ERRO NA GRAVACAO DO ARQUIVO DE VEICULOS" TO MENS
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
                   GO TO START-TELA.
                IF W-ACT = 03
                   GO TO LER-PROPIETARIO.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO START-TELA.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE VEICULO RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***           " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO START-TELA.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO LER-MARCA.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO START-TELA.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGVEICULO
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO START-TELA.
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
