       01  CFT001.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01
               VALUE  "浜様様様様様様様様様様様様様様様様様様様".
           05  LINE 01  COLUMN 41
               VALUE  "様様様様様様様様様様様様様様様様様様様融".
           05  LINE 02  COLUMN 01
               VALUE  "�                        *** CADASTRO DE".
           05  LINE 02  COLUMN 41
               VALUE  " AMIGOS ***                            �".
           05  LINE 03  COLUMN 01
               VALUE  "麺様様様様様様様様様様様様様様様様様様様".
           05  LINE 03  COLUMN 41
               VALUE  "様様様様様様様様様様様様様様様様様様様郵".
           05  LINE 04  COLUMN 01
               VALUE  "�".
           05  LINE 04  COLUMN 41
               VALUE  "                                       �".
           05  LINE 05  COLUMN 01
               VALUE  "�".
           05  LINE 05  COLUMN 41
               VALUE  "                                       �".
           05  LINE 06  COLUMN 01
               VALUE  "� �APELIDO陳陳陳朕 �NOME陳陳陳陳陳陳陳陳".
           05  LINE 06  COLUMN 41
               VALUE  "陳陳陳陳陳陳�   �IDADE朕               �".
           05  LINE 07  COLUMN 01
               VALUE  "� �              � �".
           05  LINE 07  COLUMN 41
               VALUE  "            �   �      �               �".
           05  LINE 08  COLUMN 01
               VALUE  "� 青陳陳陳陳陳陳潰 青陳陳陳陳陳陳陳陳陳�".
           05  LINE 08  COLUMN 41
               VALUE  "陳陳陳陳陳陳�   青陳陳潰               �".
           05  LINE 09  COLUMN 01
               VALUE  "� �ENDERECO陳陳陳陳陳陳陳陳陳陳陳陳� �BA".
           05  LINE 09  COLUMN 41
               VALUE  "IRRO陳陳陳陳陳陳陳陳�                  �".
           05  LINE 10  COLUMN 01
               VALUE  "� �                                � �".
           05  LINE 10  COLUMN 41
               VALUE  "                    �                  �".
           05  LINE 11  COLUMN 01
               VALUE  "� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 青�".
           05  LINE 11  COLUMN 41
               VALUE  "陳陳陳陳陳陳陳陳陳陳�                  �".
           05  LINE 12  COLUMN 01
               VALUE  "� �CIDADE陳陳陳陳陳陳陳陳朕 �UF陳� �TELE".
           05  LINE 12  COLUMN 41
               VALUE  "FONE陳陳陳陳陳陳陳� �CEP陳陳陳陳�      �".
           05  LINE 13  COLUMN 01
               VALUE  "� �                       � �    � �".
           05  LINE 13  COLUMN 41
               VALUE  "                  � �           �      �".
           05  LINE 14  COLUMN 01
               VALUE  "� 青陳陳陳陳陳陳陳陳陳陳陳� 青陳潰 青陳�".
           05  LINE 14  COLUMN 41
               VALUE  "陳陳陳陳陳陳陳陳陳� 青陳陳陳陳陳�      �".
           05  LINE 15  COLUMN 01
               VALUE  "� �EMAIL陳陳陳陳陳陳陳陳陳陳陳陳陳朕".
           05  LINE 15  COLUMN 41
               VALUE  "                                       �".
           05  LINE 16  COLUMN 01
               VALUE  "� �                                �".
           05  LINE 16  COLUMN 41
               VALUE  "                                       �".
           05  LINE 17  COLUMN 01
               VALUE  "� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰".
           05  LINE 17  COLUMN 41
               VALUE  "                                       �".
           05  LINE 18  COLUMN 01
               VALUE  "�".
           05  LINE 18  COLUMN 41
               VALUE  "                                       �".
           05  LINE 19  COLUMN 01
               VALUE  "�".
           05  LINE 19  COLUMN 41
               VALUE  "                                       �".
           05  LINE 20  COLUMN 01
               VALUE  "�".
           05  LINE 20  COLUMN 41
               VALUE  "                                       �".
           05  LINE 21  COLUMN 01
               VALUE  "�".
           05  LINE 21  COLUMN 41
               VALUE  "                                       �".
           05  LINE 22  COLUMN 01
               VALUE  "麺様様様様様様様様様様様様様様様様様様様".
           05  LINE 22  COLUMN 41
               VALUE  "様様様様様様様様様様様様様様様僕様様様郵".
           05  LINE 23  COLUMN 01
               VALUE  "�MENSAGEM :".
           05  LINE 23  COLUMN 41
               VALUE  "                              � CFP001 �".
           05  LINE 24  COLUMN 01
               VALUE  "藩様様様様様様様様様様様様様様様様様様様".
           05  LINE 24  COLUMN 41
               VALUE  "様様様様様様様様様様様様様様様瞥様様様夕".
           05  T-APELIDO
               LINE 07  COLUMN 05  PIC X(12)
               USING  V-APELIDO
               HIGHLIGHT.
           05  T-NOME
               LINE 07  COLUMN 22  PIC X(30)
               USING  V-NOME
               HIGHLIGHT.
           05  T-IDADE
               LINE 07  COLUMN 60  PIC 9(02)
               USING  V-IDADE
               HIGHLIGHT.
           05  T-ENDERECO
               LINE 10  COLUMN 05  PIC X(30)
               USING  V-ENDERECO
               HIGHLIGHT.
           05  T-BAIRRO
               LINE 10  COLUMN 40  PIC
               USING  V-BAIRRO
               HIGHLIGHT.
           05  T-CIDADE
               LINE 13  COLUMN 05  PIC X(21)
               USING  V-CIDADE
               HIGHLIGHT.
           05  T-ESTADO
               LINE 13  COLUMN 31  PIC X(02)
               USING  V-ESTADO
               HIGHLIGHT.
           05  T-FONE
               LINE 13  COLUMN 38  PIC X(20)
               USING  V-FONE
               HIGHLIGHT.
           05  T-CEP
               LINE 13  COLUMN 63  PIC 99999.999
               USING  V-CEP
               HIGHLIGHT.
           05  T-EMAIL
               LINE 16  COLUMN 05  PIC X(30)
               USING  V-EMAIL
               HIGHLIGHT.