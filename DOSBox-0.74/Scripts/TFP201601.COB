
       01  TFP201601.
           05  LINE 01  COLUMN 01 
               VALUE  "麺様様様様様様様様様様様様様様様様様様様".
           05  LINE 01  COLUMN 41 
               VALUE  "様様様様様様様様様様様様様様様様様様様融".
           05  LINE 02  COLUMN 01 
               VALUE  "�".
           05  LINE 02  COLUMN 41 
               VALUE  "                                       �".
           05  LINE 03  COLUMN 01 
               VALUE  "�                               CADASTRO".
           05  LINE 03  COLUMN 41 
               VALUE  " DE CEP                                �".
           05  LINE 04  COLUMN 01 
               VALUE  "�".
           05  LINE 04  COLUMN 41 
               VALUE  "                                       �".
           05  LINE 05  COLUMN 01 
               VALUE  "麺様様様様様様様様様様様様様様様様様様様".
           05  LINE 05  COLUMN 41 
               VALUE  "様様様様様様様様様様様様様様様様様様様郵".
           05  LINE 06  COLUMN 01 
               VALUE  "�".
           05  LINE 06  COLUMN 41 
               VALUE  "                                       �".
           05  LINE 07  COLUMN 01 
               VALUE  "�  敖[ NUMERO ]賃�[ LOGRADOURO ]陳陳陳陳".
           05  LINE 07  COLUMN 41 
               VALUE  "陳陳陳陳陳陳陳朕                       �".
           05  LINE 08  COLUMN 01 
               VALUE  "�  �            �".
           05  LINE 08  COLUMN 41 
               VALUE  "               �                       �".
           05  LINE 09  COLUMN 01 
               VALUE  "�  青陳陳陳陳陳珍陳陳陳陳陳陳陳陳陳陳陳�".
           05  LINE 09  COLUMN 41 
               VALUE  "陳陳陳陳陳陳陳潰                       �".
           05  LINE 10  COLUMN 01 
               VALUE  "�  敖[ BAIRRO ]陳陳陳陳陳賃�[ CIDADE ]陳".
           05  LINE 10  COLUMN 41 
               VALUE  "陳陳陳陳賃�[ UF ]朕                    �".
           05  LINE 11  COLUMN 01 
               VALUE  "�  �                      �".
           05  LINE 11  COLUMN 41 
               VALUE  "         �        �                    �".
           05  LINE 12  COLUMN 01 
               VALUE  "�  青陳陳陳陳陳陳陳陳陳陳珍陳陳陳陳陳陳�".
           05  LINE 12  COLUMN 41 
               VALUE  "陳陳陳陳珍陳陳陳陳�                    �".
           05  LINE 13  COLUMN 01 
               VALUE  "�  敖[ TIPO LOCALIDADE ]陳陳�".
           05  LINE 13  COLUMN 41 
               VALUE  "                                       �".
           05  LINE 14  COLUMN 01 
               VALUE  "�  �                        �".
           05  LINE 14  COLUMN 41 
               VALUE  "                                       �".
           05  LINE 15  COLUMN 01 
               VALUE  "�  青陳陳陳陳陳陳陳陳陳陳陳潰".
           05  LINE 15  COLUMN 41 
               VALUE  "                                       �".
           05  LINE 16  COLUMN 01 
               VALUE  "�".
           05  LINE 16  COLUMN 41 
               VALUE  "                                       �".
           05  LINE 17  COLUMN 01 
               VALUE  "�".
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
               VALUE  "様様様様様様様様様様様様様様様様曜様様郵".
           05  LINE 23  COLUMN 01 
               VALUE  "�MENSAGEM:".
           05  LINE 23  COLUMN 41 
               VALUE  "                                 � CPF �".
           05  LINE 24  COLUMN 01 
               VALUE  "藩様様様様様様様様様様様様様様様様様様様".
           05  LINE 24  COLUMN 41 
               VALUE  "様様様様様様様様様様様様様様様様擁様様夕".
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
