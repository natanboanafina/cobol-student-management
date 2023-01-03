       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.MANAGEMENT.
      *=================================================================
      *    NAME: NATAN BOANAFINA
      *    ENTERPRISE: N/A
      *    DATE: UNTIL 20/12/2022
      *    PROFESSOR: IVAN PETRUCCI
      *    PURPOSE: EXERCISE REQUIRED TO COMPLETE COBOL COURSE.
      *=================================================================

      *--------------- ENVIRONMENT DIVISION BEGINNING
       ENVIRONMENT                       DIVISION.
      *-----------------------------------------------------------------
      *--------------- CONFIGURATION SECTION BEGINNING
       CONFIGURATION                     SECTION.
      *-----------------------------------------------------------------
       SPECIAL-NAMES.
                 DECIMAL-POINT IS COMMA.
      *--------------- FILES SECTION BEGINNING
       INPUT-OUTPUT                      SECTION.
       FILE-CONTROL.
           SELECT STUDENTS ASSIGN
           TO "C:\Users\natan\Desktop\TASK\DATA\ALUNO.DAT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS RM
           FILE STATUS IS FS-STATUS.
      *=================================================================
      *--------------- DATA DIVISION BEGINNING
       DATA                              DIVISION.
      *--------------- FILE SECTION BEGINNING
       FILE                              SECTION.
      *-----------------------------------------------------------------
      *--------------- FD AREA BEGINNING
       FD  STUDENTS.
       01  REG-STUDENTS.
           05 RM                 PIC 9(05).
           05 STUDENT-NAME       PIC X(20).
           05 GRADE              PIC X(03).
           05 FSSCORE            PIC 9(02)V99.
           05 SSCORE             PIC 9(02)V99.
      *-----------------------------------------------------------------
      *--------------- WORKING-STORAGE BEGINNING
       WORKING-STORAGE                   SECTION.
      *-----------------------------------------------------------------
       01  FILLER          PIC X(35)
                           VALUE "WORKING-STORAGE VARIABLES BEGINNING".
      *-----------------------------------------------------------------
       01  FILLER          PIC X(29)
                           VALUE "VARIABLES OF STATUS BEGINNING".
      *--------------- VARIABLE OF STATUS BEGINNING
       77  FS-STATUS       PIC 9(02)   VALUE ZEROS.
      *-----------------------------------------------------------------
       01  FILLER          PIC X(31)
                           VALUE "PROCESSMENT VARIABLES BEGINNING".
      *-----------------------------------------------------------------
       01  WRK-STUDENT.
           05 WRK-RM              PIC 9(05)    VALUE ZEROS
                                               BLANK WHEN ZEROS.
           05 WRK-NAME            PIC X(20)    VALUE SPACES.
           05 WRK-GRADE           PIC X(03)    VALUE SPACES.
      ****** First and Second Scores
           05  WRK-FSCORE         PIC 9(02)V99 COMP-3 VALUE ZEROS.
           05  WRK-SSCORE         PIC 9(02)V99 COMP-3 VALUE ZEROS.
      *-----------------------------------------------------------------
       01  FILLER            PIC X(30)
                             VALUE "VARIABLES OF CONTROL BEGINNING".
      *-----------------------------------------------------------------
       77  WRK-OPTION        PIC 9(01)     VALUE ZEROS.
       77  WRK-KEY           PIC A(01)     VALUE SPACES.
      *-----------------------------------------------------------------
       01  FILLER            PIC X(31)
                             VALUE "VARIABLES OF MESSAGES BEGINNING".
      *-----------------------------------------------------------------
       77  WRK-WARNING          PIC X(26)
                                VALUE "ERROR: DOCUMENT NOT OPENED".
       77  WRK-END              PIC X(17) VALUE "FINAL DE PROGRAMA".
       77  WRK-INVALID          PIC X(13) VALUE "MENU INVALIDO".
       77  WRK-INVALID-OPTION   PIC X(14) VALUE "OPCAO INVALIDA".
       77  WRK-CREATE-STUDENT   PIC X(27) VALUE
                                "DESEJA CRIAR UM NOVO ALUNO?".
       77  WRK-ADDED            PIC X(56) VALUE
           "DADOS GRAVADOS - Aperte ENTER/RETURN para voltar ao menu".
       77  WRK-PROCESS-CANCELED PIC X(18) VALUE "PROCESSO CANCELADO".
       77  WRK-EXIST            PIC X(15) VALUE "ALUNO JA EXISTE".
       77  WRK-NOT-EXIST        PIC X(16) VALUE "ALUNO NAO EXISTE".
       77  WRK-RECORD           PIC X(16) VALUE "REGISTRO GRAVADO".
       77  WRK-CHOICE           PIC X(20) VALUE "ESCOLHA UMA OPCAO:".
       77  WRK-NOT-FOUND        PIC X(62) VALUE
       "ALUNO NAO ENCONTRADO. Aperte ENTER para voltar ao menu inicial".
       77  WRK-FOUND            PIC X(58) VALUE
       "ALUNO ENCONTRADO - Aperte ENTER/RETURN para voltar ao menu".
       77  WRK-WILL-UPDATE      PIC X(21) VALUE "MODIFICAR DADOS?(S/N)".
       77  WRK-UPDATED          PIC X(57) VALUE
           "DADOS ALTERADOS. APERTE ENTER PARA VOLTAR AO MENU INICIAL".
       77  WRK-NOT-UPDATED      PIC X(19) VALUE "DADOS NAO ALTERADOS".
       77  WRK-DELETE-DATA      PIC X(19) VALUE "EXCLUIR DADOS?(S/N)".
       77  WRK-CONFIRM-DELETE   PIC X(15) VALUE "ALUNO EXCLUIDO!".
       77  WRK-FAIL-DELETE      PIC X(19) VALUE "ALUNO NAO EXCLUIDO!".
       77  WRK-DATA-NOT-FOUND   PIC X(22) VALUE
                                "NENHUM DADO ENCONTRADO".
      *-----------------------------------------------------------------
       01  FILLER            PIC X(24)
                             VALUE "MENU VARIABLES BEGINNING".
      *-----------------------------------------------------------------
       77  WRK-HEADER        PIC X(25)
                             VALUE "SISTEMA DE GESTAO ESCOLAR".

      *////////// USAR MOVE + NOME DA SECAO PARA MUDAR O VALOR DO TITLE
       77  WRK-MAIN-TITLE         PIC X(25)
            VALUE "SELECIONE  UMA  OPCAO".
       77  WRK-ADD-TITLE          PIC X(15) VALUE "ADICIONAR ALUNO".
       77  WRK-SEARCH-TITLE       PIC X(14) VALUE "PROCURAR ALUNO".
       77  WRK-UPDATE-TITLE       PIC X(13) VALUE "ALTERAR ALUNO".
       77  WRK-DELETE-TITLE       PIC X(13) VALUE "EXCLUIR ALUNO".
       77  WRK-DATA-REPORT-TITLE  PIC X(09) VALUE "RELATORIO".
       77  WRK-DIVIDER            PIC X(25)
            VALUE "*************************".

       01  SET-OPTIONS.
           05  WRK-FIRST-OPTION  PIC X(17) VALUE "1 - INCLUIR ALUNO".
           05  WRK-SECOND-OPTION PIC X(18) VALUE "2 - CONSULTA ALUNO".
           05  WRK-THIRD-OPTION  PIC X(17) VALUE "3 - ALTERAR ALUNO".
           05  WRK-FOURTH-OPTION PIC X(17) VALUE "4 - EXCLUIR ALUNO".
           05  WRK-FIFTH-OPTION  PIC X(20) VALUE "5 - RELACAO DE ALUNO".
           05  WRK-SIXTH-OPTION  PIC X(08) VALUE "6 - SAIR".

      *--------------- SETUP PARA SECAO ADDSTUDENTS
       01  SETUP.
           05  WRK-RM-OPTION        PIC X(03) VALUE "RM:".
           05  WRK-NAME-OPTION      PIC X(05) VALUE "NOME:".
           05  WRK-GRADE-OPTION     PIC X(06) VALUE "SERIE:".
           05  WRK-FSCORE-OPTION    PIC X(17) VALUE "NOTA BIMESTRE(1):".
           05  WRK-SSCORE-OPTION    PIC X(17) VALUE "NOTA BIMESTRE(2):".
           05  WRK-SAVE-OPTION      PIC X(22)
                                    VALUE "Gravar os dados (S/N)?".
      *=================================================================

      *--------------- SCREEN SECTION BEGINNING
       SCREEN                            SECTION.
      *=================================================================
       01  SCR.
           05 CLEAN-SCR.
                 10 BLANK SCREEN BACKGROUND-COLOR 7.
                 10 LINE 01 COLUMN 1 ERASE EOL BACKGROUND-COLOR 1.
                 10 LINE 02 COLUMN 1 ERASE EOL BACKGROUND-COLOR 2.
                 10 LINE 02 COLUMN 40 PIC X(25)
                                FROM WRK-HEADER
                                ERASE EOL BACKGROUND-COLOR 2
                                FOREGROUND-COLOR 0.
                 10 LINE 29 COLUMN 1 ERASE EOL BACKGROUND-COLOR 2.
                 10 LINE 30 COLUMN 1 ERASE EOL BACKGROUND-COLOR 1.

      *--------------- MAIN TITLE
       01  SCR-MAIN-TITLE.
           05 LINE 04 COLUMN 38 PIC X(26) FROM WRK-DIVIDER
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 3.
           05 LINE 05 COLUMN 38           VALUE "*"
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 3.
           05 LINE 05 COLUMN 40 PIC X(25) FROM WRK-MAIN-TITLE
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.
           05 LINE 05 COLUMN 62           VALUE "*"
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 3.
           05 LINE 06 COLUMN 38 PIC X(26) FROM WRK-DIVIDER
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 3.
      *--------------- ADD STUDENTS - TITLE
       01  SCR-ADD-TITLE.
           05 LINE 04 COLUMN 38 PIC X(26) FROM WRK-DIVIDER
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 3.
           05 LINE 05 COLUMN 38           VALUE "*"
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 3.
           05 LINE 05 COLUMN 43 PIC X(15) FROM WRK-ADD-TITLE
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.
           05 LINE 05 COLUMN 62           VALUE "*"
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 3.
           05 LINE 06 COLUMN 38 PIC X(26) FROM WRK-DIVIDER
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 3.

      *--------------- SEARCH FOR STUDENTS - TITLE
       01  SCR-SEARCH-TITLE.
           05 LINE 04 COLUMN 38 PIC X(26) FROM WRK-DIVIDER
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 3.
           05 LINE 05 COLUMN 38           VALUE "*"
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 3.
           05 LINE 05 COLUMN 43 PIC X(14) FROM WRK-SEARCH-TITLE
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.
           05 LINE 05 COLUMN 62           VALUE "*"
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 3.
           05 LINE 06 COLUMN 38 PIC X(26) FROM WRK-DIVIDER
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 3.

      *--------------- UPDATE STUDENTS - TITLE
       01  SCR-UPDATE-TITLE.
           05 LINE 04 COLUMN 38 PIC X(26) FROM WRK-DIVIDER
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 3.
           05 LINE 05 COLUMN 38           VALUE "*"
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 3.
           05 LINE 05 COLUMN 43 PIC X(13) FROM WRK-UPDATE-TITLE
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.
           05 LINE 05 COLUMN 62           VALUE "*"
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 3.
           05 LINE 06 COLUMN 38 PIC X(26) FROM WRK-DIVIDER
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 3.

      *--------------- SEARCH FOR STUDENTS - TITLE
       01  SCR-DELETE-TITLE.
           05 LINE 04 COLUMN 38 PIC X(26) FROM WRK-DIVIDER
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 3.
           05 LINE 05 COLUMN 38           VALUE "*"
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 3.
           05 LINE 05 COLUMN 43 PIC X(13) FROM WRK-DELETE-TITLE
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.
           05 LINE 05 COLUMN 62           VALUE "*"
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 3.
           05 LINE 06 COLUMN 38 PIC X(26) FROM WRK-DIVIDER
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 3.

      *--------------- DATA REPORT - TITLE
       01  SCR-REPORT-TITLE.
           05 LINE 04 COLUMN 38 PIC X(26) FROM WRK-DIVIDER
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 3.
           05 LINE 05 COLUMN 38           VALUE "*"
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 3.
           05 LINE 05 COLUMN 46 PIC X(09) FROM WRK-DATA-REPORT-TITLE
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.
           05 LINE 05 COLUMN 62           VALUE "*"
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 3.
           05 LINE 06 COLUMN 38 PIC X(26) FROM WRK-DIVIDER
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 3.
      *--------------- MAIN MENU
       01  SCR-MENU.
           05 LINE 08 COLUMN 40 PIC X(17) FROM WRK-FIRST-OPTION
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.
           05 LINE 10 COLUMN 40 PIC X(18) FROM WRK-SECOND-OPTION
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.
           05 LINE 12 COLUMN 40 PIC X(17) FROM WRK-THIRD-OPTION
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.
           05 LINE 14 COLUMN 40 PIC X(17) FROM WRK-FOURTH-OPTION
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.
           05 LINE 16 COLUMN 40 PIC X(20) FROM WRK-FIFTH-OPTION
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.
           05 LINE 18 COLUMN 40 PIC X(08) FROM WRK-SIXTH-OPTION
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.
           05 LINE 21 COLUMN 40 PIC X(20) FROM WRK-CHOICE
                                          ERASE EOL
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 2.
           05 LINE 21 COLUMN 59 PIC 9(01) USING WRK-OPTION
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 2.
      *--------------- ESCOLHA DO MENU PRINCIPAL
       01  CHOICE.
           05 LINE 21 COLUMN 40 PIC X(20) FROM WRK-CHOICE
                                ERASE EOL
                                BACKGROUND-COLOR 7
                                FOREGROUND-COLOR 2.
           05 LINE 21 COLUMN 59 PIC 9(01) USING WRK-OPTION
                                BLANK WHEN ZEROS
                                BACKGROUND-COLOR 7
                                FOREGROUND-COLOR 2.

      *--------------- MENU ADDSTUDENTS
       01  SCR-SETUP.
           05 LINE 08 COLUMN 40 PIC X(03) FROM WRK-RM-OPTION
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.
           05 LINE 10 COLUMN 40 PIC X(05) FROM WRK-NAME-OPTION
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.
           05 LINE 12 COLUMN 40 PIC X(06) FROM WRK-GRADE-OPTION
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.
           05 LINE 14 COLUMN 40 PIC X(17) FROM WRK-FSCORE-OPTION
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.
           05 LINE 16 COLUMN 40 PIC X(17) FROM WRK-SSCORE-OPTION
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.
           05 LINE 18 COLUMN 40 PIC X(22) FROM WRK-SAVE-OPTION
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.

       01  SCR-OPT.
           05 LINE 10 COLUMN 40 PIC X(05) FROM WRK-NAME-OPTION
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.
           05 LINE 12 COLUMN 40 PIC X(06) FROM WRK-GRADE-OPTION
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.
           05 LINE 14 COLUMN 40 PIC X(17) FROM WRK-FSCORE-OPTION
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.
           05 LINE 16 COLUMN 40 PIC X(17) FROM WRK-SSCORE-OPTION
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.

       01  SCR-UPDT-OPT.
           05 LINE 10 COLUMN 40 PIC X(05) FROM WRK-NAME-OPTION
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.
           05 LINE 12 COLUMN 40 PIC X(06) FROM WRK-GRADE-OPTION
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.
           05 LINE 14 COLUMN 40 PIC X(17) FROM WRK-FSCORE-OPTION
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.
           05 LINE 16 COLUMN 40 PIC X(17) FROM WRK-SSCORE-OPTION
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.
           05 LINE 18 COLUMN 40 PIC X(21) FROM WRK-WILL-UPDATE
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.

       01  SCR-DELETE-OPT.
           05 LINE 10 COLUMN 40 PIC X(05) FROM WRK-NAME-OPTION
                                                BACKGROUND-COLOR 7
                                                FOREGROUND-COLOR 0.
           05 LINE 12 COLUMN 40 PIC X(06) FROM WRK-GRADE-OPTION
                                                BACKGROUND-COLOR 7
                                                FOREGROUND-COLOR 0.
           05 LINE 14 COLUMN 40 PIC X(17) FROM WRK-FSCORE-OPTION
                                                BACKGROUND-COLOR 7
                                                FOREGROUND-COLOR 0.
           05 LINE 16 COLUMN 40 PIC X(17) FROM WRK-SSCORE-OPTION
                                                BACKGROUND-COLOR 7
                                                FOREGROUND-COLOR 0.
           05 LINE 18 COLUMN 40 PIC X(19) FROM WRK-DELETE-DATA
                                                BACKGROUND-COLOR 7
                                                FOREGROUND-COLOR 0.


      *--------------- MENU SEARCH STUDENTS / MENU UPDATE STUDENTS
       01  SCR-DISPLAY-CREATE-STUDENT.
           05 LINE 29 COLUMN 30 PIC X(28) FROM WRK-CREATE-STUDENT
                                          BACKGROUND-COLOR 2
                                          FOREGROUND-COLOR 7.
       01  SCR-CREATE-STUDENT.
           05 LINE 29 COLUMN 59 PIC A(01) USING WRK-KEY
                                          BACKGROUND-COLOR 2
                                          FOREGROUND-COLOR 7.
       01  SCR-EDIT-SETUP.
           05 LINE 08 COLUMN 40 PIC X(03) USING WRK-RM-OPTION
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.
       01  SCR-DISPLAY-RESULT.
           05 LINE 10 COLUMN 40 PIC X(05) USING WRK-NAME-OPTION
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.
           05 LINE 12 COLUMN 40 PIC X(06) USING WRK-GRADE-OPTION
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.
           05 LINE 14 COLUMN 40 PIC X(17) USING WRK-FSCORE-OPTION
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.
           05 LINE 16 COLUMN 40 PIC X(17) USING WRK-SSCORE-OPTION
                                          BACKGROUND-COLOR 7
                                          FOREGROUND-COLOR 0.

      *--------------- GETTING DATA FROM USER
       01  FILL.
           05 LINE 08 COLUMN 59 PIC 9(05)
                                USING WRK-RM
                                BACKGROUND-COLOR 7
                                FOREGROUND-COLOR 2.
           05 LINE 10 COLUMN 59 PIC X(20)
                                USING WRK-NAME
                                BACKGROUND-COLOR 7
                                FOREGROUND-COLOR 2.
           05 LINE 12 COLUMN 59 PIC X(03)
                                USING WRK-GRADE
                                BACKGROUND-COLOR 7
                                FOREGROUND-COLOR 2.
           05 LINE 14 COLUMN 59 PIC 9(02)V99
                                USING WRK-FSCORE
                                BACKGROUND-COLOR 7
                                FOREGROUND-COLOR 2.
           05 LINE 16 COLUMN 59 PIC 9(02)V99
                                USING WRK-SSCORE
                                BACKGROUND-COLOR 7
                                FOREGROUND-COLOR 2.
           05 LINE 18 COLUMN 63 PIC A(01)
                                USING WRK-KEY
                                BACKGROUND-COLOR 7
                                FOREGROUND-COLOR 2.

       01  FILL-SEARCH.
           05 LINE 08 COLUMN 59 PIC 9(05)
                                USING RM
                                BACKGROUND-COLOR 7
                                FOREGROUND-COLOR 2.
           05 LINE 10 COLUMN 59 PIC X(20)
                                USING STUDENT-NAME
                                BACKGROUND-COLOR 7
                                FOREGROUND-COLOR 2.
           05 LINE 12 COLUMN 59 PIC X(03)
                                USING GRADE
                                BACKGROUND-COLOR 7
                                FOREGROUND-COLOR 2.
           05 LINE 14 COLUMN 59 PIC 9(02)V99
                                USING FSSCORE
                                BLANK WHEN ZEROS
                                BACKGROUND-COLOR 7
                                FOREGROUND-COLOR 2.
           05 LINE 16 COLUMN 59 PIC 9(02)V99
                                USING SSCORE
                                BACKGROUND-COLOR 7
                                FOREGROUND-COLOR 2.

       01  FILL-UPDATE.
           05 UPD.
              10 LINE 08 COLUMN 59 PIC 9(05)
                                USING RM
                                BACKGROUND-COLOR 7
                                FOREGROUND-COLOR 2.
              10 LINE 10 COLUMN 59 PIC X(20)
                                USING STUDENT-NAME
                                BACKGROUND-COLOR 7
                                FOREGROUND-COLOR 2.
              10 LINE 12 COLUMN 59 PIC X(03)
                                USING GRADE
                                BACKGROUND-COLOR 7
                                FOREGROUND-COLOR 2.
              10 LINE 14 COLUMN 59 PIC 9(02)V99
                                USING FSSCORE
                                BLANK WHEN ZEROS
                                BACKGROUND-COLOR 7
                                FOREGROUND-COLOR 2.
              10 LINE 16 COLUMN 59 PIC 9(02)V99
                                USING SSCORE
                                BACKGROUND-COLOR 7
                                FOREGROUND-COLOR 2.
           05 WILL-UPDATE.
              10 LINE 18 COLUMN 62 PIC A(01)
                                USING WRK-KEY
                                ERASE EOL
                                BACKGROUND-COLOR 7
                                FOREGROUND-COLOR 2.

       01  FILL-DELETE.
           05 DLT.
              10 LINE 08 COLUMN 59 PIC 9(05)
                                FROM RM
                                BACKGROUND-COLOR 7
                                FOREGROUND-COLOR 2.
              10 LINE 10 COLUMN 59 PIC X(20)
                                FROM STUDENT-NAME
                                BACKGROUND-COLOR 7
                                FOREGROUND-COLOR 2.
              10 LINE 12 COLUMN 59 PIC X(03)
                                FROM GRADE
                                BACKGROUND-COLOR 7
                                FOREGROUND-COLOR 2.
              10 LINE 14 COLUMN 59 PIC 9(02)V99
                                FROM FSSCORE
                                BLANK WHEN ZEROS
                                BACKGROUND-COLOR 7
                                FOREGROUND-COLOR 2.
              10 LINE 16 COLUMN 59 PIC 9(02)V99
                                FROM SSCORE
                                BACKGROUND-COLOR 7
                                FOREGROUND-COLOR 2.
           05 WILL-DELETE.
              10 LINE 18 COLUMN 61 PIC A(01)
                                USING WRK-KEY
                                ERASE EOL
                                BACKGROUND-COLOR 7
                                FOREGROUND-COLOR 2.

       01  SEARCHING-RM.
           05 LINE 08 COLUMN 59 PIC 9(05)
                                USING WRK-RM
                                BLANK WHEN ZEROS
                                BACKGROUND-COLOR 7
                                FOREGROUND-COLOR 2.
       01  ADDING-RM.
           05 LINE 08 COLUMN 59 PIC 9(05)
                                USING WRK-RM
                                BLANK WHEN ZEROS
                                BACKGROUND-COLOR 7
                                FOREGROUND-COLOR 2.

      *--------------- DISPLAYING MESSAGES
       01  SCR-ADDED.
           05 LINE 29 COLUMN 35 PIC X(56) USING WRK-ADDED
                                ERASE EOL
                                BACKGROUND-COLOR 2
                                FOREGROUND-COLOR 7.

       01  SCR-PROCESS-CANCELED.
           05 LINE 29 COLUMN 35 PIC X(18) USING WRK-PROCESS-CANCELED
                                ERASE EOL
                                BACKGROUND-COLOR 2
                                FOREGROUND-COLOR 7.
       01  SCR-EXIST.
           05 LINE 29 COLUMN 35 PIC X(15) FROM WRK-EXIST
                                ERASE EOL
                                BACKGROUND-COLOR 2
                                FOREGROUND-COLOR 7.
           05 LINE 29 COLUMN 52 PIC 9(01)
                                ERASE EOL
                                BACKGROUND-COLOR 2
                                FOREGROUND-COLOR 7
                                USING WRK-OPTION.

       01  SCR-NOT-EXIST.
           05 LINE 29 COLUMN 35 PIC X(16) FROM WRK-NOT-EXIST
                                ERASE EOL
                                BACKGROUND-COLOR 2
                                FOREGROUND-COLOR 7.
           05 LINE 29 COLUMN 52 PIC 9(01)
                                ERASE EOL
                                BACKGROUND-COLOR 2
                                FOREGROUND-COLOR 7
                                USING WRK-OPTION.

       01  SCR-NOT-FOUND.
           05 LINE 29 COLUMN 30 PIC X(62) FROM WRK-NOT-FOUND
                                ERASE EOL
                                BACKGROUND-COLOR 2
                                FOREGROUND-COLOR 7.
           05 LINE 29 COLUMN 93 PIC A(01)
                                ERASE EOL
                                BACKGROUND-COLOR 2
                                FOREGROUND-COLOR 7
                                USING WRK-KEY.

       01  SCR-FOUND.
           05 LINE 29 COLUMN 32 PIC X(58) FROM WRK-FOUND
                                ERASE EOL
                                BACKGROUND-COLOR 2
                                FOREGROUND-COLOR 7.
           05 LINE 29 COLUMN 91 PIC A(01)
                                ERASE EOL
                                BACKGROUND-COLOR 2
                                FOREGROUND-COLOR 7
                                USING WRK-KEY.

       01  SCR-UPDATED.
           05 LINE 29 COLUMN 32 PIC X(57) FROM WRK-UPDATED
                                ERASE EOL
                                BACKGROUND-COLOR 2
                                FOREGROUND-COLOR 7.
           05 LINE 29 COLUMN 90 PIC A(01)
                                ERASE EOL
                                BACKGROUND-COLOR 2
                                FOREGROUND-COLOR 7
                                USING WRK-KEY.

       01  SCR-NOT-UPDATED.
           05 LINE 29 COLUMN 45 PIC X(19) FROM WRK-NOT-UPDATED
                                ERASE EOL
                                BACKGROUND-COLOR 2
                                FOREGROUND-COLOR 7.
           05 LINE 29 COLUMN 64 PIC A(01)
                                ERASE EOL
                                BACKGROUND-COLOR 2
                                FOREGROUND-COLOR 7
                                USING WRK-KEY.

       01  SCR-DELETED.
           05 LINE 29 COLUMN 45 PIC X(15) FROM WRK-CONFIRM-DELETE
                                ERASE EOL
                                BACKGROUND-COLOR 2
                                FOREGROUND-COLOR 7.
           05 LINE 29 COLUMN 61 PIC A(01)
                                ERASE EOL
                                BACKGROUND-COLOR 2
                                FOREGROUND-COLOR 7
                                USING WRK-KEY.

       01  SCR-NOT-DELETED.
           05 LINE 29 COLUMN 45 PIC X(19) FROM WRK-FAIL-DELETE
                                ERASE EOL
                                BACKGROUND-COLOR 2
                                FOREGROUND-COLOR 7.
           05 LINE 29 COLUMN 65 PIC A(01)
                                ERASE EOL
                                BACKGROUND-COLOR 2
                                FOREGROUND-COLOR 7
                                USING WRK-KEY.

       01  SCR-INVALID.
           05 LINE 29 COLUMN 35 PIC X(14) USING WRK-INVALID-OPTION
                                ERASE EOL
                                BACKGROUND-COLOR 2
                                FOREGROUND-COLOR 7.

       01  SCR-DATA-NOT-FOUND.
           05 LINE 29 COLUMN 35 PIC X(22) USING WRK-DATA-NOT-FOUND
                                ERASE EOL
                                BACKGROUND-COLOR 2
                                FOREGROUND-COLOR 7.

      *=================================================================
      *--------------- PROCEDURE DIVISION BEGINNING
       PROCEDURE                        DIVISION.
      *-----------------------------------------------------------------
       0001-MAIN                       SECTION.
           PERFORM 0100-OPEN-DATA THRU 0200-INIT.
           PERFORM 0300-PROCESS   UNTIL WRK-OPTION EQUAL 6.
           PERFORM 0400-END.
           CLOSE STUDENTS.
           GOBACK.

       0100-OPEN-DATA                  SECTION.
           OPEN I-O STUDENTS.
                IF FS-STATUS EQUAL 35 THEN
                      OPEN OUTPUT STUDENTS
                      CLOSE STUDENTS
                      OPEN I-O STUDENTS
                END-IF.
       0200-INIT                       SECTION.
           INITIALIZE SCR, REG-STUDENTS, WRK-STUDENT, WRK-OPTION.
           DISPLAY SCR.
           DISPLAY SCR-MAIN-TITLE.
           ACCEPT SCR-MENU.

       0300-PROCESS                    SECTION.
           EVALUATE WRK-OPTION
               WHEN 1
                 PERFORM 1000-ADD-STUDENTS
               WHEN 2
                 PERFORM 2000-SEARCH-STUDENTS
               WHEN 3
                 PERFORM 3000-UPDATE-STUDENT
               WHEN 4
                 PERFORM 4000-DEL-STUDENTS
               WHEN 5
                 PERFORM 5000-DATA-REPORT
               WHEN 6
                 DISPLAY 6
               WHEN OTHER
                 ACCEPT SCR-INVALID
                 PERFORM 0200-INIT
           END-EVALUATE.
       0400-END                        SECTION.
           CLOSE STUDENTS.
       1000-ADD-STUDENTS               SECTION.
           INITIALIZE SCR, WRK-KEY.
           DISPLAY SCR.
           DISPLAY SCR-ADD-TITLE.
      *     DISPLAY SCR-SETUP.
      *     ACCEPT FILL.
      *     MOVE FILL TO REG-STUDENTS.
           DISPLAY SCR-EDIT-SETUP.
           ACCEPT ADDING-RM.
           MOVE ADDING-RM TO RM.
           READ STUDENTS
             NOT INVALID KEY
               ACCEPT SCR-EXIST
               DISPLAY SCR-DISPLAY-CREATE-STUDENT
               ACCEPT SCR-CREATE-STUDENT
                   IF WRK-KEY EQUAL "S" OR WRK-KEY EQUAL "s"
                      INITIALIZE SCR, SCR-CREATE-STUDENT, WRK-KEY,
                      ADDING-RM, RM
                        DISPLAY SCR
                        DISPLAY SCR-SETUP
                        ACCEPT FILL
                        MOVE FILL TO REG-STUDENTS
                         EVALUATE WRK-KEY
                           WHEN "S"
                           WHEN "s"
                             WRITE REG-STUDENTS
                               NOT INVALID KEY
                                 INITIALIZE SCR, SCR-CREATE-STUDENT,
                                 WRK-KEY, ADDING-RM, RM
                                 ACCEPT SCR-ADDED
                                 EXIT
                               INVALID KEY
                                 ACCEPT SCR-EXIST
                                 EXIT
                             END-WRITE
                                WHEN "N"
                                WHEN "n"
                                   INITIALIZE WRK-KEY
                                   ACCEPT SCR-PROCESS-CANCELED
                                   EXIT
                                WHEN OTHER
                                   EXIT
                             END-EVALUATE
                   END-IF
             INVALID KEY
              ACCEPT SCR-NOT-EXIST
               DISPLAY SCR-DISPLAY-CREATE-STUDENT
               ACCEPT SCR-CREATE-STUDENT
                   IF WRK-KEY EQUAL "S" OR WRK-KEY EQUAL "s"
                      INITIALIZE SCR, SCR-CREATE-STUDENT, WRK-KEY
                        DISPLAY SCR
                        DISPLAY SCR-SETUP
                        ACCEPT FILL
                        MOVE FILL TO REG-STUDENTS
                        EVALUATE WRK-KEY
                           WHEN "S"
                           WHEN "s"
                             WRITE REG-STUDENTS
                               NOT INVALID KEY
                                 INITIALIZE SCR, SCR-CREATE-STUDENT,
                                 WRK-KEY, ADDING-RM, RM
                                 ACCEPT SCR-ADDED
                                 EXIT
                               INVALID KEY
                                 ACCEPT SCR-EXIST
                                 EXIT
                             END-WRITE
                                WHEN "N"
                                WHEN "n"
                                   INITIALIZE WRK-KEY
                                   ACCEPT SCR-PROCESS-CANCELED
                                   EXIT
                                WHEN OTHER
                                   EXIT
                             END-EVALUATE
                 END-IF
           END-READ.

       1200-CLOSE.
           EXIT.
           PERFORM 0200-INIT.

       2000-SEARCH-STUDENTS            SECTION.
           INITIALIZE SCR, WRK-KEY, SEARCHING-RM, WRK-RM.
           DISPLAY SCR.
           DISPLAY SCR-SEARCH-TITLE.
           DISPLAY SCR-EDIT-SETUP.
           ACCEPT SEARCHING-RM.
           MOVE SEARCHING-RM TO RM.
           READ STUDENTS
                INVALID KEY
                    ACCEPT SCR-NOT-FOUND
                NOT INVALID KEY
                     DISPLAY SCR-OPT
                     DISPLAY FILL-SEARCH
                     ACCEPT SCR-FOUND
           END-READ.

       2200-CLOSE.
           EXIT.
           PERFORM 0200-INIT.

       3000-UPDATE-STUDENT             SECTION.
           INITIALIZE SCR, WRK-KEY, WRK-RM.
           DISPLAY SCR.
           DISPLAY SCR-UPDATE-TITLE.
           DISPLAY SCR-EDIT-SETUP.
           ACCEPT SEARCHING-RM.
           MOVE SEARCHING-RM TO RM.
           READ STUDENTS
                INVALID KEY
                    ACCEPT SCR-NOT-FOUND
                NOT INVALID KEY
                    DISPLAY SCR-UPDT-OPT
                    ACCEPT FILL-UPDATE
                      IF (WRK-KEY EQUAL "S" OR WRK-KEY EQUAL "s")
                          AND FS-STATUS EQUAL 00
                                MOVE FILL-UPDATE TO REG-STUDENTS
                                REWRITE REG-STUDENTS
                                INITIALIZE WRK-KEY
                                ACCEPT SCR-UPDATED
                                EXIT
                      ELSE
                                INITIALIZE WRK-KEY
                                ACCEPT SCR-NOT-UPDATED
                                EXIT
                      END-IF.

       3200-CLOSE.
           EXIT.
           PERFORM 0200-INIT.

       4000-DEL-STUDENTS               SECTION.
           INITIALIZE SCR, WRK-KEY, WRK-RM.
           DISPLAY SCR.
           DISPLAY SCR-DELETE-TITLE.
           DISPLAY SCR-EDIT-SETUP.
           ACCEPT SEARCHING-RM.
           MOVE SEARCHING-RM TO RM.
           READ STUDENTS
                INVALID KEY
                    ACCEPT SCR-NOT-FOUND
                NOT INVALID KEY
                    DISPLAY SCR-DELETE-OPT
                    DISPLAY DLT
                    ACCEPT WILL-DELETE
                    IF WRK-KEY EQUAL "S" OR WRK-KEY EQUAL "s"
                         INITIALIZE WRK-KEY
                         DELETE STUDENTS
                            INVALID KEY
                                ACCEPT SCR-NOT-DELETED
                            NOT INVALID KEY
                                ACCEPT SCR-DELETED
                         END-DELETE
                    ELSE
                         INITIALIZE WRK-KEY
                         ACCEPT SCR-NOT-DELETED
                    END-IF
           END-READ.

       4200-CLOSE.
           EXIT.
           PERFORM 0200-INIT.

       5000-DATA-REPORT                SECTION.
           CONTINUE.
