      ******************************************************************
      *                                                                *
      *  PROGRAMA: V11C01Y                                             *
      *                                                                *
      *  FECHA CREACION: 13/06/2008                                    *
      *                                                                *
      *  AUTOR: FACTORIA                                               *
      *                                                                *
      *  APLICACION: BPC                                               *
      *                                                                *
      ******************************************************************
      * DOCUMENTACION.                                                 *
      * ------------------                                             *
      * ARCHIVO DE ENTRADA:                                            *
      * ------------------                                             *
      * PROCESO GLOBAL:PERMITE MOSTRAR LOS CLIENTE QUE SE ENCUENTRA EN *
      *  EL ARCHIVOS MERCADO LIBRE DE DIVISA EL CUAL HAN VIOLADO       *
      *  LOS PARAMETROS.                                               *
      ******************************************************************
      *                                                                *
      *         I D E N T I F I C A T I O N   D I V I S I O N          *
      *                                                                *
      *             INFORMACION GENERAL SOBRE EL PROGRAMA              *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.   V11C01Y.
       AUTHOR.       FACTORIA.
       DATE-WRITTEN. 13/06/2008
       DATE-COMPILED.
      ******************************************************************
      *                                                                *
      *        E N V I R O N M E N T         D I V I S I O N           *
      *                                                                *
      *     DESCRIPCION DEL ENTORNO Y ESPECIFICACIONES INICIALES       *
      *                                                                *
      ******************************************************************
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
      *
      *SPECIAL-NAMES.
      *    DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT SECTION.
      ******************************************************************
      *                                                                *
      *                D A T A            D I V I S I O N              *
      *                                                                *
      *            DESCRIPCION DE TODOS LOS DATOS UTILIZADOS           *
      *                                                                *
      ******************************************************************
       DATA DIVISION.
      ******************************************************************
      *                                                                *
      *         W O R K I N G   S T O R A G E   S E C T I O N          *
      *                                                                *
      ******************************************************************
       WORKING-STORAGE SECTION.
      ******************************************************************
      *                  AREA DE VARIABLES AUXILIARES                  *
      ******************************************************************
       01  WS-VARIABLES-AUXILIARES.
      *
           05  WS-PROGRAMA                 PIC X(07) VALUE SPACES.
           05  WS-MONTO-EDIT               PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99.
      *    * AD01-I : RECONVERSION MONETARIA
           05  WS-CT-LABEL                 PIC X(03) VALUE SPACES.
           05  WS-CT-LITERAL               PIC X(17) VALUE SPACES.
           05  REDEFINES WS-CT-LITERAL.
               10  WS-CT-LIT-PLURAL        PIC X(09).
               10  WS-CT-RESTO             PIC X(08).
           05  WS-DIVISA-NAT               PIC X(08) VALUE 'VJ8NDIV0'.
      *    * AD01-F

      ******************************************************************
      *                    AREA  DE  SWITCHES                          *
      ******************************************************************
       01  SW-SWITCHES.
      *
           05  SW-ENTRADO                  PIC X(01).
               88  OK-DATOS                           VALUE 'S'.
               88  NO-DATOS                           VALUE 'N'.
           05  SW-GET                      PIC 9(01)  VALUE ZEROS.
               88  SI-GET                             VALUE 1.
               88  NO-GET                             VALUE 0.
           05  SW-SALIR                    PIC 9(01)  VALUE ZEROS.
               88  SI-SALIR                           VALUE 1.
               88  NO-SALIR                           VALUE 0.
           05  SW-PRIMERA                  PIC 9(01)  VALUE 1.
               88  SI-PRIMERA                         VALUE 1.
               88  NO-PRIMERA                         VALUE 0.
           05  SW-FIN-CURSOR               PIC X(01)  VALUE 'N'.
               88  FIN-CURSOR                         VALUE 'S'.
               88  NO-FIN-CURSOR                      VALUE 'N'.
           05  SW-ENCONTRADO               PIC X(01)   VALUE 'N'.
               88  ENCONTRADO                          VALUE 'S'.
               88  NO-ENCONTRADO                       VALUE 'N'.
      ******************************************************************
      *                        AREA DE CONTANTES                       *
      ******************************************************************
       01  CT-CONTANTES.
      *                                                                *
           05  CT-PROGRAMA                 PIC X(08)   VALUE 'V11C01Y'.
           05  CT-ESTADO-INICIO            PIC X(01)   VALUE 'I'.
           05  CT-ESTADO-CONTINUA          PIC X(01)   VALUE 'C'.
           05  CT-CASO-1                   PIC X(01)   VALUE '1'.
           05  CT-CASO-2                   PIC X(01)   VALUE '2'.
           05  CT-10-SPACES                PIC X(10)   VALUE SPACES.
           05  CT-11-SPACES                PIC X(11)   VALUE SPACES.
           05  CT-10-NUEVES                PIC X(10)   VALUE
                                                       '9999999999'.
           05  CT-11-NUEVES                PIC X(11)   VALUE
                                                       '99999999999'.
           05  CT-QR4CDB0                  PIC X(07)   VALUE 'QR4CDB0'.
           05  CT-OPEN                     PIC X(04)   VALUE 'OPEN'.
           05  CT-CLOSE                    PIC X(05)   VALUE 'CLOSE'.
           05  CT-FETCH                    PIC X(05)   VALUE 'FETCH'.
           05  CT-SELECT                   PIC X(06)   VALUE 'SELECT'.
           05  CT-TAB-V1DT001              PIC X(08)   VALUE 'V1DT001'.
           05  CT-SI                       PIC X(01)   VALUE 'S'.
      ******************************************************************
      *                AREA DE MENSAJES                                *
      ******************************************************************
       01  ME-MENSAJES-ERROR.
           05  ME-TECLA-INCORRECTA         PIC X(07)   VALUE 'V1E0030'.
      ******************************************************************
      *                        AREA DE INDICES
      ******************************************************************
       01  IN-INDICES.
           05  IN-REGISTRO                 PIC S9(03) COMP
                                   VALUE ZEROES.
      ******************************************************************
      *                    COPYS UTILIZADAS                            *
      ******************************************************************
      **** COPY DFHAID *************************************************
           COPY DFHAID.
           COPY QGECABC.
      *
       COPY QCWCI20.
      *
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA.
           COPY QGECCAA.
       COPY V1EC001.
       COPY V1NC007.
      ******************************************************************
      *                                                                *
      *           P R O C E D U R E      D I V I S I O N               *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION USING V1EC001 V1NC007I.
      *
       MAINLINE.
      *
           PERFORM 1000-INICIO
              THRU 1000-INICIO-EXIT
      *
           PERFORM 2000-PROCESO
              THRU 2000-PROCESO-EXIT
      *
           PERFORM 6000-FIN
              THRU 6000-FIN-EXIT
      *
           GOBACK
           .
      ******************************************************************
      *                         1000-INICIO                            *
      ******************************************************************
       1000-INICIO.
      *
           SET ADDRESS OF V1NC007I          TO CAA-PTR-COPYIN
           SET ADDRESS OF V1EC001           TO CAA-PTRDATA
      *
           EXEC CICS
               IGNORE CONDITION ERROR
           END-EXEC
      *
           INITIALIZE WS-VARIABLES-AUXILIARES
           MOVE SPACES                     TO CAA-COD-AVISO1
                                              CAA-COD-AVISO2
                                              CAA-VAR1-ERROR
                                              CAA-VAR2-ERROR
                                              CAA-COD-ERROR
      *
           MOVE CAA-CODTRAN                TO CAA-CODTRAN-SIG
      *
           .
       1000-INICIO-EXIT.
           EXIT.
      ******************************************************************
      *                        2000-PROCESO                            *
      ******************************************************************
       2000-PROCESO.
      *
           EVALUATE CAA-ESTADO
               WHEN CT-ESTADO-INICIO
                   PERFORM 2100-ESTADO-INICIO
                      THRU 2100-ESTADO-INICIO-EXIT

               WHEN CT-ESTADO-CONTINUA
                   PERFORM 2200-ESTADO-CONTINUACION
                      THRU 2200-ESTADO-CONTINUACION-EXIT
           END-EVALUATE
      *
           .
       2000-PROCESO-EXIT.
           EXIT.
      ******************************************************************
      *                        2100-ESTADO-INICIO                      *
      ******************************************************************
       2100-ESTADO-INICIO.
      *
           INITIALIZE V1NC007I
           MOVE CT-PROGRAMA                TO WS-PROGRAMA
      *
           MOVE V153-BPBCRP90              TO BPBC618I
           MOVE V153-DESC-IDEN             TO BPBN636I
           MOVE V153-CT-LIT-PLURAL         TO CTL1850I

           MOVE V153-TOTAL-COMPRA          TO WS-MONTO-EDIT
           MOVE WS-MONTO-EDIT              TO TOT1921I

           MOVE V153-TOTAL-COMPRA-BS       TO WS-MONTO-EDIT
           MOVE WS-MONTO-EDIT              TO TOT1948I

           MOVE V153-TOTAL-VENTA           TO WS-MONTO-EDIT
           MOVE WS-MONTO-EDIT              TO TOT2021I

           MOVE V153-TOTAL-VENTA-BS        TO WS-MONTO-EDIT
           MOVE WS-MONTO-EDIT              TO TOT2048I
      *
           SET CAA-88-ACCION-TERMINAL      TO TRUE
           SET CAA-88-ESTADO-CONTIN        TO TRUE
           PERFORM ATT-CAMPOS
      *
           .
       2100-ESTADO-INICIO-EXIT.
           EXIT.
      ******************************************************************
      *                        2200-ESTADO-CONTINUACION                *
      ******************************************************************
       2200-ESTADO-CONTINUACION.
      *
           SET CAA-88-CODTRAN-SIG-ULTI     TO TRUE
           SET CAA-88-ACCION-PROGRAMA      TO TRUE
           SET CAA-88-ESTADO-INICIO        TO TRUE
      *
           .
       2200-ESTADO-CONTINUACION-EXIT.
           EXIT.
      ******************************************************************
      * ATT-CAMPOS
      ******************************************************************
       ATT-CAMPOS.
      *
           MOVE ATRI-PRO-BRI-FST           TO BPBC618A
                                              BPBN636A
                                              TOT1921A
                                              TOT2021A
                                              TOT1948A
                                              TOT2048A
                                              CTL1850A
      *
           .
      ******************************************************************
      *                            6000-FIN                            *
      ******************************************************************
       6000-FIN.
      *
           EXEC CICS
               RETURN
           END-EXEC
      *
           .
       6000-FIN-EXIT.
           EXIT.
      ******************************************************************
      *                          9999-ABEND-CICS                       *
      *  SE ABENDA LA TAREA CUANDO SE PRODUCE UN ERROR CICS NO DESEADO *
      ******************************************************************
       9999-ABEND-CICS.
      *
           MOVE 'S'                        TO ABC-ABEND
           MOVE CT-PROGRAMA                TO ABC-PROGRAMA
           MOVE EIBFN                      TO ABC-EIBFN
           MOVE EIBRSRCE                   TO ABC-EIBRSRCE
           MOVE EIBRCODE                   TO ABC-EIBRCODE
           MOVE EIBRESP                    TO ABC-EIBRESP1
           MOVE EIBRESP2                   TO ABC-EIBRESP2
      *
           PERFORM 9999-LINK-ABEND
              THRU 9999-LINK-ABEND-EXIT
      *
           .
       9999-ABEND-CICS-EXIT.
           EXIT.
      ******************************************************************
      *                         9999-LINK-ABEND                        *
      ******************************************************************
       9999-LINK-ABEND.
      *
           EXEC CICS
               LINK PROGRAM('QG1CABC')
               COMMAREA (QGECABC)
               NOHANDLE
           END-EXEC
      *
           .
       9999-LINK-ABEND-EXIT.
           EXIT.
      *
