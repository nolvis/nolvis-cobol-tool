      ******************************************************************
      *                                                                *
      *  PROGRAMA: V11C01Z                                             *
      *                                                                *
      *  FECHA CREACION: 19/06/2008                                    *
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
      * PROCESO GLOBAL: PIDE RANGO DE FECHAS                           *
      ******************************************************************
      *                                                                *
      *         I D E N T I F I C A T I O N   D I V I S I O N          *
      *                                                                *
      *             INFORMACION GENERAL SOBRE EL PROGRAMA              *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.   V11C01Z.
       AUTHOR.       FACTORIA.
       DATE-WRITTEN. 19/06/2008
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
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
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
           05  WS-ERROR                    PIC 9(01) VALUE ZEROS.
           05  WS-CURRENT-DATE.
               10  WS-FECHA-ACT.
                   15  WS-FECHA-AA         PIC 9(04).
                   15  WS-FECHA-MM         PIC 9(02).
                   15  WS-FECHA-DD         PIC 9(02).
               10  WS-HORA-ACTUAL.
                   15  WS-HACTUAL-HH       PIC 9(02).
                   15  WS-HACTUAL-MM       PIC 9(02).
                   15  WS-HACTUAL-SS       PIC 9(02).
                   15  WS-HACTUAL-CS       PIC 9(02).
               10  FILLER                  PIC X(05).
      *
           05 WS-FECHA-DESDE-AUX   PIC 9(8)  VALUE ZEROS.
           05 WS-FECHA-HASTA-AUX   PIC 9(8)  VALUE ZEROS.

           05 WS-FECHA             PIC 9(8) VALUE ZEROS.
           05 REDEFINES WS-FECHA.
             10 WS-FECHA-6         PIC 9(8).

           05 ZMENSAJE                   PIC X(40) VALUE SPACES.
           05 WS-FECHA-DESDE       PIC 9(8)  VALUE ZEROS.
           05 REDEFINES WS-FECHA-DESDE.
             10 WS-DD-DESDE        PIC 9(2).
             10 WS-MM-DESDE        PIC 9(2).
             10 WS-AA-DESDE        PIC 9(4).
      *    *
           05 WS-FECHA-HASTA       PIC 9(8) VALUE ZEROS.
           05 REDEFINES WS-FECHA-HASTA.
             10 WS-DD-HASTA        PIC 9(2).
             10 WS-MM-HASTA        PIC 9(2).
             10 WS-AA-HASTA        PIC 9(4).

      ******************************************************************
      *                    AREA  DE  SWITCHES                          *
      ******************************************************************
       01  SW-SWITCHES.
      *
           05  SW-TECLA-PRESS              PIC X(02).
               88  PF-INTRO                           VALUE '00'.
               88  PF-CLEAR                           VALUE '99'.
               88  PF-F01                             VALUE '01'.
               88  PF-F02                             VALUE '02'.
               88  PF-F04                             VALUE '04'.
               88  PF-F06                             VALUE '06'.
               88  PF-F07                             VALUE '07'.
               88  PF-F08                             VALUE '08'.
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
           05  CT-TC8C1220                 PIC X(08)   VALUE 'TC8C1220'.
           05  CT-V19C001                  PIC X(08)   VALUE 'V19C001'.
           05  CT-PROGRAMA                 PIC X(08)   VALUE 'V11C01Z'.
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
           05  CT-OPCP                     PIC 9(04)   VALUE 493.
           05  CT-OPCP1                    PIC 9(04)   VALUE 494.
           05  CT-OPCP2                    PIC 9(04)   VALUE 495.
           05  CT-OPCP3                    PIC 9(04)   VALUE 496.
           05  CT-OPC1                     PIC 9(04)   VALUE 803.
           05  CT-OPC2                     PIC 9(04)   VALUE 883.
           05  CT-OPC3                     PIC 9(04)   VALUE 963.
           05  CT-OPC4                     PIC 9(04)   VALUE 1043.
           05  CT-OPC5                     PIC 9(04)   VALUE 1123.
           05  CT-OPC6                     PIC 9(04)   VALUE 1203.
           05  CT-OPC7                     PIC 9(04)   VALUE 1283.
           05  CT-OPC8                     PIC 9(04)   VALUE 1363.
           05  CT-OPC9                     PIC 9(04)   VALUE 1443.
           05  CT-OPC10                    PIC 9(04)   VALUE 1523.
           05  CT-QR4CDB0                  PIC X(07)   VALUE 'QR4CDB0'.
           05  CT-OPEN                     PIC X(04)   VALUE 'OPEN'.
           05  CT-CLOSE                    PIC X(05)   VALUE 'CLOSE'.
           05  CT-FETCH                    PIC X(05)   VALUE 'FETCH'.
           05  CT-SELECT                   PIC X(06)   VALUE 'SELECT'.
           05  CT-TAB-V1DT001              PIC X(08)   VALUE 'V1DT001'.
           05  CT-SI                       PIC X(01)   VALUE 'S'.
           05  CT-CURSOR                   PIC S9 COMP-3
                                   VALUE -1.

      ******************************************************************
      *                      AREA DE CONTADORES                        *
      ******************************************************************
       01  CN-CONTADORES.
           05  CN-REGISTROS                PIC 9(03)   VALUE ZEROES.
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
       COPY DFHAID.
       COPY QGECABC.
      *
       COPY TCWC1750.
      *
       COPY QCWCI20.
      *
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA.
           COPY QGECCAA.
       COPY V1NC180.
       COPY V1EC001.
      ******************************************************************
      *                                                                *
      *           P R O C E D U R E      D I V I S I O N               *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION USING V1NC180I V1EC001.
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
           SET ADDRESS OF V1NC180I          TO CAA-PTR-COPYIN
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

           MOVE FUNCTION CURRENT-DATE      TO WS-CURRENT-DATE
           MOVE WS-FECHA-ACT               TO WS-FECHA-6
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
           INITIALIZE V1NC180I
      *
           SET CAA-88-ESTADO-CONTIN        TO TRUE
           SET CAA-88-ACCION-TERMINAL      TO TRUE
      *
           MOVE CT-CASO-1                  TO CAA-CASO
           PERFORM ATT-CAMPOS
           MOVE -1                         TO FEC1241L
      *
           .
       2100-ESTADO-INICIO-EXIT.
           EXIT.
      ******************************************************************
      *                        2200-ESTADO-CONTINUACION                *
      ******************************************************************
       2200-ESTADO-CONTINUACION.
      *
           MOVE CAA-TECLA                  TO SW-TECLA-PRESS
      *
           SET CAA-88-ESTADO-CONTIN        TO TRUE
           SET CAA-88-ACCION-TERMINAL      TO TRUE
      *
           EVALUATE TRUE
               WHEN PF-INTRO
                   PERFORM 2300-VALIDA-FECHAS
                      THRU 2300-VALIDA-FECHAS-EXIT

               WHEN PF-CLEAR
                   SET CAA-88-CODTRAN-SIG-ULTI TO TRUE
                   SET CAA-88-ACCION-PROGRAMA  TO TRUE
                   SET CAA-88-ESTADO-INICIO    TO TRUE

               WHEN OTHER
                   SET CAA-88-ACCION-TERMINAL  TO TRUE
                   SET CAA-88-ESTADO-CONTIN    TO TRUE
                   MOVE ME-TECLA-INCORRECTA    TO CAA-COD-ERROR
                   PERFORM 6000-FIN
                      THRU 6000-FIN-EXIT
           END-EVALUATE
      *
           .
       2200-ESTADO-CONTINUACION-EXIT.
           EXIT.
      ******************************************************************
      *                    2300-VALIDA-FECHAS.
      ******************************************************************
       2300-VALIDA-FECHAS.
      *    FECHA DESDE
           IF  FEC1241I = ZEROS OR SPACES OR LOW-VALUES
               MOVE 'V1E0512'              TO CAA-COD-ERROR
               MOVE 'DESDE'                TO CAA-VAR1-ERROR
               MOVE -1                     TO FEC1241L
               PERFORM REINPUT
           END-IF
      *
           INITIALIZE TCWC1750
           MOVE '1'                        TO W175-CDOPCIO
           MOVE FEC1241I                   TO W175-FHGRE1

           CALL CT-TC8C1220 USING TCWC1750

           IF  W175-CDRETORN NOT EQUAL '00'
               MOVE 'V1E0513'              TO CAA-COD-ERROR
               MOVE 'DESDE'                TO CAA-VAR1-ERROR
               MOVE -1                     TO FEC1241L
               PERFORM REINPUT
           END-IF
      *
           MOVE FEC1241I(1:2)              TO WS-FECHA-DESDE-AUX(7:2)
           MOVE FEC1241I(3:2)              TO WS-FECHA-DESDE-AUX(5:2)
           MOVE FEC1241I(5:4)              TO WS-FECHA-DESDE-AUX(1:4)
      *
           IF  WS-FECHA-DESDE-AUX > WS-FECHA-6
               MOVE 'V1E0514'              TO CAA-COD-ERROR
               MOVE 'DESDE'                TO CAA-VAR1-ERROR
               MOVE -1                     TO FEC1241L
               PERFORM REINPUT
           END-IF
      *    FECHA HASTA
           IF  FEC1341I = ZEROS OR SPACES OR LOW-VALUES
               MOVE 'V1E0512'              TO CAA-COD-ERROR
               MOVE 'HASTA'                TO CAA-VAR1-ERROR
               MOVE -1                     TO FEC1341L
               PERFORM REINPUT
           END-IF
      *
           INITIALIZE TCWC1750
           MOVE '1'                        TO W175-CDOPCIO
           MOVE FEC1341I                   TO W175-FHGRE1

           CALL CT-TC8C1220 USING TCWC1750

           IF  W175-CDRETORN NOT EQUAL '00'
               MOVE 'V1E0513'              TO CAA-COD-ERROR
               MOVE 'HASTA'                TO CAA-VAR1-ERROR
               MOVE -1                     TO FEC1341L
               PERFORM REINPUT
           END-IF
      *
           MOVE FEC1341I(1:2)              TO WS-FECHA-HASTA-AUX(7:2)
           MOVE FEC1341I(3:2)              TO WS-FECHA-HASTA-AUX(5:2)
           MOVE FEC1341I(5:4)              TO WS-FECHA-HASTA-AUX(1:4)
      *
           IF  WS-FECHA-DESDE-AUX > WS-FECHA-HASTA-AUX
               MOVE 'V1E0515'              TO CAA-COD-ERROR
               MOVE -1                     TO FEC1341L
               PERFORM REINPUT
           END-IF
      *
           MOVE 'V157'                     TO CAA-CODTRAN-SIG
           SET CAA-88-ACCION-PROGRAMA      TO TRUE
           SET CAA-88-ESTADO-INICIO        TO TRUE
           INITIALIZE CAA-CADENA

           MOVE WS-FECHA-HASTA-AUX         TO V180-FECHA-HASTA
           MOVE WS-FECHA-DESDE-AUX         TO V180-FECHA-DESDE
           MOVE V153-BPBCRP90              TO V180-BPBCRP90
      *
           .
       2300-VALIDA-FECHAS-EXIT.
           EXIT.
      ******************************************************************
      * REINPUT
      ******************************************************************
       REINPUT.
      *
           SET CAA-88-ACCION-TERMINAL TO TRUE
           SET CAA-88-ESTADO-CONTIN   TO TRUE
           INITIALIZE CAA-CADENA

           PERFORM ATT-CAMPOS

           PERFORM 6000-FIN
              THRU 6000-FIN-EXIT
      *
           .
      ******************************************************************
      * ATT-CAMPOS
      ******************************************************************
       ATT-CAMPOS.
      *
           MOVE ATRI-NOP-NUM-BRI-FST       TO FEC1241A
                                              FEC1341A
      *
           .
      ******************************************************************
      *                            6000-FIN                            *
      *   - DEVOLVER EL CONTROL A LA ARQUITECTURA                      *
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
