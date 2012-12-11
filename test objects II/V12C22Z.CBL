      ******************************************************************
      *                                                                *
      *  PROGRAMA: V12C22Z                                             *
      *                                                                *
      *  FECHA CREACION: 03/01/2008                                    *
      *                                                                *
      *  AUTOR: FACTORIA                                               *
      *                                                                *
      *  APLICACION: MERCADO LIBRE DE DIVISAS. (V1) VENEZUELA.         *
      *                                                                *
      *  DESCRIPCION: PERMITE CAPTAR LA FECHA DE OPERACION CON LA      *
      *               QUE SE MOSTRARAN LOS CLIENTES QUE SE ENCUENTRA EN*
      *               EL ARCHIVO BPB-SEGURIDAD BANCARIA PARA DICHA     *
      *               FECHA                                            *
      *                                                                *
      ******************************************************************
      ******************************************************************
      *                                                                *
      *         I D E N T I F I C A T I O N   D I V I S I O N          *
      *                                                                *
      *             INFORMACION GENERAL SOBRE EL PROGRAMA              *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.   V12C22Z.
       AUTHOR.       FACTORIA.
       DATE-WRITTEN. 03/01/2008
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
      *
           05  WS-CURRENT-DATE.
               10  WS-FEC-ACTUAL           PIC 9(08).
               10  FILLER                  PIC X(08).
               10  FILLER                  PIC X(05).
      *
           05  WS-FECHA-AUX                PIC 9(08).
           05  WS-TABLA                    PIC X(08).
      ******************************************************************
      *                    AREA DE CONTANTES                           *
      ******************************************************************
       01  CT-CONTANTES.
      *                                                                *
           05  CT-PROGRAMA                 PIC X(07)   VALUE 'V12C22Z'.
           05  CT-TC8C1220                 PIC X(08)   VALUE 'TC8C1220'.
           05  CT-QG1CABC                  PIC X(08)   VALUE 'QG1CABC'.
           05  CT-SI                       PIC X(01)   VALUE 'S'.
           05  CT-INTRO                    PIC X(02)   VALUE '00'.
      ******************************************************************
      *                AREA DE MENSAJES                                *
      ******************************************************************
       01  ME-MENSAJES-ERROR.
           05  ME-TECLA-INCORRECTA         PIC X(07)   VALUE 'V1E0030'.
           05  ME-INGRESAR-FECHA           PIC X(07)   VALUE 'V1E0063'.
           05  ME-FECHA-ERRADA             PIC X(07)   VALUE 'V1E0064'.
           05  ME-FECHA-NO-DEL-DIA         PIC X(07)   VALUE 'V1E0065'.
           05  ME-ERROR-RUTINA             PIC X(27)
                                   VALUE 'ERROR EN LA RUTINA TC8C1220'.
      ******************************************************************
      *                       COPYS UTILIZADAS                         *
      ******************************************************************
       COPY TCWC1750.
      * COPY DFHAID
           COPY DFHAID.
      * COPY PARA ATRIBUTOS DE LA PANTALLA
           COPY QCWCI20.
      * COPY DE LA RUTINA QG1CABC (ABENDS)
           COPY QGECABC.
      * COPY DE VARIABLES DE MANEJO ERRORES NO DB2.
           COPY QRWCWAB.
      ******************************************************************
      *                       AREA DE TABLAS                           *
      ******************************************************************
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC
      *
           EXEC SQL
               INCLUDE BPBB0090
           END-EXEC
      *
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA.
      *
           COPY QGECCAA.
       COPY V1NC02Z.
       COPY V1EC001.
      ******************************************************************
      *                                                                *
      *           P R O C E D U R E      D I V I S I O N               *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION USING V1NC02ZI V1EC001.
      *
       MAINLINE.
      *
           PERFORM 1000-INICIO
              THRU 1000-INICIO-EXIT
      *
           PERFORM 2000-PROCESO
              THRU 2000-PROCESO-EXIT
      *
           PERFORM 3000-FIN
              THRU 3000-FIN-EXIT
      *
           GOBACK
           .
      ******************************************************************
      *                         1000-INICIO                            *
      ******************************************************************
       1000-INICIO.
      *
           SET ADDRESS OF V1NC02ZI         TO CAA-PTR-COPYIN
           SET ADDRESS OF V1EC001          TO CAA-PTRDATA
      *
           EXEC CICS
               IGNORE CONDITION ERROR
           END-EXEC
      *
           MOVE CAA-CODTRAN                TO CAA-CODTRAN-SIG
           SET CAA-88-CONTABLE-NO          TO TRUE
      *
           MOVE SPACES                     TO CAA-COD-AVISO1
                                              CAA-COD-AVISO2
                                              CAA-VAR1-ERROR
                                              CAA-VAR2-ERROR
                                              CAA-COD-ERROR
      *
           MOVE FUNCTION CURRENT-DATE      TO WS-CURRENT-DATE
      *
           .
       1000-INICIO-EXIT.
           EXIT.
      ******************************************************************
      *                        2000-PROCESO                            *
      ******************************************************************
       2000-PROCESO.
      *
           EVALUATE TRUE
               WHEN CAA-88-ESTADO-INICIO
                   PERFORM 2100-ESTADO-INICIO
                      THRU 2100-ESTADO-INICIO-EXIT

               WHEN CAA-88-ESTADO-CONTIN
                   PERFORM 2200-ESTADO-CONTINUACION
                      THRU 2200-ESTADO-CONTINUACION-EXIT
           END-EVALUATE
      *
           .
       2000-PROCESO-EXIT.
           EXIT.
      ******************************************************************
      *                     2100-ESTADO-INICIO                         *
      ******************************************************************
       2100-ESTADO-INICIO.
      *
           INITIALIZE V1NC02ZI
      *
           SET CAA-88-ESTADO-CONTIN        TO TRUE
           SET CAA-88-ACCION-TERMINAL      TO TRUE
      *
           MOVE ATRI-NOP-NUM-BRI           TO FECHAA
           MOVE -1                         TO FECHAL
      *
           .
       2100-ESTADO-INICIO-EXIT.
           EXIT.
      ******************************************************************
      * 2200-ESTADO-CONTINUACION
      ******************************************************************
       2200-ESTADO-CONTINUACION.
      *
           IF  CAA-TECLA NOT = CT-INTRO
               MOVE ME-TECLA-INCORRECTA    TO CAA-COD-ERROR
               MOVE -1                     TO FECHAL
               PERFORM REINPUT
           END-IF
      *
           MOVE ATRI-NOP-NUM-BRI           TO FECHAA

           IF  FECHAI EQUAL ZEROES OR SPACES OR LOW-VALUES
               MOVE ME-INGRESAR-FECHA      TO CAA-COD-ERROR
               MOVE -1                     TO FECHAL
               PERFORM REINPUT
           END-IF
      *
           INITIALIZE TCWC1750
           MOVE '1'                        TO W175-CDOPCIO
           MOVE FECHAI                     TO W175-FHGRE1
           CALL CT-TC8C1220 USING TCWC1750
           IF  W175-CDRETORN NOT = '00'
               MOVE ME-FECHA-ERRADA        TO CAA-COD-ERROR
               MOVE -1                     TO FECHAL
               PERFORM REINPUT
           END-IF
      *
           MOVE FECHAI(1:2) TO WS-FECHA-AUX(7:2)
           MOVE FECHAI(3:2) TO WS-FECHA-AUX(5:2)
           MOVE FECHAI(5:4) TO WS-FECHA-AUX(1:4)
      *
           IF  WS-FECHA-AUX GREATER OR EQUAL WS-FEC-ACTUAL
               MOVE ME-FECHA-NO-DEL-DIA    TO CAA-COD-ERROR
               MOVE -1                     TO FECHAL
               PERFORM REINPUT
           END-IF
      *
           PERFORM 2700-LLAMAR-V152
              THRU 2700-LLAMAR-V152-EXIT
      *
           .
       2200-ESTADO-CONTINUACION-EXIT.
           EXIT.
      ******************************************************************
      * 2700-LLAMAR-V152
      ******************************************************************
       2700-LLAMAR-V152.
      *
           MOVE 'V152'                     TO CAA-CODTRAN-SIG
           SET CAA-88-ACCION-PROGRAMA      TO TRUE
           SET CAA-88-ESTADO-INICIO        TO TRUE
           INITIALIZE CAA-CADENA
                      CAA-CASO-CAD
      *
           MOVE CAA-CODTRAN                TO COD-TRANSACCION
           MOVE WS-FECHA-AUX               TO FEC-BPBFIN90
      *
           .
       2700-LLAMAR-V152-EXIT.
           EXIT.
      ******************************************************************
      * REINPUT
      ******************************************************************
       REINPUT.
      *
           SET CAA-88-ACCION-TERMINAL TO TRUE
           SET CAA-88-ESTADO-CONTIN   TO TRUE
           INITIALIZE CAA-CADENA
           PERFORM 3000-FIN
              THRU 3000-FIN-EXIT
      *
           .
      ******************************************************************
      * 3000-FIN
      ******************************************************************
       3000-FIN.
      *
           EXEC CICS
               RETURN
           END-EXEC
      *
           .
       3000-FIN-EXIT.
      ******************************************************************
      * 9999-ABEND-DB2                                                 *
      ******************************************************************
       9999-ABEND-DB2.
      *
           INITIALIZE                         QGECABC
           MOVE CT-SI                      TO ABC-ABEND
           MOVE CT-PROGRAMA                TO ABC-PROGRAMA
           MOVE WS-TABLA                   TO ABC-OBJETO-ERROR
           MOVE SQLCODE                    TO ABC-SQLCODE
           MOVE SQLERRM                    TO ABC-SQLERRM
      *
           EXEC CICS
                LINK PROGRAM (CT-QG1CABC)
                COMMAREA (QGECABC)
           END-EXEC
      *
           .
       9999-ABEND-DB2-EXIT.
           EXIT.
      ******************************************************************
       COPY QRWCPAB.
