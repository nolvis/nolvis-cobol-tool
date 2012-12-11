      ******************************************************************
      *                                                                *
      *  PROGRAMA: V12C08Z                                             *
      *                                                                *
      *  FECHA CREACION: 07/01/2008                                    *
      *                                                                *
      *  AUTOR: FACTORIA                                               *
      *                                                                *
      *  APLICACION: MERCADO LIBRE DE DIVISAS. (V1) VENEZUELA.         *
      *                                                                *
      *  DESCRIPCION: PROGRAMA QUE PIDE NOMBRE PARA CONSULTA DE        *
      *               EXPEDIENTE                                       *
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
       PROGRAM-ID.   V12C08Z.
       AUTHOR.       FACTORIA.
       DATE-WRITTEN. 22/01/2008
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
      *
      *                        AREA DE CONTANTES                       *
      ******************************************************************
       01  CT-CONTANTES.
      *                                                                *
           05  CT-TRAN-V168                PIC X(04)   VALUE 'V168'.
           05  CT-TRAN-V175                PIC X(04)   VALUE 'V175'.
           05  CT-CASO1                    PIC X(1)    VALUE '1'.
           05  CT-CASO2                    PIC X(1)    VALUE '2'.
           05  CT-CURSOR                   PIC S9      VALUE -1.
           05  CT-INTRO                    PIC X(02)   VALUE '00'.
           05  CT-BORRA                    PIC X(02)   VALUE '99'.
           05  CT-PF10                     PIC X(02)   VALUE '10'.
      ******************************************************************
      *                AREA DE MENSAJES                                *
      ******************************************************************
       01  ME-MENSAJES-ERROR.
           05  ME-TECLA-INCORRECTA         PIC X(07)   VALUE 'V1E0030'.
      ******************************************************************
      *                    COPYS UTILIZADAS                            *
      ******************************************************************
      *  COPY PARA EL PROGRAMA DE ABEND
       01  WS-QGECABC-01.
           COPY QGECABC.
      * COPY DFHAID
           COPY DFHAID.
      * COPY PARA ATRIBUTOS DE LA PANTALLA
       COPY QCWCI20.
           COPY DFHBMSCA.
      *  COPY PARA ERRORES DB2
           COPY QCWCL20.
      ******* TABLA MERCADO LIBRE DE DIVISA*****************************
           EXEC SQL
               INCLUDE V1GT001
           END-EXEC
      *
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA.
      *
           COPY QGECCAA.
       COPY V1NC08Z.
       COPY V1EC001.
      ******************************************************************
      *                                                                *
      *           P R O C E D U R E      D I V I S I O N               *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION USING V1NC08ZI V1EC001.
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
           GOBACK.
      ******************************************************************
      *                         1000-INICIO                            *
      ******************************************************************
       1000-INICIO.
      *
           SET ADDRESS OF V1NC08ZI         TO CAA-PTR-COPYIN
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
      *                        2100-ESTADO-INICIO                      *
      *   - SE DA CONTROL A LA ARQUITECTURA PARA ENTRAR NOSOTROS       *
      *     MISMOS EN ESTADO CONTINUACION.                             *
      ******************************************************************
       2100-ESTADO-INICIO.
      *
           INITIALIZE V1NC08ZI
      *
           IF  CAA-CASO = '2'
               INITIALIZE CAA-CASO CAA-CASO-CAD
               MOVE NOM-NOMBRE             TO NOM1330I
           END-IF
      *
           SET CAA-88-ESTADO-CONTIN        TO TRUE
           SET CAA-88-ACCION-TERMINAL      TO TRUE
           MOVE ATRI-NOP-BRI-FST           TO NOM1330A
           MOVE -1                         TO NOM1330L
      *
           .
       2100-ESTADO-INICIO-EXIT.
           EXIT.
      ******************************************************************
      *                        2200-ESTADO-CONTINUACION                *
      *     COMPROBAR QUE LA TECLA PULSADA, SE CORRESPONDE CON ALGUNA  *
      *     DE LAS OPCIONES VALIDAS DEL MENU (INTRO(00)).   CUALQUIER  *
      *     OTRO CASO DAREMOS ERROR DE TECLA INCORRECTA.               *
      ******************************************************************
       2200-ESTADO-CONTINUACION.
      *
           INITIALIZE GRP-DATOS-V167
      *
           EVALUATE CAA-TECLA
               WHEN CT-INTRO
                   PERFORM 2300-LLAMAR-V168
                      THRU 2300-LLAMAR-V168-EXIT

               WHEN OTHER
                   MOVE ME-TECLA-INCORRECTA     TO CAA-COD-ERROR
                   MOVE -1                      TO NOM1330L
                   PERFORM REINPUT
           END-EVALUATE
      *
           .
       2200-ESTADO-CONTINUACION-EXIT.
           EXIT.
      ******************************************************************
      *                        2300-LLAMAR-V168                        *
      ******************************************************************
       2300-LLAMAR-V168.
      *
           MOVE CT-TRAN-V168               TO CAA-CODTRAN-SIG
           SET CAA-88-ACCION-PROGRAMA      TO TRUE
           SET CAA-88-ESTADO-INICIO        TO TRUE
           SET CAA-88-CADENA-ANADIR        TO TRUE
           MOVE '2'                        TO CAA-CASO-CAD
           MOVE '1'                        TO CAA-CASO
      *
           MOVE NOM1330I                   TO NOM-NOMBRE
      *
           MOVE CAA-CODTRAN                TO COD-TRANSACCION
      *
           .
       2300-LLAMAR-V168-EXIT.
           EXIT.
      ******************************************************************
      * REINPUT
      ******************************************************************
       REINPUT.
      *
           SET CAA-88-ESTADO-CONTIN        TO TRUE
           SET CAA-88-ACCION-TERMINAL      TO TRUE
           INITIALIZE CAA-CADENA
           MOVE ATRI-NOP-BRI-FST           TO NOM1330A
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
           END-EXEC.
      *
       3000-FIN-EXIT.
           EXIT.
      ******************************************************************
      *                       9999-ABEND-DB2                           *
      *  SE ABENDA LA TAREA CUANDO SE PRODUCE UN ERROR DB2             *
      ******************************************************************
       9999-ABEND-DB2.
      *
           MOVE 'S'                        TO ABC-ABEND
           MOVE 'V12C08Z'                  TO ABC-PROGRAMA
           MOVE SQLCODE                    TO ABC-SQLCODE
           MOVE SQLERRM                    TO ABC-SQLERRM
      *
           EXEC CICS
               LINK PROGRAM ('QG1CABC')
               COMMAREA  (QGECABC)
               NOHANDLE
           END-EXEC
      *
           .
       9999-ABEND-DB2-EXIT.
           EXIT.
      *
