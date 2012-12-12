      ******************************************************************
      *                                                                *
      *  PROGRAMA: V12C06Z                                             *
      *                                                                *
      *  FECHA CREACION: 06/01/2007                                    *
      *                                                                *
      *  AUTOR: FACTORIA                                               *
      *                                                                *
      *  APLICACION: V1 - COMPRA Y VENTA DE DIVISAS                    *
      *                                                                *
      *  DESCRIPCION: PERMITE MOSTRAR LOS CLIENTE QUE SE ENCUENTRA EN EL
      *               ARCHIVOS BPB-SEGURIDAD BANCARIA PARA UNA DETERMINA
      *               FECHA .                                          *
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
       PROGRAM-ID.   V12C06Z.
       AUTHOR.       FACTORIA.
       DATE-WRITTEN. 06/01/2007
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
      *
       01  WS-CANTIDAD                  PIC S9(6)V COMP-3 VALUE ZEROES.
      ******************************************************************
      *
      *                        AREA DE CONTANTES                       *
      ******************************************************************
       01  CT-CONTANTES.
      *                                                                *
           05  CT-CURSOR                   PIC S9      VALUE -1.
           05  CT-INTRO                    PIC X(02)   VALUE '00'.
           05  CT-PE9C2010                 PIC X(08)   VALUE 'PE9C2010'.
      ******************************************************************
      *                VARIABLES AUXILIARES                            *
      ******************************************************************
       01  VARIABLES.
      *
           05  RIF-VIG                     PIC  X(02)  VALUE SPACES.
      ******************************************************************
      *                AREA DE MENSAJES                                *
      ******************************************************************
       01  ME-MENSAJES-ERROR.
           05  ME-TECLA-INCORRECTA         PIC X(07)   VALUE 'V1E0030'.
           05  ME-NACIONALIDAD-INC         PIC X(07)   VALUE 'V1E0066'.
           05  ME-CELULA-INCORRECT         PIC X(07)   VALUE 'V1E0067'.
           05  ME-RIF-NO-EXITE             PIC X(07)   VALUE 'V1E0068'.
           05  ME-NO-OPERACIONES           PIC X(07)   VALUE 'V1E0069'.
      ******************************************************************
      *                    AREA  DE  SWITCHES                          *
      ******************************************************************
       01  SW-SWITCHES.
      *
           05  SW-PF-PRO                   PIC 9(02).
               88  PF-PF2                              VALUE 02.
               88  PF-PF4                              VALUE 04.
               88  PF-PF5                              VALUE 05.
               88  PF-PF8                              VALUE 08.
               88  PF-PF10                             VALUE 10.
               88  PF-BORRA                            VALUE 99.
               88  PF-INTRO                            VALUE 00.
      *  COPY PARA EL PROGRAMA DE ABEND
       01  WS-QGECABC-01.
           COPY QGECABC.
      *
       01  WS-PEEC201A.
           COPY PEEC201A.
      *
           COPY QCWCI20.
      *  COPY PARA ERRORES DB2
           COPY QCWCL20.
      *
      ******* TABLA MERCADO LIBRE DE DIVISA*****************************
      *
           EXEC SQL
               INCLUDE V1GT001
           END-EXEC
      *
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA.
           COPY QGECCAA.
       COPY V1NC06Z.
       COPY V1EC001.
      ******************************************************************
      *                                                                *
      *           P R O C E D U R E      D I V I S I O N               *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION USING V1NC06ZI V1EC001.
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
           SET ADDRESS OF V1NC06ZI         TO CAA-PTR-COPYIN
           SET ADDRESS OF V1EC001          TO CAA-PTRDATA
      *
           EXEC CICS
               IGNORE CONDITION ERROR
           END-EXEC
      *
           MOVE CAA-CODTRAN                TO CAA-CODTRAN-SIG
      *
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
      ******************************************************************
       2100-ESTADO-INICIO.
      *
           INITIALIZE V1NC06ZI
      *
           IF  CAA-CASO = '2'
               MOVE COD-RIF(1:1)           TO NACIONAI
               MOVE COD-RIF(2:14)          TO NUMRIFI
           END-IF
      *
           SET CAA-88-ESTADO-CONTIN        TO TRUE
           SET CAA-88-ACCION-TERMINAL      TO TRUE
           INITIALIZE CAA-CASO CAA-CASO-CAD
           PERFORM ATT-CAMPOS
           MOVE -1                         TO NACIONAL
      *
           .
       2100-ESTADO-INICIO-EXIT.
           EXIT.
      ******************************************************************
      *                        2200-ESTADO-CONTINUACION                *
      ******************************************************************
       2200-ESTADO-CONTINUACION.
      *
           IF  CAA-TECLA NOT EQUAL CT-INTRO
               MOVE ME-TECLA-INCORRECTA    TO CAA-COD-ERROR
               PERFORM REINPUT
           END-IF
      *
           IF  NOT (NACIONAI = 'V' OR 'E' OR 'J' OR 'W' OR
                               'G' OR 'P')
               MOVE ME-NACIONALIDAD-INC    TO CAA-COD-ERROR
      *        LA NACIONALIDAD DEBE SER "V , E , J , W , G , P"
               MOVE -1                     TO NACIONAL
               PERFORM REINPUT
           END-IF
      *
           IF  NUMRIFI = ZEROES OR SPACES OR LOW-VALUES
               MOVE 'V1E0510'              TO CAA-COD-ERROR
      *        '** DEBE COLOCAR EL NUMERO DEL RIF **'.
               MOVE -1                     TO NUMRIFL
               PERFORM REINPUT
           END-IF
      *
           PERFORM BUSCA-RIF
      *
           MOVE 'V161'                     TO CAA-CODTRAN-SIG
           SET CAA-88-ACCION-PROGRAMA      TO TRUE
           SET CAA-88-ESTADO-INICIO        TO TRUE
           SET CAA-88-CADENA-ANADIR        TO TRUE
           MOVE '2'                        TO CAA-CASO-CAD
           MOVE NACIONAI                   TO COD-RIF(1:1)
           MOVE NUMRIFI                    TO COD-RIF(2:14)
      *
           .
       2200-ESTADO-CONTINUACION-EXIT.
           EXIT.
      ******************************************************************
      * BUSCA-RIF
      ******************************************************************
       BUSCA-RIF.
      *
           INITIALIZE PEEC201A
           MOVE '0102'                     TO PEEC201A-PECDGENT
           MOVE SPACES                     TO PEEC201A-PENUMPER
           MOVE NACIONAI                   TO PEEC201A-PETIPDOC
           MOVE '000'                      TO PEEC201A-PENUMDOC(1:3)
           MOVE NUMRIFI(7:8)               TO PEEC201A-PENUMDOC(4:8)
           MOVE '01'                       TO PEEC201A-PESECDOC
           MOVE '00'                       TO PEEC201A-IND-GRUPO
      *
           CALL CT-PE9C2010 USING WS-PEEC201A
      *
           EVALUATE PEEC201A-RETORNO
               WHEN '00'
                   MOVE 'S'                TO RIF-ENC

               WHEN '20'
                   MOVE 'N'                TO RIF-ENC

               WHEN OTHER
                   MOVE 'N'                TO RIF-ENC
                   MOVE PEEC201A-COD-ERROR TO CAA-COD-ERROR
      *            PERFORM REINPUT
           END-EVALUATE

      *    DONE: COMENTADO POR NOLVIS
      *    MOVE NACIONAI             TO NUM-DOC-PERSONA(1:1)
      *    MOVE NUMRIFI              TO NUM-DOC-PERSONA(2:14)
      *    MOVE SW-ENCONTRADO        TO RIF-ENC
      *
      *    EXEC SQL
      *        SELECT COUNT(*)
      *        INTO :WS-CANTIDAD
      *        FROM V1DT001
      *        WHERE NUM_DOC_PERSONA = :NUM-DOC-PERSONA
      *    END-EXEC
      *
      *    MOVE SQLCODE                    TO DB2-RETURN-CDE
      *    EVALUATE TRUE
      *        WHEN DB2-OK
      *            CONTINUE
      *
      *        WHEN OTHER
      *            INITIALIZE QGECABC
      *            MOVE 'V1DT001'          TO ABC-OBJETO-ERROR
      *            PERFORM 9999-ABEND-DB2
      *               THRU 9999-ABEND-DB2-EXIT
      *    END-EVALUATE
      *
      *    IF  WS-CANTIDAD = 0
      *        IF  NO-ENCONTRADO
      *            MOVE ME-RIF-NO-EXITE    TO CAA-COD-ERROR
      *        ELSE
      *            MOVE ME-NO-OPERACIONES  TO CAA-COD-ERROR
      *        END-IF
      *       PERFORM 3000-FIN
      *          THRU 3000-FIN-EXIT
      *    END-IF
      *
           .
      ******************************************************************
      * REINPUT
      ******************************************************************
       REINPUT.
      *
           SET CAA-88-ACCION-TERMINAL TO TRUE
           SET CAA-88-ESTADO-CONTIN   TO TRUE
           INITIALIZE CAA-CADENA

           PERFORM ATT-CAMPOS

           PERFORM 3000-FIN
              THRU 3000-FIN-EXIT
      *
           .
      ******************************************************************
      * ATT-CAMPOS
      ******************************************************************
       ATT-CAMPOS.
      *
           MOVE ATRI-NOP-BRI-FST           TO NACIONAA
           MOVE ATRI-NOP-NUM-BRI-FST       TO NUMRIFA
      *
           .
      ******************************************************************
      *                            3000-FIN                            *
      ******************************************************************
       3000-FIN.
      *
           MOVE CAA-CODTRAN                TO COD-TRANSACCION
           MOVE '-'                        TO SWPG42A
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
           MOVE 'V12C06Z'                  TO ABC-PROGRAMA
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
