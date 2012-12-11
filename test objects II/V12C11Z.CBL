      ******************************************************************
      *                                                                *
      *  PROGRAMA: V12C11Z                                             *
      *                                                                *
      *  FECHA CREACION: 07/01/2008                                    *
      *                                                                *
      *  AUTOR: FACTORIA                                               *
      *                                                                *
      *  APLICACION: MERCADO LIBRE DE DIVISAS. (BPC) VENEZUELA.        *
      *                                                                *
      *  DESCRIPCION:RESUMEN ESTADISTICO DE OPERACIONES POR SEGEMENTOS *
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
       PROGRAM-ID.   V12C11Z.
       AUTHOR.       FACTORIA.
       DATE-WRITTEN. 07/01/2008
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
      *                     VARIABLES AUXILIARES                       *
      ******************************************************************
       01  WS-VARIABLES-AUXILIARES.
      *
           05  WS-FECHA-DESDE              PIC X(08)    VALUE SPACES.
           05  WS-FECHA-DESDE-R    REDEFINES WS-FECHA-DESDE.
               10  WS-DESDE-ANO            PIC X(04).
               10  WS-DESDE-MES            PIC X(02).
               10  WS-DESDE-DIA            PIC X(02).
      *
           05  WS-FECHA-HASTA              PIC X(08)    VALUE SPACES.
           05  WS-FECHA-HASTA-R  REDEFINES WS-FECHA-HASTA.
               10  WS-HASTA-ANO            PIC X(04).
               10  WS-HASTA-MES            PIC X(02).
               10  WS-HASTA-DIA            PIC X(02).
      *
           05  WS-SEGMENT                  PIC 9(01)   VALUE ZEROS.
           05  WS-CONCEPTO                 PIC 9(03)   VALUE ZEROS.
           05  WS-RIF.
               10  WS-RIF-1                PIC X(01).
               10  WS-RIF-6                PIC X(06).
               10  WS-RIF-8                PIC X(08).
      *
           05  WS-FECHA-DAUX.
               10  WS-FEC-DDIA             PIC X(02)   VALUE ZEROS.
               10  FILLER                  PIC X(01)   VALUE '/'.
               10  WS-FEC-DMES             PIC X(02)   VALUE ZEROS.
               10  FILLER                  PIC X(01)   VALUE '/'.
               10  WS-FEC-DANO             PIC X(04)   VALUE ZEROS.
      *
           05  WS-FECHA-HAUX.
               10  WS-FEC-HDIA             PIC X(02)   VALUE ZEROS.
               10  FILLER                  PIC X(01)   VALUE '/'.
               10  WS-FEC-HMES             PIC X(02)   VALUE ZEROS.
               10  FILLER                  PIC X(01)   VALUE '/'.
               10  WS-FEC-HANO             PIC X(04)   VALUE ZEROS.
           05  WS-MASCARA1                 PIC ZZZ9.
           05  WS-MASCARA2                 PIC ZZ,ZZ9.
           05  WS-MASCARA3                 PIC ZZ,ZZZ,ZZZ,ZZ9.99.
      ******************************************************************
      *                    AREA  DE  SWITCHES                          *
      ******************************************************************
       01  SW-SWITCHES.
      *
           05  SW-FIN-CURSOR               PIC X(01)   VALUE 'N'.
               88  FIN-CURSOR                          VALUE 'S'.
               88  NO-FIN-CURSOR                       VALUE 'N'.
      *
           05  SW-TECLA-PRESS              PIC X(02).
               88  PF-CLEAR                            VALUE '99'.
               88  PF-INTRO                            VALUE '00'.
               88  PF-PF2                              VALUE '02'.
               88  PF-PF10                             VALUE '10'.
      ******************************************************************
      *
      *                        AREA DE CONTANTES                       *
      ******************************************************************
       01  CT-CONSTANTES.
      *                                                                *
           05  CT-1                        PIC 9(01)   VALUE 1.
           05  CT-2                        PIC 9(01)   VALUE 2.
           05  CT-3                        PIC 9(01)   VALUE 3.
           05  CT-4                        PIC 9(01)   VALUE 4.
           05  CT-99                       PIC 9(02)   VALUE 99.
           05  CT-199                      PIC 9(03)   VALUE 199.
           05  CT-200                      PIC 9(03)   VALUE 200.
           05  CT-300                      PIC 9(03)   VALUE 300.
           05  CT-NOT-FND                  PIC S9(03)  VALUE +100.
      *
           05  CT-PROGRAMA                 PIC X(08)   VALUE 'V12C11Z'.
           05  CT-QR4CDB0                  PIC X(08)   VALUE 'QR4CDB0'.
           05  CT-QG1CABC                  PIC X(08)   VALUE 'QG1CABC'.
           05  CT-V1DT001                  PIC X(08)   VALUE 'V1DT001'.
           05  CT-PE9C2010                 PIC X(08)   VALUE 'PE9C2010'.
           05  CT-S                        PIC X(01)   VALUE 'S'.
           05  CT-HELP                     PIC X(02)   VALUE 'HP'.
      *
           05  CT-ERROR-HANDLE             PIC X(20)
                                   VALUE 'ERROR EN CICS HANDLE'.
      ******************************************************************
      *                       AREA DE ACUMULADORES                     *
      *                               CONTADORES
      ******************************************************************
       01  AC-ACUMULADORES.
      *
           05  AC-TOTAL-SEG-ME             PIC 9(05)   VALUE ZEROS.
           05  AC-TOTAL-ME-CR              PIC S9(11)V9(02) COMP-3
                                   VALUE ZEROS.
           05  AC-TOTAL-ME-DB              PIC S9(11)V9(02) COMP-3
                                   VALUE ZEROS.
           05  CN-LEIDOS                   PIC 9(08)   VALUE ZEROS.
      ******************************************************************
      *                AREA DE MENSAJES                                *
      ******************************************************************
       01  ME-MENSAJES-ERROR.
           05  ME-TECLA-INCORRECTA         PIC X(07)   VALUE 'VEE0030'.
      ******************************************************************
      *                AREA DE TABLAS                                  *
      ******************************************************************
       01  TB-TABLAS.
           05  TB-SEGMENTOS OCCURS 3 TIMES.
               10  WS-SEGMENTO-ME          PIC 9(04)   VALUE ZEROS.
               10  WS-SEG-ME-CR            PIC 9(11)V9(02) COMP-3
                                   VALUE ZEROS.
               10  WS-SEG-ME-DB            PIC 9(11)V9(02) COMP-3
                                   VALUE ZEROS.
      *
       01  INDICES.
      *
           05  IN-I                        PIC 9(01)   VALUE ZEROS.
      ******************************************************************
      *                    COPYS UTILIZADAS                            *
      ******************************************************************
       01  WS-PEEC201A.
           COPY PEEC201A.
      *
      **** COPY DFHAID *************************************************
      *
           COPY DFHAID.
      *
      *---- COPY DE VARIABLES DE MANEJO DE ERRORES DB2.
       COPY QGECABC.

       COPY QCWCI20.
      ******************************************************************
      *                    AREA DE INCLUDES                            *
      ******************************************************************
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC
      *
           EXEC SQL
               INCLUDE V1GT001
           END-EXEC
      *
           EXEC SQL
               DECLARE V12C11Z-V1DC0011 CURSOR FOR
               SELECT FEC_OPE,
                      NUM_DOC_PERSONA,
                      TIP_PAGO,
                      COD_CPT_CMP_VTA,
                      IMP_OPE,
                      TIP_OPE
               FROM V1DT001
               WHERE (FEC_OPE BETWEEN :WS-FECHA-DESDE AND
                                      :WS-FECHA-HASTA) AND
      *              (NUM_DOC_PERSONA BETWEEN
      *               'A1' AND 'Z99999999999999') AND
                     NOT (TIP_OPE BETWEEN '5' AND '8')
               ORDER BY FEC_OPE,
                        NUM_DOC_PERSONA
           END-EXEC
      *
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA.
           COPY QGECCAA.
       COPY V1NC060.
       COPY V1EC001.
      ******************************************************************
      *                                                                *
      *           P R O C E D U R E      D I V I S I O N               *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION USING V1NC060I V1EC001.
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
           SET ADDRESS OF V1NC060I         TO CAA-PTR-COPYIN
           SET ADDRESS OF V1EC001          TO CAA-PTRDATA
      *
           EXEC CICS
               IGNORE CONDITION ERROR
           END-EXEC
      *
           INITIALIZE WS-VARIABLES-AUXILIARES
                      AC-ACUMULADORES
           MOVE SPACES                     TO CAA-COD-AVISO1
                                              CAA-COD-AVISO2
                                              CAA-VAR1-ERROR
                                              CAA-VAR2-ERROR
                                              CAA-COD-ERROR
      *
           MOVE CAA-CODTRAN                TO CAA-CODTRAN-SIG
           SET  CAA-88-CONTABLE-NO         TO TRUE
      *
           .
       1000-INICIO-EXIT.
           EXIT.
      ******************************************************************
      *                        2000-PROCESO                            *
      ******************************************************************
       2000-PROCESO.
      *
      *    OJO: PRUEBA
      *    IF TRANS-HELP EQUAL CT-HELP
      *       SET  CAA-88-ESTADO-INICIO    TO TRUE
      *       MOVE SPACES                  TO TRANS-HELP
      *    END-IF
      *
           EVALUATE TRUE
               WHEN CAA-88-ESTADO-INICIO
                   MOVE FEC-FECHA-DESDE    TO WS-FECHA-DESDE
                   MOVE FEC-FECHA-HASTA    TO WS-FECHA-HASTA
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
           SET NO-FIN-CURSOR               TO TRUE
           PERFORM 2500-ABRIR-CURSOR
              THRU 2500-ABRIR-CURSOR-EXIT
      *
           PERFORM 2600-LEER-CURSOR
              THRU 2600-LEER-CURSOR-EXIT
      *
           PERFORM 2300-PROCESO-DATOS
              THRU 2300-PROCESO-DATOS-EXIT
             UNTIL FIN-CURSOR
      *
           MOVE ZEROES                     TO IN-I
           PERFORM VARYING IN-I FROM 1 BY 1
           UNTIL IN-I > 3
               ADD  WS-SEGMENTO-ME(IN-I)       TO AC-TOTAL-SEG-ME
               ADD  WS-SEG-ME-CR(IN-I)         TO AC-TOTAL-ME-CR
               ADD  WS-SEG-ME-DB(IN-I)         TO AC-TOTAL-ME-DB
           END-PERFORM
      *
           PERFORM 2700-LLENA-MAPA
              THRU 2700-LLENA-MAPA-EXIT

           PERFORM CERRAR-CURSOR
              THRU CERRAR-CURSOR-EXIT
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
           MOVE CAA-TECLA                  TO SW-TECLA-PRESS
           SET  CAA-88-ACCION-PROGRAMA     TO TRUE
           SET  CAA-88-ESTADO-INICIO       TO TRUE
      *
           EVALUATE TRUE
               WHEN PF-INTRO
               WHEN PF-CLEAR
                   SET CAA-88-CODTRAN-SIG-ULTI TO TRUE
                   SET CAA-88-ACCION-PROGRAMA  TO TRUE
                   SET CAA-88-ESTADO-INICIO    TO TRUE

               WHEN PF-PF10
      *            OJO: PRUEBA
      *            MOVE CT-HELP               TO TRANS-HELP

                   MOVE 'V175'                TO CAA-CODTRAN-SIG
                   SET CAA-88-ACCION-PROGRAMA TO TRUE
                   SET CAA-88-ESTADO-INICIO   TO TRUE
                   SET  CAA-88-CADENA-ANADIR  TO TRUE

               WHEN OTHER
                   SET CAA-88-ACCION-TERMINAL TO TRUE
                   SET CAA-88-ESTADO-CONTIN   TO TRUE
                   MOVE ME-TECLA-INCORRECTA   TO CAA-COD-ERROR
                   PERFORM ATT-CAMPOS
           END-EVALUATE
      *
           .
       2200-ESTADO-CONTINUACION-EXIT.
           EXIT.
      ******************************************************************
      * 2300-PROCESO-DATOS
      ******************************************************************
       2300-PROCESO-DATOS.
      *
           ADD CT-1                        TO CN-LEIDOS
      *
           PERFORM 2310-SEGMENTO
      *
           IF  WS-SEGMENT EQUAL CT-1 OR CT-2 OR CT-3
               ADD 1                       TO WS-SEGMENTO-ME(WS-SEGMENT)
               MOVE COD-CPT-CMP-VTA        TO WS-CONCEPTO

               EVALUATE TRUE
                   WHEN (TIP-PAGO EQUAL 1 OR 2 OR 3) AND
                        (WS-CONCEPTO > 99 AND WS-CONCEPTO < 200)
                       ADD IMP-OPE         TO WS-SEG-ME-DB(WS-SEGMENT)

                   WHEN (TIP-PAGO EQUAL CT-1 OR CT-2 OR
                         CT-3 OR CT-4) AND
                        (WS-CONCEPTO > 199 AND WS-CONCEPTO < 300)
                       ADD IMP-OPE         TO WS-SEG-ME-CR(WS-SEGMENT)
               END-EVALUATE
           END-IF
      *
           PERFORM 2600-LEER-CURSOR
              THRU 2600-LEER-CURSOR-EXIT
      *
           .
       2300-PROCESO-DATOS-EXIT.
           EXIT.
      ******************************************************************
      * 2310-SEGMENTO
      ******************************************************************
       2310-SEGMENTO.
      *
           MOVE NUM-DOC-PERSONA            TO WS-RIF
      *
           INITIALIZE PEEC201A
           MOVE '0102'                     TO PEEC201A-PECDGENT
           MOVE SPACES                     TO PEEC201A-PENUMPER
           MOVE WS-RIF-1                   TO PEEC201A-PETIPDOC
           MOVE '000'                      TO PEEC201A-PENUMDOC(1:3)
           MOVE WS-RIF-8                   TO PEEC201A-PENUMDOC(4:8)
           MOVE '01'                       TO PEEC201A-PESECDOC
           MOVE '00'                       TO PEEC201A-IND-GRUPO
      *
           CALL CT-PE9C2010 USING WS-PEEC201A
      *
           EVALUATE PEEC201A-RETORNO
               WHEN '00'
                   MOVE PEEC201A-PEARENEG(1:1) TO WS-SEGMENT

               WHEN '20'
      *            NO EXISTE CLIENTE
                   MOVE ZEROS              TO WS-SEGMENT

               WHEN OTHER
                   MOVE PEEC201A-COD-ERROR TO CAA-COD-ERROR
                   MOVE ZEROS              TO WS-SEGMENT
           END-EVALUATE
      *
           .
       2310-SEGMENTO-EXIT.
           EXIT.
      ******************************************************************
      * 2500-ABRIR-CURSOR
      ******************************************************************
       2500-ABRIR-CURSOR.
      *
           EXEC SQL
              OPEN V12C11Z-V1DC0011
           END-EXEC
      *
           IF  SQLCODE NOT EQUAL ZEROS
               MOVE CT-V1DT001             TO ABC-OBJETO-ERROR
               PERFORM 9999-ABEND-DB2
                  THRU 9999-ABEND-DB2-EXIT
           END-IF
      *
           .
       2500-ABRIR-CURSOR-EXIT.
           EXIT.
      ******************************************************************
      * 2600-LEER-CURSOR
      ******************************************************************
       2600-LEER-CURSOR.
      *
           EXEC SQL
               FETCH V12C11Z-V1DC0011
               INTO  :FEC-OPE,
                     :NUM-DOC-PERSONA,
                     :TIP-PAGO,
                     :COD-CPT-CMP-VTA,
                     :IMP-OPE,
                     :TIP-OPE
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN ZEROS
                   CONTINUE

               WHEN CT-NOT-FND
                   SET FIN-CURSOR          TO TRUE

               WHEN OTHER
                   MOVE CT-V1DT001         TO ABC-OBJETO-ERROR
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *
           .
       2600-LEER-CURSOR-EXIT.
           EXIT.
      ******************************************************************
      * CERRAR-CURSOR
      ******************************************************************
       CERRAR-CURSOR.
      *
           EXEC SQL
              CLOSE V12C11Z-V1DC0011
           END-EXEC
      *
           IF  SQLCODE NOT EQUAL ZEROS
               MOVE CT-V1DT001             TO ABC-OBJETO-ERROR
               PERFORM 9999-ABEND-DB2
                  THRU 9999-ABEND-DB2-EXIT
           END-IF
      *
           .
       CERRAR-CURSOR-EXIT.
           EXIT.
      ******************************************************************
      * 2700-LLENA-MAPA
      ******************************************************************
       2700-LLENA-MAPA.
      *
           PERFORM 2800-MUESTRA-FECHA
              THRU 2800-MUESTRA-FECHA-EXIT
      *
           MOVE WS-SEGMENTO-ME(CT-1)       TO WS-MASCARA1
           MOVE WS-MASCARA1                TO TOT1222I
           MOVE WS-SEGMENTO-ME(CT-2)       TO WS-MASCARA1
           MOVE WS-MASCARA1                TO TOT1240I
           MOVE WS-SEGMENTO-ME(CT-3)       TO WS-MASCARA1
           MOVE WS-MASCARA1                TO TOT1258I
           MOVE AC-TOTAL-SEG-ME            TO WS-MASCARA2
           MOVE WS-MASCARA2                TO TOT1274I
      *
           MOVE WS-SEG-ME-DB(CT-1)         TO WS-MASCARA3
           MOVE WS-MASCARA3                TO TOTA139I

           MOVE WS-SEG-ME-DB(CT-2)         TO WS-MASCARA3
           MOVE WS-MASCARA3                TO TOT1327I

           MOVE WS-SEG-ME-DB(CT-3)         TO WS-MASCARA3
           MOVE WS-MASCARA3                TO TOT1345I

           MOVE AC-TOTAL-ME-DB             TO WS-MASCARA3
           MOVE WS-MASCARA3                TO TOT1363I
      *
           MOVE WS-SEG-ME-CR(CT-1)         TO WS-MASCARA3
           MOVE WS-MASCARA3                TO TOTA149I

           MOVE WS-SEG-ME-CR(CT-2)         TO WS-MASCARA3
           MOVE WS-MASCARA3                TO TOT1427I

           MOVE WS-SEG-ME-CR(CT-3)         TO WS-MASCARA3
           MOVE WS-MASCARA3                TO TOT1445I

           MOVE AC-TOTAL-ME-CR             TO WS-MASCARA3
           MOVE WS-MASCARA3                TO TOT1463I
      *
           .
       2700-LLENA-MAPA-EXIT.
           EXIT.
      ******************************************************************
      * 2800-MUESTRA-FECHA
      ******************************************************************
       2800-MUESTRA-FECHA.
      *
           MOVE WS-DESDE-DIA               TO WS-FEC-DDIA
           MOVE WS-DESDE-MES               TO WS-FEC-DMES
           MOVE WS-DESDE-ANO               TO WS-FEC-DANO
           MOVE WS-FECHA-DAUX              TO FECH729I
      *
           MOVE WS-HASTA-DIA               TO WS-FEC-HDIA
           MOVE WS-HASTA-MES               TO WS-FEC-HMES
           MOVE WS-HASTA-ANO               TO WS-FEC-HANO
           MOVE WS-FECHA-HAUX              TO FECH749I
      *
           .
       2800-MUESTRA-FECHA-EXIT.
           EXIT.
      ******************************************************************
      * ATT-CAMPOS
      ******************************************************************
       ATT-CAMPOS.
      *
           MOVE ATRI-PRO-BRI-FST           TO FECH729A
                                              FECH749A
                                              TOT1222A
                                              TOT1240A
                                              TOT1258A
                                              TOT1274A
                                              TOTA139A
                                              TOT1327A
                                              TOT1345A
                                              TOT1363A
                                              TOTA149A
                                              TOT1427A
                                              TOT1445A
                                              TOT1463A
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
           EXIT.
      ******************************************************************
      *            9999-ABEND-DB2                                      *
      *   LLAMADA A LA RUTINA DE ABEND PARA EL DB2                     *
      ******************************************************************
       9999-ABEND-DB2.
      *
           MOVE  CT-S                      TO ABC-ABEND
           MOVE  CT-PROGRAMA               TO ABC-PROGRAMA
           MOVE  SQLCODE                   TO ABC-SQLCODE
           MOVE  SQLERRM                   TO ABC-SQLERRM
      *
           PERFORM 9999-LINK-ABEND
              THRU 9999-LINK-ABEND-EXIT
      *
           .
       9999-ABEND-DB2-EXIT.
           EXIT.
      ******************************************************************
      *                          9999-ABEND-CICS                       *
      *  SE ABENDA LA TAREA CUANDO SE PRODUCE UN ERROR CICS NO DESEADO *
      ******************************************************************
       9999-ABEND-CICS.
      *
           MOVE CT-S                       TO ABC-ABEND
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
               LINK PROGRAM(CT-QG1CABC)
               COMMAREA (QGECABC)
               NOHANDLE
           END-EXEC
      *
           .
       9999-LINK-ABEND-EXIT.
           EXIT.
      *
