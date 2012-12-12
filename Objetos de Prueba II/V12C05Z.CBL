      ******************************************************************
      *                                                                *
      *  PROGRAMA: V12C05Z                                             *
      *                                                                *
      *  FECHA CREACION: 10/01/2008                                    *
      *                                                                *
      *  AUTOR: FACTORIA                                               *
      *                                                                *
      *  APLICACION: MERCADO LIBRE DE DIVISAS. (V1) VENEZUELA.         *
      *                                                                *
      *  DESCRIPCION: CONSULTA POR NOMBRE                              *
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
       PROGRAM-ID.   V12C05Z.
       AUTHOR.       FACTORIA.
       DATE-WRITTEN. 10/01/2008
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
      *    05  WS-IMP-MONTO-TOTAL          PIC S9(13)V9(02) COMP-3.
      *
           05  WS-GTS.
               10  FILLER                  PIC X(4)    VALUE '+GTS'.
               10  WS-GTS-SUFI             PIC X(4)    VALUE SPACES.
      *
           05  WS-GTS-DATOS                PIC X(80)   VALUE SPACES.
      *
           05  WS-GTS-FILA.
               10  WS-GTS-OPCION           PIC X(01)   VALUE SPACES.
               10  FILLER                  PIC X(09)   VALUE SPACES.
               10  WS-GTS-NOMBRE           PIC X(50)   VALUE SPACES.
               10  FILLER                  PIC X(02)   VALUE SPACES.
               10  WS-GTS-RIF              PIC X(15)   VALUE SPACES.
      *
           05  WS-IDF-CELULA-RIF           PIC X(15)   VALUE SPACES.
           05  WS-NUM-CONT                 PIC X(06)   VALUE SPACE.
           05  WS-NOM-NOMBRE               PIC X(16)   VALUE SPACE.
           05  WS-NOM-COMPLETO             PIC X(35)   VALUE SPACE.
      ******************************************************************
      *                    AREA  DE  SWITCHES                          *
      ******************************************************************
       01  SW-SWITCHES.
      *
           05  SW-FIN-CURSOR               PIC X(01)   VALUE 'N'.
               88  FIN-CURSOR                          VALUE 'S'.
               88  NO-FIN-CURSOR                       VALUE 'N'.
      *
           05  SW-PF-PRO                   PIC 9(02).
               88  PF-PF2                              VALUE 02.
               88  PF-PF4                              VALUE 04.
               88  PF-PF5                              VALUE 05.
               88  PF-PF8                              VALUE 08.
               88  PF-PF10                             VALUE 10.
               88  PF-BORRA                            VALUE 99.
               88  PF-INTRO                            VALUE 00.
      *
           05  SW-FIN-COLA                 PIC X(01)   VALUE SPACES.
               88  FIN-COLA                            VALUES 'S'.
               88  NO-FIN-COLA                         VALUES 'N'.
      ******************************************************************
      *                        AREA DE CONTANTES                       *
      ******************************************************************
       01  CT-CONTANTES.
      *
           05  CT-PROGRAMA                 PIC X(08)   VALUE 'V12C05Z'.
           05  CT-CASO1                    PIC X(01)   VALUE '1'.
           05  CT-CASO2                    PIC X(01)   VALUE '2'.
           05  CT-SI                       PIC X(01)   VALUE 'S'.
           05  CT-TAB-V1DT001              PIC X(08)   VALUE 'V1DT001'.
           05  CT-CONSULTAR                PIC X(01)   VALUE 'X'.
           05  CT-INTRO                    PIC X(02)   VALUE '00'.
           05  CT-TRAN-RESUMEN             PIC X(04)   VALUE 'V161'.
           05  CT-TRAN-AYUDA               PIC X(04)   VALUE 'V175'.
           05  CT-INCRE-1                  PIC 9(01)   VALUE 1.
           05  CT-IND-DATOS                PIC 9(01)   VALUE 4.
           05  CT-UNO                      PIC 9(01)   VALUE 1.
           05  CT-TOTAL                    PIC X(01)   VALUE 'T'.
      *
           05  CT-ERROR-WRITEQ             PIC X(20)
                                   VALUE 'ERROR EN CICS WRITQ'.
           05  CT-ERROR-DELETEQ            PIC X(20)
                                   VALUE 'ERROR CICS DELETEQ'.
           05  CT-ERROR-READQ              PIC X(20)
                                   VALUE 'ERROR CICS READQ'.
      *
           05  CT-NUM-REGISTROS            PIC 9(02)   VALUE 50.
           05  CT-QGECABC                  PIC X(08)   VALUE 'QG1CABC '.
           05  CT-NUM-LINEAS-CABEC         PIC 9(02)   VALUE 05.
      *
           05  CT-GTS-CABECERA1.
               10  FILLER                  PIC X(02)   VALUE SPACES.
               10  FILLER                  PIC X(17)   VALUE SPACES.
               10  FILLER                  PIC X(40)   VALUE
                                   '** CONSULTA DE OPERACIONES POR NOMBR
      -                            'E **'.
      *
           05  CT-GTS-CABECERA2.
               10  FILLER                  PIC X(02)   VALUE SPACES.
               10  FILLER                  PIC X(75)   VALUE ALL '-'.
      *
           05  CT-GTS-CABECERA3.
               10  FILLER                  PIC X(02)   VALUE SPACES.
               10  FILLER                  PIC X(08)   VALUE SPACES.
               10  FILLER                  PIC X(23)   VALUE
                                   'NOMBRE DEL BENEFICIARIO'.
               10  FILLER                  PIC X(28)   VALUE SPACES.
               10  FILLER                  PIC X(16)   VALUE
                                   'R.I.F. COMPRADOR'.
      ******************************************************************
      *                      AREA DE CONTADORES                        *
      ******************************************************************
       01  CN-CONTADORES.
           05  CN-REGISTROS                PIC 9(05)   VALUE ZEROES.
      ******************************************************************
      *                        AREA DE INDICES
      ******************************************************************
       01  IN-INDICES.
           05  IN-REGISTRO                 PIC S9(03) COMP
                                   VALUE ZEROES.
      ******************************************************************
      *                    COPYS UTILIZADAS                            *
      ******************************************************************
      *  COPY PARA ERRORES DB2
           COPY QCWCL20.
      *  COPY PARA ATRIBUTOS DE LA PANTALLA
           COPY DFHBMSCA.
      *  COPY PARA EL PROGRAMA DE ABEND
       01  WS-QGECABC-01.
           COPY QGECABC.
      *  C0PY DE CODIGOS DE RETORNO
       01  WS-BGECRET0.
           COPY BGECRET0.
      *
      ******* TABLA MERCADO LIBRE DE DIVISA*****************************
      *
           EXEC SQL
               INCLUDE V1GT001
           END-EXEC
      ******************************************************************
      *                        CURSORES
      ******************************************************************
           EXEC SQL
               DECLARE V12C05Z-V1DC0011
               CURSOR FOR
                   SELECT DISTINCT
                          NOM_BENEF,
                          NUM_DOC_PERSONA
                   FROM V1DT001
                   WHERE NOM_BENEF LIKE :WS-NOM-NOMBRE
                   ORDER BY NOM_BENEF,
                            NUM_DOC_PERSONA
           END-EXEC
      *
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA.
      *
           COPY QGECCAA.
       COPY V1EC001.
       COPY QCRMGTS.
       COPY QCRMGTS1.
      ******************************************************************
      *                                                                *
      *           P R O C E D U R E      D I V I S I O N               *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION USING V1EC001 QCRMGTSI.
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
      *                                                                *
      *   - INICIALIZAR VARIABLES AUXILIARES, COPYS,  AVISOS Y ERRORES *
      *   - DIRECCIONAR PARAMETROS DE ENTRADA                          *
      *   - ACTIVAR SWITCH DE OPERACION NO CONTABLE                    *
      ******************************************************************
       1000-INICIO.
      *
           SET ADDRESS OF V1EC001          TO CAA-PTRDATA
           SET ADDRESS OF QCRMGTSI         TO CAA-PTR-COPYIN
      *
           EXEC CICS
               IGNORE CONDITION ERROR
           END-EXEC
      *
           INITIALIZE WS-VARIABLES-AUXILIARES
                      WS-BGECRET0

           INITIALIZE CAA-COD-AVISO1
                      CAA-COD-AVISO2
                      CAA-VAR1-ERROR
                      CAA-VAR2-ERROR
                      CAA-COD-ERROR

      *
           MOVE CAA-TERMINAL               TO WS-GTS-SUFI
           SET CAA-88-ACCION-TERMINAL      TO TRUE
           SET CAA-88-CONTABLE-NO          TO TRUE
           MOVE CAA-CODTRAN                TO CAA-CODTRAN-SIG
           MOVE CAA-CODTRAN                TO COD-TRANSACCION
      *
           .
       1000-INICIO-EXIT.
             EXIT.
      ******************************************************************
      *                        2000-PROCESO                            *
      *  -EVALUA EL ESTADO DE LA TRANSACION  EJECUTA LA ACCION         *
      *   CORRESPONDIENTE                                              *
      ******************************************************************
       2000-PROCESO.
      *
           EVALUATE TRUE
               WHEN CAA-88-ESTADO-INICIO
                   PERFORM 2100-ESTADO-INICIO
                      THRU 2100-ESTADO-INICIO-EXIT

               WHEN CAA-88-ESTADO-CONTIN
                   PERFORM 3000-ESTADO-CONTINUACION
                      THRU 3000-ESTADO-CONTINUACION-EXIT
           END-EVALUATE
      *
           .
       2000-PROCESO-EXIT.
           EXIT.
      ******************************************************************
      *                        2100-ESTADO-INICIO                      *
      *                                                                *
      *  -BORRA LA COLA TS DEL LISTADO                                 *
      *  -GRABA CABECERA Y REGISTROS EN LA COLA TS                     *
      *  -SE INTRODUCEN LOS PARAMETROS PARA EL MODULO DE PAGINACION    *
      ******************************************************************
       2100-ESTADO-INICIO.
      *
      *    IF  CAA-CASO = '2'
      *        PERFORM 2150-PREPARAR-PAGINACION
      *           THRU 2150-PREPARAR-PAGINACION-EXIT
      *    ELSE
               PERFORM 6000-BORRAR-COLA
                  THRU 6000-BORRAR-COLA-EXIT

               PERFORM 5000-GRABAR-REGISTROS
                  THRU 5000-GRABAR-REGISTROS-EXIT

               IF  CN-REGISTROS EQUAL ZEROS
                   SET CAA-88-CODTRAN-SIG-SAME TO TRUE
                   SET CAA-88-ACCION-TERMINAL  TO TRUE
                   SET CAA-88-ESTADO-CONTIN    TO TRUE
                   MOVE 'V1A0501'              TO CAA-COD-AVISO1
                   PERFORM 6000-FIN
                      THRU 6000-FIN-EXIT
               END-IF

               PERFORM 2150-PREPARAR-PAGINACION
                  THRU 2150-PREPARAR-PAGINACION-EXIT
      *    END-IF
      *
           SET CAA-88-ACCION-TERMINAL      TO TRUE
           SET CAA-88-TIPO-SALIDA-PAG      TO TRUE
           MOVE CAA-CODTRAN                TO CAA-CODTRAN-SIG
           SET CAA-88-ESTADO-CONTIN        TO TRUE
      *
           .
       2100-ESTADO-INICIO-EXIT.
           EXIT.
      ******************************************************************
      *                    2150-PREPARAR-PAGINACION                    *
      *  INFORMA LOS PARAMETROS DEL MODULO DE PAGINACION               *
      ******************************************************************
       2150-PREPARAR-PAGINACION.
      *
           INITIALIZE CAA-PAGINAR
           MOVE 'X'                        TO CAA-SEL-PERMIT(1)
           SET CAA-88-IND-VARSEL-NO        TO TRUE
           MOVE '00'                       TO CAA-FKEY-NUM(1)
           MOVE 'ENTER'                    TO CAA-FKEY-LIT(1)
           MOVE 'S'                        TO CAA-FKEY-SEL(1)
           MOVE SPACES                     TO CAA-CONTENID
           SET CAA-88-IND-MOD-DATO-NO      TO TRUE
           MOVE 5                          TO CAA-NUM-LIN-CAB-9
           SET CAA-88-IND-SCROLL-NO        TO TRUE
           IF  IND-MAS-DATOS-164 = 'S'
               SET CAA-88-IND-AVPAG-SI     TO TRUE
           ELSE
               SET CAA-88-IND-AVPAG-NO     TO TRUE
           END-IF
      *
           .
       2150-PREPARAR-PAGINACION-EXIT.
           EXIT.
      ******************************************************************
      *                    3000-ESTADO-CONTINUACION                    *
      *  EFECTUA LA ACCION CORRESPONDIENTE EN DEPENDENCIA DEL          *
      *  CASO                                                          *
      ******************************************************************
       3000-ESTADO-CONTINUACION.
      *
           MOVE CAA-TECLA                  TO SW-PF-PRO
      *
           EVALUATE TRUE
               WHEN PF-INTRO
                   PERFORM 3150-COMENZAR-PROCESO
                      THRU 3150-COMENZAR-PROCESO-EXIT

               WHEN PF-BORRA
                   PERFORM 6000-BORRAR-COLA
                      THRU 6000-BORRAR-COLA-EXIT
                   SET CAA-88-CODTRAN-SIG-ULTI TO TRUE
                   SET CAA-88-ACCION-PROGRAMA TO TRUE
                   SET CAA-88-ESTADO-INICIO TO TRUE

               WHEN PF-PF8
                   PERFORM 5000-GRABAR-REGISTROS
                      THRU 5000-GRABAR-REGISTROS-EXIT

                   IF  IND-MAS-DATOS-164 EQUAL CT-SI
                       SET CAA-88-IND-AVPAG-SI     TO TRUE
                   ELSE
                       SET CAA-88-IND-AVPAG-NO     TO TRUE
                   END-IF

                   SET CAA-88-ESTADO-CONTIN TO TRUE

               WHEN PF-PF10
                   MOVE CT-TRAN-AYUDA          TO CAA-CODTRAN-SIG
                   SET  CAA-88-ESTADO-INICIO   TO TRUE
                   SET  CAA-88-CADENA-ANADIR   TO TRUE
                   SET  CAA-88-ACCION-PROGRAMA TO TRUE
                   MOVE CT-CASO2               TO CAA-CASO-CAD
      *
           END-EVALUATE
      *
           .
       3000-ESTADO-CONTINUACION-EXIT.
           EXIT.
      ******************************************************************
      *                                                                *
      *                      3150-COMENZAR-PROCESO                     *
      *  SE BUSCA EL PRIMER REGISTRO SELECCIONADO Y SE COMIENZA        *
      *  EL PROCESO                                                    *
      ******************************************************************
       3150-COMENZAR-PROCESO.
      *
           INITIALIZE COD-RIF
      *
           MOVE CT-NUM-LINEAS-CABEC        TO IN-REGISTRO
           ADD CT-INCRE-1                  TO IN-REGISTRO
           MOVE ZEROES                     TO CN-REGISTROS
           MOVE SPACES                     TO WS-GTS-DATOS(1:1)
           SET NO-FIN-COLA                 TO TRUE
      *
           PERFORM 3300-BUSCAR-SELECCION
              THRU 3300-BUSCAR-SELECCION-EXIT
           UNTIL WS-GTS-DATOS(1:1) NOT EQUAL SPACES OR
                 FIN-COLA
      *
           IF NO-FIN-COLA
               PERFORM 4900-PROCESAR-SELECCION
                  THRU 4900-PROCESAR-SELECCION-EXIT
           END-IF
      *
           .
       3150-COMENZAR-PROCESO-EXIT.
           EXIT.
      ******************************************************************
      *                                                                *
      *                         BORRAR-SELECION                        *
      *                                                                *
      ******************************************************************
       BORRAR-SELECION-TS.
      *
           MOVE SPACES                     TO WS-GTS-DATOS (1:1)
      *
           EXEC CICS
              WRITEQ TS
              QUEUE(WS-GTS)
              FROM (WS-GTS-DATOS)
              ITEM(IN-REGISTRO) REWRITE
              NOHANDLE
           END-EXEC
      *
           EVALUATE EIBRESP
      *
               WHEN DFHRESP(NORMAL)
      *
                   CONTINUE
      *
               WHEN OTHER
      *
                 INITIALIZE QGECABC
                 MOVE CT-ERROR-READQ       TO ABC-REFERENCIA
      *
                 PERFORM 9999-ABEND-CICS
                    THRU 9999-ABEND-CICS-EXIT
      *
           END-EVALUATE
      *
           .
       BORRAR-SELECION-TS-EXIT.
           EXIT.
      ******************************************************************
      *                   3300-BUSCAR-SELECCION
      *    MANDA A LEER EL SIGUIENTE REGISTRO E INCREMENTA
      *    EL INDICE Y EL CONTADOR DE REGISTROS LEIDOS
      ******************************************************************
       3300-BUSCAR-SELECCION.
      *
           PERFORM 6200-LEER-REGISTRO-COLA
              THRU 6200-LEER-REGISTRO-COLA-EXIT
      *
           ADD CT-INCRE-1                  TO IN-REGISTRO
           ADD CT-INCRE-1                  TO CN-REGISTROS
      *
           .
       3300-BUSCAR-SELECCION-EXIT.
           EXIT.
      ******************************************************************
      *                                                                *
      *                    4900-PROCESAR-SELECCION                     *
      *  -DESMARCA EL REGISTRO EN LA COLA TS                           *
      *  -PREPARA LLAMADO A LA TRANSACCION DE CONSULTA                 *
      *                                                                *
      ******************************************************************
       4900-PROCESAR-SELECCION.
      *
           MOVE WS-GTS-DATOS               TO WS-GTS-FILA
           SUBTRACT CT-UNO                 FROM IN-REGISTRO
      *
           PERFORM BORRAR-SELECION-TS
              THRU BORRAR-SELECION-TS-EXIT
      *
           MOVE CT-TRAN-RESUMEN            TO CAA-CODTRAN-SIG
           SET  CAA-88-ESTADO-INICIO       TO TRUE
           SET  CAA-88-CADENA-ANADIR       TO TRUE
           SET  CAA-88-ACCION-PROGRAMA     TO TRUE
           MOVE WS-GTS-RIF                 TO COD-RIF
           MOVE CT-CASO2                   TO CAA-CASO-CAD
      *
           .
       4900-PROCESAR-SELECCION-EXIT.
           EXIT.
      ******************************************************************
      *                                                                *
      *                     5000-GRABAR-REGISTROS                      *
      *  GRABA REGISTROS EN LA COLA TS                                 *
      *    -SI QUEDAN DATOS POR GRABAR ACTIVA EL SWITCH SW-MAS-DATOS   *
      *     Y GUARDAR LAS LLAVES DE REPOSICIONAMIENTO                  *
      ******************************************************************
       5000-GRABAR-REGISTROS.
      *
           SET NO-FIN-CURSOR               TO TRUE
           MOVE ZEROES                     TO CN-REGISTROS
           MOVE CT-SI                      TO IND-MAS-DATOS-164
      *
           MOVE SPACES                     TO WS-NOM-NOMBRE
           STRING NOM-NOMBRE DELIMITED BY '  '
                  '%' DELIMITED BY SIZE
           INTO WS-NOM-NOMBRE
      *
           PERFORM 5100-ABRIR-CURSOR
              THRU 5100-ABRIR-CURSOR-EXIT

           PERFORM 5200-LEER-CURSOR
              THRU 5200-LEER-CURSOR-EXIT
             UNTIL FIN-CURSOR

           PERFORM 5700-CERRAR-CURSOR
              THRU 5700-CERRAR-CURSOR-EXIT
      *
           .
       5000-GRABAR-REGISTROS-EXIT.
           EXIT.
      ******************************************************************
      *                      5100-ABRIR-CURSOR                         *
      ******************************************************************
       5100-ABRIR-CURSOR.
      *
           EXEC SQL
               OPEN V12C05Z-V1DC0011
           END-EXEC
      *
           MOVE SQLCODE                    TO DB2-RETURN-CDE
      *
           IF  NOT DB2-OK
               INITIALIZE QGECABC
               MOVE CT-TAB-V1DT001         TO ABC-OBJETO-ERROR
               PERFORM 9999-ABEND-DB2
                  THRU 9999-ABEND-DB2-EXIT
           END-IF
      *
           .
       5100-ABRIR-CURSOR-EXIT.
           EXIT.
      ******************************************************************
      *                       5200-LEER-CURSOR                         *
      *  -EFECTUA LECTURA DEL CURSOR                                   *
      *  -SI EXISTEN DATOS SE LLAMA AL PARRAFO QUE LOS PROCESA         *
      ******************************************************************
       5200-LEER-CURSOR.
      *
           EXEC SQL
               FETCH V12C05Z-V1DC0011
               INTO :NOM-BENEF,
                    :NUM-DOC-PERSONA
           END-EXEC
      *
           MOVE SQLCODE                    TO DB2-RETURN-CDE
           EVALUATE TRUE
               WHEN DB2-OK
                   PERFORM 5300-PROCESAR-REGISTRO
                      THRU 5300-PROCESAR-REGISTRO-EXIT

               WHEN DB2-NOTFND
                   SET FIN-CURSOR          TO TRUE
                   MOVE SPACES             TO IND-MAS-DATOS-164

               WHEN OTHER
                   INITIALIZE QGECABC
                   MOVE CT-TAB-V1DT001     TO ABC-OBJETO-ERROR
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *
           .
       5200-LEER-CURSOR-EXIT.
           EXIT.
      ******************************************************************
      *                     5300-PROCESAR-REGISTRO                     *
      ******************************************************************
       5300-PROCESAR-REGISTRO.
      *
           ADD CT-UNO                      TO CN-REGISTROS
      *
           IF  CN-REGISTROS EQUAL CT-UNO AND CAA-88-ESTADO-INICIO
               PERFORM 5400-GRABAR-CABECERA
                  THRU 5400-GRABAR-CABECERA-EXIT
           END-IF
           IF  CN-REGISTROS > 5000
               MOVE 'V1A0502'              TO CAA-COD-AVISO1
               SET FIN-CURSOR              TO TRUE
           ELSE
               MOVE NOM-BENEF              TO WS-GTS-NOMBRE
               MOVE NUM-DOC-PERSONA        TO WS-GTS-RIF
               MOVE WS-GTS-FILA            TO WS-GTS-DATOS
               PERFORM 6100-ADD-REGISTRO-COLA
                  THRU 6100-ADD-REGISTRO-COLA-EXIT
           END-IF
      *
           .
       5300-PROCESAR-REGISTRO-EXIT.
           EXIT.
      ******************************************************************
      *                     5400-GRABAR-CABECERA                       *
      *  GRABAR LA CABECERA DEL LISTADO                                *
      ******************************************************************
       5400-GRABAR-CABECERA.
      *
           MOVE CT-GTS-CABECERA1           TO WS-GTS-DATOS
           PERFORM 6100-ADD-REGISTRO-COLA
              THRU 6100-ADD-REGISTRO-COLA-EXIT
      *
           MOVE SPACES                     TO WS-GTS-DATOS
           PERFORM 6100-ADD-REGISTRO-COLA
              THRU 6100-ADD-REGISTRO-COLA-EXIT
      *
           MOVE CT-GTS-CABECERA2           TO WS-GTS-DATOS
           PERFORM 6100-ADD-REGISTRO-COLA
              THRU 6100-ADD-REGISTRO-COLA-EXIT
      *
           MOVE CT-GTS-CABECERA3           TO WS-GTS-DATOS
           PERFORM 6100-ADD-REGISTRO-COLA
              THRU 6100-ADD-REGISTRO-COLA-EXIT
      *
           MOVE CT-GTS-CABECERA2           TO WS-GTS-DATOS
           PERFORM 6100-ADD-REGISTRO-COLA
              THRU 6100-ADD-REGISTRO-COLA-EXIT
      *
           .
       5400-GRABAR-CABECERA-EXIT.
           EXIT.
      ******************************************************************
      *                      5700-CERRAR-CURSOR                        *
      ******************************************************************
       5700-CERRAR-CURSOR.
      *
           EXEC SQL
               CLOSE V12C05Z-V1DC0011
           END-EXEC
      *
           MOVE SQLCODE                    TO DB2-RETURN-CDE
           IF  NOT DB2-OK
               INITIALIZE QGECABC
               MOVE CT-TAB-V1DT001         TO ABC-OBJETO-ERROR
               PERFORM 9999-ABEND-DB2
                  THRU 9999-ABEND-DB2-EXIT
           END-IF
      *
           .
       5700-CERRAR-CURSOR-EXIT.
           EXIT.
      ******************************************************************
      *                                                                *
      *                        6000-BORRAR-COLA                        *
      *  BORRA LA COLA TS DEL LISTADO                                  *
      ******************************************************************
       6000-BORRAR-COLA.
      *
           EXEC CICS
               DELETEQ TS
               QUEUE(WS-GTS)
               NOHANDLE
           END-EXEC
      *
           EVALUATE EIBRESP
               WHEN (DFHRESP(NORMAL))
                   CONTINUE

               WHEN (DFHRESP(QIDERR))
                   CONTINUE

               WHEN OTHER
                   INITIALIZE QGECABC
                   MOVE CT-ERROR-DELETEQ   TO ABC-REFERENCIA
                   PERFORM 9999-ABEND-CICS
           END-EVALUATE
      *
           .
       6000-BORRAR-COLA-EXIT.
           EXIT.
      ******************************************************************
      *                                                                *
      *                     6100-ADD-REGISTRO-COLA                     *
      *  AGREGA UN REGISTRO A LA COLA TS                               *
      ******************************************************************
       6100-ADD-REGISTRO-COLA.
      *
           EXEC CICS
                WRITEQ TS QUEUE(WS-GTS)
                FROM(WS-GTS-DATOS)
                MAIN
                NOHANDLE
           END-EXEC
      *
           IF  EIBRESP NOT EQUAL DFHRESP(NORMAL)
               INITIALIZE QGECABC
               MOVE CT-ERROR-WRITEQ        TO ABC-REFERENCIA
               PERFORM 9999-ABEND-CICS
                  THRU 9999-ABEND-CICS-EXIT
           END-IF
      *
           .
       6100-ADD-REGISTRO-COLA-EXIT.
           EXIT.
      ******************************************************************
      *                                                                *
      *                     6200-LEER-REGISTRO-COLA                    *
      *  LEE UN REGISTRO DE LA COL TS                                  *
      ******************************************************************
       6200-LEER-REGISTRO-COLA.
      *
           EXEC CICS
              READQ TS QUEUE(WS-GTS)
              INTO (WS-GTS-DATOS)
              ITEM(IN-REGISTRO)
              NOHANDLE
           END-EXEC
      *
           EVALUATE EIBRESP
      *
               WHEN DFHRESP(NORMAL)
      *
                   CONTINUE
      *
               WHEN DFHRESP(ITEMERR)
      *
                   SET FIN-COLA            TO TRUE
      *
               WHEN OTHER
      *
                 INITIALIZE QGECABC
                 MOVE CT-ERROR-READQ       TO ABC-REFERENCIA
      *
                 PERFORM 9999-ABEND-CICS
                    THRU 9999-ABEND-CICS-EXIT
      *
           END-EVALUATE
      *
           .
       6200-LEER-REGISTRO-COLA-EXIT.
           EXIT.
      ******************************************************************
      *                                                                *
      *                          6000-FIN                              *
      *  TERMINA LA EJECUCION DEL PROGRAMA                             *
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
      *                                                                *
      *                        9999-ABEND-CICS                         *
      *  SE ABENDA LA TAREA CUANDO SE PRODUCE UN ERROR CICS            *
      ******************************************************************
       9999-ABEND-CICS.
      *
           MOVE CT-SI                      TO ABC-ABEND
           MOVE CT-PROGRAMA                TO ABC-PROGRAMA
           MOVE EIBFN                      TO ABC-EIBFN
           MOVE EIBRSRCE                   TO ABC-EIBRSRCE
           MOVE EIBRCODE                   TO ABC-EIBRCODE
           MOVE EIBRESP                    TO ABC-EIBRESP1
           MOVE EIBRESP2                   TO ABC-EIBRESP2
      *
           EXEC CICS
             LINK PROGRAM(CT-QGECABC)
             COMMAREA (QGECABC)
             NOHANDLE
           END-EXEC
      *
           .
       9999-ABEND-CICS-EXIT.
           EXIT.
      ******************************************************************
      *                                                                *
      *                       9999-ABEND-DB2                           *
      *  SE ABENDA LA TAREA CUANDO SE PRODUCE UN ERROR DB2             *
      ******************************************************************
       9999-ABEND-DB2.
      *
           MOVE CT-SI                      TO ABC-ABEND
           MOVE CT-PROGRAMA                TO ABC-PROGRAMA
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
