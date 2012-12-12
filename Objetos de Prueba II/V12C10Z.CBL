      ******************************************************************
      *                                                                *
      *  PROGRAMA: V12C10Z                                             *
      *                                                                *
      *  FECHA CREACION: 07/10/2008                                    *
      *                                                                *
      *  AUTOR: FACTORIA                                               *
      *                                                                *
      *  APLICACION: MERCADO LIBRE DE DIVISAS. (V1) VENEZUELA.         *
      *                                                                *
      *  DESCRIPCION: RESUMEN GLOBAL POR CLIENTE EN MN Y ME            *
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
       PROGRAM-ID.   V12C10Z.
       AUTHOR.       FACTORIA.
       DATE-WRITTEN. 07/10/2008
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
           05  IND-MAS-DATOS               PIC X(01).
      *
           05  WS-NUM-DOC-PERSONA          PIC X(15).
           05  REDEFINES WS-NUM-DOC-PERSONA.
               10  BPB-NAC                 PIC X(01).
               10  BPB-NUM                 PIC 9(14).
               10  REDEFINES BPB-NUM.
                   15  BPB-NUM1            PIC 9(06).
                   15  BPB-NUM2            PIC 9(08).
      *
           05  WS-CED-RIF1                 PIC X(15).
           05  REDEFINES WS-CED-RIF1.
               10  NAC1                    PIC X(01).
               10  NUM1                    PIC 9(14).
      *
           05  FECHA                       PIC 9(08).
           05  REDEFINES FECHA.
               10 AA                       PIC 9(04).
               10 MM                       PIC 9(02).
               10 DD                       PIC 9(02).
           05  SW-1                        PIC 9(01).
           05  WS-NUM-AUX                  PIC 9(02).
      *
           05  WS-GTS-DATOS                PIC X(80)   VALUE SPACES.
      *
           05  WS-GTS-FILA.
               10  WS-GTS-OPCION           PIC X(01)   VALUE SPACES.
               10  FILLER                  PIC X(01)   VALUE 'B'.
               10  WS-GTS-FEC-OPER.
                   15  WS-GTS-FEC-OPER-DD  PIC 9(02).
                   15  WS-GTS-FEC-OPER-MM  PIC 9(02).
                   15  WS-GTS-FEC-OPER-AA  PIC 9(04).
               10  FILLER                  PIC X(01)   VALUE SPACES.
               10  WS-GTS-CED-RIF          PIC X(15)   VALUE SPACES.
               10  FILLER                  PIC X(01)   VALUE SPACES.
               10  WS-GTS-NOMBRES          PIC X(20)   VALUE SPACES.
               10  FILLER                  PIC X(01)   VALUE SPACES.
               10  WS-GTS-MTO-ME           PIC ZZZZZZZZZZZZ9.99.
      *
           05  WS-GTS-TOTALES.
               10  FILLER                  PIC X(01)   VALUE SPACES.
               10  FILLER                  PIC X(01)   VALUE '+'.
               10  FILLER                  PIC X(15)   VALUE
                                   'T O T A L E S :'.
               10  FILLER                  PIC X(10)   VALUE SPACES.
               10  WS-GTS-NUM-REG          PIC X(05).
               10  FILLER                  PIC X(16)   VALUE SPACES.
               10  WS-GTS-TOTAL-ME         PIC ZZZZZZZZZZZZ9.99.
      *
           05  WS-GTS-CABECERA1.
               10  FILLER                  PIC X(08)   VALUE SPACES.
               10  WS-GTS-BANCA            PIC X(26)   VALUE SPACES.
               10  FILLER                  PIC X(08)   VALUE ' DESDE: '.
               10  WS-GTS-F-DESD-DD        PIC 9(02)   VALUE ZEROES.
               10  FILLER                  PIC X(01)   VALUE '/'.
               10  WS-GTS-F-DESD-MM        PIC 9(02)   VALUE ZEROES.
               10  FILLER                  PIC X(01)   VALUE '/'.
               10  WS-GTS-F-DESD-AAAA      PIC 9(04)   VALUE ZEROES.
               10  FILLER                  PIC X(08)   VALUE ' HASTA: '.
               10  WS-GTS-F-HAST-DD        PIC 9(02)   VALUE ZEROES.
               10  FILLER                  PIC X(01)   VALUE '/'.
               10  WS-GTS-F-HAST-MM        PIC 9(02)   VALUE ZEROES.
               10  FILLER                  PIC X(01)   VALUE '/'.
               10  WS-GTS-F-HAST-AAAA      PIC 9(04)   VALUE ZEROES.
      *
           05  WS-TIP-FORM-PAGO            PIC X(01)   VALUE  SPACE.
           05  WS-NUM-REFERENCI            PIC S9(10)V USAGE COMP-3.
           05  WS-NUM-REFEREN-MAX          PIC S9(10)V USAGE COMP-3.
           05  WS-FEC-DESDE                PIC X(08)   VALUE SPACES.
           05  WS-FEC-HASTA                PIC X(08)   VALUE SPACES.
           05  WS-COD-OFICIN               PIC X(06)   VALUE SPACES.
           05  WS-COD-OFICIN-MAX           PIC X(06)   VALUE SPACES.
           05  WS-IDF-CELULA-RIF           PIC X(15)   VALUE SPACES.
           05  WS-IDF-CELULA-RIF-MAX       PIC X(15)   VALUE SPACES.
           05  WS-COD-DIVISA               PIC S9(03)V COMP-3.
           05  WS-COMPRA-VENT              PIC X(01)   VALUE SPACE.
           05  WS-NUM-CONT                 PIC X(06)   VALUE SPACE.
      *
           05  WS-FECHA-DESDE              PIC X(08)   VALUE SPACES.
           05  WS-FECHA-HASTA              PIC X(08)   VALUE SPACES.
           05  WS-CEDULA1                  PIC X(15)   VALUE SPACES.
           05  WS-CEDULA2                  PIC X(15)   VALUE SPACES.
      *
           05  WS-MONTO-TOTAL              PIC 9(13)V9(2).
           05  WS-LAST-FEC                 PIC 9(08).
           05  WS-LAST-RIF                 PIC X(15).
           05  WS-LAST-NOMBRE              PIC X(40).
      *
           05  WS-TOT-GEN                  PIC 9(13)V9(2).
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
               88  PF-BORRA                            VALUE 99.
               88  PF-INTRO                            VALUE 00.
      *
           05  SW-FIN-COLA                 PIC X(01)   VALUE SPACES.
               88  FIN-COLA                            VALUES 'S'.
               88  NO-FIN-COLA                         VALUES 'N'.
      *
           05  SW-PROCESAR-REG             PIC X(01)   VALUE SPACES.
               88  SI-PROCESAR                         VALUES 'S'.
               88  NO-PROCESAR                         VALUES 'N'.
      *
           05  SW-EXISTE-CLIENTE           PIC X(01)   VALUE 'S'.
               88  SI-EXISTE-CLIENTE                    VALUE 'S'.
               88  NO-EXISTE-CLIENTE                    VALUE 'N'.
      ******************************************************************
      *                        AREA DE CONTANTES                       *
      ******************************************************************
       01  CT-CONSTANTES.
      *
           05  WS-GTS.
               10  FILLER                  PIC X(4)    VALUE '+GTS'.
               10  WS-GTS-SUFI             PIC X(4)    VALUE SPACES.
      *
           05  CT-PROGRAMA                 PIC X(08)   VALUE 'V12C10Z'.
           05  CT-CASO1                    PIC X(01)   VALUE '1'.
           05  CT-CASO2                    PIC X(01)   VALUE '2'.
           05  CT-SI                       PIC X(01)   VALUE 'S'.
           05  CT-TAB-V1DT001              PIC X(08)   VALUE 'V1DT001'.
           05  CT-OBLIGA-SELECCION         PIC X(01)   VALUE 'S'.
           05  CT-INTRO                    PIC X(02)   VALUE '00'.
           05  CT-PF2                      PIC X(02)   VALUE '02'.
           05  CT-RTRNO                    PIC X(05)   VALUE 'RTRNO'.
           05  CT-INCRE-1                  PIC 9(01)   VALUE 1.
           05  CT-IND-DATOS                PIC 9(01)   VALUE 4.
           05  CT-UNO                      PIC 9(01)   VALUE 1.
           05  CT-TOTAL                    PIC X(01)   VALUE 'T'.
      *
           05  CT-CONTENIDO.
               10  FILLER                  PIC X(10)   VALUE SPACE.
               10  FILLER                  PIC X(29)
               VALUE '** CONSOLIDADO DE CLIENTES **'.
      *
           05  CT-ERROR-WRITEQ             PIC X(20)
                                   VALUE 'ERROR EN CICS WRITQ'.
           05  CT-ERROR-DELETEQ            PIC X(20)
                                   VALUE 'ERROR CICS DELETEQ'.
           05  CT-ERROR-READQ              PIC X(20)
                                   VALUE 'ERROR CICS READQ'.
           05  CT-QGECABC                  PIC X(08)   VALUE 'QG1CABC '.
           05  CT-NUM-LINEAS-CABEC         PIC 9(02)   VALUE 05.
      *
           05  CT-GTS-LINEAH.
               10  FILLER                  PIC X(02)   VALUE SPACES.
               10  FILLER                  PIC X(77)   VALUE ALL '-'.

           05  CT-GTS-CABECERA2.
               10  FILLER                  PIC X(02)   VALUE ' -'.
               10  FILLER                  PIC X(02)   VALUE SPACES.
               10  FILLER                  PIC X(05)   VALUE 'FECHA'.
               10  FILLER                  PIC X(06)   VALUE SPACES.
               10  FILLER                  PIC X(06)   VALUE 'CEDULA'.
               10  FILLER                  PIC X(09)   VALUE SPACES.
               10  FILLER                  PIC X(13)   VALUE
                                   'N O M B R E S'.
               10  FILLER                  PIC X(08)   VALUE SPACES.
               10  FILLER                  PIC X(12)   VALUE
                                   'MONTO GLOBAL'.
      *
           05  CT-GTS-CABECERA3.
               10  FILLER                  PIC X(02)   VALUE ' -'.
               10  FILLER                  PIC X(09)   VALUE
                                   'OPERACION'.
               10  FILLER                  PIC X(06)   VALUE SPACES.
               10  FILLER                  PIC X(03)   VALUE 'RIF'.
               10  FILLER                  PIC X(29)   VALUE SPACES.
               10  FILLER                  PIC X(17)   VALUE
                                   'MONEDA EXTRANJERA'.
      *
           05  CT-CCO-100                  PIC X(03)   VALUE '100'.
           05  CT-CCO-199                  PIC X(03)   VALUE '199'.
           05  CT-CCO-500                  PIC X(03)   VALUE '500'.
           05  CT-CCO-599                  PIC X(03)   VALUE '599'.
      *
           05  CT-COMPRA                   PIC X(01)   VALUE 'C'.
           05  CT-VENTA                    PIC X(01)   VALUE 'V'.
           05  CT-TOTAL-OPER               PIC X(09)
                                       VALUE 'TOT.OPER:'.
           05  CT-TOTAL-MONTO              PIC X(10)
                                       VALUE 'TOT.MONTO:'.
           05  CT-PE9C2010                 PIC X(08)   VALUE 'PE9C2010'.
      ******************************************************************
      *                      AREA DE CONTADORES                        *
      ******************************************************************
       01  CN-CONTADORES.
           05  CN-REGISTROS                PIC 9(05).
      ******************************************************************
      *                        AREA DE INDICES
      ******************************************************************
       01  IN-INDICES.
           05  IN-REGISTRO                 PIC S9(03) COMP VALUE ZEROES.
      *
       01  TB-VECTORES.
           05  TB-VEC-BANCA.
               10  FILLER                  PIC X(26)
                                   VALUE 'CORPORATIVA'.
               10  FILLER                  PIC X(26)
                                   VALUE 'EMPRESARIAL'.
               10  FILLER                  PIC X(26)
                                   VALUE 'COMERCIAL'.
           05  REDEFINES TB-VEC-BANCA.
               10  VEC-BANCA OCCURS 3 TIMES PIC X(26).
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
       01  WS-PEEC201A.
           COPY PEEC201A.
      *
      ******* TABLA MERCADO LIBRE DE DIVISA*****************************
      *
           EXEC SQL
               INCLUDE V1GT001
           END-EXEC.
      ******************************************************************
      *                        CURSORES
      ******************************************************************
           EXEC SQL
               DECLARE V12C10Z-V1DC0011
               CURSOR FOR
               SELECT FEC_OPE         ,
                      NUM_DOC_PERSONA ,
                      NOM_RAZON       ,
                      IMP_OPE         ,
                      TAS_DOLAR
               FROM V1DT001
               WHERE (FEC_OPE BETWEEN :WS-FECHA-DESDE AND
                                      :WS-FECHA-HASTA) AND
                     NUM_DOC_PERSONA = :NUM-DOC-PERSONA AND
                     NOT (TIP_OPE BETWEEN '5' AND '8')
               ORDER BY FEC_OPE,
                        NUM_DOC_PERSONA
           END-EXEC
      *
           EXEC SQL
               DECLARE V12C10Z-V1DC0012
               CURSOR FOR
               SELECT FEC_OPE         ,
                      NUM_DOC_PERSONA ,
                      NOM_RAZON       ,
                      IMP_OPE         ,
                      TAS_DOLAR
               FROM V1DT001
               WHERE (FEC_OPE BETWEEN :WS-FECHA-DESDE AND
                                      :WS-FECHA-HASTA) AND
                     NOT (TIP_OPE BETWEEN '5' AND '8')
               ORDER BY FEC_OPE,
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
       PROCEDURE DIVISION USING DFHCOMMAREA V1EC001 QCRMGTSI.
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
           MOVE SPACES                     TO CAA-COD-AVISO1
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
      *  -BORRA LA COLA TS DEL LISTADO                                 *
      *  -GRABA CABECERA Y REGISTROS EN LA COLA TS                     *
      *  -SE INTRODUCEN LOS PARAMETROS PARA EL MODULO DE PAGINACION    *
      ******************************************************************
       2100-ESTADO-INICIO.
      *
           PERFORM 6000-BORRAR-COLA
              THRU 6000-BORRAR-COLA-EXIT

           PERFORM 5000-GRABAR-REGISTROS
              THRU 5000-GRABAR-REGISTROS-EXIT

           IF  CN-REGISTROS GREATER THAN ZEROES
               MOVE CN-REGISTROS           TO WS-GTS-NUM-REG
               MOVE WS-TOT-GEN             TO WS-GTS-TOTAL-ME
               MOVE WS-GTS-TOTALES         TO WS-GTS-DATOS
               PERFORM 6100-ADD-REGISTRO-COLA
                  THRU 6100-ADD-REGISTRO-COLA-EXIT

               PERFORM 2150-PREPARAR-PAGINACION
                  THRU 2150-PREPARAR-PAGINACION-EXIT

               SET  CAA-88-ACCION-TERMINAL TO TRUE
               SET  CAA-88-ESTADO-CONTIN   TO TRUE
               SET  CAA-88-TIPO-SALIDA-PAG TO TRUE
               MOVE CAA-CODTRAN            TO CAA-CODTRAN-SIG
           ELSE
               SET CAA-88-CODTRAN-SIG-SAME TO TRUE
               SET CAA-88-ACCION-TERMINAL  TO TRUE
               SET CAA-88-ESTADO-CONTIN    TO TRUE
      *        NO EXISTE INFORMACION SOLICITADA
               MOVE 'V1E0060'              TO CAA-COD-ERROR
           END-IF
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
           SET CAA-88-IND-VARSEL-SI        TO TRUE
      *
           MOVE '00'                       TO CAA-FKEY-NUM(1)
           MOVE 'DETA.'                    TO CAA-FKEY-LIT(1)
           MOVE 'S'                        TO CAA-FKEY-SEL(1)
           MOVE CT-CONTENIDO               TO CAA-CONTENID
      *
           SET CAA-88-IND-MOD-DATO-NO      TO TRUE
           MOVE CT-NUM-LINEAS-CABEC        TO CAA-NUM-LIN-CAB
           SET CAA-88-IND-SCROLL-NO        TO TRUE
      *
           SET CAA-88-IND-AVPAG-NO     TO TRUE
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
           EVALUATE TRUE
               WHEN PF-INTRO
                   PERFORM 3150-COMENZAR-PROCESO
                      THRU 3150-COMENZAR-PROCESO-EXIT

               WHEN PF-BORRA
                   PERFORM 6000-BORRAR-COLA
                      THRU 6000-BORRAR-COLA-EXIT
                   SET CAA-88-CODTRAN-SIG-ULTI TO TRUE
                   SET CAA-88-ACCION-PROGRAMA  TO TRUE
                   SET CAA-88-ESTADO-INICIO    TO TRUE

               WHEN PF-PF8
                   PERFORM 5000-GRABAR-REGISTROS
                      THRU 5000-GRABAR-REGISTROS-EXIT

                   IF  IND-MAS-DATOS EQUAL CT-SI
                       SET CAA-88-IND-AVPAG-SI     TO TRUE
                   ELSE
                       SET CAA-88-IND-AVPAG-NO     TO TRUE
                   END-IF

                   SET CAA-88-ESTADO-CONTIN        TO TRUE
                   MOVE CT-CASO1                   TO CAA-CASO
           END-EVALUATE
      *
           .
       3000-ESTADO-CONTINUACION-EXIT.
           EXIT.
      ******************************************************************
      *                      3150-COMENZAR-PROCESO                     *
      ******************************************************************
       3150-COMENZAR-PROCESO.
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
           PERFORM 4900-PROCESAR-SELECCION
              THRU 4900-PROCESAR-SELECCION-EXIT
      *
           .
       3150-COMENZAR-PROCESO-EXIT.
           EXIT.
      ******************************************************************
      *                   3300-BUSCAR-SELECCION
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
      *                    4900-PROCESAR-SELECCION                     *
      ******************************************************************
       4900-PROCESAR-SELECCION.
      *
           MOVE WS-GTS-DATOS               TO WS-GTS-FILA
           SUBTRACT CT-UNO                 FROM IN-REGISTRO

           PERFORM BORRAR-SELECION-TS
      *
           MOVE 'V173'                 TO CAA-CODTRAN-SIG
           SET  CAA-88-ACCION-PROGRAMA TO TRUE
           SET  CAA-88-ESTADO-INICIO   TO TRUE
           SET  CAA-88-CADENA-ANADIR   TO TRUE
           MOVE '1'                    TO CAA-CASO
           MOVE '2'                    TO CAA-CASO-CAD
      *
           MOVE WS-GTS-FEC-OPER        TO V173-FECHA-B
           MOVE WS-GTS-CED-RIF         TO V173-CED-RIF1
           MOVE FEC-FECHA-DESDE        TO V173-FECHA-DESDE
           MOVE FEC-FECHA-HASTA        TO V173-FECHA-HASTA
           MOVE WS-GTS-NOMBRES         TO V173-NOMBRES
           MOVE SEG                    TO V173-SEG
      *
           .
       4900-PROCESAR-SELECCION-EXIT.
           EXIT.
      ******************************************************************
      * BORRAR-SELECION-TS
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
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN OTHER
                   INITIALIZE QGECABC
                   MOVE CT-ERROR-READQ     TO ABC-REFERENCIA
                   PERFORM 9999-ABEND-CICS
                      THRU 9999-ABEND-CICS-EXIT
           END-EVALUATE
      *
           .
      ******************************************************************
      *                     5000-GRABAR-REGISTROS                      *
      ******************************************************************
       5000-GRABAR-REGISTROS.
      *
           MOVE ZEROES                     TO CN-REGISTROS
      *
           INITIALIZE WS-MONTO-TOTAL
                      WS-TOT-GEN
      *
           PERFORM ABRIR-CURSOR

           SET NO-FIN-CURSOR               TO TRUE
           SET NO-PROCESAR                 TO TRUE
           PERFORM LEER-CURSOR
             UNTIL SI-PROCESAR OR FIN-CURSOR

           IF  NOT FIN-CURSOR
               PERFORM ANALIZAR-REGISTRO
                 UNTIL FIN-CURSOR
           END-IF
           PERFORM CERRAR-CURSOR
      *
           .
       5000-GRABAR-REGISTROS-EXIT.
           EXIT.
      ******************************************************************
      * ABRIR-CURSOR
      ******************************************************************
       ABRIR-CURSOR.
      *
           MOVE FEC-FECHA-DESDE TO WS-FECHA-DESDE
           MOVE FEC-FECHA-HASTA TO WS-FECHA-HASTA

           IF  CED-NAC1 NOT EQUAL SPACES AND LOW-VALUES AND
               CED-NUM1 NOT EQUAL SPACES AND ZEROS
               MOVE CED-RIF1    TO  NUM-DOC-PERSONA
               EXEC SQL
                   OPEN V12C10Z-V1DC0011
               END-EXEC
           ELSE
               EXEC SQL
                   OPEN V12C10Z-V1DC0012
               END-EXEC
           END-IF
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
      ******************************************************************
      * LEER-CURSOR
      ******************************************************************
       LEER-CURSOR.
      *
           IF  CED-NAC1 NOT EQUAL SPACES AND LOW-VALUES AND
               CED-NUM1 NOT EQUAL SPACES AND ZEROS
               EXEC SQL
                   FETCH V12C10Z-V1DC0011
                   INTO :FEC-OPE,
                        :NUM-DOC-PERSONA,
                        :NOM-RAZON,
                        :IMP-OPE,
                        :TAS-DOLAR
               END-EXEC
           ELSE
               EXEC SQL
                   FETCH V12C10Z-V1DC0012
                   INTO :FEC-OPE,
                        :NUM-DOC-PERSONA,
                        :NOM-RAZON,
                        :IMP-OPE,
                        :TAS-DOLAR
               END-EXEC
           END-IF
      *
           MOVE SQLCODE                    TO DB2-RETURN-CDE
           EVALUATE TRUE
               WHEN DB2-OK
                   SET SI-PROCESAR TO TRUE
                   IF  SEG NOT = ZEROS
                       PERFORM VERIFICAR-SEGMENTO
                   END-IF

               WHEN DB2-NOTFND
                   SET FIN-CURSOR          TO TRUE

               WHEN OTHER
                   INITIALIZE QGECABC
                   MOVE CT-TAB-V1DT001     TO ABC-OBJETO-ERROR
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *
           .
      ******************************************************************
      * ANALIZAR-REGISTRO
      ******************************************************************
       ANALIZAR-REGISTRO.
      *
           COMPUTE WS-MONTO-TOTAL = WS-MONTO-TOTAL +
                   IMP-OPE * TAS-DOLAR
      *    ACTUALIZAR VARIABLES DE CONTROL
           MOVE FEC-OPE         TO WS-LAST-FEC
           MOVE NUM-DOC-PERSONA TO WS-LAST-RIF
           MOVE NOM-RAZON       TO WS-LAST-NOMBRE
      *
           SET NO-PROCESAR TO TRUE
           PERFORM LEER-CURSOR
             UNTIL SI-PROCESAR OR FIN-CURSOR
      *
           IF  FIN-CURSOR OR
               FEC-OPE NOT = WS-LAST-FEC OR
               NUM-DOC-PERSONA NOT = WS-LAST-RIF
               PERFORM GUARDAR-RESUMEN
      *
               INITIALIZE WS-MONTO-TOTAL
           END-IF
      *
           .
      ******************************************************************
      * VERIFICAR-SEGMENTO
      ******************************************************************
       VERIFICAR-SEGMENTO.
      *
           INITIALIZE PEEC201A
           MOVE '0102'                     TO PEEC201A-PECDGENT
           MOVE SPACES                     TO PEEC201A-PENUMPER
           MOVE NUM-DOC-PERSONA(1:1)       TO PEEC201A-PETIPDOC
           MOVE '000'                      TO PEEC201A-PENUMDOC(1:3)
           MOVE NUM-DOC-PERSONA(8:8)       TO PEEC201A-PENUMDOC(4:8)
           MOVE '01'                       TO PEEC201A-PESECDOC
           MOVE '00'                       TO PEEC201A-IND-GRUPO
      *
           CALL CT-PE9C2010 USING WS-PEEC201A
      *
           EVALUATE PEEC201A-RETORNO
               WHEN '00'
                   SET SI-EXISTE-CLIENTE       TO TRUE
                   MOVE PEEC201A-PEARENEG(1:1) TO WS-NUM-AUX
                   IF WS-NUM-AUX = SEG
                       SET SI-PROCESAR         TO TRUE
                   ELSE
                       SET NO-PROCESAR         TO TRUE
                   END-IF

               WHEN '20'
                   SET NO-EXISTE-CLIENTE       TO TRUE
                   SET NO-PROCESAR             TO TRUE

               WHEN OTHER
                   MOVE PEEC201A-COD-ERROR     TO CAA-COD-ERROR
           END-EVALUATE
      *
           .
      ******************************************************************
      * CERRAR-CURSOR
      ******************************************************************
       CERRAR-CURSOR.
      *
           IF  CED-NAC1 NOT EQUAL SPACES AND LOW-VALUES AND
               CED-NUM1 NOT EQUAL SPACES AND ZEROS
               EXEC SQL
                   CLOSE V12C10Z-V1DC0011
               END-EXEC
           ELSE
               EXEC SQL
                   CLOSE V12C10Z-V1DC0012
               END-EXEC
           END-IF
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
      ******************************************************************
      * GUARDAR-RESUMEN
      ******************************************************************
       GUARDAR-RESUMEN.
      *
           ADD CT-UNO                      TO CN-REGISTROS
      *
           IF  CN-REGISTROS EQUAL 1 AND CAA-88-ESTADO-INICIO
               PERFORM 5400-GRABAR-CABECERA
                  THRU 5400-GRABAR-CABECERA-EXIT
           END-IF
           IF  CN-REGISTROS > 5000
               MOVE 'V1A0502'              TO CAA-COD-AVISO1
               SET FIN-CURSOR              TO TRUE
           ELSE
               MOVE WS-LAST-FEC(1:4)       TO WS-GTS-FEC-OPER-AA
               MOVE WS-LAST-FEC(5:2)       TO WS-GTS-FEC-OPER-MM
               MOVE WS-LAST-FEC(7:2)       TO WS-GTS-FEC-OPER-DD
               MOVE WS-LAST-RIF            TO WS-GTS-CED-RIF
               MOVE WS-LAST-NOMBRE         TO WS-GTS-NOMBRES
               MOVE WS-MONTO-TOTAL         TO WS-GTS-MTO-ME
      *
               MOVE WS-GTS-FILA            TO WS-GTS-DATOS
               PERFORM 6100-ADD-REGISTRO-COLA
                  THRU 6100-ADD-REGISTRO-COLA-EXIT
      *
               ADD WS-MONTO-TOTAL          TO WS-TOT-GEN
           END-IF
      *
           .
      ******************************************************************
      *                     5400-GRABAR-CABECERA                       *
      ******************************************************************
       5400-GRABAR-CABECERA.
      *
           IF  SEG NOT = ZEROS
               STRING 'BANCA' DELIMITED BY SIZE
               ' ' DELIMITED BY SIZE
               VEC-BANCA(SEG) DELIMITED BY SPACE
               INTO WS-GTS-BANCA
           ELSE
               MOVE 'TODOS LOS SEGMENTOS'  TO WS-GTS-BANCA
           END-IF
      *
           MOVE FEC-FECHA-DESDE(1:4)       TO WS-GTS-F-DESD-AAAA
           MOVE FEC-FECHA-DESDE(5:2)       TO WS-GTS-F-DESD-MM
           MOVE FEC-FECHA-DESDE(7:2)       TO WS-GTS-F-DESD-DD
           MOVE FEC-FECHA-HASTA(1:4)       TO WS-GTS-F-HAST-AAAA
           MOVE FEC-FECHA-HASTA(5:2)       TO WS-GTS-F-HAST-MM
           MOVE FEC-FECHA-HASTA(7:2)       TO WS-GTS-F-HAST-DD
           MOVE WS-GTS-CABECERA1           TO WS-GTS-DATOS
           PERFORM 6100-ADD-REGISTRO-COLA
              THRU 6100-ADD-REGISTRO-COLA-EXIT
      *
           MOVE CT-GTS-LINEAH              TO WS-GTS-DATOS
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
           MOVE CT-GTS-LINEAH              TO WS-GTS-DATOS
           PERFORM 6100-ADD-REGISTRO-COLA
              THRU 6100-ADD-REGISTRO-COLA-EXIT
      *
           .
       5400-GRABAR-CABECERA-EXIT.
           EXIT.
      ******************************************************************
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
      *                     6100-ADD-REGISTRO-COLA                     *
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
      *                     6200-LEER-REGISTRO-COLA                    *
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
               WHEN DFHRESP(NORMAL)
                   CONTINUE

               WHEN DFHRESP(ITEMERR)
                   SET FIN-COLA            TO TRUE

               WHEN OTHER
                   INITIALIZE QGECABC
                   MOVE CT-ERROR-READQ       TO ABC-REFERENCIA
                   PERFORM 9999-ABEND-CICS
                      THRU 9999-ABEND-CICS-EXIT
           END-EVALUATE
      *
           .
       6200-LEER-REGISTRO-COLA-EXIT.
           EXIT.
      ******************************************************************
      *                          6000-FIN                              *
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
