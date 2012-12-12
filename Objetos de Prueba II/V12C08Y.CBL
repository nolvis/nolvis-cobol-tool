      ******************************************************************
      *                                                                *
      *  PROGRAMA: V12C08Y                                             *
      *                                                                *
      *  FECHA CREACION: 06/01/2007                                    *
      *                                                                *
      *  AUTOR: FACTORIA                                               *
      *                                                                *
      *  APLICACION: V1 - MERCADO LIBRE DE DIVISAS (V1)                *
      *                                                                *
      *  DESCRIPCION: LISTADO DE OPERACIONES                           *
      *
      ******************************************************************
      *                                                                *
      *         I D E N T I F I C A T I O N   D I V I S I O N          *
      *                                                                *
      *             INFORMACION GENERAL SOBRE EL PROGRAMA              *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.   V12C08Y.
       AUTHOR.       FACTORIA.
       DATE-WRITTEN. 02/10/2008
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
           05  WS-IMP-MONTO-TOTAL          PIC S9(13)V9(02) COMP-3.
      *
           05  WS-GTS.
               10  FILLER                  PIC X(4)    VALUE '+GTS'.
               10  WS-GTS-SUFI             PIC X(4)    VALUE SPACES.
           05  WS-COLA-AUX.
               10  FILLER                  PIC X(04)   VALUE 'V169'.
               10  WS-COLA-SUFI            PIC X(04)   VALUE SPACES.
      *
           05  WS-GTS-DATOS                PIC X(156)  VALUE SPACES.
           05  WS-DATOS-COLA               PIC X(16)   VALUE SPACES.
      *
           05  WS-GTS-FILA-AUX.
               10  WS-COL-COD-DIV-OPE      PIC 9(03).
               10  WS-COL-COD-CPT-CMP-VTA  PIC X(03).
               10  WS-COL-NUMREF           PIC 9(10).
      *
           05  WS-EIBRESP-AUX              PIC -9999.

           05  WS-LONG                     PIC S9(04) COMP.

           05  FECHA-HASTA                 PIC 9(08).
           05  FECHA-KEY                   PIC 9(08).
           05  WS-MTO-TOTAL-AUX            PIC S9(13)V9(2) COMP-3.
           05  FECHA-D.
               10  FEC-DIA                  PIC X(2).
               10  FILLER                   PIC X(1) VALUE '-'.
               10  FEC-MES                  PIC X(2).
               10  FILLER                   PIC X(1) VALUE '-'.
               10  FEC-ANNO                 PIC X(4).
      *
           05  WS-IDF-LLAVE.
               10 WS-COD-OPE-BANCO         PIC X(06).
               10 WS-FEC-OPE               PIC X(08).
               10 WS-COD-DIV-OPE           PIC S9(03)V COMP-3.
               10 WS-COD-CPT-CMP-VTA       PIC X(03).
               10 WS-NUM-REF               PIC S9(10)V USAGE COMP-3.
           05  WS-COD-RIF                  PIC X(15).
           05  WS-TIP-FORMA-PAGO           PIC X(01).
           05  WS-CON-CONCEPTO-D           PIC X(03).
           05  WS-CON-CONCEPTO-H           PIC X(03).
           05  WS-IND                      PIC S9(03)V COMP-3.
      *
       01  WS-CABECERA.
      ******************************************************************
      *     WS-CAB-LIN-01                                              *
      ******************************************************************
           05  WS-CAB-LIN-01.
               10  FILLER                  PIC X(01)   VALUE SPACES.
               10  FILLER                  PIC X(24)   VALUE SPACES.
               10  FILLER                  PIC X(30)   VALUE
               '** EXPEDIENTE DE UN CLIENTE **'.
               10  FILLER                  PIC X(47)   VALUE SPACES.
               10  FILLER                  PIC X(30)   VALUE
               '** EXPEDIENTE DE UN CLIENTE **'.
      ******************************************************************
      *     WS-CAB-LIN-02                                              *
      ******************************************************************
           05  WS-CAB-LIN-02.
               10  FILLER                  PIC X(15)   VALUE SPACES.
               10  C-2-MENSAJE-1           PIC X(50)   VALUE SPACES.
               10  FILLER                  PIC X(27)   VALUE SPACES.
               10  C-2-MENSAJE-2           PIC X(50)   VALUE SPACES.
      ******************************************************************
      *     WS-CAB-LIN-03                                              *
      ******************************************************************
           05  WS-CAB-LIN-03.
               10  FILLER                  PIC X(02)   VALUE ' -'.
               10  FILLER                  PIC X(16)
                                  VALUE  'IDENTIFICACION: '.
               10  C-3-NOM-RAZON-1         PIC X(40)   VALUE SPACES.
               10  FILLER                  PIC X(21)   VALUE SPACES.
               10  FILLER                  PIC X(16)
                                  VALUE  'IDENTIFICACION: '.
               10  C-3-NOM-RAZON-2         PIC X(40)   VALUE SPACES.
      ******************************************************************
      *     WS-CAB-LIN-04                                              *
      ******************************************************************
           05  WS-CAB-LIN-04.
               10  FILLER                  PIC X(02)   VALUE ' -'.
               10  FILLER                  PIC X(08)   VALUE 'FEC.OPE.'.
               10  FILLER                  PIC X(03)   VALUE SPACES.
               10  FILLER                  PIC X(04)   VALUE 'OFC.'.
               10  FILLER                  PIC X(08)   VALUE SPACES.
               10  FILLER                  PIC X(09)   VALUE
                                  'MON./DIV.'.
               10  FILLER                  PIC X(08)   VALUE SPACES.
               10  FILLER                  PIC X(20)   VALUE
                                  'NOMBRE DEL COMPRADOR'.
               10  FILLER                  PIC X(17)   VALUE SPACES.
               10  FILLER                  PIC X(08)   VALUE 'FEC.OPE.'.
               10  FILLER                  PIC X(03)   VALUE SPACES.
               10  FILLER                  PIC X(04)   VALUE 'OFC.'.
               10  FILLER                  PIC X(08)   VALUE SPACES.
               10  FILLER                  PIC X(09)   VALUE
                                  'MON./DIV.'.
               10  FILLER                  PIC X(11)   VALUE SPACES.
               10  FILLER                  PIC X(06)   VALUE 'CUENTA'.
               10  FILLER                  PIC X(07)   VALUE SPACES.
               10  FILLER                  PIC X(04)   VALUE 'PAIS'.
               10  FILLER                  PIC X(04)   VALUE SPACES.
               10  FILLER                  PIC X(05)   VALUE 'BANCO'.
      ******************************************************************
      *     WS-DETALLE
      ******************************************************************
           05  WS-DETALLE.
               10  FILLER                  PIC X(02)   VALUE ' B'.
               10  D-FECHA-OPE-1           PIC X(08)   VALUE SPACES.
               10  FILLER                  PIC X(02)   VALUE SPACES.
               10  D-COD-OPE-BANCO-1       PIC X(06)   VALUE SPACES.
               10  FILLER                  PIC X(02)   VALUE SPACES.
               10  D-IMP-OPE-1             PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99.
               10  FILLER                  PIC X(02)   VALUE SPACES.
               10  D-NOM-BENEF             PIC X(30)   VALUE SPACES.
               10  FILLER                  PIC X(05)   VALUE SPACES.
               10  D-MARCA                 PIC X(01)   VALUE SPACES.
               10  FILLER                  PIC X(01)   VALUE SPACES.

               10  D-FECHA-OPE-2           PIC X(08)   VALUE SPACES.
               10  FILLER                  PIC X(02)   VALUE SPACES.
               10  D-COD-OPE-BANCO-2       PIC X(06)   VALUE SPACES.
               10  FILLER                  PIC X(02)   VALUE SPACES.
               10  D-IMP-OPE-2             PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99.
               10  FILLER                  PIC X(02)   VALUE SPACES.
               10  D-CTA-PASIVO-CL         PIC X(12)   VALUE ZEROES.
               10  FILLER                  PIC X(04)   VALUE SPACES.
               10  D-COD-PAIS-CORR         PIC X(04)   VALUE ZEROES.
               10  FILLER                  PIC X(04)   VALUE SPACES.
               10  D-ENT-BANCO             PIC X(04)   VALUE ZEROES.
               10  FILLER                  PIC X(08)   VALUE SPACES.
               10  D-MARCA2                PIC X(01)   VALUE SPACES.
      ******************************************************************
      *     WS-TOTALES                                                 *
      ******************************************************************
           05  WS-TOTALES.
               10  FILLER                  PIC X(03)   VALUE ' +'.
               10  FILLER                  PIC X(10)   VALUE
                                   'TOTAL ....'.
               10  FILLER                  PIC X(07)   VALUE SPACES.
               10  C-T-MTO-TOTAL-1         PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99.
               10  FILLER                  PIC X(40)   VALUE SPACES.
               10  FILLER                  PIC X(10)   VALUE
                                   'TOTAL ....'.
               10  FILLER                  PIC X(07)   VALUE SPACES.
               10  C-T-MTO-TOTAL-2         PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99.
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
      ******************************************************************
      *                        AREA DE CONTANTES                       *
      ******************************************************************
       01  CT-CONTANTES.
      *
           05  CT-PROGRAMA                 PIC X(08)   VALUE 'V12C08Y'.
           05  CT-CASO1                    PIC X(01)   VALUE '1'.
           05  CT-CASO2                    PIC X(01)   VALUE '2'.
           05  CT-SI                       PIC X(01)   VALUE 'S'.
           05  CT-PF2                      PIC X(02)   VALUE '02'.
           05  CT-RTRNO                    PIC X(05)   VALUE 'RTRNO'.
           05  CT-INCRE-1                  PIC 9(01)   VALUE 1.
      *    05  CT-IND-DATOS                PIC 9(01)   VALUE 6.
           05  CT-UNO                      PIC 9(01)   VALUE 1.
           05  CT-TOTAL                    PIC X(01)   VALUE 'T'.
      *
           05  CT-CONTENIDO.
               10  FILLER                  PIC X(25)   VALUE SPACE.
               10  FILLER                  PIC X(22)
                                   VALUE 'LISTADO DE SOLICITUDES'.
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
           05  CT-PE9C2010                 PIC X(08) VALUE 'PE9C2010'.
           05  CT-NUM-LINEAS-CABEC         PIC 9(02)   VALUE 05.
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
      *
           COPY PEEC201A.
      *  C0PY DE CODIGOS DE RETORNO
       01  WS-BGECRET0.
           COPY BGECRET0.
      ******* TABLA MERCADO LIBRE DE DIVISA*****************************
           EXEC SQL
               INCLUDE V1GT001
           END-EXEC
      ******************************************************************
      *                        CURSORES
      ******************************************************************
           EXEC SQL
               DECLARE V12C08Y-V1DC0011
               CURSOR FOR
               SELECT COD_OPE_BANCO    ,
                      NUM_REF          ,
                      FEC_OPE          ,
                      NOM_BENEF        ,
                      CTA_PASIVO_CL    ,
                      COD_PAIS_CORR    ,
                      COD_ENT_BANCO    ,
                      NUM_DOC_PERSONA  ,
                      NOM_RAZON        ,
                      IDF_SEGURID      ,
                      COD_DIV_OPE      ,
                      TAS_DOLAR        ,
                      IMP_OPE          ,
                      COD_CPT_CMP_VTA
               FROM V1DT001
                    WHERE NUM_DOC_PERSONA = :WS-COD-RIF AND
                    NOT (TIP_OPE BETWEEN '5' AND '8') AND
                    TIP_PAGO = :WS-TIP-FORMA-PAGO AND
                    (COD_CPT_CMP_VTA BETWEEN
                       :WS-CON-CONCEPTO-D AND :WS-CON-CONCEPTO-H)
               ORDER BY NUM_DOC_PERSONA,
                        FEC_OPE,
                        NUM_REF
           END-EXEC
      *
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA.
           COPY QGECCAA.
       COPY V1EC001.
       COPY QCRMGTS.
       COPY QCRMGTS1.
      ******************************************************************
      *                                                                *
      *           P R O C E D U R E      D I V I S I O N               *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION USING  V1EC001 QCRMGTSI.
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
                                              WS-COLA-SUFI
      *
           SET CAA-88-ACCION-TERMINAL      TO TRUE
           SET CAA-88-CONTABLE-NO          TO TRUE
           MOVE CAA-CODTRAN                TO CAA-CODTRAN-SIG
           MOVE CAA-CODTRAN                TO COD-TRANSACCION
      *
           MOVE COD-RIF                    TO WS-COD-RIF
           MOVE CON-CONCEPTO-D             TO WS-CON-CONCEPTO-D
           MOVE CON-CONCEPTO-H             TO WS-CON-CONCEPTO-H
           MOVE TIP-FORMA-PAGO             TO WS-TIP-FORMA-PAGO
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
           IF  CAA-CASO = '2'
               PERFORM 2150-PREPARAR-PAGINACION
           ELSE
               PERFORM BORRAR-COLA-GTS
               PERFORM BORRAR-COLA-AUX

               PERFORM VALIDAR-RIF
               PERFORM BUSCAR-NOM-RAZOM

               PERFORM GRABAR-CABECERA

               INITIALIZE WS-IDF-LLAVE
               MOVE ZEROES TO WS-MTO-TOTAL-AUX

               PERFORM GRABAR-REGISTROS

               PERFORM 2150-PREPARAR-PAGINACION
           END-IF
      *
           SET  CAA-88-ACCION-TERMINAL TO TRUE
           SET  CAA-88-ESTADO-CONTIN   TO TRUE
           SET  CAA-88-TIPO-SALIDA-PAG TO TRUE
           MOVE CAA-CODTRAN            TO CAA-CODTRAN-SIG
      *
           .
       2100-ESTADO-INICIO-EXIT.
           EXIT.
      ******************************************************************
      *VALIDAR-RIF
      ******************************************************************
       VALIDAR-RIF.
      *
           INITIALIZE PEEC201A
           MOVE '0102'                     TO PEEC201A-PECDGENT
           MOVE SPACES                     TO PEEC201A-PENUMPER
           MOVE COD-RIF(1:1)               TO PEEC201A-PETIPDOC
           MOVE '000'                      TO PEEC201A-PENUMDOC(1:3)
           MOVE COD-RIF(8:8)               TO PEEC201A-PENUMDOC(4:8)
           MOVE '01'                       TO PEEC201A-PESECDOC
           MOVE '00'                       TO PEEC201A-IND-GRUPO
           MOVE '01'                       TO PEEC201A-PESECDOC
           MOVE '00'                       TO PEEC201A-IND-GRUPO
      *
           CALL CT-PE9C2010  USING PEEC201A
      *
           EVALUATE PEEC201A-RETORNO
               WHEN '00'
                   MOVE SPACES               TO C-2-MENSAJE-1
                                                C-2-MENSAJE-2
               WHEN '20'
                   MOVE '*** RIF NO EXISTE EN REGISTRO DE CLIENTE ***'
                                             TO C-2-MENSAJE-1
                                                C-2-MENSAJE-2
               WHEN OTHER
                   MOVE  PEEC201A-COD-ERROR TO CAA-COD-ERROR
           END-EVALUATE
      *
           .
      ******************************************************************
      * BUSCAR-NOM-RAZOM
      ******************************************************************
       BUSCAR-NOM-RAZOM.
      *
           EXEC SQL
               SELECT DISTINCT NOM_RAZON
               INTO :NOM-RAZON
               FROM V1DT001
               WHERE NUM_DOC_PERSONA = :COD-RIF
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN ZEROES
               WHEN -811
                   MOVE NOM-RAZON          TO NOM-RAZON-V161

               WHEN +100
                   MOVE  SPACES            TO NOM-RAZON-V161

               WHEN OTHER
                   INITIALIZE QGECABC
                   MOVE 'V1DT001'         TO ABC-OBJETO-ERROR
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *
           .
      ******************************************************************
      * 2150-PREPARAR-PAGINACION
      ******************************************************************
       2150-PREPARAR-PAGINACION.
      *
           INITIALIZE CAA-PAGINAR
           MOVE 'X'                        TO CAA-SEL-PERMIT(1)
           SET CAA-88-IND-VARSEL-SI        TO TRUE
      *
           MOVE '00'                       TO CAA-FKEY-NUM(1)
           MOVE 'ENTER'                    TO CAA-FKEY-LIT(1)
           MOVE 'S'                        TO CAA-FKEY-SEL(1)
           MOVE CT-CONTENIDO               TO CAA-CONTENID
      *
           SET CAA-88-IND-MOD-DATO-NO      TO TRUE
           MOVE CT-NUM-LINEAS-CABEC        TO CAA-NUM-LIN-CAB
           SET CAA-88-IND-SCROLL-SI        TO TRUE
      *
           IF  IND-MAS-DATOS-164 EQUAL CT-SI
               SET CAA-88-IND-AVPAG-SI     TO TRUE
           ELSE
               SET CAA-88-IND-AVPAG-NO     TO TRUE
           END-IF
      *
           .
      ******************************************************************
      * 3000-ESTADO-CONTINUACION
      ******************************************************************
       3000-ESTADO-CONTINUACION.
      *
           MOVE CAA-TECLA                  TO SW-PF-PRO
           EVALUATE TRUE
               WHEN PF-INTRO
                   PERFORM 3150-COMENZAR-PROCESO
                      THRU 3150-COMENZAR-PROCESO-EXIT

               WHEN PF-BORRA
                   PERFORM BORRAR-COLA-GTS
                   PERFORM BORRAR-COLA-AUX

                   SET CAA-88-CODTRAN-SIG-ULTI TO TRUE
                   SET CAA-88-ACCION-PROGRAMA  TO TRUE
                   SET CAA-88-ESTADO-INICIO    TO TRUE

               WHEN PF-PF8
                   PERFORM GRABAR-REGISTROS

                   IF  IND-MAS-DATOS-164 EQUAL CT-SI
                       SET CAA-88-IND-AVPAG-SI     TO TRUE
                   ELSE
                       SET CAA-88-IND-AVPAG-NO     TO TRUE
                   END-IF

                   SET CAA-88-ESTADO-CONTIN        TO TRUE
           END-EVALUATE
      *
           .
       3000-ESTADO-CONTINUACION-EXIT.
           EXIT.
      ******************************************************************
      *                      3150-COMENZAR-PROCESO                     *
      *  SE BUSCA EL PRIMER REGISTRO SELECCIONADO Y SE COMIENZA        *
      *  EL PROCESO                                                    *
      ******************************************************************
       3150-COMENZAR-PROCESO.
      *
           MOVE CT-NUM-LINEAS-CABEC        TO IN-REGISTRO
           ADD 1 TO IN-REGISTRO
           MOVE ZEROES                     TO CN-REGISTROS
           MOVE SPACES                     TO WS-GTS-DATOS(1:1)
           SET NO-FIN-COLA                 TO TRUE
      *
           PERFORM 3300-BUSCAR-SELECCION
              THRU 3300-BUSCAR-SELECCION-EXIT
           UNTIL WS-GTS-DATOS(1:1) NOT EQUAL SPACES
                 OR FIN-COLA
      *
           PERFORM 4900-PROCESAR-SELECCION
              THRU 4900-PROCESAR-SELECCION-EXIT
      *
           .
       3150-COMENZAR-PROCESO-EXIT.
           EXIT.
      ******************************************************************
      *                   3300-BUSCAR-SELECCION
      *    MANDA A LEER EL SIGUIENTE REGISTRO E INCREMENTA
      *    EL INDICE Y EL CONTADOR DE REGISTROS LEIDOS
      ******************************************************************
       3300-BUSCAR-SELECCION.
      *
           PERFORM LEER-REGISTRO-COLA-GTS
      *
           ADD CT-INCRE-1                  TO IN-REGISTRO
           ADD CT-INCRE-1                  TO CN-REGISTROS
      *
           .
       3300-BUSCAR-SELECCION-EXIT.
           EXIT.
      ******************************************************************
      *                    4900-PROCESAR-SELECCION                     *
      *  -DESMARCA EL REGISTRO EN LA COLA TS                           *
      *  -PREPARA LLAMADO A LA TRANSACCION DE CONSULTA                 *
      ******************************************************************
       4900-PROCESAR-SELECCION.
      *
           MOVE WS-GTS-DATOS               TO WS-DETALLE
           SUBTRACT CT-UNO                 FROM IN-REGISTRO

           IF  NO-FIN-COLA
               PERFORM BORRAR-SELECCION-GTS

               SUBTRACT CT-NUM-LINEAS-CABEC    FROM IN-REGISTRO
      *        BUSCAR DATOS EN COLA AUX
               PERFORM LEER-REGISTRO-COLA-AUX
               MOVE WS-DATOS-COLA              TO WS-GTS-FILA-AUX
      *
               IF  WS-GTS-DATOS(4:1) NOT EQUAL 'T'
                   MOVE 'V170'                 TO CAA-CODTRAN-SIG
                   SET CAA-88-ACCION-PROGRAMA  TO TRUE
                   SET CAA-88-ESTADO-INICIO    TO TRUE
                   SET CAA-88-CADENA-ANADIR    TO TRUE
                   MOVE '2'                    TO CAA-CASO-CAD
      *
                   MOVE D-COD-OPE-BANCO-1      TO COD-OPE-BANCO-V163
                   MOVE D-FECHA-OPE-1          TO FEC-OPE-V163
                   MOVE WS-COL-COD-DIV-OPE     TO COD-DIV-OPE-V163
                   MOVE WS-COL-COD-CPT-CMP-VTA TO COD-CPT-CMP-VTA-V163
                   MOVE WS-COL-NUMREF          TO NUM-REF-V163
               END-IF
           END-IF
      *
           .
       4900-PROCESAR-SELECCION-EXIT.
           EXIT.
      ******************************************************************
      * GRABAR-REGISTROS
      ******************************************************************
       GRABAR-REGISTROS.
      *
           SET NO-FIN-CURSOR               TO TRUE
           MOVE ZEROES                     TO CN-REGISTROS
           MOVE CT-SI                      TO IND-MAS-DATOS-164
      *
           PERFORM 5100-ABRIR-CURSOR
              THRU 5100-ABRIR-CURSOR-EXIT

           PERFORM 5200-LEER-CURSOR
              THRU 5200-LEER-CURSOR-EXIT
           UNTIL FIN-CURSOR

           PERFORM 5700-CERRAR-CURSOR
              THRU 5700-CERRAR-CURSOR-EXIT
      *
           IF  FIN-CURSOR
               MOVE WS-MTO-TOTAL-AUX          TO C-T-MTO-TOTAL-1
                                                 C-T-MTO-TOTAL-2
               MOVE WS-TOTALES                TO WS-GTS-DATOS
      *
               PERFORM ADD-REGISTRO-COLA-GTS
      *
           END-IF
           MOVE WS-MTO-TOTAL-AUX               TO MONTO-T-164
      *
           .
      ******************************************************************
      * 5100-ABRIR-CURSOR
      ******************************************************************
       5100-ABRIR-CURSOR.
      *
           EXEC SQL
               OPEN V12C08Y-V1DC0011
           END-EXEC
      *
           MOVE SQLCODE                    TO DB2-RETURN-CDE
           IF  NOT DB2-OK
               INITIALIZE QGECABC
               MOVE 'V1DT001'              TO ABC-OBJETO-ERROR
               PERFORM 9999-ABEND-DB2
                  THRU 9999-ABEND-DB2-EXIT
           END-IF
      *
           .
       5100-ABRIR-CURSOR-EXIT.
           EXIT.
      ******************************************************************
      * 5200-LEER-CURSOR
      ******************************************************************
       5200-LEER-CURSOR.
      *
           EXEC SQL
               FETCH V12C08Y-V1DC0011
               INTO :COD-OPE-BANCO    ,
                    :NUM-REF          ,
                    :FEC-OPE          ,
                    :NOM-BENEF        ,
                    :CTA-PASIVO-CL    ,
                    :COD-PAIS-CORR    ,
                    :COD-ENT-BANCO    ,
                    :NUM-DOC-PERSONA  ,
                    :NOM-RAZON        ,
                    :IDF-SEGURID      ,
                    :COD-DIV-OPE      ,
                    :TAS-DOLAR        ,
                    :IMP-OPE          ,
                    :COD-CPT-CMP-VTA
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
                   MOVE 'V1DT001'          TO ABC-OBJETO-ERROR
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *
           .
       5200-LEER-CURSOR-EXIT.
           EXIT.
      ******************************************************************
      * 5700-CERRAR-CURSOR
      ******************************************************************
       5700-CERRAR-CURSOR.
      *
           EXEC SQL
               CLOSE V12C08Y-V1DC0011
           END-EXEC
      *
           MOVE SQLCODE                    TO DB2-RETURN-CDE
           IF  NOT DB2-OK
               INITIALIZE QGECABC
               MOVE 'V1DT001'              TO ABC-OBJETO-ERROR
               PERFORM 9999-ABEND-DB2
                  THRU 9999-ABEND-DB2-EXIT
           END-IF
      *
           .
       5700-CERRAR-CURSOR-EXIT.
           EXIT.
      ******************************************************************
      * 5300-PROCESAR-REGISTRO
      ******************************************************************
       5300-PROCESAR-REGISTRO.
      *
           ADD CT-INCRE-1                  TO CN-REGISTROS
      *
           IF  CN-REGISTROS > 5000
               MOVE 'V1A0502'              TO CAA-COD-AVISO1
               SET FIN-CURSOR              TO TRUE
           ELSE
               MOVE COD-OPE-BANCO          TO D-COD-OPE-BANCO-1
                                              D-COD-OPE-BANCO-2

               MOVE NOM-BENEF              TO D-NOM-BENEF

      *        CUENTA
               IF  CTA-PASIVO-CL = ZEROS
                   MOVE SPACES             TO D-CTA-PASIVO-CL
               ELSE
                   MOVE CTA-PASIVO-CL      TO D-CTA-PASIVO-CL
               END-IF

      *        PAIS
               IF  COD-PAIS-CORR = ZEROS
                   MOVE SPACES             TO D-COD-PAIS-CORR
               ELSE
                   MOVE COD-PAIS-CORR      TO D-COD-PAIS-CORR
               END-IF

      *        BANCO
               IF  COD-ENT-BANCO = ZEROS
                   MOVE SPACES             TO D-ENT-BANCO
               ELSE
                   MOVE COD-ENT-BANCO      TO D-ENT-BANCO
               END-IF

      *
               IF  IDF-SEGURID = 1
                   MOVE '*'                TO D-MARCA
               END-IF
      *
               MOVE FEC-OPE(1:4)           TO D-FECHA-OPE-1 (5:4)
               MOVE FEC-OPE(5:2)           TO D-FECHA-OPE-1 (3:2)
               MOVE FEC-OPE(7:2)           TO D-FECHA-OPE-1 (1:2)
               MOVE D-FECHA-OPE-1          TO D-FECHA-OPE-2
      *
               IF COD-DIV-OPE  NOT  EQUAL  1
                   COMPUTE D-IMP-OPE-2  = IMP-OPE * TAS-DOLAR
                   COMPUTE WS-MTO-TOTAL-AUX = WS-MTO-TOTAL-AUX +
                           IMP-OPE * TAS-DOLAR
               ELSE
                   MOVE IMP-OPE            TO D-IMP-OPE-2
                   COMPUTE WS-MTO-TOTAL-AUX = WS-MTO-TOTAL-AUX + IMP-OPE
               END-IF
               MOVE D-IMP-OPE-2            TO D-IMP-OPE-1

               MOVE WS-DETALLE             TO WS-GTS-DATOS
               PERFORM ADD-REGISTRO-COLA-GTS

      *        GUARDAR LLAVE EN COLA AUXILIAR
               MOVE COD-DIV-OPE            TO WS-COL-COD-DIV-OPE
               MOVE COD-CPT-CMP-VTA        TO WS-COL-COD-CPT-CMP-VTA
               MOVE NUM-REF                TO WS-COL-NUMREF
               MOVE WS-GTS-FILA-AUX        TO WS-DATOS-COLA
               PERFORM ADD-REGISTRO-COLA-AUX
           END-IF
      *
           .
       5300-PROCESAR-REGISTRO-EXIT.
           EXIT.
      ******************************************************************
      * GRABAR-CABECERA
      ******************************************************************
       GRABAR-CABECERA.
      *
           MOVE WS-CAB-LIN-01           TO WS-GTS-DATOS
           PERFORM ADD-REGISTRO-COLA-GTS
      *
           MOVE WS-CAB-LIN-02           TO WS-GTS-DATOS
           PERFORM ADD-REGISTRO-COLA-GTS
      *
           MOVE NOM-RAZON-V161          TO C-3-NOM-RAZON-1
                                           C-3-NOM-RAZON-2
           MOVE WS-CAB-LIN-03           TO WS-GTS-DATOS
           PERFORM ADD-REGISTRO-COLA-GTS

           MOVE SPACES                  TO WS-GTS-DATOS
           PERFORM ADD-REGISTRO-COLA-GTS
      *
           MOVE WS-CAB-LIN-04           TO WS-GTS-DATOS
           PERFORM ADD-REGISTRO-COLA-GTS
      *
           .
      ******************************************************************
      * BORRAR-COLA-GTS
      *  BORRA LA COLA TS DEL LISTADO
      ******************************************************************
       BORRAR-COLA-GTS.
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
      ******************************************************************
      * ADD-REGISTRO-COLA-GTS
      ******************************************************************
       ADD-REGISTRO-COLA-GTS.
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
           INITIALIZE WS-GTS-DATOS
      *
           .
      ******************************************************************
      * LEER-REGISTRO-COLA-GTS
      ******************************************************************
       LEER-REGISTRO-COLA-GTS.
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
      ******************************************************************
      * BORRAR-SELECCION-GTS
      ******************************************************************
       BORRAR-SELECCION-GTS.
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
                 MOVE CT-ERROR-READQ       TO ABC-REFERENCIA
                 PERFORM 9999-ABEND-CICS
                    THRU 9999-ABEND-CICS-EXIT
           END-EVALUATE
      *
           .
      ******************************************************************
      * BORRAR-COLA-AUX
      ******************************************************************
       BORRAR-COLA-AUX.
      *
           EXEC CICS
               DELETEQ TS
               QUEUE(WS-COLA-AUX)
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
      ******************************************************************
      * ADD-REGISTRO-COLA-AUX
      ******************************************************************
       ADD-REGISTRO-COLA-AUX.
      *
           EXEC CICS
               WRITEQ TS
               QUEUE(WS-COLA-AUX)
               FROM(WS-DATOS-COLA)
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
      ******************************************************************
      * LEER-REGISTRO-COLA-AUX
      ******************************************************************
       LEER-REGISTRO-COLA-AUX.
      *
           EXEC CICS
               READQ TS
               QUEUE(WS-COLA-AUX)
               INTO (WS-DATOS-COLA)
               ITEM(IN-REGISTRO)
               NOHANDLE
           END-EXEC
      *
           EVALUATE EIBRESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(ITEMERR)
                   CONTINUE
               WHEN OTHER
                   INITIALIZE QGECABC
                   MOVE CT-ERROR-READQ       TO ABC-REFERENCIA
                   PERFORM 9999-ABEND-CICS
                      THRU 9999-ABEND-CICS-EXIT
           END-EVALUATE
      *
           .
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
