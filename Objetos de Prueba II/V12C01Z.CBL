      ******************************************************************
      *                                                                *
      *  PROGRAMA: V12C01Z                                             *
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
       PROGRAM-ID.   V12C01Z.
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
           05  WS-COLA.
               10  FILLER                  PIC X(04)   VALUE 'V153'.
               10  WS-COLA-SUFI            PIC X(04)   VALUE SPACES.
      *
           05  WS-PROGRAMA                 PIC X(07) VALUE SPACES.

           05  WS-CURRENT-DATE.
               10  WS-FEC-DIA-AAAAMMDD     PIC X(08)   VALUE SPACES.
               10  WS-HORA-DIA.
                   15  WS-HH-DIA           PIC X(02)   VALUE SPACES.
                   15  WS-MM-DIA           PIC X(02)   VALUE SPACES.
                   15  WS-SS-DIA           PIC X(02)   VALUE SPACES.
                   15  WS-CS-DIA           PIC X(02)   VALUE SPACES.
               10  FILLER                  PIC X(05).

      **
           05 WS-NUM-DOC-PERSONA          PIC X(15) VALUE SPACES.

           05 WS-I                 PIC 9(8)  VALUE ZEROS.
           05 WS-J                 PIC 9(2)  VALUE ZEROS.
           05 WS-PROGRAM           PIC X(8)  VALUE SPACES.
           05 WS-TOTAL-COMPRA-1    PIC 9(13)V9(2) VALUE ZEROS.
           05 WS-TOTAL-VENTA-1     PIC 9(13)V9(2) VALUE ZEROS.
           05 WS-TOTAL-COMPRA-BS   PIC 9(13)V9(2) VALUE ZEROS.
           05 WS-TOTAL-VENTA-BS    PIC 9(13)V9(2) VALUE ZEROS.
      *    * AD01-I : RECONVERSION MONETARIA
           05 WS-CT-LABEL                PIC X(03) VALUE SPACES.
           05 WS-CT-LITERAL              PIC X(17) VALUE SPACES.
           05 REDEFINES WS-CT-LITERAL.
             10 WS-CT-LIT-PLURAL          PIC X(09).
             10 WS-CT-RESTO               PIC X(08).
      *    * AD01-F
           05 WS-DESCRIPCION       PIC X(40) VALUE SPACES.
           05 FILLER REDEFINES WS-DESCRIPCION.
             10 WS-DES-OFICINA     PIC X(25).
             10 FILLER             PIC X(15).
      *    *
           05 WS-BPBSP189          PIC X(33) VALUE SPACES.
           05 REDEFINES WS-BPBSP189.
             10 WS-NUM-DOC-PERSONA-S      PIC X(15).
             10 WS-FEC-OPE-S      PIC 9(8).
             10  REDEFINES  WS-FEC-OPE-S.
               15 WS-FEC-OPE-C         PIC X(08).
             10 WS-BPNREF89-S      PIC 9(10).
      *    BEGIN NEW
           05  WS-LONG-COLA                PIC S9(4) COMP VALUE ZEROS.
           05  WS-COLA-DATOS.
               10  WS-COLA-FECHA           PIC X(08).
               10  WS-COLA-OFC             PIC X(06).
               10  WS-COLA-MTODV           PIC ZZZZZZZZZZZZ9.99.
               10  WS-COLA-DIV             PIC X(03).
               10  WS-COLA-MTO             PIC ZZZZZZZZZZZZ9.99.
               10  WS-COLA-CON             PIC X(03).
               10  WS-COLA-TT              PIC X(01).
               10  WS-COLA-REFER           PIC 9(10).
           05  WS-MONTO-DL                 PIC S9(13)V9(2).
           05  WS-MONTO-BV                 PIC S9(13)V9(2).
      ******************************************************************
      *                    AREA  DE  SWITCHES                          *
      ******************************************************************
       01  SW-SWITCHES.
      *
           05  SW-FIN-COLA                 PIC X(01)   VALUE SPACES.
               88  FIN-COLA                            VALUES 'S'.
               88  NO-FIN-COLA                         VALUES 'N'.

           05  SW-FIN-CURSOR               PIC X(01)   VALUE 'N'.
               88  FIN-CURSOR                          VALUE 'S'.
               88  NO-FIN-CURSOR                       VALUE 'N'.
      ******************************************************************
      *                        AREA DE CONTANTES                       *
      ******************************************************************
       01  CT-CONTANTES.
      *                                                                *
           05  CT-V19C001                  PIC X(08)   VALUE 'V19C001'.
           05  CT-PROGRAMA                 PIC X(08)   VALUE 'V12C01Z'.
           05  CT-VJ8CDIV0                 PIC X(08)   VALUE 'VJ8CDIV0'.
           05  CT-ESTADO-INICIO            PIC X(01)   VALUE 'I'.
           05  CT-ESTADO-CONTINUA          PIC X(01)   VALUE 'C'.
           05  CT-CASO1                    PIC X(01)   VALUE '1'.
           05  CT-CASO2                    PIC X(01)   VALUE '2'.
           05  CT-QR4CDB0                  PIC X(07)   VALUE 'QR4CDB0'.
           05  CT-OPEN                     PIC X(04)   VALUE 'OPEN'.
           05  CT-CLOSE                    PIC X(05)   VALUE 'CLOSE'.
           05  CT-FETCH                    PIC X(05)   VALUE 'FETCH'.
           05  CT-SELECT                   PIC X(06)   VALUE 'SELECT'.
           05  CT-TAB-V1DT001              PIC X(08)   VALUE 'V1DT001'.
           05  CT-SI                       PIC X(01)   VALUE 'S'.
           05  CT-INTRO                    PIC X(02)   VALUE '00'.
           05  CT-PF06                     PIC X(02)   VALUE '06'.
           05  CT-INCRE-1                  PIC 9(01)   VALUE 1.
           05  CT-UNO                      PIC 9(01)   VALUE 1.
      ******************************************************************
      *                      AREA DE CONTADORES                        *
      ******************************************************************
       01  CN-CONTADORES.
           05  CN-REGISTROS                PIC 9(05)   VALUE ZEROES.
      ******************************************************************
      *                AREA DE MENSAJES                                *
      ******************************************************************
       01  ME-MENSAJES-ERROR.
           05  ME-TECLA-INCORRECTA         PIC X(07)   VALUE 'V1E0030'.
      ******************************************************************
      *                        AREA DE INDICES
      ******************************************************************
       01  IN-INDICES.
           05  IN-REGISTRO                 PIC S9(04) COMP VALUE ZEROES.
      ******************************************************************
      *                    AREA DE INCLUDES                            *
      ******************************************************************
           EXEC SQL
               INCLUDE BKAB0003
           END-EXEC
      *
           EXEC SQL
               INCLUDE V1GT001
           END-EXEC
      ******************************************************************
      *                    COPYS UTILIZADAS                            *
      ******************************************************************
      *  COPY PARA ERRORES DB2
           COPY QCWCL20.
      *  COPY PARA ATRIBUTOS DE LA PANTALLA
           COPY QCWCI20.
      *  COPY PARA EL PROGRAMA DE ABEND
       01  WS-QGECABC-01.
           COPY QGECABC.
      *  C0PY DE CODIGOS DE RETORNO
       01  WS-BGECRET0.
           COPY BGECRET0.
      *01 WS-V1WC901.
      *    COPY V1WC901.
       COPY VJECDIV0.
      ******************************************************************
      *                        CURSORES
      ******************************************************************
           EXEC SQL
               DECLARE V12C01Z-V1DC01Z1
               CURSOR FOR
               SELECT NUM_DOC_PERSONA,
                      FEC_OPE,
                      NUM_REF,
                      IDF_SEGURID,
                      COD_OPE_BANCO,
                      NOM_RAZON,
                      NOM_BENEF,
                      IMP_OPE,
                      TAS_DOLAR,
                      TAS_BOLIVAR,
                      COD_CPT_CMP_VTA,
                      COD_DIV_OPE
               FROM V1DT001
               WHERE NUM_DOC_PERSONA = :DCLV1DT001.NUM-DOC-PERSONA AND
                     FEC_OPE = :DCLV1DT001.FEC-OPE AND
                     IDF_SEGURID = '1'
               ORDER BY FEC_OPE,
                        NUM_REF
            END-EXEC
      *
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA.
          COPY QGECCAA.
       COPY V1EC001.
       COPY V1NC200.
       01  FILLER REDEFINES V1NC200I.
           05  FILLER                      PIC X(79).
           05  FILAS OCCURS 13 TIMES.
               10 FECHAL                   PIC S9(4) COMP.
               10 FECHAF                   PIC X.
               10 FILLER REDEFINES FECHAF.
                  15 FECHAA                PIC X.
               10 FECHAI                   PIC X(8).
               10 OFCL                     PIC S9(4) COMP.
               10 OFCF                     PIC X.
               10 FILLER REDEFINES OFCF.
                  15 OFCA                  PIC X.
               10 OFCI                     PIC X(6).
               10 MTODVL                   PIC S9(4) COMP.
               10 MTODVF                   PIC X.
               10 FILLER REDEFINES MTODVF.
                  15 MTODVA                PIC X.
               10 MTODVI                   PIC X(16).
               10 DIVL                     PIC S9(4) COMP.
               10 DIVF                     PIC X.
               10 FILLER REDEFINES DIVF.
                  15 DIVA                  PIC X.
               10 DIVI                     PIC X(3).
               10 MTOL                     PIC S9(4) COMP.
               10 MTOF                     PIC X.
               10 FILLER REDEFINES MTOF.
                  15 MTOA                  PIC X.
               10 MTOI                     PIC X(16).
               10 CONL                     PIC S9(4) COMP.
               10 CONF                     PIC X.
               10 FILLER REDEFINES CONF.
                  15 CONA                  PIC X.
               10 CONI                     PIC X(3).
               10 TTL                      PIC S9(4) COMP.
               10 TTF                      PIC X.
               10 FILLER REDEFINES TTF.
                  15 TTA                   PIC X.
               10 TTI                      PIC X(1).
               10 SEL                      PIC S9(4) COMP.
               10 SEF                      PIC X.
               10 FILLER REDEFINES SEF.
                  15 SEA                   PIC X.
               10 SEI                      PIC X(1).
      ******************************************************************
      *                                                                *
      *           P R O C E D U R E      D I V I S I O N               *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION USING V1NC200I V1EC001.
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
           SET ADDRESS OF V1NC200I         TO CAA-PTR-COPYIN
           SET ADDRESS OF V1EC001          TO CAA-PTRDATA
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
           MOVE CAA-TERMINAL               TO WS-COLA-SUFI
           SET CAA-88-ACCION-TERMINAL      TO TRUE
           SET CAA-88-CONTABLE-NO          TO TRUE
           MOVE CAA-CODTRAN                TO CAA-CODTRAN-SIG
           MOVE CAA-CODTRAN                TO COD-TRANSACCION
      *
           PERFORM 2150-PROCESO-INICIAL
              THRU 2150-PROCESO-INICIAL-EXIT
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
                   PERFORM 3000-ESTADO-CONTINUACION
                      THRU 3000-ESTADO-CONTINUACION-EXIT
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
           IF  CAA-CASO = '2'
               INITIALIZE V1NC200I
               INITIALIZE CAA-CASO

               PERFORM MOSTRAR-DATOS

               SET CAA-88-ESTADO-CONTIN    TO TRUE
               SET CAA-88-ACCION-TERMINAL  TO TRUE
               INITIALIZE CAA-CASO
               PERFORM ATT-CAMPOS
               MOVE -1                     TO SEL(1)
           ELSE
               PERFORM BORRAR-COLA
               PERFORM BUSCAR-DATOS
               IF  CN-REGISTROS NOT EQUAL ZEROS
                   INITIALIZE V1NC200I

                   MOVE 1                      TO V153-PRI-REG
                   PERFORM MOSTRAR-DATOS

                   SET CAA-88-ESTADO-CONTIN    TO TRUE
                   SET CAA-88-ACCION-TERMINAL  TO TRUE
                   PERFORM ATT-CAMPOS
                   MOVE -1                     TO SEL(1)
               ELSE
                   SET CAA-88-CODTRAN-SIG-SAME TO TRUE
                   SET CAA-88-ACCION-TERMINAL  TO TRUE
                   SET CAA-88-ESTADO-CONTIN    TO TRUE
                   MOVE 'V1E0511'              TO CAA-COD-ERROR
      *            '** CLIENTE SIN OPERACIONES EN ESTA FECHA **'
               END-IF
           END-IF
      *
           .
       2100-ESTADO-INICIO-EXIT.
           EXIT.
      ******************************************************************
      *                  2150-PROCESO-INICIAL
      ******************************************************************
       2150-PROCESO-INICIAL.
      *
           MOVE FUNCTION CURRENT-DATE      TO WS-CURRENT-DATE
      *
           CALL CT-VJ8CDIV0 USING VJECDIV0

           MOVE VJE-LABEL                  TO WS-CT-LABEL

           MOVE VJE-LIT-PLURAL             TO WS-CT-LITERAL
           MOVE WS-CT-LIT-PLURAL           TO V153-CT-LIT-PLURAL
      *
           .
       2150-PROCESO-INICIAL-EXIT.
           EXIT.
      ******************************************************************
      *                    3000-ESTADO-CONTINUACION                    *
      ******************************************************************
       3000-ESTADO-CONTINUACION.
      *
           EVALUATE CAA-TECLA
               WHEN '00'
                   PERFORM TECLA-ENTER

               WHEN '05'
                   MOVE 'V154'                TO CAA-CODTRAN-SIG
                   SET CAA-88-ACCION-PROGRAMA TO TRUE
                   SET CAA-88-ESTADO-INICIO   TO TRUE
                   SET CAA-88-CADENA-ANADIR   TO TRUE
                   MOVE '2'                   TO CAA-CASO-CAD

               WHEN '06'
      *            MOVE 'V156'                TO CAA-CODTRAN-SIG
                   MOVE 'V180'                TO CAA-CODTRAN-SIG
                   SET CAA-88-ACCION-PROGRAMA TO TRUE
                   SET CAA-88-ESTADO-INICIO   TO TRUE
                   SET CAA-88-CADENA-ANADIR   TO TRUE
                   MOVE '2'                   TO CAA-CASO-CAD
                   INITIALIZE CAA-CASO

               WHEN '07'
                   IF  V153-PRI-REG - 13 < ZEROS
                       MOVE 'V1A0504'      TO CAA-COD-AVISO1
                       PERFORM REINPUT
                   END-IF
                   SUBTRACT 13 FROM V153-PRI-REG
                   PERFORM MOSTRAR-DATOS
                   PERFORM REINPUT

               WHEN '08'
                   PERFORM BUSCAR-LONG-COLA
                   IF  V153-PRI-REG + 13 > WS-LONG-COLA
                       MOVE 'V1A0505'      TO CAA-COD-AVISO1
                       PERFORM REINPUT
                   END-IF
                   ADD 13 TO V153-PRI-REG
                   PERFORM MOSTRAR-DATOS
                   PERFORM REINPUT

               WHEN '99'
                   PERFORM BORRAR-COLA
                   SET CAA-88-CODTRAN-SIG-ULTI TO TRUE
                   SET CAA-88-ACCION-PROGRAMA  TO TRUE
                   SET CAA-88-ESTADO-INICIO    TO TRUE
           END-EVALUATE
      *
           .
       3000-ESTADO-CONTINUACION-EXIT.
           EXIT.
      ******************************************************************
      * TECLA-ENTER
      ******************************************************************
       TECLA-ENTER.
      *
           PERFORM VARYING WS-I FROM 1 BY 1
           UNTIL WS-I > 13 OR SEI(WS-I) = 'X'
           END-PERFORM
      *
           IF  WS-I LESS OR EQUAL 13
      *        IR A LA TRANSACCION DE CONSULTA DETALLE
               MOVE 'V155'                 TO CAA-CODTRAN-SIG
               SET CAA-88-ACCION-PROGRAMA  TO TRUE
               SET CAA-88-ESTADO-INICIO    TO TRUE
               SET CAA-88-CADENA-ANADIR    TO TRUE
               MOVE '2'                    TO CAA-CASO-CAD
      *        BUSCAR LLAVE
               COMPUTE IN-REGISTRO = V153-PRI-REG + WS-I - 1
               PERFORM LEER-REG-COLA
               MOVE WS-COLA-REFER          TO NUM-REF-V153
               MOVE WS-COLA-DIV            TO SWIFT-DIV-V153
           ELSE
               PERFORM BUSCAR-LONG-COLA
               IF  V153-PRI-REG + 13 > WS-LONG-COLA
      *            IR A LA TRANSACCION DE TOTALES
                   MOVE 'V154'                TO CAA-CODTRAN-SIG
                   SET CAA-88-ACCION-PROGRAMA TO TRUE
                   SET CAA-88-ESTADO-INICIO   TO TRUE
                   INITIALIZE CAA-CADENA
                   PERFORM BORRAR-COLA
               ELSE
      *            MOSTRAR LA SIGUIENTE PAGINA
                   ADD 13 TO V153-PRI-REG
                   PERFORM MOSTRAR-DATOS
                   PERFORM REINPUT
               END-IF
           END-IF
      *
           .
      ******************************************************************
      * BUSCAR-DATOS
      ******************************************************************
       BUSCAR-DATOS.
      *
           SET NO-FIN-CURSOR               TO TRUE
           MOVE ZEROES                     TO CN-REGISTROS
      *
           MOVE V153-BPBCRP90              TO NUM-DOC-PERSONA
           MOVE V153-BPBFIN90              TO FEC-OPE
      *
           PERFORM ABRIR-CURSOR

           PERFORM LEER-CURSOR
             UNTIL FIN-CURSOR

           PERFORM CERRAR-CURSOR
      *
           .
      ******************************************************************
      * ABRIR-CURSOR
      ******************************************************************
       ABRIR-CURSOR.
      *
           EXEC SQL
               OPEN V12C01Z-V1DC01Z1
           END-EXEC
      *
           IF  SQLCODE NOT EQUAL ZEROES
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
           EXEC SQL
               FETCH V12C01Z-V1DC01Z1
               INTO :NUM-DOC-PERSONA,
                    :FEC-OPE,
                    :NUM-REF,
                    :IDF-SEGURID,
                    :COD-OPE-BANCO,
                    :NOM-RAZON,
                    :NOM-BENEF,
                    :IMP-OPE,
                    :TAS-DOLAR,
                    :TAS-BOLIVAR,
                    :COD-CPT-CMP-VTA,
                    :COD-DIV-OPE
           END-EXEC
      *
           MOVE SQLCODE                    TO DB2-RETURN-CDE
      *
           EVALUATE TRUE
               WHEN DB2-OK
                   PERFORM 5700-PROCESAR-REGISTRO
                      THRU 5700-PROCESAR-REGISTRO-EXIT

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
      * CERRAR-CURSOR
      ******************************************************************
       CERRAR-CURSOR.
      *
           EXEC SQL
               CLOSE V12C01Z-V1DC01Z1
           END-EXEC
      *
           IF  SQLCODE NOT EQUAL ZEROES
               INITIALIZE QGECABC
               MOVE CT-TAB-V1DT001         TO ABC-OBJETO-ERROR
               PERFORM 9999-ABEND-DB2
                  THRU 9999-ABEND-DB2-EXIT
           END-IF
      *
           .
      ******************************************************************
      *                     5700-PROCESAR-REGISTRO                     *
      ******************************************************************
       5700-PROCESAR-REGISTRO.
      *
           ADD 1                           TO CN-REGISTROS
      *
           IF  CN-REGISTROS > 5000
               SET FIN-CURSOR              TO TRUE
               MOVE 'V1A0502'              TO CAA-COD-AVISO1
           ELSE
               INITIALIZE WS-COLA-DATOS
               PERFORM PROCESAR-DATOS
               PERFORM ADD-REG-COLA
           END-IF
      *
           .
       5700-PROCESAR-REGISTRO-EXIT.
           EXIT.
      ******************************************************************
      * PROCESAR-DATOS
      ******************************************************************
       PROCESAR-DATOS.
      *
           IF  COD-OPE-BANCO NOT =  010297
               MOVE NOM-RAZON              TO V153-NOM-RAZON
               MOVE NUM-DOC-PERSONA        TO WS-NUM-DOC-PERSONA
           ELSE
               MOVE NOM-BENEF              TO V153-NOM-RAZON
               MOVE IDF-RIF-BENEF          TO WS-NUM-DOC-PERSONA
           END-IF
      *
           MOVE COD-OPE-BANCO              TO WS-COLA-OFC
           MOVE NUM-REF                    TO WS-COLA-REFER
      *
           MOVE FEC-OPE                    TO WS-COLA-FECHA
      *
           IF  COD-DIV-OPE = 1
               MOVE IMP-OPE                TO WS-MONTO-DL
           ELSE
               COMPUTE WS-MONTO-DL = IMP-OPE * TAS-DOLAR
           END-IF

           COMPUTE WS-MONTO-BV = IMP-OPE * TAS-BOLIVAR

           MOVE WS-MONTO-DL                TO WS-COLA-MTODV
           MOVE WS-MONTO-BV                TO WS-COLA-MTO

           IF  COD-CPT-CMP-VTA >= 111 AND COD-CPT-CMP-VTA <= 199
               MOVE 'C'                    TO WS-COLA-TT
               COMPUTE WS-TOTAL-COMPRA-1 =
                   WS-TOTAL-COMPRA-1 + WS-MONTO-DL
               COMPUTE WS-TOTAL-COMPRA-BS =
                  WS-TOTAL-COMPRA-BS + WS-MONTO-BV
           END-IF

           IF  COD-CPT-CMP-VTA >= 211 AND COD-CPT-CMP-VTA <= 299
               MOVE 'V'                    TO WS-COLA-TT
               COMPUTE WS-TOTAL-VENTA-1 =
                   WS-TOTAL-VENTA-1 + WS-MONTO-DL
               COMPUTE WS-TOTAL-VENTA-BS =
                   WS-TOTAL-VENTA-BS + WS-MONTO-BV
           END-IF

           MOVE COD-CPT-CMP-VTA            TO WS-COLA-CON
      *
           PERFORM BUSCAR-DIVISA
      *
           MOVE NOM-RAZON          TO V153-DESC-IDEN
           MOVE NOM-RAZON          TO V153-NOM-RAZON
           MOVE WS-TOTAL-COMPRA-1  TO V153-TOTAL-COMPRA
           MOVE WS-TOTAL-COMPRA-BS TO V153-TOTAL-COMPRA-BS
           MOVE WS-TOTAL-VENTA-1   TO V153-TOTAL-VENTA
           MOVE WS-TOTAL-VENTA-BS  TO V153-TOTAL-VENTA-BS
      *
           .
      ******************************************************************
      * BUSCAR-DIVISA
      ******************************************************************
       BUSCAR-DIVISA.
      *
           MOVE COD-DIV-OPE                TO CTBCDT03
      *
           EXEC SQL
               SELECT CTBCOD03
               INTO :CTBCOD03
               FROM BKAB0003_BASE
               WHERE CTBNDT03 = '094' AND
                     CTBCDT03 = :TABLAS-BANCO-BASE.Z-CTBCDT03
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN ZEROS
               WHEN -811
                   MOVE CTBCOD03           TO WS-COLA-DIV

               WHEN +100
                   MOVE SPACES             TO WS-COLA-DIV

               WHEN OTHER
                   MOVE SPACES             TO WS-COLA-DIV
           END-EVALUATE
      *
           .
      ******************************************************************
      * BORRAR-COLA
      ******************************************************************
       BORRAR-COLA.
      *
           EXEC CICS
               DELETEQ TS
               QUEUE(WS-COLA)
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
                   MOVE 'ERROR CICS DELETEQ' TO ABC-REFERENCIA
                   PERFORM 9999-ABEND-CICS
           END-EVALUATE
      *
           .
      ******************************************************************
      * ADD-REG-COLA
      ******************************************************************
       ADD-REG-COLA.
      *
           EXEC CICS
               WRITEQ TS
               QUEUE(WS-COLA)
               FROM(WS-COLA-DATOS)
               MAIN
               NOHANDLE
           END-EXEC
      *
           IF  EIBRESP NOT EQUAL DFHRESP(NORMAL)
               INITIALIZE QGECABC
               MOVE 'ERROR EN CICS WRITQ'  TO ABC-REFERENCIA
               PERFORM 9999-ABEND-CICS
                  THRU 9999-ABEND-CICS-EXIT
           END-IF
      *
           .
      ******************************************************************
      * LEER-REG-COLA
      ******************************************************************
       LEER-REG-COLA.
      *
           EXEC CICS
               READQ TS
               QUEUE(WS-COLA)
               INTO (WS-COLA-DATOS)
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
                   MOVE 'ERROR CICS READQ' TO ABC-REFERENCIA
                   PERFORM 9999-ABEND-CICS
                      THRU 9999-ABEND-CICS-EXIT
           END-EVALUATE
      *
           .
      ******************************************************************
      * BUSCAR-LONG-COLA
      ******************************************************************
       BUSCAR-LONG-COLA.
      *
           EXEC CICS
               READQ TS QUEUE(WS-COLA)
               INTO (WS-COLA-DATOS)
               NUMITEMS(WS-LONG-COLA)
               ITEM(1)
               NOHANDLE
           END-EXEC
           EVALUATE EIBRESP
               WHEN (DFHRESP(NORMAL))
                   CONTINUE

               WHEN DFHRESP(ITEMERR)
                   MOVE ZEROS              TO WS-LONG-COLA

               WHEN OTHER
                   INITIALIZE QGECABC
                   MOVE 'ERROR CICS READQ' TO ABC-REFERENCIA
                   PERFORM 9999-ABEND-CICS
                      THRU 9999-ABEND-CICS-EXIT
           END-EVALUATE
      *
           .
      ******************************************************************
      * MOSTRAR-DATOS
      ******************************************************************
       MOSTRAR-DATOS.
      *    CABECERA
           MOVE V153-BPBCRP90              TO CRPI
           MOVE V153-NOM-RAZON             TO NOMBREI
           MOVE WS-CT-LABEL                TO CTLA855I
      *    INICIALIZAR LINEAS DE DETALLES
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 13
               MOVE SPACES TO FECHAI(WS-I)
                              OFCI(WS-I)
                              MTODVI(WS-I)
                              DIVI(WS-I)
                              MTOI(WS-I)
                              CONI(WS-I)
                              TTI(WS-I)
                              SEI(WS-I)
           END-PERFORM
      *    DETALLES
           MOVE V153-PRI-REG               TO IN-REGISTRO
           MOVE 1 TO WS-I
           SET NO-FIN-COLA TO TRUE
           PERFORM LEER-REG-COLA
           PERFORM UNTIL FIN-COLA OR WS-I > 13
               PERFORM MOSTRAR-REGISTRO
      *
               ADD 1                       TO IN-REGISTRO
               PERFORM LEER-REG-COLA
               ADD 1                       TO WS-I
           END-PERFORM
      *
           .
      ******************************************************************
      * MOSTRAR-REGISTRO
      ******************************************************************
       MOSTRAR-REGISTRO.
      *
           MOVE WS-COLA-FECHA(1:4)         TO FECHAI(WS-I)(5:4)
           MOVE WS-COLA-FECHA(5:2)         TO FECHAI(WS-I)(3:2)
           MOVE WS-COLA-FECHA(7:2)         TO FECHAI(WS-I)(1:2)
           MOVE WS-COLA-OFC                TO OFCI(WS-I)
           MOVE WS-COLA-MTODV              TO MTODVI(WS-I)
           MOVE WS-COLA-DIV                TO DIVI(WS-I)
           MOVE WS-COLA-MTO                TO MTOI(WS-I)
           MOVE WS-COLA-CON                TO CONI(WS-I)
           MOVE WS-COLA-TT                 TO TTI(WS-I)
      *
           .
      ******************************************************************
      * REINPUT
      ******************************************************************
       REINPUT.
      *
           SET CAA-88-ACCION-TERMINAL      TO TRUE
           SET CAA-88-ESTADO-CONTIN        TO TRUE
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
      *    CABECERA
           MOVE ATRI-PRO-BRI-FST           TO CRPA
           MOVE ATRI-PRO-BRI-FST           TO NOMBREA
           MOVE ATRI-PRO-FST               TO CTLA855A
      *    LINEAS DE DETALLE
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 13
               MOVE ATRI-PRO-BRI-FST       TO FECHAA(WS-I)
                                              OFCA(WS-I)
                                              MTODVA(WS-I)
                                              DIVA(WS-I)
                                              MTOA(WS-I)
                                              CONA(WS-I)
                                              TTA(WS-I)
                                              SEA(WS-I)
      *
               IF  (FECHAI(WS-I) NOT = SPACES AND LOW-VALUES) OR
                   (OFCI(WS-I) NOT = SPACES AND LOW-VALUES) OR
                   (MTODVI(WS-I) NOT = SPACES AND LOW-VALUES)
                   MOVE ATRI-NOP-BRI-FST   TO SEA(WS-I)
               ELSE
                   MOVE ATRI-PRO-BRI-FST   TO SEA(WS-I)
               END-IF
           END-PERFORM
      *
           .
      ******************************************************************
      *                            6000-FIN                            *
      ******************************************************************
       6000-FIN.
      *
           MOVE CAA-CODTRAN                TO COD-TRANSACCION
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
