      ******************************************************************
      *                                                                *
      *  PROGRAMA: V12C02Y                                             *
      *                                                                *
      *  FECHA CREACION: 25/06/2008                                    *
      *                                                                *
      *  AUTOR: FACTORIA                                               *
      *                                                                *
      *  APLICACION: V1 -                                              *
      *                                                                *
      *  DESCRIPCION: LISTADO DE LAS OPERACIONES DE UN CLIENTE         *
      *               QUE NO VIOLAN LOS PARAMETROS                     *
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
       PROGRAM-ID.   V12C02Y.
       AUTHOR.       FACTORIA.
       DATE-WRITTEN. 12/05/2008
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
           05  WS-PROGRAMA                 PIC X(08)   VALUE SPACES.
           05  WS-TABLA                    PIC X(08)   VALUE SPACES.
      * AD01-I : RECONVERSION MONETARIA
           05  WS-CT-LITERAL               PIC X(17).
           05  WS-CT-LITERAL-RE REDEFINES WS-CT-LITERAL.
               10  WS-CT-LIT-PLURAL        PIC X(09).
               10  WS-CT-RESTO             PIC X(08).
      * AD01-F
      *
           05  WS-SQL-FECHA-DES            PIC X(08).
           05  WS-SQL-FECHA-HAS            PIC X(08).
      *
           05  WS-MONTO-EDIT               PIC ZZZZZZZZZZZZ9,99.
           05  WS-DIV-SWIFT                PIC X(03).
           05  IND-MAS-DATOS               PIC X(01).
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
      *
           05  WS-MONTO-DL                 PIC S9(13)V9(2).
           05  WS-MONTO-BV                 PIC S9(13)V9(2).
      *
           05  WS-I                        PIC 9(05).
           05  WS-COLA.
               10  FILLER                  PIC X(04)   VALUE 'V157'.
               10  WS-COLA-SUFI            PIC X(04)   VALUE SPACES.
      *
           05  WS-PRI-REG                  PIC 9(05).
      ******************************************************************
      *                    AREA  DE  SWITCHES                          *
      ******************************************************************
       01  SW-SWITCHES.
      *
           05  SW-FIN-CURSOR               PIC X(01)   VALUE 'N'.
               88  FIN-CURSOR                          VALUE 'S'.
               88  NO-FIN-CURSOR                       VALUE 'N'.
      *
           05  SW-FIN-COLA                 PIC X(01)   VALUE SPACES.
               88  FIN-COLA                            VALUES 'S'.
               88  NO-FIN-COLA                         VALUES 'N'.
      ******************************************************************
      *                        AREA DE CONTANTES                       *
      ******************************************************************
       01  CT-CONTANTES.
      *
           05  CT-PROGRAMA                 PIC X(08)   VALUE 'V12C02Y'.
           05  CT-VJ8CDIV0                 PIC X(08)   VALUE 'VJ8CDIV0'.
      ******************************************************************
      *                      AREA DE CONTADORES                        *
      ******************************************************************
       01  CN-CONTADORES.
           05  CN-REGISTROS                PIC 9(05)   VALUE ZEROES.
      ******************************************************************
      *                        AREA DE INDICES
      ******************************************************************
       01  IN-INDICES.
           05  IN-REGISTRO                 PIC S9(04) COMP VALUE ZEROES.
      ******************************************************************
      *                    COPYS UTILIZADAS                            *
      ******************************************************************
      *--- COPY PARA ATRIBUTOS DE LA PANTALLA
       COPY QCWCI20.
      *--- COPY PARA EL PROGRAMA DE ABEND
       01  WS-QGECABC-01.
           COPY QGECABC.
      *--- C0PY DE CODIGOS DE RETORNO
       01  WS-BGECRET0.
           COPY BGECRET0.
      *--- COPY CONVERSION BOLIVAR FUERTE
       COPY VJECDIV0.
      *--- COPY QCWCL20
       COPY QCWCL20.
      *--- TABLA BKAB0003_BASE
           EXEC SQL
               INCLUDE BKAB0003
           END-EXEC.
      *--- TABLA V1DT001
           EXEC SQL
               INCLUDE V1GT001
           END-EXEC.
      ******************************************************************
      * CURSORES                                                       *
      ******************************************************************
           EXEC SQL
               DECLARE V12C02A-V1DC0011
               CURSOR FOR
               SELECT COD_OPE_BANCO,
                      FEC_OPE,
                      COD_DIV_OPE,
                      COD_CPT_CMP_VTA,
                      NUM_REF,
                      NUM_DOC_PERSONA,
                      TIP_OPE,
                      TIP_PAGO,
                      NOM_RAZON,
                      IMP_OPE,
                      TAS_BOLIVAR,
                      TAS_DOLAR,
                      TAS_CAMBIO,
                      HOR_OPE,
                      FEC_VALOR,
                      IND_TIP_OPE,
                      COD_ENT_BANCO,
                      IDF_CMP_VTA,
                      CTA_PASIVO_CL,
                      IDF_RIF_BENEF,
                      IDF_SEGURID
               FROM V1DT001
               WHERE NUM_DOC_PERSONA = :NUM-DOC-PERSONA AND
                     (FEC_OPE >= :WS-SQL-FECHA-DES AND
                      FEC_OPE <= :WS-SQL-FECHA-HAS) AND
                     IDF_SEGURID = '0'
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
       COPY V1NC201.
       01  FILLER REDEFINES V1NC201I.
           05  FILLER                      PIC X(79).
           05  FILAS OCCURS 13 TIMES.
               10  FECHAL                  PIC S9(4) COMP.
               10  FECHAF                  PIC X.
               10  FILLER REDEFINES FECHAF.
                   15  FECHAA              PIC X.
               10  FECHAI                  PIC X(8).
               10  OFCL                    PIC S9(4) COMP.
               10  OFCF                    PIC X.
               10  FILLER REDEFINES OFCF.
                   15  OFCA                PIC X.
               10  OFCI                    PIC X(6).
               10  MTODVL                  PIC S9(4) COMP.
               10  MTODVF                  PIC X.
               10  FILLER REDEFINES MTODVF.
                   15  MTODVA              PIC X.
               10  MTODVI                  PIC X(16).
               10  DIVL                    PIC S9(4) COMP.
               10  DIVF                    PIC X.
               10  FILLER REDEFINES DIVF.
                   15  DIVA                PIC X.
               10  DIVI                    PIC X(3).
               10  MTOL                    PIC S9(4) COMP.
               10  MTOF                    PIC X.
               10  FILLER REDEFINES MTOF.
                   15  MTOA                PIC X.
               10  MTOI                    PIC X(16).
               10  CONL                    PIC S9(4) COMP.
               10  CONF                    PIC X.
               10  FILLER REDEFINES CONF.
                   15  CONA                PIC X.
               10  CONI                    PIC X(3).
               10  TOL                     PIC S9(4) COMP.
               10  TOF                     PIC X.
               10  FILLER REDEFINES TOF.
                   15  TOA                 PIC X.
               10  TOI                     PIC X(1).
               10  SEL                    PIC S9(4) COMP.
               10  SEF                    PIC X.
               10  FILLER REDEFINES SEF.
                   15  SEA                PIC X.
               10  SEI                    PIC X(1).
      ******************************************************************
      *                                                                *
      *           P R O C E D U R E      D I V I S I O N               *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION USING V1EC001 V1NC201I.
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
      * 1000-INICIO                                                    *
      ******************************************************************
       1000-INICIO.
      *
           SET ADDRESS OF V1EC001          TO CAA-PTRDATA
           SET ADDRESS OF V1NC201I         TO CAA-PTR-COPYIN
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
           MOVE CT-PROGRAMA                TO WS-PROGRAMA
      *
           .
       1000-INICIO-EXIT.
           EXIT.
      ******************************************************************
      * 2000-PROCESO                                                   *
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
      * 2100-ESTADO-INICIO                                             *
      ******************************************************************
       2100-ESTADO-INICIO.
      *
           EVALUATE CAA-CASO
           WHEN '2'
      *        COMENZAR EN LA PAGINA DONDE ESTABA ANTES DE SALIR
               INITIALIZE V1NC201I CAA-CASO CAA-CASO-CAD
               MOVE V157-FEC-OPE-REP(1:5)  TO WS-PRI-REG
               PERFORM MOSTRAR-DATOS

               SET CAA-88-ESTADO-CONTIN    TO TRUE
               SET CAA-88-ACCION-TERMINAL  TO TRUE
               PERFORM ATT-CAMPOS
               MOVE -1                     TO SEL(1)

           WHEN '3'
      *        COMENZAR EN LA PRIMERA PAGINA
               INITIALIZE V1NC201I CAA-CASO CAA-CASO-CAD
               MOVE 1                      TO WS-PRI-REG
               PERFORM MOSTRAR-DATOS

               SET CAA-88-ESTADO-CONTIN    TO TRUE
               SET CAA-88-ACCION-TERMINAL  TO TRUE
               PERFORM ATT-CAMPOS
               MOVE -1                     TO SEL(1)

           WHEN OTHER
               PERFORM BORRAR-COLA
               PERFORM BUSCAR-DATOS
               IF  CN-REGISTROS > 0
                   INITIALIZE V1NC201I

                   MOVE 1                      TO WS-PRI-REG
                   PERFORM MOSTRAR-DATOS

                   SET CAA-88-ESTADO-CONTIN    TO TRUE
                   SET CAA-88-ACCION-TERMINAL  TO TRUE
                   PERFORM ATT-CAMPOS
                   MOVE -1                     TO SEL(1)
               ELSE
                   SET CAA-88-CODTRAN-SIG-SAME TO TRUE
                   SET CAA-88-ACCION-TERMINAL  TO TRUE
                   SET CAA-88-ESTADO-CONTIN    TO TRUE
                   MOVE 'V1A0507'              TO CAA-COD-AVISO1
      *            '** NO EXISTE INFORMACION PARA ESTA FECHA **'
               END-IF
           END-EVALUATE
      *
           .
       2100-ESTADO-INICIO-EXIT.
           EXIT.
      ******************************************************************
      * 3000-ESTADO-CONTINUACION                                       *
      ******************************************************************
       3000-ESTADO-CONTINUACION.
      *
           MOVE V157-FEC-OPE-REP(1:5)          TO WS-PRI-REG
      *
           EVALUATE CAA-TECLA
               WHEN '00'
                   PERFORM TECLA-ENTER

               WHEN '05'
      *            MOSTRAR TOTALES
                   MOVE 'V158'                 TO CAA-CODTRAN-SIG
                   SET CAA-88-ACCION-PROGRAMA  TO TRUE
                   SET CAA-88-ESTADO-INICIO    TO TRUE
                   SET CAA-88-CADENA-ANADIR    TO TRUE
                   MOVE '2'                    TO CAA-CASO-CAD

               WHEN '07'
                   IF  WS-PRI-REG - 13 < ZEROS
                       MOVE 'V1A0504'          TO CAA-COD-AVISO1
                       PERFORM REINPUT
                   END-IF
                   SUBTRACT 13 FROM WS-PRI-REG
                   PERFORM MOSTRAR-DATOS
                   PERFORM REINPUT

               WHEN '08'
                   PERFORM BUSCAR-LONG-COLA
                   IF  WS-PRI-REG + 13 > WS-LONG-COLA
                       MOVE 'V1A0505'          TO CAA-COD-AVISO1
                       PERFORM REINPUT
                   END-IF
                   ADD 13 TO WS-PRI-REG
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
               MOVE 'V159'                 TO CAA-CODTRAN-SIG
               SET CAA-88-ACCION-PROGRAMA  TO TRUE
               SET CAA-88-ESTADO-INICIO    TO TRUE
               SET CAA-88-CADENA-ANADIR    TO TRUE
               MOVE '2'                    TO CAA-CASO-CAD
      *        BUSCAR LLAVE
               COMPUTE IN-REGISTRO = WS-PRI-REG + WS-I - 1
               PERFORM LEER-REG-COLA
      *        MOVER PARAMETROS DE LA CONSULTA
               MOVE WS-COLA-FECHA          TO V159-FEC-OPE
               MOVE WS-COLA-REFER          TO V159-NUM-REF
               MOVE WS-COLA-DIV            TO V159-SIGLA-CDI
           ELSE
               PERFORM BUSCAR-LONG-COLA
               IF  WS-PRI-REG + 13 > WS-LONG-COLA
      *            IR A LA TRANSACCION DE TOTALES
                   MOVE 'V158'                 TO CAA-CODTRAN-SIG
                   SET CAA-88-ACCION-PROGRAMA  TO TRUE
                   SET CAA-88-ESTADO-INICIO    TO TRUE
                   SET CAA-88-CADENA-ANADIR    TO TRUE
                   MOVE '3'                    TO CAA-CASO-CAD
               ELSE
                   ADD 13 TO WS-PRI-REG
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
           MOVE V180-BPBCRP90              TO NUM-DOC-PERSONA
           MOVE V180-FECHA-DESDE           TO WS-SQL-FECHA-DES
           MOVE V180-FECHA-HASTA           TO WS-SQL-FECHA-HAS
      *
           MOVE ZEROS                      TO V157-TOTAL-COMPRA-DL
                                              V157-TOTAL-COMPRA-BV
                                              V157-TOTAL-VENTA-DL
                                              V157-TOTAL-VENTA-BV
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
               OPEN V12C02A-V1DC0011
           END-EXEC
      *
           IF  SQLCODE NOT EQUAL ZEROS
               MOVE 'V1DT001'              TO WS-TABLA
               PERFORM 9999-ABEND-DB2
                  THRU 9999-ABEND-DB2-EXIT
           END-IF
      *
           .
       ABRIR-CURSOR-EXIT.
           EXIT.
      ******************************************************************
      * LEER-CURSOR
      ******************************************************************
       LEER-CURSOR.
      *
           EXEC SQL
               FETCH V12C02A-V1DC0011
               INTO :COD-OPE-BANCO,
                    :FEC-OPE,
                    :COD-DIV-OPE,
                    :COD-CPT-CMP-VTA,
                    :NUM-REF,
                    :NUM-DOC-PERSONA,
                    :TIP-OPE,
                    :TIP-PAGO,
                    :NOM-RAZON,
                    :IMP-OPE,
                    :TAS-BOLIVAR,
                    :TAS-DOLAR,
                    :TAS-CAMBIO,
                    :HOR-OPE,
                    :FEC-VALOR,
                    :IND-TIP-OPE,
                    :COD-ENT-BANCO,
                    :IDF-CMP-VTA,
                    :CTA-PASIVO-CL,
                    :IDF-RIF-BENEF,
                    :IDF-SEGURID
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN ZEROS
                   PERFORM PROCESAR-REGISTRO

               WHEN +100
                   SET FIN-CURSOR          TO TRUE

               WHEN OTHER
                   MOVE 'V1DT001'          TO WS-TABLA
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *
           .
      ******************************************************************
      * PROCESAR-REGISTRO
      ******************************************************************
       PROCESAR-REGISTRO.
      *
           ADD 1                           TO CN-REGISTROS
      *
           IF  CN-REGISTROS > 5000
               SET FIN-CURSOR              TO TRUE
               MOVE 'V1A0502'              TO CAA-COD-AVISO1
           ELSE
               INITIALIZE WS-COLA-DATOS
               PERFORM PROC-REG-DETALLE
               PERFORM ADD-REG-COLA
           END-IF
      *
           .
       5300-PROCESAR-REGISTRO-EXIT.
           EXIT.
      ******************************************************************
      * PROC-REG-DETALLE
      ******************************************************************
       PROC-REG-DETALLE.
      *
           MOVE COD-OPE-BANCO TO WS-COLA-OFC
           MOVE NUM-REF       TO WS-COLA-REFER
           MOVE FEC-OPE       TO WS-COLA-FECHA
           IF  COD-DIV-OPE = 1
               MOVE IMP-OPE   TO WS-MONTO-DL
           ELSE
               COMPUTE WS-MONTO-DL = IMP-OPE * TAS-DOLAR
           END-IF
           MOVE WS-MONTO-DL   TO WS-COLA-MTODV

           COMPUTE WS-MONTO-BV = IMP-OPE * TAS-BOLIVAR
           MOVE WS-MONTO-BV   TO WS-COLA-MTO

      *    COMPRA
           IF  COD-CPT-CMP-VTA >= 111 AND COD-CPT-CMP-VTA <= 199
               MOVE 'C'         TO WS-COLA-TT
               ADD WS-MONTO-DL  TO V157-TOTAL-COMPRA-DL
               ADD WS-MONTO-BV  TO V157-TOTAL-COMPRA-BV
           END-IF
      *    VENTA
           IF  COD-CPT-CMP-VTA >= 211 AND COD-CPT-CMP-VTA <= 299
               MOVE 'V'         TO WS-COLA-TT
               ADD WS-MONTO-DL  TO V157-TOTAL-VENTA-DL
               ADD WS-MONTO-BV  TO V157-TOTAL-VENTA-BV
           END-IF
      *
           MOVE COD-CPT-CMP-VTA TO WS-COLA-CON

      *    DIVISA
           PERFORM BUSCAR-DIV
              THRU BUSCAR-DIV-EXIT
           MOVE WS-DIV-SWIFT    TO WS-COLA-DIV
      *
           .
       5310-PROC-REG-DETALLE-EXIT.
           EXIT.
      ******************************************************************
      * BUSCAR-DIV                                                     *
      ******************************************************************
       BUSCAR-DIV.
      *
           MOVE COD-DIV-OPE TO CTBCDT03
      *
           EXEC SQL
               SELECT CTBCOD03
               INTO :CTBCOD03
               FROM BKAB0003_BASE
               WHERE CTBNDT03 = '094' AND
                     CTBCDT03 = :Z-CTBCDT03
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN ZEROS
               WHEN -811
                   MOVE CTBCOD03           TO WS-DIV-SWIFT

               WHEN +100
                   MOVE SPACES             TO WS-DIV-SWIFT

               WHEN OTHER
                   MOVE 'BKAB0003_BASE'    TO WS-TABLA
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *
           .
       BUSCAR-DIV-EXIT.
           EXIT.
      ******************************************************************
      * CERRAR-CURSOR
      ******************************************************************
       CERRAR-CURSOR.
      *
           EXEC SQL
               CLOSE V12C02A-V1DC0011
           END-EXEC
      *
           IF  SQLCODE NOT EQUAL ZEROS
               MOVE 'V1DT001'              TO WS-TABLA
               PERFORM 9999-ABEND-DB2
                  THRU 9999-ABEND-DB2-EXIT
           END-IF
      *
           .
       5700-CERRAR-CURSOR-EXIT.
           EXIT.
      ******************************************************************
      * MOSTRAR-DATOS
      ******************************************************************
       MOSTRAR-DATOS.
      *    CABECERA
           MOVE V153-BPBCRP90              TO CRPI
           MOVE V153-NOM-RAZON             TO NOMBREI
      *
           CALL CT-VJ8CDIV0 USING VJECDIV0
           MOVE VJE-LABEL        TO CTLA855I
           MOVE VJE-LIT-PLURAL   TO WS-CT-LITERAL
           MOVE WS-CT-LIT-PLURAL TO V157-LIT-PLURAL
      *    INICIALIZAR LINEAS DE DETALLES
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 13
               MOVE SPACES TO FECHAI(WS-I)
                              OFCI(WS-I)
                              MTODVI(WS-I)
                              DIVI(WS-I)
                              MTOI(WS-I)
                              CONI(WS-I)
                              TOI(WS-I)
                              SEI(WS-I)
           END-PERFORM
      *    DETALLES
           MOVE WS-PRI-REG                 TO IN-REGISTRO
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
           MOVE WS-COLA-TT                 TO TOI(WS-I)
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
                                              TOA(WS-I)
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
      * 6000-FIN                                                       *
      ******************************************************************
       6000-FIN.
      *
           MOVE CAA-CODTRAN                TO COD-TRANSACCION
           MOVE WS-PRI-REG                 TO V157-FEC-OPE-REP(1:5)
      *
           EXEC CICS
               RETURN
           END-EXEC
      *
           .
       6000-FIN-EXIT.
           EXIT.
      ******************************************************************
      *  9999-ABEND-CICS                                               *
      *  SE ABENDA LA TAREA CUANDO SE PRODUCE UN ERROR CICS            *
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
           EXEC CICS
             LINK PROGRAM('QG1CABC')
             COMMAREA (QGECABC)
             NOHANDLE
           END-EXEC
      *
           .
       9999-ABEND-CICS-EXIT.
           EXIT.
      ******************************************************************
      * 9999-ABEND-DB2                                                 *
      ******************************************************************
       9999-ABEND-DB2.
      *
           INITIALIZE QGECABC
           MOVE 'S'                        TO ABC-ABEND
           MOVE WS-PROGRAMA                TO ABC-PROGRAMA
           MOVE WS-TABLA                   TO ABC-OBJETO-ERROR
           MOVE SQLCODE                    TO ABC-SQLCODE
           MOVE SQLERRM                    TO ABC-SQLERRM
      *
           EXEC CICS
                LINK PROGRAM ('QG1CABC')
                COMMAREA (QGECABC)
           END-EXEC
      *
           .
       9999-ABEND-DB2-EXIT.
           EXIT.
      *
