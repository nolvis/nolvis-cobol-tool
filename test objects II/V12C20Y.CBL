      ******************************************************************
      *                                                                *
      *  PROGRAMA: V12C20Y                                             *
      *                                                                *
      *  FECHA CREACION: 18/06/2008                                    *
      *                                                                *
      *  AUTOR: FACTORIA                                               *
      *                                                                *
      *  APLICACION: MERCADO LIBRE DE DIVISAS. (V1) VENEZUELA.         *
      *                                                                *
      *  DESCRIPCION: PROGRAMA QUE MUESTRA EL LISTADO DE OPERACIONES   *
      *               PARA UN RIF                                      *
      ******************************************************************
      *                                                                *
      *         I D E N T I F I C A T I O N   D I V I S I O N          *
      *                                                                *
      *             INFORMACION GENERAL SOBRE EL PROGRAMA              *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.   V12C20Y.
       AUTHOR.       FACTORIA.
       DATE-WRITTEN. 18/06/2008.
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
           05 WS-I                         PIC 9(7).
           05 WS-J                         PIC 9(7).
           05 WS-PROGRAM                   PIC X(8).
           05 WS-MTO-TOTAL-AUX             PIC 9(13)V9(2).
      *    * AD01-I : RECONVERSION MONETARIA
           05 WS-LITERAL                  PIC X(17).
           05 FILLER REDEFINES WS-LITERAL.
              10 WS-CT-LIT-PLURAL         PIC X(09).
              10 WS-CT-RESTO              PIC X(08).
           05 WS-DIVISA-NAT               PIC X(08) VALUE 'VJ8NDIV0'.
      *    * AD01-F
           05 WS-RIF                      PIC X(15).
           05 FILLER REDEFINES WS-RIF.
              10 WS-NAC-RIF               PIC X(1).
              10 WS-NUM-RIF               PIC 9(14).
           05 FILLER REDEFINES WS-RIF.
              10 FILLER                   PIC X(06).
              10 WS-NUM-RIF-1             PIC 9(08).
              10 FILLER                   PIC X(01).
           05 FILLER REDEFINES WS-RIF.
              10 FILLER                   PIC X(07).
              10 WS-NUM-RIF-11            PIC 9(08).
           05 WS-NUM-DOC-PERSONA                 PIC X(15).
           05 FILLER REDEFINES WS-NUM-DOC-PERSONA.
              10 WS-NAC-RIF89             PIC X(1).
              10 WS-NUM-RIF89             PIC 9(14).
      *-----------------------------------------------------------------
      * VARIABLES CONSOLIDADO
      *-----------------------------------------------------------------
           05  WS-BPCSP020-FORMA-PAGO      PIC 9(01).
           05  WS-BPCSP020-CONCEPTO-D      PIC 9(03).
           05  WS-BPCSP020-CONCEPTO-D-ALF REDEFINES
               WS-BPCSP020-CONCEPTO-D      PIC X(03).
           05  WS-BPCSP020-CONCEPTO-H      PIC 9(03).
           05  WS-BPCSP020-CONCEPTO-H-ALF REDEFINES
               WS-BPCSP020-CONCEPTO-H      PIC X(03).
           05  WS-BPCSP020-MENSAJE         PIC X(80).
           05  WS-BPCSP020-PROCESO         PIC 9(01).
      *    COMIENZO NOLVIS
           05  WS-MONTO-OPE                PIC S9(13)V9(02).
           05  WS-MONTO-EDIT               PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  WS-CUENTA-MAPA.
               10  WS-CTA1                 PIC 9(02).
               10  SEP1                    PIC X(01)   VALUE '-'.
               10  WS-CTA2                 PIC 9(03).
               10  SEP2                    PIC X(01)   VALUE '-'.
               10  WS-CTA3                 PIC 9(07).
      *
           05  WS-COLA.
               10  FILLER                  PIC X(04)   VALUE 'V162'.
               10  WS-COLA-SUFI            PIC X(04)   VALUE SPACES.
           05  IN-REGISTRO                 PIC S9(04) COMP VALUE ZEROES.
      *
           05  WS-PRI-REG                  PIC 9(5).
           05  WS-LONG-COLA                PIC S9(4) COMP VALUE ZEROS.
           05  WS-RIF-2                    PIC X(09).
           05  WS-RIF-2-RE REDEFINES WS-RIF-2.
               10  WS-NAC-RIF-2            PIC X(01).
               10  WS-NUM-RIF-2            PIC 9(08).
           05  WS-COUNTER                  PIC 9(05) VALUE ZEROS.
      *
       01  WS-COLA-DATOS.
           05  WS-COLA-FECHA               PIC 9(08).
           05  WS-COLA-OFICINA             PIC 9(06).
           05  WS-COLA-MONTO               PIC X(20).
           05  WS-COLA-CUENTA              PIC 9(12).
           05  WS-COLA-PAIS                PIC 9(04).
           05  WS-COLA-BANCO               PIC 9(04).
           05  WS-COLA-MARCA               PIC X(01).
      *
           05  WS-COLA-DIVISA              PIC 9(03).
           05  WS-COLA-CONCEPTO            PIC X(03).
           05  WS-COLA-REFERENCIA          PIC 9(10).
      ******************************************************************
      *                    AREA  DE  SWITCHES                          *
      ******************************************************************
       01  SW-SWITCHES.
           05  SW-TECLA-PRESS              PIC X(02).
               88  PF-INTRO                           VALUE '00'.
               88  PF-CLEAR                           VALUE '99'.
               88  PF-04                              VALUE '04'.
               88  PF-07                              VALUE '07'.
               88  PF-08                              VALUE '08'.
           05  SW-FIN-CURSOR               PIC X(01)   VALUE 'N'.
               88  FIN-CURSOR                          VALUE 'S'.
               88  NO-FIN-CURSOR                       VALUE 'N'.
           05  SW-CBBSP012                 PIC X(01)   VALUE 'N'.
               88  EXISTE-CBBSP012                     VALUE 'S'.
               88  NO-EXISTE-CBBSP012                  VALUE 'N'.
           05  SW-FIN-COLA                 PIC X(01)   VALUE SPACES.
               88  FIN-COLA                            VALUES 'S'.
               88  NO-FIN-COLA                         VALUES 'N'.
      ******************************************************************
      *                        AREA DE CONTANTES                       *
      ******************************************************************
       01  CT-CONTANTES.
      *                                                                *
           05  CT-PROGRAMA                 PIC X(08)   VALUE 'V12C20Y'.
           05  CT-QG1CABC                  PIC X(07)   VALUE 'QG1CABC'.
           05  CT-TAB-V1DT001              PIC X(07)   VALUE 'V1DT001'.
           05  CT-1                        PIC 9(01)   VALUE 1.
           05  CT-2                        PIC 9(01)   VALUE 2.
           05  CT-3                        PIC 9(01)   VALUE 3.
           05  CT-4                        PIC 9(01)   VALUE 4.
           05  CT-5                        PIC 9(01)   VALUE 5.
           05  CT-6                        PIC 9(01)   VALUE 6.
           05  CT-7                        PIC 9(01)   VALUE 7.
           05  CT-ESTADO-INICIO            PIC X(01)   VALUE 'I'.
           05  CT-ESTADO-CONTINUA          PIC X(01)   VALUE 'C'.
           05  CT-SI                       PIC X(01)   VALUE 'S'.
           05  CT-X                        PIC X(01)   VALUE 'X'.
           05  CT-INTRO                    PIC X(02)   VALUE '00'.
           05  CT-AYUDA                    PIC X(02)   VALUE '10'.
           05  CT-BORRA                    PIC X(02)   VALUE '99'.
           05  CT-DOLAR                    PIC 9(03)   VALUE 001.
           05  CT-100                      PIC X(03)   VALUE '100'.
           05  CT-199                      PIC X(03)   VALUE '199'.
           05  CT-200                      PIC X(03)   VALUE '200'.
           05  CT-299                      PIC X(03)   VALUE '299'.
           05  CT-CURSOR                   PIC S9      VALUE -1.
           05  CT-TOTAL                    PIC X(08)   VALUE 'TOTAL : '.
      ******************************************************************
      *                AREA DE MENSAJES                                *
      ******************************************************************
       01  ME-MENSAJES-ERROR.
           05  ME-TECLA-INCORRECTA         PIC X(07)   VALUE 'V1E0030'.
           05  ME-ERROR-SELECCION          PIC X(07)   VALUE 'V1E0011'.
           05  ME-NO-OPERACIONES           PIC X(07)   VALUE 'V1E0062'.
      ******************************************************************
      *                             CONTADORES                         *
      ******************************************************************
       01  CN-CONTADORES.
           05  CN-SELECCION                PIC 9(01)   VALUE ZEROS.
      ******************************************************************
      *                    COPYS UTILIZADAS                            *
      ******************************************************************
      ****  COPY PARA ERRORES DB2
           COPY QRECDB2.
           COPY QCWCL20.
      ****  COPY PARA EL PROGRAMA DE ABEND
       01  WS-QGECABC-01.
           COPY QGECABC.
      **** COPY DFHAID *************************************************
           COPY DFHAID.
      **********  COPY PARA ATRIBUTOS DE LA PANTALLA *******************
           COPY QCWCI20.
      ******************************************************************
      *  INCLUDE DE LA TABLA V1DT001                                   *
      ******************************************************************
      *    EXEC SQL
      *        INCLUDE SQLCA
      *    END-EXEC
      *
           EXEC SQL
               INCLUDE V1GT001
           END-EXEC.
      ******************************************************************
      *  CURSORES
      ******************************************************************
           EXEC SQL
               DECLARE CUR-VEDT001-SIG
               CURSOR FOR
               SELECT COD_ENT_BANCO    ,
                      COD_OPE_BANCO    ,
                      COD_PAIS_CORR    ,
                      CTA_PASIVO_CL    ,
                      NUM_REF          ,
                      NUM_DOC_PERSONA  ,
                      NOM_RAZON        ,
                      IMP_OPE          ,
                      TAS_DOLAR        ,
                      FEC_OPE          ,
                      COD_DIV_OPE      ,
                      COD_CPT_CMP_VTA
               FROM V1DT001
               WHERE NUM_DOC_PERSONA  = :NUM-DOC-PERSONA AND
                     NOT (TIP_OPE BETWEEN '5' AND '8') AND
                     TIP_PAGO = :TIP-PAGO AND
                     (COD_CPT_CMP_VTA BETWEEN
                      :WS-BPCSP020-CONCEPTO-D-ALF AND
                      :WS-BPCSP020-CONCEPTO-H-ALF)
               ORDER BY FEC_OPE, NUM_REF
           END-EXEC.
      *
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA.
           COPY QGECCAA.
       COPY V1EC001.
       COPY V1NCX03.
       01  FILLER REDEFINES V1NCX03I.
           03  FILLER                      PIC X(126).
           03  TABLA1 OCCURS 13 TIMES.
               04 FECHAN                   PIC X(2).
               04 FECHAA                   PIC X(1).
               04 FECHAI                   PIC X(8).
      *
               04 OFICINAN                 PIC X(2).
               04 OFICINAA                 PIC X(1).
               04 OFICINAI                 PIC X(6).
      *
               04 MONTONN                  PIC X(2).
               04 MONTOA                   PIC X(1).
               04 MONTOI                   PIC X(20).
      *
               04 CUENTAN                  PIC X(2).
               04 CUENTAA                  PIC X(1).
               04 CUENTAI                  PIC X(14).
      *
               04 PAISN                    PIC X(2).
               04 PAISA                    PIC X(1).
               04 PAISI                    PIC X(04).
      *
               04 BANCON                   PIC X(2).
               04 BANCOA                   PIC X(1).
               04 BANCOI                   PIC X(04).
      *
               04 SELECN                    PIC X(2).
               04 SELECA                    PIC X(1).
               04 SELECI                    PIC X(1).
      *
               04 MARCAN                    PIC X(2).
               04 MARCAA                    PIC X(1).
               04 MARCAI                    PIC X(1).
      ******************************************************************
      *                                                                *
      *           P R O C E D U R E      D I V I S I O N               *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION USING V1EC001 V1NCX03I.
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
      *   - DIRECCIONAR COPY DEL MAPA Y DE COMUNICACION VE1C001        *
      *   - ACTIVAR SWITCH DE OPERACION NO CONTABLE                    *
      ******************************************************************
       1000-INICIO.
      *
           SET ADDRESS OF V1NCX03I         TO CAA-PTR-COPYIN
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
           MOVE CAA-TERMINAL               TO WS-COLA-SUFI
           MOVE COD-RIF                    TO WS-RIF
           MOVE WS-NAC-RIF                 TO WS-NAC-RIF-2
           MOVE WS-NUM-RIF(7:8)            TO WS-NUM-RIF-2
      *
           .
       1000-INICIO-EXIT.
           EXIT.
      ******************************************************************
      *                        2000-PROCESO                            *
      *   - ANALIZAR SI FUE ESTADO DE INICIO O CONTINUACION            *
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
           IF  CAA-CASO = '2'
               INITIALIZE V1NCX03I
               MOVE TO-REGISTROS           TO WS-PRI-REG
               MOVE MONTO-TOTAL            TO WS-MTO-TOTAL-AUX
               PERFORM MOSTRAR-DATOS
               SET CAA-88-ESTADO-CONTIN    TO TRUE
               SET CAA-88-ACCION-TERMINAL  TO TRUE
               INITIALIZE CAA-CASO
               PERFORM ATT-CAMPOS
               MOVE -1                     TO SELECN(01)
           ELSE
               PERFORM BORRAR-COLA
               PERFORM BUSCAR-DATOS
               IF  WS-COUNTER NOT EQUAL ZEROS
                   INITIALIZE V1NCX03I

                   MOVE 1                      TO WS-PRI-REG
                   PERFORM MOSTRAR-DATOS

                   MOVE WS-MTO-TOTAL-AUX       TO MONTO-TOTAL

                   SET CAA-88-ESTADO-CONTIN    TO TRUE
                   SET CAA-88-ACCION-TERMINAL  TO TRUE
                   PERFORM ATT-CAMPOS
                   MOVE -1                     TO SELECN(01)
               ELSE
                   SET CAA-88-CODTRAN-SIG-SAME TO TRUE
                   SET CAA-88-ACCION-TERMINAL  TO TRUE
                   SET CAA-88-ESTADO-CONTIN    TO TRUE
                   MOVE ME-NO-OPERACIONES      TO CAA-COD-ERROR
               END-IF
           END-IF
      *
           .
       2100-ESTADO-INICIO-EXIT.
           EXIT.
      ******************************************************************
      *                  3300-ESTADO-CONTINUACION                      *
      ******************************************************************
       2200-ESTADO-CONTINUACION.
      *
           MOVE TO-REGISTROS               TO WS-PRI-REG
           MOVE MONTO-TOTAL                TO WS-MTO-TOTAL-AUX
      *
           MOVE CAA-TECLA                  TO SW-TECLA-PRESS
           EVALUATE TRUE
               WHEN PF-CLEAR
                   SET CAA-88-CODTRAN-SIG-ULTI TO TRUE
                   SET CAA-88-ACCION-PROGRAMA  TO TRUE
                   SET CAA-88-ESTADO-INICIO    TO TRUE
                   PERFORM BORRAR-COLA

               WHEN PF-INTRO
                   PERFORM TECLA-ENTER

               WHEN PF-04
      *            OJO: DUDA
      *            FETCH RETURN 'CBBLC490' #RIF-2
                   MOVE 'V1A0503' TO CAA-COD-AVISO1
                   PERFORM REINPUT

               WHEN PF-07
                   IF  WS-PRI-REG - 13 < ZEROS
                       MOVE 'V1A0504'          TO CAA-COD-AVISO1
                       PERFORM REINPUT
                   END-IF
                   SUBTRACT 13 FROM WS-PRI-REG
                   PERFORM MOSTRAR-DATOS
                   PERFORM REINPUT

               WHEN PF-08
                   PERFORM BUSCAR-LONG-COLA
                   IF  WS-PRI-REG + 13 > WS-LONG-COLA
                       MOVE 'V1A0505'          TO CAA-COD-AVISO1
                       PERFORM REINPUT
                   END-IF
                   ADD 13 TO WS-PRI-REG
                   PERFORM MOSTRAR-DATOS
                   PERFORM REINPUT

               WHEN OTHER
                   MOVE ME-TECLA-INCORRECTA    TO CAA-COD-ERROR
                   PERFORM REINPUT
           END-EVALUATE
      *
           .
       2200-ESTADO-CONTINUACION-EXIT.
           EXIT.
      ******************************************************************
      * TECLA-ENTER
      ******************************************************************
       TECLA-ENTER.
      *
           PERFORM BUSCAR-SELECCION
      *
           IF  WS-I > 13
               MOVE 'V1A0506'              TO CAA-COD-AVISO1
               MOVE -1                     TO SELECN(1)
               PERFORM REINPUT
           END-IF

           IF  WS-I > 0 AND WS-I < 14
               MOVE 'V163'                 TO CAA-CODTRAN-SIG
               SET CAA-88-ACCION-PROGRAMA  TO TRUE
               SET CAA-88-ESTADO-INICIO    TO TRUE
               SET CAA-88-CADENA-ANADIR    TO TRUE
               MOVE 2                      TO CAA-CASO-CAD
      *
               COMPUTE IN-REGISTRO = WS-PRI-REG + WS-I - 1
               PERFORM LEER-REG-COLA

               MOVE WS-COLA-OFICINA    TO COD-OPE-BANCO-V163
               MOVE WS-COLA-FECHA      TO FEC-OPE-V163
               MOVE WS-COLA-DIVISA     TO COD-DIV-OPE-V163
               MOVE WS-COLA-CONCEPTO   TO COD-CPT-CMP-VTA-V163
               MOVE WS-COLA-REFERENCIA TO NUM-REF-V163
           END-IF
      *
           .
      ******************************************************************
      * BUSCAR-SELECCION
      ******************************************************************
       BUSCAR-SELECCION.
      *
           MOVE ZEROS TO WS-I
           PERFORM VARYING WS-I FROM 1 BY 1
           UNTIL WS-I > 13 OR SELECI(WS-I) = 'X'
               IF  SELECI(WS-I) NOT = SPACES AND LOW-VALUES AND 'X'
                   MOVE 'V1A0506'      TO CAA-COD-AVISO1
                   MOVE -1             TO SELECN(WS-I)
                   PERFORM REINPUT
               END-IF
           END-PERFORM
      *
           .
      ******************************************************************
      * BUSCAR-DATOS
      ******************************************************************
       BUSCAR-DATOS.
      *
           SET NO-FIN-CURSOR               TO TRUE
      *
           PERFORM 5100-ABRIR-CURSOR-SIG
              THRU 5100-ABRIR-CURSOR-SIG-EXIT

           MOVE ZEROS                      TO WS-COUNTER
                                              WS-MTO-TOTAL-AUX
           PERFORM 5200-LEER-CURSOR-SIG
              THRU 5200-LEER-CURSOR-SIG-EXIT
             UNTIL FIN-CURSOR

           PERFORM 5400-CERRAR-CURSOR-SIG
              THRU 5400-CERRAR-CURSOR-SIG-EXIT
      *
           .
      ******************************************************************
      *                   5100-ABRIR-CURSOR-SIG
      ******************************************************************
       5100-ABRIR-CURSOR-SIG.
      *
           MOVE WS-NAC-RIF                 TO WS-NAC-RIF89
           MOVE WS-NUM-RIF                 TO WS-NUM-RIF89
      *
           MOVE WS-NUM-DOC-PERSONA         TO NUM-DOC-PERSONA
           MOVE TIP-FORMA-PAGO             TO TIP-PAGO
                                              WS-BPCSP020-FORMA-PAGO
           MOVE CON-CONCEPTO-D             TO WS-BPCSP020-CONCEPTO-D-ALF
           MOVE CON-CONCEPTO-H             TO WS-BPCSP020-CONCEPTO-H-ALF
      *
           EXEC SQL
               OPEN CUR-VEDT001-SIG
           END-EXEC
      *
           MOVE SQLCODE                    TO DB2-RETURN-CDE
           IF  NOT DB2-OK
               INITIALIZE QGECABC
               MOVE 'VEDT001-BASE'         TO ABC-OBJETO-ERROR
               PERFORM 9999-ABEND-DB2
                  THRU 9999-ABEND-DB2-EXIT
           END-IF
      *
           .
       5100-ABRIR-CURSOR-SIG-EXIT.
           EXIT.
      ******************************************************************
      *                       5200-LEER-CURSOR                         *
      ******************************************************************
       5200-LEER-CURSOR-SIG.
      *
           EXEC SQL
               FETCH CUR-VEDT001-SIG
                   INTO :COD-ENT-BANCO    ,
                        :COD-OPE-BANCO    ,
                        :COD-PAIS-CORR    ,
                        :CTA-PASIVO-CL    ,
                        :NUM-REF          ,
                        :NUM-DOC-PERSONA  ,
                        :NOM-RAZON        ,
                        :IMP-OPE          ,
                        :TAS-DOLAR        ,
                        :FEC-OPE          ,
                        :COD-DIV-OPE      ,
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

               WHEN OTHER
                   INITIALIZE QGECABC
                   MOVE 'VEDT001-BASE'     TO ABC-OBJETO-ERROR
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *
           .
       5200-LEER-CURSOR-SIG-EXIT.
           EXIT.
      ******************************************************************
      *                     5300-PROCESAR-REGISTRO                     *
      ******************************************************************
       5300-PROCESAR-REGISTRO.
      *
           ADD 1                           TO WS-COUNTER
      *
           IF  WS-COUNTER > 5000
               SET FIN-CURSOR              TO TRUE
               MOVE 'V1A0502'              TO CAA-COD-AVISO1
           ELSE
               INITIALIZE WS-COLA-DATOS
               MOVE FEC-OPE(1:4)           TO WS-COLA-FECHA(5:4)
               MOVE FEC-OPE(5:2)           TO WS-COLA-FECHA(3:2)
               MOVE FEC-OPE(7:2)           TO WS-COLA-FECHA(1:2)
               MOVE COD-OPE-BANCO          TO WS-COLA-OFICINA
               MOVE CTA-PASIVO-CL          TO WS-COLA-CUENTA
               MOVE COD-PAIS-CORR          TO WS-COLA-PAIS
               MOVE COD-ENT-BANCO          TO WS-COLA-BANCO
               IF  IDF-SEGURID = 1
                   MOVE '*'                TO WS-COLA-MARCA
               END-IF
      *
               IF  COD-DIV-OPE NOT = 1
                   COMPUTE WS-MONTO-OPE = IMP-OPE * TAS-DOLAR
               ELSE
                   COMPUTE WS-MONTO-OPE = IMP-OPE
               END-IF
               COMPUTE WS-MTO-TOTAL-AUX =
                      WS-MTO-TOTAL-AUX + WS-MONTO-OPE
               MOVE WS-MONTO-OPE           TO WS-MONTO-EDIT
               MOVE WS-MONTO-EDIT          TO WS-COLA-MONTO
      *
               MOVE COD-DIV-OPE            TO WS-COLA-DIVISA
               MOVE COD-CPT-CMP-VTA        TO WS-COLA-CONCEPTO
               MOVE NUM-REF                TO WS-COLA-REFERENCIA
      *
               MOVE NOM-RAZON              TO NOMBRE-V162

               PERFORM ADD-REG-COLA
           END-IF
      *
           .
       5300-PROCESAR-REGISTRO-EXIT.
           EXIT.
      ******************************************************************
      *                      5700-CERRAR-CURSOR                        *
      ******************************************************************
       5400-CERRAR-CURSOR-SIG.
      *
           EXEC SQL
               CLOSE CUR-VEDT001-SIG
           END-EXEC
      *
           MOVE SQLCODE                    TO DB2-RETURN-CDE
           IF  NOT DB2-OK
               INITIALIZE QGECABC
               MOVE 'VEDT001-BASE'         TO ABC-OBJETO-ERROR
               PERFORM 9999-ABEND-DB2
                  THRU 9999-ABEND-DB2-EXIT
           END-IF
      *
           .
       5400-CERRAR-CURSOR-SIG-EXIT.
           EXIT.
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
           MOVE COD-RIF                    TO BPBC519I
           MOVE NOMBRE-V162                TO BPBN537I
      *    INICIALIZAR LINEAS DE DETALLES
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 13
               MOVE SPACES TO FECHAI(WS-I)
                              OFICINAI(WS-I)
                              MONTOI(WS-I)
                              CUENTAI(WS-I)
                              PAISI(WS-I)
                              BANCOI(WS-I)
                              SELECI(WS-I)
                              MARCAI(WS-I)
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
      *    TOTALES
           MOVE 'TOTAL ....'               TO TOTA212I
           MOVE WS-MTO-TOTAL-AUX           TO WS-MONTO-EDIT
           MOVE WS-MONTO-EDIT              TO MTO2121I
      *
           .
      ******************************************************************
      * MOSTRAR-REGISTRO
      ******************************************************************
       MOSTRAR-REGISTRO.
      *
           MOVE WS-COLA-FECHA              TO FECHAI(WS-I)
           MOVE WS-COLA-OFICINA            TO OFICINAI(WS-I)
           MOVE WS-COLA-MONTO              TO MONTOI(WS-I)
      *    CUENTA
           IF  WS-COLA-CUENTA = ZEROS
               MOVE SPACES                 TO CUENTAI(WS-I)
           ELSE
               MOVE WS-COLA-CUENTA(1:2)    TO WS-CTA1
               MOVE WS-COLA-CUENTA(3:3)    TO WS-CTA2
               MOVE WS-COLA-CUENTA(6:7)    TO WS-CTA3
               MOVE WS-CUENTA-MAPA         TO CUENTAI(WS-I)
           END-IF

      *    PAIS
           PERFORM VARYING WS-J FROM 1 BY 1
           UNTIL WS-J > 4 OR WS-COLA-PAIS(WS-J:1) NOT = '0'
           END-PERFORM
           MOVE WS-COLA-PAIS(WS-J:4 - WS-J + 1) TO PAISI(WS-I)

      *    BANCO
           PERFORM VARYING WS-J FROM 1 BY 1
           UNTIL WS-J > 4 OR WS-COLA-BANCO(WS-J:1) NOT = '0'
           END-PERFORM
           MOVE WS-COLA-BANCO(WS-J:4 - WS-J + 1) TO BANCOI(WS-I)
      *    MARCA
           MOVE WS-COLA-MARCA              TO MARCAI(WS-I)
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
           MOVE ATRI-PRO-BRI-FST           TO MENS415A
           MOVE ATRI-PRO-BRI-FST           TO BPBC519A
                                              BPBN537A
      *    LINEAS DE DETALLE
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 13
               MOVE ATRI-PRO-BRI-FST       TO FECHAA(WS-I)
                                              OFICINAA(WS-I)
                                              MONTOA(WS-I)
                                              CUENTAA(WS-I)
                                              PAISA(WS-I)
                                              BANCOA(WS-I)
                                              MARCAA(WS-I)
               IF  (FECHAI(WS-I)   NOT = SPACES AND LOW-VALUES) OR
                   (OFICINAI(WS-I) NOT = SPACES AND LOW-VALUES) OR
                   (MONTOI(WS-I)   NOT = SPACES AND LOW-VALUES)
                   MOVE ATRI-NOP-BRI-FST       TO SELECA(WS-I)
               ELSE
                   MOVE ATRI-PRO-BRI-FST       TO SELECA(WS-I)
               END-IF
           END-PERFORM
      *    LINEA DE TOTALES
           MOVE ATRI-PRO-FST               TO TOTA212A
                                              MTO2121A
      *
           .
      ******************************************************************
      *                            6000-FIN                            *
      ******************************************************************
       6000-FIN.
      *
           MOVE CAA-CODTRAN                TO COD-TRANSACCION
           MOVE WS-PRI-REG                 TO TO-REGISTROS
           MOVE WS-MTO-TOTAL-AUX           TO MONTO-TOTAL
      *
           EXEC CICS
               RETURN
           END-EXEC
      *
           .
       6000-FIN-EXIT.
           EXIT.
      ******************************************************************
      *                       9999-ABEND-DB2                           *
      *    ABENDAR CUANDO SE PRODUCE UN ERROR DB2                      *
      ******************************************************************
       9999-ABEND-DB2.
      *
           MOVE  CT-SI                     TO ABC-ABEND
           MOVE  CT-PROGRAMA               TO ABC-PROGRAMA
           MOVE  CT-TAB-V1DT001            TO ABC-OBJETO-ERROR
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
