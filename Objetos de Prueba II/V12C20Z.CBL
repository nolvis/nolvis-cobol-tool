      ******************************************************************
      *                                                                *
      *  PROGRAMA: V12C20Z                                             *
      *                                                                *
      *  FECHA CREACION: 07/01/2008                                    *
      *                                                                *
      *  AUTOR: FACTORIA                                               *
      *                                                                *
      *  APLICACION: MERCADO LIBRE DE DIVISAS. (V1) VENEZUELA.         *
      *                                                                *
      *  DESCRIPCION: PROGRAMA QUE MUESTRA EL RESUMEN DE OPERACIONES   *
      *               POR RIF                                          *
      *                                                                *
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
       PROGRAM-ID.   V12C20Z.
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
           05 WS-VARNUM15             PIC Z.ZZZ.ZZZ.ZZZ.ZZ9,99.
           05 WS-VARALF15 REDEFINES WS-VARNUM15 PIC X(20).
      *
           05 WS-RIF                           PIC X(015).
           05 FILLER REDEFINES WS-RIF.
              10 WS-NAC-RIF                    PIC X(001).
              10 WS-NUM-RIF                    PIC 9(014).
           05 FILLER REDEFINES WS-RIF.
              10 FILLER                        PIC X(006).
              10 WS-NUM-RIF2                   PIC 9(008).
              10 FILLER                        PIC X(001).
           05 FILLER REDEFINES WS-RIF.
              10 FILLER                        PIC X(007).
              10 WS-NUM-RIF3                   PIC 9(008).
           05 WS-FORMA-PAGO                    PIC 9(001).
           05 WS-COD-CPT-CMP-VTA-D             PIC 9(003).
           05 WS-COD-CPT-CMP-VTA-H             PIC 9(003).
           05 WS-MENSAJE-ERROR                 PIC X(080).
           05 WS-IND-PROCESO                   PIC 9(001).
           05 VARIABLES-DE-TABLA.
              10 WS-COD-PAIS-CORR              PIC 9(004).
              10 FILLER REDEFINES WS-COD-PAIS-CORR.
                 15 COD-PAIS-CORR-1            PIC 9(001).
                 15 COD-PAIS-CORR-2            PIC 9(003).
              10 WS-COD-PAIS-DEST              PIC 9(004).
              10 FILLER REDEFINES WS-COD-PAIS-DEST.
                 15 COD-PAIS-DEST-1            PIC 9(001).
                 15 COD-PAIS-DEST-2            PIC 9(003).
              10 WS-COD-OPE-BANCO              PIC X(006).
              10 FILLER REDEFINES WS-COD-OPE-BANCO.
                 15 WS-UNI-COF89               PIC 9(003).
                 15 WS-OFC-COF89               PIC 9(003).
              10 WS-FEC-OPE                    PIC 9(008).
              10 FILLER REDEFINES WS-FEC-OPE.
                 15 WS-AA-FIN89                PIC 9(004).
                 15 WS-MM-FIN89                PIC 9(002).
                 15 WS-DD-FIN89                PIC 9(002).
           05 WS-X                             PIC 9(007).
           05 WS-PROGRAM                       PIC X(008).
           05 WS-NOM-RAZON                     PIC X(040).
           05 WS-MTO-TOTAL                     PIC 9(013)V9(02) COMP-3.
           05 WS-MTO-TOTAL-AUX                 PIC 9(013)V9(02) COMP-3.
           05 WS-TOTAL                         PIC X(010).
           05 WS-RIF-2                         PIC X(009).
           05 FILLER REDEFINES WS-RIF-2.
              10 WS-NAC-RIF-2                  PIC X(001).
              10 WS-NUM-RIF-2                  PIC 9(008).
           05 WS-NUM-DOC-PERSONA               PIC X(015).
           05 FILLER REDEFINES WS-NUM-DOC-PERSONA.
              10 WS-NAC-RIF89                  PIC X(001).
              10 WS-NUM-RIF89                  PIC 9(014).
           05 FILLER REDEFINES WS-NUM-DOC-PERSONA.
              10 NUM-DOC-PERSONA-NAC           PIC X(001).
              10 NUM-DOC-PERSONA-NUM           PIC 9(014).
           05 FILLER REDEFINES WS-NUM-DOC-PERSONA.
              10 FILLER                        PIC X(006).
              10 NUM-DOC-PERSONA-NUM2          PIC 9(008).
              10 FILLER                        PIC X(001).
           05 FILLER REDEFINES WS-NUM-DOC-PERSONA.
              10 FILLER                        PIC X(007).
              10 NUM-DOC-PERSONA-NUM3          PIC 9(008).
           05 WS-BPBSP189                      PIC X(033).
           05 FILLER REDEFINES WS-BPBSP189.
              10 WS-NUM-DOC-PERSONA-S                 PIC X(015).
              10 WS-FEC-OPE-S                 PIC X(008).
              10 WS-NUM-REF-S                 PIC S9(10)V USAGE COMP-3.
           05 WS-BPBSP189-H                   PIC X(033).
           05 FILLER REDEFINES WS-BPBSP189-H.
              10 WS-NUM-DOC-PERSONA-SH        PIC X(015).
              10 WS-FEC-OPE-SH                PIC X(008).
              10 WS-NUM-REF-SH                PIC S9(10)V USAGE COMP-3.
           05 WS-MENSAJE                      PIC X(050).
           05 WS-CBBSP012-PARAMETROS.
              10 WS-CBBSP012-RIF              PIC X(009).
              10 FILLER REDEFINES WS-CBBSP012-RIF.
                 15 WS-CBBSP012-NAC-RIF        PIC X(001).
                 15 WS-CBBSP012-NUM-RIF        PIC 9(008).
              10 WS-CBBSP012-NOMBRE            PIC X(060).
              10 WS-CBBSP012-DIRECCION         PIC X(180).
              10 WS-CBBSP012-TELEFONOS         PIC 9(011).
              10 WS-CBBSP012-E-MAIL            PIC X(050).
      * ----------------------------------------------------------------
      * VARIABLES CONSOLIDADO
      * ----------------------------------------------------------------
       01  TB-TABLAS.
           05 WS-FORMA-PAGO-X.
              10 WS-COD-FORMA-PAGO       OCCURS 13 TIMES
                                               PIC X(001).
           05 FILLER REDEFINES WS-FORMA-PAGO-X.
              10 WS-COD-FORMA-PAGO-N  OCCURS 13 TIMES
                                               PIC 9(001).
           05 WS-DES-FORMA-PAGO       OCCURS 13 TIMES
                                               PIC X(025).
           05 WS-DES-OPERACION        OCCURS 13 TIMES
                                               PIC X(010).
           05 WS-MONTO-OPERACION      OCCURS 13 TIMES
                                               PIC 9(13)V9(02) COMP-3.
           05 WS-VEC-SEL              OCCURS 13 TIMES
                                               PIC X(001).
           05 WS-TOTAL-OPERACION               PIC 9(013)V9(02) COMP-3.
           05 WS-FIN                           PIC 9(001).
           05 WS-CONTADOR-OPE                  PIC 9(009).
           05 WS-Z                             PIC 9(009).
           05 WS-TOTAL-EFECTIVO                PIC 9(013)V9(02) COMP-3.
           05 WS-TOTAL-CHEQUE                  PIC 9(013)V9(02) COMP-3.
           05 WS-TOTAL-TRANSFERENCIA           PIC 9(013)V9(02) COMP-3.
           05 WS-TOTAL-CHEQUE-VIAJERO          PIC 9(013)V9(02) COMP-3.
      *
           05 WS-BPCSP020-CONCEPTO-D      PIC 9(03).
           05 WS-BPCSP020-CONCEPTO-D-ALF REDEFINES
              WS-BPCSP020-CONCEPTO-D      PIC X(03).
           05 WS-BPCSP020-CONCEPTO-H      PIC 9(03).
           05 WS-BPCSP020-CONCEPTO-H-ALF REDEFINES
              WS-BPCSP020-CONCEPTO-H      PIC X(03).
      ******************************************************************
      *                    AREA  DE  SWITCHES                          *
      ******************************************************************
       01  SW-SWITCHES.
           05  SW-FIN-CURSOR               PIC X(01)   VALUE 'N'.
               88  FIN-CURSOR                          VALUE 'S'.
               88  NO-FIN-CURSOR                       VALUE 'N'.

           05  SW-SELECCION                PIC X(01)   VALUE 'N'.
               88 SI-SELECCION                         VALUE 'S'.
               88 NO-SELECCION                         VALUE 'N'.
      ******************************************************************
      *                        AREA DE CONTANTES                       *
      ******************************************************************
       01  CT-CONTANTES.
      *                                                                *
           05  CT-PROGRAMA                 PIC X(08)   VALUE 'V12C20Z'.
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
           05  CT-TRAN-V160                PIC X(04)   VALUE 'V160'.
           05  CT-TRAN-V161                PIC X(04)   VALUE 'V161'.
           05  CT-TRAN-V162                PIC X(04)   VALUE 'V162'.
           05  CT-TRAN-V164                PIC X(04)   VALUE 'V164'.
           05  CT-TRAN-V165                PIC X(04)   VALUE 'V165'.
           05  CT-TRAN-V168                PIC X(04)   VALUE 'V168'.
           05  CT-TRAN-V169                PIC X(04)   VALUE 'V169'.
           05  CT-TRAN-V175                PIC X(04)   VALUE 'V175'.
           05  CT-INTRO                    PIC X(02)   VALUE '00'.
           05  CT-AYUDA                    PIC X(02)   VALUE '10'.
           05  CT-BORRA                    PIC X(02)   VALUE '99'.
           05  CT-DOLAR                    PIC 9(03)   VALUE 001.
           05  CT-100                      PIC X(03)   VALUE '100'.
           05  CT-199                      PIC X(03)   VALUE '199'.
           05  CT-200                      PIC X(03)   VALUE '200'.
           05  CT-299                      PIC X(03)   VALUE '299'.
           05  CT-CURSOR                   PIC S9      VALUE -1.
           05  CT-EFECTIVO                 PIC X(08)   VALUE 'EFECTIVO'.
           05  CT-CHEQUES                  PIC X(07)   VALUE 'CHEQUES'.
           05  CT-TRANSFERENCIA            PIC X(13)   VALUE
                                   'TRANSFERENCIA'.
           05  CT-CHEQUES-VIA              PIC X(18)   VALUE
                                   'CHEQUES DE VIAJERO'.
           05  CT-COMPRA                   PIC X(06)   VALUE 'COMPRA'.
           05  CT-VENTA                    PIC X(05)   VALUE 'VENTA'.
           05  CT-RECIBIDA                 PIC X(08)   VALUE 'RECIBIDA'.
           05  CT-ENVIADA                  PIC X(07)   VALUE 'ENVIADA'.
           05  CT-TOTAL                    PIC X(08)   VALUE 'TOTAL : '.
           05  CT-SELECCION                PIC X(09)   VALUE
                                   'SELECCION'.
      ******************************************************************
      *                VARIABLES DE TRABAJO
      ******************************************************************
       01  VARIABLES-NEW.
           05  MONT849-TEMP1                   PIC X(20).
           05  FILLER REDEFINES MONT849-TEMP1.
               10 FILLER                       PIC X(02).
               10 MONT849-TEMPO1               PIC 9(16)V9(02).
           05  MONT949-TEMP2                   PIC X(20).
           05  FILLER REDEFINES MONT949-TEMP2.
               10 FILLER                       PIC X(02).
               10 MONT949-TEMPO2               PIC 9(16)V9(02).
           05  MON1149-TEMP3                   PIC X(20).
           05  FILLER REDEFINES MON1149-TEMP3.
               10 FILLER                       PIC X(02).
               10 MON1149-TEMPO3               PIC 9(16)V9(02).
           05  MON1249-TEMP4                   PIC X(20).
           05  FILLER REDEFINES MON1249-TEMP4.
               10 FILLER                       PIC X(02).
               10 MON1249-TEMPO4               PIC 9(16)V9(02).
           05  MON1449-TEMP5                   PIC X(20).
           05  FILLER REDEFINES MON1449-TEMP5.
               10 FILLER                       PIC X(02).
               10 MON1449-TEMPO5               PIC 9(16)V9(02).
           05  MON1549-TEMP6                   PIC X(20).
           05  FILLER REDEFINES MON1549-TEMP6.
               10 FILLER                       PIC X(02).
               10 MON1549-TEMPO6               PIC 9(16)V9(02).
           05  MON1749-TEMP7                   PIC X(20).
           05  FILLER REDEFINES MON1749-TEMP7.
               10 FILLER                       PIC X(02).
               10 MON1749-TEMPO7               PIC 9(16)V9(02).
      ******************************************************************
      *                AREA DE MENSAJES                                *
      ******************************************************************
       01  ME-MENSAJES-ERROR.
           05  ME-TECLA-INCORRECTA         PIC X(07)   VALUE 'V1E0030'.
           05  ME-ERROR-SELECCION          PIC X(07)   VALUE 'V1E0503'.
           05  ME-OPERACIONES-FORMA        PIC X(07)   VALUE 'V1E0502'.
           05  ME-UNA-SELECCION            PIC X(07)   VALUE 'V1E0500'.
           05  ME-SOLO-UNA-SELECCION       PIC X(07)   VALUE 'V1E0501'.
      ******************************************************************
      *                             CONTADORES                         *
      ******************************************************************
       01  CN-CONTADORES.
           05  CN-CONTADOR-OPE             PIC 9(09)   VALUE ZEROS.
           05  CN-SELECCION                PIC 9(01)   VALUE ZEROS.
           05  COD-TEMP                    PIC 9(03)   VALUE ZEROS.
      ******************************************************************
      *                             INDICES                            *
      ******************************************************************
       01  IN-INDICES.
           05  IN-I                        PIC S9(01)  COMP-3
                                                       VALUE ZEROS.
      ******************************************************************
      *                    COPYS UTILIZADAS                            *
      ******************************************************************
      ****  COPY PARA ERRORES DB2
           COPY QCWCL20.
      ****  COPY PARA EL PROGRAMA DE ABEND
       01  WS-QGECABC-01.
           COPY QGECABC.
      **** COPY DFHAID *************************************************
           COPY DFHAID.
      **********  COPY PARA ATRIBUTOS DE LA PANTALLA *******************
           COPY DFHBMSCA.
       COPY QCWCI20.
      ******************************************************************
      *  INCLUDE DE LA TABLA V1DT001                                   *
      ******************************************************************
           EXEC SQL
               INCLUDE V1GT001
           END-EXEC.
      ******************************************************************
      *  CURSORES
      ******************************************************************
           EXEC SQL
               DECLARE CUR-VEDT001
               CURSOR FOR
               SELECT COD_ENT_BANCO,
                      COD_OPE_BANCO,
                      COD_DIV_OPE,
                      COD_PAIS_CORR,
                      CTA_PASIVO_CL,
                      NUM_REF,
                      NUM_DOC_PERSONA,
                      NOM_RAZON,
                      IMP_OPE,
                      TAS_DOLAR,
                      FEC_OPE,
                      COD_CPT_CMP_VTA,
                      TIP_PAGO
               FROM V1DT001
               WHERE NUM_DOC_PERSONA = :NUM-DOC-PERSONA AND
                     (COD_CPT_CMP_VTA BETWEEN '100' AND '299') AND
                     NOT (TIP_OPE IN ('5', '6', '7', '8'))
               ORDER BY FEC_OPE, NUM_REF
           END-EXEC.
      *
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA.
           COPY QGECCAA.
       COPY V1NC070.
       COPY V1EC001.
      ******************************************************************
      *                                                                *
      *           P R O C E D U R E      D I V I S I O N               *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION USING V1NC070I V1EC001.
      *
       MAINLINE.
      *
           PERFORM 1000-INICIO
              THRU 1000-INICIO-EXIT
      *
           PERFORM 2000-PROCESO
              THRU 2000-PROCESO-EXIT
      *
           PERFORM 4000-FIN
              THRU 4000-FIN-EXIT
      *
           GOBACK
           .
      ******************************************************************
      *                         1000-INICIO                            *
      *                                                                *
      *   - INICIALIZAR VARIABLES AUXILIARES, COPYS,  AVISOS Y ERRORES *
      *   - DIRECCIONAR COPY DEL MAPA Y DE COMUNICACION VE1C001        *
      *   - ACTIVAR SWITCH DE OPERACION NO CONTABLE                    *
      *                                                                *
      ******************************************************************
       1000-INICIO.
      *
           SET ADDRESS OF V1NC070I         TO CAA-PTR-COPYIN
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
      *   - ANALIZAR SI FUE ESTADO DE INICIO O CONTINUACION            *
      ******************************************************************
       2000-PROCESO.
      *
           EVALUATE CAA-ESTADO
               WHEN CT-ESTADO-INICIO
                   PERFORM 2100-ESTADO-INICIO
                      THRU 2100-ESTADO-INICIO-EXIT

               WHEN CT-ESTADO-CONTINUA
                   PERFORM 3300-ESTADO-CONTINUACION
                      THRU 3300-ESTADO-CONTINUACION-EXIT
           END-EVALUATE
      *
           .
       2000-PROCESO-EXIT.
           EXIT.
      ******************************************************************
      *                        2100-ESTADO-INICIO                      *
      *   - DAR CONTROL A LA ARQUITECTURA PARA ENTRAR NOSOTROS MISMOS  *
      *     EN ESTADO CONTINUACION                                     *
      *   - ENTRADA DESDE V160, V164 O V168                            *
      *   - RETORNO DESDE V162, V165 O V169                            *
      ******************************************************************
       2100-ESTADO-INICIO.
      *
           MOVE COD-RIF                    TO WS-RIF
           MOVE WS-NAC-RIF                 TO WS-NAC-RIF-2
           MOVE WS-NUM-RIF3                TO WS-NUM-RIF-2
      *    ENTRADAS A EL DESDE 160, 164 Y 168
           IF  COD-TRANSACCION EQUAL CT-TRAN-V160 OR
               CT-TRAN-V164 OR CT-TRAN-V168
               PERFORM INIT-TABLA
                  THRU INIT-TABLA-EXIT

               PERFORM 2200-BUSCA-CONSOLIDADO
                  THRU 2200-BUSCA-CONSOLIDADO-EXIT

               IF  IND-PROCESO NOT EQUAL CT-1
                   PERFORM 9100-INICIALIZA
                      THRU 9100-INICIALIZA-EXIT

                   PERFORM 2700-MUESTRA
                      THRU 2700-MUESTRA-EXIT

                   MOVE WS-TOTAL-OPERACION TO WS-VARNUM15
                   MOVE WS-VARALF15        TO TOT2149O

                   MOVE CT-CURSOR                  TO VECS875L
                   SET CAA-88-ESTADO-CONTIN        TO TRUE
                   SET CAA-88-ACCION-TERMINAL      TO TRUE
               ELSE
                   PERFORM 3000-REGRESAR-A-V160
                      THRU 3000-REGRESAR-A-V160-EXIT
               END-IF
           ELSE
      *    RETORNOS A EL DESDE 162, 165 Y 169
               PERFORM 3100-REPINTAR-MAPA
                  THRU 3100-REPINTAR-MAPA-EXIT

               PERFORM 2850-MUESTRA-DESCRIP
                  THRU 2850-MUESTRA-DESCRIP-EXIT

               PERFORM 2750-PROTEGE-CAMPOS
                  THRU 2750-PROTEGE-CAMPOS-EXIT

               MOVE CT-CURSOR              TO VECS875L
               SET CAA-88-ESTADO-CONTIN    TO TRUE
               SET CAA-88-ACCION-TERMINAL  TO TRUE
      *
               EVALUATE COD-TRANSACCION
                   WHEN 'V162'
                       MOVE 'V160'         TO COD-TRANSACCION
                   WHEN 'V165'
                       MOVE 'V164'         TO COD-TRANSACCION
                   WHEN 'V169'
                       MOVE 'V168'         TO COD-TRANSACCION
               END-EVALUATE
           END-IF
      *
           .
       2100-ESTADO-INICIO-EXIT.
           EXIT.
      ******************************************************************
      *                        2200-BUSCA-CONSOLIDADO                  *
      ******************************************************************
       2200-BUSCA-CONSOLIDADO.
      *
           INITIALIZE CN-CONTADOR-OPE
                      WS-TOTAL-OPERACION
      *
           MOVE WS-NAC-RIF                 TO WS-NAC-RIF89
           MOVE WS-NUM-RIF                 TO WS-NUM-RIF89
           MOVE WS-NUM-DOC-PERSONA         TO NUM-DOC-PERSONA
      *
           PERFORM 2300-ABRIR-CURSOR
              THRU 2300-ABRIR-CURSOR-EXIT
      *
           PERFORM 9000-LEER-CURSOR
              THRU 9000-LEER-CURSOR-EXIT
      *
           PERFORM 2400-TRATAR-CURSOR
              THRU 2400-TRATAR-CURSOR-EXIT
             UNTIL FIN-CURSOR
      *
           PERFORM 2600-CERRAR-CURSOR
              THRU 2600-CERRAR-CURSOR-EXIT
      *
           MOVE ZEROS                      TO IND-PROCESO
           IF  CN-CONTADOR-OPE EQUAL ZEROS
               MOVE CT-1                   TO IND-PROCESO
           END-IF
      *
           .
       2200-BUSCA-CONSOLIDADO-EXIT.
           EXIT.
      ******************************************************************
      * 2300-ABRIR-CURSOR
      ******************************************************************
       2300-ABRIR-CURSOR.
      *
           EXEC SQL
               OPEN CUR-VEDT001
           END-EXEC
      *
           MOVE SQLCODE                    TO DB2-RETURN-CDE
           IF  NOT DB2-OK
               PERFORM 9999-ABEND-DB2
                  THRU 9999-ABEND-DB2-EXIT
           END-IF
      *
           .
       2300-ABRIR-CURSOR-EXIT.
           EXIT.
      ******************************************************************
      * 2400-TRATAR-CURSOR
      ******************************************************************
       2400-TRATAR-CURSOR.
      *
           ADD CT-1                        TO CN-CONTADOR-OPE
           MOVE NOM-RAZON                  TO WS-NOM-RAZON
      *
           PERFORM 2500-FORMA-PAGO
              THRU 2500-FORMA-PAGO-EXIT
      *
           IF  WS-X GREATER OR EQUAL CT-1 AND
               WS-X LESS OR EQUAL CT-7
               MOVE COD-DIV-OPE   TO COD-TEMP
               IF  COD-TEMP NOT EQUAL CT-1
                   COMPUTE WS-MONTO-OPERACION(WS-X) =
                      WS-MONTO-OPERACION(WS-X) + (IMP-OPE * TAS-DOLAR)

                   COMPUTE WS-TOTAL-OPERACION =
                      WS-TOTAL-OPERACION + (IMP-OPE * TAS-DOLAR)

               ELSE
                   COMPUTE WS-MONTO-OPERACION(WS-X) =
                      WS-MONTO-OPERACION(WS-X) + IMP-OPE

                   COMPUTE WS-TOTAL-OPERACION =
                      WS-TOTAL-OPERACION + IMP-OPE
               END-IF
           END-IF
      *
           PERFORM 9000-LEER-CURSOR
              THRU 9000-LEER-CURSOR-EXIT
      *
           .
       2400-TRATAR-CURSOR-EXIT.
           EXIT.
      ******************************************************************
      *                        2500-FORMA-PAGO                         *
      ******************************************************************
       2500-FORMA-PAGO.
      *
           INITIALIZE  WS-X
      *
           EVALUATE TRUE
      *COMPRA EFECTIVO
               WHEN TIP-PAGO EQUAL CT-1 AND
                   (COD-CPT-CMP-VTA GREATER OR EQUAL CT-100 AND
                     LESS OR EQUAL CT-199)
      *
                   MOVE CT-1               TO WS-X
      *VENTA EFECTIVO
               WHEN TIP-PAGO EQUAL CT-1 AND
                    (COD-CPT-CMP-VTA GREATER OR EQUAL CT-200 AND
                     LESS OR EQUAL CT-299)
      *
                   MOVE CT-2               TO WS-X
      *COMPRA CHEQUE
               WHEN TIP-PAGO EQUAL CT-2 AND
                    (COD-CPT-CMP-VTA GREATER OR EQUAL CT-100 AND
                     LESS OR EQUAL CT-199)
      *
                   MOVE CT-3               TO WS-X
      *VENTA CHEQUE
               WHEN TIP-PAGO EQUAL CT-2 AND
                    (COD-CPT-CMP-VTA GREATER OR EQUAL CT-200 AND
                     LESS OR EQUAL CT-299)
      *
                   MOVE CT-4               TO WS-X
      *TRANSF RECIBIDA
               WHEN TIP-PAGO EQUAL CT-3 AND
                    (COD-CPT-CMP-VTA GREATER OR EQUAL CT-100 AND
                     LESS OR EQUAL CT-199)
      *
                   MOVE CT-5               TO WS-X
      *TRANSF ENVIADA
               WHEN TIP-PAGO EQUAL CT-3 AND
                    (COD-CPT-CMP-VTA GREATER OR EQUAL CT-200 AND
                     LESS OR EQUAL CT-299)
      *
                   MOVE CT-6               TO WS-X
      *VENTA TRAVEL
               WHEN TIP-PAGO EQUAL CT-4 AND
                    (COD-CPT-CMP-VTA GREATER OR EQUAL CT-200 AND
                     LESS OR EQUAL CT-299)
      *
                   MOVE CT-7               TO WS-X
      *
           END-EVALUATE
      *
           .
       2500-FORMA-PAGO-EXIT.
           EXIT.
      ******************************************************************
      *                        2600-CERRAR-CURSOR                      *
      ******************************************************************
       2600-CERRAR-CURSOR.
      *
           EXEC SQL
               CLOSE CUR-VEDT001
           END-EXEC
      *
           MOVE SQLCODE                    TO DB2-RETURN-CDE
           IF  NOT DB2-OK
               PERFORM 9999-ABEND-DB2
                  THRU 9999-ABEND-DB2-EXIT
           END-IF
      *
           .
       2600-CERRAR-CURSOR-EXIT.
           EXIT.
      ******************************************************************
      * 2700-MUESTRA
      ******************************************************************
       2700-MUESTRA.
      *
           INITIALIZE                         WS-TOTAL-EFECTIVO
                                              WS-TOTAL-CHEQUE
                                              WS-TOTAL-TRANSFERENCIA
                                              WS-TOTAL-CHEQUE-VIAJERO
                                              WS-FORMA-PAGO
                                              WS-COD-CPT-CMP-VTA-D
                                              WS-COD-CPT-CMP-VTA-H
      *
           ADD WS-MONTO-OPERACION(CT-1) WS-MONTO-OPERACION(CT-2)
                                           TO WS-TOTAL-EFECTIVO
           ADD WS-MONTO-OPERACION(CT-3) WS-MONTO-OPERACION(CT-4)
                                           TO WS-TOTAL-CHEQUE
           ADD WS-MONTO-OPERACION(CT-5) WS-MONTO-OPERACION(CT-6)
                                           TO WS-TOTAL-TRANSFERENCIA
           ADD WS-MONTO-OPERACION(CT-7)    TO WS-TOTAL-CHEQUE-VIAJERO
      *

      *    MOVE WS-NOM-RAZON               TO BPBN537O

           PERFORM 2750-PROTEGE-CAMPOS
              THRU 2750-PROTEGE-CAMPOS-EXIT
      *
           PERFORM 2800-MUESTRA-TOTALES
              THRU 2800-MUESTRA-TOTALES-EXIT
      *
           PERFORM 2850-MUESTRA-DESCRIP
              THRU 2850-MUESTRA-DESCRIP-EXIT
      *
           PERFORM 2900-MUESTRA-DETALLES
              THRU 2900-MUESTRA-DETALLES-EXIT
      *
           .
       2700-MUESTRA-EXIT.
           EXIT.
      ******************************************************************
      *                        2800-MUESTRA-TOTALES                    *
      ******************************************************************
       2800-MUESTRA-TOTALES.
      *TOTAL EFECTIVO
           MOVE WS-TOTAL-EFECTIVO          TO WS-VARNUM15
           MOVE WS-VARALF15                TO TOT1049O
      *TOTAL CHEQUE
           MOVE WS-TOTAL-CHEQUE            TO WS-VARNUM15
           MOVE WS-VARALF15                TO TOT1349O
      *TOTAL TRANSFERENCIA
           MOVE WS-TOTAL-TRANSFERENCIA     TO WS-VARNUM15
           MOVE WS-VARALF15                TO TOT1649O
      *TOTAL CHEQUE VIAJERO
           MOVE WS-TOTAL-CHEQUE-VIAJERO    TO WS-VARNUM15
           MOVE WS-VARALF15                TO TOT1849O
      *
           .
       2800-MUESTRA-TOTALES-EXIT.
           EXIT.
      ******************************************************************
      *                    2850-MUESTRA-DESCRIPCIONES
      ******************************************************************
       2850-MUESTRA-DESCRIP.
      *
           MOVE WS-COD-FORMA-PAGO (1)      TO CODFO82I
           MOVE WS-DES-FORMA-PAGO (1)      TO DESFO84I
           MOVE WS-DES-OPERACION  (1)      TO DESO834I
      *
           MOVE WS-COD-FORMA-PAGO (2)      TO CODFO92I
           MOVE WS-DES-FORMA-PAGO (2)      TO DESFO94I
           MOVE WS-DES-OPERACION  (2)      TO DESO934I
      *
           MOVE WS-COD-FORMA-PAGO (3)      TO CODF112I
           MOVE WS-DES-FORMA-PAGO (3)      TO DESF114I
           MOVE WS-DES-OPERACION  (3)      TO DES1134I
      *
           MOVE WS-COD-FORMA-PAGO (4)      TO CODF122I
           MOVE WS-DES-FORMA-PAGO (4)      TO DESF124I
           MOVE WS-DES-OPERACION  (4)      TO DES1234I
      *
           MOVE WS-COD-FORMA-PAGO (5)      TO CODF142I
           MOVE WS-DES-FORMA-PAGO (5)      TO DESF144I
           MOVE WS-DES-OPERACION  (5)      TO DES1434I
      *
           MOVE WS-COD-FORMA-PAGO (6)      TO CODF152I
           MOVE WS-DES-FORMA-PAGO (6)      TO DESF154I
           MOVE WS-DES-OPERACION  (6)      TO DES1534I
      *
           MOVE WS-COD-FORMA-PAGO (7)      TO CODF172I
           MOVE WS-DES-FORMA-PAGO (7)      TO DESF174I
           MOVE WS-DES-OPERACION  (7)      TO DES1734I
      *
           MOVE WS-NOM-RAZON               TO BPBN537I
      *
           .
       2850-MUESTRA-DESCRIP-EXIT.
           EXIT.
      ******************************************************************
      *                        2900-MUESTRA-DETALLES                   *
      ******************************************************************
       2900-MUESTRA-DETALLES.
      *EFETIVO COMPRA
           MOVE WS-MONTO-OPERACION(CT-1)   TO WS-VARNUM15
           MOVE WS-VARALF15                TO MONT849O
      *EFETIVO VENTA
           MOVE WS-MONTO-OPERACION(CT-2)   TO WS-VARNUM15
           MOVE WS-VARALF15                TO MONT949O
      *CHEQUE COMPRA
           MOVE WS-MONTO-OPERACION(CT-3)   TO WS-VARNUM15
           MOVE WS-VARALF15                TO MON1149O
      *CHEQUE VENTA
           MOVE WS-MONTO-OPERACION(CT-4)   TO WS-VARNUM15
           MOVE WS-VARALF15                TO MON1249O
      *TRANFERENCIA RECIBIDA
           MOVE WS-MONTO-OPERACION(CT-5)   TO WS-VARNUM15
           MOVE WS-VARALF15                TO MON1449O
      *TRANFERENCIA ENVIADA
           MOVE WS-MONTO-OPERACION(CT-6)   TO WS-VARNUM15
           MOVE WS-VARALF15                TO MON1549O
      *CHEQUE VIAJERO
           MOVE WS-MONTO-OPERACION(CT-7)   TO WS-VARNUM15
           MOVE WS-VARALF15                TO MON1749O
      *
           .
       2900-MUESTRA-DETALLES-EXIT.
           EXIT.
      ******************************************************************
      * 3000-REGRESAR-A-V160
      ******************************************************************
       3000-REGRESAR-A-V160.
      *
           SET CAA-88-CODTRAN-SIG-SAME    TO TRUE
           SET CAA-88-ACCION-TERMINAL     TO TRUE
           SET CAA-88-ESTADO-CONTIN       TO TRUE
           MOVE CAA-CODTRAN               TO COD-TRANSACCION
           MOVE 'V1A0500'                 TO CAA-COD-AVISO1
      *
           .
       3000-REGRESAR-A-V160-EXIT.
           EXIT.
      ******************************************************************
      *                        3100-REPINTAR-MAPA                      *
      ******************************************************************
       3100-REPINTAR-MAPA.
      *
           PERFORM 9100-INICIALIZA
              THRU 9100-INICIALIZA-EXIT

           PERFORM INIT-TABLA
              THRU INIT-TABLA-EXIT
      *NOMBRE
           MOVE NOM-RAZON-V161             TO BPBN537O
                                              WS-NOM-RAZON
      *TOTALES
           MOVE TOT-EFECTIVO-V161          TO TOT1049O
           MOVE TOT-CHEQUE-V161            TO TOT1349O
           MOVE TOT-TRANSFERENCIA-V161     TO TOT1649O
           MOVE TOT-CHEQUE-VIA-V161        TO TOT1849O
           MOVE TOT-TOTAL                  TO TOT2149O
      *DETALLES
           MOVE TOT-EFECTIVO-C-V161        TO MONT849O
           MOVE TOT-EFECTIVO-V-V161        TO MONT949O
           MOVE TOT-CHEQUE-C-V161          TO MON1149O
           MOVE TOT-CHEQUE-V-V161          TO MON1249O
           MOVE TOT-TRANSFE-R-V161         TO MON1449O
           MOVE TOT-TRANSFE-E-V161         TO MON1549O
           MOVE TOT-CHEQUE-VIA-C-V161      TO MON1749O

      *    ARMANDO QUIERE QUE SE BORRE LA SELECCION AL RETORNAR
      *    PERFORM 3200-SELECCION
      *       THRU 3200-SELECCION-EXIT
           INITIALIZE VECS875O
                      VECS975O
                      VEC1175O
                      VEC1275O
                      VEC1475O
                      VEC1575O
                      VEC1775O
      *
           .
       3100-REPINTAR-MAPA-EXIT.
           EXIT.
      ******************************************************************
      *                        3200-SELECCION
      ******************************************************************
       3200-SELECCION.
      *
           EVALUATE IND-SELECCION
               WHEN CT-1
                   MOVE CT-X               TO VECS875O
               WHEN CT-2
                   MOVE CT-X               TO VECS975O
               WHEN CT-3
                   MOVE CT-X               TO VEC1175O
               WHEN CT-4
                   MOVE CT-X               TO VEC1275O
               WHEN CT-5
                   MOVE CT-X               TO VEC1475O
               WHEN CT-6
                   MOVE CT-X               TO VEC1575O
               WHEN CT-7
                   MOVE CT-X               TO VEC1775O
           END-EVALUATE
      *
           .
       3200-SELECCION-EXIT.
           EXIT.
      ******************************************************************
      *                        3300-ESTADO-CONTINUACION                *
      ******************************************************************
       3300-ESTADO-CONTINUACION.
      *
           INITIALIZE                      GRP-DATOS-V161
                                           GRP-DATOS-LISTADO
      *
           PERFORM 3400-LLENAR-MEMORIA
              THRU 3400-LLENAR-MEMORIA-EXIT
      *
           EVALUATE CAA-TECLA
               WHEN CT-INTRO
                   PERFORM 3500-VALIDAR-SELECCION
                      THRU 3500-VALIDAR-SELECCION-EXIT

                   PERFORM 9200-GUARDAR-MAPA-V161
                      THRU 9200-GUARDAR-MAPA-V161-EXIT

                   PERFORM 3800-LLAMAR-A
                      THRU 3800-LLAMAR-A-EXIT

                   MOVE CAA-CODTRAN        TO COD-TRANSACCION

               WHEN CT-AYUDA
                   PERFORM 9200-GUARDAR-MAPA-V161
                      THRU 9200-GUARDAR-MAPA-V161-EXIT

                   SET CAA-88-ACCION-PROGRAMA TO TRUE
                   SET CAA-88-CADENA-ANADIR   TO TRUE
                   SET CAA-88-ESTADO-INICIO   TO TRUE
                   MOVE CT-TRAN-V175          TO CAA-CODTRAN-SIG
                   MOVE CAA-CODTRAN           TO COD-TRANSACCION

               WHEN CT-BORRA
                   SET CAA-88-ACCION-PROGRAMA  TO TRUE
                   SET CAA-88-ESTADO-INICIO    TO TRUE
                   SET CAA-88-CODTRAN-SIG-ULTI TO TRUE
                   MOVE CAA-CODTRAN            TO COD-TRANSACCION

               WHEN OTHER
                   SET CAA-88-ACCION-TERMINAL TO TRUE
                   SET CAA-88-ESTADO-CONTIN   TO TRUE
                   MOVE ME-TECLA-INCORRECTA   TO CAA-COD-ERROR
                   PERFORM 4000-FIN
                      THRU 4000-FIN-EXIT
           END-EVALUATE
      *
           .
       3300-ESTADO-CONTINUACION-EXIT.
           EXIT.
      ******************************************************************
      *                        3400-LLENAR-MEMORIA                     *
      ******************************************************************
       3400-LLENAR-MEMORIA.
      *
           MOVE MONT849I                   TO WS-VARALF15
           MOVE WS-VARNUM15                TO WS-MONTO-OPERACION(CT-1)
           MOVE VECS875I                   TO WS-VEC-SEL(CT-1)

           MOVE MONT949I                   TO WS-VARALF15
           MOVE WS-VARNUM15                TO WS-MONTO-OPERACION(CT-2)
           MOVE VECS975I                   TO WS-VEC-SEL(CT-2)

           MOVE MON1149I                   TO WS-VARALF15
           MOVE WS-VARNUM15                TO WS-MONTO-OPERACION(CT-3)
           MOVE VEC1175I                   TO WS-VEC-SEL(CT-3)

           MOVE MON1249I                   TO WS-VARALF15
           MOVE WS-VARNUM15                TO WS-MONTO-OPERACION(CT-4)
           MOVE VEC1275I                   TO WS-VEC-SEL(CT-4)

           MOVE MON1449I                   TO WS-VARALF15
           MOVE WS-VARNUM15                TO WS-MONTO-OPERACION(CT-5)
           MOVE VEC1475I                   TO WS-VEC-SEL(CT-5)

           MOVE MON1549I                   TO WS-VARALF15
           MOVE WS-VARNUM15                TO WS-MONTO-OPERACION(CT-6)
           MOVE VEC1575I                   TO WS-VEC-SEL(CT-6)

           MOVE MON1749I                   TO WS-VARALF15
           MOVE WS-VARNUM15                TO WS-MONTO-OPERACION(CT-7)
           MOVE VEC1775I                   TO WS-VEC-SEL(CT-7)
      *
           .
       3400-LLENAR-MEMORIA-EXIT.
           EXIT.
      ******************************************************************
      *                        3500-VALIDAR-SELECCION                  *
      ******************************************************************
       3500-VALIDAR-SELECCION.
      *
           PERFORM 3600-CHEQUEAR-SEL
              THRU 3600-CHEQUEAR-SEL-EXIT
           VARYING IN-I FROM 1 BY 1
             UNTIL IN-I GREATER CT-7
      *
           IF CN-SELECCION EQUAL ZEROS
               MOVE ME-UNA-SELECCION       TO CAA-COD-ERROR
               MOVE CT-CURSOR              TO VECS875L
               PERFORM REINPUT
           ELSE
               IF  CN-SELECCION GREATER CT-1
                   MOVE ME-SOLO-UNA-SELECCION TO CAA-COD-ERROR
                   MOVE CT-CURSOR             TO VECS875L
                   PERFORM REINPUT
               END-IF
           END-IF
      *
           PERFORM VARYING IN-I FROM 1 BY 1
           UNTIL IN-I GREATER CT-7
               IF  WS-VEC-SEL(IN-I) = 'X'
                   PERFORM 3700-DATOS-A-PASAR
                      THRU 3700-DATOS-A-PASAR-EXIT
               END-IF
           END-PERFORM
      *
           .
       3500-VALIDAR-SELECCION-EXIT.
           EXIT.
      ******************************************************************
      *                        3600-CHEQUEAR-SEL                       *
      ******************************************************************
       3600-CHEQUEAR-SEL.
      *
           IF  WS-VEC-SEL(IN-I) NOT EQUAL SPACE AND LOW-VALUE
               AND CT-X
               MOVE ME-ERROR-SELECCION     TO CAA-COD-ERROR
               MOVE CT-SELECCION           TO CAA-VAR1-ERROR
               PERFORM COLOCA-CURSOR-OPC
               PERFORM REINPUT
           END-IF
      *
           IF  WS-VEC-SEL(IN-I) EQUAL CT-X
               ADD CT-1                    TO CN-SELECCION
               IF  WS-MONTO-OPERACION(IN-I) EQUAL ZEROS
                   MOVE ME-OPERACIONES-FORMA  TO CAA-COD-ERROR
                   PERFORM COLOCA-CURSOR-OPC
                   PERFORM REINPUT
               END-IF
           END-IF
      *
           .
       3600-CHEQUEAR-SEL-EXIT.
           EXIT.
      ******************************************************************
      * COLOCA-CURSOR-OPC
      ******************************************************************
       COLOCA-CURSOR-OPC.
      *
           EVALUATE IN-I
               WHEN 1
                   MOVE -1             TO VECS875L
               WHEN 2
                   MOVE -1             TO VECS975L
               WHEN 3
                   MOVE -1             TO VEC1175L
               WHEN 4
                   MOVE -1             TO VEC1275L
               WHEN 5
                   MOVE -1             TO VEC1475L
               WHEN 6
                   MOVE -1             TO VEC1575L
               WHEN 7
                   MOVE -1             TO VEC1775L
           END-EVALUATE
      *
           .
      ******************************************************************
      *                        3700-DATOS-A-PASAR                      *
      ******************************************************************
       3700-DATOS-A-PASAR.
      *
           EVALUATE IN-I
               WHEN CT-1
      *
                   MOVE CT-1               TO TIP-FORMA-PAGO
                                              IND-SELECCION
                   MOVE CT-100             TO CON-CONCEPTO-D
                   MOVE CT-199             TO CON-CONCEPTO-H
      *
               WHEN CT-2
                   MOVE CT-2               TO IND-SELECCION
                   MOVE CT-1               TO TIP-FORMA-PAGO
                   MOVE CT-200             TO CON-CONCEPTO-D
                   MOVE CT-299             TO CON-CONCEPTO-H
      *
               WHEN CT-3
                   MOVE CT-3               TO IND-SELECCION
                   MOVE CT-2               TO TIP-FORMA-PAGO
                   MOVE CT-100             TO CON-CONCEPTO-D
                   MOVE CT-199             TO CON-CONCEPTO-H
      *
               WHEN CT-4
                   MOVE CT-4               TO IND-SELECCION
                   MOVE CT-2               TO TIP-FORMA-PAGO
                   MOVE CT-200             TO CON-CONCEPTO-D
                   MOVE CT-299             TO CON-CONCEPTO-H
      *
               WHEN CT-5
                   MOVE CT-5               TO IND-SELECCION
                   MOVE CT-3               TO TIP-FORMA-PAGO
                   MOVE CT-100             TO CON-CONCEPTO-D
                   MOVE CT-199             TO CON-CONCEPTO-H
      *
               WHEN CT-6
                   MOVE CT-6               TO IND-SELECCION
                   MOVE CT-3               TO TIP-FORMA-PAGO
                   MOVE CT-200             TO CON-CONCEPTO-D
                   MOVE CT-299             TO CON-CONCEPTO-H
      *
               WHEN CT-7
                   MOVE CT-7               TO IND-SELECCION
                   MOVE CT-4               TO TIP-FORMA-PAGO
                   MOVE CT-200             TO CON-CONCEPTO-D
                   MOVE CT-299             TO CON-CONCEPTO-H
           END-EVALUATE
      *
           .
       3700-DATOS-A-PASAR-EXIT.
           EXIT.
      ******************************************************************
      *                        3800-LLAMAR-A                           *
      ******************************************************************
       3800-LLAMAR-A.
      *
           SET CAA-88-ACCION-PROGRAMA      TO TRUE
           SET CAA-88-ESTADO-INICIO        TO TRUE
           SET CAA-88-CADENA-ANADIR        TO TRUE
           MOVE '1'                        TO CAA-CASO
      *
           EVALUATE COD-TRANSACCION
               WHEN CT-TRAN-V160
                   MOVE CT-TRAN-V162       TO CAA-CODTRAN-SIG

               WHEN CT-TRAN-V164
                   MOVE CT-TRAN-V165       TO CAA-CODTRAN-SIG

               WHEN CT-TRAN-V168
                   MOVE CT-TRAN-V169       TO CAA-CODTRAN-SIG
           END-EVALUATE
      *
           .
       3800-LLAMAR-A-EXIT.
           EXIT.
      ******************************************************************
      * REINPUT
      ******************************************************************
       REINPUT.
      *
           SET CAA-88-ACCION-TERMINAL TO TRUE
           SET CAA-88-ESTADO-CONTIN   TO TRUE
           INITIALIZE CAA-CADENA

           PERFORM 2750-PROTEGE-CAMPOS
              THRU 2750-PROTEGE-CAMPOS-EXIT

           PERFORM 4000-FIN
              THRU 4000-FIN-EXIT
      *
           .
      ******************************************************************
      *                            4000-FIN                            *
      ******************************************************************
       4000-FIN.
      *
           EXEC CICS
               RETURN
           END-EXEC
      *
           .
       4000-FIN-EXIT.
           EXIT.
      ******************************************************************
      *                      9000-LEER-CURSOR                          *
      ******************************************************************
       9000-LEER-CURSOR.
      *
           EXEC SQL
               FETCH CUR-VEDT001
                   INTO :COD-ENT-BANCO,
                        :COD-OPE-BANCO,
                        :COD-DIV-OPE,
                        :COD-PAIS-CORR,
                        :CTA-PASIVO-CL,
                        :NUM-REF,
                        :NUM-DOC-PERSONA,
                        :NOM-RAZON,
                        :IMP-OPE,
                        :TAS-DOLAR,
                        :FEC-OPE,
                        :COD-CPT-CMP-VTA,
                        :TIP-PAGO
           END-EXEC
      *
           MOVE SQLCODE                    TO DB2-RETURN-CDE
           EVALUATE TRUE
               WHEN DB2-OK
                   CONTINUE

               WHEN DB2-NOTFND
                   SET FIN-CURSOR          TO TRUE

               WHEN OTHER
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *
           .
       9000-LEER-CURSOR-EXIT.
           EXIT.
      ******************************************************************
      *                        9100-INICIALIZA                         *
      ******************************************************************
       9100-INICIALIZA.
      *
           INITIALIZE V1NC070I
                      BPBC519I
                      TOTA212I
           MOVE COD-RIF                    TO BPBC519I
           MOVE CT-TOTAL                   TO TOTA212I
      *
           .
       9100-INICIALIZA-EXIT.
           EXIT.
      ******************************************************************
      * INIT-TABLA
      ******************************************************************
       INIT-TABLA.
      *
           INITIALIZE TB-TABLAS
      *
           MOVE CT-1                       TO WS-COD-FORMA-PAGO  (1)
           MOVE CT-EFECTIVO                TO WS-DES-FORMA-PAGO  (1)
           MOVE CT-COMPRA                  TO WS-DES-OPERACION   (1)
           MOVE ZEROS                      TO WS-MONTO-OPERACION (1)
      *
           MOVE CT-1                       TO WS-COD-FORMA-PAGO  (2)
           MOVE CT-EFECTIVO                TO WS-DES-FORMA-PAGO  (2)
           MOVE CT-VENTA                   TO WS-DES-OPERACION   (2)
           MOVE ZEROS                      TO WS-MONTO-OPERACION (2)
      *
           MOVE CT-2                       TO WS-COD-FORMA-PAGO  (3)
           MOVE CT-CHEQUES                 TO WS-DES-FORMA-PAGO  (3)
           MOVE CT-COMPRA                  TO WS-DES-OPERACION   (3)
           MOVE ZEROS                      TO WS-MONTO-OPERACION (3)
      *
           MOVE CT-2                       TO WS-COD-FORMA-PAGO  (4)
           MOVE CT-CHEQUES                 TO WS-DES-FORMA-PAGO  (4)
           MOVE CT-VENTA                   TO WS-DES-OPERACION   (4)
           MOVE ZEROS                      TO WS-MONTO-OPERACION (4)
      *
           MOVE CT-3                       TO WS-COD-FORMA-PAGO  (5)
           MOVE CT-TRANSFERENCIA           TO WS-DES-FORMA-PAGO  (5)
           MOVE CT-RECIBIDA                TO WS-DES-OPERACION   (5)
           MOVE ZEROS                      TO WS-MONTO-OPERACION (5)
      *
           MOVE CT-3                       TO WS-COD-FORMA-PAGO  (6)
           MOVE CT-TRANSFERENCIA           TO WS-DES-FORMA-PAGO  (6)
           MOVE CT-ENVIADA                 TO WS-DES-OPERACION   (6)
           MOVE ZEROS                      TO WS-MONTO-OPERACION (6)
      *
           MOVE CT-4                       TO WS-COD-FORMA-PAGO  (7)
           MOVE CT-CHEQUES-VIA             TO WS-DES-FORMA-PAGO  (7)
           MOVE CT-VENTA                   TO WS-DES-OPERACION   (7)
           MOVE ZEROS                      TO WS-MONTO-OPERACION (7)
      *
           .
       INIT-TABLA-EXIT.
           EXIT.
      ******************************************************************
      *                        9200-GUARDAR-MAPA-V161                  *
      ******************************************************************
       9200-GUARDAR-MAPA-V161.
      *
           MOVE BPBN537I                   TO NOM-RAZON-V161
           MOVE TOT1049I                   TO TOT-EFECTIVO-V161
           MOVE TOT1349I                   TO TOT-CHEQUE-V161
           MOVE TOT1649I                   TO TOT-TRANSFERENCIA-V161
           MOVE TOT1849I                   TO TOT-CHEQUE-VIA-V161
           MOVE TOT2149I                   TO TOT-TOTAL

           MOVE MONT849I                   TO TOT-EFECTIVO-C-V161
           MOVE MONT949I                   TO TOT-EFECTIVO-V-V161
           MOVE MON1149I                   TO TOT-CHEQUE-C-V161
           MOVE MON1249I                   TO TOT-CHEQUE-V-V161
           MOVE MON1449I                   TO TOT-TRANSFE-R-V161
           MOVE MON1549I                   TO TOT-TRANSFE-E-V161
           MOVE MON1749I                   TO TOT-CHEQUE-VIA-C-V161
      *
           .
       9200-GUARDAR-MAPA-V161-EXIT.
           EXIT.
      ******************************************************************
      *                        2750-PROTEJE-CAMPOS                     *
      ******************************************************************
       2750-PROTEGE-CAMPOS.
      *
           MOVE ATRI-PRO-FST               TO TOT1049A
                                              TOT1349A
                                              TOT1649A
                                              TOT1849A
                                              TOTA212A
                                              TOT2149A
           MOVE ATRI-PRO-BRI-FST           TO
                                              MONT849A
                                              MONT949A
                                              MON1149A
                                              MON1249A
                                              MON1449A
                                              MON1549A
                                              MON1749A
                                              CODFO82A
                                              DESFO84A
                                              DESO834A
                                              CODFO92A
                                              DESFO94A
                                              DESO934A
                                              CODF112A
                                              DESF114A
                                              DES1134A
                                              CODF122A
                                              DESF124A
                                              DES1234A
                                              CODF142A
                                              DESF144A
                                              DES1434A
                                              CODF152A
                                              DESF154A
                                              DES1534A
                                              CODF172A
                                              DESF174A
                                              DES1734A
                                              MENS415A
                                              BPBC519A
                                              BPBN537A

           MOVE ATRI-NOP-BRI-FST           TO VECS875A
                                              VECS975A
                                              VEC1175A
                                              VEC1275A
                                              VEC1475A
                                              VEC1575A
                                              VEC1775A
      *
           .
       2750-PROTEGE-CAMPOS-EXIT.
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
           PERFORM 9999-ABEND
      *
           .
       9999-ABEND-DB2-EXIT.
           EXIT.
      ******************************************************************
      *                           9999-ABEND                           *
      *    ABENDAR EL PROGRAMA REALIZANDO UN LINK A LA RUTINA          *
      *    QG1CABC                                                     *
      ******************************************************************
       9999-ABEND.
      *
           EXEC CICS
              LINK PROGRAM(CT-QG1CABC)
              COMMAREA(WS-QGECABC-01)
           END-EXEC.
      ******************************************************************
      ************ F I N    D E    P R O G R A M A  ********************
      ******************************************************************
