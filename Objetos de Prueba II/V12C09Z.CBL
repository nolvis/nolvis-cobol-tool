      ******************************************************************
      *                                                                *
      *  PROGRAMA: V12C09Z                                             *
      *                                                                *
      *  FECHA CREACION: 03/01/2008                                    *
      *                                                                *
      *  AUTOR: FACTORIA                                               *
      *                                                                *
      *  APLICACION: MERCADO LIBRE DE DIVISAS. (V1) VENEZUELA.         *
      *                                                                *
      *  DESCRIPCION: DETERMINA EL CRITERIO DE BUSQUEDA PARA LA        *
      *               CONSULTA                                         *
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
       PROGRAM-ID.   V12C09Z.
       AUTHOR.       FACTORIA.
       DATE-WRITTEN. 03/01/2008
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
           05  WS-CURRENT-DATE.
               10  WS-FEC-DIA-AAAAMMDD     PIC X(08)   VALUE SPACES.
               10  FILLER                  PIC X(06).
      *
           05  WS-FECHA-E                  PIC 9(08)   VALUE ZEROS.
           05  WS-FECHA-E-R REDEFINES WS-FECHA-E.
               10  WS-ANO-E                PIC 9(04).
               10  WS-MES-E                PIC 9(02).
               10  WS-DIA-E                PIC 9(02).
      *
           05  WS-FECHA-DESDE-DMA.
               10  WS-DIA-DESDE-DMA        PIC X(02)   VALUE ZEROS.
               10  WS-MES-DESDE-DMA        PIC X(02)   VALUE ZEROS.
               10  WS-ANO-DESDE-DMA        PIC X(04)   VALUE ZEROS.
           05  WS-FECHA-HASTA-DMA.
               10  WS-DIA-HASTA-DMA        PIC X(02)   VALUE ZEROS.
               10  WS-MES-HASTA-DMA        PIC X(02)   VALUE ZEROS.
               10  WS-ANO-HASTA-DMA        PIC X(04)   VALUE ZEROS.
           05  WS-CANT-REG                 PIC S9(12)V USAGE COMP-3.
      *
           05  WS-FECHA-DESDE.
               10  WS-AA-D                 PIC 9(04)   VALUE ZEROES.
               10  WS-MM-D                 PIC 9(02)   VALUE ZEROES.
               10  WS-DD-D                 PIC 9(02)   VALUE ZEROES.
      *
           05  WS-FECHA-HASTA.
               10  WS-AA-H                 PIC 9(04)   VALUE ZEROES.
               10  WS-MM-H                 PIC 9(02)   VALUE ZEROES.
               10  WS-DD-H                 PIC 9(02)   VALUE ZEROES.
      *
           05  WS-SEG                      PIC 9(02)   VALUE ZEROES.
           05  WS-SEGMENTO                 PIC X(12)   VALUE SPACES.
      *
           05  WS-SP1-FIN                  PIC X(08)   VALUE ZEROES.
           05  WS-SP1-CED                  PIC X(15).
           05  WS-SP1-CED-R REDEFINES WS-SP1-CED.
                   15  WS-SP1-NAC          PIC X(01).
                   15  WS-SP1-NUM          PIC 9(14).
      *
           05  WS-SP2-FIN                  PIC X(08)   VALUE ZEROES.
           05  WS-SP2-CED                  PIC X(15).
           05  WS-SP2-CED-R REDEFINES WS-SP2-CED.
                   15  WS-SP2-NAC          PIC X(01).
                   15  WS-SP2-NUM          PIC 9(14).

           05  TEMP-NUM                    PIC X(14)   VALUE ZEROS.
      ******************************************************************
      *                    AREA  DE  SWITCHES                          *
      ******************************************************************
       01  W-DFECHA.
           05  W-DANO                      PIC 9(04).
               88  W-DANOOK                       VALUE 1993 THRU 2999.
           05  W-DMES                      PIC 9(02).
               88  W-DMESOK                        VALUE 01 THRU 12.
           05  W-DDIA                      PIC 9(02).
               88  W-DDIAOK                        VALUE 01 THRU 31.
       01  W-HFECHA.
           05  W-HANO                       PIC 9(04).
               88 W-HANOOK                         VALUE 1993 THRU 2999.
           05  W-HMES                       PIC 9(02).
               88 W-HMESOK                        VALUE 01 THRU 12.
           05  W-HDIA                       PIC 9(02).
               88 W-HDIAOK                        VALUE 01 THRU 31.
      ******************************************************************
      *                    AREA DE CONTANTES                           *
      ******************************************************************
       01  CT-CONTANTES.
      *                                                                *
           05  CT-PROGRAMA                 PIC X(07)   VALUE 'V12C09Z'.
           05  CT-TRAN-V172                PIC X(04)   VALUE 'V172'.
           05  CT-TRAN-V174                PIC X(04)   VALUE 'V174'.
           05  CT-V1DT001                  PIC X(07)   VALUE 'VIDT001'.
           05  CT-ESTADO-INICIO            PIC X(01)   VALUE 'I'.
           05  CT-ESTADO-CONTINUA          PIC X(01)   VALUE 'C'.
           05  CT-A                        PIC X(01)   VALUE 'A'.
           05  CT-E                        PIC X(01)   VALUE 'E'.
           05  CT-G                        PIC X(01)   VALUE 'G'.
           05  CT-S                        PIC X(01)   VALUE 'S'.
           05  CT-J                        PIC X(01)   VALUE 'J'.
           05  CT-P                        PIC X(01)   VALUE 'P'.
           05  CT-V                        PIC X(01)   VALUE 'V'.
           05  CT-X                        PIC X(01)   VALUE 'X'.
           05  CT-Z                        PIC X(01)   VALUE 'Z'.
           05  CT-OPCION-1                 PIC X(01)   VALUE '1'.
           05  CT-INTRO                    PIC X(02)   VALUE '00'.
           05  CT-RETORNO-OK               PIC X(02)   VALUE '00'.
           05  CT-F6                       PIC X(02)   VALUE '06'.
           05  CT-FECHA-ILOGICA            PIC X(02)   VALUE '10'.
           05  CT-BORRA                    PIC X(02)   VALUE '99'.
           05  CT-TC8C1220                 PIC X(08)   VALUE 'TC8C1220'.
           05  CT-CORPORATIVO              PIC X(11)   VALUE
                                   'CORPORATIVO'.
           05  CT-EMPRESARIAL              PIC X(11)   VALUE
                                   'EMPRESARIAL'.
           05  CT-COMERCIAL                PIC X(09)   VALUE
                                   'COMERCIAL'.
           05  CT-CURSOR                   PIC S9      VALUE -1.
           05  CT-1                        PIC 9(01)   VALUE 1.
           05  CT-2                        PIC 9(01)   VALUE 2.
           05  CT-3                        PIC 9(01)   VALUE 3.
           05  CT-ANO-MINIMO               PIC 9(04)   VALUE 1993.
           05  CT-2000                     PIC 9(04)   VALUE 2000.
           05  CT-99999999999999           PIC 9(14)   VALUE
                                   99999999999999.
      ******************************************************************
      *                AREA DE MENSAJES                                *
      ******************************************************************
       01  ME-MENSAJES-ERROR.
           05  ME-ESPECIFICAR-FECHAS       PIC X(07)   VALUE 'V1E0052'.
           05  ME-FECHA-DESDE-VAL          PIC X(07)   VALUE 'V1E0053'.
           05  ME-ESPECIFICAR-ANO-VALIDO   PIC X(07)   VALUE 'V1E0054'.
           05  ME-FECHA-HASTA-VAL          PIC X(07)   VALUE 'V1E0055'.
           05  ME-INGRESAR-NAC             PIC X(07)   VALUE 'V1E0057'.
           05  ME-INGRESAR-IDENTIFI        PIC X(07)   VALUE 'V1E0058'.
           05  ME-FECHA-HASTA-MENOR        PIC X(07)   VALUE 'V1E0056'.
           05  ME-TECLA-INCORRECTA         PIC X(07)   VALUE 'V1E0030'.
           05  ME-ERROR-EN-RUTINA          PIC X(07)   VALUE 'V1E0034'.
           05  ME-FECHA-ILOGICA            PIC X(07)   VALUE 'V1E0006'.
           05  ME-NO-INF-SOLICITADA        PIC X(07)   VALUE 'V1E0060'.
           05  ME-CAPACIDAD-EXCEDIDA       PIC X(07)   VALUE 'V1E0061'.
      ******************************************************************
      *                       COPYS UTILIZADAS                         *
      ******************************************************************
      * COPY DE LA RUTINA TC8C1220
           COPY TCWC1750.
      * COPY DFHAID
           COPY DFHAID.
      * COPY PARA ATRIBUTOS DE LA PANTALLA
           COPY DFHBMSCA.
      * COPY PARA ERRORES DB2
           COPY QCWCL20.
      * COPY PARA EL PROGRAMA DE ABEND
       01  WS-QGECABC-01.
           COPY QGECABC.
       COPY QCWCI20.
      ******************************************************************
      *                       AREA DE TABLAS                           *
      ******************************************************************
      *    EXEC SQL
      *        INCLUDE SQLCA
      *    END-EXEC
      *
           EXEC SQL
               INCLUDE V1GT001
           END-EXEC
      *
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA.
      *
           COPY QGECCAA.
       COPY V1NC058.
       COPY V1EC001.
      ******************************************************************
      *                                                                *
      *           P R O C E D U R E      D I V I S I O N               *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION USING V1NC058I V1EC001.
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
           GOBACK
           .
      ******************************************************************
      *                                                                *
      *                         1000-INICIO                            *
      *                                                                *
      *    INICIALIZAR VARIABLES AUXILIARES, COPYS,  AVISOS Y ERRORES  *
      *    DIRECCIONAR COPY DEL MAPA Y DE COMUNICACION VE1C001         *
      *    ACTIVAR SWITCH DE OPERACION NO CONTABLE                     *
      ******************************************************************
       1000-INICIO.
      *
           SET ADDRESS OF V1NC058I         TO CAA-PTR-COPYIN
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
      *    ANALIZAR SI FUE ESTADO DE INICIO O CONTINUACION             *
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
      *                     2100-ESTADO-INICIO                         *
      *    DAR CONTROL A LA ARQUITECTURA PARA ENTRAR NOSOTROS MISMOS   *
      *    EN ESTADO CONTINUACION                                      *
      *    POSICIONAR EL CURSOR EN EL PRIMER CAMPO DEL MAPA            *
      ******************************************************************
       2100-ESTADO-INICIO.
      *
           INITIALIZE V1NC058I
      *
           IF  CAA-CASO = '2'
      *        RETORNO CON CLEAR
               MOVE FEC-FECHA-DESDE(7:2)   TO FEC1038I
               MOVE FEC-FECHA-DESDE(5:2)   TO FEC1043I
               MOVE FEC-FECHA-DESDE(1:4)   TO FEC1048I
               MOVE FEC-FECHA-HASTA(7:2)   TO FEC1138I
               MOVE FEC-FECHA-HASTA(5:2)   TO FEC1143I
               MOVE FEC-FECHA-HASTA(1:4)   TO FEC1148I

               MOVE CED-NAC1               TO CED1338I
               IF  CED-NUM1 EQUAL ZEROS OR SPACES OR LOW-VALUES
                   MOVE LOW-VALUES         TO CED1342I
               ELSE
                   MOVE CED-NUM1           TO CED1342I
               END-IF

               EVALUATE SEG
                   WHEN 1
                       MOVE 'X' TO SE1438I
                   WHEN 2
                       MOVE 'X' TO SE1538I
                   WHEN 3
                       MOVE 'X' TO SE1638I
               END-EVALUATE
           ELSE
               MOVE FUNCTION CURRENT-DATE  TO WS-CURRENT-DATE
               MOVE WS-FEC-DIA-AAAAMMDD    TO WS-FECHA-E
               MOVE WS-DIA-E               TO FEC1038I
                                              FEC1138I
               MOVE WS-MES-E               TO FEC1043I
                                              FEC1143I
               MOVE WS-ANO-E               TO FEC1048I
                                              FEC1148I
           END-IF
      *
           SET CAA-88-ESTADO-CONTIN        TO TRUE
           SET CAA-88-ACCION-TERMINAL      TO TRUE
           PERFORM ATT-CAMPOS
           MOVE CT-CURSOR                  TO FEC1038L
      *
           .
       2100-ESTADO-INICIO-EXIT.
           EXIT.
      ******************************************************************
      *                    2200-ESTADO-CONTINUACION                    *
      *                                                                *
      *   - PARA TECLA INTRO- VALIDAR MAPA                             *
      *                     - BUSCAR REGISTROS EN MECADO LIBRE         *
      *                     - LLAMAR A V172                            *
      *   - PARA TECLA F6   - VALIDAR MAPA                             *
      *                     - LLAMAR A V174                            *
      *   - PARA TECLA CL   - RETORNAR AL MENU PRINCIPAL               *
      *   - PARA OTRA TECLA - EMITIR ERROR                             *
      ******************************************************************
       2200-ESTADO-CONTINUACION.
      *
           INITIALIZE                      GRP-DATOS-V171
      *
           PERFORM 2300-VALIDAR-MAPA
              THRU 2300-VALIDAR-MAPA-EXIT
      *
           EVALUATE CAA-TECLA
               WHEN CT-INTRO
                   PERFORM 2400-BUSCA-REGISTROS
                      THRU 2400-BUSCA-REGISTROS-EXIT

                   PERFORM 2700-LLAMAR-V172
                      THRU 2700-LLAMAR-V172-EXIT

               WHEN CT-F6
                   PERFORM 2800-LLAMAR-V174
                      THRU 2800-LLAMAR-V174-EXIT

               WHEN CT-BORRA
                   SET CAA-88-CODTRAN-SIG-ULTI TO TRUE
                   SET CAA-88-ACCION-PROGRAMA  TO TRUE
                   SET CAA-88-ESTADO-INICIO    TO TRUE

               WHEN OTHER
                   MOVE ME-TECLA-INCORRECTA    TO CAA-COD-ERROR
                   PERFORM REINPUT
           END-EVALUATE
      *
           .
       2200-ESTADO-CONTINUACION-EXIT.
           EXIT.
      ******************************************************************
      *                    2300-VALIDAR-MAPA                           *
      ******************************************************************
       2300-VALIDAR-MAPA.
      *
           MOVE FEC1038I                   TO WS-DD-D
           MOVE FEC1043I                   TO WS-MM-D
           MOVE FEC1048I                   TO WS-AA-D
           MOVE WS-FECHA-DESDE             TO FEC-FECHA-DESDE
           MOVE FEC1138I                   TO WS-DD-H
           MOVE FEC1143I                   TO WS-MM-H
           MOVE FEC1148I                   TO WS-AA-H
           MOVE WS-FECHA-HASTA             TO FEC-FECHA-HASTA
           MOVE CED1338I                   TO CED-NAC1
           MOVE CED1342I                   TO TEMP-NUM
           MOVE TEMP-NUM                   TO CED-NUM1
      *    MOVE SE1438I                    TO SEG

      *
           PERFORM 2600-FECHAS-LOGICAS
              THRU 2600-FECHAS-LOGICAS-EXIT
      *
           IF  WS-FECHA-DESDE IS EQUAL ZEROES AND
               WS-FECHA-HASTA IS EQUAL ZEROES
               MOVE ME-ESPECIFICAR-FECHAS  TO CAA-COD-ERROR
               MOVE CT-CURSOR              TO FEC1038L
               PERFORM REINPUT
           END-IF
      *
           MOVE FEC1048I                   TO W-DANO
           MOVE FEC1043I                   TO W-DMES
           MOVE FEC1038I                   TO W-DDIA
           MOVE FEC1148I                   TO W-HANO
           MOVE FEC1143I                   TO W-HMES
           MOVE FEC1138I                   TO W-HDIA

           IF  NOT W-DANOOK AND NOT W-DMESOK AND NOT  W-DDIAOK
               MOVE ME-FECHA-DESDE-VAL     TO CAA-COD-ERROR
               MOVE CT-CURSOR              TO FEC1038L
               PERFORM REINPUT
           END-IF
      *
           IF  NOT W-HANOOK AND NOT W-HMESOK AND NOT W-HDIAOK
               MOVE ME-FECHA-HASTA-VAL     TO CAA-COD-ERROR
               MOVE CT-CURSOR              TO FEC1038L
               PERFORM REINPUT
           END-IF
      *
           IF  WS-AA-D LESS CT-ANO-MINIMO
               MOVE ME-ESPECIFICAR-ANO-VALIDO
                                           TO CAA-COD-ERROR
               MOVE CT-CURSOR              TO FEC1048L
               PERFORM REINPUT
           END-IF
      *
           IF  WS-AA-H LESS CT-ANO-MINIMO
               MOVE ME-ESPECIFICAR-ANO-VALIDO
                                           TO CAA-COD-ERROR
               MOVE CT-CURSOR              TO FEC1148L
               PERFORM REINPUT
           END-IF
      *
           IF  WS-FECHA-DESDE GREATER WS-FECHA-HASTA
               MOVE ME-FECHA-HASTA-MENOR   TO CAA-COD-ERROR
               MOVE CT-CURSOR              TO FEC1138L
               PERFORM REINPUT
           END-IF
      *
           PERFORM 2910-VALIDA-MAPA-I
              THRU 2910-VALIDA-MAPA-I-EXIT
      *
           .
       2300-VALIDAR-MAPA-EXIT.
           EXIT.
      ******************************************************************
      *                     2400-BUSCA-REGISTROS                       *
      *    VALIDA QUE EXISTA INFORMACION PARA EL CRITERIO  DE          *
      *    BUSQUEDA DADO                                               *
      ******************************************************************
       2400-BUSCA-REGISTROS.
      *
           MOVE WS-FECHA-DESDE             TO WS-SP1-FIN
           MOVE WS-FECHA-HASTA             TO WS-SP2-FIN
      *
           IF  CED-NAC1 NOT EQUAL SPACES AND LOW-VALUES AND
               CED-NUM1 NOT EQUAL ZEROS
               MOVE CED1338I               TO WS-SP1-NAC
                                              WS-SP2-NAC
               MOVE CED1342I               TO WS-SP1-NUM
                                              WS-SP2-NUM
               PERFORM 2500-LEER-MERC-LIB-DIV
                  THRU 2500-LEER-MERC-LIB-DIV-EXIT
           ELSE
               MOVE CT-A                   TO WS-SP1-NAC
               MOVE CT-Z                   TO WS-SP2-NAC
               MOVE CT-1                   TO WS-SP1-NUM
               MOVE CT-99999999999999      TO WS-SP2-NUM
               PERFORM 2500-LEER-MERC-LIB-DIV
                  THRU 2500-LEER-MERC-LIB-DIV-EXIT
           END-IF
      *
           IF  WS-CANT-REG EQUAL ZERO
               MOVE ME-NO-INF-SOLICITADA   TO CAA-COD-ERROR
               MOVE -1                     TO FEC1038L
               PERFORM REINPUT
           END-IF
      *
           IF  WS-CANT-REG GREATER THAN CT-2000
               MOVE ME-CAPACIDAD-EXCEDIDA  TO CAA-COD-ERROR
               MOVE -1                     TO FEC1038L
               PERFORM REINPUT
           END-IF
      *
           .
       2400-BUSCA-REGISTROS-EXIT.
           EXIT.
      ******************************************************************
      *                     2500-LEER-MERC-LIB-DIV                     *
      *    SE HACE UN SELECT COUNT PARA SABER CANTIDAD DE REGISTOS QUE *
      *    CUMPLEN LA CONDICION                                        *
      ******************************************************************
       2500-LEER-MERC-LIB-DIV.
      *
           EXEC SQL
               SELECT COUNT(*)
               INTO :WS-CANT-REG
               FROM V1DT001
              WHERE (FEC_OPE BETWEEN :WS-SP1-FIN AND :WS-SP2-FIN) AND
                    (NUM_DOC_PERSONA BETWEEN :WS-SP1-CED AND
                                             :WS-SP2-CED) AND
                    NOT (TIP_OPE BETWEEN '5' AND '8')
           END-EXEC
      *
           MOVE SQLCODE                    TO DB2-RETURN-CDE
           EVALUATE TRUE
               WHEN DB2-OK
                   CONTINUE

               WHEN OTHER
                   INITIALIZE QGECABC
                   MOVE CT-V1DT001         TO ABC-OBJETO-ERROR
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *
           .
       2500-LEER-MERC-LIB-DIV-EXIT.
           EXIT.
      ******************************************************************
      *                      2600-FECHAS-LOGICAS                       *
      *    COMPROBAR QUE LAS FECHAS SEAN LOGICAS                       *
      ******************************************************************
       2600-FECHAS-LOGICAS.
      *
           INITIALIZE TCWC1750
           MOVE CT-OPCION-1                TO W175-CDOPCIO
           MOVE WS-DD-D                    TO WS-DIA-DESDE-DMA
           MOVE WS-MM-D                    TO WS-MES-DESDE-DMA
           MOVE WS-AA-D                    TO WS-ANO-DESDE-DMA
           MOVE WS-FECHA-DESDE-DMA         TO W175-FHGRE1

           PERFORM 2900-LLAMADA-TC8C1220
              THRU 2900-LLAMADA-TC8C1220-EXIT

           IF  W175-CDRETORN EQUAL CT-FECHA-ILOGICA
               MOVE CT-CURSOR              TO FEC1038L
               MOVE ME-FECHA-ILOGICA       TO CAA-COD-ERROR
               MOVE 'DESDE'                TO CAA-VAR1-ERROR
               PERFORM REINPUT
           END-IF
      *
           INITIALIZE                      TCWC1750
           MOVE CT-OPCION-1                TO W175-CDOPCIO
           MOVE WS-DD-H                    TO WS-DIA-HASTA-DMA
           MOVE WS-MM-H                    TO WS-MES-HASTA-DMA
           MOVE WS-AA-H                    TO WS-ANO-HASTA-DMA
           MOVE WS-FECHA-HASTA-DMA         TO W175-FHGRE1

           PERFORM 2900-LLAMADA-TC8C1220
              THRU 2900-LLAMADA-TC8C1220-EXIT

           IF  W175-CDRETORN EQUAL CT-FECHA-ILOGICA
               MOVE CT-CURSOR              TO FEC1138L
               MOVE ME-FECHA-ILOGICA       TO CAA-COD-ERROR
               MOVE 'HASTA'                TO CAA-VAR1-ERROR
               PERFORM REINPUT
           END-IF
      *
           .
       2600-FECHAS-LOGICAS-EXIT.
           EXIT.
      ******************************************************************
      *                        2700-LLAMAR-V172                        *
      *    LLAMAR A LA TRANSACION V172 PASANDO A TRAVES DE LA COPY     *
      *    V1EC001 LA FECHA DESDE-HASTA                                *
      ******************************************************************
       2700-LLAMAR-V172.
      *
           SET CAA-88-ACCION-PROGRAMA      TO TRUE
           SET CAA-88-ESTADO-INICIO        TO TRUE
           SET CAA-88-CADENA-ANADIR        TO TRUE
           MOVE '2'                        TO CAA-CASO-CAD
           MOVE CT-TRAN-V172               TO CAA-CODTRAN-SIG
           MOVE CAA-CODTRAN                TO COD-TRANSACCION

           MOVE WS-SEG                     TO SEG
      *
           .
       2700-LLAMAR-V172-EXIT.
           EXIT.
      ******************************************************************
      *                      2800-LLAMAR-V174                          *
      *    LLAMAR A LA TRANSACION V174 PASANDO A TRAVES DE LA COPY     *
      *    V1EC001 LA FECHA DESDE-HASTA                                *
      ******************************************************************
       2800-LLAMAR-V174.
      *
           SET CAA-88-ACCION-PROGRAMA      TO TRUE
           SET CAA-88-ESTADO-INICIO        TO TRUE
           SET CAA-88-CADENA-ANADIR        TO TRUE
           MOVE '2'                        TO CAA-CASO-CAD
           MOVE CT-TRAN-V174               TO CAA-CODTRAN-SIG
           MOVE CAA-CODTRAN                TO COD-TRANSACCION
      *
           .
       2800-LLAMAR-V174-EXIT.
           EXIT.
      ******************************************************************
      *                    2900-LLAMADA-TC8C1220                       *
      *    VALIDAR QUE LA FECHA SEA LOGICA                             *
      ******************************************************************
       2900-LLAMADA-TC8C1220.
      *
           CALL CT-TC8C1220 USING TCWC1750
      *
           EVALUATE W175-CDRETORN
      *
               WHEN CT-RETORNO-OK
               WHEN CT-FECHA-ILOGICA
      *
                   CONTINUE
      *
               WHEN OTHER
      *
                   MOVE ME-ERROR-EN-RUTINA TO CAA-COD-ERROR
                   MOVE CT-TC8C1220        TO CAA-VAR1-ERROR
                   PERFORM REINPUT
      *
           END-EVALUATE
           .
       2900-LLAMADA-TC8C1220-EXIT.
           EXIT.
      ******************************************************************
      *                     2910-VALIDA-MAPA-I                         *
      ******************************************************************
       2910-VALIDA-MAPA-I.
      *
           IF  CED1338I NOT EQUAL SPACES AND LOW-VALUES
               EVALUATE CED-NAC1
                   WHEN CT-V
                   WHEN CT-E
                   WHEN CT-J
                   WHEN CT-X
                   WHEN CT-G
                   WHEN CT-P
                       CONTINUE

                   WHEN OTHER
                       MOVE ME-INGRESAR-NAC    TO CAA-COD-ERROR
                       MOVE CT-CURSOR          TO CED1338L
                       PERFORM REINPUT
               END-EVALUATE

               IF  CED1342I EQUAL ZEROES OR SPACES OR LOW-VALUES
                   MOVE ME-INGRESAR-IDENTIFI   TO CAA-COD-ERROR
                   MOVE CT-CURSOR              TO CED1342L
                   PERFORM REINPUT
               END-IF
           END-IF
      *
           PERFORM 2920-VALIDA-MAPA-II
              THRU 2920-VALIDA-MAPA-II-EXIT
      *
           .
       2910-VALIDA-MAPA-I-EXIT.
           EXIT.
      ******************************************************************
      *                      2920-VALIDA-MAPA-II                       *
      ******************************************************************
       2920-VALIDA-MAPA-II.
      *
           IF  CED-NAC1 EQUAL SPACE
               MOVE SPACE                  TO CED1338I
               MOVE SPACES                 TO CED1342I
           END-IF
      *
           IF  SE1438I NOT EQUAL (SPACE AND CT-X)
               MOVE SPACE                  TO SE1438I
           END-IF
      *
           MOVE ZEROS                      TO WS-SEG
           IF  SE1438I EQUAL CT-X
               MOVE CT-1                   TO WS-SEG
               MOVE CT-CORPORATIVO         TO WS-SEGMENTO
           END-IF
      *
           IF  SE1538I NOT EQUAL (SPACE AND CT-X)
               MOVE SPACE                  TO SE1538I
           END-IF
      *
           IF  SE1538I EQUAL CT-X
               MOVE CT-2                   TO WS-SEG
               MOVE CT-EMPRESARIAL         TO WS-SEGMENTO
           END-IF
      *
           IF  SE1638I NOT EQUAL (SPACE AND CT-X)
               MOVE SPACE                  TO SE1638I
           END-IF
      *
           IF  SE1638I EQUAL CT-X
               MOVE CT-3                   TO WS-SEG
               MOVE CT-COMERCIAL           TO WS-SEGMENTO
           END-IF
      *
           .
       2920-VALIDA-MAPA-II-EXIT.
           EXIT.
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
           MOVE ATRI-NOP-NUM-FST           TO FEC1038A
                                              FEC1138A
                                              FEC1043A
                                              FEC1143A
                                              FEC1048A
                                              FEC1148A
                                              CED1342A
           MOVE ATRI-NOP-FST               TO CED1338A
                                              SE1438A
                                              SE1538A
                                              SE1638A
      *
           .
      ******************************************************************
      *                         3000-FIN                               *
      ******************************************************************
       3000-FIN.
      *
           EXEC CICS
               RETURN
           END-EXEC
      *
           .
       3000-FIN-EXIT.
      ******************************************************************
      *                      9999-ABEND-DB2                            *
      *   LLAMADA A LA RUTINA DE ABEND PARA EL DB2                     *
      ******************************************************************
       9999-ABEND-DB2.
      *
           MOVE  CT-S                      TO ABC-ABEND
           MOVE  CT-PROGRAMA               TO ABC-PROGRAMA
           MOVE  SQLCODE                   TO ABC-SQLCODE
           MOVE  SQLERRM                   TO ABC-SQLERRM
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
