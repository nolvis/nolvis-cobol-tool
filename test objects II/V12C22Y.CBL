      ******************************************************************
      *                                                                *
      *  PROGRAMA: V12C22Z                                             *
      *                                                                *
      *  FECHA CREACION: 03/01/2008                                    *
      *                                                                *
      *  AUTOR: FACTORIA                                               *
      *                                                                *
      *  APLICACION: MERCADO LIBRE DE DIVISAS. (V1) VENEZUELA.         *
      *                                                                *
      *  DESCRIPCION: PERMITE MOSTRAR LOS CLIENTE QUE SE ENCUENTRA EN  *
      *               EL ARCHIVO BPB-SEGURIDAD BANCARIA PARA UNA       *
      *               DETERMINADA FECHA
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
       PROGRAM-ID.   V12C22Z.
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
      *
           05  WS-CURRENT-DATE.
               10  WS-FEC-ACTUAL           PIC X(08)   VALUE SPACES.
               10  WS-HORA-ACTUAL          PIC X(08).
               10  FILLER                  PIC X(05)   VALUE SPACES.
      *
           05  WS-FECHA-E                  PIC 9(08)   VALUE ZEROS.
           05  WS-FECHA-E-R REDEFINES WS-FECHA-E.
               10  WS-ANO-E                PIC 9(04).
               10  WS-MES-E                PIC 9(02).
               10  WS-DIA-E                PIC 9(02).
      *
           05  IND-MAS-DATOS               PIC X(01)   VALUE SPACES.
           05  WS-GTS.
               10  FILLER                  PIC X(4)    VALUE '+GTS'.
               10  WS-GTS-SUFI             PIC X(4)    VALUE SPACES.
      *
           05  WS-GTS-DATOS                PIC X(80)   VALUE SPACES.
           05  WS-GTS-FILA.
               10  FILLER                  PIC X(02)   VALUE ' B'.
      *        COMIENZO
               10  FILLER                  PIC X(05)   VALUE SPACES.
               10  WS-GTS-NOMB-RAZON       PIC X(40)   VALUE SPACES.
               10  FILLER                  PIC X(02)   VALUE SPACES.
               10  WS-GTS-IDENTIDAD.
                   15  WS-GTS-NAC          PIC X(01)   VALUE SPACES.
                   15  FILLER              PIC X(01)   VALUE '-'.
                   15  WS-GTS-NUM          PIC X(14)   VALUE SPACES.
               10  WS-GTS-LLAVE            PIC 9(09).
      *
           05  WS-BPBSP190-2               PIC X(23)   VALUE SPACES.
           05  FILLER REDEFINES WS-BPBSP190-2.
               10  WS-BPBFIN90-2           PIC 9(08).
               10  WS-BPBCRP90-2           PIC X(15).
      *
           05  WS-TABLA                    PIC X(08)   VALUE SPACES.
           05  WS-SQL-LLAVE-REP            PIC S9(09) COMP.
           05  BPBCRP90-ANT                PIC X(15)   VALUE SPACES.
      ******************************************************************
      *                        REPORTES                                *
      ******************************************************************
       01  WS-CABECERA.
      *
           05  WS-CAB-LIN-01.
               10  FILLER                  PIC X(02)   VALUE ' -'.
               10  FILLER                  PIC X(16)   VALUE SPACES.
               10  FILLER                  PIC X(30)   VALUE
                                   '** MONITOREO DE COMPRA Y VENTA'.
               10  FILLER                  PIC X(14)   VALUE
                                   ' DE DIVISAS **'.
      *
           05  WS-CAB-LIN-02.
               10  FILLER                  PIC X(02)   VALUE ' -'.
               10  FILLER                  PIC X(27)   VALUE SPACES.
               10  FILLER                  PIC X(08)   VALUE
                                   'PARA EL '.
               10  WS-CAB-FECHA.
                   15  WS-CAB-DD           PIC 9(02)   VALUE ZEROES.
                   15  FILLER              PIC X(01)   VALUE SPACES.
                   15  WS-CAB-MM           PIC 9(02)   VALUE ZEROES.
                   15  FILLER              PIC X(01)   VALUE SPACES.
                   15  WS-CAB-AA           PIC 9(04)   VALUE ZEROES.
      *
           05  WS-CAB-LIN-04.
               10  FILLER                  PIC X(02)   VALUE ' -'.
               10  FILLER                  PIC X(10)   VALUE SPACES.
               10  FILLER                  PIC X(21)   VALUE
                                   'NOMBRE O RAZON SOCIAL'.
               10  FILLER                  PIC X(17)   VALUE SPACES.
               10  FILLER                  PIC X(14)   VALUE
                                   'IDENTIFICACION'.
      *
           05  WS-MSG-LIN-01.
               10  FILLER                  PIC X(10)   VALUE SPACES.
               10  FILLER                  PIC X(55)   VALUE ALL '*'.
           05  WS-MSG-LIN-02.
               10  FILLER                  PIC X(10)   VALUE SPACES.
               10  FILLER                  PIC X(01)   VALUE '*'.
               10  FILLER                  PIC X(53)   VALUE SPACES.
               10  FILLER                  PIC X(01)   VALUE '*'.
           05  WS-MSG-LIN-03.
               10  FILLER                  PIC X(10)   VALUE SPACES.
               10  FILLER                  PIC X(01)   VALUE '*'.
               10  FILLER                  PIC X(07)   VALUE SPACES.
               10  FILLER                  PIC X(15)   VALUE
                                   'PARA LA FECHA: '.
               10  WS-MSG-FEC-DD           PIC X(02)   VALUE SPACES.
               10  FILLER                  PIC X(01)   VALUE '/'.
               10  WS-MSG-FEC-MM           PIC X(02)   VALUE SPACES.
               10  FILLER                  PIC X(01)   VALUE '/'.
               10  WS-MSG-FEC-AA           PIC X(04)   VALUE SPACES.
               10  FILLER                  PIC X(01)   VALUE SPACES.
               10  FILLER                  PIC X(20)   VALUE
                                        'NO HUBO OPERACIONES '.
               10  FILLER                  PIC X(01)   VALUE '*'.
           05  WS-MSG-LIN-04.
               10  FILLER                  PIC X(10)   VALUE SPACES.
               10  FILLER                  PIC X(01)   VALUE '*'.
               10  FILLER                  PIC X(14)   VALUE SPACES.
               10  FILLER                  PIC X(31)   VALUE
                 'PULSE  <ENTER>  PARA  CONTINUAR'.
               10  FILLER                  PIC X(08)   VALUE SPACES.
               10  FILLER                  PIC X(01)   VALUE '*'.
      ******************************************************************
      *                    AREA  DE  SWITCHES                          *
      ******************************************************************
       01  SW-FIN-CURSOR                   PIC X(01)   VALUE 'N'.
           88  FIN-CURSOR                              VALUE 'S'.
           88  NO-FIN-CURSOR                           VALUE 'N'.
       01  SW-FIN-COLA                     PIC X(01)   VALUE SPACES.
           88  FIN-COLA                                VALUES 'S'.
           88  NO-FIN-COLA                             VALUES 'N'.
       01  SW-RET-CONSULTA                 PIC X(01)   VALUE 'N'.
           88  SI-RET-CONSULTA                         VALUE 'S'.
           88  NO-RET-CONSULTA                         VALUE 'N'.
      ******************************************************************
      *                    AREA DE CONTANTES                           *
      ******************************************************************
       01  CT-CONTANTES.
      *                                                                *
           05  CT-PROGRAMA                 PIC X(07)   VALUE 'V12C22Z'.
           05  CT-QG1CABC                  PIC X(07)   VALUE 'QG1CABC'.
           05  CT-V18C1220                 PIC X(08)   VALUE 'V18C1220'.
           05  CT-SI                       PIC X(01)   VALUE 'S'.
           05  CT-REG-GRABAR               PIC 9(02)   VALUE 50.
           05  CT-ERROR-WRITEQ             PIC X(20)
                                   VALUE 'ERROR EN CICS WRITQ'.
           05  CT-ERROR-DELETEQ            PIC X(20)
                                   VALUE 'ERROR CICS DELETEQ'.
           05  CT-ERROR-READQ              PIC X(20)
                                   VALUE 'ERROR CICS READQ'.
      ******************************************************************
      *                AREA DE MENSAJES                                *
      ******************************************************************
      ******************************************************************
      *                          CONTADORES                            *
      ******************************************************************
       01  CN-CONTADORES.
           05  CN-REGISTROS                PIC 9(05)   VALUE ZEROES.
      ******************************************************************
      *                          CONTADORES                            *
      ******************************************************************
       01  IN-INDICE.
           05  IN-REGISTRO                 PIC S9(03) COMP
                                   VALUE ZEROES.
      ******************************************************************
      *                       COPYS UTILIZADAS                         *
      ******************************************************************
       COPY TCWC1750.
      * COPY DFHAID
           COPY DFHAID.
      * COPY PARA ATRIBUTOS DE LA PANTALLA
           COPY QCWCI20.
      * COPY DE LA RUTINA QG1CABC (ABENDS)
           COPY QGECABC.
      * COPY DE VARIABLES DE MANEJO ERRORES NO DB2.
           COPY QRWCWAB.
      ******************************************************************
      *                       AREA DE TABLAS                           *
      ******************************************************************
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC
      *
           EXEC SQL
               INCLUDE BPBB0090
           END-EXEC
      *
           EXEC SQL
               DECLARE V12C22Z-V1DC22Z0 CURSOR FOR
               SELECT KP_B090BASE,
                      BPBNRS90,
                      BPBFIN90,
                      BPBMON90,
                      BPBCRP90
               FROM BPBB0090_BASE
               WHERE BPBFIN90 = :Z-BPBFIN90
               ORDER BY BPBCRP90,
                        KP_B090BASE
           END-EXEC
      *
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA.
           COPY QGECCAA.
       COPY QCRMGTS.
       COPY QCRMGTS1.
       COPY V1EC001.
      ******************************************************************
      *                                                                *
      *           P R O C E D U R E      D I V I S I O N               *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION USING QCRMGTSI V1EC001.
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
      *                         1000-INICIO                            *
      *    INICIALIZAR VARIABLES AUXILIARES, COPYS,  AVISOS Y ERRORES  *
      *    DIRECCIONAR COPY DEL MAPA Y DE COMUNICACION VE1C001         *
      *    ACTIVAR SWITCH DE OPERACION NO CONTABLE                     *
      ******************************************************************
       1000-INICIO.
      *
           SET ADDRESS OF QCRMGTSI         TO CAA-PTR-COPYIN
           SET ADDRESS OF V1EC001          TO CAA-PTRDATA
      *
           EXEC CICS
               IGNORE CONDITION ERROR
           END-EXEC
      *
           MOVE FUNCTION CURRENT-DATE      TO WS-CURRENT-DATE
           MOVE WS-FEC-ACTUAL              TO WS-FECHA-E

      *
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
      *
           SET NO-RET-CONSULTA             TO TRUE
           IF  COD-TRANSACCION EQUAL 'V153'
               SET SI-RET-CONSULTA         TO TRUE
           END-IF
           MOVE CAA-CODTRAN                TO COD-TRANSACCION
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
      *                     2100-ESTADO-INICIO                         *
      ******************************************************************
       2100-ESTADO-INICIO.
      *
           IF  CAA-CASO = '2'
               INITIALIZE CAA-CASO
               PERFORM PREPARAR-PAGINACION
           ELSE
               PERFORM BORRAR-COLA
               IF  FEC-BPBFIN90 = ZEROS
                   PERFORM BUSCAR-FECHA
               END-IF
      *
               PERFORM GRABAR-REGISTROS
                  THRU GRABAR-REGISTROS-EXIT
               IF  CN-REGISTROS = 0
                   PERFORM LISTADO-VACIO
                   MOVE '2'                TO CAA-CASO
               ELSE
                   PERFORM PREPARAR-PAGINACION
                   MOVE '1'                TO CAA-CASO
               END-IF
           END-IF
      *
           SET CAA-88-ESTADO-CONTIN        TO TRUE
           SET CAA-88-ACCION-TERMINAL      TO TRUE
           SET CAA-88-TIPO-SALIDA-PAG      TO TRUE
           MOVE CAA-CODTRAN                TO CAA-CODTRAN-SIG
      *
           .
       2100-ESTADO-INICIO-EXIT.
           EXIT.
      ******************************************************************
      * BUSCAR-FECHA
      ******************************************************************
       BUSCAR-FECHA.
      *
           INITIALIZE TCWC1750
           MOVE '2'                        TO W175-CDOPCIO
           MOVE WS-FEC-ACTUAL(1:4)         TO W175-FHGRE1(5:4)
           MOVE WS-FEC-ACTUAL(5:2)         TO W175-FHGRE1(3:2)
           MOVE WS-FEC-ACTUAL(7:2)         TO W175-FHGRE1(1:2)
           MOVE -14                        TO W175-FACTOR
           MOVE 'NA'                       TO W175-TPTRAT
      *
           CALL CT-V18C1220 USING TCWC1750
      *
           EVALUATE W175-CDRETORN
               WHEN ZEROS
                   MOVE W175-FHGREG(1:2)   TO FEC-BPBFIN90(7:2)
                   MOVE W175-FHGREG(3:2)   TO FEC-BPBFIN90(5:2)
                   MOVE W175-FHGREG(5:4)   TO FEC-BPBFIN90(1:4)

               WHEN OTHER
                   CONTINUE
           END-EVALUATE
      *
           MOVE FEC-BPBFIN90               TO BPBFIN90
           EXEC SQL
               SELECT MAX(BPBFIN90)
               INTO :Z-BPBFIN90
               FROM BPBB0090_BASE
               WHERE BPBFIN90 >= :Z-BPBFIN90
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN ZEROS
                   MOVE Z-BPBFIN90         TO FEC-BPBFIN90

               WHEN -305
                   CONTINUE

               WHEN OTHER
                   MOVE 'BPBB0090_BASE'    TO WS-TABLA
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *
           .
      ******************************************************************
      * GRABAR-CABECERA
      ******************************************************************
       GRABAR-CABECERA.
      *
           MOVE WS-CAB-LIN-01              TO WS-GTS-DATOS
           PERFORM ADD-REGISTRO-COLA
              THRU ADD-REGISTRO-COLA-EXIT
      *
           MOVE FEC-BPBFIN90(1:4)          TO WS-CAB-AA
           MOVE FEC-BPBFIN90(5:2)          TO WS-CAB-MM
           MOVE FEC-BPBFIN90(7:2)          TO WS-CAB-DD
           MOVE WS-CAB-LIN-02              TO WS-GTS-DATOS
           PERFORM ADD-REGISTRO-COLA
              THRU ADD-REGISTRO-COLA-EXIT
      *
           MOVE SPACES                     TO WS-GTS-DATOS
           PERFORM ADD-REGISTRO-COLA
              THRU ADD-REGISTRO-COLA-EXIT
      *
           MOVE WS-CAB-LIN-04              TO WS-GTS-DATOS
           PERFORM ADD-REGISTRO-COLA
              THRU ADD-REGISTRO-COLA-EXIT
      *
           .
       GRABAR-CABECERA-EXIT.
           EXIT.
      ******************************************************************
      * GRABAR-REGISTROS
      ******************************************************************
       GRABAR-REGISTROS.
      *
           SET NO-FIN-CURSOR               TO TRUE
           MOVE ZEROES                     TO CN-REGISTROS
           MOVE 'S'                        TO IND-MAS-DATOS
      *
           MOVE SPACES                     TO BPBCRP90-ANT
           MOVE FEC-BPBFIN90               TO BPBFIN90
      *
           PERFORM ABRIR-CURSOR
              THRU ABRIR-CURSOR-EXIT

           PERFORM LEER-CURSOR1
              THRU LEER-CURSOR1-EXIT
             UNTIL FIN-CURSOR

           PERFORM CERRAR-CURSOR
              THRU CERRAR-CURSOR-EXIT
      *
           .
       GRABAR-REGISTROS-EXIT.
           EXIT.
      ******************************************************************
      *                        ABRIR-CURSOR                            *
      ******************************************************************
       ABRIR-CURSOR.
      *
           EXEC SQL
               OPEN V12C22Z-V1DC22Z0
           END-EXEC
      *
           IF  SQLCODE NOT EQUAL ZEROES
               MOVE 'BPBB0090_BASE'        TO WS-TABLA
               PERFORM 9999-ABEND-DB2
                  THRU 9999-ABEND-DB2-EXIT
           END-IF
      *
           .
       ABRIR-CURSOR-EXIT.
           EXIT.
      ******************************************************************
      *                       LEER-CURSOR1                             *
      ******************************************************************
       LEER-CURSOR1.
      *
           EXEC SQL
               FETCH V12C22Z-V1DC22Z0
               INTO :KP-B090BASE,
                    :BPBNRS90,
                    :Z-BPBFIN90,
                    :BPBMON90,
                    :BPBCRP90
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN ZEROES
                   PERFORM PROC-LECTURA
                      THRU PROC-LECTURA-EXIT

               WHEN +100
                   SET FIN-CURSOR          TO TRUE
                   MOVE 'N'                TO IND-MAS-DATOS

               WHEN OTHER
                   MOVE 'BPBB0090_BASE'    TO WS-TABLA
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *
           .
       LEER-CURSOR1-EXIT.
           EXIT.
      ******************************************************************
      *                        PROC-LECTURA                            *
      ******************************************************************
       PROC-LECTURA.
      *
           IF  BPBCRP90-ANT NOT EQUAL BPBCRP90
               ADD 1                       TO CN-REGISTROS
      *
               IF  CAA-88-ESTADO-INICIO AND CN-REGISTROS = 1
                   PERFORM GRABAR-CABECERA
                      THRU GRABAR-CABECERA-EXIT
               END-IF

               IF  CN-REGISTROS > 5000
                   MOVE 'V1A0502'          TO CAA-COD-AVISO1
                   SET FIN-CURSOR          TO TRUE
               ELSE
                   MOVE BPBCRP90           TO BPBCRP90-ANT
      *
                   MOVE BPBNRS90           TO WS-GTS-NOMB-RAZON
                   MOVE BPBCRP90(1:1)      TO WS-GTS-NAC
                   MOVE BPBCRP90(2:14)     TO WS-GTS-NUM

                   MOVE WS-GTS-FILA        TO WS-GTS-DATOS
                   PERFORM ADD-REGISTRO-COLA
                      THRU ADD-REGISTRO-COLA-EXIT
               END-IF
      *
           .
       PROC-LECTURA-EXIT.
           EXIT.
      ******************************************************************
      *                       CERRAR-CURSOR                            *
      ******************************************************************
       CERRAR-CURSOR.
      *
           EXEC SQL
               CLOSE V12C22Z-V1DC22Z0
           END-EXEC
      *
           IF  SQLCODE NOT EQUAL ZEROES
               MOVE 'BPBB0090_BASE'        TO WS-TABLA
               PERFORM 9999-ABEND-DB2
                  THRU 9999-ABEND-DB2-EXIT
           END-IF
      *
           .
       CERRAR-CURSOR-EXIT.
           EXIT.
      ******************************************************************
      * LISTADO-VACIO
      ******************************************************************
       LISTADO-VACIO.
      *
           PERFORM GRABAR-CABECERA
      *
           MOVE ' +'                       TO WS-GTS-DATOS(1:2)
           MOVE SPACES                     TO WS-GTS-DATOS(3:77)
           PERFORM ADD-REGISTRO-COLA

           MOVE WS-MSG-LIN-01              TO WS-GTS-DATOS(3:77)
           PERFORM ADD-REGISTRO-COLA

           MOVE WS-MSG-LIN-02              TO WS-GTS-DATOS(3:77)
           PERFORM ADD-REGISTRO-COLA

           MOVE FEC-BPBFIN90(1:4)          TO WS-MSG-FEC-AA
           MOVE FEC-BPBFIN90(5:2)          TO WS-MSG-FEC-MM
           MOVE FEC-BPBFIN90(7:2)          TO WS-MSG-FEC-DD
           MOVE WS-MSG-LIN-03              TO WS-GTS-DATOS(3:77)
           PERFORM ADD-REGISTRO-COLA

           MOVE WS-MSG-LIN-04              TO WS-GTS-DATOS(3:77)
           PERFORM ADD-REGISTRO-COLA

           MOVE WS-MSG-LIN-02              TO WS-GTS-DATOS(3:77)
           PERFORM ADD-REGISTRO-COLA

           MOVE WS-MSG-LIN-01              TO WS-GTS-DATOS(3:77)
           PERFORM ADD-REGISTRO-COLA
      *
           INITIALIZE CAA-PAGINAR
           SET CAA-88-IND-VARSEL-NO        TO TRUE
           MOVE '00'                       TO CAA-FKEY-NUM(1)
           MOVE 'ENTER'                    TO CAA-FKEY-LIT(1)
           MOVE 'N'                        TO CAA-FKEY-SEL(1)
           MOVE SPACES                     TO CAA-CONTENID
           SET CAA-88-IND-MOD-DATO-NO      TO TRUE
           MOVE 04                         TO CAA-NUM-LIN-CAB-9
           SET CAA-88-IND-SCROLL-NO        TO TRUE
           MOVE 'N'                        TO CAA-IND-AVPAG
      *
           .
      ******************************************************************
      *                   PREPARAR-PAGINACION                          *
      ******************************************************************
       PREPARAR-PAGINACION.
      *
           INITIALIZE CAA-PAGINAR
           MOVE 'X'                        TO CAA-SEL-PERMIT(1)
           SET CAA-88-IND-VARSEL-NO        TO TRUE
      *
           MOVE '00'                       TO CAA-FKEY-NUM(1)
           MOVE 'ENTER'                    TO CAA-FKEY-LIT(1)
           MOVE 'S'                        TO CAA-FKEY-SEL(1)
           MOVE '06'                       TO CAA-FKEY-NUM(2)
           MOVE 'FECHA'                    TO CAA-FKEY-LIT(2)
           MOVE 'N'                        TO CAA-FKEY-SEL(2)
           MOVE SPACES                     TO CAA-CONTENID
      *
           SET CAA-88-IND-MOD-DATO-NO      TO TRUE
           MOVE 04                         TO CAA-NUM-LIN-CAB-9
           SET CAA-88-IND-SCROLL-NO        TO TRUE
           MOVE IND-MAS-DATOS              TO CAA-IND-AVPAG
      *
           .
      ******************************************************************
      *                    2200-ESTADO-CONTINUACION                    *
      ******************************************************************
       2200-ESTADO-CONTINUACION.
      *
           EVALUATE CAA-TECLA
               WHEN '00'
                   IF  CAA-CASO = '2'
                       PERFORM SALIR-TRAN-ANTERIOR
                   ELSE
                       PERFORM COMENZAR-PROCESO
                          THRU COMENZAR-PROCESO-EXIT
                   END-IF
               WHEN '06'
                   MOVE 'V151'                TO CAA-CODTRAN-SIG
                   SET CAA-88-ACCION-PROGRAMA TO TRUE
                   SET CAA-88-ESTADO-INICIO   TO TRUE
                   INITIALIZE CAA-CADENA
                              CAA-CASO-CAD
                              CAA-CASO
               WHEN '99'
                   PERFORM SALIR-TRAN-ANTERIOR

               WHEN '08'
                   PERFORM GRABAR-REGISTROS
                      THRU GRABAR-REGISTROS-EXIT

                   MOVE IND-MAS-DATOS          TO CAA-IND-AVPAG
                   SET CAA-88-ESTADO-CONTIN    TO TRUE
                   SET CAA-88-ACCION-TERMINAL  TO TRUE
           END-EVALUATE
      *
           .
       2200-ESTADO-CONTINUACION-EXIT.
           EXIT.
      ******************************************************************
      *                      COMENZAR-PROCESO                          *
      ******************************************************************
       COMENZAR-PROCESO.
      *
           MOVE 5                          TO IN-REGISTRO
           MOVE SPACES                     TO WS-GTS-DATOS(1:1)
           SET NO-FIN-COLA                 TO TRUE
      *
           PERFORM BUSCAR-SELECCION
              THRU BUSCAR-SELECCION-EXIT
             UNTIL WS-GTS-DATOS(1:1) NOT EQUAL SPACES OR
                   FIN-COLA
      *
           IF  NO-FIN-COLA
               PERFORM PROCESAR-SELECCION
                  THRU PROCESAR-SELECCION-EXIT
           END-IF
      *
           .
       COMENZAR-PROCESO-EXIT.
           EXIT.
      ******************************************************************
      *                  3300-BUSCAR-SELECCION                         *
      ******************************************************************
       BUSCAR-SELECCION.
      *
           PERFORM LEER-REGISTRO-COLA
              THRU LEER-REGISTRO-COLA-EXIT
      *
           ADD 1                           TO IN-REGISTRO
      *
           .
       BUSCAR-SELECCION-EXIT.
           EXIT.
      ******************************************************************
      *                    PROCESAR-SELECCION                          *
      ******************************************************************
       PROCESAR-SELECCION.
      *
           MOVE WS-GTS-DATOS               TO WS-GTS-FILA
           SUBTRACT 1                      FROM IN-REGISTRO
      *
           MOVE FEC-BPBFIN90               TO BPBFIN90
           MOVE WS-GTS-NAC                 TO BPBCRP90(1:1)
           MOVE WS-GTS-NUM                 TO BPBCRP90(2:14)
      *
           PERFORM BORRAR-SELECION-TS
              THRU BORRAR-SELECION-TS-EXIT

           PERFORM ACT-RECONOCIMIENTO
      *
           .
       PROCESAR-SELECCION-EXIT.
           EXIT.
      ******************************************************************
      * ACT-RECONOCIMIENTO
      ******************************************************************
       ACT-RECONOCIMIENTO.
      *
           EXEC SQL
               SELECT MIN(KP_B090BASE)
               INTO :KP-B090BASE
               FROM BPBB0090_BASE
               WHERE BPBFIN90 = :Z-BPBFIN90 AND
                     BPBCRP90 = :BPBCRP90
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN ZEROES
                   PERFORM ACTUALIZAR-HIJAS
      *
                   MOVE 'V153'                 TO CAA-CODTRAN-SIG
                   SET CAA-88-ACCION-PROGRAMA  TO TRUE
                   SET CAA-88-ESTADO-INICIO    TO TRUE
                   SET CAA-88-CADENA-ANADIR    TO TRUE
                   MOVE '2'                    TO CAA-CASO-CAD
                   INITIALIZE CAA-CASO
                   MOVE BPBFIN90               TO V153-BPBFIN90
                   MOVE BPBCRP90               TO V153-BPBCRP90

               WHEN -305
                   CONTINUE

               WHEN OTHER
                   MOVE 'BPBB0090_BASE'    TO WS-TABLA
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *
           .
      ******************************************************************
      * ACTUALIZAR-HIJAS
      ******************************************************************
       ACTUALIZAR-HIJAS.
      *
           MOVE WS-FEC-ACTUAL              TO W-BPBFRE90
           MOVE WS-HORA-ACTUAL(1:6)        TO W-BPBHRE90
           MOVE CAA-TERMINAL               TO W-BPBCRE90
      *
           EXEC SQL
               SELECT MIN(KS_B090F010)
               INTO :KS-B090F010
               FROM BPBB0090_F010
               WHERE KP_B090F010 = :KP-B090BASE AND
                     (BPBFRE90 = '        ' OR
                      BPBFRE90 = '00000000')
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN ZEROS
                   PERFORM REALIZAR-UPDATE

               WHEN -305
                   PERFORM BUSCAR-LLAVE-HIJAS
                   IF  KS-B090F010 > 199
                       MOVE 199 TO KS-B090F010
                       PERFORM REALIZAR-UPDATE
                   ELSE
                       PERFORM REALIZAR-INSERT
                   END-IF

               WHEN OTHER
                   MOVE 'BPBB0090_F010'    TO WS-TABLA
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *
           .
      ******************************************************************
      * BUSCAR-LLAVE-HIJAS
      ******************************************************************
       BUSCAR-LLAVE-HIJAS.
      *
           EXEC SQL
               SELECT MAX (KS_B090F010)
               INTO :KS-B090F010
               FROM BPBB0090_F010
               WHERE KP_B090F010 = :KP-B090BASE
           END-EXEC
      *    BUSCAR LLAVE HIJAS
           EVALUATE SQLCODE
               WHEN ZEROES
                   ADD 1                   TO KS-B090F010

               WHEN -305
                   MOVE 1                  TO KS-B090F010

               WHEN OTHER
                   MOVE 'BPBB0090_F010'    TO WS-TABLA
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *
           .
      ******************************************************************
      * REALIZAR-INSERT
      ******************************************************************
       REALIZAR-INSERT.
      *    FECHA
           EXEC SQL
               INSERT INTO BPBB0090_F010(
                  KP_B090F010,
                  KS_B090F010,
                  BPBFRE90)
               VALUES(
                  :KP-B090BASE,
                  :KS-B090F010,
                  :W-BPBFRE90)
           END-EXEC
           EVALUATE SQLCODE
               WHEN ZEROES
                   CONTINUE
               WHEN OTHER
                   MOVE 'BPBB0090_F010'    TO WS-TABLA
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *    HORA
           EXEC SQL
               INSERT INTO BPBB0090_F020(
                   KP_B090F020,
                   KS_B090F020,
                   BPBHRE90)
               VALUES(
                   :KP-B090BASE,
                   :KS-B090F010,
                   :W-BPBHRE90)
           END-EXEC
           EVALUATE SQLCODE
               WHEN ZEROES
                   CONTINUE
               WHEN OTHER
                   MOVE 'BPBB0090_F020'    TO WS-TABLA
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *    CODIGO
           EXEC SQL
               INSERT INTO BPBB0090_F030(
                   KP_B090F030,
                   KS_B090F030,
                   BPBCRE90)
               VALUES(
                   :KP-B090BASE,
                   :KS-B090F010,
                   :W-BPBCRE90)
           END-EXEC
           EVALUATE SQLCODE
               WHEN ZEROES
                   CONTINUE
               WHEN OTHER
                   MOVE 'BPBB0090_F030'    TO WS-TABLA
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *
           .
      ******************************************************************
      * REALIZAR-UPDATE
      ******************************************************************
       REALIZAR-UPDATE.
      *    FECHA
           EXEC SQL
               UPDATE BPBB0090_F010
               SET BPBFRE90 = :W-BPBFRE90
               WHERE KP_B090F010 = :KP-B090BASE AND
                     KS_B090F010 = :KS-B090F010
           END-EXEC
           EVALUATE SQLCODE
               WHEN ZEROES
                   CONTINUE
               WHEN OTHER
                   MOVE 'BPBB0090_F010'    TO WS-TABLA
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *    HORA
           EXEC SQL
               UPDATE BPBB0090_F020
               SET BPBHRE90 = :W-BPBHRE90
               WHERE KP_B090F020 = :KP-B090BASE AND
                     KS_B090F020 = :KS-B090F010
           END-EXEC
           EVALUATE SQLCODE
               WHEN ZEROES
                   CONTINUE
               WHEN OTHER
                   MOVE 'BPBB0090_F020'    TO WS-TABLA
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *    CODIGO
           EXEC SQL
               UPDATE BPBB0090_F030
               SET BPBCRE90 = :W-BPBCRE90
               WHERE KP_B090F030 = :KP-B090BASE AND
                     KS_B090F030 = :KS-B090F010
           END-EXEC
           EVALUATE SQLCODE
               WHEN ZEROES
                   CONTINUE
               WHEN OTHER
                   MOVE 'BPBB0090_F030'    TO WS-TABLA
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *
           .
      ******************************************************************
      *                    LEER-REGISTRO-COLA                          *
      ******************************************************************
       LEER-REGISTRO-COLA.
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
                   MOVE CT-ERROR-READQ     TO ABC-REFERENCIA
                   PERFORM 9999-ABEND-CICS
                      THRU 9999-ABEND-CICS-EXIT
           END-EVALUATE
      *
           .
       LEER-REGISTRO-COLA-EXIT.
           EXIT.
      ******************************************************************
      *                    BORRAR-SELECION-TS                          *
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
       BORRAR-SELECION-TS-EXIT.
           EXIT.
      ******************************************************************
      * BORRAR-COLA
      ******************************************************************
       BORRAR-COLA.
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
      * ADD-REGISTRO-COLA
      ******************************************************************
       ADD-REGISTRO-COLA.
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
       ADD-REGISTRO-COLA-EXIT.
           EXIT.
      ******************************************************************
      * SALIR-TRAN-ANTERIOR
      ******************************************************************
       SALIR-TRAN-ANTERIOR.
      *
           PERFORM BORRAR-COLA
           SET CAA-88-CODTRAN-SIG-ULTI TO TRUE
           SET CAA-88-ACCION-PROGRAMA  TO TRUE
           SET CAA-88-ESTADO-INICIO    TO TRUE
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
      ******************************************************************
      * 9999-ABEND-CICS                                                *
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
           INITIALIZE                         QGECABC
           MOVE CT-SI                      TO ABC-ABEND
           MOVE CT-PROGRAMA                TO ABC-PROGRAMA
           MOVE WS-TABLA                   TO ABC-OBJETO-ERROR
           MOVE SQLCODE                    TO ABC-SQLCODE
           MOVE SQLERRM                    TO ABC-SQLERRM
      *
           EXEC CICS
                LINK PROGRAM (CT-QG1CABC)
                COMMAREA (QGECABC)
           END-EXEC
      *
           .
       9999-ABEND-DB2-EXIT.
           EXIT.
      *
