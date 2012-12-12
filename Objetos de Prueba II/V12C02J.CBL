      ******************************************************************
      *                                                                *
      *  PROGRAMA: V12C02J                                             *
      *                                                                *
      *  FECHA CREACION: 26/06/2008                                    *
      *                                                                *
      *  AUTOR: FACTORIA                                               *
      *                                                                *
      *  APLICACION: V1                                                *
      *                                                                *
      *  DESCRIPCION: MUESTRA TOTALES DE COMPRA - VENTA                *
      *               PARA UN RIF, FECHA DESDE Y FECHA HASTA           *
      *               OPERACIONES QUE NO VIOLAN LOS PARAMETROS         *
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
       PROGRAM-ID.   V12C02J.
       AUTHOR.       FACTORIA.
       DATE-WRITTEN. 26/06/2008
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
      *
      *                        AREA DE CONTANTES                       *
      ******************************************************************
       01  CT-CONTANTES.
           05  CT-PROGRAMA                 PIC X(07)   VALUE 'V12C02J'.
           05  CT-QG1CABC                  PIC X(07)   VALUE 'QG1CABC'.
           05  CT-SI                       PIC X(01)   VALUE 'S'.
      ******************************************************************
      *                AREA DE SWITCHES                                *
      ******************************************************************
      ******************************************************************
      *                AREA DE VARIABLES AUXILIARES                    *
      ******************************************************************
       01  WS-VARIABLES-AUXILIARES.
           05  WS-PROGRAMA                 PIC X(08)   VALUE SPACES.
           05  WS-TABLA                    PIC X(08)   VALUE SPACES.
           05  WS-MONTO-EDIT               PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99.
      *
           05  WS-TOT-DIV                  PIC S9(13)V9(2) COMP-3.
           05  WS-TOT-BRS                  PIC S9(13)V9(2) COMP-3.
           05  WS-SQL-FECHA-DES            PIC X(08).
           05  WS-SQL-FECHA-HAS            PIC X(08).
      ******************************************************************
      *                    COPYS UTILIZADAS                            *
      ******************************************************************
      *--- COPY DFHAID
           COPY DFHAID.
      *--- ATRIBUTOS DE LA PANTALLA
           COPY QCWCI20.
      *--- COPY DE LA RUTINA QG1CABC (ABENDS)
           COPY QGECABC.
      *--- COPY QCWCL20
       COPY QCWCL20.
      *--- TABLA V1DT001
           EXEC SQL
               INCLUDE V1GT001
           END-EXEC.
      *
       LINKAGE SECTION.
      *
      *********** A R E A  D E  E N L A C E S  (DFHCOMMAREA)************
      *
       01  DFHCOMMAREA.
      *
       COPY QGECCAA.
           COPY V1EC001.
           COPY V1NC02J.
      ******************************************************************
      *                                                                *
      *           P R O C E D U R E      D I V I S I O N               *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION USING DFHCOMMAREA V1EC001 V1NC02JI.
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
      * 1000-INICIO                                                    *
      ******************************************************************
       1000-INICIO.
      *
           SET ADDRESS OF V1NC02JI         TO CAA-PTR-COPYIN
           SET ADDRESS OF V1EC001          TO CAA-PTRDATA
      *
           EXEC CICS
               IGNORE CONDITION ERROR
           END-EXEC
      *
           MOVE SPACES                     TO CAA-COD-AVISO1
                                              CAA-COD-AVISO2
                                              CAA-VAR1-ERROR
                                              CAA-VAR2-ERROR
                                              CAA-COD-ERROR
      *
           MOVE CAA-CODTRAN                TO CAA-CODTRAN-SIG
           SET  CAA-88-CONTABLE-NO         TO TRUE
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
                   PERFORM 2200-ESTADO-CONTINUACION
                      THRU 2200-ESTADO-CONTINUACION-EXIT
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
           INITIALIZE V1NC02JI
           SET CAA-88-ESTADO-CONTIN        TO TRUE
           SET CAA-88-ACCION-TERMINAL      TO TRUE
           MOVE CAA-CODTRAN                TO COD-TRANSACCION
      *
           PERFORM ATT-CAMPOS
      *
           MOVE V180-BPBCRP90    TO CRPI
           MOVE V153-NOM-RAZON   TO NOMBREI
           MOVE V157-LIT-PLURAL  TO LITERALI
      *
           MOVE V157-TOTAL-COMPRA-DL       TO WS-MONTO-EDIT
           MOVE WS-MONTO-EDIT              TO COMDOLI
           MOVE V157-TOTAL-COMPRA-BV       TO WS-MONTO-EDIT
           MOVE WS-MONTO-EDIT              TO COMBOLI
           MOVE V157-TOTAL-VENTA-DL        TO WS-MONTO-EDIT
           MOVE WS-MONTO-EDIT              TO VENDOLI
           MOVE V157-TOTAL-VENTA-BV        TO WS-MONTO-EDIT
           MOVE WS-MONTO-EDIT              TO VENBOLI
      *
           .
       2100-ESTADO-INICIO-EXIT.
           EXIT.
      ******************************************************************
      * 2200-ESTADO-CONTINUACION                                       *
      ******************************************************************
       2200-ESTADO-CONTINUACION.
      *
           SET CAA-88-CODTRAN-SIG-ULTI TO TRUE
           SET CAA-88-ACCION-PROGRAMA  TO TRUE
           SET CAA-88-ESTADO-INICIO    TO TRUE
      *
           .
       2200-ESTADO-CONTINUACION-EXIT.
           EXIT.
      ******************************************************************
      * ATT-CAMPOS
      ******************************************************************
       ATT-CAMPOS.
      *
           MOVE ATRI-PRO-BRI-FST TO COMDOLA VENDOLA
                                    COMBOLA VENBOLA
                                    CRPA
                                    NOMBREA
                                    LITERALA
      *
           .
      ******************************************************************
      * 3000-FIN                                                       *
      *  DEVUELVE CONTROL A LA ARQUITECTURA                            *
      ******************************************************************
       3000-FIN.
      *
           MOVE CAA-CODTRAN TO COD-TRANSACCION
           EXEC CICS
                RETURN
           END-EXEC
      *
           .
       3000-FIN-EXIT.
           EXIT.
      ******************************************************************
      * 9999-ABEND-DB2                                                 *
      ******************************************************************
       9999-ABEND-DB2.
      *
           INITIALIZE                         QGECABC
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
