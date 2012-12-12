      ******************************************************************
      *                                                                *
      *  PROGRAMA: V11C01M                                             *
      *                                                                *
      *  FECHA CREACION: 06/01/2007                                    *
      *                                                                *
      *  AUTOR: FACTORIA                                               *
      *                                                                *
      *  APLICACION: V1 - COMPRA Y VENTA DE DIVISAS                    *
      *                                                                *
      *  DESCRIPCION: MENU PRINCIPAL DE SEGURIDAD Y PROTECCION         *
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
       PROGRAM-ID.   V11C01M.
       AUTHOR.       FACTORIA.
       DATE-WRITTEN. 06/01/2007
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
      *
      *                        AREA DE CONTANTES                       *
      ******************************************************************
       01  CT-CONTANTES.
      *                                                                *
           05  CT-TRAN-V151                PIC X(04)   VALUE 'V151'.
           05  CT-TRAN-V160                PIC X(04)   VALUE 'V160'.
           05  CT-TRAN-V164                PIC X(04)   VALUE 'V164'.
           05  CT-TRAN-V167                PIC X(04)   VALUE 'V167'.
           05  CT-TRAN-V171                PIC X(04)   VALUE 'V171'.
           05  CT-OPC-COMPRA-VENTA         PIC 9(03)   VALUE 506.
           05  CT-OPC-EXP-CLIENTE          PIC 9(04)   VALUE 666.
      *    05  CT-OPC-TAQUILLA             PIC 9(04)   VALUE 826.
           05  CT-OPC-BENEFICIARIO         PIC 9(04)   VALUE 986.
           05  CT-OPC-NOM-BENEFIC          PIC 9(04)   VALUE 1146.
           05  CT-OPC-CONSOLIDADOS         PIC 9(04)   VALUE 1306.
      *
           05  CT-CURSOR                   PIC S9      VALUE -1.
           05  CT-INTRO                    PIC X(02)   VALUE '00'.
      ******************************************************************
      *                AREA DE MENSAJES                                *
      ******************************************************************
       01  ME-MENSAJES-ERROR.
           05  ME-TECLA-INCORRECTA         PIC X(07)   VALUE 'V1E0030'.
      *
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA.
           COPY QGECCAA.
       COPY V1NC01M.
       COPY V1EC001.
      ******************************************************************
      *                                                                *
      *           P R O C E D U R E      D I V I S I O N               *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION USING DFHCOMMAREA V1NC01MI V1EC001.
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
      *                         1000-INICIO                            *
      ******************************************************************
       1000-INICIO.
      *
           SET ADDRESS OF V1NC01MI         TO CAA-PTR-COPYIN
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
           SET  CAA-88-CONTABLE-NO         TO TRUE
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
      *                        2100-ESTADO-INICIO                      *
      ******************************************************************
       2100-ESTADO-INICIO.
      *
           INITIALIZE V1NC01MI
           REPLACING ALPHANUMERIC DATA BY LOW-VALUES
      *
           SET CAA-88-ESTADO-CONTIN        TO TRUE
           SET CAA-88-ACCION-TERMINAL      TO TRUE
      *
           .
      *
       2100-ESTADO-INICIO-EXIT.
           EXIT.
      ******************************************************************
      *                        2200-ESTADO-CONTINUACION                *
      ******************************************************************
       2200-ESTADO-CONTINUACION.
      *
           MOVE CT-CURSOR                  TO OPC726L
      *
           IF CAA-TECLA NOT EQUAL CT-INTRO
              SET CAA-88-ACCION-TERMINAL   TO TRUE
              SET CAA-88-ESTADO-CONTIN     TO TRUE
              MOVE ME-TECLA-INCORRECTA     TO CAA-COD-ERROR
              PERFORM 3000-FIN
                 THRU 3000-FIN-EXIT
           END-IF
      *
           SET CAA-88-ACCION-PROGRAMA      TO TRUE
           SET CAA-88-CADENA-ANADIR        TO TRUE
           SET CAA-88-ESTADO-INICIO        TO TRUE
      *
           EVALUATE EIBCPOSN
               WHEN CT-OPC-COMPRA-VENTA
      *            MOVE ZEROS              TO FEC-BPBFIN90
                   MOVE '20081030'         TO FEC-BPBFIN90
                   MOVE 'V152'             TO CAA-CODTRAN-SIG

               WHEN CT-OPC-EXP-CLIENTE
                   MOVE CT-TRAN-V160       TO CAA-CODTRAN-SIG

      *        WHEN CT-OPC-TAQUILLA
      *            MOVE CT-TRAN-VE08       TO CAA-CODTRAN-SIG

               WHEN CT-OPC-BENEFICIARIO
                   MOVE CT-TRAN-V164       TO CAA-CODTRAN-SIG

               WHEN CT-OPC-NOM-BENEFIC
                   MOVE CT-TRAN-V167       TO CAA-CODTRAN-SIG

               WHEN CT-OPC-CONSOLIDADOS
                   MOVE CT-TRAN-V171       TO CAA-CODTRAN-SIG

               WHEN OTHER
                   SET CAA-88-ACCION-TERMINAL TO TRUE
                   SET CAA-88-ESTADO-CONTIN   TO TRUE
                   INITIALIZE CAA-CADENA
           END-EVALUATE
      *
           .
       2200-ESTADO-CONTINUACION-EXIT.
           EXIT.
      ******************************************************************
      *                            3000-FIN                            *
      ******************************************************************
       3000-FIN.
      *
           EXEC CICS
               RETURN
           END-EXEC.
      *
       3000-FIN-EXIT.
           EXIT.
      *
