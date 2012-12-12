      ******************************************************************
      *                                                                *
      *  PROGRAMA: V11C03Z                                             *
      *                                                                *
      *  FECHA CREACION: 18/06/2008                                    *
      *                                                                *
      *  AUTOR: FACTORIA                                               *
      *                                                                *
      *  APLICACION: MERCADO LIBRE DE DIVISAS. (V1) VENEZUELA.         *
      *                                                                *
      *  DESCRIPCION: PROGRAMA QUE PIDE RIF PARA DETALLES DE UN CLIENTE*
      *                                                                *
      ******************************************************************
      *                                                                *
      *         I D E N T I F I C A T I O N   D I V I S I O N          *
      *                                                                *
      *             INFORMACION GENERAL SOBRE EL PROGRAMA              *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.   V11C03Z.
       AUTHOR.       FACTORIA.
       DATE-WRITTEN. 18/06/2008
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
      *                        AREA DE CONTANTES                       *
      ******************************************************************
       01  CT-CONTANTES.
      *                                                                *
           05  CT-1                        PIC 9(01)   VALUE 1.
           05  CT-2                        PIC 9(01)   VALUE 2.
           05  CT-14                       PIC 9(02)   VALUE 14.
           05  CT-ESTADO-INICIO            PIC X(01)   VALUE 'I'.
           05  CT-ESTADO-CONTINUA          PIC X(01)   VALUE 'C'.
           05  CT-INTRO                    PIC X(02)   VALUE '00'.
           05  CT-BORRA                    PIC X(02)   VALUE '99'.
           05  TEMP-CED.
               10 TEMP-NAC                 PIC X(01)   VALUE SPACE.
               10 TEMP-NUM                 PIC X(14)   VALUE ZEROS.
      ******************************************************************
      *                AREA DE MENSAJES                                *
      ******************************************************************
       01  ME-MENSAJES-ERROR.
           05  ME-TECLA-INCORRECTA         PIC X(07)   VALUE 'V1E0030'.
           05  ME-ERROR-NACIONALIDAD       PIC X(07)   VALUE 'V1E0066'.
           05  ME-ERROR-RIF                PIC X(07)   VALUE 'V1E0001'.
       01  MA-MENSAJES-AVISO.
           05  MA-NO-HAY-OPERACIONES       PIC X(07)   VALUE 'V1A0055'.
      ******************************************************************
      *                    COPYS UTILIZADAS                            *
      ******************************************************************
      **** COPY DFHAID *************************************************
           COPY DFHAID.
      *
       COPY QCWCI20.
      *
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA.
      *
           COPY QGECCAA.
       COPY V1NCX20.
       COPY V1EC001.
      ******************************************************************
      *                                                                *
      *           P R O C E D U R E      D I V I S I O N               *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION USING V1NCX20I V1EC001.
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
      ******************************************************************
       1000-INICIO.
      *
           SET ADDRESS OF V1NCX20I         TO CAA-PTR-COPYIN
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
      ******************************************************************
       2000-PROCESO.
      *
           EVALUATE CAA-ESTADO
      *
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
      *   - SI COD-TRAN = 150 (LLAMADA DESDE EL MENU) LIMPIAR RIF  *
      *     DEL MAPA. SI COD-TRAN = 161 (RETORNO DE V161) MOVER EL *
      *     RIF DE LA COPY AL CAMPO DEL MAPA.                          *
      *   - POSICIONAR EL CURSOR EN EL PRIMER CAMPO DEL MAPA           *
      ******************************************************************
       2100-ESTADO-INICIO.
      *
           INITIALIZE V1NCX20I
      *
           IF  CAA-CASO = '2'
               INITIALIZE CAA-CASO CAA-CASO-CAD

               MOVE COD-RIF(1:1)           TO NAC1338I
               MOVE COD-RIF(2:14)          TO NUM1340I
           ELSE
               MOVE LOW-VALUES             TO NAC1338I
                                              NUM1340I
           END-IF
      *
           SET CAA-88-ESTADO-CONTIN        TO TRUE
           SET CAA-88-ACCION-TERMINAL      TO TRUE
           PERFORM ATT-CAMPOS
           MOVE -1                         TO NAC1338L
      *
           .
       2100-ESTADO-INICIO-EXIT.
           EXIT.
      ******************************************************************
      *                        2200-ESTADO-CONTINUACION                *
      *   - PARA TECLA INTRO- VALIDAR QUE VENGAN INFORMADOS            *
      *                       CORRECTAMENTE LA NACIONALIDAD Y EL RIF   *
      *                     - LLAMAR A V161                            *
      *   - PARA TECLA CL- RETORNAR AL MENU PRINCIPAL                  *
      *   - PARA OTRA TECLA EMITIR ERROR                               *
      ******************************************************************
       2200-ESTADO-CONTINUACION.
      *
           INITIALIZE GRP-DATOS-V160
           EVALUATE CAA-TECLA
               WHEN CT-INTRO
                   PERFORM 2300-VALIDAR-ENTRADA
                      THRU 2300-VALIDAR-ENTRADA-EXIT

                   PERFORM 2400-LLAMAR-V161
                      THRU 2400-LLAMAR-V161-EXIT

               WHEN CT-BORRA
                   SET CAA-88-CODTRAN-SIG-ULTI TO TRUE
                   SET CAA-88-ACCION-PROGRAMA  TO TRUE
                   SET CAA-88-ESTADO-INICIO    TO TRUE

               WHEN OTHER
                   MOVE ME-TECLA-INCORRECTA    TO CAA-COD-ERROR
                   MOVE -1                     TO NAC1338L
                   PERFORM REINPUT
           END-EVALUATE
      *
           .
       2200-ESTADO-CONTINUACION-EXIT.
           EXIT.
      ******************************************************************
      *                        2300-VALIDAR-ENTRADA                    *
      *    VALIDAR QUE VENGAN INFORMADOS CORRECTAMENTE LA NACIONALIDAD *
      *    Y EL RIF                                                    *
      ******************************************************************
       2300-VALIDAR-ENTRADA.
      *
           IF  NAC1338I NOT = 'V' AND 'E' AND 'J' AND 'W' AND
                              'P' AND 'G'
               MOVE ME-ERROR-NACIONALIDAD  TO CAA-COD-ERROR
               MOVE -1                     TO NAC1338L
               PERFORM REINPUT
           END-IF

           IF  NUM1340I EQUAL SPACES OR ZEROS OR LOW-VALUES
               MOVE ME-ERROR-RIF           TO CAA-COD-ERROR
               MOVE 'RIF'                  TO CAA-VAR1-ERROR
               MOVE -1                     TO NUM1340L
               PERFORM REINPUT
           END-IF
      *
           .
       2300-VALIDAR-ENTRADA-EXIT.
           EXIT.
      ******************************************************************
      *                        2400-LLAMAR-V161                        *
      ******************************************************************
       2400-LLAMAR-V161.
      *
           SET CAA-88-ACCION-PROGRAMA      TO TRUE
           SET CAA-88-ESTADO-INICIO        TO TRUE
           SET CAA-88-CADENA-ANADIR        TO TRUE
           MOVE '1'                        TO CAA-CASO
           MOVE '2'                        TO CAA-CASO-CAD
      *
           MOVE 'V161'                     TO CAA-CODTRAN-SIG
           STRING NAC1338I NUM1340I DELIMITED BY SIZE
                                           INTO COD-RIF
      *
           MOVE ZEROS                      TO IND-PROCESO
      *
           .
       2400-LLAMAR-V161-EXIT.
           EXIT.
      ******************************************************************
      * REINPUT
      ******************************************************************
       REINPUT.
      *
           SET CAA-88-ESTADO-CONTIN   TO TRUE
           SET CAA-88-ACCION-TERMINAL TO TRUE
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
           MOVE ATRI-NOP-BRI-FST     TO NAC1338A
           MOVE ATRI-NOP-NUM-BRI-FST TO NUM1340A
      *
           .
      ******************************************************************
      *                            3000-FIN                            *
      ******************************************************************
       3000-FIN.
      *
           MOVE CAA-CODTRAN                TO COD-TRANSACCION
      *
           EXEC CICS
               RETURN
           END-EXEC
      *
           .
       3000-FIN-EXIT.
           EXIT.
      *
