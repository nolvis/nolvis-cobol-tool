      ******************************************************************
      *                                                                *
      *  PROGRAMA: V12C01J                                             *
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
      * PROCESO GLOBAL: DETALLE DE OPERACION QUE VIOLA LOS PARAMETROS  *
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
       PROGRAM-ID.   V12C01J.
       AUTHOR.       FACTORIA.
       DATE-WRITTEN. 18/11/2008
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
           05  WS-IDF-LLAVE-BASE           PIC S9(04) COMP-3.
           05  WS-PROGRAMA                 PIC X(07) VALUE SPACES.
           05  WS-CURRENT-DATE.
               10  WS-FEC-DIA-AAAAMMDD     PIC X(08)   VALUE SPACES.
               10  WS-HORA-DIA.
                   15  WS-HH-DIA           PIC X(02)   VALUE SPACES.
                   15  WS-MM-DIA           PIC X(02)   VALUE SPACES.
                   15  WS-SS-DIA           PIC X(02)   VALUE SPACES.
                   15  WS-CS-DIA           PIC X(02)   VALUE SPACES.
      *
            05 WS-FEC-OPE                    PIC X(8).
            05 REDEFINES WS-FEC-OPE.
               10 WS-AA-FIN89              PIC 9(4).
               10 WS-MM-FIN89              PIC 9(2).
               10 WS-DD-FIN89              PIC 9(2).
      *    CUENTA
           05  WS-CUENTA-EDIT              PIC Z(12).
           05  WS-CUENTA-MAPA.
               10  WS-CTA1                 PIC X(02).
               10  WS-SEP1                 PIC X(01) VALUE '-'.
               10  WS-CTA2                 PIC X(03).
               10  WS-SEP2                 PIC X(01) VALUE '-'.
               10  WS-CTA3                 PIC X(07).
      *    MONTOS
           05  WS-MONTO-EDIT               PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99.
      *
           05 WS-NUM-DOC-PERSONA-BE       PIC X(15) VALUE SPACES.
           05 WS-NOM-RAZON-BE       PIC X(40) VALUE SPACES.

           05 WS-I                 PIC 9(8)  VALUE ZEROS.
           05 WS-SW-CONTROL        PIC 9(2)  VALUE ZEROS.
           05 WS-PROGRAM           PIC X(8)  VALUE SPACES.
           05 WS-MTO-BS            PIC S9(13)V9(2) VALUE ZEROS.
           05 WS-MTO-DS            PIC S9(13)V9(2)  VALUE ZEROS.
           05  WS-OFICINA          PIC 9(06).
           05  WS-OFICINA-RE REDEFINES WS-OFICINA.
               10  WS-UNI-COF89    PIC 9(03).
               10  WS-OFC-COF89    PIC 9(03).
      *
           05 WS-CENTRO            PIC 9(9) VALUE ZEROS.
           05 REDEFINES WS-CENTRO.
             10 WS-FILLER          PIC 9(3).
             10 WS-COD-CENTRO      PIC 9(6).
             10 REDEFINES WS-COD-CENTRO.
               15 WS-UNI-COF105    PIC 9(3).
               15 WS-OFC-COF105    PIC 9(3).
      *
           05 WS-DESCRIPCION       PIC X(40) VALUE SPACES.
           05 REDEFINES WS-DESCRIPCION.
             10 WS-DES-OFICINA     PIC X(25).
      ******************************************************************
      *                    AREA  DE  SWITCHES                          *
      ******************************************************************
       01  SW-SWITCHES.
      *
           05  SW-TECLA-PRESS              PIC X(02).
               88  PF-INTRO                           VALUE '00'.
               88  PF-CLEAR                           VALUE '99'.
               88  PF-F01                             VALUE '01'.
               88  PF-F02                             VALUE '02'.
               88  PF-F04                             VALUE '04'.
               88  PF-F06                             VALUE '06'.
               88  PF-F07                             VALUE '07'.
               88  PF-F08                             VALUE '08'.
           05  SW-FIN-CURSOR               PIC X(01)  VALUE 'N'.
               88  FIN-CURSOR                         VALUE 'S'.
               88  NO-FIN-CURSOR                      VALUE 'N'.
      ******************************************************************
      *                        AREA DE CONTANTES                       *
      ******************************************************************
       01  CT-CONTANTES.
      *                                                                *
           05  CT-V19C001                  PIC X(08)   VALUE 'V19C001'.
           05  CT-PROGRAMA                 PIC X(08)   VALUE 'V12C01J'.
           05  CT-ESTADO-INICIO            PIC X(01)   VALUE 'I'.
           05  CT-ESTADO-CONTINUA          PIC X(01)   VALUE 'C'.
           05  CT-QR4CDB0                  PIC X(07)   VALUE 'QR4CDB0'.
           05  CT-OPEN                     PIC X(04)   VALUE 'OPEN'.
           05  CT-CLOSE                    PIC X(05)   VALUE 'CLOSE'.
           05  CT-FETCH                    PIC X(05)   VALUE 'FETCH'.
           05  CT-SELECT                   PIC X(06)   VALUE 'SELECT'.
           05  CT-TAB-V1DT001              PIC X(08)   VALUE 'V1DT001'.
           05  CT-SI                       PIC X(01)   VALUE 'S'.
      ******************************************************************
      *                      AREA DE CONTADORES                        *
      ******************************************************************
       01  CN-CONTADORES.
           05  CN-REGISTROS                PIC 9(03)   VALUE ZEROES.
           05  IND-PAGINA                  PIC 9(01)   VALUE ZEROES.
      ******************************************************************
      *                AREA DE MENSAJES                                *
      ******************************************************************
       01  ME-MENSAJES-ERROR.
           05  ME-TECLA-INCORRECTA         PIC X(07)   VALUE 'V1E0030'.
      ******************************************************************
      *                        AREA DE INDICES
      ******************************************************************
       01  IN-INDICES.
           05  IN-REGISTRO                 PIC S9(03) COMP
                                   VALUE ZEROES.
      ******************************************************************
      *                    AREA DE INCLUDES                            *
      ******************************************************************
           EXEC SQL
               INCLUDE BKAB0003
           END-EXEC

           EXEC SQL
               INCLUDE V1GT001
           END-EXEC

           EXEC SQL
               INCLUDE V1GT002
           END-EXEC
      ******************************************************************
      *                        CURSORES
      ******************************************************************
           EXEC SQL
               DECLARE V12C01J-V1DC01J1
               CURSOR FOR
               SELECT NUM_DOC_PERSONA,
                      NOM_RAZON,
                      COD_CPT_CMP_VTA,
                      COD_ENT_BANCO,
                      COD_PAIS_CORR,
                      CLA_USO_OPE,
                      COD_OPE_BANCO,
                      NUM_REF,
                      TIP_OPE,
                      TIP_PAGO,
                      IMP_OPE,
                      CTA_PASIVO_CL,
                      TAS_BOLIVAR,
                      TAS_DOLAR,
                      FEC_OPE,
                      COD_DIV_OPE
               FROM V1DT001
               WHERE NUM_DOC_PERSONA     = :DCLV1DT001.NUM-DOC-PERSONA
                     AND FEC_OPE         = :DCLV1DT001.FEC-OPE
                     AND NUM_REF         = :DCLV1DT001.NUM-REF
           END-EXEC
      *
           EXEC SQL
               DECLARE V12C01J-V1DC01J2
               CURSOR FOR
               SELECT COD_USUARIO,
                      COD_OPE_BANCO,
                      COD_DIV_OPE,
                      COD_CPT_CMP_VTA,
                      FEC_OPE,
                      NUM_REF
               FROM V1DT002
               WHERE COD_OPE_BANCO       = :DCLV1DT001.COD-OPE-BANCO
                     AND FEC_OPE         = :DCLV1DT001.FEC-OPE
                     AND COD_DIV_OPE     = :DCLV1DT001.COD-DIV-OPE
                     AND COD_CPT_CMP_VTA = :DCLV1DT001.COD-CPT-CMP-VTA
                     AND NUM_REF         = :DCLV1DT001.NUM-REF
           END-EXEC
      ******************************************************************
      *                    COPYS UTILIZADAS                            *
      ******************************************************************
      **** COPY DFHAID *************************************************
           COPY DFHAID.
           COPY QGECABC.

      *  COPY PARA ERRORES DB2
           COPY QCWCL20.

       01  WS-BGECRET0.
           COPY BGECRET0.

       01 WS-V1WC901.
           COPY V1WC901.
      * COPY PARA ATRIBUTOS DE LA PANTALLA
           COPY QCWCI20.
      *
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA.
           COPY QGECCAA.
       COPY V1NC202.
       COPY V1EC001.
      ******************************************************************
      *                                                                *
      *           P R O C E D U R E      D I V I S I O N               *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION USING DFHCOMMAREA V1NC202I V1EC001.
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
           SET ADDRESS OF V1NC202I          TO CAA-PTR-COPYIN
           SET ADDRESS OF V1EC001           TO CAA-PTRDATA
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
           MOVE CAA-CODTRAN                TO CAA-CODTRAN-SIG
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
           INITIALIZE V1NC202I
      *
           SET CAA-88-ESTADO-CONTIN        TO TRUE
           SET CAA-88-ACCION-TERMINAL      TO TRUE
      *
           MOVE CT-PROGRAMA                TO WS-PROGRAMA
           MOVE '1'                        TO CAA-CASO
      *
           MOVE V153-BPBCRP90         TO NUM-DOC-PERSONA OF DCLV1DT001
           MOVE V153-BPBFIN90         TO FEC-OPE OF DCLV1DT001
           MOVE NUM-REF-V153          TO NUM-REF OF DCLV1DT001
      *
           PERFORM BUSCAR-DATOS
           PERFORM ATT-CAMPOS
      *
           .
       2100-ESTADO-INICIO-EXIT.
           EXIT.
      ******************************************************************
      * BUSCAR-DATOS
      ******************************************************************
       BUSCAR-DATOS.
      *
           PERFORM ABRIR-CURSOR-OPER
              THRU ABRIR-CURSOR-OPER-EXIT

           PERFORM LEER-CURSOR-OPER
              THRU LEER-CURSOR-OPER-EXIT

           PERFORM CERRAR-CURSOR-OPER
              THRU CERRAR-CURSOR-OPER-EXIT
      *
           SET NO-FIN-CURSOR           TO TRUE
           MOVE 1                      TO WS-I

           PERFORM ABRIR-CURSOR-FIR
              THRU ABRIR-CURSOR-FIR-EXIT

           PERFORM LEER-CURSOR-FIR
              THRU LEER-CURSOR-FIR-EXIT
             UNTIL FIN-CURSOR

           PERFORM CERRAR-CURSOR-FIR
              THRU CERRAR-CURSOR-FIR-EXIT
      *
           .
      ******************************************************************
      *                        2200-ESTADO-CONTINUACION                *
      ******************************************************************
       2200-ESTADO-CONTINUACION.
      *
           MOVE CAA-TECLA                  TO SW-TECLA-PRESS
           EVALUATE TRUE
               WHEN PF-INTRO
               WHEN PF-CLEAR
                   SET CAA-88-CODTRAN-SIG-ULTI TO TRUE
                   SET CAA-88-ACCION-PROGRAMA  TO TRUE
                   SET CAA-88-ESTADO-INICIO    TO TRUE

               WHEN OTHER
                   SET CAA-88-ACCION-TERMINAL TO TRUE
                   SET CAA-88-ESTADO-CONTIN   TO TRUE
                   MOVE ME-TECLA-INCORRECTA   TO CAA-COD-ERROR
                   PERFORM 6000-FIN
                      THRU 6000-FIN-EXIT
           END-EVALUATE
      *
           .
       2200-ESTADO-CONTINUACION-EXIT.
           EXIT.
      ******************************************************************
      * ABRIR-CURSOR-OPER
      ******************************************************************
       ABRIR-CURSOR-OPER.
      *
           EXEC SQL
               OPEN V12C01J-V1DC01J1
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
       ABRIR-CURSOR-OPER-EXIT.
           EXIT.
      ******************************************************************
      * LEER-CURSOR-OPER
      ******************************************************************
       LEER-CURSOR-OPER.
      *
           EXEC SQL
               FETCH V12C01J-V1DC01J1
               INTO :DCLV1DT001.NUM-DOC-PERSONA,
                    :DCLV1DT001.NOM-RAZON,
                    :DCLV1DT001.COD-CPT-CMP-VTA,
                    :DCLV1DT001.COD-ENT-BANCO,
                    :DCLV1DT001.COD-PAIS-CORR,
                    :DCLV1DT001.CLA-USO-OPE,
                    :DCLV1DT001.COD-OPE-BANCO,
                    :DCLV1DT001.NUM-REF,
                    :DCLV1DT001.TIP-OPE,
                    :DCLV1DT001.TIP-PAGO,
                    :DCLV1DT001.IMP-OPE,
                    :DCLV1DT001.CTA-PASIVO-CL,
                    :DCLV1DT001.TAS-BOLIVAR,
                    :DCLV1DT001.TAS-DOLAR,
                    :DCLV1DT001.FEC-OPE,
                    :DCLV1DT001.COD-DIV-OPE
           END-EXEC
      *
           MOVE SQLCODE                    TO DB2-RETURN-CDE
           EVALUATE TRUE
               WHEN DB2-OK
                    PERFORM PROCESA-REGISTRO
                       THRU PROCESA-REGISTRO-EXIT

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
       LEER-CURSOR-OPER-EXIT.
           EXIT.
      ******************************************************************
      * PROCESA-REGISTRO
      ******************************************************************
       PROCESA-REGISTRO.
      *
           MOVE NOM-RAZON                  TO WS-NOM-RAZON-BE
           MOVE NUM-DOC-PERSONA            TO WS-NUM-DOC-PERSONA-BE

      *    CALCULAR MONTOS
           COMPUTE WS-MTO-BS = IMP-OPE * TAS-BOLIVAR

           IF  COD-DIV-OPE OF DCLV1DT001 = 1
               MOVE IMP-OPE                TO WS-MTO-DS
           ELSE
               COMPUTE WS-MTO-DS = IMP-OPE * TAS-DOLAR
           END-IF

           INITIALIZE WS-CENTRO WS-DESCRIPCION

      *    OFICINA
           MOVE COD-OPE-BANCO OF DCLV1DT001 TO WS-OFICINA
           IF  WS-UNI-COF89 = WS-OFC-COF89
               MOVE WS-OFC-COF89       TO WS-COD-CENTRO
           ELSE
               MOVE WS-OFICINA         TO WS-COD-CENTRO
           END-IF
           MOVE ZEROS                  TO WS-CENTRO(1:3)
      *
           PERFORM 5450-LLAMAR-V19C001
              THRU 5450-LLAMAR-V19C001-EXIT

      *
           MOVE NUM-DOC-PERSONA            TO BPBC821I
           MOVE NOM-RAZON                  TO BPBN838I
           MOVE WS-NUM-DOC-PERSONA-BE      TO BPB1021I
           MOVE WS-NOM-RAZON-BE            TO BPB1038I
           MOVE COD-CPT-CMP-VTA OF DCLV1DT001
                                           TO MER1121I

      *    BANCO
           IF  COD-ENT-BANCO NOT = ZEROS
               MOVE COD-ENT-BANCO          TO MER1169I
           END-IF

           MOVE FEC-OPE OF DCLV1DT001      TO WS-FEC-OPE
           MOVE WS-DD-FIN89                TO MER1221I
           MOVE WS-MM-FIN89                TO MER1224I
           MOVE WS-AA-FIN89                TO MER1227I

      *    PAIS
           IF  COD-PAIS-CORR NOT = ZEROS
               MOVE COD-PAIS-CORR          TO MER1269I
           END-IF

           MOVE SWIFT-DIV-V153             TO SIG1321I
           MOVE CLA-USO-OPE                TO MER1369I
           MOVE COD-OPE-BANCO OF DCLV1DT001 TO MER1421I
           MOVE WS-DES-OFICINA             TO DES1429I
           MOVE NUM-REF  OF DCLV1DT001     TO MER1521I
           MOVE TIP-OPE                    TO MER1621I
           MOVE TIP-PAGO                   TO MER1721I

           MOVE IMP-OPE                    TO WS-MONTO-EDIT
           MOVE WS-MONTO-EDIT              TO MER1821I

           MOVE V153-CT-LIT-PLURAL         TO CTL1910I

           MOVE WS-MTO-BS                  TO WS-MONTO-EDIT
           MOVE WS-MONTO-EDIT              TO MTO1922I

           MOVE WS-MTO-DS                  TO WS-MONTO-EDIT
           MOVE WS-MONTO-EDIT              TO MTO2021I

      *    CUENTA
           MOVE CTA-PASIVO-CL              TO WS-CUENTA-EDIT
           MOVE WS-CUENTA-EDIT(1:2)        TO WS-CTA1
           MOVE WS-CUENTA-EDIT(3:3)        TO WS-CTA2
           MOVE WS-CUENTA-EDIT(6:7)        TO WS-CTA3
           IF  WS-CTA1 = SPACES
               MOVE SPACES TO WS-SEP1
               IF  WS-CTA2 = SPACES
                   MOVE SPACES TO WS-SEP2
               END-IF
           END-IF
           MOVE WS-CUENTA-MAPA             TO MER2121I
      *
           MOVE SPACES                     TO SIT2149I
      *
           .
       PROCESA-REGISTRO-EXIT.
           EXIT.
      ******************************************************************
      * CERRAR-CURSOR-OPER
      ******************************************************************
       CERRAR-CURSOR-OPER.
      *
           EXEC SQL
               CLOSE V12C01J-V1DC01J1
           END-EXEC
      *
            IF SQLCODE NOT EQUAL ZEROES
               INITIALIZE QGECABC
               MOVE CT-TAB-V1DT001         TO ABC-OBJETO-ERROR
               PERFORM 9999-ABEND-DB2
                  THRU 9999-ABEND-DB2-EXIT
           END-IF
      *
           .
       CERRAR-CURSOR-OPER-EXIT.
           EXIT.
      ******************************************************************
      * ABRIR-CURSOR-FIR
      ******************************************************************
       ABRIR-CURSOR-FIR.
      *
           EXEC SQL
               OPEN V12C01J-V1DC01J2
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
       ABRIR-CURSOR-FIR-EXIT.
           EXIT.
      ******************************************************************
      * LEER-CURSOR-FIR
      ******************************************************************
       LEER-CURSOR-FIR.
      *
           EXEC SQL
               FETCH V12C01J-V1DC01J2
               INTO :DCLV1DT002.COD-USUARIO,
                    :DCLV1DT002.COD-OPE-BANCO,
                    :DCLV1DT002.COD-DIV-OPE,
                    :DCLV1DT002.COD-CPT-CMP-VTA,
                    :DCLV1DT002.FEC-OPE,
                    :DCLV1DT002.NUM-REF
           END-EXEC
      *
           MOVE SQLCODE                    TO DB2-RETURN-CDE
           EVALUATE TRUE
               WHEN DB2-OK
                   EVALUATE WS-I
                       WHEN 1
                           MOVE   COD-USUARIO TO MER1558I
                       WHEN 2
                           MOVE  COD-USUARIO  TO MER1658I
                       WHEN 3
                           MOVE  COD-USUARIO  TO MER1758I
                       WHEN 4
                           MOVE  COD-USUARIO  TO MER1858I
                   END-EVALUATE
                   ADD 1                      TO WS-I
                   IF  WS-I = 5
                       SET FIN-CURSOR         TO TRUE
                   END-IF

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
       LEER-CURSOR-FIR-EXIT.
           EXIT.
      ******************************************************************
      * CERRAR-CURSOR-FIR
      ******************************************************************
       CERRAR-CURSOR-FIR.
      *
           EXEC SQL
               CLOSE V12C01J-V1DC01J2
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
       CERRAR-CURSOR-FIR-EXIT.
           EXIT.
      ******************************************************************
      * 5450-LLAMAR-V19C001
      ******************************************************************
       5450-LLAMAR-V19C001.
      *
           INITIALIZE WS-V1WC901 WS-BGECRET0
           MOVE WS-CENTRO                  TO COD-OFICINA
      *
           CALL CT-V19C001 USING WS-V1WC901 WS-BGECRET0
      *
           EVALUATE TRUE
               WHEN RET0-88-OK
                   MOVE DES-OFICINA        TO WS-DESCRIPCION

               WHEN RET0-88-COD-AVISO
                   MOVE SPACES             TO WS-DESCRIPCION
                   MOVE RET0-COD-ERROR     TO CAA-COD-AVISO1
                   MOVE RET0-VAR1-ERROR    TO CAA-VAR1-AVISO1
                   MOVE RET0-VAR2-ERROR    TO CAA-VAR2-AVISO1

               WHEN RET0-88-COD-ERROR
                  MOVE 'CODIGO OFICINA NO VALIDO' TO DES-OFICINA

               WHEN OTHER
                  INITIALIZE QGECABC
                  MOVE RET0-TABLENAME      TO ABC-OBJETO-ERROR
                  PERFORM 9999-ABEND-DB2
                     THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *
           .
       5450-LLAMAR-V19C001-EXIT.
           EXIT.
      ******************************************************************
      * ATT-CAMPOS
      ******************************************************************
       ATT-CAMPOS.
      *
           MOVE ATRI-PRO-BRI-FST           TO BPBC821A  BPBN838A
                                              BPB1021A  BPB1038A
                                              MER1121A  MER1169A
                                              MER1221A  MER1224A
                                              MER1227A  MER1269A
                                              SIG1321A  MER1369A
                                              MER1421A  DES1429A
                                              MER1521A  MER1558A
                                              MER1621A  MER1658A
                                              MER1721A  MER1758A
                                              MER1821A  MER1858A
                                              MER1821A  MER1858A
                                              MER1821A
                                              MTO1922A  MTO2021A
                                              MER2121A  SIT2149A
           MOVE ATRI-PRO-FST               TO CTL1910A

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
           MOVE WS-PROGRAMA                TO ABC-PROGRAMA
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
