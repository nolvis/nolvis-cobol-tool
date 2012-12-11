      ******************************************************************
      *                                                                *
      *  PROGRAMA: V12C20J                                             *
      *                                                                *
      *  FECHA CREACION: 20/06/2008                                    *
      *                                                                *
      *  AUTOR: FACTORIA                                               *
      *                                                                *
      *  APLICACION: MERCADO LIBRE DE DIVISAS. (V1) VENEZUELA.         *
      *                                                                *
      *  DESCRIPCION: PROGRAMA QUE MUESTRA EL DETALLE DE OPERACIONES   *
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
       PROGRAM-ID.   V12C20J.
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
           05 WS-PAIS-ORI                       PIC X(25).
           05 WS-PAIS-ORI-I                     PIC X(25).
           05 WS-PARAISO-FISCAL                 PIC X(02).
           05 WS-I                              PIC 99 VALUE ZEROS.
           05 WS-VAREDIT15         PIC 9(13)V99 VALUE ZEROS.
           05 WS-VARNUM15          PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05 WS-VARALF15 REDEFINES WS-VARNUM15 PIC X(20).
      *
           05 WS-FEC-OPE                  PIC 9(08).
           05 FILLER REDEFINES WS-FEC-OPE.
              10 WS-AA-FIN89               PIC 9(4).
              10 WS-MM-FIN89               PIC 9(2).
              10 WS-DD-FIN89               PIC 9(2).
      *
           05 WS-BPBFIA89   OCCURS 4 TIMES PIC X(09).

           05  WS-COD-OPE-BANCO                  PIC 9(06).
           05  FILLER REDEFINES WS-COD-OPE-BANCO.
               10  WS-UNI-COF89            PIC 9(3).
               10  WS-OFC-COF89            PIC 9(3).
      *
           05  WS-CENTRO                   PIC 9(09).
           05  FILLER REDEFINES WS-CENTRO.
               10  FILL1                   PIC 9(3).
               10  WS-COD-CTRO             PIC 9(6).
      *
           05  WS-CUENTA-EDIT              PIC Z(12).
           05  WS-CUENTA-MAPA.
               10  WS-CTA1                 PIC X(02).
               10  WS-SEP1                 PIC X(01) VALUE '-'.
               10  WS-CTA2                 PIC X(03).
               10  WS-SEP2                 PIC X(01) VALUE '-'.
               10  WS-CTA3                 PIC X(07).
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

           05  SW-BPBB0089                 PIC X(01)   VALUE 'N'.
               88  EXISTE-BPBB0089                     VALUE 'S'.
               88  NO-EXISTE-BPBB0089                  VALUE 'N'.
      ******************************************************************
      *                        AREA DE CONTANTES                       *
      ******************************************************************
       01  CT-CONTANTES.
      *                                                                *
           05  CT-PROGRAMA                 PIC X(08)   VALUE 'V12C20J'.
           05  CT-QG1CABC                  PIC X(07)   VALUE 'QG1CABC'.
           05  CT-TAB-V1DT001              PIC X(07)   VALUE 'V1DT001'.
           05  CT-V19C001                  PIC X(07)   VALUE 'V19C001'.
           05  CT-VQ9C006                  PIC X(07)   VALUE 'VQ9C006'.
           05  CT-VJ8CDIV0                 PIC X(08)   VALUE 'VJ8CDIV0'.
           05  CT-1                        PIC 9(01)   VALUE 1.
           05  CT-ESTADO-INICIO            PIC X(01)   VALUE 'I'.
           05  CT-ESTADO-CONTINUA          PIC X(01)   VALUE 'C'.
           05  CT-SI                       PIC X(01)   VALUE 'S'.
           05  CT-X                        PIC X(01)   VALUE 'X'.
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
           05  CN-CONTADOR-OPE             PIC 9(09)   VALUE ZEROS.
           05  CN-SELECCION                PIC 9(01)   VALUE ZEROS.
      ******************************************************************
      *                    COPYS UTILIZADAS                            *
      ******************************************************************
      ****  COPY PARA ERRORES DB2
           COPY QRECDB2.
           COPY QCWCL20.
      *
       01  WS-VQWC006-01.
       COPY VQWC006.
      *
       COPY VJECDIV0.
      *
       01  WS-V1WC901-01.
       COPY V1WC901.
      *  COPY PARA EL CONTROL DE RETORNOS DE RUTINAS
       01  WS-BGECRET0-01.
       COPY BGECRET0.
      *  COPY PARA EL PROGRAMA DE ABEND
       01  WS-QGECABC-01.
           COPY QGECABC.
      *  COPY DFHAID
           COPY DFHAID.
      *  ATRIBUTOS DE CAMPOS
           COPY QCWCI20.
      ******************************************************************
      *  INCLUDE DE LA TABLA V1DT001                                   *
      ******************************************************************
           EXEC SQL
               INCLUDE V1GT001
           END-EXEC.
      *
           EXEC SQL
               INCLUDE V1GT002
           END-EXEC.
      ******************************************************************
      *  CURSORES
      ******************************************************************
           EXEC SQL
               DECLARE CUR-V1GT002
               CURSOR FOR
               SELECT COD_USUARIO
               FROM V1DT002
               WHERE
                  COD_OPE_BANCO       = :DCLV1DT002.COD-OPE-BANCO
                  AND FEC_OPE         = :DCLV1DT002.FEC-OPE
                  AND COD_DIV_OPE     = :DCLV1DT002.COD-DIV-OPE
                  AND COD_CPT_CMP_VTA = :DCLV1DT002.COD-CPT-CMP-VTA
                  AND NUM_REF         = :DCLV1DT002.NUM-REF
           END-EXEC.
      *
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA.
           COPY QGECCAA.
       COPY V1NC022.
       COPY V1EC001.
      ******************************************************************
      *                                                                *
      *           P R O C E D U R E      D I V I S I O N               *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION USING DFHCOMMAREA V1NC022I V1EC001.
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
           SET ADDRESS OF V1NC022I         TO CAA-PTR-COPYIN
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
           INITIALIZE V1NC022I
           SET CAA-88-ESTADO-CONTIN        TO TRUE
           SET CAA-88-ACCION-TERMINAL      TO TRUE
      *
           PERFORM BUSCAR-DATOS
              THRU BUSCAR-DATOS-EXIT
           PERFORM ATT-CAMPOS
      *
           .
       2100-ESTADO-INICIO-EXIT.
           EXIT.
      ******************************************************************
      *                  3300-ESTADO-CONTINUACION                      *
      ******************************************************************
       2200-ESTADO-CONTINUACION.
      *
           EVALUATE CAA-TECLA
               WHEN '00'
               WHEN '99'
                   SET CAA-88-CODTRAN-SIG-ULTI TO TRUE
                   SET CAA-88-ACCION-PROGRAMA  TO TRUE
                   SET CAA-88-ESTADO-INICIO    TO TRUE

               WHEN OTHER
                   SET CAA-88-ACCION-TERMINAL          TO TRUE
                   SET CAA-88-ESTADO-CONTIN            TO TRUE

                   PERFORM ATT-CAMPOS

                   PERFORM 6000-FIN
                      THRU 6000-FIN-EXIT
           END-EVALUATE
      *
           .
       2200-ESTADO-CONTINUACION-EXIT.
           EXIT.
      ******************************************************************
      * BUSCAR-DATOS
      ******************************************************************
       BUSCAR-DATOS.
      *
           MOVE COD-OPE-BANCO-V163   TO COD-OPE-BANCO OF DCLV1DT001
           MOVE FEC-OPE-V163(1:2)    TO FEC-OPE OF DCLV1DT001(7:2)
           MOVE FEC-OPE-V163(3:2)    TO FEC-OPE OF DCLV1DT001(5:2)
           MOVE FEC-OPE-V163(5:4)    TO FEC-OPE OF DCLV1DT001(1:4)
           MOVE COD-DIV-OPE-V163     TO COD-DIV-OPE OF DCLV1DT001
           MOVE COD-CPT-CMP-VTA-V163 TO COD-CPT-CMP-VTA OF DCLV1DT001
           MOVE NUM-REF-V163         TO NUM-REF OF DCLV1DT001
      *
           EXEC SQL
               SELECT COD_ENT_BANCO,
                      COD_CPT_CMP_VTA,
                      COD_DIV_OPE,
                      CLA_USO_OPE,
                      COD_OPE_BANCO,
                      COD_PAIS_CORR,
                      IDF_RIF_BENEF,
                      NUM_DOC_PERSONA,
                      CTA_PASIVO_CL,
                      TIP_PAGO,
                      IMP_OPE,
                      NOM_BENEF,
                      NOM_RAZON,
                      COD_PAIS_DEST,
                      NUM_REF,
                      TIP_OPE,
                      FEC_OPE,
                      TAS_BOLIVAR,
                      TAS_DOLAR
                INTO :DCLV1DT001.COD-ENT-BANCO,
                     :DCLV1DT001.COD-CPT-CMP-VTA,
                     :DCLV1DT001.COD-DIV-OPE,
                     :DCLV1DT001.CLA-USO-OPE,
                     :DCLV1DT001.COD-OPE-BANCO,
                     :DCLV1DT001.COD-PAIS-CORR,
                     :DCLV1DT001.IDF-RIF-BENEF,
                     :DCLV1DT001.NUM-DOC-PERSONA,
                     :DCLV1DT001.CTA-PASIVO-CL,
                     :DCLV1DT001.TIP-PAGO,
                     :DCLV1DT001.IMP-OPE,
                     :DCLV1DT001.NOM-BENEF,
                     :DCLV1DT001.NOM-RAZON,
                     :DCLV1DT001.COD-PAIS-DEST,
                     :DCLV1DT001.NUM-REF,
                     :DCLV1DT001.TIP-OPE,
                     :DCLV1DT001.FEC-OPE,
                     :DCLV1DT001.TAS-BOLIVAR,
                     :DCLV1DT001.TAS-DOLAR
               FROM V1DT001
               WHERE
                 COD_OPE_BANCO        = :DCLV1DT001.COD-OPE-BANCO
                 AND FEC_OPE          = :DCLV1DT001.FEC-OPE
                 AND COD_DIV_OPE      = :DCLV1DT001.COD-DIV-OPE
                 AND COD_CPT_CMP_VTA  = :DCLV1DT001.COD-CPT-CMP-VTA
                 AND NUM_REF          = :DCLV1DT001.NUM-REF
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN ZEROS
               WHEN -811
                   PERFORM 2500-PROCESAR-REGISTRO
                      THRU 2500-PROCESAR-REGISTRO-EXIT

               WHEN +100
                   SET NO-EXISTE-BPBB0089  TO TRUE

               WHEN OTHER
                   INITIALIZE QGECABC
                   MOVE 'V1DT001'          TO ABC-OBJETO-ERROR
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *
           .
       BUSCAR-DATOS-EXIT.
           EXIT.
      ******************************************************************
      * 2500-PROCESAR-REGISTRO
      ******************************************************************
       2500-PROCESAR-REGISTRO.
      *
           PERFORM BUSCAR-OFICINA
              THRU BUSCAR-OFICINA-EXIT
      *
           PERFORM 2600-BUSCA-PARAISO-FISCAL
              THRU 2600-BUSCA-PARAISO-FISCAL-EXIT
      *
           PERFORM 2400-LEER-TABLA-HIJA
              THRU 2400-LEER-TABLA-HIJA-EXIT
      *
           PERFORM 7000-MOVER-AL-MAPA
              THRU 7000-MOVER-AL-MAPA-EXIT
      *
           .
       2500-PROCESAR-REGISTRO-EXIT.
           EXIT.
      ******************************************************************
      * 2400-LEER-TABLA-HIJA
      ******************************************************************
       2400-LEER-TABLA-HIJA.
           SET NO-FIN-CURSOR               TO TRUE
           MOVE ZEROS                      TO WS-I
      *
           PERFORM 5500-ABRIR-CURSOR
              THRU 5500-ABRIR-CURSOR-EXIT
      *
           PERFORM 5600-LEER-CURSOR
              THRU 5600-LEER-CURSOR-EXIT
             UNTIL FIN-CURSOR OR WS-I > 4
      *
           PERFORM 5700-CERRAR-CURSOR
              THRU 5700-CERRAR-CURSOR-EXIT
      *
           .
       2400-LEER-TABLA-HIJA-EXIT.
           EXIT.
      ******************************************************************
      * 2600-BUSCA-PARAISO-FISCAL
      ******************************************************************
       2600-BUSCA-PARAISO-FISCAL.
      *
           INITIALIZE VQWC006
           MOVE COD-PAIS-CORR              TO WC006-COD-PAIS

           PERFORM 8100-LLAMAR-VQ9C006
              THRU 8100-LLAMAR-VQ9C006-EXIT

           MOVE WC006-DESC-PAIS            TO WS-PAIS-ORI
      *
           INITIALIZE VQWC006
           MOVE COD-PAIS-DEST              TO WC006-COD-PAIS

           PERFORM 8100-LLAMAR-VQ9C006
              THRU 8100-LLAMAR-VQ9C006-EXIT

           MOVE WC006-DESC-PAIS            TO WS-PAIS-ORI-I
           MOVE WC006-IN-PAIS              TO WS-PARAISO-FISCAL
      *
           .
       2600-BUSCA-PARAISO-FISCAL-EXIT.
           EXIT.
      ******************************************************************
      * 5500-ABRIR-CURSOR
      ******************************************************************
       5500-ABRIR-CURSOR.
      *
           MOVE COD-OPE-BANCO OF DCLV1DT001
                                     TO COD-OPE-BANCO OF DCLV1DT002
           MOVE FEC-OPE OF DCLV1DT001
                                     TO FEC-OPE OF DCLV1DT002
           MOVE COD-DIV-OPE OF DCLV1DT001
                                     TO COD-DIV-OPE OF DCLV1DT002
           MOVE COD-CPT-CMP-VTA OF DCLV1DT001
                                     TO COD-CPT-CMP-VTA OF DCLV1DT002
           MOVE NUM-REF OF DCLV1DT001
                                     TO NUM-REF OF DCLV1DT002
      *
           EXEC SQL
               OPEN CUR-V1GT002
           END-EXEC
      *
           MOVE SQLCODE               TO DB2-RETURN-CDE
      *
           IF  NOT DB2-OK
               INITIALIZE QGECABC
               MOVE 'V1GT002'         TO ABC-OBJETO-ERROR
               PERFORM 9999-ABEND-DB2
                  THRU 9999-ABEND-DB2-EXIT
           END-IF
      *
           .
       5500-ABRIR-CURSOR-EXIT.
           EXIT.
      ******************************************************************
      * 5600-LEER-CURSOR
      ******************************************************************
       5600-LEER-CURSOR.
      *
           EXEC SQL
               FETCH CUR-V1GT002
               INTO :COD-USUARIO
           END-EXEC
      *
           MOVE SQLCODE                    TO DB2-RETURN-CDE
           EVALUATE TRUE
               WHEN DB2-OK
                   ADD 1                   TO WS-I
                   MOVE COD-USUARIO        TO WS-BPBFIA89(WS-I)

               WHEN DB2-NOTFND
                   SET FIN-CURSOR          TO TRUE

               WHEN OTHER
                   INITIALIZE QGECABC
                   MOVE 'V1DT002'          TO ABC-OBJETO-ERROR
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *
           .
       5600-LEER-CURSOR-EXIT.
           EXIT.
      ******************************************************************
      * 5700-CERRAR-CURSOR
      ******************************************************************
       5700-CERRAR-CURSOR.
      *
           EXEC SQL
               CLOSE CUR-V1GT002
           END-EXEC
      *
           MOVE SQLCODE                    TO DB2-RETURN-CDE
      *
           IF  NOT DB2-OK
               INITIALIZE QGECABC
               MOVE 'V1GT002'              TO ABC-OBJETO-ERROR
               PERFORM 9999-ABEND-DB2
                  THRU 9999-ABEND-DB2-EXIT
           END-IF
      *
           .
       5700-CERRAR-CURSOR-EXIT.
           EXIT.
      ******************************************************************
      * 7000-MOVER-AL-MAPA
      ******************************************************************
       7000-MOVER-AL-MAPA.
      *
           MOVE FEC-OPE OF DCLV1DT001         TO WS-FEC-OPE
      *
           MOVE NUM-DOC-PERSONA               TO MERC721I
           MOVE NOM-RAZON                     TO MERC738I
           MOVE IDF-RIF-BENEF                 TO MERC921I
           MOVE NOM-BENEF                     TO MERC938I
           MOVE COD-CPT-CMP-VTA OF DCLV1DT001 TO MER1021I

      *    BANCO
           IF  COD-ENT-BANCO OF DCLV1DT001 = ZEROS
               MOVE SPACES                    TO MER1064I
           ELSE
               MOVE COD-ENT-BANCO OF DCLV1DT001 TO MER1064I
           END-IF

           MOVE WS-DD-FIN89                   TO MER1121I
           MOVE WS-MM-FIN89                   TO MER1124I
           MOVE WS-AA-FIN89                   TO MER1127I
           MOVE COD-DIV-OPE    OF DCLV1DT001  TO MER1144I
           MOVE CLA-USO-OPE    OF DCLV1DT001  TO MER1164I
           MOVE COD-OPE-BANCO  OF DCLV1DT001  TO MER1221I
           MOVE DES-OFICINA                   TO DES1230I
           MOVE NUM-REF        OF DCLV1DT001  TO MER1321I

      *    PAIS
           IF  COD-PAIS-CORR = ZEROS OR SPACES
               MOVE SPACES                    TO MER1425I
                                                 PAI1431I
           ELSE
               MOVE COD-PAIS-CORR             TO MER1425I
               MOVE WS-PAIS-ORI               TO PAI1431I
           END-IF

      *    PAIS
           IF  COD-PAIS-DEST EQUAL ZEROS OR SPACES
               MOVE SPACES                    TO MER1533I
                                                 PAI1539I
                                                 PAR1578I
           ELSE
               MOVE COD-PAIS-DEST             TO MER1533I
               MOVE WS-PAIS-ORI-I             TO PAI1539I
               MOVE WS-PARAISO-FISCAL         TO PAR1578I
           END-IF

           MOVE WC006-IN-PAIS                 TO PAR1578I
           MOVE TIP-OPE                       TO MER1621I
           MOVE TIP-PAGO                      TO MER1721I
           MOVE WS-BPBFIA89(1)                TO MER1758I
           MOVE IMP-OPE                       TO WS-VARNUM15
           MOVE WS-VARALF15                   TO MER1821I
           MOVE WS-BPBFIA89(2)                TO MER1858I
      *
           CALL CT-VJ8CDIV0 USING VJECDIV0
           MOVE VJE-LIT-PLURAL(1:9)        TO CTL1910I

      *    CALCULO DEL MTO-BS
           COMPUTE WS-VAREDIT15 = IMP-OPE * TAS-BOLIVAR
           MOVE WS-VAREDIT15               TO WS-VARNUM15
           MOVE WS-VARALF15                TO MTO1922I

      *    CALCULO DEL MTO-DS
           IF  COD-DIV-OPE OF DCLV1DT001  = 1
               COMPUTE WS-VAREDIT15 = IMP-OPE
           ELSE
               COMPUTE WS-VAREDIT15 = IMP-OPE * TAS-DOLAR
           END-IF
           MOVE WS-VAREDIT15               TO WS-VARNUM15
           MOVE WS-VARALF15                TO MTO2021I
      *
           MOVE WS-BPBFIA89(3)             TO MER1958I
           MOVE WS-BPBFIA89(4)             TO MER2058I

      *    CUENTA
           MOVE CTA-PASIVO-CL           TO WS-CUENTA-EDIT
           MOVE WS-CUENTA-EDIT(1:2)     TO WS-CTA1
           MOVE WS-CUENTA-EDIT(3:3)     TO WS-CTA2
           MOVE WS-CUENTA-EDIT(6:7)     TO WS-CTA3
           IF  WS-CTA1 = SPACES
               MOVE SPACES TO WS-SEP1
               IF  WS-CTA2 = SPACES
                   MOVE SPACES TO WS-SEP2
               END-IF
           END-IF
           MOVE WS-CUENTA-MAPA          TO MER2121I

      *
           MOVE SPACES                     TO SIT2149I
      *
           .
       7000-MOVER-AL-MAPA-EXIT.
           EXIT.
      ******************************************************************
      * BUSCAR-OFICINA
      ******************************************************************
       BUSCAR-OFICINA.
      *
           INITIALIZE WS-V1WC901-01 WS-BGECRET0-01
      *
           MOVE COD-OPE-BANCO OF DCLV1DT001     TO WS-COD-OPE-BANCO
           IF  WS-UNI-COF89 = WS-OFC-COF89
               MOVE WS-OFC-COF89                TO WS-COD-CTRO
           ELSE
               MOVE COD-OPE-BANCO OF DCLV1DT001 TO WS-COD-CTRO
           END-IF
           MOVE ZEROS                           TO FILL1
           MOVE WS-CENTRO                       TO COD-OFICINA
      *
           CALL CT-V19C001 USING WS-V1WC901-01 WS-BGECRET0-01
      *
           EVALUATE TRUE
               WHEN RET0-88-OK
                   CONTINUE

               WHEN RET0-88-COD-AVISO
                   MOVE SPACES               TO DES-OFICINA
                   MOVE RET0-COD-ERROR       TO CAA-COD-AVISO1
                   MOVE RET0-VAR1-ERROR      TO CAA-VAR1-AVISO1
                   MOVE RET0-VAR2-ERROR      TO CAA-VAR2-AVISO1

               WHEN RET0-88-COD-ERROR
                   MOVE 'CODIGO OFICINA NO VALIDO' TO DES-OFICINA

               WHEN OTHER
                   MOVE RET0-TABLENAME     TO DB2-OBJETO
                   MOVE RET0-SQLCA         TO DB2-SQLCA
                   MOVE RET0-PROGRAMA      TO DB2-PGRNAME
                   PERFORM 9999-ABEND-DB2
           END-EVALUATE
      *
           .
       BUSCAR-OFICINA-EXIT.
           EXIT.
      ******************************************************************
      * 8100-LLAMAR-VQ9C006
      ******************************************************************
       8100-LLAMAR-VQ9C006.
      *
           CALL CT-VQ9C006 USING WS-VQWC006-01 WS-BGECRET0-01
      *
           EVALUATE TRUE
             WHEN RET0-88-OK
                CONTINUE

             WHEN RET0-88-COD-AVISO
                MOVE RET0-COD-ERROR   TO CAA-COD-AVISO1
                MOVE RET0-VAR1-ERROR  TO CAA-VAR1-AVISO1
                MOVE RET0-VAR2-ERROR  TO CAA-VAR2-AVISO1

             WHEN RET0-88-COD-ERROR
                MOVE 'CODIGO PAIS NO VALIDO' TO WC006-DESC-PAIS

             WHEN OTHER
                MOVE RET0-TABLENAME    TO DB2-OBJETO
                MOVE RET0-SQLCA        TO DB2-SQLCA
                MOVE RET0-PROGRAMA     TO DB2-PGRNAME
                PERFORM 9999-ABEND-DB2
           END-EVALUATE
      *
           .
       8100-LLAMAR-VQ9C006-EXIT.
           EXIT.
      ******************************************************************
      * ATT-CAMPOS
      ******************************************************************
       ATT-CAMPOS.
      *
           MOVE ATRI-PRO-BRI-FST TO
                       MERC721A MERC738A
                       MERC921A MERC938A
                       MER1021A MER1064A
                       MER1121A MER1124A MER1127A MER1144A MER1164A
                       MER1221A DES1230A
                       MER1321A
                       MER1425A PAI1431A
                       MER1533A PAI1539A PAR1578A
                       MER1621A
                       MER1721A
                       MER1821A
                       MTO1922A
                       MTO2021A
                       MER2121A
                       MER1758A MER1858A
                       MER1958A MER2058A
                       SIT2149A
           MOVE ATRI-PRO-FST TO CTL1910A
      *
           .
      ******************************************************************
      * 6000-FIN
      ******************************************************************
       6000-FIN.
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
      *
