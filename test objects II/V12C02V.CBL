      ******************************************************************
      *                                                                *
      *  PROGRAMA: V12C02V                                             *
      *                                                                *
      *  FECHA CREACION: 26/06/2008                                    *
      *                                                                *
      *  AUTOR: FACTORIA                                               *
      *                                                                *
      *  APLICACION: V1                                                *
      *                                                                *
      *  DESCRIPCION: CONSULTA DE UNA OPERACION QUE NO VIOLA LOS       *
      *               PARAMETROS                                       *
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
       PROGRAM-ID.   V12C02V.
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
           05  CT-PROGRAMA                 PIC X(07)   VALUE 'V12C02V'.
           05  CT-QG1CABC                  PIC X(07)   VALUE 'QG1CABC'.
           05  CT-SI                       PIC X(01)   VALUE 'S'.
           05  CT-V19C001                  PIC X(08)   VALUE 'V19C001'.
      ******************************************************************
      *                AREA DE SWITCHES                                *
      ******************************************************************
       01  SW-SWITCHES.
      *
           05  SW-FIN-CURSOR               PIC X(01)   VALUE 'N'.
               88  FIN-CURSOR                          VALUE 'S'.
               88  NO-FIN-CURSOR                       VALUE 'N'.
      ******************************************************************
      *                AREA DE VARIABLES AUXILIARES                    *
      ******************************************************************
       01  WS-VARIABLES-AUXILIARES.
           05  WS-PROGRAMA                 PIC X(08)   VALUE SPACES.
           05  WS-TABLA                    PIC X(08)   VALUE SPACES.
           05  WS-MONTO-EDIT               PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.99.
           05  WS-MTO-BS                   PIC S9(13)V9(2) COMP-3.
           05  WS-MTO-DS                   PIC S9(13)V9(2) COMP-3.
           05  WS-CENTRO                   PIC 9(09).
           05  WS-CENTRO-RE REDEFINES WS-CENTRO.
               10  FILLER                  PIC 9(03).
               10  WS-COD-CENTRO           PIC 9(06).
               10  WS-COD-CENTRO-RE REDEFINES WS-COD-CENTRO.
                   15  WS-UNI-COF105       PIC 9(03).
                   15  WS-OFC-COF105       PIC 9(03).
           05  WS-DESCRIPCION              PIC X(40).
           05  WS-DESCRIPCION-RE REDEFINES WS-DESCRIPCION.
               10  WS-DES-OFICINA          PIC X(25).
      *
           05  WS-COD-OPE-BANCO            PIC 9(06).
           05  WS-COD-OPE-BANCO-RE REDEFINES WS-COD-OPE-BANCO.
               10  WS-UNI-COF89            PIC 9(03).
               10  WS-OFC-COF89            PIC 9(03).
           05  WS-PAIS                     PIC 9(04).
           05  WS-I                        PIC 9(05).
           05  WS-CUENTA-EDIT              PIC Z(12).
           05  WS-CUENTA-MAPA.
               10  WS-CTA1                 PIC X(02).
               10  WS-SEP1                 PIC X(01) VALUE '-'.
               10  WS-CTA2                 PIC X(03).
               10  WS-SEP2                 PIC X(01) VALUE '-'.
               10  WS-CTA3                 PIC X(07).
      ******************************************************************
      *                    COPYS UTILIZADAS                            *
      ******************************************************************
      *--- COPY DFHAID
           COPY DFHAID.
      *--- ATRIBUTOS DE LA PANTALLA
           COPY QCWCI20.
      *--- COPY DE LA RUTINA QG1CABC (ABENDS)
           COPY QGECABC.
      *--- COPY RUTINA V19C001
       01  WS-V1WC901-01.
       COPY V1WC901.
      *--- COPY CODIGOS DE RETORNO
       01  WS-BGECRET0-01.
       COPY BGECRET0.
      *--- COPY QCWCL20
       COPY QCWCL20.
      *--- TABLA V1DT001
           EXEC SQL
               INCLUDE V1GT001
           END-EXEC.
      *--- TABLA V1DT002
           EXEC SQL
               INCLUDE V1GT002
           END-EXEC.
      ******************************************************************
      * CURSORES                                                       *
      ******************************************************************
           EXEC SQL
               DECLARE V12C02V-V1DC0021
               CURSOR FOR
               SELECT COD_USUARIO,
                      COD_NIV_AUTORIZ
               FROM V1DT002
               WHERE COD_OPE_BANCO   = :DCLV1DT002.COD-OPE-BANCO AND
                     FEC_OPE         = :DCLV1DT002.FEC-OPE AND
                     COD_DIV_OPE     = :DCLV1DT002.COD-DIV-OPE AND
                     COD_CPT_CMP_VTA = :DCLV1DT002.COD-CPT-CMP-VTA AND
                     NUM_REF         = :DCLV1DT002.NUM-REF
               ORDER BY COD_NIV_AUTORIZ
           END-EXEC
      *
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA.
           COPY QGECCAA.
       COPY V1EC001.
       COPY V1NC009.
      ******************************************************************
      *                                                                *
      *           P R O C E D U R E      D I V I S I O N               *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION USING DFHCOMMAREA V1EC001 V1NC009I.
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
           SET ADDRESS OF V1NC009I         TO CAA-PTR-COPYIN
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
           INITIALIZE V1NC009I
           SET CAA-88-ESTADO-CONTIN        TO TRUE
           SET CAA-88-ACCION-TERMINAL      TO TRUE
           MOVE CAA-CODTRAN                TO COD-TRANSACCION
      *
           PERFORM ATRIBUTOS-MAPA
              THRU ATRIBUTOS-MAPA-EXIT

           PERFORM BUSCAR-DATOS
              THRU BUSCAR-DATOS-EXIT

           PERFORM MOVER-DATOS
              THRU MOVER-DATOS-EXIT
      *
           .
       2100-ESTADO-INICIO-EXIT.
           EXIT.
      ******************************************************************
      * BUSCAR-DATOS
      ******************************************************************
       BUSCAR-DATOS.
      *
           MOVE V180-BPBCRP90        TO NUM-DOC-PERSONA OF DCLV1DT001
           MOVE V159-FEC-OPE         TO FEC-OPE OF DCLV1DT001
           MOVE V159-NUM-REF         TO NUM-REF OF DCLV1DT001
      *
           EXEC SQL
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
                      COD_ENT_BANCO,
                      CLA_USO_OPE,
                      NOM_BENEF,
                      COD_PAIS_CORR,
                      CTA_PASIVO_CL,
                      IDF_RIF_BENEF,
                      IDF_SEGURID
               INTO   :DCLV1DT001.COD-OPE-BANCO,
                      :DCLV1DT001.FEC-OPE,
                      :DCLV1DT001.COD-DIV-OPE,
                      :DCLV1DT001.COD-CPT-CMP-VTA,
                      :DCLV1DT001.NUM-REF,
                      :DCLV1DT001.NUM-DOC-PERSONA,
                      :DCLV1DT001.TIP-OPE,
                      :DCLV1DT001.TIP-PAGO,
                      :DCLV1DT001.NOM-RAZON,
                      :DCLV1DT001.IMP-OPE,
                      :DCLV1DT001.TAS-BOLIVAR,
                      :DCLV1DT001.TAS-DOLAR,
                      :DCLV1DT001.COD-ENT-BANCO,
                      :DCLV1DT001.CLA-USO-OPE,
                      :DCLV1DT001.NOM-BENEF,
                      :DCLV1DT001.COD-PAIS-CORR,
                      :DCLV1DT001.CTA-PASIVO-CL,
                      :DCLV1DT001.IDF-RIF-BENEF,
                      :DCLV1DT001.IDF-SEGURID
               FROM V1DT001
               WHERE NUM_DOC_PERSONA = :DCLV1DT001.NUM-DOC-PERSONA AND
                     FEC_OPE = :DCLV1DT001.FEC-OPE AND
                     NUM_REF = :DCLV1DT001.NUM-REF
           END-EXEC
      *
           IF  SQLCODE NOT EQUAL ZEROS
               MOVE 'V1DT001'              TO WS-TABLA
               PERFORM 9999-ABEND-DB2
                  THRU 9999-ABEND-DB2-EXIT
           END-IF
      *
           .
       BUSCAR-DATOS-EXIT.
           EXIT.
      ******************************************************************
      * MOVER-DATOS                                                    *
      ******************************************************************
       MOVER-DATOS.
      *
           IF  COD-OPE-BANCO OF DCLV1DT001 = 010297
               MOVE NOM-RAZON       TO NOMBENI
               MOVE NUM-DOC-PERSONA TO CRPBENI
           ELSE
               MOVE NOM-BENEF       TO NOMBENI
               MOVE IDF-RIF-BENEF   TO CRPBENI
           END-IF

      *    MONTOS
           COMPUTE WS-MTO-BS = IMP-OPE * TAS-BOLIVAR
           MOVE WS-MTO-BS     TO WS-MONTO-EDIT
           MOVE WS-MONTO-EDIT TO MTOBSI

           IF  COD-DIV-OPE OF DCLV1DT001 = 1
               MOVE IMP-OPE   TO WS-MTO-DS
           ELSE
               COMPUTE WS-MTO-DS = IMP-OPE * TAS-DOLAR
           END-IF
           MOVE WS-MTO-DS     TO WS-MONTO-EDIT
           MOVE WS-MONTO-EDIT TO MTODSI

      *
           INITIALIZE WS-CENTRO WS-DESCRIPCION NOTAI

      *    OFICINA
           MOVE COD-OPE-BANCO OF DCLV1DT001 TO WS-COD-OPE-BANCO
           IF  WS-UNI-COF89 = WS-OFC-COF89
               MOVE WS-OFC-COF89     TO WS-COD-CENTRO
           ELSE
               MOVE WS-COD-OPE-BANCO TO WS-COD-CENTRO
           END-IF
           MOVE ZEROS                TO WS-CENTRO(1:3)

           PERFORM BUSCAR-DES-CENTRO
              THRU BUSCAR-DES-CENTRO-EXIT
           MOVE WS-DES-OFICINA  TO DESOFCI

      *
           MOVE NUM-DOC-PERSONA TO CRPI
           MOVE NOM-RAZON       TO NOMBREI
           MOVE COD-CPT-CMP-VTA OF DCLV1DT001 TO CONI

      *    BANCO
           IF  COD-ENT-BANCO NOT = ZEROS
               MOVE COD-ENT-BANCO   TO BANCOI
           END-IF

           MOVE FEC-OPE OF DCLV1DT001(1:4) TO FOAAI
           MOVE FEC-OPE OF DCLV1DT001(5:2) TO FOMMI
           MOVE FEC-OPE OF DCLV1DT001(7:2) TO FODDI

      *    PAIS
           IF  COD-PAIS-CORR NOT = ZEROS
               MOVE COD-PAIS-CORR   TO WS-PAIS
               MOVE WS-PAIS         TO PAISI
           END-IF

           MOVE V159-SIGLA-CDI  TO DIVI
           MOVE CLA-USO-OPE     TO OPERADI
           MOVE COD-OPE-BANCO OF DCLV1DT001 TO OFCI
           MOVE NUM-REF OF DCLV1DT001       TO REFERI
           MOVE TIP-OPE         TO TIPOPEI
           MOVE TIP-PAGO        TO FORPAGI
           MOVE IMP-OPE         TO WS-MONTO-EDIT
           MOVE WS-MONTO-EDIT   TO MTODIVI
           MOVE V157-LIT-PLURAL TO LITERALI

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
           MOVE WS-CUENTA-MAPA          TO NUMCTAI

      *
           PERFORM BUSCAR-FIRMAS
              THRU BUSCAR-FIRMAS-EXIT
      *
           .
      *
       MOVER-DATOS-EXIT.
           EXIT.
      ******************************************************************
      * BUSCAR-DES-CENTRO                                              *
      ******************************************************************
       BUSCAR-DES-CENTRO.
      *
           INITIALIZE WS-V1WC901-01 WS-BGECRET0-01
           MOVE WS-CENTRO TO COD-OFICINA
      *
           CALL CT-V19C001 USING WS-V1WC901-01 WS-BGECRET0-01
      *
           EVALUATE TRUE
               WHEN RET0-88-OK
                   MOVE DES-OFICINA        TO WS-DESCRIPCION

               WHEN RET0-88-COD-AVISO
                   MOVE SPACES             TO WS-DESCRIPCION
                   MOVE RET0-COD-AVISO1    TO CAA-COD-AVISO1
                   MOVE RET0-VAR1-AVISO1   TO CAA-VAR1-AVISO1
                   MOVE RET0-VAR2-AVISO1   TO CAA-VAR2-AVISO1

               WHEN RET0-88-COD-ERROR
                   MOVE 'CODIGO OFICINA NO VALIDO' TO WS-DESCRIPCION

               WHEN OTHER
                   MOVE CT-V19C001         TO WS-PROGRAMA
                   MOVE RET0-TABLENAME     TO WS-TABLA
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *
           .
       BUSCAR-DES-CENTRO-EXIT.
           EXIT.
      ******************************************************************
      * BUSCAR-FIRMAS                                                  *
      ******************************************************************
       BUSCAR-FIRMAS.
      *
           MOVE COD-OPE-BANCO OF DCLV1DT001
                                     TO COD-OPE-BANCO OF DCLV1DT002
           MOVE V159-FEC-OPE         TO FEC-OPE OF DCLV1DT002
           MOVE COD-DIV-OPE OF DCLV1DT001
                                     TO COD-DIV-OPE OF DCLV1DT002
           MOVE COD-CPT-CMP-VTA OF DCLV1DT001
                                     TO COD-CPT-CMP-VTA OF DCLV1DT002
           MOVE V159-NUM-REF         TO NUM-REF OF DCLV1DT002
      *
           EXEC SQL
               OPEN V12C02V-V1DC0021
           END-EXEC
           IF  SQLCODE NOT EQUAL ZEROS
               MOVE 'V1DT002'        TO WS-TABLA
               PERFORM 9999-ABEND-DB2
                  THRU 9999-ABEND-DB2-EXIT
           END-IF

           MOVE ZEROS        TO WS-I
           SET NO-FIN-CURSOR TO TRUE
           PERFORM LEER-CURSOR
              THRU LEER-CURSOR-EXIT
           UNTIL WS-I = 4 OR FIN-CURSOR

           EXEC SQL
               CLOSE V12C02V-V1DC0021
           END-EXEC
      *
           IF  SQLCODE NOT EQUAL ZEROS
               MOVE 'V1DT002'              TO WS-TABLA
               PERFORM 9999-ABEND-DB2
                  THRU 9999-ABEND-DB2-EXIT
           END-IF
      *
           .
       BUSCAR-FIRMAS-EXIT.
           EXIT.
      ******************************************************************
      * LEER-CURSOR                                                    *
      ******************************************************************
       LEER-CURSOR.
      *
           EXEC SQL
               FETCH V12C02V-V1DC0021
               INTO :DCLV1DT002.COD-USUARIO,
                    :DCLV1DT002.COD-NIV-AUTORIZ
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN ZEROS
                   ADD 1                   TO WS-I
                   EVALUATE WS-I
                       WHEN 1
                           MOVE COD-USUARIO OF DCLV1DT002 TO FIR1I
                       WHEN 2
                           MOVE COD-USUARIO OF DCLV1DT002 TO FIR2I
                       WHEN 3
                           MOVE COD-USUARIO OF DCLV1DT002 TO FIR3I
                       WHEN 4
                           MOVE COD-USUARIO OF DCLV1DT002 TO FIR4I
                   END-EVALUATE

               WHEN +100
                   SET FIN-CURSOR          TO TRUE

               WHEN OTHER
                   MOVE 'V1DT002'          TO WS-TABLA
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *
           .
       LEER-CURSOR-EXIT.
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
      * ATRIBUTOS-MAPA                                                 *
      ******************************************************************
       ATRIBUTOS-MAPA.
      *
           MOVE ATRI-PRO-BRI TO CRPA
                                NOMBREA
                                CRPBENA
                                NOMBENA
                                CONA
                                BANCOA
                                FODDA FOMMA FOAAA
                                PAISA
                                DIVA
                                OPERADA
                                OFCA DESOFCA
                                FIR1A FIR2A FIR3A FIR4A
                                REFERA
                                TIPOPEA
                                FORPAGA
                                MTODIVA
                                MTOBSA
                                MTODSA
                                NUMCTAA
                                NOTAA
           MOVE ATRI-PRO     TO LITERALA
      *
           .
       ATRIBUTOS-MAPA-EXIT.
           EXIT.
      ******************************************************************
      * 3000-FIN                                                       *
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
      ******************************************************************
      *  9999-ABEND-CICS                                               *
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
