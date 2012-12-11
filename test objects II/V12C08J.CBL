      ******************************************************************
      *                                                                *
      *  PROGRAMA: V12C08J                                             *
      *                                                                *
      *  FECHA CREACION: 06/01/2007                                    *
      *                                                                *
      *  AUTOR: FACTORIA                                               *
      *                                                                *
      *  APLICACION: V1 - MERCADO LIBRE DE DIVISAS (V1)                *
      *                                                                *
      *  DESCRIPCION: DETALLE DE LA OPERACION
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
       PROGRAM-ID.   V12C08J.
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
      *
       01  WS-VARIABLES.
           05  IMPORTE-EDIT                PIC Z.ZZZ.ZZZ.ZZZ.ZZ9,99.
           05  IMP-OPE-AUX                 PIC S9(13)V9(02) COMP-3.
           05  XCENTRO.
               10  FILLER                  PIC 9(3).
               10  XCOD-CENTRO.
                   15  XUNI-COF105         PIC 9(3).
                   15  XOFC-COF105         PIC 9(3).
      *
           05  COD-OPE-BANCO-A.
               10  XUNI-COF89              PIC 9(3).
               10  XOFC-COF89              PIC 9(3).
      *
           05  COD-PAIS-AUX                PIC 9(03).
           05  WS-BANCO-EDIT               PIC ZZZZ.
      ******************************************************************
      *
      *                        AREA DE CONTANTES                       *
      ******************************************************************
       01  CT-CONTANTES.
      *                                                                *
           05  CT-CURSOR                   PIC S9      VALUE -1.
           05  CT-INTRO                    PIC X(02)   VALUE '00'.
           05  CT-V19C001                  PIC X(08)   VALUE 'V19C001'.
           05  CT-V18CGEN                  PIC X(08)   VALUE 'V18CGEN'.
           05  CT-VQ9C006                  PIC X(08)   VALUE 'VQ9C006'.
      *
           05  WS-CUENTA-EDIT              PIC Z(12).
           05  WS-CUENTA-MAPA.
               10  WS-CTA1                 PIC X(02).
               10  WS-SEP1                 PIC X(01)   VALUE '-'.
               10  WS-CTA2                 PIC X(03).
               10  WS-SEP2                 PIC X(01)   VALUE '-'.
               10  WS-CTA3                 PIC X(07).
      ******************************************************************
      *                    AREA  DE  SWITCHES                          *
      ******************************************************************
       01  SW-SWITCHES.
      *
           05  SW-ENCONTRADO               PIC X(01)   VALUE 'N'.
               88  ENCONTRADO                          VALUE 'S'.
               88  NO-ENCONTRADO                       VALUE 'N'.
           05  SW-FIN-CURSOR               PIC X(01)   VALUE 'N'.
               88  FIN-CURSOR                          VALUE 'S'.
               88  NO-FIN-CURSOR                       VALUE 'N'.
      *
      *  COPY PARA EL PROGRAMA DE ABEND
       01  WS-QGECABC-01.
           COPY QGECABC.
      *    COPY DE RUTINA V19C001.
       01 WS-V1WC901 .
           COPY V1WC901.
      *
      *01  WS-V1WCGEN .
      *    COPY  V1WCGEN.
         COPY VJECDIV0.
      *
       01  WS-VQWC006 .
           COPY VQWC006.
      *
       01  WS-BGECRET0.
           COPY BGECRET0.
      *
           COPY QCWCI20.
      *  COPY PARA ERRORES DB2
           COPY QCWCL20.
      *
      ******* TABLA MERCADO LIBRE DE DIVISA*****************************
      *
           EXEC SQL
               INCLUDE V1GT001
           END-EXEC
      *
           EXEC SQL
               INCLUDE V1GT002
           END-EXEC
      ******************************************************************
      *                        CURSORES
      ******************************************************************
           EXEC SQL
               DECLARE V12C08J-V1DC0011
               CURSOR FOR
               SELECT COD_NIV_AUTORIZ,
                      COD_USUARIO
               FROM V1DT002
               WHERE
                  COD_OPE_BANCO       = :DCLV1DT001.COD-OPE-BANCO
                  AND FEC_OPE         = :DCLV1DT001.FEC-OPE
                  AND COD_DIV_OPE     = :DCLV1DT001.COD-DIV-OPE
                  AND COD_CPT_CMP_VTA = :DCLV1DT001.COD-CPT-CMP-VTA
                  AND NUM_REF         = :DCLV1DT001.NUM-REF
               ORDER BY COD_NIV_AUTORIZ
           END-EXEC
      *
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA.
           COPY QGECCAA.
       COPY V1NC06J.
       COPY V1EC001.
      ******************************************************************
      *                                                                *
      *           P R O C E D U R E      D I V I S I O N               *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION  USING V1NC06JI V1EC001.
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
      *   - INICIALIZAR VARIABLES AUXILIARES, COPYS,  AVISOS Y ERRORES *
      *   - DIRECCIONAR COPY DEL MAPA
      ******************************************************************
       1000-INICIO.
      *
           SET ADDRESS OF V1NC06JI         TO CAA-PTR-COPYIN
           SET ADDRESS OF V1EC001          TO CAA-PTRDATA
      *
           EXEC CICS
               IGNORE CONDITION ERROR
           END-EXEC
      *
           MOVE CAA-CODTRAN                TO CAA-CODTRAN-SIG
           SET CAA-88-ACCION-TERMINAL      TO TRUE
           SET CAA-88-CONTABLE-NO          TO TRUE
      *
           MOVE SPACES                     TO CAA-COD-AVISO1
                                              CAA-COD-AVISO2
                                              CAA-VAR1-ERROR
                                              CAA-VAR2-ERROR
                                              CAA-COD-ERROR
      *
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
      *                        2100-ESTADO-INICIO                      *
      *   - SE DA CONTROL A LA ARQUITECTURA PARA ENTRAR NOSOTROS       *
      *     MISMOS EN ESTADO CONTINUACION.                             *
      ******************************************************************
       2100-ESTADO-INICIO.
      *
           INITIALIZE V1NC06JI
           INITIALIZE DCLV1DT001
                      DCLV1DT002
                      WS-VARIABLES
      *
           PERFORM LEER-MERCADO
              THRU LEER-MERCADO-EXIT
      *
           IF  ENCONTRADO
               PERFORM MOVER-MERCADO-1
                  THRU MOVER-MERCADO-1-EXIT

               PERFORM BUSCAR-OFICINA
                  THRU BUSCAR-OFICINA-EXIT

      *        FIRMAS
               PERFORM 5100-ABRIR-CURSOR
                  THRU 5100-ABRIR-CURSOR-EXIT

               PERFORM 5200-LEER-CURSOR
                  THRU 5200-LEER-CURSOR-EXIT
               UNTIL FIN-CURSOR

               PERFORM 5700-CERRAR-CURSOR
                  THRU 5700-CERRAR-CURSOR-EXIT

               PERFORM BUSCAR-DIVISA
                  THRU BUSCAR-DIVISA-EXIT

               SET CAA-88-ACCION-TERMINAL TO TRUE
               SET CAA-88-ESTADO-CONTIN   TO TRUE
               PERFORM ATT-CAMPOS
           ELSE
               SET CAA-88-ACCION-TERMINAL  TO TRUE
               SET CAA-88-ESTADO-CONTIN    TO TRUE
               INITIALIZE V1NC06JI
               MOVE 'V1A0501'              TO CAA-COD-AVISO1
           END-IF
      *
           .
       2100-ESTADO-INICIO-EXIT.
           EXIT.
      ******************************************************************
      *                      5100-ABRIR-CURSOR                         *
      ******************************************************************
       5100-ABRIR-CURSOR.
      *
           EXEC SQL
               OPEN V12C08J-V1DC0011
           END-EXEC
      *
           MOVE SQLCODE                    TO DB2-RETURN-CDE

           IF  NOT DB2-OK
               INITIALIZE QGECABC
               MOVE 'V1DT002'              TO ABC-OBJETO-ERROR
               PERFORM 9999-ABEND-DB2
                  THRU 9999-ABEND-DB2-EXIT
           END-IF
      *
           .
       5100-ABRIR-CURSOR-EXIT.
           EXIT.
      ******************************************************************
      *                       5200-LEER-CURSOR                         *
      ******************************************************************
       5200-LEER-CURSOR.
      *
           EXEC SQL
               FETCH V12C08J-V1DC0011
                   INTO :COD-NIV-AUTORIZ ,
                        :COD-USUARIO

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
                   MOVE 'V1DT002'          TO ABC-OBJETO-ERROR
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *
           .
       5200-LEER-CURSOR-EXIT.
           EXIT.
      ******************************************************************
      *                      5300-PROCESAR-REGISTRO                    *
      ******************************************************************
       5300-PROCESAR-REGISTRO.
      *
           EVALUATE COD-NIV-AUTORIZ
               WHEN  '01'
                   MOVE COD-USUARIO  TO FIRMA1I
               WHEN  '02'
                   MOVE COD-USUARIO  TO FIRMA2I
               WHEN  '03'
                   MOVE COD-USUARIO  TO FIRMA3I
               WHEN  '04'
                   MOVE COD-USUARIO  TO FIRMA4I
               WHEN OTHER
                   CONTINUE
      *
           END-EVALUATE
      *
           .
       5300-PROCESAR-REGISTRO-EXIT.
           EXIT.
      ******************************************************************
      *                      5700-CERRAR-CURSOR                        *
      ******************************************************************
       5700-CERRAR-CURSOR.
      *
           EXEC SQL
               CLOSE V12C08J-V1DC0011
           END-EXEC
      *
           MOVE SQLCODE                    TO DB2-RETURN-CDE
           IF  NOT DB2-OK
               INITIALIZE QGECABC
               MOVE 'V1DT002'              TO ABC-OBJETO-ERROR
               PERFORM 9999-ABEND-DB2
                  THRU 9999-ABEND-DB2-EXIT
           END-IF
      *
           .
       5700-CERRAR-CURSOR-EXIT.
           EXIT.
      ******************************************************************
      *                        LEER-MERCADO                            *
      ******************************************************************
       LEER-MERCADO.
      *
           MOVE COD-OPE-BANCO-V163   TO COD-OPE-BANCO   OF DCLV1DT001
           MOVE FEC-OPE-V163(1:2)    TO FEC-OPE OF DCLV1DT001(7:2)
           MOVE FEC-OPE-V163(3:2)    TO FEC-OPE OF DCLV1DT001(5:2)
           MOVE FEC-OPE-V163(5:4)    TO FEC-OPE OF DCLV1DT001(1:4)
           MOVE COD-DIV-OPE-V163     TO COD-DIV-OPE     OF DCLV1DT001
           MOVE COD-CPT-CMP-VTA-V163 TO COD-CPT-CMP-VTA OF DCLV1DT001
           MOVE NUM-REF-V163         TO NUM-REF OF DCLV1DT001
      *
           EXEC SQL
               SELECT  COD_OPE_BANCO    ,
                       FEC_OPE          ,
                       COD_DIV_OPE      ,
                       COD_CPT_CMP_VTA  ,
                       NUM_REF          ,
                       NUM_DOC_PERSONA  ,
                       TIP_OPE          ,
                       TIP_PAGO         ,
                       NOM_RAZON        ,
                       IMP_OPE          ,
                       TAS_BOLIVAR      ,
                       TAS_DOLAR        ,
                       TAS_CAMBIO       ,
                       HOR_OPE          ,
                       FEC_VALOR        ,
                       IND_TIP_OPE      ,
                       COD_ENT_BANCO    ,
                       IDF_CMP_VTA      ,
                       CLA_USO_OPE      ,
                       NOM_BENEF        ,
                       COD_PAIS_CORR    ,
                       CTA_PASIVO_CL    ,
                       IDF_RIF_BENEF    ,
                       COD_PAIS_DEST
      *
               INTO :DCLV1DT001.COD-OPE-BANCO     ,
                    :DCLV1DT001.FEC-OPE           ,
                    :DCLV1DT001.COD-DIV-OPE       ,
                    :DCLV1DT001.COD-CPT-CMP-VTA   ,
                    :DCLV1DT001.NUM-REF           ,
                    :DCLV1DT001.NUM-DOC-PERSONA   ,
                    :DCLV1DT001.TIP-OPE           ,
                    :DCLV1DT001.TIP-PAGO          ,
                    :DCLV1DT001.NOM-RAZON         ,
                    :DCLV1DT001.IMP-OPE           ,
                    :DCLV1DT001.TAS-BOLIVAR       ,
                    :DCLV1DT001.TAS-DOLAR         ,
                    :DCLV1DT001.TAS-CAMBIO        ,
                    :DCLV1DT001.HOR-OPE           ,
                    :DCLV1DT001.FEC-VALOR         ,
                    :DCLV1DT001.IND-TIP-OPE       ,
                    :DCLV1DT001.COD-ENT-BANCO     ,
                    :DCLV1DT001.IDF-CMP-VTA       ,
                    :DCLV1DT001.CLA-USO-OPE       ,
                    :DCLV1DT001.NOM-BENEF         ,
                    :DCLV1DT001.COD-PAIS-CORR     ,
                    :DCLV1DT001.CTA-PASIVO-CL     ,
                    :DCLV1DT001.IDF-RIF-BENEF     ,
                    :DCLV1DT001.COD-PAIS-DEST
               FROM V1DT001
               WHERE COD_OPE_BANCO       = :DCLV1DT001.COD-OPE-BANCO
                     AND FEC_OPE         = :DCLV1DT001.FEC-OPE
                     AND COD_DIV_OPE     = :DCLV1DT001.COD-DIV-OPE
                     AND COD_CPT_CMP_VTA = :DCLV1DT001.COD-CPT-CMP-VTA
                     AND NUM_REF         = :DCLV1DT001.NUM-REF
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN ZEROES
               WHEN -811
                   SET ENCONTRADO          TO TRUE

               WHEN +100
                   SET NO-ENCONTRADO       TO TRUE

               WHEN OTHER
                   INITIALIZE QGECABC
                   MOVE 'V1DT001'          TO ABC-OBJETO-ERROR
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *
           .
       LEER-MERCADO-EXIT.
           EXIT.
      ******************************************************************
      *                        MOVER-MERCADO-1
      ******************************************************************
       MOVER-MERCADO-1.
      *
           MOVE COD-RIF                    TO NUMDOCI
           MOVE NOM-RAZON                  TO NOMRAZOI
           MOVE IDF-RIF-BENEF              TO IDFRIFBI
           MOVE NOM-BENEF                  TO NOMBENEI
           MOVE COD-CPT-CMP-VTA OF DCLV1DT001 TO CODCPTI
      *
           MOVE COD-ENT-BANCO              TO WS-BANCO-EDIT
           MOVE WS-BANCO-EDIT              TO ENTBANCI
      *
           MOVE FEC-OPE OF DCLV1DT001(7:2) TO FECOPEDI
           MOVE FEC-OPE OF DCLV1DT001(5:2) TO FECOPEMI
           MOVE FEC-OPE OF DCLV1DT001(1:4) TO FECOPEAI
      *
           MOVE COD-DIV-OPE OF DCLV1DT001  TO CODIVOPI
           MOVE CLA-USO-OPE                TO CLAUSOOI
           MOVE COD-OPE-BANCO OF DCLV1DT001 TO OPEBANCI
           MOVE NUM-REF OF DCLV1DT001      TO NUMREFI

      *    PAIS
           IF  COD-PAIS-CORR = ZEROS OR SPACES
               MOVE SPACES                 TO CPAISCOI
                                              PAISORII
           ELSE
               MOVE COD-PAIS-CORR          TO COD-PAIS-AUX
               PERFORM BUSCAR-PARAISO
                  THRU BUSCAR-PARAISO-EXIT
               MOVE COD-PAIS-CORR          TO CPAISCOI
               MOVE WC006-DESC-PAIS        TO PAISORII
           END-IF

      *    PAIS
           IF  COD-PAIS-DEST = ZEROS OR SPACES
               MOVE SPACES                 TO CPAISDEI
                                              PAISOIII
                                              PARAISOI
           ELSE
               MOVE COD-PAIS-DEST          TO COD-PAIS-AUX
               PERFORM BUSCAR-PARAISO
                  THRU BUSCAR-PARAISO-EXIT
               MOVE COD-PAIS-DEST          TO CPAISDEI
               MOVE WC006-DESC-PAIS        TO PAISOIII
               MOVE WC006-IN-PAIS          TO PARAISOI
           END-IF

           MOVE TIP-OPE                    TO TIPOPEI
           MOVE TIP-PAGO                   TO TIPPAGI
      *
           MOVE IMP-OPE                    TO IMPORTE-EDIT
           MOVE IMPORTE-EDIT               TO IMPOPEI

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
           MOVE WS-CUENTA-MAPA             TO CTAPASII

      *
           COMPUTE IMP-OPE-AUX = IMP-OPE * TAS-BOLIVAR
           MOVE IMP-OPE-AUX                TO IMPORTE-EDIT
           MOVE IMPORTE-EDIT               TO MTOBSI
      *
           IF COD-DIV-OPE OF DCLV1DT001  = 1
               MOVE IMP-OPE                TO  IMP-OPE-AUX
           ELSE
               COMPUTE IMP-OPE-AUX = IMP-OPE * TAS-DOLAR
           END-IF
           MOVE IMP-OPE-AUX                TO IMPORTE-EDIT
           MOVE IMPORTE-EDIT               TO MTODSI
      *                                                    .
           .
       MOVER-MERCADO-1-EXIT.
           EXIT.
      ******************************************************************
      *                        2200-ESTADO-CONTINUACION                *
      *     COMPROBAR QUE LA TECLA PULSADA, SE CORRESPONDE CON ALGUNA  *
      *     DE LAS OPCIONES VALIDAS DEL MENU (INTRO(00)).   CUALQUIER  *
      *     OTRO CASO DAREMOS ERROR DE TECLA INCORRECTA.               *
      ******************************************************************
       2200-ESTADO-CONTINUACION.
      *
           SET CAA-88-CODTRAN-SIG-ULTI     TO TRUE
           SET CAA-88-ACCION-PROGRAMA      TO TRUE
           SET CAA-88-ESTADO-INICIO        TO TRUE
      *
           .
       2200-ESTADO-CONTINUACION-EXIT.
           EXIT.
      ******************************************************************
      *                        BUSCAR-OFICINA
      ******************************************************************
       BUSCAR-OFICINA.
      *
           INITIALIZE WS-V1WC901
                      WS-BGECRET0
           MOVE ZEROES TO XCENTRO
      *
           MOVE COD-OPE-BANCO OF DCLV1DT001 TO COD-OPE-BANCO-A
      *
           IF  XUNI-COF89 = XOFC-COF89
               MOVE ZEROS                  TO XUNI-COF105
               MOVE XOFC-COF89             TO XOFC-COF105
           ELSE
               MOVE COD-OPE-BANCO-A        TO XCOD-CENTRO
           END-IF
      *
           MOVE ZEROS                      TO XCENTRO(1:3)
           MOVE XCENTRO                    TO COD-OFICINA
      *
           CALL CT-V19C001 USING WS-V1WC901 WS-BGECRET0
      *
           EVALUATE TRUE
               WHEN RET0-88-OK
                   MOVE DES-OFICINA        TO DESOFICI

               WHEN RET0-88-COD-AVISO
                   MOVE SPACES             TO DESOFICI
                   MOVE RET0-COD-AVISO1    TO CAA-COD-AVISO1
                   MOVE RET0-VAR1-AVISO1   TO CAA-VAR1-AVISO1

               WHEN RET0-88-COD-ERROR
                   MOVE 'CODIGO OFICINA NO VALIDO' TO DESOFICI

               WHEN OTHER
                   MOVE RET0-SQLCODE       TO SQLCODE
                   MOVE RET0-SQLERRM       TO SQLERRM
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *
           .
       BUSCAR-OFICINA-EXIT.
           EXIT.
      ******************************************************************
      *                        BUSCAR-DIVISA
      ******************************************************************
       BUSCAR-DIVISA.
      *
           CALL VJE-CT-RUT-DIVISA USING VJECDIV0
      *
           MOVE VJE-LIT-PLURAL(1:9)     TO CTL1910I
      *
           .
       BUSCAR-DIVISA-EXIT.
           EXIT.
      ******************************************************************
      *                        BUSCAR-PARAISO
      ******************************************************************
       BUSCAR-PARAISO.
      *
           INITIALIZE WS-VQWC006 WS-BGECRET0
      *
           MOVE COD-PAIS-AUX               TO WC006-COD-PAIS
      *
           CALL CT-VQ9C006 USING WS-VQWC006 WS-BGECRET0
      *
           EVALUATE TRUE
               WHEN RET0-88-OK
                   CONTINUE

               WHEN RET0-88-COD-AVISO
                   MOVE SPACES             TO WC006-DESC-PAIS
                   MOVE RET0-COD-ERROR     TO CAA-COD-ERROR
                   MOVE RET0-VAR1-ERROR    TO CAA-VAR1-ERROR

               WHEN RET0-88-COD-ERROR
                   MOVE 'CODIGO PAIS NO VALIDO' TO WC006-DESC-PAIS

               WHEN OTHER
                   MOVE RET0-SQLCODE   TO SQLCODE
                   MOVE RET0-SQLERRM   TO SQLERRM
                   MOVE RET0-TABLENAME TO ABC-OBJETO-ERROR
                   MOVE RET0-PROGRAMA  TO ABC-PROGRAMA
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT

                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *
           .
       BUSCAR-PARAISO-EXIT.
           EXIT.
      ******************************************************************
      * ATT-CAMPOS
      ******************************************************************
       ATT-CAMPOS.
      *
           MOVE ATRI-PRO-BRI-FST           TO NUMDOCA
                                              NOMRAZOA
                                              IDFRIFBA
                                              NOMBENEA
                                              CODCPTA
                                              ENTBANCA
                                              FECOPEDA
                                              FECOPEMA
                                              FECOPEAA
                                              CODIVOPA
                                              CLAUSOOA
                                              OPEBANCA
                                              DESOFICA
                                              NUMREFA
                                              CPAISCOA
                                              PAISORIA
                                              CPAISDEA
                                              PAISOIIA
                                              PARAISOA
                                              TIPOPEA
                                              TIPPAGA
                                              FIRMA1A
                                              IMPOPEA
                                              FIRMA2A
                                              MTOBSA
                                              FIRMA3A
                                              MTODSA
                                              FIRMA4A
                                              CTAPASIA
                                              SITUACIA
      *
           MOVE ATRI-PRO-FST               TO CTL1910A
      *
           .
      ******************************************************************
      *                            3000-FIN                            *
      ******************************************************************
       3000-FIN.
      *
           MOVE CAA-CODTRAN              TO  COD-TRANSACCION
      *
           EXEC CICS
               RETURN
           END-EXEC.
      *
       3000-FIN-EXIT.
           EXIT.
      ******************************************************************
      *                       9999-ABEND-DB2                           *
      *  SE ABENDA LA TAREA CUANDO SE PRODUCE UN ERROR DB2             *
      ******************************************************************
       9999-ABEND-DB2.
      *
           MOVE 'S'                        TO ABC-ABEND
           MOVE 'V12C08J'                  TO ABC-PROGRAMA
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
