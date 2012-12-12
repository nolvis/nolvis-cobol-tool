      ******************************************************************
      *                                                                *
      *  PROGRAMA: V12C11Y                                             *
      *                                                                *
      *  FECHA CREACION: 06/11/2008                                    *
      *                                                                *
      *  AUTOR: NOLVIS URQUIZA ELIAS                                   *
      *                                                                *
      *  APLICACION: TABLAS CORPORATIVAS ADABAS (V1).                  *
      *                                                                *
      *  DESCRIPCION: DETALLE DE TRANSACCIONES EN MONEDA EXTRANJERA    *
      *                                                                *
      *  TRANSACCION: V173                                             *
      *                                                                *
      ******************************************************************
      *SISTEMA : MONITOREO DE DIVISAS     MODULO : CONSOLIDADO CLTES . *
      *PROGRAMA: BPCLC011                 MAPA   : BPCMV056            *
      *FUNCION : DETALLE DE TRANSACCIONES EN MONEDA EXTRANJERA         *
      *AUTOR   : ROBERT HERNANDEZ/IMG/03-2000                          *
      *FECHA   : 16/03/2000                                            *
      ******************************************************************
      *                                                                *
      *         I D E N T I F I C A T I O N   D I V I S I O N          *
      *                                                                *
      *             INFORMACION GENERAL SOBRE EL PROGRAMA              *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID.   V12C11Y.
       AUTHOR.       FACTORIA.
       DATE-WRITTEN. 06/11/2008.
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
      *
           05  WS-ITEM                     PIC S9(4) COMP VALUE ZEROS.
           05  WS-GTS.
               10  FILLER                  PIC X(04)   VALUE '+GTS'.
               10  WS-GTS-SUFI             PIC X(04)   VALUE SPACES.
      *
           05  WS-COLA-AUX.
               10  FILLER                  PIC X(04)   VALUE 'V173'.
               10  WS-COLA-SUFI            PIC X(04)   VALUE SPACES.
      *
           05  WS-GTS-DATOS                PIC X(156)  VALUE SPACES.
           05  WS-DATOS-COLA               PIC X(23)   VALUE SPACES.
      *
           05  WS-GTS-FILA.
               10  WS-GTS-OPCION           PIC X(01)   VALUE SPACES.
               10  WS-GTS-ATT              PIC X(01)   VALUE 'B'.
               10  FILLER                  PIC X(02)   VALUE SPACES.
               10  WS-GTS-DIVISA1          PIC X(03).
               10  FILLER                  PIC X(04)   VALUE SPACES.
               10  WS-GTS-COMPRA-EFE       PIC ZZZZZZZZZ9.99.
               10  FILLER                  PIC X(03)   VALUE SPACES.
               10  WS-GTS-VENTA-EFE        PIC ZZZZZZZZZ9.99.
               10  FILLER                  PIC X(07)   VALUE SPACES.
               10  WS-GTS-COMPRA-CHQ       PIC ZZZZZZZZZ9.99.
               10  FILLER                  PIC X(03)   VALUE SPACES.
               10  WS-GTS-VENTA-CHQ        PIC ZZZZZZZZZ9.99.
               10  FILLER                  PIC X(05)   VALUE SPACES.
               10  WS-GTS-DIVISA2          PIC X(03).
               10  FILLER                  PIC X(04)   VALUE SPACES.
               10  WS-GTS-VENTA-TRA        PIC ZZZZZZZZZ9.99.
               10  FILLER                  PIC X(06)   VALUE SPACES.
               10  WS-GTS-TRANS-REC        PIC ZZZZZZZZZ9.99.
               10  FILLER                  PIC X(03)   VALUE SPACES.
               10  WS-GTS-TRANS-ENV        PIC ZZZZZZZZZ9.99.
               10  FILLER                  PIC X(06)   VALUE SPACES.
               10  WS-GTS-OFC-ORIG         PIC X(06).
               10  FILLER                  PIC X(08)   VALUE SPACES.
      *
           05  WS-GTS-FILA-AUX.
               10  WS-COL-TIPO             PIC X(01).
               10  WS-COL-COD-OPE-BANCO    PIC X(6).
               10  WS-COL-COD-DIV-OPE      PIC 9(03).
               10  WS-COL-COD-CPT-CMP-VTA  PIC X(03).
               10  WS-COL-NUMREF           PIC 9(10).
      *
           05  WS-GTS-CABECERA1.
               10  FILLER                  PIC X(03)   VALUE SPACES.
               10  FILLER                  PIC X(09)   VALUE
                                   'CED/RIF: '.
               10  WS-GTS-CRP              PIC X(15).
               10  FILLER                  PIC X(02)   VALUE SPACES.
               10  WS-GTS-NOMBRE           PIC X(25).
               10  FILLER                  PIC X(03)   VALUE SPACES.
               10  FILLER                  PIC X(07)   VALUE 'FECHA: '.
               10  WS-GTS-FECHA.
                   15  WS-GTS-FECHA-DD     PIC 9(02).
                   15  FILLER              PIC X(01)   VALUE '/'.
                   15  WS-GTS-FECHA-MM     PIC 9(02).
                   15  FILLER              PIC X(01)   VALUE '/'.
                   15  WS-GTS-FECHA-AA     PIC 9(04).
               10  FILLER                  PIC X(03)   VALUE SPACES.
      *
           05  WS-GTS-CABECERA2.
               10  FILLER                  PIC X(03)   VALUE SPACES.
               10  FILLER                  PIC X(11)   VALUE
                                   'EJECUTIVO: '.
               10  WS-GTS-EJECUTIVO        PIC X(25).
               10  FILLER                  PIC X(18)   VALUE SPACES.
               10  FILLER                  PIC X(07)   VALUE 'TELF : '.
               10  WS-GTS-TEL.
                   15  WS-GTS-TEL1         PIC 9(03).
                   15  FILLER              PIC X(01)   VALUE '-'.
                   15  WS-GTS-TEL2         PIC 9(03).
                   15  FILLER              PIC X(01)   VALUE '-'.
                   15  WS-GTS-TEL3         PIC 9(02).
                   15  FILLER              PIC X(01)   VALUE '-'.
                   15  WS-GTS-TEL4         PIC 9(02).
      *
           05  WS-GTS-CABECERA3.
               10  FILLER                  PIC X(29)   VALUE ALL '-'.
               10  FILLER                  PIC X(01)   VALUE SPACES.
               10  FILLER                  PIC X(17)   VALUE
                                   'MONEDA EXTRANJERA'.
               10  FILLER                  PIC X(01)   VALUE SPACES.
               10  FILLER                  PIC X(29)   VALUE ALL '-'.
      *
           05  WS-GTS-CABECERA4.
               10  FILLER                  PIC X(01)   VALUE SPACES.
               10  FILLER                  PIC X(01)   VALUE '-'.
               10  FILLER                  PIC X(06)   VALUE 'DIVISA'.
               10  FILLER                  PIC X(09)   VALUE SPACES.
               10  FILLER                  PIC X(15)   VALUE
                                   'E F E C T I V O'.
               10  FILLER                  PIC X(22)   VALUE SPACES.
               10  FILLER                  PIC X(13)   VALUE
                                   'C H E Q U E S'.
               10  FILLER                  PIC X(12)   VALUE SPACES.
               10  FILLER                  PIC X(06)   VALUE 'DIVISA'.
               10  FILLER                  PIC X(03)   VALUE SPACES.
               10  FILLER                  PIC X(13)   VALUE
                                   'T R A V E L S'.
               10  FILLER                  PIC X(12)   VALUE SPACES.
               10  FILLER                  PIC X(14)   VALUE
                                   'TRANSFERENCIAS'.
               10  FILLER                  PIC X(14)   VALUE SPACES.
               10  FILLER                  PIC X(07)   VALUE 'OFICINA'.
      *
           05  WS-GTS-CABECERA5.
               10  FILLER                  PIC X(01)   VALUE SPACES.
               10  FILLER                  PIC X(01)   VALUE '-'.
               10  FILLER                  PIC X(12)   VALUE SPACES.
               10  FILLER                  PIC X(06)   VALUE 'COMPRA'.
               10  FILLER                  PIC X(10)   VALUE SPACES.
               10  FILLER                  PIC X(05)   VALUE 'VENTA'.
               10  FILLER                  PIC X(15)   VALUE SPACES.
               10  FILLER                  PIC X(06)   VALUE 'COMPRA'.
               10  FILLER                  PIC X(10)   VALUE SPACES.
               10  FILLER                  PIC X(05)   VALUE 'VENTA'.
               10  FILLER                  PIC X(20)   VALUE SPACES.
               10  FILLER                  PIC X(05)   VALUE 'VENTA'.
               10  FILLER                  PIC X(13)   VALUE SPACES.
               10  FILLER                  PIC X(08)   VALUE
                                   'RECIBIDA'.
               10  FILLER                  PIC X(08)   VALUE SPACES.
               10  FILLER                  PIC X(07)   VALUE
                                   'ENVIADA'.
               10  FILLER                  PIC X(09)   VALUE SPACES.
               10  FILLER                  PIC X(06)   VALUE 'ORIGEN'.
      *
           05  WS-GTS-TOTALES.
               10  FILLER                  PIC X(01)   VALUE SPACES.
               10  FILLER                  PIC X(01)   VALUE '+'.
               10  FILLER                  PIC X(09)   VALUE
                                   'TOTALES: '.
               10  WS-GTS-TOT-EFECT-COMPRA PIC ZZZZZZZZZ9.99.
               10  FILLER                  PIC X(03)   VALUE SPACES.
               10  WS-GTS-TOT-EFECT-VENTA  PIC ZZZZZZZZZ9.99.
               10  FILLER                  PIC X(07)   VALUE SPACES.
               10  WS-GTS-TOT-CHEQUE-COMPRA PIC ZZZZZZZZZ9.99.
               10  FILLER                  PIC X(03)   VALUE SPACES.
               10  WS-GTS-TOT-CHEQUE-VENTA PIC ZZZZZZZZZ9.99.
               10  FILLER                  PIC X(03)   VALUE SPACES.
               10  FILLER                  PIC X(09)   VALUE
                                   'TOTALES: '.
               10  WS-GTS-TOT-TRAVEL-VENTA PIC ZZZZZZZZZ9.99.
               10  FILLER                  PIC X(06)   VALUE SPACES.
               10  WS-GTS-TOT-TRANSF-COMPRA PIC ZZZZZZZZZ9.99.
               10  FILLER                  PIC X(03)   VALUE SPACES.
               10  WS-GTS-TOT-TRANSF-VENTA PIC ZZZZZZZZZ9.99.
      *
           05  WS-COD-EJECUTIVO            PIC 9(04).
           05  WS-FECHA                    PIC X(08).
           05  WS-FECHA-RE REDEFINES WS-FECHA.
               10  WS-FECHA-DD             PIC 9(02).
               10  WS-FECHA-MM             PIC 9(02).
               10  WS-FECHA-AA             PIC 9(04).
      *
           05  WS-TOTAL-CHEQUE-COMPRA      PIC 9(10)V9(2).
           05  WS-TOTAL-CHEQUE-VENTA       PIC 9(10)V9(2).
           05  WS-TOTAL-EFECT-COMPRA       PIC 9(10)V9(2).
           05  WS-TOTAL-EFECT-VENTA        PIC 9(10)V9(2).
           05  WS-TOTAL-TRANSF-COMPRA      PIC 9(10)V9(2).
           05  WS-TOTAL-TRANSF-VENTA       PIC 9(10)V9(2).
           05  WS-TOTAL-TRAVEL-VENTA       PIC 9(10)V9(2).
      *
           05  WS-COD-CPT-CMP-VTA           PIC X(03).
           05  WS-CONCEPTO-NUM REDEFINES WS-COD-CPT-CMP-VTA
                                           PIC 9(03).
      *
           05  VEC-FECHA-ALF               PIC X(08).
           05  WS-FECHA-BPC                PIC 9(08).
           05  WS-EXP-BPC                  PIC X(10)   JUST RIGHT.
      *
           05  WS-CED-RIF-CBB              PIC X(09).
           05  REDEFINES WS-CED-RIF-CBB.
               10  WS-NAC-CBB              PIC X(01).
               10  WS-NUM-CBB              PIC 9(08).
           05  NOMBRE-CBB                  PIC X(40).
      *
           05  WS-NUM-DOC-PERSONA          PIC X(15).
           05  REDEFINES WS-NUM-DOC-PERSONA.
               10  BPB-NAC                 PIC X(01).
               10  BPB-NUM                 PIC 9(14).
               10  REDEFINES BPB-NUM.
                   15  BPB-NUM1            PIC 9(06).
                   15  BPB-NUM2            PIC 9(08).
           05  WS-EXP-CLC                  PIC X(10).
      *
           05  IND-MAS-DATOS               PIC X(01)   VALUE SPACES.
      ******************************************************************
      *                    AREA  DE  SWITCHES                          *
      ******************************************************************
       01  SW-SWITCHES.
      *
           05  SW-FIN-CURSOR               PIC X(01)   VALUE 'N'.
               88  FIN-CURSOR                          VALUE 'S'.
               88  NO-FIN-CURSOR                       VALUE 'N'.
      *
           05  SW-PF-PRO                   PIC 9(02).
               88  PF-PF2                              VALUE 02.
               88  PF-PF3                              VALUE 03.
               88  PF-PF4                              VALUE 04.
               88  PF-PF5                              VALUE 05.
               88  PF-PF6                              VALUE 06.
               88  PF-PF7                              VALUE 07.
               88  PF-PF8                              VALUE 08.
               88  PF-PF9                              VALUE 09.
               88  PF-PF10                             VALUE 10.
               88  PF-PF11                             VALUE 11.
               88  PF-BORRA                            VALUE 99.
               88  PF-INTRO                            VALUE 00.
      *
           05  SW-FIN-COLA                 PIC X(01)   VALUE SPACES.
               88  FIN-COLA                            VALUES 'S'.
               88  NO-FIN-COLA                         VALUES 'N'.
      *
           05  SW-PERSONA-EXISTE           PIC X(01)   VALUE SPACES.
               88  SI-PERSONA-EXISTE                   VALUE 'S'.
               88  NO-PERSONA-EXISTE                   VALUE 'N'.
      ******************************************************************
      *                        AREA DE CONTANTES                       *
      ******************************************************************
       01  CT-CONSTANTES.
      *
           05  CT-PROGRAMA                 PIC X(08)   VALUE 'V12C11Y'.
           05  CT-CASO1                    PIC X(01)   VALUE '1'.
           05  CT-CASO2                    PIC X(01)   VALUE '2'.
           05  CT-CASO3                    PIC X(01)   VALUE '3'.
           05  CT-SI                       PIC X(01)   VALUE 'S'.
           05  CT-TAB-V1DT001              PIC X(08)   VALUE 'V1DT001'.
           05  CT-CONSULTAR                PIC X(01)   VALUE 'C'.
           05  CT-OBLIGA-SELECCION         PIC X(01)   VALUE 'S'.
           05  CT-INTRO                    PIC X(02)   VALUE '00'.
           05  CT-ENTER                    PIC X(05)   VALUE 'ENTER'.
           05  CT-UNO                      PIC 9(01)   VALUE 1.
      *
           05  CT-CONTENIDO.
               10  FILLER                  PIC X(12)   VALUE SPACES.
               10  FILLER                  PIC X(29)
               VALUE '** CONSOLIDADO DE CLIENTES **'.
      *
           05  CT-ERROR-WRITEQ             PIC X(20)
                                   VALUE 'ERROR EN CICS WRITQ'.
           05  CT-ERROR-DELETEQ            PIC X(20)
                                   VALUE 'ERROR CICS DELETEQ'.
           05  CT-ERROR-READQ              PIC X(20)
                                   VALUE 'ERROR CICS READQ'.
      *
           05  CT-QGECABC                  PIC X(08)   VALUE 'QG1CABC '.
           05  CT-PE9C2010                 PIC X(08)   VALUE 'PE9C2010'.
           05  CT-NUM-LINEAS-CABEC         PIC 9(02)   VALUE 05.
      ******************************************************************
      *                      AREA DE CONTADORES                        *
      ******************************************************************
       01  CN-CONTADORES.
           05  CN-REGISTROS                PIC 9(03)   VALUE ZEROES.
      ******************************************************************
      *                        AREA DE INDICES
      ******************************************************************
       01  IN-INDICES.
           05  IN-REGISTRO                 PIC S9(03) COMP VALUE ZEROES.
           05  IN-K                        PIC 9(07)  COMP-3 VALUE 1.
           05  MP                          PIC S9(07)  COMP-3.
      *
           EXEC SQL
               INCLUDE BKAB0003
           END-EXEC
           EXEC SQL
               INCLUDE V1GT001
           END-EXEC
      ******************************************************************
      *                    COPYS UTILIZADAS                            *
      ******************************************************************
      *  COPY PARA ERRORES DB2
           COPY QCWCL20.
      *  COPY PARA ATRIBUTOS DE LA PANTALLA
           COPY DFHBMSCA.
      *  COPY PARA EL PROGRAMA DE ABEND
       01  WS-QGECABC-01.
           COPY QGECABC.
      *  C0PY DE CODIGOS DE RETORNO
       01  WS-BGECRET0.
           COPY BGECRET0.
      *
       01  WS-PEEC201A.
           COPY PEEC201A.
      *---- COPY DE VARIABLES DE MANEJO DE ERRORES DB2.
       COPY QRECDB2.
      ******************************************************************
      *                        CURSORES
      ******************************************************************
           EXEC SQL
               DECLARE V12C11Y-V1DC0011
               CURSOR FOR
               SELECT COD_OPE_BANCO   ,
                          NUM_DOC_PERSONA ,
                          FEC_OPE         ,
                          COD_DIV_OPE     ,
                          NOM_RAZON       ,
                          IMP_OPE         ,
                          TAS_DOLAR       ,
                          CTA_PASIVO_CL   ,
                          NUM_REF         ,
                          IDF_ORD_PAGO    ,
                          TIP_PAGO        ,
                          COD_CPT_CMP_VTA ,
                          TIP_OPE
               FROM V1DT001
               WHERE FEC_OPE = :FEC-OPE AND
                     NUM_DOC_PERSONA = :NUM-DOC-PERSONA AND
                     NOT (TIP_OPE BETWEEN '5' AND '8')
               ORDER BY NUM_REF
           END-EXEC
      *
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA.
      *
       COPY QGECCAA.
           COPY V1EC001.
      ******************************************************************
      *                                                                *
      *           P R O C E D U R E      D I V I S I O N               *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION USING DFHCOMMAREA V1EC001.
      *
       MAINLINE.
      *
           PERFORM 1000-INICIO
              THRU 1000-INICIO-EXIT
      *
           PERFORM 2000-PROCESO
              THRU 2000-PROCESO-EXIT
      *
           PERFORM 7000-FIN
              THRU 7000-FIN-EXIT
      *
           GOBACK
           .
      ******************************************************************
      *                         1000-INICIO                            *
      ******************************************************************
       1000-INICIO.
      *
           SET ADDRESS OF V1EC001          TO CAA-PTRDATA
      *
           EXEC CICS
               IGNORE CONDITION ERROR
           END-EXEC
      *
           INITIALIZE WS-VARIABLES-AUXILIARES
                      WS-BGECRET0
           MOVE SPACES                     TO CAA-COD-AVISO1
                                              CAA-COD-AVISO2
                                              CAA-VAR1-ERROR
                                              CAA-VAR2-ERROR
                                              CAA-COD-ERROR
      *
           MOVE CAA-TERMINAL               TO WS-GTS-SUFI
                                              WS-COLA-SUFI
      *
           SET CAA-88-ACCION-TERMINAL      TO TRUE
           SET CAA-88-CONTABLE-NO          TO TRUE
           MOVE CAA-CODTRAN                TO CAA-CODTRAN-SIG
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
                   PERFORM 3000-ESTADO-CONTINUACION
                      THRU 3000-ESTADO-CONTINUACION-EXIT
           END-EVALUATE
      *
           .
       2000-PROCESO-EXIT.
           EXIT.
      ******************************************************************
      *2100-ESTADO-INICIO
      * CASO2 = REGRESO DE LA TRAN V163
      ******************************************************************
       2100-ESTADO-INICIO.
      *
           IF  CAA-CASO = '2'
               PERFORM PREPARAR-PAGINACION
               SET CAA-88-ACCION-TERMINAL  TO TRUE
               SET CAA-88-ESTADO-CONTIN    TO TRUE
               SET CAA-88-TIPO-SALIDA-PAG  TO TRUE
           ELSE
               PERFORM BORRAR-COLA-GTS
               PERFORM BORRAR-COLA-AUX
               PERFORM GRABAR-REGISTROS
               IF  CN-REGISTROS GREATER THAN ZEROES
                   PERFORM GRABAR-TOTALES
                   PERFORM PREPARAR-PAGINACION

                   SET CAA-88-ACCION-TERMINAL  TO TRUE
                   SET CAA-88-ESTADO-CONTIN    TO TRUE
                   SET CAA-88-TIPO-SALIDA-PAG  TO TRUE
               ELSE
                   SET CAA-88-CODTRAN-SIG-SAME TO TRUE
                   SET CAA-88-ACCION-TERMINAL  TO TRUE
                   SET CAA-88-ESTADO-CONTIN    TO TRUE
                   MOVE 'V1E0060'              TO CAA-COD-ERROR
               END-IF
           END-IF
      *
           .
       2100-ESTADO-INICIO-EXIT.
           EXIT.
      ******************************************************************
      * PREPARAR-PAGINACION
      ******************************************************************
       PREPARAR-PAGINACION.
      *
           INITIALIZE CAA-PAGINAR
           MOVE 'X'                        TO CAA-SEL-PERMIT(1)
           SET CAA-88-IND-VARSEL-SI        TO TRUE
      *
           MOVE '00'                       TO CAA-FKEY-NUM(1)
           MOVE 'DETA.'                    TO CAA-FKEY-LIT(1)
           MOVE 'S'                        TO CAA-FKEY-SEL(1)

           MOVE '03'                       TO CAA-FKEY-NUM(2)
           MOVE 'CLTES'                    TO CAA-FKEY-LIT(2)
           MOVE 'N'                        TO CAA-FKEY-SEL(2)

           MOVE '06'                       TO CAA-FKEY-NUM(3)
           MOVE 'PERFI'                    TO CAA-FKEY-LIT(3)
           MOVE 'N'                        TO CAA-FKEY-SEL(3)

           MOVE CT-CONTENIDO               TO CAA-CONTENID
      *
           SET CAA-88-IND-MOD-DATO-NO      TO TRUE
           MOVE CT-NUM-LINEAS-CABEC        TO CAA-NUM-LIN-CAB
           SET CAA-88-IND-SCROLL-SI        TO TRUE
      *
           SET CAA-88-IND-AVPAG-NO     TO TRUE
      *
           .
      ******************************************************************
      * 3000-ESTADO-CONTINUACION
      ******************************************************************
       3000-ESTADO-CONTINUACION.
      *
           MOVE CAA-TECLA                  TO SW-PF-PRO
           EVALUATE TRUE
               WHEN PF-INTRO
                   PERFORM COMENZAR-PROCESO

               WHEN PF-PF3
                   PERFORM VERIFICAR-PERSONA
      *            OJO: DUDA:
      *            FETCH RETURN 'CBBLC490' CED-RIF-CBB

                   MOVE 'V1A0503' TO CAA-COD-AVISO1
                   PERFORM REINPUT

               WHEN PF-PF6
                   PERFORM VERIFICAR-PERSONA
      *            OJO: DUDA:
      *            CALLNAT 'CBBSP520' CED-RIF-CBB NU-COR

                   MOVE 'V1A0503' TO CAA-COD-AVISO1
                   PERFORM REINPUT

               WHEN PF-PF8
                   PERFORM GRABAR-REGISTROS

                   IF  IND-MAS-DATOS EQUAL CT-SI
                       SET CAA-88-IND-AVPAG-SI TO TRUE
                   ELSE
                       SET CAA-88-IND-AVPAG-NO TO TRUE
                   END-IF
                   SET CAA-88-ESTADO-CONTIN    TO TRUE

               WHEN PF-BORRA
                   PERFORM BORRAR-COLA-GTS
                   PERFORM BORRAR-COLA-AUX
                   SET CAA-88-CODTRAN-SIG-ULTI TO TRUE
                   SET CAA-88-ACCION-PROGRAMA  TO TRUE
                   SET CAA-88-ESTADO-INICIO    TO TRUE

      *        OJO: FALTA LLAMAR A LA V181
      *        WHEN PF-PF11
      *            MOVE 'V181'                 TO CAA-CODTRAN-SIG
      *            SET CAA-88-ACCION-PROGRAMA  TO TRUE
      *            SET CAA-88-ESTADO-INICIO    TO TRUE
      *            SET CAA-88-CADENA-ANADIR    TO TRUE
           END-EVALUATE
      *
           .
       3000-ESTADO-CONTINUACION-EXIT.
           EXIT.
      ******************************************************************
      * COMENZAR-PROCESO
      ******************************************************************
       COMENZAR-PROCESO.
      *
           MOVE CT-NUM-LINEAS-CABEC        TO IN-REGISTRO
           ADD 1                           TO IN-REGISTRO
           MOVE ZEROES                     TO CN-REGISTROS
           MOVE SPACES                     TO WS-GTS-DATOS(1:1)
           SET NO-FIN-COLA                 TO TRUE
      *
           PERFORM
           UNTIL WS-GTS-DATOS(1:1) NOT EQUAL SPACES OR FIN-COLA
               PERFORM LEER-REGISTRO-COLA-GTS
               ADD 1                       TO IN-REGISTRO
               ADD 1                       TO CN-REGISTROS
           END-PERFORM
      *
           PERFORM PROCESAR-SELECCION
      *
           .
      ******************************************************************
      * PROCESAR-SELECCION
      ******************************************************************
       PROCESAR-SELECCION.
      *
           MOVE WS-GTS-DATOS               TO WS-GTS-FILA
           SUBTRACT CT-UNO                 FROM IN-REGISTRO
      *
           PERFORM BORRAR-SELECION-TS
      *
           SUBTRACT CT-NUM-LINEAS-CABEC    FROM IN-REGISTRO
      *    BUSCAR DATOS EN COLA AUX
           PERFORM LEER-REGISTRO-COLA-AUX
           MOVE WS-DATOS-COLA              TO WS-GTS-FILA-AUX
      *
           EVALUATE TRUE
               WHEN WS-COL-TIPO = 6 AND
                    WS-GTS-OFC-ORIG = '010297'
                   MOVE WS-COL-NUMREF TO WS-EXP-CLC
                   MOVE V173-NAC1     TO WS-NAC-CBB
                   MOVE V173-NUM12    TO WS-NUM-CBB

      *            OJO: DUDA:
      *            FETCH RETURN 'CLCLC23'
      *              WSCED-RIF-CBB 01 WS-EXP-CLC V173-FECHA-B
                   MOVE 'V1A0503' TO CAA-COD-AVISO1
                   PERFORM REINPUT

               WHEN (WS-COL-TIPO = 6 AND
                     WS-GTS-OFC-ORIG NOT = '010297') OR
                    (WS-COL-TIPO = 1 OR 2 OR 3 OR 4 OR 5 OR 8)

                   MOVE 'V163'                 TO CAA-CODTRAN-SIG
                   SET CAA-88-ACCION-PROGRAMA  TO TRUE
                   SET CAA-88-ESTADO-INICIO    TO TRUE
                   SET CAA-88-CADENA-ANADIR    TO TRUE
                   MOVE '2'                    TO CAA-CASO-CAD
      *
                   MOVE WS-COL-COD-OPE-BANCO   TO COD-OPE-BANCO-V163
                   MOVE V173-FECHA-B           TO FEC-OPE-V163
                   MOVE WS-COL-COD-DIV-OPE     TO COD-DIV-OPE-V163
                   MOVE WS-COL-COD-CPT-CMP-VTA TO COD-CPT-CMP-VTA-V163
                   MOVE WS-COL-NUMREF          TO NUM-REF-V163
           END-EVALUATE
      *
           .
      ******************************************************************
      * BORRAR-SELECION-TS
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
      ******************************************************************
      * VERIFICAR-PERSONA
      ******************************************************************
       VERIFICAR-PERSONA.
      *
           INITIALIZE PEEC201A
           MOVE '0102'                     TO PEEC201A-PECDGENT
           MOVE SPACES                     TO PEEC201A-PENUMPER
           MOVE V173-CED-RIF1(1:1)         TO PEEC201A-PETIPDOC
           MOVE '000'                      TO PEEC201A-PENUMDOC(1:3)
           MOVE V173-CED-RIF1(8:8)         TO PEEC201A-PENUMDOC(4:8)
           MOVE '01'                       TO PEEC201A-PESECDOC
           MOVE '00'                       TO PEEC201A-IND-GRUPO
      *
           CALL CT-PE9C2010 USING WS-PEEC201A
      *
           EVALUATE PEEC201A-RETORNO
               WHEN '00'
                   CONTINUE

               WHEN '20'
                   MOVE 'V1E0504'          TO CAA-COD-ERROR
      *            ** CEDULA / RIF NO EXISTE EN REGISTRO DE CLIENTES **
                   PERFORM REINPUT

               WHEN OTHER
                   MOVE PEEC201A-COD-ERROR TO CAA-COD-ERROR
                   PERFORM REINPUT
           END-EVALUATE
      *
           .
      ******************************************************************
      * GRABAR-REGISTROS
      ******************************************************************
       GRABAR-REGISTROS.
      *
           INITIALIZE WS-TOTAL-EFECT-COMPRA
                      WS-TOTAL-EFECT-VENTA
                      WS-TOTAL-CHEQUE-COMPRA
                      WS-TOTAL-CHEQUE-VENTA
                      WS-TOTAL-TRAVEL-VENTA
                      WS-TOTAL-TRANSF-COMPRA
                      WS-TOTAL-TRANSF-VENTA
      *
           MOVE ZEROES                     TO CN-REGISTROS
      *
           MOVE V173-FECHA-B               TO WS-FECHA
           MOVE WS-FECHA-AA                TO FEC-OPE(1:4)
           MOVE WS-FECHA-MM                TO FEC-OPE(5:2)
           MOVE WS-FECHA-DD                TO FEC-OPE(7:2)
           MOVE V173-CED-RIF1              TO NUM-DOC-PERSONA
      *
           PERFORM ABRIR-CURSOR

           SET NO-FIN-CURSOR               TO TRUE
           PERFORM LEER-CURSOR
             UNTIL FIN-CURSOR

           PERFORM CERRAR-CURSOR
      *
           .
      ******************************************************************
      * ABRIR-CURSOR
      ******************************************************************
       ABRIR-CURSOR.
      *
           EXEC SQL
               OPEN V12C11Y-V1DC0011
           END-EXEC
      *
           MOVE SQLCODE                    TO DB2-RETURN-CDE
           IF  NOT DB2-OK
               INITIALIZE QGECABC
               MOVE CT-TAB-V1DT001         TO ABC-OBJETO-ERROR
               PERFORM 9999-ABEND-DB2
                  THRU 9999-ABEND-DB2-EXIT
           END-IF
      *
           .
      ******************************************************************
      * LEER-CURSOR
      ******************************************************************
       LEER-CURSOR.
      *
           EXEC SQL
               FETCH V12C11Y-V1DC0011
               INTO :COD-OPE-BANCO   ,
                    :NUM-DOC-PERSONA ,
                    :FEC-OPE         ,
                    :COD-DIV-OPE     ,
                    :NOM-RAZON       ,
                    :IMP-OPE         ,
                    :TAS-DOLAR       ,
                    :CTA-PASIVO-CL   ,
                    :NUM-REF         ,
                    :IDF-ORD-PAGO    ,
                    :TIP-PAGO        ,
                    :COD-CPT-CMP-VTA ,
                    :TIP-OPE
           END-EXEC
      *
           MOVE SQLCODE                    TO DB2-RETURN-CDE
           EVALUATE TRUE
               WHEN DB2-OK
                   PERFORM PROCESAR-REGISTRO

               WHEN DB2-NOTFND
                   SET FIN-CURSOR          TO TRUE
                   MOVE SPACES             TO IND-MAS-DATOS

               WHEN OTHER
                   INITIALIZE QGECABC
                   MOVE CT-TAB-V1DT001     TO ABC-OBJETO-ERROR
                   PERFORM 9999-ABEND-DB2
                      THRU 9999-ABEND-DB2-EXIT
           END-EVALUATE
      *
           .
      ******************************************************************
      * PROCESAR-REGISTRO
      ******************************************************************
       PROCESAR-REGISTRO.
      *
           ADD 1                           TO CN-REGISTROS
           IF  CN-REGISTROS EQUAL CT-UNO AND CAA-88-ESTADO-INICIO
               PERFORM GRABAR-CABECERA
           END-IF
           IF  CN-REGISTROS > 5000
               MOVE 'V1A0502'              TO CAA-COD-AVISO1
               SET FIN-CURSOR              TO TRUE
           ELSE
               INITIALIZE WS-GTS-FILA
               MOVE 'B'                    TO WS-GTS-ATT
      *
               PERFORM BUSCAR-DIVISA
               MOVE COD-OPE-BANCO          TO WS-GTS-OFC-ORIG
               MOVE NUM-REF                TO WS-COL-NUMREF
               PERFORM ANALIZAR-TIP-OPER
      *
               MOVE WS-GTS-FILA            TO WS-GTS-DATOS
               PERFORM ADD-REGISTRO-COLA-GTS

      *        GUARDAR LLAVE EN COLA AUXILIAR
               MOVE COD-OPE-BANCO     TO WS-COL-COD-OPE-BANCO
               MOVE COD-DIV-OPE       TO WS-COL-COD-DIV-OPE
               MOVE COD-CPT-CMP-VTA   TO WS-COL-COD-CPT-CMP-VTA
               MOVE WS-GTS-FILA-AUX   TO WS-DATOS-COLA
               PERFORM ADD-REGISTRO-COLA-AUX
           END-IF
      *
           .
      ******************************************************************
      * ANALIZAR-TIP-OPER
      ******************************************************************
       ANALIZAR-TIP-OPER.
      *
           MOVE COD-CPT-CMP-VTA            TO WS-COD-CPT-CMP-VTA
      *
           EVALUATE TRUE
      *        COMPRA EFECTIVO
               WHEN TIP-PAGO = '1' AND
                    WS-CONCEPTO-NUM >= 100 AND WS-CONCEPTO-NUM <= 199
                   MOVE 1                  TO WS-COL-TIPO
                   MOVE IMP-OPE            TO WS-GTS-COMPRA-EFE
                   COMPUTE WS-TOTAL-EFECT-COMPRA =
                           WS-TOTAL-EFECT-COMPRA + IMP-OPE

      *        VENTA EFECTIVO
               WHEN TIP-PAGO = '1' AND
                    WS-CONCEPTO-NUM >= 200 AND WS-CONCEPTO-NUM <= 299
                   MOVE 2                  TO WS-COL-TIPO
                   MOVE IMP-OPE            TO WS-GTS-VENTA-EFE
                   COMPUTE WS-TOTAL-EFECT-VENTA =
                           WS-TOTAL-EFECT-VENTA + IMP-OPE

      *        COMPRA CHEQUE
               WHEN TIP-PAGO = '2' AND
                    WS-CONCEPTO-NUM >= 100 AND WS-CONCEPTO-NUM <= 199
                   MOVE 3                  TO WS-COL-TIPO
                   MOVE IMP-OPE            TO WS-GTS-COMPRA-CHQ
                   COMPUTE WS-TOTAL-CHEQUE-COMPRA =
                           WS-TOTAL-CHEQUE-COMPRA + IMP-OPE

      *        VENTA CHEQUE
               WHEN TIP-PAGO = '2' AND
                    WS-CONCEPTO-NUM >= 200 AND WS-CONCEPTO-NUM <= 299
                   MOVE 4                  TO WS-COL-TIPO
                   MOVE IMP-OPE            TO WS-GTS-VENTA-CHQ
                   COMPUTE WS-TOTAL-CHEQUE-VENTA =
                           WS-TOTAL-CHEQUE-VENTA + IMP-OPE

      *        VENTA TRAVEL
               WHEN TIP-PAGO = '4' AND
                    WS-CONCEPTO-NUM >= 200 AND WS-CONCEPTO-NUM <= 299
                   MOVE 5                  TO WS-COL-TIPO
                   MOVE IMP-OPE            TO WS-GTS-VENTA-TRA
                   COMPUTE WS-TOTAL-TRAVEL-VENTA =
                           WS-TOTAL-TRAVEL-VENTA + IMP-OPE

      *        TRANSF RECIBIDA
               WHEN TIP-PAGO = '3' AND
                    WS-CONCEPTO-NUM >= 100 AND WS-CONCEPTO-NUM <= 199
                   MOVE 6                  TO WS-COL-TIPO

                   IF  COD-OPE-BANCO = '010297'
      *                CLC
                       MOVE IDF-ORD-PAGO   TO WS-COL-NUMREF
                   ELSE
      *                BPC
                       MOVE NUM-REF        TO WS-COL-NUMREF
                   END-IF

                   MOVE IMP-OPE            TO WS-GTS-TRANS-REC
                   COMPUTE WS-TOTAL-TRANSF-COMPRA =
                           WS-TOTAL-TRANSF-COMPRA + IMP-OPE

      *        TRANSF ENVIADA
               WHEN TIP-PAGO = '3' AND
                    WS-CONCEPTO-NUM >= 200 AND WS-CONCEPTO-NUM <= 299
                   MOVE 8                  TO WS-COL-TIPO
                   MOVE IMP-OPE            TO WS-GTS-TRANS-ENV
                   COMPUTE WS-TOTAL-TRANSF-VENTA =
                           WS-TOTAL-TRANSF-VENTA + IMP-OPE
           END-EVALUATE
      *
           .
      ******************************************************************
      * GRABAR-CABECERA
      ******************************************************************
       GRABAR-CABECERA.
      *
           MOVE V173-CED-RIF1              TO WS-GTS-CRP
           MOVE V173-NOMBRES               TO WS-GTS-NOMBRE
           MOVE V173-FECHA-B(1:2)          TO WS-GTS-FECHA-DD
           MOVE V173-FECHA-B(3:2)          TO WS-GTS-FECHA-MM
           MOVE V173-FECHA-B(5:4)          TO WS-GTS-FECHA-AA

           MOVE '  '                       TO WS-GTS-DATOS(1:2)
           MOVE WS-GTS-CABECERA1           TO WS-GTS-DATOS(3:77)
           MOVE WS-GTS-CABECERA1           TO WS-GTS-DATOS(80:77)
           PERFORM ADD-REGISTRO-COLA-GTS
      *
           PERFORM BUSCAR-EJECUTIVO
           MOVE '  '                       TO WS-GTS-DATOS(1:2)
           MOVE WS-GTS-CABECERA2           TO WS-GTS-DATOS(3:77)
           MOVE WS-GTS-CABECERA2           TO WS-GTS-DATOS(80:77)
           PERFORM ADD-REGISTRO-COLA-GTS
      *
           MOVE '  '                       TO WS-GTS-DATOS(1:2)
           MOVE WS-GTS-CABECERA3           TO WS-GTS-DATOS(3:77)
           MOVE WS-GTS-CABECERA3           TO WS-GTS-DATOS(80:77)
           PERFORM ADD-REGISTRO-COLA-GTS
      *
           MOVE WS-GTS-CABECERA4           TO WS-GTS-DATOS
           PERFORM ADD-REGISTRO-COLA-GTS
      *
           MOVE WS-GTS-CABECERA5           TO WS-GTS-DATOS
           PERFORM ADD-REGISTRO-COLA-GTS
      *
           .
      ******************************************************************
      * BUSCAR-EJECUTIVO
      ******************************************************************
       BUSCAR-EJECUTIVO.
      *
           INITIALIZE PEEC201A
           MOVE '0102'                     TO PEEC201A-PECDGENT
           MOVE SPACES                     TO PEEC201A-PENUMPER
           MOVE NUM-DOC-PERSONA(1:1)       TO PEEC201A-PETIPDOC
           MOVE '000'                      TO PEEC201A-PENUMDOC(1:3)
           MOVE NUM-DOC-PERSONA(8:8)       TO PEEC201A-PENUMDOC(4:8)
           MOVE '01'                       TO PEEC201A-PESECDOC
           MOVE '00'                       TO PEEC201A-IND-GRUPO
      *
           CALL CT-PE9C2010 USING WS-PEEC201A
      *
           EVALUATE PEEC201A-RETORNO
               WHEN '00'
      *            EXISTE
      *            OJO: TODO: EN ESPERA DE RESPUESTA A DUDA
      *            PARA SABER CUAL ES EL CAMPO A TOMAR
      *            COMO CODIGO DE EJECUTIVO
                   MOVE 1                  TO WS-COD-EJECUTIVO
                   PERFORM BUSCAR-DESC-EJE

               WHEN '20'
      *            NO EXISTE
                   MOVE '** PERSONA NO EXISTE **' TO WS-GTS-EJECUTIVO

               WHEN OTHER
                   MOVE PEEC201A-COD-ERROR     TO CAA-COD-ERROR
           END-EVALUATE
      *
           .
      ******************************************************************
      * BUSCAR-DESC-EJE
      ******************************************************************
       BUSCAR-DESC-EJE.
      *
           MOVE WS-COD-EJECUTIVO           TO CTBCDT03
      *
           EXEC SQL
               SELECT CTBDES03
               INTO :CTBDES03
               FROM BKAB0003_BASE
               WHERE CTBTDR03 = '2'
                 AND CTBNDT03 = '093'
                 AND CTBCDT03 = :Z-CTBCDT03
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN ZEROS
               WHEN -811
                   MOVE CTBDES03           TO WS-GTS-EJECUTIVO

               WHEN +100
                   MOVE SPACES             TO WS-GTS-EJECUTIVO

               WHEN OTHER
                   MOVE 'BKABBASE'         TO DB2-OBJETO
                   MOVE 'SELECT'           TO DB2-SENTENCIA
                   MOVE CT-PROGRAMA        TO DB2-PGRNAME
                   PERFORM 9999-ABEND-DB2
           END-EVALUATE
      *
           .
      ******************************************************************
      * BUSCAR-DIVISA
      ******************************************************************
       BUSCAR-DIVISA.
      *
           MOVE COD-DIV-OPE                TO CTBCDT03
      *
           EXEC SQL
               SELECT CTBCOD03
               INTO :CTBCOD03
               FROM BKAB0003_BASE
               WHERE CTBTDR03 = '2'
                 AND CTBNDT03 = '094'
                 AND CTBCDT03 = :Z-CTBCDT03
           END-EXEC
      *
           EVALUATE SQLCODE
               WHEN ZEROS
               WHEN -811
                   MOVE CTBCOD03           TO WS-GTS-DIVISA1
                                              WS-GTS-DIVISA2
               WHEN +100
                   MOVE SPACES             TO WS-GTS-DIVISA1
                                              WS-GTS-DIVISA2
               WHEN OTHER
                   MOVE 'BKABBASE'         TO DB2-OBJETO
                   MOVE 'SELECT'           TO DB2-SENTENCIA
                   MOVE CT-PROGRAMA        TO DB2-PGRNAME
                   PERFORM 9999-ABEND-DB2
           END-EVALUATE
      *
           .
      ******************************************************************
      * CERRAR-CURSOR
      ******************************************************************
       CERRAR-CURSOR.
      *
           EXEC SQL
               CLOSE V12C11Y-V1DC0011
           END-EXEC
      *
           MOVE SQLCODE                    TO DB2-RETURN-CDE
           IF  NOT DB2-OK
               INITIALIZE QGECABC
               MOVE CT-TAB-V1DT001         TO ABC-OBJETO-ERROR
               PERFORM 9999-ABEND-DB2
                  THRU 9999-ABEND-DB2-EXIT
           END-IF
      *
           .
      ******************************************************************
      * GRABAR-TOTALES
      ******************************************************************
       GRABAR-TOTALES.
      *
           MOVE WS-TOTAL-EFECT-COMPRA  TO WS-GTS-TOT-EFECT-COMPRA
           MOVE WS-TOTAL-EFECT-VENTA   TO WS-GTS-TOT-EFECT-VENTA
           MOVE WS-TOTAL-CHEQUE-COMPRA TO WS-GTS-TOT-CHEQUE-COMPRA
           MOVE WS-TOTAL-CHEQUE-VENTA  TO WS-GTS-TOT-CHEQUE-VENTA
           MOVE WS-TOTAL-TRAVEL-VENTA  TO WS-GTS-TOT-TRAVEL-VENTA
           MOVE WS-TOTAL-TRANSF-COMPRA TO WS-GTS-TOT-TRANSF-COMPRA
           MOVE WS-TOTAL-TRANSF-VENTA  TO WS-GTS-TOT-TRANSF-VENTA

           MOVE WS-GTS-TOTALES         TO WS-GTS-DATOS
           PERFORM ADD-REGISTRO-COLA-GTS
      *
           .
      ******************************************************************
      *                     3200-CONTIN-CASO2                          *
      ******************************************************************
       3200-CONTIN-CASO2.
      *
           MOVE 'V181'                     TO CAA-CODTRAN-SIG
           SET CAA-88-CADENA-ANADIR        TO TRUE
           SET CAA-88-ESTADO-INICIO        TO TRUE
           SET CAA-88-ACCION-PROGRAMA      TO TRUE
           MOVE CT-CASO1                   TO V173-CASO
      *
           .
       3200-CONTIN-CASO2-EXIT.
           EXIT.
      ******************************************************************
      * BORRAR-COLA-GTS
      ******************************************************************
       BORRAR-COLA-GTS.
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
      * ADD-REGISTRO-COLA-GTS
      ******************************************************************
       ADD-REGISTRO-COLA-GTS.
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
      ******************************************************************
      * LEER-REGISTRO-COLA-GTS
      ******************************************************************
       LEER-REGISTRO-COLA-GTS.
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
                   MOVE CT-ERROR-READQ       TO ABC-REFERENCIA
                   PERFORM 9999-ABEND-CICS
                      THRU 9999-ABEND-CICS-EXIT
           END-EVALUATE
      *
           .
      ******************************************************************
      * BORRAR-COLA-AUX
      ******************************************************************
       BORRAR-COLA-AUX.
      *
           EXEC CICS
               DELETEQ TS
               QUEUE(WS-COLA-AUX)
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
      * ADD-REGISTRO-COLA-AUX
      ******************************************************************
       ADD-REGISTRO-COLA-AUX.
      *
           EXEC CICS
               WRITEQ TS
               QUEUE(WS-COLA-AUX)
               FROM(WS-DATOS-COLA)
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
      ******************************************************************
      * LEER-REGISTRO-COLA-AUX
      ******************************************************************
       LEER-REGISTRO-COLA-AUX.
      *
           EXEC CICS
               READQ TS
               QUEUE(WS-COLA-AUX)
               INTO (WS-DATOS-COLA)
               ITEM(IN-REGISTRO)
               NOHANDLE
           END-EXEC
      *
           EVALUATE EIBRESP
               WHEN DFHRESP(NORMAL)
                   CONTINUE
               WHEN DFHRESP(ITEMERR)
                   CONTINUE
               WHEN OTHER
                   INITIALIZE QGECABC
                   MOVE CT-ERROR-READQ       TO ABC-REFERENCIA
                   PERFORM 9999-ABEND-CICS
                      THRU 9999-ABEND-CICS-EXIT
           END-EVALUATE
      *
           .
      ******************************************************************
      * REINPUT
      ******************************************************************
       REINPUT.
      *
           SET CAA-88-ACCION-TERMINAL TO TRUE
           SET CAA-88-ESTADO-CONTIN   TO TRUE
           INITIALIZE CAA-CADENA
           PERFORM 7000-FIN
              THRU 7000-FIN-EXIT
      *
           .
      ******************************************************************
      * 7000-FIN
      ******************************************************************
       7000-FIN.
      *
           EXEC CICS
               RETURN
           END-EXEC
      *
           .
       7000-FIN-EXIT.
           EXIT.
      ******************************************************************
      *            9999-ABEND-DB2                                      *
      *   LLAMADA A LA RUTINA DE ABEND PARA EL DB2                     *
      ******************************************************************
       9999-ABEND-DB2.
      *
           MOVE  'S'                       TO ABC-ABEND
           MOVE  CT-PROGRAMA               TO ABC-PROGRAMA
           MOVE  SQLCODE                   TO ABC-SQLCODE
           MOVE  SQLERRM                   TO ABC-SQLERRM
      *
           PERFORM 9999-LINK-ABEND
              THRU 9999-LINK-ABEND-EXIT
      *
           .
       9999-ABEND-DB2-EXIT.
           EXIT.
      ******************************************************************
      *                          9999-ABEND-CICS                       *
      *  SE ABENDA LA TAREA CUANDO SE PRODUCE UN ERROR CICS NO DESEADO *
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
      *
