      ***************************************************************** *
      *                                                               * *
      *                       *HA2C006C*                              * *
      *                        --------                               * *
      *                                                               * *
      *   ESTA TRANSACCION HA SIDO DISEÝADA PARA LA IMPRESI¢N DE LOS  * *
      *   CERTIFICADOS TRIBUTARIOS DE AÝOS ANTERIORES. CERTIFICADO DE * *
      *   RETEFUENTE Y TITULOS DE RENTA                               * *
      *                                                               * *
      *                                                               * *
      *   DESCRIPCION DE LA LINKAGE :                                 * *
      *                                                               * *
      *   NOMBRE          DESCRIPCION                                 * *
      *  --------         -------------                               * *
      *   QGECCAA         COMMAREA DE APLICACIONES                    * *
      *   HAECCOM1        COMMAREA DE CONTABILIDAD                    * *
      *   HANC200         COPYBOOK DE PANTALLA (HANC200)              * *
      *                                                               * *
      ***************************************************************** *
      *        L O G    D E   M O D I F I C A C I O N E S             * *
      ***************************************************************** *
      *  AUTOR   FECHA          DESCRIPCION                           * *
      ***************************************************************** *
       IDENTIFICATION DIVISION.
       PROGRAM-ID.   HA2C006C.
       AUTHOR. ALFREDO J PERTUZ MONTENEGRO.
       DATE-WRITTEN. 09/06/05.
         ENVIRONMENT DIVISION.
          DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-VARI
      *
      -    ABLES


           .
           05  WS-INDEX PIC 9(02).
           05  WS-PRUEBA-01                                            P
      -    I
      -
      -
      -    C X(100).

           05  WS-STR PIC X(100)
                                                         VALUE 'DFSDFSDF
      -                                                 'DSFSDFSDFSDFSDF
      -                                                 'DSF'.
      /
       LINKAGE SECTION.
       01  DFHCOMMAREA.
           COPY QGECCAA.
           COPY HAECCOM1.
           COPY HANC200.
       PROCEDURE DIVISION.

          PRUEBA SECTION.

           PERFORM
              MOVE WS-PRUEBA-01(1:WS-INDEX) TO WS-PRUEBA-01
           END-PERFORM


           PERFORM 1000-INICIO
              THRU 1000-INICIO

           PERFORM 2000-PROCESO.
      ****** DIRECCIONAMIENTO DE PUNTEROS E INICIO DE COPIES ******
       PARAGRAPH.
           MOVE WS-PRUEBA-01(1:WS-INDEX) TO WS-PRUEBA-01
           CONTINUE.

       1000-INICIO.
           MOVE WS-PRUEBA-01(1:WS-INDEX) TO WS-PRUEBA-01
           CONTINUE.

       PARAGRAPH.
           MOVE WS-PRUEBA-01(1:WS-INDEX) TO                          WS-
      -    PRUEBA-01
           CONTINUE.

      *
          2000-PROCESO.
           CONTINUE.
      *
