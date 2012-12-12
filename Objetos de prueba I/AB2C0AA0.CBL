      *  AB2C0AA0: PROGRAMA PARA EL SUBMENU DE POSICION DEL CLIENTE    *
      ******************************************************************
      **                 IDENTIFICATION DIVISION                       *
      ******************************************************************
      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID.   AB2C0AA0.
       SECURITY.
         AUTHOR. SDASDAS
        dsfljsdljfsdf
        dsfsdfs.
         DATE-WRITTEN.
      ******************************************************************
      *                     ENVIRONMENT DIVISION                       *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      ******************************************************************
      *                        DATA DIVISION                           *
      ******************************************************************
      *
       DATA DIVISION.
      ******************************************************************
      *                  WORKING-STORAGE SECTION                       *
      ******************************************************************
      *
       WORKING-STORAGE SECTION.
       01  WS-VARIABLES.
           05  WS-PRUEBA PIC X(10).
           05  WS-I      PIC 9(01).
      ******************************************************************
      *                     LINKAGE SECTION                            *
      ******************************************************************
        LINKAGE SECTION.
      ******************************************************************
      *                   PROCEDURE DIVISION                           *
      ******************************************************************
       PROCEDURE DIVISION.

           PERFORM 100-INICIO-PROGRAMA

           PERFORM 200-PROCESO-PROGRAMA

           GOBACK
           .
      ******************************************************************
      *                    100-INICIO-PROGRAMA                         *
      ******************************************************************
       100-INICIO-PROGRAMA.
           MOVE WS-PRUEBA(WS-I:1) TO WS-PRUEBA
           .
      ******************************************************************
      *                    200-PROCESO-PROGRAMA                        *
      ******************************************************************
       200-PROCESO-PROGRAMA.
           CONTINUE
           .
      ******************************************************************
      *                       300-FIN-PROGRAMA                         *
      ******************************************************************
       300-FIN-PROGRAMA.

           EXEC CICS
                RETURN
           END-EXEC
           .
