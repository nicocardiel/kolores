C------------------------------------------------------------------------------
C Copyright 2008-2019 Nicolas Cardiel
C
C This file is part of kolores.
C 
C Kolores is free software: you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published by
C the Free Software Foundation, either version 3 of the License, or
C (at your option) any later version.
C 
C Kolores is distributed in the hope that it will be useful,
C but WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C GNU General Public License for more details.
C 
C You should have received a copy of the GNU General Public License
C along with kolores. If not, see <http://www.gnu.org/licenses/>.
C------------------------------------------------------------------------------
C Subrutina para leer imagenes FITS.
C FITSFILE: nombre del fichero a leer
C LSHOW: if .TRUE. mostramos las keywords
C------------------------------------------------------------------------------
        SUBROUTINE SLEEFITS(FITSFILE,LSHOW)
        IMPLICIT NONE
        CHARACTER*(*) FITSFILE
        LOGICAL LSHOW
C parametros
        INCLUDE 'photometry.inc'
C funciones auxiliares
        INTEGER TRUELEN
C variables globales (COMMONs)
        INTEGER NAXIS(2)
        REAL IMAGEN_(NXMAX,NYMAX)
        REAL CRVAL1,CDELT1
        LOGICAL LNULL(NXMAX,NYMAX),ANYNULL
C variables locales
        INTEGER JROW(NXMAX)
        INTEGER I,J,L
        INTEGER FIRSTPIX
        INTEGER BITPIX
        INTEGER ISTATUS,IREADWRITE,IUNIT
        INTEGER BLOCKSIZE,NULLVAL
        INTEGER NKEYS,NSPACE,NFOUND
        INTEGER NAXIS_(0:2)                                !OJO: el limite es 2
        REAL FROW(NXMAX)
        DOUBLE PRECISION DROW(NXMAX)
        CHARACTER*50 COMMENT
        CHARACTER*80 CLINEA
        LOGICAL LOGFILE,LANYNULL
        LOGICAL LROW(NXMAX)
C
        COMMON/BLKIMAGEN_/IMAGEN_            !imagen FITS leida en formato REAL
        COMMON/BLKLNULL/LNULL,ANYNULL   !mascara que indica si existen NaN, etc
        COMMON/BLKNAXIS/NAXIS                                      !dimensiones
        COMMON/BLKSTWVDISP/CRVAL1,CDELT1
C------------------------------------------------------------------------------
C inicializamos variables
        ISTATUS=0               !controla posibles errores durante la ejecucion
        IREADWRITE=0                      !la imagen se abrira en modo READONLY
        NULLVAL=-999
        LANYNULL=.FALSE.
C------------------------------------------------------------------------------
C chequeamos si existe la imagen
        INQUIRE(FILE=FITSFILE,EXIST=LOGFILE)
        IF(LOGFILE)THEN
        ELSE
          WRITE(*,100) 'FATAL ERROR: the file "'
          WRITE(*,100) FITSFILE(1:TRUELEN(FITSFILE))
          WRITE(*,101) '" does not exist.'
          STOP
        END IF
C localizamos un numero de unidad de fichero no utilizada
ccc     CALL FTGIOU(IUNIT,ISTATUS)
        IUNIT=80 !ojo, IUNIT=99 entra en conflicto con el Postcript y es
                 !precisamente este numero el que toma esta funcion
C abrimos el fichero
        CALL FTOPEN(IUNIT,FITSFILE,IREADWRITE,BLOCKSIZE,ISTATUS)
C determinamos el numero de keywords en la cabecera y se muestran
        CALL FTGHSP(IUNIT,NKEYS,NSPACE,ISTATUS)
        IF(LSHOW)THEN
          DO I=1,NKEYS
            CALL FTGREC(IUNIT,I,CLINEA,ISTATUS)
            L=TRUELEN(CLINEA)
            WRITE(*,101) CLINEA(1:L)
          END DO
          IF(ISTATUS.EQ.0)THEN                                !todo ha ido bien
            WRITE(*,101) 'END'
            WRITE(*,*)
          END IF
        END IF
C leemos BITPIX
        CALL FTGKYJ(IUNIT,'BITPIX',BITPIX,COMMENT,ISTATUS)
C comprobamos que NAXIS=2
        CALL FTGKYJ(IUNIT,'NAXIS',NAXIS_(0),COMMENT,ISTATUS)
        IF(NAXIS_(0).GT.2)THEN
          WRITE(*,101) '***FATAL ERROR***'
          WRITE(*,100) '=> NAXIS='
          WRITE(*,*) NAXIS_(0)
          WRITE(*,101) '=> NAXIS > 2'
          CALL FTCLOS(IUNIT,ISTATUS)
          STOP
        ELSEIF(NAXIS_(0).EQ.1)THEN
          NAXIS_(2)=1
        END IF
C leemos NAXIS1 y NAXIS2 [notar que el quinto parametro es NAXIS(1) en lugar
C de NAXIS para asi recuperar NAXIS(1) y NAXIS(2)]
        CALL FTGKNJ(IUNIT,'NAXIS',1,2,NAXIS_(1),NFOUND,ISTATUS)
        IF(NAXIS_(1).GT.NXMAX)THEN
          WRITE(*,101) '* FATAL ERROR in subroutine SLEEFITS:'
          WRITE(*,101) 'NAXIS(1) > NXMAX'
          CALL FTCLOS(IUNIT,ISTATUS)
          STOP
        END IF
        IF(NAXIS_(2).GT.NYMAX)THEN
          WRITE(*,101) '* FATAL ERROR in subroutine SLEEFITS:'
          WRITE(*,101) 'NAXIS(2) > NYMAX'
          CALL FTCLOS(IUNIT,ISTATUS)
          STOP
        END IF
C leemos CRVAL1 y CDELT1
        CALL FTGKYE(IUNIT,'CRVAL1',CRVAL1,COMMENT,ISTATUS)
        CALL FTGKYE(IUNIT,'CDELT1',CDELT1,COMMENT,ISTATUS)
C leemos la imagen
        IF(BITPIX.EQ.16)THEN
          DO I=1,NAXIS_(2)
            FIRSTPIX=(I-1)*NAXIS_(1)+1
            CALL FTGPFJ(IUNIT,1,FIRSTPIX,NAXIS_(1),JROW(1),LROW(1),
     +       ANYNULL,ISTATUS)
            DO J=1,NAXIS_(1)
              IMAGEN_(J,I)=REAL(JROW(J))
            END DO
            IF(ANYNULL)THEN
              DO J=1,NAXIS_(1)
                LNULL(J,I)=LROW(J)
              END DO
              LANYNULL=.TRUE.
            END IF
          END DO
        ELSEIF(BITPIX.EQ.32)THEN
          DO I=1,NAXIS_(2)
            FIRSTPIX=(I-1)*NAXIS_(1)+1
            CALL FTGPFJ(IUNIT,1,FIRSTPIX,NAXIS_(1),JROW(1),LROW(1),
     +       ANYNULL,ISTATUS)
            DO J=1,NAXIS_(1)
              IMAGEN_(J,I)=REAL(JROW(J))
            END DO
            IF(ANYNULL)THEN
              DO J=1,NAXIS_(1)
                LNULL(J,I)=LROW(J)
              END DO
              LANYNULL=.TRUE.
            END IF
          END DO
        ELSEIF(BITPIX.EQ.-32)THEN
          DO I=1,NAXIS_(2)
            FIRSTPIX=(I-1)*NAXIS_(1)+1
            CALL FTGPFE(IUNIT,1,FIRSTPIX,NAXIS_(1),FROW(1),LROW(1),
     +       ANYNULL,ISTATUS)
            DO J=1,NAXIS_(1)
              IMAGEN_(J,I)=FROW(J)
            END DO
            IF(ANYNULL)THEN
              DO J=1,NAXIS_(1)
                LNULL(J,I)=LROW(J)
              END DO
              LANYNULL=.TRUE.
            END IF
          END DO
        ELSEIF(BITPIX.EQ.-64)THEN
          DO I=1,NAXIS_(2)
            FIRSTPIX=(I-1)*NAXIS_(1)+1
            CALL FTGPFD(IUNIT,1,FIRSTPIX,NAXIS_(1),DROW(1),LROW(1),
     +       ANYNULL,ISTATUS)
            DO J=1,NAXIS_(1)
              IMAGEN_(J,I)=REAL(DROW(J))
            END DO
            IF(ANYNULL)THEN
              DO J=1,NAXIS_(1)
                LNULL(J,I)=LROW(J)
              END DO
              LANYNULL=.TRUE.
            END IF
          END DO
        ELSE
          WRITE(*,100) 'FATAL ERROR in subroutine SLEEFITS: BITPIX ='
          WRITE(*,*) BITPIX
          CALL FTCLOS(IUNIT,ISTATUS)
          STOP
        END IF
C cerramos el fichero
        CALL FTCLOS(IUNIT,ISTATUS)
C liberamos el numero de unidad del fichero utilizado
ccc     CALL FTFIOU(IUNIT,ISTATUS)
C chequeamos si se ha producido algun error
        IF(ISTATUS.GT.0)THEN
          CALL PRINTERROR(ISTATUS)
        END IF
        ANYNULL=LANYNULL                  !basta que haya ocurrido una sola vez
        NAXIS(1)=NAXIS_(1)
        NAXIS(2)=NAXIS_(2)
C------------------------------------------------------------------------------
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
C
C******************************************************************************
C
        SUBROUTINE PRINTERROR(ISTATUS)
C Print out the FITSIO error messages to the user
        INTEGER ISTATUS
        CHARACTER ERRTEXT*30,ERRMESSAGE*80
C Check if status is OK (no error); if so, simply return
        IF(ISTATUS.LE.0) RETURN
C Get the text string which describes the error
        CALL FTGERR(ISTATUS,ERRTEXT)
        WRITE(*,'(A,$)') 'FITSIO Error Status = '
        WRITE(*,*) ISTATUS
        WRITE(*,'(A)') ERRTEXT
C Read and print out all the error messages on the FITSIO stack
        CALL FTGMSG(ERRMESSAGE)
        DO WHILE(ERRMESSAGE.NE.' ')
          WRITE(*,'(A)') ERRMESSAGE
          CALL FTGMSG(ERRMESSAGE)
        END DO
        END
