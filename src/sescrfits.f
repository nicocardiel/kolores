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
C Subrutina para escribir una image FITS
        SUBROUTINE SESCRFITS
        IMPLICIT NONE
C
        INCLUDE 'photometry.inc'
C
        INTEGER TRUELEN
        CHARACTER*255 READC
C
        INTEGER L
        INTEGER BITPIX,NAXIS(2)
        INTEGER ISTATUS,IUNIT
        INTEGER BLOCKSIZE,NULLVAL
        REAL IMAGEN_(NXMAX,NYMAX)
        CHARACTER*255 INFILE
        LOGICAL LOGFILE
        LOGICAL SIMPLE,EXTEND
C
        COMMON/BLKIMAGEN_/IMAGEN_
        COMMON/BLKNAXIS/NAXIS
C------------------------------------------------------------------------------
        ISTATUS=0               !controla posibles errores durante la ejecucion
        NULLVAL=-999
        BLOCKSIZE=1           !normalmente usaremos un valor de 1 (=2880 bytes)
C------------------------------------------------------------------------------
C pedimos nombre de la imagen
        LOGFILE=.TRUE.
        DO WHILE(LOGFILE)
          INFILE=READC('Output FITS file name','@','@')
          L=TRUELEN(INFILE)
          INQUIRE(FILE=INFILE(1:L),EXIST=LOGFILE)
          IF(LOGFILE)THEN
            WRITE(*,100) 'ERROR: the file '
            WRITE(*,100) INFILE(1:L)
            WRITE(*,101) ' already exists. Try again.'
            WRITE(*,100) 'Press <CR> to continue...'
            READ(*,*)
          END IF
        END DO
        WRITE(*,100) 'Saving file...'
C localizamos un numero de unidad de fichero no utilizada
ccc     CALL FTGIOU(IUNIT,ISTATUS)
        IUNIT=82 !ojo, IUNIT=99 entra en conflicto con el Postcript y es
                 !precisamente este numero el que toma esta funcion
C creamos un nuevo fichero FITS vacio
        CALL FTINIT(IUNIT,INFILE,BLOCKSIZE,ISTATUS)
C inicializamos los parametros
        SIMPLE=.TRUE.
        BITPIX=-32
        EXTEND=.FALSE.
C escribimos los keywords indispensables
        CALL FTPHPR(IUNIT,SIMPLE,BITPIX,2,NAXIS(1),0,1,
     +   EXTEND,ISTATUS)
        CALL FTP2DE(IUNIT,1,NXMAX,NAXIS(1),NAXIS(2),IMAGEN_,ISTATUS)
C cerramos el fichero
        CALL FTCLOS(IUNIT,ISTATUS)
C liberamos el numero de unidad del fichero utilizado
ccc     CALL FTFIOU(IUNIT,ISTATUS)
C chequeamos si se ha producido algun error
        IF(ISTATUS.GT.0)THEN
          WRITE(*,*)
          CALL PRINTERROR(ISTATUS)
        ELSE
          WRITE(*,101) 'OK!'
        END IF
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
