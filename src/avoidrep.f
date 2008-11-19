C------------------------------------------------------------------------------
C Copyright 2008 Nicolas Cardiel
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
C Toma una matriz ordenada XIN() y devuelve otra XOUT() en la que ha eliminado
C valores repetidos, y cuyos valores estan comprendidos entre XMIN y XMAX.
        SUBROUTINE AVOIDREP(NIN,XIN,NOUT,XOUT,XMIN,XMAX)
        IMPLICIT NONE
        INTEGER NIN
        REAL XIN(NIN)
        INTEGER NOUT
        REAL XOUT(NIN) !OJO: debe estar dimensionado a NIN
        REAL XMIN,XMAX
C
        INTEGER I
        REAL XMIN_,XMAX_
C------------------------------------------------------------------------------
C un poco de proteccion
        IF(XMIN.GT.XMAX)THEN
          STOP 'FATAL ERROR #1 in subroutine AVOIDREP'
        ELSEIF(XMIN.GT.XIN(NIN))THEN
          STOP 'FATAL ERROR #2 in subroutine AVOIDREP'
        ELSEIF(XMAX.LT.XIN(1))THEN
          STOP 'FATAL ERROR #3 in subroutine AVOIDREP'
        END IF
C
        IF(XMIN.LT.XIN(1))THEN
          XMIN_=XIN(1)
        ELSE
          XMIN_=XMIN
        END IF
        IF(XMAX.GT.XIN(NIN))THEN
          XMAX_=XIN(NIN)
        ELSE
          XMAX_=XMAX
        END IF
C buscamos el primer elemento
        I=1
        DO WHILE(XIN(I).LT.XMIN_)
          I=I+1
        END DO
        NOUT=1
        XOUT(NOUT)=XIN(I)
C seguimos hasta llegar al final
        DO WHILE((I.LT.NIN).AND.(XIN(I).LT.XMAX))
          I=I+1
          IF(XIN(I).NE.XIN(I-1))THEN !solo si son diferentes
            IF(XIN(I).LE.XMAX)THEN
              NOUT=NOUT+1
              XOUT(NOUT)=XIN(I)
            END IF
          END IF
        END DO
C
        END
