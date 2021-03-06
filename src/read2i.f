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
        SUBROUTINE READ2I(CQUESTION,CDEF,N1,N2)
        IMPLICIT NONE
        CHARACTER*(*) CQUESTION
        INTEGER N1,N2
        CHARACTER*(*) CDEF
C
        INTEGER I,L1,L2
        INTEGER TRUEBEG,TRUELEN
        INTEGER NERR
        CHARACTER*1 C
        CHARACTER*255 CADENA
        LOGICAL LECHO
        COMMON/BLKLECHO/LECHO
C------------------------------------------------------------------------------
        NERR=0
10      L2=TRUELEN(CQUESTION)
        IF(L2.NE.0) WRITE(*,100) CQUESTION(1:L2)
        IF(CDEF.NE.'@')THEN
          L1=TRUEBEG(CDEF)
          IF(L1.NE.0)THEN
            L2=TRUELEN(CDEF)
            WRITE(*,100) ' ['
            WRITE(*,100) CDEF(L1:L2)
            WRITE(*,100) '] ? '
          END IF
        ELSE
          WRITE(*,100) '? '
        END IF
        READ(*,'(A)',ERR=20) CADENA
        IF(TRUELEN(CADENA).EQ.0)THEN
          IF(CDEF.EQ.'@')THEN
            GOTO 10
          END IF
          CADENA=CDEF
        END IF
        DO I=1,TRUELEN(CADENA)
          C=CADENA(I:I)
          IF((INDEX('abcdefghijklmnopqrstuvwxyz',C).NE.0).OR.
     +     (INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ./',C).NE.0))THEN
            GOTO 20
          END IF
        END DO
        READ(CADENA,*,ERR=20) N1,N2
        IF(LECHO) WRITE(*,*) N1, N2
        RETURN
20      WRITE(*,101) 'ERROR: invalid character(s) found in '//
     +   'numbers. Try again.'
        NERR=NERR+1
        IF(NERR.GT.10) STOP 'FATAL ERROR: too many errors in READ2I.'
        GOTO 10
100     FORMAT(A,$)
101     FORMAT(A)
        END
