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
C lee la transmision de la atmosfera
        SUBROUTINE LEEATM(NPATM,WL_ATM,FLUX_ATM)
        IMPLICIT NONE
C
        INCLUDE 'photometry.inc'
C
        INTEGER NPATM !no. of data points
        REAL WL_ATM(NPMAX),FLUX_ATM(NPMAX)
C
        INTEGER TRUEBEG
        INTEGER TRUELEN
C
        INTEGER L1,L2,LL1,LL2
        INTEGER I
C------------------------------------------------------------------------------
        L1=TRUEBEG(PHOTODIR)
        L2=TRUELEN(PHOTODIR)
        LL1=TRUEBEG(ATMFILE)
        LL2=TRUELEN(ATMFILE)
        WRITE(*,101) 'Reading file: '
        WRITE(*,101) PHOTODIR(L1:L2)//'/atmosphere/'//ATMFILE(LL1:LL2)
        OPEN(10,FILE=PHOTODIR(L1:L2)//'/atmosphere/'//ATMFILE(LL1:LL2),
     +   STATUS='OLD',FORM='FORMATTED')
C
        I=1
10      READ(10,*,END=20) WL_ATM(I),FLUX_ATM(I)
        WL_ATM(I)=WL_ATM(I)*10000.0
        I=I+1
        IF(I.GT.NPMAX)THEN
          WRITE(*,101) 'FATAL ERROR: file is too large. Redim NPMAX'
          STOP
        END IF
        GOTO 10
20      CLOSE(10)
        NPATM=I-1
C
        WRITE(*,100) '> No. of points it atmosphere transmission file: '
        WRITE(*,*) NPATM
        WRITE(*,*)
C
100     FORMAT(A,$)
101     FORMAT(A)
        END
