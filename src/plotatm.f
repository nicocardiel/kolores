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
        SUBROUTINE PLOTATM(NPATM,WL_ATM,FLUX_ATM)
        IMPLICIT NONE
C
        INTEGER NPATM
        REAL WL_ATM(NPATM),FLUX_ATM(NPATM)
C
        REAL XW1,XW2,YW1,YW2
        REAL YMIN,YMAX,DY
C------------------------------------------------------------------------------
        CALL PGBBUF
        CALL PGQWIN(XW1,XW2,YW1,YW2)
        CALL FINDMML(NPATM,1,NPATM,FLUX_ATM,YMIN,YMAX)
        DY=YMAX-YMIN
        YMIN=YMIN-DY/20.
        YMAX=YMAX+DY/10.
        CALL PGSWIN(XW1,XW2,YMIN,YMAX)
        CALL PGSCI(12)
        CALL PGLINE(NPATM,WL_ATM,FLUX_ATM)
        CALL PGSCI(1)
        CALL PGSWIN(XW1,XW2,YW1,YW2)
        CALL PGEBUF
C
        END
