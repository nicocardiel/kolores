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
C expande los limites un cierto factor
        SUBROUTINE EXPAND(XMIN,XMAX,YMIN,YMAX,
     +   FACTORX1,FACTORX2,FACTORY1,FACTORY2)
        IMPLICIT NONE
C
        REAL XMIN,XMAX,YMIN,YMAX
        REAL FACTORX1,FACTORY1
        REAL FACTORX2,FACTORY2
C
        REAL DD
C------------------------------------------------------------------------------
        DD=XMAX-XMIN
        XMIN=XMIN-DD*FACTORX1
        XMAX=XMAX+DD*FACTORX2
        DD=YMAX-YMIN
        YMIN=YMIN-DD*FACTORY1
        YMAX=YMAX+DD*FACTORY2
C
        END
