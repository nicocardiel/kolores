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
Comment
C
C SUBROUTINE BUTTSCF(FONT)
C
C Input : FONT
C
C Set the character font type in buttons.
C
C INTEGER FONT -> the current font number (in range 1-4)
C
Comment
C------------------------------------------------------------------------------
C
        SUBROUTINE BUTTSCF(FONT)
        IMPLICIT NONE
        INTEGER FONT
        INCLUDE 'button.inc'
C------------------------------------------------------------------------------
        IF((FONT.LT.1).OR.(FONT.GT.4))THEN
          WRITE(*,101)'ERROR: invalid font type in subroutine BUTTSCF.'
          WRITE(*,101)'=> previous font type unchanged'
          RETURN
        END IF
        PGSCF_BUTT=FONT
101     FORMAT(A)
        END
