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
Comment
C
C SUBROUTINE BUTTSIT(LOUTSIDE)
C
C Input: LOUTSIDE
C
C Set whether tick marks are drawn outside the viewport instead of inside.
C
C LOGICAL LOUTSIDE -> .TRUE. if ticks are drawn outside the viewport
C
Comment
C------------------------------------------------------------------------------
C
        SUBROUTINE BUTTSIT(LOUTSIDE)
        IMPLICIT NONE
        LOGICAL LOUTSIDE
        INCLUDE 'button.inc'
C------------------------------------------------------------------------------
        ITICKS_BUTT=LOUTSIDE
        END
