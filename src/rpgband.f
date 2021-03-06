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
C SUBROUTINE RPGBAND(MODE,POSN,XREF,YREF,XC,YC,CH)
C
C Input: MODE,POSN,XREF,YREF
C Output: XC,YC,CH
C
C This routine is similar to PGBAND, but it also allows the utilization of
C buttons in text mode.
C
C INTEGER     MODE -> display mode (see PGPLOT manual)
C INTEGER     POSN -> if POSN=1, the routine positions the cursor at the
C             position specified by XREF,YREF
C REAL        XREF -> reference position
C REAL        YREF -> reference position
C REAL        XC -> the world x-coordinate of the cursor
C REAL        YC -> the world y-coordinate of the cursor
C CHARACTER*1 CH -> the character typed by the user
C
Comment
C------------------------------------------------------------------------------
        SUBROUTINE RPGBAND(MODE,POSN,XREF,YREF,XC,YC,CH)
        IMPLICIT NONE
        INTEGER MODE,POSN
        REAL XREF,YREF,XC,YC
        CHARACTER*1 CH
C
C Comando identico a PGCURSE de PGPLOT pero que en caso de utilizar botones en
C modo texto permite la entrada de los parametros de la subrutina desde
C teclado.
C------------------------------------------------------------------------------
        INCLUDE 'button.inc'
        REAL READF
        CHARACTER*255 READC
C------------------------------------------------------------------------------
C Variables locales
        INTEGER PGBAND,IDUM
        CHARACTER*1 CBUTTON,CPGPAGE
C------------------------------------------------------------------------------
C------------------------------------------------------------------------------
        IF(MODOTEXT_BUTT)THEN
          CBUTTON(1:1)=READC('Are you pressing a button (y/n/#)',
     +     'y','yn#')
          IF(CBUTTON.EQ.'y')THEN
            XC=0.
            YC=0.
            CH='A'         !caracter devuelto por el boton izquierdo del mouse
          ELSEIF(CBUTTON.EQ.'n')THEN
            XC=READF('XC','@')
            YC=READF('YC','@')
            CH(1:1)=READC('CH','A','@')
          ELSEIF(CBUTTON.EQ.'#')THEN
            XC=0.
            YC=0.
            CH='A'         !caracter devuelto por el boton izquierdo del mouse
            CPGPAGE(1:1)=
     +       READC('Do you want to insert a CALL PGPAGE (y/n)',
     +       'y','yn')
            IF(CPGPAGE.EQ.'y')CALL PGPAGE
          END IF
        ELSE
          IDUM=PGBAND(MODE,POSN,XREF,YREF,XC,YC,CH)
          IF(IDUM.EQ.0)THEN
            WRITE(*,101)'ERROR: in subroutine PGBAND.'
            WRITE(*,101)'=> returned XC=YC=0.0, CH="X"'
            XC=0.
            YC=0.
            CH='X'
          END IF
        END IF
C------------------------------------------------------------------------------
101     FORMAT(A)
        END
