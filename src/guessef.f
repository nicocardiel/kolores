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
C SUBROUTINE GUESSEF(INFILE,OUTFILE)
C
C Input: INFILE
C Ouput: OUTFILE
C
C This subroutine determines the expected error file name OUTFILE from INFILE.
C An additional "e" character is located between the portion of the file name
C (INFILE) preceding the last period (if present) and the last period itself.
C
C CHARACTER*(*) INFILE
C CHARACTER*(*) OUTFILE
C
Comment
C------------------------------------------------------------------------------
        SUBROUTINE GUESSEF(INFILE,OUTFILE)
        IMPLICIT NONE
        CHARACTER*(*) INFILE
        CHARACTER*(*) OUTFILE
C
        INTEGER TRUELEN
C
        INTEGER LIN,LOUT,I
C------------------------------------------------------------------------------
        LIN=TRUELEN(INFILE)
        LOUT=LEN(OUTFILE)
        IF(LIN.GT.LOUT-1) LIN=LOUT-1
        IF(LIN.GT.255) LIN=254
C
        DO I=1,LOUT
          OUTFILE(I:I)=' '
        END DO
C
        IF(INDEX(INFILE(1:LIN),'.').EQ.0)THEN
          OUTFILE(1:LIN)=INFILE(1:LIN)
          OUTFILE(LIN+1:LIN+1)='e'
          RETURN
        ENDIF
C
        DO I=LIN,1,-1
          IF(INFILE(I:I).EQ.'.')GOTO 10
          IF(INFILE(I:I).EQ.'/')THEN     !si hay barra de directorio, escapamos
            OUTFILE(1:LIN)=INFILE(1:LIN)
            OUTFILE(LIN+1:LIN+1)='e'
            RETURN
          END IF
        END DO
10      OUTFILE(1:I-1)=INFILE(1:I-1)
        OUTFILE(I:I)='e'
        OUTFILE(I+1:LIN+1)=INFILE(I:LIN)
C
        END
