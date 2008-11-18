C******************************************************************************
C lee la transmision de la atmosfera
        SUBROUTINE LEEATM(NPATM,WL_ATM,FLUX_ATM)
        IMPLICIT NONE
C
        INCLUDE 'photometry.inc'
C
        INTEGER NPATM !no. of data points
        REAL WL_ATM(NPMAX),FLUX_ATM(NPMAX)
C
        INTEGER I
C------------------------------------------------------------------------------
        OPEN(10,FILE=ATMFILE,STATUS='OLD',FORM='FORMATTED')
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
C
100     FORMAT(A,$)
101     FORMAT(A)
        END
