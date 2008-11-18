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
