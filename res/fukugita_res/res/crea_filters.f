	PROGRAM CREA_FILTERS
	IMPLICIT NONE
C
	INTEGER TRUELEN
C
	INTEGER I
	REAL WV,FILTER
	CHARACTER*80 LISTFILE
	CHARACTER*80 OUTFILE
	CHARACTER*80 RESFILE
C------------------------------------------------------------------------------
	LISTFILE='listado_filtros'
	OUTFILE='fukugita_res'
	OPEN(10,FILE=LISTFILE,STATUS='OLD',FORM='FORMATTED')
	OPEN(20,FILE=OUTFILE,STATUS='UNKNOWN',FORM='FORMATTED')
10	READ(10,101,END=20) RESFILE
	OPEN(30,FILE=RESFILE,STATUS='OLD',FORM='FORMATTED')
	I=0
11	READ(30,*,END=12)
	I=I+1
	GOTO 11
12	CLOSE(30)
	WRITE(20,'(4X,I5,2X,A)') I,RESFILE(1:TRUELEN(RESFILE))
	OPEN(30,FILE=RESFILE,STATUS='OLD',FORM='FORMATTED')
	I=0
13	READ(30,*,END=14) WV,FILTER
	I=I+1
	WRITE(20,*) I,WV,FILTER
	GOTO 13
14	CLOSE(30)
	GOTO 10
20	CLOSE(10)
	CLOSE(20)
C
	STOP
101	FORMAT(A)
	END
C
C******************************************************************************
C
	INTEGER FUNCTION TRUELEN(CADENA)
        IMPLICIT NONE
        CHARACTER*(*) CADENA
C
        INTEGER I,L
C------------------------------------------------------------------------------
        L=LEN(CADENA)
C
        DO I=L,1,-1
          IF(ICHAR(CADENA(I:I)).GT.32)THEN
            TRUELEN=I
            RETURN
          END IF
        END DO
        TRUELEN=0
        END
C
C******************************************************************************
C
	INTEGER FUNCTION TRUEBEG(CADENA)
        IMPLICIT NONE
        CHARACTER*(*) CADENA
C
        INTEGER I,L
C------------------------------------------------------------------------------
        L=LEN(CADENA)
C
        DO I=1,L
          IF(ICHAR(CADENA(I:I)).GT.32)THEN
            TRUEBEG=I
            RETURN
          END IF
        END DO
        TRUEBEG=0
        END
C
C******************************************************************************
C
        SUBROUTINE CHUPPER(CADENA)
        IMPLICIT NONE
        CHARACTER*(*) CADENA
C
        INTEGER I,N
C------------------------------------------------------------------------------
        DO I=1,LEN(CADENA)
          N=ICHAR(CADENA(I:I))
          IF((N.GE.97).AND.(N.LE.122)) CADENA(I:I)=CHAR(N-32)
        END DO
        END
