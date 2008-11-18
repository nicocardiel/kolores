	PROGRAM MERGE
	IMPLICIT NONE
C
	INTEGER NMAX
	PARAMETER (NMAX=896)
C
	REAL LININTERP
C
	INTEGER I
	INTEGER IFLAG,IDUM1,IDUM2
	REAL W1(NMAX),W2(NMAX),W3(NMAX),W4(NMAX)
	REAL F1(NMAX),F2(NMAX),F3(NMAX),F4(NMAX),F1_,F4_
C------------------------------------------------------------------------------
	OPEN(10,FILE='CWW_E_ext.sed',STATUS='OLD',FORM='FORMATTED')
	DO I=1,867
	  READ(10,*) W1(I),F1(I)
	END DO	
	CLOSE(10)
	OPEN(10,FILE='CWW_Sbc_ext.sed',STATUS='OLD',FORM='FORMATTED')
	DO I=1,896
	  READ(10,*) W2(I),F2(I)
	END DO	
	CLOSE(10)
	OPEN(10,FILE='CWW_Scd_ext.sed',STATUS='OLD',FORM='FORMATTED')
	DO I=1,896
	  READ(10,*) W3(I),F3(I)
	END DO	
	CLOSE(10)
	OPEN(10,FILE='CWW_Im_ext.sed',STATUS='OLD',FORM='FORMATTED')
	DO I=1,867
	  READ(10,*) W4(I),F4(I)
	END DO	
	CLOSE(10)
C------------------------------------------------------------------------------
	OPEN(10,FILE='CWW_all_ext.sed',STATUS='UNKNOWN',
     +   FORM='FORMATTED')
	DO I=1,896
	  F1_=LININTERP(867,W1,F1,W3(I),IFLAG,IDUM1,IDUM2)
	  F4_=LININTERP(867,W4,F4,W3(I),IFLAG,IDUM1,IDUM2)
	  WRITE(10,*) W3(I),F1_,F2(I),F3(I),F4_
	END DO
	CLOSE(10)
C------------------------------------------------------------------------------
	STOP
	END
C------------------------------------------------------------------------------
C Version 31-January-2001                                     File: lininterp.f
C------------------------------------------------------------------------------
C Copyright N. Cardiel & J. Gorgas, Departamento de Astrofisica
C Universidad Complutense de Madrid, 28040-Madrid, Spain
C E-mail: ncl@astrax.fis.ucm.es or fjg@astrax.fis.ucm.es
C------------------------------------------------------------------------------
C This routine is free software; you can redistribute it and/or modify it
C under the terms of the GNU General Public License as published by the Free
C Software Foundation; either version 2 of the License, or (at your option) any
C later version. See the file gnu-public-license.txt for details.
C------------------------------------------------------------------------------
Comment
C
C REAL FUNCTION LININTERP(N,X,Y,X0,IFLAG)
C
C Input: N,X,Y,X0
C Output: LININTERP(function), IFLAG
C
C Performs a linear interpolation in the table X(N),Y(N) at x=X0. Note that the
C X matrix must be sorted in ascending order, although the 
C
C INTEGER N -> input number of data in X and Y
C REAL    X(N) -> data matrix 
C REAL    Y(N) -> data matrix 
C REAL    X0 -> x-point at which the linear interpolation is evaluated
C INTEGER IFLAG -> = 0 : interpolation
C                  = -1 : extrapolation towards lower X values
C                  = +1 : extrapolation towards higher X values
C                  = +9 : error (division by zero)
C
Comment
C------------------------------------------------------------------------------
        REAL FUNCTION LININTERP(N,X,Y,X0,IFLAG,N1,N2)
        IMPLICIT NONE
C       
        INTEGER N
        REAL X(N),Y(N),X0
        INTEGER IFLAG
        INTEGER N1,N2
C local variables
        INTEGER I
C------------------------------------------------------------------------------
        IF(X0.EQ.X(N))THEN !extremo superior (da division por cero abajo)
          IFLAG=0
          LININTERP=Y(N)
          RETURN
        ELSEIF(X0.LT.X(1))THEN !extrapolacion a la izquierda
          IFLAG=-1
          N1=1
          N2=2
        ELSEIF(X0.GT.X(N))THEN !extrapolacion a la derecha
          IFLAG=1
          N1=N-1
          N2=N
        ELSE !caso general
          IFLAG=0
	  N1=N/2 !como inicio de busqueda tomamos el centro de la tabla
          CALL BINSEARCH(X,N,X0,N1)
          N2=N1+1
        END IF
C       
        IF(X(N1).NE.X(N2))THEN
          LININTERP=Y(N1)+((X0-X(N1))/(X(N2)-X(N1)))*(Y(N2)-Y(N1))
        ELSE
          IFLAG=9
          LININTERP=0.
        END IF
C       
        END
C------------------------------------------------------------------------------
C Version 16-June-1998                                         File:binsearch.f
C------------------------------------------------------------------------------
C Copyright N. Cardiel & J. Gorgas, Departamento de Astrofisica
C Universidad Complutense de Madrid, 28040-Madrid, Spain
C E-mail: ncl@astrax.fis.ucm.es or fjg@astrax.fis.ucm.es
C------------------------------------------------------------------------------
C This routine is free software; you can redistribute it and/or modify it
C under the terms of the GNU General Public License as published by the Free
C Software Foundation; either version 2 of the License, or (at your option) any
C later version. See the file gnu-public-license.txt for details.
C------------------------------------------------------------------------------
Comment
C
C SUBROUTINE BINSEARCH(X,N,X0,N0)
C
C Input: X,N,X0,N0
C Output: N0
C
C Given the array X(N), and the test value X0, this subroutine returns an
C integer N0, such that X0 is between X(N0) and X(N0+1). As input N0 is
C employed to start the searching. If X0.LT.X(1) then N0=0 on output, whereas 
C if X0.GT.X(N) then N0=N.
C
C REAL    X(N) -> ordered input array (not necesarilly equally-spaced)
C INTEGER N -> no. of points in input array
C REAL    X0 -> argument to be searched for
C INTEGER N0 -> location of X0 in the input array
C
Comment
C------------------------------------------------------------------------------
	SUBROUTINE BINSEARCH(X,N,X0,N0)
	IMPLICIT NONE
C
	INTEGER N,N0
	REAL X(N),X0
C local variables
	INTEGER L,U,I
	INTEGER STEP
	LOGICAL LOOP
C------------------------------------------------------------------------------
	IF(N0.LT.1)THEN
ccc	  WRITE(*,100)'* WARNING: in subroutine BINSEARCH: '
ccc	  WRITE(*,101)'N0.LT.1'
	  N0=1
	END IF
	IF(N0.GT.N)THEN
ccc	  WRITE(*,100)'* WARNING: in subroutine BINSEARCH: '
ccc	  WRITE(*,101)'N0.GT.N'
	  N0=N
	END IF
C------------------------------------------------------------------------------
C Buscamos el intervalo inicial duplicando el paso de busqueda
	STEP=1
	L=N0
	LOOP=.TRUE.
c..............................................................................
	IF((X(1).LT.X(N)).EQV.(X0.GE.X(L)))THEN
	  DO WHILE(LOOP)
	    U=L+STEP
	    IF(U.GT.N)THEN
	      U=N+1
	      LOOP=.FALSE.
	    ELSE
	      IF((X(1).LT.X(N)).EQV.(X0.GE.X(U)))THEN
	        L=U
	        STEP=2*STEP
	      ELSE
	        LOOP=.FALSE.
	      END IF
	    END IF
	  END DO
c..............................................................................
	ELSE
	  U=L
	  DO WHILE(LOOP)
	    L=U-STEP
	    IF(L.LT.1)THEN
	      L=0
	      LOOP=.FALSE.
	    ELSE
	      IF((X(1).LT.X(N)).EQV.(X0.LT.X(L)))THEN
	        U=L
	        STEP=2*STEP
	      ELSE
	        LOOP=.FALSE.
	      END IF
	    END IF
	  END DO
c..............................................................................
	END IF
C------------------------------------------------------------------------------
C Ahora buscamos el valor de N0 dividiendo el paso de busqueda
	DO WHILE(U-L.GT.1)
	  I=(U+L)/2
	  IF(X0.EQ.X(I))THEN
	    N0=I
	    RETURN
	  END IF
	  IF((X(1).LT.X(N)).EQV.(X0.GT.X(I)))THEN
	    L=I
	  ELSE
	    U=I
	  END IF
	END DO
C
	IF(U.LT.L)THEN
	  WRITE(*,101) 'FATAL ERROR: in subroutine BINSEARCH.'
	  STOP
	END IF
C
	N0=L
C
100	FORMAT(A,$)
101	FORMAT(A)
	END
