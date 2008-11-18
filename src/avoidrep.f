C******************************************************************************
C Toma una matriz ordenada XIN() y devuelve otra XOUT() en la que ha eliminado
C valores repetidos, y cuyos valores estan comprendidos entre XMIN y XMAX.
        SUBROUTINE AVOIDREP(NIN,XIN,NOUT,XOUT,XMIN,XMAX)
        IMPLICIT NONE
        INTEGER NIN
        REAL XIN(NIN)
        INTEGER NOUT
        REAL XOUT(NIN) !OJO: debe estar dimensionado a NIN
        REAL XMIN,XMAX
C
        INTEGER I
        REAL XMIN_,XMAX_
C------------------------------------------------------------------------------
C un poco de proteccion
        IF(XMIN.GT.XMAX)THEN
          STOP 'FATAL ERROR #1 in subroutine AVOIDREP'
        ELSEIF(XMIN.GT.XIN(NIN))THEN
          STOP 'FATAL ERROR #2 in subroutine AVOIDREP'
        ELSEIF(XMAX.LT.XIN(1))THEN
          STOP 'FATAL ERROR #3 in subroutine AVOIDREP'
        END IF
C
        IF(XMIN.LT.XIN(1))THEN
          XMIN_=XIN(1)
        ELSE
          XMIN_=XMIN
        END IF
        IF(XMAX.GT.XIN(NIN))THEN
          XMAX_=XIN(NIN)
        ELSE
          XMAX_=XMAX
        END IF
C buscamos el primer elemento
        I=1
        DO WHILE(XIN(I).LT.XMIN_)
          I=I+1
        END DO
        NOUT=1
        XOUT(NOUT)=XIN(I)
C seguimos hasta llegar al final
        DO WHILE((I.LT.NIN).AND.(XIN(I).LT.XMAX))
          I=I+1
          IF(XIN(I).NE.XIN(I-1))THEN !solo si son diferentes
            IF(XIN(I).LE.XMAX)THEN
              NOUT=NOUT+1
              XOUT(NOUT)=XIN(I)
            END IF
          END IF
        END DO
C
        END
