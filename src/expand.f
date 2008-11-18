C******************************************************************************
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
