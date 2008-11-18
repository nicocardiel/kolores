        SUBROUTINE PLOTATM(NPATM,WL_ATM,FLUX_ATM)
        IMPLICIT NONE
C
        INTEGER NPATM
        REAL WL_ATM(NPATM),FLUX_ATM(NPATM)
C
        REAL XW1,XW2,YW1,YW2
        REAL YMIN,YMAX,DY
C------------------------------------------------------------------------------
        CALL PGBBUF
        CALL PGQWIN(XW1,XW2,YW1,YW2)
        CALL FINDMML(NPATM,1,NPATM,FLUX_ATM,YMIN,YMAX)
        DY=YMAX-YMIN
        YMIN=YMIN-DY/20.
        YMAX=YMAX+DY/10.
        CALL PGSWIN(XW1,XW2,YMIN,YMAX)
        CALL PGSCI(12)
        CALL PGLINE(NPATM,WL_ATM,FLUX_ATM)
        CALL PGSCI(1)
        CALL PGSWIN(XW1,XW2,YW1,YW2)
        CALL PGEBUF
C
        END
