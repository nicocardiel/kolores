        SUBROUTINE PLOTFILTER(NPFILT,WL_FILT,FLUX_FILT,
     +   FILTERNAME,NFILTER)
        IMPLICIT NONE
C
        INCLUDE 'photometry.inc'
C
        INTEGER NPFILT(NFILTROS)
        REAL WL_FILT(NPMAX,NFILTROS),FLUX_FILT(NPMAX,NFILTROS)
        CHARACTER*80 FILTERNAME(NFILTROS)
        INTEGER NFILTER(NFILTROS)
C
        INTEGER TRUEBEG,TRUELEN
C
        INTEGER NF
        INTEGER NCOLOR(NFILTROS)
        INTEGER L,L1,L2
        REAL XW1,XW2,YW1,YW2
        REAL YMIN(NFILTROS),YMAX(NFILTROS),YMIN_,YMAX_
        REAL DY
        REAL XOFF(NFILTROS),XCEN(NFILTROS)
        CHARACTER*1 CLOC(NFILTROS)
        CHARACTER*50 CDUMMY
        LOGICAL LANY
C------------------------------------------------------------------------------
        DATA (NCOLOR(NF),CLOC(NF),XOFF(NF),XCEN(NF),NF=1,NFILTROS) /
     +   4,'T',3.0,0.0,
     +   3,'T',2.0,0.0,
     +   2,'T',1.0,0.0,
     +   5,'T',3.0,1.0,
     +   7,'T',2.0,1.0,
     +   6,'T',1.0,1.0/
C------------------------------------------------------------------------------
        DO NF=1,NFILTROS
          IF(NPFILT(NF).GT.0) LANY=.TRUE.
        END DO
        IF(.NOT.LANY) RETURN
C------------------------------------------------------------------------------
        CALL PGQWIN(XW1,XW2,YW1,YW2)
C dibujamos todos los filtros normalizados a la misma escala
        DO NF=1,NFILTROS
          YMIN(NF)=0
          YMAX(NF)=0
          IF(NPFILT(NF).GT.0)
     +     CALL FINDMML(NPFILT(NF),1,NPFILT(NF),FLUX_FILT(1,NF),
     +     YMIN(NF),YMAX(NF))
        END DO
        DO NF=1,NFILTROS
          IF(NPFILT(NF).GT.0)THEN
            DY=YMAX(NF)-YMIN(NF)
            YMIN_=YMIN(NF)-DY/20.
            YMAX_=YMAX(NF)+DY/10.
            CALL PGSWIN(XW1,XW2,YMIN_,YMAX_)
            CALL PGSCI(NCOLOR(NF))
            CALL PGLINE(NPFILT(NF),WL_FILT(1,NF),FLUX_FILT(1,NF))
            L1=TRUEBEG(FILTERNAME(NF))
            L2=TRUELEN(FILTERNAME(NF))
            WRITE(CDUMMY,'(A1,I8,A1)') '(',NFILTER(NF),')'
            CALL RMBLANK(CDUMMY,CDUMMY,L)
            CALL PGMTEXT(CLOC(NF),XOFF(NF),XCEN(NF),XCEN(NF),
     +       FILTERNAME(NF)(L1:L2)//' '//CDUMMY(1:L))
          END IF
        END DO
        CALL PGSCI(1)
        CALL PGSWIN(XW1,XW2,YW1,YW2)
C
        END
