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
C Plot Vega spectrum, and AB_nu=0 and ST_lambda=0 spectra
        SUBROUTINE PLOTVEGA(NPVEGA,WL_VEGA,FLUX_VEGA,EFLUX_VEGA,LPERR,
     +   LPABST0,CLABVEGA)
        IMPLICIT NONE
C
        INTEGER NPVEGA
        REAL WL_VEGA(NPVEGA),FLUX_VEGA(NPVEGA),EFLUX_VEGA(NPVEGA)
        LOGICAL LPERR,LPABST0
C
        INTEGER NPMAX
        PARAMETER (NPMAX=1000)
C
        INTEGER TRUEBEG
        INTEGER TRUELEN
C
        INTEGER I,L
        INTEGER L1,L2
        REAL XMIN,XMIN_,XMAX,YMIN,YMAX
        REAL XP(NPMAX),YP(NPMAX)
        CHARACTER*20 CLABVEGA
        CHARACTER*80 VEGAFILE
C
        COMMON/BLKVEGAFILE/VEGAFILE
C------------------------------------------------------------------------------
        CALL PGBBUF
        IF(LPERR)THEN
          CALL PGSCI(15)
          DO I=1,NPVEGA
            CALL PGMOVE(WL_VEGA(I),FLUX_VEGA(I)-EFLUX_VEGA(I))
            CALL PGDRAW(WL_VEGA(I),FLUX_VEGA(I)+EFLUX_VEGA(I))
          END DO
        END IF
        CALL PGSCI(8)
        CALL PGBOX(' ',0.0,0,'BTSN',0.0,0)
        CALL PGLINE(NPVEGA,WL_VEGA,FLUX_VEGA)
        IF(CLABVEGA.EQ.'\ga Lyr')THEN
          L=INDEX(VEGAFILE,'/')
          DO WHILE(INDEX(VEGAFILE(L+1:),'/').NE.0)
            L=L+INDEX(VEGAFILE(L+1:),'/')
          END DO
          CALL PGMTEXT('T',-1.5,0.02,0.0,'\ga Lyr: '//
     +     VEGAFILE(L+1:TRUELEN(VEGAFILE)))
        ELSE
          L1=TRUEBEG(CLABVEGA)
          L2=TRUELEN(CLABVEGA)
          CALL PGMTEXT('T',-1.5,0.02,0.0,CLABVEGA(L1:L2))
        END IF
        CALL PGMTEXT('L',2.5,0.5,0.5,
     +   'flux [erg s\u-1\d cm\u-2\d \A\u-1\d]')
C
        IF(LPABST0)THEN
          CALL PGQWIN(XMIN,XMAX,YMIN,YMAX)
C plot AB_nu=0
          IF(XMIN.LT.3000.)THEN
            XMIN_=3000.0
          ELSE
            XMIN_=XMIN
          END IF
          DO I=1,NPMAX
            XP(I)=XMIN_+REAL(I-1)/REAL(NPMAX-1)*(XMAX-XMIN)
            YP(I)=0.1088/(XP(I)*XP(I))
          END DO
          CALL PGSLS(2)
          CALL PGLINE(NPMAX,XP,YP)
C plot ST_lambda=0
          CALL PGSLS(4)
          CALL PGMOVE(XMIN,3.63E-9)
          CALL PGDRAW(XMAX,3.63E-9)
        END IF
C
        CALL PGSLS(1)
        CALL PGSCI(1)
        CALL PGMTEXT('B',3.0,0.5,0.5,'wavelength [\A]')
        CALL PGEBUF
C
        END
