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
        SUBROUTINE PLOTSPECTRUM2(NPSPEC,WL_SPEC,FLUX_SPEC,
     +   EFLUX_SPEC,FLUX_RESID,SPECNAME,LPERR,CERR)
        IMPLICIT NONE
C
        INTEGER NPSPEC
        REAL WL_SPEC(NPSPEC),FLUX_SPEC(NPSPEC),EFLUX_SPEC(NPSPEC)
        REAL FLUX_RESID(NPSPEC)
        CHARACTER*80 SPECNAME
        LOGICAL LPERR
        CHARACTER*1 CERR
C
        INTEGER TRUEBEG
        INTEGER TRUELEN
C
        INTEGER I
        INTEGER L1,L2
        REAL XW1,XW2,YW1,YW2
        REAL YMIN,YMAX,DY
C------------------------------------------------------------------------------
        CALL PGBBUF
        CALL PGQWIN(XW1,XW2,YW1,YW2)
        CALL FINDMML(NPSPEC,1,NPSPEC,FLUX_SPEC,YMIN,YMAX)
        DY=YMAX-YMIN
        YMIN=YMIN-DY/20.
        YMAX=YMAX+DY/10.
        CALL PGSWIN(XW1,XW2,YMIN,YMAX)
        IF(LPERR.AND.(CERR.EQ.'y'))THEN
          CALL PGSCI(15)
          DO I=1,NPSPEC
            CALL PGMOVE(WL_SPEC(I),FLUX_SPEC(I)-EFLUX_SPEC(I))
            CALL PGDRAW(WL_SPEC(I),FLUX_SPEC(I)+EFLUX_SPEC(I))
          END DO
        END IF
        CALL PGSCI(1)
        CALL PGBOX(' ',0.0,0,'CTSM',0.0,0)
        CALL PGLINE(NPSPEC,WL_SPEC,FLUX_SPEC)
        CALL PGSCI(10)
        CALL PGMOVE(WL_SPEC(1),FLUX_RESID(1))
        IF(NPSPEC.GT.1)THEN
          DO I=2,NPSPEC
            CALL PGDRAW(WL_SPEC(I),FLUX_RESID(I))
          END DO
        END IF
        CALL PGSCI(11)
        CALL PGMOVE(WL_SPEC(1),EFLUX_SPEC(1))
        IF(NPSPEC.GT.1)THEN
          DO I=2,NPSPEC
            CALL PGDRAW(WL_SPEC(I),EFLUX_SPEC(I))
          END DO
        END IF
        CALL PGSCI(1)
        L1=TRUEBEG(SPECNAME)
        L2=TRUELEN(SPECNAME)
        CALL PGMTEXT('T',-1.5,0.98,1.0,SPECNAME(L1:L2))
        CALL PGSCI(10)
        CALL PGMTEXT('T',-3.0,0.98,1.0,'residual errors in simulations')
        CALL PGSCI(11)
        CALL PGMTEXT('T',-4.5,0.98,1.0,'errors in data spectrum')
        CALL PGSCI(1)
        CALL PGSWIN(XW1,XW2,YW1,YW2)
        CALL PGEBUF
C
        END
