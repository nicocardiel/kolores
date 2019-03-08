C------------------------------------------------------------------------------
C Copyright 2008 Nicolas Cardiel
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
C lee el espectro de Vega
        SUBROUTINE LEEVEGA(NPVEGA,WL_VEGA,FLUX_VEGA,EFLUX_VEGA,CLABVEGA)
        IMPLICIT NONE
C
        INCLUDE 'photometry.inc'
C
        INTEGER NPVEGA !no. of data points in the Vega spectrum
        REAL WL_VEGA(NPMAX),FLUX_VEGA(NPMAX),EFLUX_VEGA(NPMAX)
        CHARACTER*20 CLABVEGA
C
        INTEGER TRUEBEG
        INTEGER TRUELEN
        REAL LININTERP
        CHARACTER*255 READC
C
        INTEGER I
        INTEGER L1,L2
        INTEGER IFLAG,N1,N2
        REAL F0, FVEGA_AT_5556
        CHARACTER*1 COPC
        CHARACTER*80 VEGAFILE
C
        COMMON/BLKVEGAFILE/VEGAFILE
C------------------------------------------------------------------------------
        WRITE(*,101) '(1) '//VEGAFILE1(1:TRUELEN(VEGAFILE1))
        WRITE(*,101) '(2) '//VEGAFILE2(1:TRUELEN(VEGAFILE2))
        WRITE(*,101) '(3) '//VEGAFILE3(1:TRUELEN(VEGAFILE3))
        WRITE(*,101) '(4) '//VEGAFILE4(1:TRUELEN(VEGAFILE4))
        WRITE(*,101) '(a) AB_nu = -2.5 log F_nu -48.60'
        WRITE(*,101) '(z) ST_lambda = -2.5 log F_lambda -21.10'
        COPC(1:1)=READC('Option (1/2/3/4/a/z)','2','1234az')
C
        IF(COPC.EQ.'a')THEN
          NPVEGA=NPMAX
          DO I=1,NPMAX
            WL_VEGA(I)=1000.0+REAL(I-1)/REAL(NPMAX-1)*(30000.0-1000.0)
            FLUX_VEGA(I)=0.1088/(WL_VEGA(I)*WL_VEGA(I))
            EFLUX_VEGA(I)=0.
          END DO
          CLABVEGA='AB\d\gn\u'
        ELSEIF(COPC.EQ.'z')THEN
          NPVEGA=NPMAX
          DO I=1,NPMAX
            WL_VEGA(I)=1000.0+REAL(I-1)/REAL(NPMAX-1)*(30000.0-1000.0)
            FLUX_VEGA(I)=3.63E-9
            EFLUX_VEGA(I)=0.
          END DO
          CLABVEGA='ST\d\gl\u'
        ELSE
          CLABVEGA='\ga Lyr'
          L1=TRUEBEG(PHOTODIR)
          L2=TRUELEN(PHOTODIR)
          IF(COPC.EQ.'1')THEN
            OPEN(10,FILE=
     +       PHOTODIR(L1:L2)//'/'//
     +       VEGAFILE1,STATUS='OLD',FORM='FORMATTED')
            VEGAFILE=VEGAFILE1
          ELSEIF(COPC.EQ.'2')THEN
            OPEN(10,FILE=
     +       PHOTODIR(L1:L2)//'/'//
     +       VEGAFILE2,STATUS='OLD',FORM='FORMATTED')
            VEGAFILE=VEGAFILE2
          ELSEIF(COPC.EQ.'3')THEN
            OPEN(10,FILE=
     +       PHOTODIR(L1:L2)//'/'//
     +       VEGAFILE3,STATUS='OLD',FORM='FORMATTED')
            VEGAFILE=VEGAFILE3
          ELSEIF(COPC.EQ.'4')THEN
            OPEN(10,FILE=
     +       PHOTODIR(L1:L2)//'/'//
     +       VEGAFILE4,STATUS='OLD',FORM='FORMATTED')
            VEGAFILE=VEGAFILE4
          END IF
          I=1
10        IF(COPC.EQ.'1')THEN
            READ(10,*,END=20) WL_VEGA(I),FLUX_VEGA(I),EFLUX_VEGA(I)
          ELSE
            READ(10,*,END=20) WL_VEGA(I),FLUX_VEGA(I)
            EFLUX_VEGA(I)=0.
            IF(COPC.EQ.'3')THEN
              FLUX_VEGA(I)=10.0**(-0.4*FLUX_VEGA(I))
            END IF
          END IF
          I=I+1
          GOTO 10
20        CLOSE(10)
          NPVEGA=I-1
C normalizamos el flujo en 5556 A usando el zeropoint de Hayes 1985:
C F_lambda= 3.44 E-9 erg cm^-2 s^-1 A^-1
          FVEGA_AT_5556=LININTERP(NPVEGA,WL_VEGA,FLUX_VEGA,5556.,
     +     IFLAG,N1,N2)
          F0=3.44E-9/FVEGA_AT_5556
          WRITE(*,100) 'Flux of Vega at 5556 Angs....: '
          WRITE(*,*) FVEGA_AT_5556
          WRITE(*,100) 'Normalization factor for Vega: '
          WRITE(*,*) F0
          DO I=1,NPVEGA
            FLUX_VEGA(I)=FLUX_VEGA(I)*F0
          END DO
        END IF
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
