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
C Lee el filtro numero NFILTER, retornado NPFILT,WL_FILT, FLUX_FILT y el
C nombre del filtro FILTERNAME.
C La subrutina tambien normaliza la respuesta del filtro al valor en el maximo
        SUBROUTINE LEEFILTER(WLMIN,WLMAX,NFILTER,
     +   NPFILT,WL_FILT,FLUX_FILT,FILTERNAME)
        IMPLICIT NONE
C
        INCLUDE 'photometry.inc'
        REAL WLMIN,WLMAX
        INTEGER NFILTER
        INTEGER NPFILT
        REAL WL_FILT(NPMAX),FLUX_FILT(NPMAX)
        CHARACTER*80 FILTERNAME
C
        INTEGER TRUEBEG,TRUELEN
        INTEGER SYSTEMFUNCTION
        CHARACTER*255 READC
C
        INTEGER I,K,L1,L2
        INTEGER LD1,LD2
        INTEGER IDUM
        INTEGER ISYSTEM
        REAL FMAX
        CHARACTER*1 CNOR
        CHARACTER*4 CDUMMY
C------------------------------------------------------------------------------
        LD1=TRUEBEG(PHOTODIR)
        LD2=TRUELEN(PHOTODIR)
C filtros "normales"
        IF(NFILTER.LE.1000)THEN
          IF(NFILTER.LE.100)THEN
            OPEN(10,FILE=
     +       PHOTODIR(LD1:LD2)//'/'//
     +       FILTERFILE1,STATUS='OLD',FORM='FORMATTED')
            K=0
          ELSEIF(NFILTER.LE.500)THEN
            OPEN(10,FILE=
     +       PHOTODIR(LD1:LD2)//'/'//
     +       FILTERFILE3,STATUS='OLD',FORM='FORMATTED')
            K=100
          ELSEIF(NFILTER.LE.700)THEN
            OPEN(10,FILE=
     +       PHOTODIR(LD1:LD2)//'/'//
     +       FILTERFILE4,STATUS='OLD',FORM='FORMATTED')
            K=500
          ELSEIF(NFILTER.LE.900)THEN
            OPEN(10,FILE=
     +       PHOTODIR(LD1:LD2)//'/'//
     +       FILTERFILE5,STATUS='OLD',FORM='FORMATTED')
            K=700
          ELSE
            OPEN(10,FILE=
     +       PHOTODIR(LD1:LD2)//'/'//
     +       FILTERFILE2,STATUS='OLD',FORM='FORMATTED')
            K=900
          END IF
10        READ(10,'(4X,I5,A)',END=12) NPFILT,FILTERNAME
          IF(NPFILT.GT.NPMAX)THEN
            CLOSE(10)
            WRITE(*,100) 'ERROR: this filter can not be read'
            WRITE(*,101) ' (NPFILT.GT.NPMAX)'
            WRITE(*,100) 'Press <CR> to continue...'
            READ(*,*)
            NPFILT=0
            RETURN
          END IF
          K=K+1
          IF((NFILTER.GE.501).AND.(NFILTER.LE.900))THEN
            DO I=1,NPFILT
              READ(10,*) WL_FILT(I),FLUX_FILT(I)
            END DO
          ELSE
            DO I=1,NPFILT
              READ(10,*) IDUM,WL_FILT(I),FLUX_FILT(I)
            END DO
          END IF
          IF(K.EQ.NFILTER)THEN
            CLOSE(10)
            L1=TRUEBEG(FILTERNAME)
            L2=TRUELEN(FILTERNAME)
            FILTERNAME=FILTERNAME(L1:L2)
            WRITE(*,101) '>>> Filter...........: '//
     +       FILTERNAME(1:TRUELEN(FILTERNAME))
            IF((WL_FILT(1).GE.WLMIN).AND.(WL_FILT(NPFILT).LE.WLMAX))THEN
            ELSE
              WRITE(*,101) 'ERROR: filter outside wavelength range.'
              WRITE(*,100) 'Press <CR> to continue...'
              READ(*,*)
              NPFILT=0
            END IF
            GOTO 90
          END IF
          GOTO 10
12        CLOSE(10)
          NPFILT=0
C------------------------------------------------------------------------------
C filtros HST
        ELSE
          L1=TRUEBEG(PHOTODIR)
          L2=TRUELEN(PHOTODIR)
          WRITE(CDUMMY,'(I4)') NFILTER
          ISYSTEM=SYSTEMFUNCTION('\rm -f .photometry_HST_filter')
          IF(NFILTER.LT.1200)THEN
            ISYSTEM=SYSTEMFUNCTION('ls '//PHOTODIR(L1:L2)//
     +       '/res/HST_res/wfpc2*.txt | nl -v 1101 '//
     +       '| grep '//CDUMMY//' | column -t | colrm 1 6 '//
     +       '> .photometry_HST_filter')
          ELSEIF(NFILTER.LT.1300)THEN
            ISYSTEM=SYSTEMFUNCTION('ls '//PHOTODIR(L1:L2)//
     +       '/res/HST_res/pcf*.txt | nl -v 1201 '//
     +       '| grep '//CDUMMY//' | column -t | colrm 1 6 '//
     +       '> .photometry_HST_filter')
          ELSEIF(NFILTER.LT.1400)THEN
            ISYSTEM=SYSTEMFUNCTION('ls '//PHOTODIR(L1:L2)//
     +       '/res/HST_res/wf2f*.txt |nl -v 1301 '//
     +       '| grep '//CDUMMY//' | column -t | colrm 1 6 '//
     +       '> .photometry_HST_filter')
          ELSEIF(NFILTER.LT.1500)THEN
            ISYSTEM=SYSTEMFUNCTION('ls '//PHOTODIR(L1:L2)//
     +       '/res/HST_res/wf3f*.txt | nl -v 1401 '//
     +       '| grep '//CDUMMY//' | column -t | colrm 1 6 '//
     +       '> .photometry_HST_filter')
          ELSE
            ISYSTEM=SYSTEMFUNCTION('ls '//PHOTODIR(L1:L2)//
     +       '/res/HST_res/wf4f*.txt | nl -v 1501 '//
     +       '| grep '//CDUMMY//' | column -t | colrm 1 6 '//
     +       '> .photometry_HST_filter')
          END IF
          OPEN(20,FILE='.photometry_HST_filter',STATUS='OLD',
     +     FORM='FORMATTED')
          READ(20,101) FILTERNAME
          CLOSE(20)
          L1=TRUEBEG(FILTERNAME)
          L2=TRUELEN(FILTERNAME)
          FILTERNAME=FILTERNAME(L1:L2)
          WRITE(*,101) '>>> Filter...........: '//
     +     FILTERNAME(1:TRUELEN(FILTERNAME))
          ISYSTEM=SYSTEMFUNCTION('\rm -f .photometry_HST_filter')
          OPEN(10,FILE=FILTERNAME,STATUS='OLD',FORM='FORMATTED')
          DO I=1,5
            READ(10,*) !ignore first 5 lines
          END DO
          I=0
50        READ(10,*,END=52) WL_FILT(I+1),FLUX_FILT(I+1)
          I=I+1
          GOTO 50
52        CLOSE(10)
          NPFILT=I
          IF((WL_FILT(1).GE.WLMIN).AND.(WL_FILT(NPFILT).LE.WLMAX))THEN
          ELSE
            WRITE(*,101) 'ERROR: filter outside wavelength range.'
            WRITE(*,100) 'Press <CR> to continue...'
            READ(*,*)
            NPFILT=0
          END IF
        END IF
C------------------------------------------------------------------------------
C normalizamos la respuesta del filtro al valor en el maximo
90      CNOR=READC('Normalize filter response (y/n)','n','yn')
        IF(CNOR.EQ.'y')THEN
          FMAX=FLUX_FILT(1)
          DO I=2,NPFILT
            FMAX=AMAX1(FMAX,FLUX_FILT(I))
          END DO
          DO I=1,NPFILT
            FLUX_FILT(I)=FLUX_FILT(I)/FMAX
          END DO
        END IF
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
