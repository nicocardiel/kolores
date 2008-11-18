C******************************************************************************
C lista los filtros definidos entre las l.d.o. WLMIN y WLMAX
        SUBROUTINE LISTFILTER(WLMIN,WLMAX)
        IMPLICIT NONE
        REAL WLMIN,WLMAX
C
        INCLUDE 'photometry.inc'
C
        INTEGER TRUEBEG,TRUELEN
        INTEGER READILIM
        INTEGER SYSTEMFUNCTION
        CHARACTER*255 READC
C
        INTEGER I,K,L1,L2
        INTEGER LD1,LD2
        INTEGER NPFILT !no. of points which define a given filter
        INTEGER IDUM
        INTEGER ISYSTEM
        INTEGER ICHIP
        REAL WL_FILT(NPMAX),FLUX_FILT(NPMAX)
        CHARACTER*1 CTYPE
        CHARACTER*80 FILTERNAME
C------------------------------------------------------------------------------
        LD1=TRUEBEG(PHOTODIR)
        LD2=TRUELEN(PHOTODIR)
        IF(WLMIN.GT.WLMAX) RETURN
C elegimos tipos de filtro a mostrar
        CTYPE(1:1)=
     +   READC('Filter list (1=usual filters, 2=NIRSPEC, 3=HST,'//
     +   ' 4=Bruzual, others=5)','1','12345')
C------------------------------------------------------------------------------
C filtros "normales"
        IF((CTYPE.EQ.'1').OR.(CTYPE.EQ.'2').OR.(CTYPE.EQ.'4').
     +    OR.(CTYPE.EQ.'5'))THEN
          IF(CTYPE.EQ.'1')THEN
            OPEN(10,FILE=
     +       PHOTODIR(LD1:LD2)//'/'//
     +       FILTERFILE1,STATUS='OLD',FORM='FORMATTED')
            K=0
          ELSEIF(CTYPE.EQ.'2')THEN
            OPEN(10,FILE=
     +       PHOTODIR(LD1:LD2)//'/'//
     +       FILTERFILE2,STATUS='OLD',FORM='FORMATTED')
            K=900
          ELSEIF(CTYPE.EQ.'4')THEN
            OPEN(10,FILE=
     +       PHOTODIR(LD1:LD2)//'/'//
     +       FILTERFILE3,STATUS='OLD',FORM='FORMATTED')
            K=100
          ELSEIF(CTYPE.EQ.'5')THEN
            OPEN(10,FILE=
     +       PHOTODIR(LD1:LD2)//'/'//
     +       FILTERFILE4,STATUS='OLD',FORM='FORMATTED')
            K=500
          END IF
10        READ(10,'(4X,I5,A)',END=20) NPFILT,FILTERNAME
          L1=TRUEBEG(FILTERNAME)
          L2=TRUELEN(FILTERNAME)
          FILTERNAME=FILTERNAME(L1:L2)
          IF(NPFILT.GT.NPMAX)THEN
            CLOSE(10)
            WRITE(*,100) '>>> Filter: '
            WRITE(*,101) FILTERNAME(1:TRUELEN(FILTERNAME))
            WRITE(*,100) 'ERROR: this filter can not be read'
            WRITE(*,101) ' (NPFILT.GT.NPMAX)'
            WRITE(*,100) 'Press <CR> to continue...'
            READ(*,*)
            NPFILT=0
            RETURN
          END IF
          K=K+1
          IF(CTYPE.EQ.'5')THEN
            DO I=1,NPFILT
              READ(10,*) WL_FILT(I),FLUX_FILT(I)
            END DO
          ELSE
            DO I=1,NPFILT
              READ(10,*) IDUM,WL_FILT(I),FLUX_FILT(I)
            END DO
          END IF
          IF((WL_FILT(1).GE.WLMIN).AND.(WL_FILT(NPFILT).LE.WLMAX))THEN
            WRITE(*,'(I4,2X,A)') K,FILTERNAME(1:TRUELEN(FILTERNAME))
          END IF
          GOTO 10
20        CLOSE(10)
C------------------------------------------------------------------------------
C filtros HST
        ELSEIF(CTYPE.EQ.'3')THEN
          WRITE(*,*)
          WRITE(*,101) '0 = trhougput for each filter in isolation'
          WRITE(*,*)
          WRITE(*,101) '-----------------'
          WRITE(*,101) '|       |       |'
          WRITE(*,101) '|   3   |   2   |'
          WRITE(*,101) '|       |       |'
          WRITE(*,101) '|-------|-------|'
          WRITE(*,101) '|       | 1  |'
          WRITE(*,101) '|   4   |____|'
          WRITE(*,101) '|       |'
          WRITE(*,101) '---------'
          WRITE(*,100) 'Note: 1, 2, 3 and 4 include the system response'
          WRITE(*,101) ' and CCD QE.'
          ICHIP=READILIM('Chip number in the WPC2','0',0,4)
          L1=TRUEBEG(PHOTODIR)
          L2=TRUELEN(PHOTODIR)
          IF(ICHIP.EQ.0)THEN
            ISYSTEM=SYSTEMFUNCTION('ls '//PHOTODIR(L1:L2)//
     +       '/res/HST_res/wfpc2*.txt | nl -v 1101')
ccc     +       '/HST_res/wfpc2*.txt | nl -v 1101 .photometry_HST_dir')
          ELSEIF(ICHIP.EQ.1)THEN
            ISYSTEM=SYSTEMFUNCTION('ls '//PHOTODIR(L1:L2)//
     +       '/res/HST_res/pcf*.txt | nl -v 1201')
ccc     +       '/HST_res/pcf*.txt | nl -v 1201 .photometry_HST_dir')
          ELSEIF(ICHIP.EQ.2)THEN
            ISYSTEM=SYSTEMFUNCTION('ls '//PHOTODIR(L1:L2)//
     +       '/res/HST_res/wf2f*.txt | nl -v 1301')
ccc     +       '/HST_res/wf2f*.txt | nl -v 1301 .photometry_HST_dir')
          ELSEIF(ICHIP.EQ.3)THEN
            ISYSTEM=SYSTEMFUNCTION('ls '//PHOTODIR(L1:L2)//
     +       '/res/HST_res/wf3f*.txt | nl -v 1401')
ccc     +       '/HST_res/wf3f*.txt | nl -v 1401 .photometry_HST_dir')
          ELSE
            ISYSTEM=SYSTEMFUNCTION('ls '//PHOTODIR(L1:L2)//
     +       '/res/HST_res/wf4f*.txt | nl -v 1501')
ccc     +       '/HST_res/wf4f*.txt | nl -v 1501 .photometry_HST_dir')
          END IF
        END IF
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
