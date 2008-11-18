C
C******************************************************************************
C Subrutina para leer un espectro almacenado en un fichero ASCII
C------------------------------------------------------------------------------
        SUBROUTINE SLEEASCII(FILENAME,CERR)
        IMPLICIT NONE
        CHARACTER*(*) FILENAME
        CHARACTER*1 CERR
C parametros
        INCLUDE 'photometry.inc'
C funciones auxiliares
        INTEGER READI
        INTEGER READILIM
        REAL READF
        REAL LININTERP
        EXTERNAL RANRED
        REAL RANRED
        CHARACTER*255 READC
C variables globales (COMMONs)
        INTEGER NAXIS(2)
        REAL IMAGEN(NXMAX,NYMAX)
        REAL IMAGENE(NXMAX,NYMAX)
        REAL WL_SPEC(NPMAX)
        REAL CRVAL1,CDELT1
        REAL SNRAT_,SNRAT,SNRATMIN,SNRATMAX
C variables locales
        INTEGER J
        INTEGER NSKIP
        INTEGER IFLAG,N1,N2
        INTEGER NPIXELS
        INTEGER NSEED
        REAL Z
        REAL CDELT1_
        REAL WL_SPEC_(NPMAX),FLUX_SPEC_(NPMAX)
        CHARACTER*1 COPC
        LOGICAL LCHANGE
C
        COMMON/BLKIMAGEN/IMAGEN
        COMMON/BLKIMAGENE/IMAGENE
        COMMON/BLKWL_SPEC/WL_SPEC
        COMMON/BLKNAXIS/NAXIS                                      !dimensiones
        COMMON/BLKSTWVDISP/CRVAL1,CDELT1
        COMMON/BLKSNRAT/SNRAT,SNRATMIN,SNRATMAX
C------------------------------------------------------------------------------
        SNRAT_=0.0 !avoid compilation warning
        NSEED=-1
        NSKIP=READI('No. of lines to be skipped','0')
        Z=READF('Redshift to be applied','0.0')
C------------------------------------------------------------------------------
        IF(CERR.EQ.'#')THEN
          IF(SNRAT.GT.0.0)THEN
            SNRAT_=SNRAT
          ELSE
            SNRAT_=SNRATMIN+(SNRATMAX-SNRATMIN)*RANRED(NSEED)
          END IF
        END IF
        OPEN(10,FILE=FILENAME,STATUS='OLD',FORM='FORMATTED')
        IF(NSKIP.GT.0)THEN
          DO J=1,NSKIP
            READ(10,*)
          END DO
        END IF
        J=1
10      IF(CERR.EQ.'y')THEN
          READ(10,*,END=20) WL_SPEC(J),IMAGEN(J,1),IMAGENE(J,1)
        ELSE
          READ(10,*,END=20) WL_SPEC(J),IMAGEN(J,1)
          IF(CERR.EQ.'#')THEN
            IMAGENE(J,1)=IMAGEN(J,1)/SNRAT_
          ELSE
            IMAGENE(J,1)=0.0
          END IF
        END IF
        WL_SPEC(J)=WL_SPEC(J)*(1.+Z)
        J=J+1
        GOTO 10
20      CLOSE(10)
        NAXIS(1)=J-1
        NAXIS(2)=1
C------------------------------------------------------------------------------
C comprobamos si CDELT1 es constante
        CDELT1=WL_SPEC(2)-WL_SPEC(1)
        LCHANGE=.FALSE.
        J=1
        DO WHILE((.NOT.LCHANGE).AND.(J.LT.NAXIS(1)-1))
          J=J+1
          CDELT1_=WL_SPEC(J+1)-WL_SPEC(J)
          IF(CDELT1_.NE.CDELT1)THEN
            LCHANGE=.TRUE.
          END IF
        END DO
        IF(LCHANGE)THEN
          WRITE(*,100) 'Initial values: '
          WRITE(*,*) 1,2,WL_SPEC(1),WL_SPEC(2),CDELT1
          WRITE(*,100) 'Last values...: '
          WRITE(*,*) J,J+1,WL_SPEC(J),WL_SPEC(J+1),CDELT1_
          WRITE(*,101) 'ERROR: CDELT1 is not constant.'
          WRITE(*,*)
          WRITE(*,101) '(1) force CRVAL1 and CDELT1 in current table'
          WRITE(*,101) '(2) compute new table using new CRVAL1 and'//
     +     ' CDELT1 (linear interpolation)'
          COPC(1:1)=READC('Option (1/2)','2','12')
          CRVAL1=READF('New CRVAL1','@')
          CDELT1=READF('New CDELT1','@')
          IF(COPC.EQ.'1')THEN
            DO J=1,NAXIS(1)
              WL_SPEC(J)=CRVAL1+REAL(J-1)*CDELT1
            END DO
          ELSE
            NPIXELS=READILIM('No. of pixels','@',1,NPMAX)
            DO J=1,NPIXELS
              WL_SPEC_(J)=CRVAL1+REAL(J-1)*CDELT1
              FLUX_SPEC_(J)=LININTERP(NAXIS(1),WL_SPEC,IMAGEN(1,1),
     +         WL_SPEC_(J),IFLAG,N1,N2)
              IF(IFLAG.EQ.-1)THEN
                WRITE(*,100) 'WARNING: extrapolation towards '//
     +           'lower X values in pixel no. '
                WRITE(*,*) J
              ELSEIF(IFLAG.EQ.1)THEN
                WRITE(*,100) 'WARNING: extrapolation towards '//
     +           'higher X values in pixel no. '
                WRITE(*,*) J
              ELSEIF(IFLAG.EQ.9)THEN
                WRITE(*,100) 'FATAL ERROR: division by zero in '
                WRITE(*,101) 'subroutine LININTERP'
                STOP
              END IF
            END DO
            NAXIS(1)=NPIXELS
            DO J=1,NAXIS(1)
              WL_SPEC(J)=WL_SPEC_(J)
              IMAGEN(J,1)=FLUX_SPEC_(J)
              IMAGENE(J,1)=0.
            END DO
          END IF
        ELSE
          CRVAL1=WL_SPEC(1)
          CDELT1=CDELT1_
        END IF
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
        END
