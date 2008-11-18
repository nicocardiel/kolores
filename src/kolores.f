        PROGRAM KOLORES
        IMPLICIT NONE
C
        INCLUDE 'photometry.inc'
        INCLUDE 'version.inc'
C
        INTEGER NBUTTX,NBUTTY,NBUTT_TOTAL
        PARAMETER (NBUTTX=6,NBUTTY=4,NBUTT_TOTAL=NBUTTX*NBUTTY)
        REAL PI2
        PARAMETER (PI2=6.283185307)
        REAL RAIZ2
        PARAMETER (RAIZ2=1.414213562)
        REAL C
        PARAMETER (C=299792.46)
C funciones
        INTEGER READI,READILIM
        INTEGER TRUELEN
        REAL READF
        REAL LAMBDAEFF,FLUXEFFVEGA,LAMBDAEFFVEGA,XIFILTER
        REAL MAGNITUDE,FILTERWIDTH
        REAL LININTERP
        REAL FMEAN0
        EXTERNAL RANRED
        REAL RANRED
        CHARACTER*255 READC
C variables
        INTEGER I,J,K,K_,L,L_
        INTEGER NPATM !no. of data points in atmosphere transmission
        INTEGER NPVEGA !no. of data points in the Vega spectrum
        INTEGER NPFILT_
        INTEGER NPFILT(NFILTROS) !no. of data points in filters
        INTEGER NB,NBLOCAL
        INTEGER NF,NF_,NFILTER_
        INTEGER NFILTER(NFILTROS)
        INTEGER NAXIS(2)
        INTEGER NS0,NS1,NS2,NS_
        INTEGER NTERM,IDN(8),ITERM
        INTEGER IFLAG,N1,N2
        INTEGER NSIMUL,NSIMULERR
        INTEGER NSEED
        INTEGER NCOLOR(NFILTROS)
        INTEGER NZ,NREDSHIFTS
        INTEGER NEBV,NEBVS
        INTEGER NOUT
        INTEGER NUNIT
        INTEGER IF1,IF2
        REAL WL_ATM(NPMAX),FLUX_ATM(NPMAX)
        REAL WL_FILT_(NPMAX),FLUX_FILT_(NPMAX)
        REAL WL_FILT(NPMAX,NFILTROS),FLUX_FILT(NPMAX,NFILTROS)
        REAL WL_VEGA(NPMAX),FLUX_VEGA(NPMAX),EFLUX_VEGA(NPMAX)
        REAL FLUX_VEGA_(NPMAX) !para simulaciones
        REAL WL_SPEC(NPMAX),FLUX_SPEC(NPMAX),EFLUX_SPEC(NPMAX)
        REAL FLUX_SPEC_EBV0(NPMAX)
        REAL ZMAS1
        REAL FLUX_SPEC_(NPMAX) !para simulaciones
        REAL EFLUX_SPEC_(NPMAX,NSIMULMAX),PIXEL(NSIMULMAX) !para simulaciones
        REAL FLUX_RESID(NPMAX) !para testear simulaciones con rvrebin
        REAL SNRAT_,SNRAT,SNRATMIN,SNRATMAX
        REAL XMINV,XMAXV,YMINV,YMAXV !limites del espectro de Vega
        REAL XMINF,XMAXF !limites para buscar filtro
        REAL XMIN,XMAX,YMIN,YMAX !limites del dibujo
        REAL X1,X2,Y1,Y2
        REAL XC,YC
        REAL IMAGEN_(NXMAX,NYMAX)
        REAL IMAGEN(NXMAX,NYMAX)
        REAL IMAGENE(NXMAX,NYMAX)
        REAL IMAGEN_SNRAT(NYMAX)
        REAL CRVAL1,CDELT1
        REAL YINTERP,FDUMMY
        REAL MAG(NFILTROS),COLOR(NFILTROS)
        REAL MAG_(NSIMULMAX,NFILTROS),COLOR_(NSIMULMAX,NFILTROS)
        REAL MAG_MEAN(NFILTROS),MAG_RMS(NFILTROS)
        REAL COLOR_MEAN(NFILTROS),COLOR_RMS(NFILTROS)
        REAL SUM1,SUM2,SUM2_(NSIMULMAX),SUM2_MEAN,SUM2_RMS
        REAL CTEMAG,CTEMAG_MEAN,CTEMAG_RMS
        REAL LAMBDAEFF0,XIFILTER0
        REAL FLUXEFFVEGA0,FLUXEFFVEGA_MEAN,FLUXEFFVEGA_RMS
        REAL LAMBDAEFFVEGA0,LAMBDAEFFVEGA_MEAN,LAMBDAEFFVEGA_RMS
        REAL FLUXEFFVEGA_(NSIMULMAX),LAMBDAEFFVEGA_(NSIMULMAX)
        REAL R1,R2
        REAL Z,ZCORTE,ZMIN,ZMAX,RADVEL
        REAL EBV,EBVMIN,EBVMAX,LAMBDA,RV_EXT,K_LAMBDA(NPMAX),FACTOR_EXT
        REAL XSORT(2*NPMAX),XOUT(2*NPMAX)
        REAL FACTOR
        CHARACTER*1 CH
        CHARACTER*1 CERR
        CHARACTER*1 COUT
        CHARACTER*1 CPLOT
        CHARACTER*1 CFITS
        CHARACTER*1 CATM
        CHARACTER*1 CMODEL_SED
        CHARACTER*1 CQUIT
        CHARACTER*1 COPC_EXT
        CHARACTER*(NBUTT_TOTAL) CLINBUT
        CHARACTER*20 CLABVEGA !etiqueta con nombre de standard de flujo
        CHARACTER*50 CDUMMY,CBUTTON
        CHARACTER*80 FILTERNAME_,FILTERNAME(NFILTROS)
        CHARACTER*80 INFILE,ERRFILE,OUTFILE
        CHARACTER*80 CLABEL_INFILE
        LOGICAL LEXIT
        LOGICAL LBEXIST
        LOGICAL LPVEGA,LPFILTER,LPSPECT,LPATM,LPABST0
        LOGICAL LPERR
        LOGICAL LOGFILE
        LOGICAL LNULL(NXMAX,NYMAX),ANYNULL
        LOGICAL LANY
        LOGICAL LMIDE(NFILTROS)
        LOGICAL LOK_EXT
C
        COMMON/BLKIMAGEN_/IMAGEN_
        COMMON/BLKIMAGEN/IMAGEN
        COMMON/BLKIMAGENE/IMAGENE
        COMMON/BLKWL_SPEC/WL_SPEC
        COMMON/BLKNULL/LNULL,ANYNULL
        COMMON/BLKNAXIS/NAXIS
        COMMON/BLKSTWVDISP/CRVAL1,CDELT1
        COMMON/BLKATM1/NPATM
        COMMON/BLKATM2/WL_ATM,FLUX_ATM
        COMMON/BLKGRAPHIC/NTERM,IDN
        COMMON/BLKSNRAT/SNRAT,SNRATMIN,SNRATMAX
C------------------------------------------------------------------------------
C welcome message
        WRITE(*,101) '************************************************'
        WRITE(*,101) '       Welcome to kolores '//
     +   '(version '//VERSION//')'
        WRITE(*,101) '************************************************'
        WRITE(*,*)
C------------------------------------------------------------------------------
C inicializamos variables
        DATA (NCOLOR(NF),NF=1,NFILTROS) /4,3,2,5,7,6/
        LPVEGA=.TRUE.
        LPFILTER=.FALSE.
        LPSPECT=.FALSE.
        LPERR=.TRUE.
        LPATM=.FALSE.
        LPABST0=.FALSE.
        CFITS='@'
        CERR='n'
        DO NF=1,NFILTROS
          NFILTER(NF)=0
          NPFILT(NF)=0
        END DO
        NS0=1
        NS1=0
        NS2=0
        NAXIS(1)=0
        NAXIS(2)=0
        NSIMUL=0
        NSIMULERR=NSIMULMAX/2
        NSEED=-1
        COUT='n'
        CPLOT='y'
        NREDSHIFTS=0
        NEBVS=0
        EBVMIN=0.0
        EBVMAX=0.0
        COPC_EXT='4'
        RV_EXT=3.1
C------------------------------------------------------------------------------
C leemos el fichero con la transmision de la atmosfera
        CALL LEEATM(NPATM,WL_ATM,FLUX_ATM)
C leemos y dibujamos el espectro de Vega
        CALL LEEVEGA(NPVEGA,WL_VEGA,FLUX_VEGA,EFLUX_VEGA,CLABVEGA)
        CALL FINDMML(NPVEGA,1,NPVEGA,WL_VEGA,XMINV,XMAXV)
        CALL FINDMML(NPVEGA,1,NPVEGA,FLUX_VEGA,YMINV,YMAXV)
        IF(YMINV.EQ.YMAXV)THEN
          YMINV=0.0
          YMAXV=2.*YMAXV
        END IF
        XMIN=XMINV
        XMAX=XMAXV
        YMIN=YMINV
        YMAX=YMAXV
        CALL EXPAND(XMIN,XMAX,YMIN,YMAX,0.05,0.05,0.05,0.10)
C
        CALL RPGBEGIN(NTERM,IDN)
        CALL BUTTSXB(NBUTTX)
        CALL BUTTSYB(NBUTTY)
        CALL BUTTSCF(1)
        CALL BUTTSCH(0.8)
        DO ITERM=NTERM,1,-1
          CALL PGSLCT(IDN(ITERM))
          IF(ITERM.EQ.1)THEN
            CALL RPGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
          ELSE
            CALL PGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
          END IF
          CALL PGBOX('BCTSN',0.0,0,'BC',0.0,0)
          CALL PLOTVEGA(NPVEGA,WL_VEGA,FLUX_VEGA,EFLUX_VEGA,LPERR,
     +     LPABST0,CLABVEGA)
        END DO
C------------------------------------------------------------------------------
        CALL BUTTON(1,'[l]oad',0)
        CALL BUTTON(2,'[z]oom',0)
        CALL BUTTON(3,'[Ww]hole',0)
        CALL BUTTON(4,'[+] nscan',0)
        CALL BUTTON(4,'[+] nscan',3)
        CALL BUTTON(5,'[-] nscan',0)
        CALL BUTTON(5,'[-] nscan',3)
        CALL BUTTON(6,'[Qq]uit',0)
        DO NF=1,NFILTROS
          WRITE(CBUTTON,'(A9,I1,A1)') 'filter #[',NF,']'
          CALL BUTTON(NF+6,CBUTTON,0)
        END DO
        CALL BUTTON(13,'plot '//CLABVEGA(1:TRUELEN(CLABVEGA)),0)
        IF(LPVEGA) 
     +   CALL BUTTON(13,'plot '//CLABVEGA(1:TRUELEN(CLABVEGA)),1)
        CALL BUTTON(14,'plot [f]ilters',0)
        CALL BUTTON(14,'plot [f]ilters',3)
        CALL BUTTON(15,'plot [s]pect.',0)
        CALL BUTTON(15,'plot [s]pect.',3)
        CALL BUTTON(16,'plot [a]tm.',0)
        IF(LPATM) CALL BUTTON(16,'plot [a]tm.',1)
        CALL BUTTON(17,'plot AB\d\gn\u=ST\d\gl\u=0',0)
        IF(LPABST0) CALL BUTTON(17,'plot AB\d\gn\u=ST\d\gl\u=0',1)
        CALL BUTTON(18,'plot [e]rrors',0)
        IF(LPERR) CALL BUTTON(18,'plot [e]rrors',1)
        CALL BUTTON(19,'filter [p]ar.',0)
        CALL BUTTON(19,'filter [p]ar.',3)
        CALL BUTTON(20,'[c]ompute',0)
        CALL BUTTON(20,'[c]ompute',3)
        CALL BUTTON(21,'[k] correc.',0)
        CALL BUTTON(21,'[k] correc.',3)
        CALL BUTTON(22,'E(B-V)',0)
        CALL BUTTON(22,'E(B-V)',3)
        CALL BUTTON(23,'Xi error',0)
        CALL BUTTON(24,'[.] fit SED',0)
C------------------------------------------------------------------------------
        LEXIT=.FALSE.
        DO WHILE(.NOT.LEXIT)
          CALL RPGBAND(0,0,0.,0.,XC,YC,CH)
          CALL IFBUTTON(XC,YC,NB)
C buscamos aceleradores
          IF(CH.EQ.'Q')THEN
            NBLOCAL=6
          ELSEIF(CH.EQ.'W')THEN
            NBLOCAL=3
          ELSE
c.....................123456
            CLINBUT=('lzw+-q'//
     +               '123456'//
     +               ' fsa e'//
     +               'pck  .')
            NBLOCAL=INDEX(CLINBUT,CH)
          END IF
          IF((NBLOCAL.NE.0).AND.(CH.NE.' '))THEN
            CALL BUTTQEX(NBLOCAL,LBEXIST)
            IF(LBEXIST) NB=NBLOCAL
          END IF
C..............................................................................
          IF(NB.EQ.0)THEN
            WRITE(*,*)
            WRITE(*,100) '>>> Wavelength: '
            WRITE(CDUMMY,*) XC
            CALL RMBLANK(CDUMMY,CDUMMY,L)
            WRITE(*,100) CDUMMY(1:L)
            WRITE(*,100) ', Flux_cursor.: '
            WRITE(CDUMMY,*) YC
            CALL RMBLANK(CDUMMY,CDUMMY,L)
            WRITE(*,101) CDUMMY(1:L)
            IF(LPVEGA)THEN
              IF((XC.GE.XMINV).AND.(XC.LE.XMAXV))THEN
                YINTERP=LININTERP(NPVEGA,WL_VEGA,FLUX_VEGA,XC,
     +           IFLAG,N1,N2)
                WRITE(*,100) '>>> Wavelength: '
                WRITE(CDUMMY,*) XC
                CALL RMBLANK(CDUMMY,CDUMMY,L)
                WRITE(*,100) CDUMMY(1:L)
                WRITE(*,100) ', Flux_Vega...: '
                WRITE(CDUMMY,*) YINTERP
                CALL RMBLANK(CDUMMY,CDUMMY,L)
                WRITE(*,100) CDUMMY(1:L)
                IF(IFLAG.EQ.-1)THEN
                  WRITE(*,101) ' (WARNING: extrapolation)'
                ELSEIF(IFLAG.EQ.1)THEN
                  WRITE(*,101) ' (WARNING: extrapolation)'
                ELSEIF(IFLAG.EQ.9)THEN
                  WRITE(*,101) ' (ERROR: divison by zero)'
                ELSE
                  WRITE(*,101)
                END IF
              END IF
            END IF
            IF(LPSPECT)THEN
              IF((XC.GE.WL_SPEC(1)).AND.(XC.LE.WL_SPEC(NAXIS(1))))THEN
                YINTERP=LININTERP(NAXIS(1),WL_SPEC,IMAGEN(1,NS0),XC,
     +           IFLAG,N1,N2)
                WRITE(*,100) '>>> Wavelength: '
                WRITE(CDUMMY,*) XC
                CALL RMBLANK(CDUMMY,CDUMMY,L)
                WRITE(*,100) CDUMMY(1:L)
                WRITE(*,100) ', Flux_Spec...: '
                WRITE(CDUMMY,*) YINTERP
                CALL RMBLANK(CDUMMY,CDUMMY,L)
                WRITE(*,100) CDUMMY(1:L)
                IF(IFLAG.EQ.-1)THEN
                  WRITE(*,101) ' (WARNING: extrapolation)'
                ELSEIF(IFLAG.EQ.1)THEN
                  WRITE(*,101) ' (WARNING: extrapolation)'
                ELSEIF(IFLAG.EQ.9)THEN
                  WRITE(*,101) ' (ERROR: divison by zero)'
                ELSE
                  WRITE(*,101)
                END IF
              END IF
            END IF
            IF(LPFILTER)THEN
              DO NF=1,NFILTROS
                IF((NPFILT(NF).GT.0).AND.(XC.GE.WL_FILT(1,NF)).AND.
     +           (XC.LE.WL_FILT(NPFILT(NF),NF)))THEN
                  WRITE(*,100) '>>> Wavelength: '
                  WRITE(CDUMMY,*) XC
                  CALL RMBLANK(CDUMMY,CDUMMY,L)
                  WRITE(*,100) CDUMMY(1:L)
                  YINTERP=LININTERP(NPFILT(NF),WL_FILT(1,NF),
     +             FLUX_FILT(1,NF),XC,IFLAG,N1,N2)
                  WRITE(*,'(A11,I1,A4,$)') ', Flux_Filt',NF,'..: '
                  WRITE(CDUMMY,*) YINTERP
                  CALL RMBLANK(CDUMMY,CDUMMY,L)
                  WRITE(*,100) CDUMMY(1:L)
                  IF(IFLAG.EQ.-1)THEN
                    WRITE(*,101) ' (WARNING: extrapolation)'
                  ELSEIF(IFLAG.EQ.1)THEN
                    WRITE(*,101) ' (WARNING: extrapolation)'
                  ELSEIF(IFLAG.EQ.9)THEN
                    WRITE(*,101) ' (ERROR: divison by zero)'
                  ELSE
                    WRITE(*,101)
                  END IF
                END IF
              END DO
            END IF
            IF(LPATM)THEN
              IF((XC.GE.WL_ATM(1)).AND.
     +           (XC.LE.WL_ATM(NPATM)))THEN
                WRITE(*,100) '>>> Wavelength: '
                WRITE(CDUMMY,*) XC
                CALL RMBLANK(CDUMMY,CDUMMY,L)
                WRITE(*,100) CDUMMY(1:L)
                YINTERP=LININTERP(NPATM,WL_ATM,
     +           FLUX_ATM,XC,IFLAG,N1,N2)
                WRITE(*,'(A16,$)') ', Flux_Atm....: '
                WRITE(CDUMMY,*) YINTERP
                CALL RMBLANK(CDUMMY,CDUMMY,L)
                WRITE(*,100) CDUMMY(1:L)
                IF(IFLAG.EQ.-1)THEN
                  WRITE(*,101) ' (WARNING: extrapolation)'
                ELSEIF(IFLAG.EQ.1)THEN
                  WRITE(*,101) ' (WARNING: extrapolation)'
                ELSEIF(IFLAG.EQ.9)THEN
                  WRITE(*,101) ' (ERROR: divison by zero)'
                ELSE
                  WRITE(*,101)
                END IF
              END IF
            END IF
C
C..............................................................................
          ELSEIF(NB.EQ.1)THEN
            CALL BUTTON(NB,'[l]oad',5)
C preguntamos por el tipo de fichero
            WRITE(*,101) '(1) input image/spectrum from FITS file'
            WRITE(*,101) '(2) input spectrum from ASCII file'
            CFITS(1:1)=READC('Option (1/2)',CFITS,'12')
C indicamos si vamos a utilizar imagenes de errores
            WRITE(*,*)
            WRITE(*,101) '(Note: # force SN ratio)'
            CERR(1:1)=READC('Are you using errors (y/n/#)',CERR,'yn#')
            IF(CERR.EQ.'#')THEN
              SNRAT=READF('Fixed SN ratio/pixel (<=0 for random)','@')
              IF(SNRAT.LE.0.0)THEN
                SNRATMIN=READF('Minimum SN ratio/pixel','@')
                SNRATMAX=READF('Maximum SN ratio/pixel','@')
              ELSE
                SNRATMIN=SNRAT
                SNRATMAX=SNRAT
              END IF
            ELSE
              SNRAT=0.0
              SNRATMIN=0.0
              SNRATMAX=0.0
            END IF
C leemos imagen de datos
            LOGFILE=.FALSE.
            DO WHILE(.NOT.LOGFILE)
              INFILE(1:80)=READC('Input file name (none=exit)','@','@')
              INQUIRE(FILE=INFILE,EXIST=LOGFILE)
              IF(.NOT.LOGFILE)THEN
                IF(INFILE.NE.'none')THEN
                  WRITE(*,100) 'ERROR: the file does not exist. '
                  WRITE(*,101) 'Try again.'
                  WRITE(*,100) 'Press <CR> to continue...'
                  READ(*,*)
                ELSE
                  LOGFILE=.TRUE.
                END IF
              END IF
            END DO
            IF(INFILE.NE.'none')THEN
              IF(CFITS.EQ.'1')THEN
                CALL SLEEFITS(INFILE,.TRUE.)
                DO J=1,NAXIS(1)
                  WL_SPEC(J)=CRVAL1+REAL(J-1)*CDELT1
                END DO
                DO I=1,NAXIS(2)
                  DO J=1,NAXIS(1)
                    IMAGEN(J,I)=IMAGEN_(J,I)
                  END DO
                  IMAGEN_SNRAT(I)=0.0
                END DO
                IF(CERR.EQ.'#')THEN
                  DO I=1,NAXIS(2)
                    IF(SNRAT.GT.0.0)THEN
                      SNRAT_=SNRAT
                    ELSE !valores aleatorios en una escala logaritmica
                      SNRAT_=10.0**(ALOG10(SNRATMIN)+
     +                 (ALOG10(SNRATMAX)-ALOG10(SNRATMIN))*
     +                 RANRED(NSEED))
                    END IF
                    IMAGEN_SNRAT(I)=SNRAT_
                    DO J=1,NAXIS(1)
                      IMAGENE(J,I)=IMAGEN(J,I)/SNRAT_
                    END DO
                  END DO
                ELSEIF(CERR.EQ.'y')THEN
                  LOGFILE=.FALSE.
                  DO WHILE(.NOT.LOGFILE)
                    CALL GUESSEF(INFILE,ERRFILE)
                    ERRFILE(1:80)=
     +               READC('Input FITS file name',ERRFILE,'@')
                    INQUIRE(FILE=ERRFILE,EXIST=LOGFILE)
                    IF(.NOT.LOGFILE)THEN
                      WRITE(*,100) 'ERROR: the file does not exist. '
                      WRITE(*,101) 'Try again.'
                      WRITE(*,100) 'Press <CR> to continue...'
                      READ(*,*)
                    END IF
                  END DO
                  CALL SLEEFITS(ERRFILE,.TRUE.)
                  DO I=1,NAXIS(2)
                    DO J=1,NAXIS(1)
                      IMAGENE(J,I)=IMAGEN_(J,I)
                    END DO
                  END DO
                END IF
              ELSE
                CALL SLEEASCII(INFILE,CERR)
              END IF
              IF(CERR.EQ.'#') CERR='y'
              WRITE(*,100) '>>> NAXIS1: '
              WRITE(*,*) NAXIS(1)
              WRITE(*,100) '>>> NAXIS2: '
              WRITE(*,*) NAXIS(2)
              WRITE(*,100) '>>> CRVAL1: '
              WRITE(*,*) CRVAL1
              WRITE(*,100) '>>> CDELT1: '
              WRITE(*,*) CDELT1
C
              IF(NAXIS(2).GT.1)THEN
                WRITE(CDUMMY,*) NS0
                NS0=READILIM('Spectrum # to be plotted',CDUMMY,
     +           1,NAXIS(2))
              ELSE
                NS0=1
              END IF
              LPSPECT=.TRUE.
              CALL BUTTON(15,'plot [s]pect.',0)
              CALL BUTTON(15,'plot [s]pect.',1)
              LANY=.FALSE.
              DO NF=1,NFILTROS
                IF(NPFILT(NF).GT.0) LANY=.TRUE.
              END DO
              IF(LANY)THEN
                CALL BUTTON(20,'[c]ompute',0)
                CALL BUTTON(21,'[k] correc.',0)
                CALL BUTTON(22,'E(B-V)',0)
              END IF
              CALL BUTTON(4,'[+] nscan',0)
              CALL BUTTON(5,'[-] nscan',0)
C
              DO ITERM=NTERM,1,-1
                CALL PGSLCT(IDN(ITERM))
                IF(ITERM.EQ.1)THEN
                  CALL RPGERASW(0.00,1.00,0.00,0.80,0)
                  CALL RPGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
                ELSE
                  CALL PGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
                END IF
                CALL PGBOX('BCTSN',0.0,0,'BC',0.0,0)
                IF(LPVEGA) CALL PLOTVEGA(NPVEGA,WL_VEGA,FLUX_VEGA,
     +           EFLUX_VEGA,LPERR,LPABST0,CLABVEGA)
                IF(LPFILTER)THEN
                  CALL PLOTFILTER(NPFILT,WL_FILT,FLUX_FILT,
     +             FILTERNAME,NFILTER)
                END IF
                IF(LPSPECT)THEN
                  WRITE(CDUMMY,*) NS0
                  CALL RMBLANK(CDUMMY,CDUMMY,L_)
                  L=TRUELEN(INFILE)
                  CLABEL_INFILE=INFILE(1:L)//' (#'//CDUMMY(1:L_)//')'
                  CALL PLOTSPECTRUM(NAXIS(1),WL_SPEC,IMAGEN(1,NS0),
     +             IMAGENE(1,NS0),CLABEL_INFILE,LPERR,CERR)
                END IF
                IF(LPATM) CALL PLOTATM(NPATM,WL_ATM,FLUX_ATM)
              END DO
            END IF
            CALL BUTTON(NB,'[l]oad',0)
C..............................................................................
          ELSEIF(NB.EQ.2)THEN
            CALL BUTTON(NB,'[z]oom',5)
            CALL PGSCI(6)
            CALL RPGBAND(6,0,0.,0.,X1,Y1,CH)
            IF(X1.LT.XMINV) X1=XMINV
            IF(X1.GT.XMAXV) X1=XMAXV
            CALL PGSCI(6)
            CALL RPGBAND(4,0,X1,Y1,X2,Y2,CH)
            CALL PGSCI(1)
            IF(X2.LT.XMINV) X2=XMINV
            IF(X2.GT.XMAXV) X2=XMAXV
            XMIN=AMIN1(X1,X2)
            XMAX=AMAX1(X1,X2)
            YMIN=YMINV
            YMAX=YMAXV
            CALL EXPAND(XMIN,XMAX,YMIN,YMAX,0.05,0.05,0.05,0.10)
            DO ITERM=NTERM,1,-1
              CALL PGSLCT(IDN(ITERM))
              IF(ITERM.EQ.1)THEN
                CALL RPGERASW(0.00,1.00,0.00,0.80,0)
                CALL RPGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
              ELSE
                CALL PGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
              END IF
              CALL PGBOX('BCTSN',0.0,0,'BC',0.0,0)
              IF(LPVEGA) CALL PLOTVEGA(NPVEGA,WL_VEGA,FLUX_VEGA,
     +         EFLUX_VEGA,LPERR,LPABST0,CLABVEGA)
              IF(LPFILTER)THEN
                CALL PLOTFILTER(NPFILT,WL_FILT,FLUX_FILT,
     +           FILTERNAME,NFILTER)
              END IF
              IF(LPSPECT)THEN
                WRITE(CDUMMY,*) NS0
                CALL RMBLANK(CDUMMY,CDUMMY,L_)
                L=TRUELEN(INFILE)
                CLABEL_INFILE=INFILE(1:L)//' (#'//CDUMMY(1:L_)//')'
                CALL PLOTSPECTRUM(NAXIS(1),WL_SPEC,IMAGEN(1,NS0),
     +           IMAGENE(1,NS0),CLABEL_INFILE,LPERR,CERR)
              END IF
              IF(LPATM) CALL PLOTATM(NPATM,WL_ATM,FLUX_ATM)
            END DO
            CALL BUTTON(NB,'[z]oom',0)
C..............................................................................
          ELSEIF(NB.EQ.3)THEN
            CALL BUTTON(NB,'[Ww]hole',5)
            YMIN=YMINV
            YMAX=YMAXV
            IF(CH.EQ.'W')THEN !ajustamos los limites al espectro de Vega
              XMIN=XMINV
              XMAX=XMAXV
            ELSE !ajustamos los limites a los filtros dibujados
              IF(LPFILTER)THEN
                LANY=.FALSE.
                DO NF=1,NFILTROS
                  IF(NPFILT(NF).GT.0) LANY=.TRUE.
                END DO
                IF(LANY)THEN
                ELSE
                  XMIN=XMINV
                  XMAX=XMAXV
                END IF
                XMIN=XMAXV !tomamos los extremos al reves
                XMAX=XMINV
                DO NF=1,NFILTROS
                  IF(NPFILT(NF).GT.0)THEN
                    XMIN=AMIN1(XMIN,WL_FILT(1,NF))
                    XMAX=AMAX1(XMAX,WL_FILT(NPFILT(NF),NF))
                  END IF
                END DO
              ELSE
                XMIN=XMINV
                XMAX=XMAXV
              END IF
            END IF
            CALL EXPAND(XMIN,XMAX,YMIN,YMAX,0.05,0.05,0.05,0.10)
            DO ITERM=NTERM,1,-1
              CALL PGSLCT(IDN(ITERM))
              IF(ITERM.EQ.1)THEN
                CALL RPGERASW(0.00,1.00,0.00,0.80,0)
                CALL RPGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
              ELSE
                CALL PGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
              END IF
              CALL PGBOX('BCTSN',0.0,0,'BC',0.0,0)
              IF(LPVEGA) CALL PLOTVEGA(NPVEGA,WL_VEGA,FLUX_VEGA,
     +         EFLUX_VEGA,LPERR,LPABST0,CLABVEGA)
              IF(LPFILTER)THEN
                CALL PLOTFILTER(NPFILT,WL_FILT,FLUX_FILT,
     +           FILTERNAME,NFILTER)
              END IF
              IF(LPSPECT)THEN
                WRITE(CDUMMY,*) NS0
                CALL RMBLANK(CDUMMY,CDUMMY,L_)
                L=TRUELEN(INFILE)
                CLABEL_INFILE=INFILE(1:L)//' (#'//CDUMMY(1:L_)//')'
                CALL PLOTSPECTRUM(NAXIS(1),WL_SPEC,IMAGEN(1,NS0),
     +           IMAGENE(1,NS0),CLABEL_INFILE,LPERR,CERR)
              END IF
              IF(LPATM) CALL PLOTATM(NPATM,WL_ATM,FLUX_ATM)
            END DO
            CALL BUTTON(NB,'[Ww]hole',0)
C..............................................................................
          ELSEIF(NB.EQ.4)THEN
            CALL BUTTON(NB,'[+] nscan',0)
            NS0=NS0+1
            IF(NS0.GT.NAXIS(2)) NS0=1
            DO ITERM=NTERM,1,-1
              CALL PGSLCT(IDN(ITERM))
              IF(ITERM.EQ.1)THEN
                CALL RPGERASW(0.00,1.00,0.00,0.80,0)
                CALL RPGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
              ELSE
                CALL PGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
              END IF
              CALL PGBOX('BCTSN',0.0,0,'BC',0.0,0)
              IF(LPVEGA) CALL PLOTVEGA(NPVEGA,WL_VEGA,FLUX_VEGA,
     +         EFLUX_VEGA,LPERR,LPABST0,CLABVEGA)
              IF(LPFILTER)THEN
                CALL PLOTFILTER(NPFILT,WL_FILT,FLUX_FILT,
     +           FILTERNAME,NFILTER)
              END IF
              IF(LPSPECT)THEN
                WRITE(CDUMMY,*) NS0
                CALL RMBLANK(CDUMMY,CDUMMY,L_)
                L=TRUELEN(INFILE)
                CLABEL_INFILE=INFILE(1:L)//' (#'//CDUMMY(1:L_)//')'
                CALL PLOTSPECTRUM(NAXIS(1),WL_SPEC,IMAGEN(1,NS0),
     +           IMAGENE(1,NS0),CLABEL_INFILE,LPERR,CERR)
              END IF
              IF(LPATM) CALL PLOTATM(NPATM,WL_ATM,FLUX_ATM)
            END DO
            CALL BUTTON(NB,'[+] nscan',0)
C..............................................................................
          ELSEIF(NB.EQ.5)THEN
            CALL BUTTON(NB,'[-] nscan',0)
            NS0=NS0-1
            IF(NS0.LT.1) NS0=NAXIS(2)
            DO ITERM=NTERM,1,-1
              CALL PGSLCT(IDN(ITERM))
              IF(ITERM.EQ.1)THEN
                CALL RPGERASW(0.00,1.00,0.00,0.80,0)
                CALL RPGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
              ELSE
                CALL PGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
              END IF
              CALL PGBOX('BCTSN',0.0,0,'BC',0.0,0)
              IF(LPVEGA) CALL PLOTVEGA(NPVEGA,WL_VEGA,FLUX_VEGA,
     +         EFLUX_VEGA,LPERR,LPABST0,CLABVEGA)
              IF(LPFILTER)THEN
                CALL PLOTFILTER(NPFILT,WL_FILT,FLUX_FILT,
     +           FILTERNAME,NFILTER)
              END IF
              IF(LPSPECT)THEN
                WRITE(CDUMMY,*) NS0
                CALL RMBLANK(CDUMMY,CDUMMY,L_)
                L=TRUELEN(INFILE)
                CLABEL_INFILE=INFILE(1:L)//' (#'//CDUMMY(1:L_)//')'
                CALL PLOTSPECTRUM(NAXIS(1),WL_SPEC,IMAGEN(1,NS0),
     +           IMAGENE(1,NS0),CLABEL_INFILE,LPERR,CERR)
              END IF
              IF(LPATM) CALL PLOTATM(NPATM,WL_ATM,FLUX_ATM)
            END DO
            CALL BUTTON(NB,'[-] nscan',0)
C..............................................................................
          ELSEIF(NB.EQ.6)THEN
            CALL BUTTON(NB,'[Qq]uit',5)
            IF(CH.EQ.'Q')THEN
              LEXIT=.TRUE.
            ELSE
              CQUIT(1:1)=
     +         READC('Do you really want to quit (y/n)','n','yn')
              LEXIT=(CQUIT.EQ.'y')
            END IF
            IF(.NOT.LEXIT) CALL BUTTON(NB,'[Qq]uit',0)
C..............................................................................
          ELSEIF((NB.GE.7).AND.(NB.LE.12))THEN
            NF=NB-6
            WRITE(CBUTTON,'(A9,I1,A1)') 'filter #[',NF,']'
            CALL BUTTON(NB,CBUTTON,5)
            NFILTER_=NFILTER(NF)
            WRITE(CDUMMY,*) NFILTER_
            IF(NAXIS(1).GT.0)THEN
              XMINF=AMIN1(XMINV,WL_SPEC(1))
              XMAXF=AMAX1(XMAXV,WL_SPEC(NAXIS(1)))
            ELSE
              XMINF=XMINV
              XMAXF=XMAXV
            END IF
            NFILTER_=-1
            DO WHILE(NFILTER_.EQ.-1)
              NFILTER_=READI('Filter number (0=none,-1=list)',CDUMMY)
              IF(NFILTER_.EQ.-1) CALL LISTFILTER(XMINV,XMAXV)
            END DO
            NF=NB-6
            IF(NFILTER_.GT.0)THEN
              CALL LEEFILTER(XMINV,XMAXV,NFILTER_,
     +        NPFILT_,WL_FILT_,FLUX_FILT_,FILTERNAME_)
              IF(NPFILT_.GT.0)THEN
                ZMAS1=READF('Redshift to move filter to rest-frame',
     +           '0.0')
                ZMAS1=1.0+ZMAS1
                DO I=1,NPFILT_
                  WL_FILT_(I)=WL_FILT_(I)/ZMAS1
                END DO
                LPFILTER=.TRUE.
                CALL BUTTON(14,'plot [f]ilters',1)
                CALL BUTTON(NB,CBUTTON,0)
                CALL BUTTON(NB,CBUTTON,-NCOLOR(NF)-1)
                CALL BUTTON(19,'filter [p]ar.',0)
                IF(NAXIS(1).GT.0)THEN
                  CALL BUTTON(20,'[c]ompute',0)
                  CALL BUTTON(21,'[k] correc.',0)
                  CALL BUTTON(22,'E(B-V)',0)
                END IF
                NFILTER(NF)=NFILTER_
                FILTERNAME(NF)=FILTERNAME_
                CATM(1:1)=
     +           READC('Apply atmospheric transmission (y/n)','n','yn')
                IF(CATM.EQ.'y')THEN
                  WRITE(*,100) 'Thinking...'
                  DO I=1,NPFILT_
                    XSORT(I)=WL_FILT_(I)
                  END DO
                  DO I=1,NPATM
                    XSORT(I+NPFILT_)=WL_ATM(I)
                  END DO
                  CALL ORDENA1F(NPFILT_+NPATM,XSORT)
                  CALL AVOIDREP(NPFILT_+NPATM,XSORT,NOUT,XOUT,
     +             WL_FILT_(1),WL_FILT_(NPFILT_))
                  IF(NOUT.GT.NPMAX)THEN
                    WRITE(*,*)
                    WRITE(*,100) 'NOUT, NPMAX: '
                    WRITE(*,*) NOUT,NPMAX
                    WRITE(*,100) 'FATAL ERROR: NOUT.GT.NPMAX.'
                    WRITE(*,101) 'Redim NPMAX'
                    STOP
                  END IF
                  NPFILT(NF)=NOUT
                  DO I=1,NPFILT(NF)
                    WL_FILT(I,NF)=XOUT(I)
                    IF(XOUT(I).LT.WL_ATM(1))THEN
                      FACTOR=1.0
                    ELSEIF(XOUT(I).GT.WL_ATM(NPATM))THEN
                      FACTOR=1.0
                    ELSE
                      FACTOR=LININTERP(NPATM,WL_ATM,FLUX_ATM,XOUT(I),
     +                 IFLAG,N1,N2)
                    END IF
                    FLUX_FILT(I,NF)=LININTERP(NPFILT_,WL_FILT_,
     +               FLUX_FILT_,XOUT(I),IFLAG,N1,N2)*FACTOR
                  END DO
                  WRITE(*,101) 'OK!'
                ELSE
                  NPFILT(NF)=NPFILT_
                  DO I=1,NPFILT(NF)
                    WL_FILT(I,NF)=WL_FILT_(I)
                    FLUX_FILT(I,NF)=FLUX_FILT_(I)
                  END DO
                END IF
              ELSE
                CALL BUTTON(NB,CBUTTON,0)
              END IF
            ELSE
              NPFILT(NF)=0
              CALL BUTTON(NB,CBUTTON,0)
            END IF
C
            DO ITERM=NTERM,1,-1
              CALL PGSLCT(IDN(ITERM))
              IF(ITERM.EQ.1)THEN
                CALL RPGERASW(0.00,1.00,0.00,0.80,0)
                CALL RPGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
              ELSE
                CALL PGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
              END IF
              CALL PGBOX('BCTSN',0.0,0,'BC',0.0,0)
              IF(LPVEGA) CALL PLOTVEGA(NPVEGA,WL_VEGA,FLUX_VEGA,
     +         EFLUX_VEGA,LPERR,LPABST0,CLABVEGA)
              IF(LPFILTER)THEN
                CALL PLOTFILTER(NPFILT,WL_FILT,FLUX_FILT,
     +           FILTERNAME,NFILTER)
              END IF
              IF(LPSPECT)THEN
                WRITE(CDUMMY,*) NS0
                CALL RMBLANK(CDUMMY,CDUMMY,L_)
                L=TRUELEN(INFILE)
                CLABEL_INFILE=INFILE(1:L)//' (#'//CDUMMY(1:L_)//')'
                CALL PLOTSPECTRUM(NAXIS(1),WL_SPEC,IMAGEN(1,NS0),
     +           IMAGENE(1,NS0),CLABEL_INFILE,LPERR,CERR)
              END IF
              IF(LPATM) CALL PLOTATM(NPATM,WL_ATM,FLUX_ATM)
            END DO
C..............................................................................
          ELSEIF(NB.EQ.13)THEN
            CALL BUTTON(NB,'plot '//CLABVEGA(1:TRUELEN(CLABVEGA)),5)
            IF(LPVEGA)THEN
              LPVEGA=.FALSE.
              CALL BUTTON(NB,'plot '//CLABVEGA(1:TRUELEN(CLABVEGA)),0)
              CALL BUTTON(17,'plot AB\d\gn\u=ST\d\gl\u=0',3)
            ELSE
              LPVEGA=.TRUE.
              CALL BUTTON(NB,'plot '//CLABVEGA(1:TRUELEN(CLABVEGA)),0)
              CALL BUTTON(NB,'plot '//CLABVEGA(1:TRUELEN(CLABVEGA)),1)
              CALL BUTTON(17,'plot AB\d\gn\u=ST\d\gl\u=0',0)
              IF(LPABST0)THEN
                CALL BUTTON(17,'plot AB\d\gn\u=ST\d\gl\u=0',1)
              END IF
            END IF
            DO ITERM=NTERM,1,-1
              CALL PGSLCT(IDN(ITERM))
              IF(ITERM.EQ.1)THEN
                CALL RPGERASW(0.00,1.00,0.00,0.80,0)
                CALL RPGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
              ELSE
                CALL PGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
              END IF
              CALL PGBOX('BCTSN',0.0,0,'BC',0.0,0)
              IF(LPVEGA) CALL PLOTVEGA(NPVEGA,WL_VEGA,FLUX_VEGA,
     +         EFLUX_VEGA,LPERR,LPABST0,CLABVEGA)
              IF(LPFILTER)THEN
                CALL PLOTFILTER(NPFILT,WL_FILT,FLUX_FILT,
     +           FILTERNAME,NFILTER)
              END IF
              IF(LPSPECT)THEN
                WRITE(CDUMMY,*) NS0
                CALL RMBLANK(CDUMMY,CDUMMY,L_)
                L=TRUELEN(INFILE)
                CLABEL_INFILE=INFILE(1:L)//' (#'//CDUMMY(1:L_)//')'
                CALL PLOTSPECTRUM(NAXIS(1),WL_SPEC,IMAGEN(1,NS0),
     +           IMAGENE(1,NS0),CLABEL_INFILE,LPERR,CERR)
              END IF
              IF(LPATM) CALL PLOTATM(NPATM,WL_ATM,FLUX_ATM)
            END DO
C..............................................................................
          ELSEIF(NB.EQ.14)THEN
            CALL BUTTON(NB,'plot [f]ilters',5)
            IF(LPFILTER)THEN
              LPFILTER=.FALSE.
              CALL BUTTON(NB,'plot [f]ilters',0)
            ELSE
              LPFILTER=.TRUE.
              CALL BUTTON(NB,'plot [f]ilters',0)
              CALL BUTTON(NB,'plot [f]ilters',1)
            END IF
C
            DO ITERM=NTERM,1,-1
              CALL PGSLCT(IDN(ITERM))
              IF(ITERM.EQ.1)THEN
                CALL RPGERASW(0.00,1.00,0.00,0.80,0)
                CALL RPGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
              ELSE
                CALL PGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
              END IF
              CALL PGBOX('BCTSN',0.0,0,'BC',0.0,0)
              IF(LPVEGA) CALL PLOTVEGA(NPVEGA,WL_VEGA,FLUX_VEGA,
     +         EFLUX_VEGA,LPERR,LPABST0,CLABVEGA)
              IF(LPFILTER)THEN
                CALL PLOTFILTER(NPFILT,WL_FILT,FLUX_FILT,
     +           FILTERNAME,NFILTER)
              END IF
              IF(LPSPECT)THEN
                WRITE(CDUMMY,*) NS0
                CALL RMBLANK(CDUMMY,CDUMMY,L_)
                L=TRUELEN(INFILE)
                CLABEL_INFILE=INFILE(1:L)//' (#'//CDUMMY(1:L_)//')'
                CALL PLOTSPECTRUM(NAXIS(1),WL_SPEC,IMAGEN(1,NS0),
     +           IMAGENE(1,NS0),CLABEL_INFILE,LPERR,CERR)
              END IF
              IF(LPATM) CALL PLOTATM(NPATM,WL_ATM,FLUX_ATM)
            END DO
C..............................................................................
          ELSEIF(NB.EQ.15)THEN
            CALL BUTTON(NB,'plot [s]pect.',5)
            IF(LPSPECT)THEN
              LPSPECT=.FALSE.
              CALL BUTTON(NB,'plot [s]pect.',0)
            ELSE
              LPSPECT=.TRUE.
              CALL BUTTON(NB,'plot [s]pect.',0)
              CALL BUTTON(NB,'plot [s]pect.',1)
            END IF
            DO ITERM=NTERM,1,-1
              CALL PGSLCT(IDN(ITERM))
              IF(ITERM.EQ.1)THEN
                CALL RPGERASW(0.00,1.00,0.00,0.80,0)
                CALL RPGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
              ELSE
                CALL PGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
              END IF
              CALL PGBOX('BCTSN',0.0,0,'BC',0.0,0)
              IF(LPVEGA) CALL PLOTVEGA(NPVEGA,WL_VEGA,FLUX_VEGA,
     +         EFLUX_VEGA,LPERR,LPABST0,CLABVEGA)
              IF(LPFILTER)THEN
                CALL PLOTFILTER(NPFILT,WL_FILT,FLUX_FILT,
     +           FILTERNAME,NFILTER)
              END IF
              IF(LPSPECT)THEN
                WRITE(CDUMMY,*) NS0
                CALL RMBLANK(CDUMMY,CDUMMY,L_)
                L=TRUELEN(INFILE)
                CLABEL_INFILE=INFILE(1:L)//' (#'//CDUMMY(1:L_)//')'
                CALL PLOTSPECTRUM(NAXIS(1),WL_SPEC,IMAGEN(1,NS0),
     +           IMAGENE(1,NS0),CLABEL_INFILE(1:L),LPERR,CERR)
              END IF
              IF(LPATM) CALL PLOTATM(NPATM,WL_ATM,FLUX_ATM)
            END DO
C..............................................................................
          ELSEIF(NB.EQ.16)THEN
            CALL BUTTON(NB,'plot [a]tm.',5)
            IF(LPATM)THEN
              LPATM=.FALSE.
              CALL BUTTON(NB,'plot [a]tm.',0)
            ELSE
              LPATM=.TRUE.
              CALL BUTTON(NB,'plot [a]tm.',0)
              CALL BUTTON(NB,'plot [a]tm.',1)
            END IF
            DO ITERM=NTERM,1,-1
              CALL PGSLCT(IDN(ITERM))
              IF(ITERM.EQ.1)THEN
                CALL RPGERASW(0.00,1.00,0.00,0.80,0)
                CALL RPGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
              ELSE
                CALL PGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
              END IF
              CALL PGBOX('BCTSN',0.0,0,'BC',0.0,0)
              IF(LPVEGA) CALL PLOTVEGA(NPVEGA,WL_VEGA,FLUX_VEGA,
     +         EFLUX_VEGA,LPERR,LPABST0,CLABVEGA)
              IF(LPFILTER)THEN
                CALL PLOTFILTER(NPFILT,WL_FILT,FLUX_FILT,
     +           FILTERNAME,NFILTER)
              END IF
              IF(LPSPECT)THEN
                WRITE(CDUMMY,*) NS0
                CALL RMBLANK(CDUMMY,CDUMMY,L_)
                L=TRUELEN(INFILE)
                CLABEL_INFILE=INFILE(1:L)//' (#'//CDUMMY(1:L_)//')'
                CALL PLOTSPECTRUM(NAXIS(1),WL_SPEC,IMAGEN(1,NS0),
     +           IMAGENE(1,NS0),CLABEL_INFILE,LPERR,CERR)
              END IF
              IF(LPATM) CALL PLOTATM(NPATM,WL_ATM,FLUX_ATM)
            END DO
C..............................................................................
          ELSEIF(NB.EQ.17)THEN
            CALL BUTTON(NB,'plot AB\d\gn\u=ST\d\gl\u=0',5)
            IF(LPABST0)THEN
              LPABST0=.FALSE.
              CALL BUTTON(NB,'plot AB\d\gn\u=ST\d\gl\u=0',0)
            ELSE
              LPABST0=.TRUE.
              CALL BUTTON(NB,'plot AB\d\gn\u=ST\d\gl\u=0',0)
              CALL BUTTON(NB,'plot AB\d\gn\u=ST\d\gl\u=0',1)
            END IF
            DO ITERM=NTERM,1,-1
              CALL PGSLCT(IDN(ITERM))
              IF(ITERM.EQ.1)THEN
                CALL RPGERASW(0.00,1.00,0.00,0.80,0)
                CALL RPGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
              ELSE
                CALL PGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
              END IF
              CALL PGBOX('BCTSN',0.0,0,'BC',0.0,0)
              IF(LPVEGA) CALL PLOTVEGA(NPVEGA,WL_VEGA,FLUX_VEGA,
     +         EFLUX_VEGA,LPERR,LPABST0,CLABVEGA)
              IF(LPFILTER)THEN
                CALL PLOTFILTER(NPFILT,WL_FILT,FLUX_FILT,
     +           FILTERNAME,NFILTER)
              END IF
              IF(LPSPECT)THEN
                WRITE(CDUMMY,*) NS0
                CALL RMBLANK(CDUMMY,CDUMMY,L_)
                L=TRUELEN(INFILE)
                CLABEL_INFILE=INFILE(1:L)//' (#'//CDUMMY(1:L_)//')'
                CALL PLOTSPECTRUM(NAXIS(1),WL_SPEC,IMAGEN(1,NS0),
     +           IMAGENE(1,NS0),CLABEL_INFILE,LPERR,CERR)
              END IF
              IF(LPATM) CALL PLOTATM(NPATM,WL_ATM,FLUX_ATM)
            END DO
C..............................................................................
          ELSEIF(NB.EQ.18)THEN
            CALL BUTTON(NB,'plot [e]rrors',5)
            IF(LPERR)THEN
              LPERR=.FALSE.
              CALL BUTTON(NB,'plot [e]rrors',0)
            ELSE
              LPERR=.TRUE.
              CALL BUTTON(NB,'plot [e]rrors',0)
              CALL BUTTON(NB,'plot [e]rrors',1)
            END IF
            DO ITERM=NTERM,1,-1
              CALL PGSLCT(IDN(ITERM))
              IF(ITERM.EQ.1)THEN
                CALL RPGERASW(0.00,1.00,0.00,0.80,0)
                CALL RPGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
              ELSE
                CALL PGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
              END IF
              CALL PGBOX('BCTSN',0.0,0,'BC',0.0,0)
              IF(LPVEGA) CALL PLOTVEGA(NPVEGA,WL_VEGA,FLUX_VEGA,
     +         EFLUX_VEGA,LPERR,LPABST0,CLABVEGA)
              IF(LPFILTER)THEN
                CALL PLOTFILTER(NPFILT,WL_FILT,FLUX_FILT,
     +           FILTERNAME,NFILTER)
              END IF
              IF(LPSPECT)THEN
                WRITE(CDUMMY,*) NS0
                CALL RMBLANK(CDUMMY,CDUMMY,L_)
                L=TRUELEN(INFILE)
                CLABEL_INFILE=INFILE(1:L)//' (#'//CDUMMY(1:L_)//')'
                CALL PLOTSPECTRUM(NAXIS(1),WL_SPEC,IMAGEN(1,NS0),
     +           IMAGENE(1,NS0),CLABEL_INFILE,LPERR,CERR)
              END IF
              IF(LPATM) CALL PLOTATM(NPATM,WL_ATM,FLUX_ATM)
            END DO
C..............................................................................
          ELSEIF(NB.EQ.19)THEN
            CALL BUTTON(NB,'filter [p]ar.',5)
            WRITE(CDUMMY,*) NSIMUL
            NSIMUL=READILIM('No. of simulations (0=none)',CDUMMY,
     +      0,NSIMULMAX)
c
            DO NF=1,NFILTROS
              NPFILT_=NPFILT(NF)
              IF(NPFILT_.GT.0)THEN
                NFILTER_=NFILTER(NF)
                FILTERNAME_=FILTERNAME(NF)
                DO I=1,NPFILT(NF)
                  WL_FILT_(I)=WL_FILT(I,NF)
                  FLUX_FILT_(I)=FLUX_FILT(I,NF)
                END DO
              END IF
              IF(NPFILT_.GT.0)THEN
                WRITE(*,*)
                WRITE(*,'(A11,I1.1,A11,$)') 
     +           '>>> Filter#',NF,'.........: '
                WRITE(*,101) FILTERNAME_(1:TRUELEN(FILTERNAME_))
                LAMBDAEFF0=LAMBDAEFF(NPFILT_,WL_FILT_,FLUX_FILT_,
     +           SUM1,SUM2)
                XIFILTER0=XIFILTER(NPFILT_,WL_FILT_,FLUX_FILT_)
                WRITE(*,100) '>>> Lambda_eff.......: '
                WRITE(*,*) LAMBDAEFF0
                WRITE(*,100) '>>> Filter width.....: '
                WRITE(*,*) SUM2
                WRITE(*,100) '>>> Xi filter........: '
                WRITE(*,*) XIFILTER0
                FLUXEFFVEGA0=FLUXEFFVEGA(NPFILT_,WL_FILT_,FLUX_FILT_,
     +           NPVEGA,WL_VEGA,FLUX_VEGA,SUM1,SUM2)
                IF(NSIMUL.GT.0)THEN
                  DO K=1,NSIMUL
                    DO J=1,NPVEGA
                      R1=RANRED(NSEED)
                      R2=RANRED(NSEED)*PI2
                      FLUX_VEGA_(J)=FLUX_VEGA(J)+RAIZ2*EFLUX_VEGA(J)*
     +                 SQRT(-1.*ALOG(1.-R1))*COS(R2)
                    END DO
                    FLUXEFFVEGA_(K)=
     +               FLUXEFFVEGA(NPFILT_,WL_FILT_,FLUX_FILT_,
     +               NPVEGA,WL_VEGA,FLUX_VEGA_,SUM1,SUM2)
                  END DO
                  FLUXEFFVEGA_MEAN=FMEAN0(NSIMUL,FLUXEFFVEGA_,
     +             FLUXEFFVEGA_RMS)
                ELSE
                  FLUXEFFVEGA_MEAN=-99.999
                  FLUXEFFVEGA_RMS=-99.999
                END IF
                WRITE(*,100) '>>> Flux_eff (Vega)..: '
                WRITE(*,*) FLUXEFFVEGA0,FLUXEFFVEGA_MEAN,
     +           FLUXEFFVEGA_RMS
                LAMBDAEFFVEGA0=
     +           LAMBDAEFFVEGA(NPFILT_,WL_FILT_,FLUX_FILT_,
     +           NPVEGA,WL_VEGA,FLUX_VEGA,SUM1,SUM2)
                CTEMAG=2.5*ALOG10(SUM2)
                IF(NSIMUL.GT.0)THEN
                  DO K=1,NSIMUL
                    DO J=1,NPVEGA
                      R1=RANRED(NSEED)
                      R2=RANRED(NSEED)*PI2
                      FLUX_VEGA_(J)=FLUX_VEGA(J)+RAIZ2*EFLUX_VEGA(J)*
     +                 SQRT(-1.*ALOG(1.-R1))*COS(R2)
                    END DO
                    LAMBDAEFFVEGA_(K)=
     +               LAMBDAEFFVEGA(NPFILT_,WL_FILT_,FLUX_FILT_,
     +               NPVEGA,WL_VEGA,FLUX_VEGA_,SUM1,SUM2_(K))
                  END DO
                  LAMBDAEFFVEGA_MEAN=FMEAN0(NSIMUL,LAMBDAEFFVEGA_,
     +             LAMBDAEFFVEGA_RMS)
                  SUM2_MEAN=FMEAN0(NSIMUL,SUM2_,SUM2_RMS)
                  CTEMAG_MEAN=2.5*ALOG10(SUM2_MEAN)
                  CTEMAG_RMS=-99.999
                ELSE
                  LAMBDAEFFVEGA_MEAN=-99.999
                  LAMBDAEFFVEGA_RMS=-99.999
                  FLUXEFFVEGA_MEAN=-99.999
                  FLUXEFFVEGA_RMS=-99.999
                  CTEMAG_MEAN=-99.999
                  CTEMAG_RMS=-99.999
                END IF
                WRITE(*,100) '>>> Lambda_eff (Vega): '
                WRITE(*,*) LAMBDAEFFVEGA0,LAMBDAEFFVEGA_MEAN,
     +           LAMBDAEFFVEGA_RMS
                WRITE(*,100) '>>> Cte..............: '
                WRITE(*,*) CTEMAG,CTEMAG_MEAN,CTEMAG_RMS
              END IF
            END DO
c
            CALL BUTTON(NB,'filter [p]ar.',0)
C..............................................................................
          ELSEIF((NB.EQ.20).OR.(NB.EQ.21).OR.(NB.EQ.22))THEN
            IF(NB.EQ.20)THEN
              CALL BUTTON(NB,'[c]ompute',5)
              ZMIN=0.
              ZMAX=0.
              NREDSHIFTS=0
              EBVMIN=0.
              EBVMAX=0.
              NEBVS=0
            ELSEIF(NB.EQ.21)THEN
              CALL BUTTON(NB,'[k] correc.',5)
              EBVMIN=0.
              EBVMAX=0.
              NEBVS=0
              !recorremos todos los filtros activos y determinamos cual es el 
              !redshift maximo que podemos calcular
              LANY=.FALSE.
              DO NF=1,NFILTROS
                IF(NPFILT(NF).GT.0)THEN !el filtro esta definido
                  CALL BUTTQEX(NF+6,LBEXIST)
                  IF(LBEXIST)THEN !si el filtro esta "activo"
                    IF(LANY)THEN
                      ZCORTE=AMIN1(ZCORTE,WL_FILT(1,NF))
                    ELSE
                      ZCORTE=WL_FILT(1,NF)
                      LANY=.TRUE.
                    END IF
                  END IF
                END IF
              END DO
              IF(LANY)THEN
                ZCORTE=ZCORTE/WL_SPEC(1)-1.
                WRITE(CDUMMY,*) ZCORTE
                ZMIN=READF('Z minimum','0.0')
                ZMAX=READF('Z maximum',CDUMMY)
                WRITE(CDUMMY,*) NREDSHIFTS
                NREDSHIFTS=READI('No. of intervals',CDUMMY)
              ELSE
                ZMIN=0.
                ZMAX=0.
                NREDSHIFTS=0
              END IF
            ELSEIF(NB.EQ.22)THEN
              CALL BUTTON(NB,'E(B-V)',5)
              ZMIN=0.
              ZMAX=0.
              NREDSHIFTS=0
              !recorremos todos los filtros y vemos si hay al menos uno activo
              LANY=.FALSE.
              DO NF=1,NFILTROS
                IF(NPFILT(NF).GT.0)THEN !el filtro esta definido
                  CALL BUTTQEX(NF+6,LBEXIST)
                  IF(LBEXIST)THEN !si el filtro esta "activo"
                    LANY=.TRUE.
                  END IF
                END IF
              END DO
              IF(LANY)THEN
                WRITE(CDUMMY,*) EBVMIN
                EBVMIN=READF('E(B-V) minimum [mag]',CDUMMY)
                WRITE(CDUMMY,*) EBVMAX
                EBVMAX=READF('E(B-V) maximum [mag]',CDUMMY)
                WRITE(CDUMMY,*) NEBVS
                NEBVS=READI('No. of intervals',CDUMMY)
                IF(NEBVS.GT.0)THEN
                  WRITE(*,*)
                  WRITE(*,101) '(1) Galaxy: Savage & Mathis (1979)'
                  WRITE(*,101) '(2) Galaxy: Seaton (1979)'
                  WRITE(*,101) '(3) Galaxy: Cardelli, Clayton and '//
     +             'Mathis (1989) + O'//CHAR(39)//'Donnell (1994)'
                  WRITE(*,101) '(4) Galaxy: Fitzpatrick (1999)'
                  WRITE(*,101) '(a) LMC: Howarth (1983)'
                  WRITE(*,101) '(b) LMC (30 Doradus): Fitzpatrick(1985)'
                  WRITE(*,101) '(m) SMC: Prevot et al. (1984) + '//
     +             'Bouchet et al. (1985)'
                  WRITE(*,101) '(p) Starburst: Calzetti (1997)'
                  WRITE(*,101) '(q) Starburst: Calzetti et al. (2000)'
                  COPC_EXT(1:1)=READC('Extinction curve',COPC_EXT,
     +             '1234abmpq')
                  WRITE(CDUMMY,*) RV_EXT
                  RV_EXT=READF('RV: A(V)/E(B-V)',CDUMMY)
                END IF
              ELSE
                EBVMIN=0.
                EBVMAX=0.
                NEBVS=0
              END IF
            ELSE
              WRITE(*,100) 'FATAL ERROR: NB='
              WRITE(*,*) NB
              WRITE(*,101) 'Something wrong here!!!'
              STOP
            END IF
C numero de simulaciones
            IF(CERR.EQ.'y')THEN
              WRITE(CDUMMY,*) NSIMULERR
              NSIMULERR=READILIM(
     +         'No. of simulations for rvrebin (0=none)',CDUMMY,
     +         0,NSIMULMAX)
              IF(NSIMULERR.GT.0)THEN
                WRITE(CDUMMY,*) NSIMUL
                NSIMUL=READILIM(
     +           'No. of simulations for mag. and colours (0=none)',
     +           CDUMMY,0,NSIMULMAX)
              ELSE
                NSIMUL=0
              END IF
            ELSE
              NSIMULERR=0
              NSIMUL=0
            END IF
C medidas sobre el/los espectros
            IF(NAXIS(2).GT.1)THEN
              WRITE(*,*)
              IF(NS1.EQ.0) NS1=NS0
              WRITE(CDUMMY,*) NS1
              NS1=READILIM('First spectrum',CDUMMY,1,NAXIS(2))
              IF(NS2.EQ.0)THEN
                IF(NS0.GE.NS1)THEN
                  NS2=NS0
                ELSE
                  NS2=NS1
                END IF
              END IF
              WRITE(CDUMMY,*) NS2
              NS2=READILIM('Last  spectrum',CDUMMY,1,NAXIS(2))
              IF(NS1.GT.NS2)THEN
                NS_=NS1
                NS1=NS2
                NS2=NS_
              END IF
            ELSE
              NS1=1
              NS2=1
            END IF
            COUT(1:1)=READC('Create output file (y/n)',COUT,'yn')
            IF(COUT.EQ.'y')THEN
              LOGFILE=.TRUE.
              DO WHILE(LOGFILE)
                OUTFILE(1:80)=READC('Ouput file name','@','@')
                INQUIRE(FILE=OUTFILE,EXIST=LOGFILE)
                IF(LOGFILE)THEN
                  WRITE(*,100) 'ERROR: this file already exist.'
                  WRITE(*,101) ' Try again.'
                  WRITE(*,100) 'Press <CR> to continue...'
                  READ(*,*)
                END IF
              END DO
              OPEN(30,FILE=OUTFILE,STATUS='NEW',FORM='FORMATTED')
            END IF
            CPLOT(1:1)=READC('Plots (y/n)',CPLOT,'yn')
            WRITE(*,*)
            WRITE(*,101) '#NOTE: magnitudes are computed as in'//
     +       ' Fukugita et al. 1995, Eq. (1)'
            WRITE(*,101) '#(01) Spectrum number'
            WRITE(*,101) '#(02) Redshift'
            WRITE(*,101) '#(03) E(B-V)'
            IF(COUT.EQ.'y')THEN
              WRITE(30,101) '#NOTE: magnitudes are computed as in'//
     +         ' Fukugita et al. 1995, Eq. (1)'
              WRITE(30,101) '#(01) Spectrum number'
              WRITE(30,101) '#(02) Redshift'
              WRITE(30,101) '#(03) E(B-V)'
            END IF
            K=3
            DO NF=1,NFILTROS
              CALL BUTTQEX(NF+6,LBEXIST)
              IF((NPFILT(NF).GT.0).AND.LBEXIST)THEN
                K=K+1
                WRITE(*,'(A2,I2.2,A)') '#(',K,') Filter: '//
     +           FILTERNAME(NF)(1:TRUELEN(FILTERNAME(NF)))
                IF(COUT.EQ.'y')THEN
                  WRITE(30,'(A2,I2.2,A)') '#(',K,') Filter: '//
     +             FILTERNAME(NF)(1:TRUELEN(FILTERNAME(NF)))
                END IF
                K=K+1
                WRITE(*,'(A2,I2.2,A11,I2.2)')'#(',K,') error of ',K-1
                IF(COUT.EQ.'y')THEN
                  WRITE(30,'(A2,I2.2,A11,I2.2)')'#(',K,') error of ',K-1
                END IF
              END IF
            END DO
            K_=0
            DO NF=1,NFILTROS
              CALL BUTTQEX(NF+6,LBEXIST)
              IF(K_.EQ.0)THEN
                IF((NPFILT(NF).GT.0).AND.LBEXIST)THEN
                  K_=K_+1
                END IF
              ELSE
                IF((NPFILT(NF).GT.0).AND.LBEXIST)THEN
                  K=K+1
                  WRITE(*,'(A2,I2.2,A9,I2.2,A1,I2.2)') '#(',K,
     +             ') Color: ',2*K_+2,'-',2*K_+4
                  IF(COUT.EQ.'y')THEN
                    WRITE(30,'(A2,I2.2,A9,I2.2,A1,I2.2)') '#(',K,
     +               ') Color: ',2*K_+2,'-',2*K_+4
                  END IF
                  K_=K_+1
                  K=K+1
                  WRITE(*,'(A2,I2.2,A11,I2.2)') '#(',K,') error of ',K-1
                  IF(COUT.EQ.'y')THEN
                    WRITE(30,'(A2,I2.2,A11,I2.2)') '#(',K,') error of ',
     +               K-1
                  END IF
                END IF
              END IF
            END DO
            WRITE(*,100) '#spec  redsh.   E(B-V) '
            K=3
            DO NF=1,NFILTROS
              CALL BUTTQEX(NF+6,LBEXIST)
              IF((NPFILT(NF).GT.0).AND.LBEXIST)THEN
                K=K+1
                WRITE(*,'(3X,A1,I2.2,A1,$)') '(',K,')'
                K=K+1
                WRITE(*,'(5X,A1,I2.2,A3,$)') '(',K,')  '
              END IF
            END DO
            NF_=0
            DO NF=1,NFILTROS
              CALL BUTTQEX(NF+6,LBEXIST)
              IF(NF_.EQ.0)THEN
                IF((NPFILT(NF).GT.0).AND.LBEXIST)THEN
                  NF_=NF
                END IF
              ELSE
                IF((NPFILT(NF).GT.0).AND.LBEXIST)THEN
                  K=K+1
                  WRITE(*,'(3X,A1,I2.2,A1,$)') '(',K,')'
                  K=K+1
                  WRITE(*,'(5X,A1,I2.2,A3,$)') '(',K,')  '
                  NF_=NF
                END IF
              END IF
            END DO
            WRITE(*,*)
            IF(COUT.EQ.'y')THEN
              WRITE(30,100) '#spec  redsh.   E(B-V) '
              K=3
              DO NF=1,NFILTROS
                CALL BUTTQEX(NF+6,LBEXIST)
                IF((NPFILT(NF).GT.0).AND.LBEXIST)THEN
                  K=K+1
                  WRITE(30,'(3X,A1,I2.2,A1,$)') '(',K,')'
                  K=K+1
                  WRITE(30,'(5X,A1,I2.2,A3,$)') '(',K,')  '
                END IF
              END DO
              NF_=0
              DO NF=1,NFILTROS
                CALL BUTTQEX(NF+6,LBEXIST)
                IF(NF_.EQ.0)THEN
                  IF((NPFILT(NF).GT.0).AND.LBEXIST)THEN
                    NF_=NF
                  END IF
                ELSE
                  IF((NPFILT(NF).GT.0).AND.LBEXIST)THEN
                    K=K+1
                    WRITE(30,'(3X,A1,I2.2,A1,$)') '(',K,')'
                    K=K+1
                    WRITE(30,'(5X,A1,I2.2,A3,$)') '(',K,')  '
                    NF_=NF
                  END IF
                END IF
              END DO
              WRITE(30,*)
            END IF
C calculamos curva de extinción
            DO J=1,NAXIS(1)
              LAMBDA=CRVAL1+REAL(J-1)*CDELT1
              CALL SCOMPEXT(COPC_EXT,LAMBDA,RV_EXT,K_LAMBDA(J),
     +         LOK_EXT)
              IF(.NOT.LOK_EXT)THEN
                WRITE(*,101) 'FATAL ERROR: wavelength out '//
     +           'of limits in SCOMPEXT'
                WRITE(*,100) 'LAMBDA (Angs.): '
                WRITE(*,*) LAMBDA
                STOP
              END IF
            END DO
C medimos los espectros
            DO I=NS1,NS2
              DO NZ=1,NREDSHIFTS+1
                IF(NREDSHIFTS.GT.0)THEN
                  Z=ZMIN+(ZMAX-ZMIN)*REAL(NZ-1)/REAL(NREDSHIFTS)
                ELSE
                  Z=0.0
                END IF
                RADVEL=C*((1.+Z)*(1.+Z)-1.)/((1.+Z)*(1.+Z)+1.)
                CALL RVREBIN(RADVEL,NAXIS(1),IMAGEN(1,I),FLUX_SPEC_EBV0,
     +           CRVAL1,CDELT1)
                DO NEBV=1,NEBVS+1
                  IF(NEBVS.GT.0)THEN
                    EBV=EBVMIN+(EBVMAX-EBVMIN)*REAL(NEBV-1)/REAL(NEBVS)
                  ELSE
                    EBV=0.0
                  END IF
                  !aplicamos una extinción interestelar
                  DO J=1,NAXIS(1)
                    FACTOR_EXT=10.**(0.4*EBV*K_LAMBDA(J))
                    FLUX_SPEC(J)=FLUX_SPEC_EBV0(J)/FACTOR_EXT
                  END DO
                  IF(NSIMULERR.GT.0)THEN
                    !Para obtener la correspondiente imagen de errores, lo mejor
                    !es utilizar un metodo tipo "bootstrap" (ya que los errores
                    !de los pixels en el nuevo espectro estaran correlacionados
                    !y no es evidente calcularlos analiticamente).
                    DO K=1,NSIMULERR
                      DO J=1,NAXIS(1)
                        R1=RANRED(NSEED)
                        R2=RANRED(NSEED)*PI2
                        FLUX_SPEC_(J)=IMAGEN(J,I)+RAIZ2*IMAGENE(J,I)*
     +                   SQRT(-1.*ALOG(1.-R1))*COS(R2)
                      END DO
                      CALL RVREBIN(RADVEL,NAXIS(1),FLUX_SPEC_,
     +                 FLUX_SPEC_EBV0,CRVAL1,CDELT1)
                      DO J=1,NAXIS(1)
                        FACTOR_EXT=10.**(0.4*EBV*K_LAMBDA(J))
                        EFLUX_SPEC_(J,K)=FLUX_SPEC_EBV0(J)/FACTOR_EXT
                      END DO
                    END DO
                    DO J=1,NAXIS(1)
                      DO K=1,NSIMULERR
                        PIXEL(K)=EFLUX_SPEC_(J,K)
                      END DO
                      FDUMMY=FMEAN0(NSIMULERR,PIXEL,EFLUX_SPEC(J))
                      FLUX_RESID(J)=FLUX_SPEC(J)-FDUMMY  !esto debe ser pequeño
                    END DO
                  END IF
                  IF(CPLOT.EQ.'y')THEN
                    DO ITERM=NTERM,1,-1
                      CALL PGSLCT(IDN(ITERM))
                      IF(ITERM.EQ.1)THEN
                        CALL RPGERASW(0.00,1.00,0.00,0.80,0)
                        CALL RPGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
                      ELSE
                        CALL PGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
                      END IF
                      CALL PGBOX('BCTSN',0.0,0,'BC',0.0,0)
                      IF(LPVEGA) CALL PLOTVEGA(NPVEGA,WL_VEGA,FLUX_VEGA,
     +                 EFLUX_VEGA,LPERR,LPABST0,CLABVEGA)
                      IF(LPFILTER)THEN
                        CALL PLOTFILTER(NPFILT,WL_FILT,FLUX_FILT,
     +                   FILTERNAME,NFILTER)
                      END IF
                      IF(LPSPECT)THEN
                        WRITE(CDUMMY,*) I
                        CALL RMBLANK(CDUMMY,CDUMMY,L_)
                        L=TRUELEN(INFILE)
                        CLABEL_INFILE=
     +                   INFILE(1:L)//' (#'//CDUMMY(1:L_)//')'
                        IF(NSIMULERR.GT.0)THEN
                          CALL PLOTSPECTRUM2(NAXIS(1),WL_SPEC,FLUX_SPEC,
     +                     EFLUX_SPEC,FLUX_RESID,CLABEL_INFILE,
     +                     LPERR,CERR)
                        ELSE
                          CALL PLOTSPECTRUM(NAXIS(1),WL_SPEC,FLUX_SPEC,
     +                     EFLUX_SPEC,CLABEL_INFILE,LPERR,CERR)
                        END IF
                        CALL PGSCI(2)
                        WRITE(CDUMMY,'(F6.4)') Z
                        CALL RMBLANK(CDUMMY,CDUMMY,L_)
                        CALL PGMTEXT('B',3.0,0.00,0.0,
     +                   'z='//CDUMMY(1:L_))
                        WRITE(CDUMMY,'(F6.3)') EBV
                        CALL RMBLANK(CDUMMY,CDUMMY,L_)
                        CALL PGMTEXT('B',3.0,1.00,1.0,
     +                   'E(B-V)='//CDUMMY(1:L_))
                        CALL PGSCI(1)
                      END IF
                      IF(LPATM) CALL PLOTATM(NPATM,WL_ATM,FLUX_ATM)
                    END DO
                  END IF
                  DO NF=1,NFILTROS
                    LMIDE(NF)=.FALSE.
                    CALL BUTTQEX(NF+6,LBEXIST)
                    IF((NPFILT(NF).GT.0).AND.LBEXIST)THEN
                      IF(
     +                 (WL_FILT(1,NF).GE.
     +                  WL_SPEC(1)*(1.+Z)).AND.
     +                 (WL_FILT(NPFILT(NF),NF).LE.
     +                  WL_SPEC(NAXIS(1))*(1.+Z)))THEN
                        LMIDE(NF)=.TRUE.
                      END IF
                    END IF
                  END DO
                  DO NF=1,NFILTROS
                    MAG(NF)=-99.999
                    MAG_MEAN(NF)=-99.999
                    MAG_RMS(NF)=-99.999
                  END DO
                  DO NF=1,NFILTROS
                    COLOR(NF)=-99.999
                    COLOR_MEAN(NF)=-99.999
                    COLOR_RMS(NF)=-99.999
                  END DO
c
                  DO NF=1,NFILTROS
                    IF(LMIDE(NF))THEN
                      MAG(NF)=MAGNITUDE(
     +                 NPFILT(NF),WL_FILT(1,NF),FLUX_FILT(1,NF),
     +                 NPVEGA,WL_VEGA,FLUX_VEGA,
     +                 NAXIS(1),WL_SPEC,FLUX_SPEC,SUM1,SUM2)
                    END IF
                  END DO
c
                  NF_=0
                  DO NF=1,NFILTROS
                    CALL BUTTQEX(NF+6,LBEXIST)
                    IF(NF_.EQ.0)THEN
                      IF((NPFILT(NF).GT.0).AND.LBEXIST)THEN
                        NF_=NF
                      END IF
                    ELSE
                      IF((NPFILT(NF).GT.0).AND.LBEXIST)THEN
                        IF(LMIDE(NF).AND.LMIDE(NF_))THEN
                          COLOR(NF_)=MAG(NF_)-MAG(NF)
                        END IF
                        NF_=NF
                      END IF
                    END IF
                  END DO
c OJO: los errores en MAG(NF) estan correlacionados => tenemos que
c calcular los valores de MAG(NF) en cada simulacion usando los 
C mismos espectros FLUX_VEGA y FLUX_SPEC.
                  IF((NSIMUL.GT.0).AND.(CERR.EQ.'y'))THEN
                    DO K=1,NSIMUL
                      DO J=1,NPVEGA
                        R1=RANRED(NSEED)
                        R2=RANRED(NSEED)*PI2
                        FLUX_VEGA_(J)=FLUX_VEGA(J)+RAIZ2*EFLUX_VEGA(J)*
     +                   SQRT(-1.*ALOG(1.-R1))*COS(R2)
                      END DO
                      DO J=1,NAXIS(1)
                        R1=RANRED(NSEED)
                        R2=RANRED(NSEED)*PI2
                        FLUX_SPEC_(J)=FLUX_SPEC(J)+RAIZ2*EFLUX_SPEC(J)*
     +                   SQRT(-1.*ALOG(1.-R1))*COS(R2)
                      END DO
                      DO NF=1,NFILTROS
                        IF(LMIDE(NF))THEN
                          MAG_(K,NF)=MAGNITUDE(
     +                     NPFILT(NF),WL_FILT(1,NF),FLUX_FILT(1,NF),
     +                     NPVEGA,WL_VEGA,FLUX_VEGA_,
     +                     NAXIS(1),WL_SPEC,FLUX_SPEC_,SUM1,SUM2)
                        END IF
                      END DO
                      NF_=0
                      DO NF=1,NFILTROS
                        CALL BUTTQEX(NF+6,LBEXIST)
                        IF(NF_.EQ.0)THEN
                          IF((NPFILT(NF).GT.0).AND.LBEXIST)THEN
                            NF_=NF
                          END IF
                        ELSE
                          IF((NPFILT(NF).GT.0).AND.LBEXIST)THEN
                            IF(LMIDE(NF).AND.LMIDE(NF_))THEN
                              COLOR_(K,NF_)=MAG_(K,NF_)-MAG_(K,NF)
                            END IF
                            NF_=NF
                          END IF
                        END IF
                      END DO
                    END DO
                    DO NF=1,NFILTROS
                      IF(LMIDE(NF))THEN
                        MAG_MEAN(NF)=FMEAN0(NSIMUL,MAG_(1,NF),
     +                   MAG_RMS(NF))
                      END IF
                    END DO
                    NF_=0
                    DO NF=1,NFILTROS
                      CALL BUTTQEX(NF+6,LBEXIST)
                      IF(NF_.EQ.0)THEN
                        IF((NPFILT(NF).GT.0).AND.LBEXIST)THEN
                          NF_=NF
                        END IF
                      ELSE
                        IF((NPFILT(NF).GT.0).AND.LBEXIST)THEN
                          IF(LMIDE(NF).AND.LMIDE(NF_))THEN
                            COLOR_MEAN(NF_)=
     +                       FMEAN0(NSIMUL,COLOR_(1,NF_),COLOR_RMS(NF_))
                          END IF
                          NF_=NF
                        END IF
                      END IF
                    END DO
                  END IF
c
                  WRITE(*,'(I5,2X,F6.4,2X,F6.3,$)') I,Z,EBV
                  DO NF=1,NFILTROS
                    CALL BUTTQEX(NF+6,LBEXIST)
                    IF((NPFILT(NF).GT.0).AND.LBEXIST)THEN
                      WRITE(*,'(2(2X,F7.3),$)') MAG(NF),MAG_RMS(NF)
                    END IF
                  END DO
                  NF_=0
                  DO NF=1,NFILTROS
                    CALL BUTTQEX(NF+6,LBEXIST)
                    IF(NF_.EQ.0)THEN
                      IF((NPFILT(NF).GT.0).AND.LBEXIST)THEN
                        NF_=NF
                      END IF
                    ELSE
                      IF((NPFILT(NF).GT.0).AND.LBEXIST)THEN
                        WRITE(*,'(2(2X,F7.3),$)')
     +                   COLOR(NF_),COLOR_RMS(NF_)
        !la siguiente línea genera un fichero fort.8? para cada color,
        !conteniendo la SN en los espectros, el color y el error
        write(80+nf_,*)imagen_snrat(i),color(nf_),color_rms(nf_)
                        NF_=NF
                      END IF
                    END IF
                  END DO
                  WRITE(*,*)
                  IF(COUT.EQ.'y')THEN
                    WRITE(30,'(I5,2X,F6.4,2X,F6.3,$)') I,Z,EBV
                    DO NF=1,NFILTROS
                      CALL BUTTQEX(NF+6,LBEXIST)
                      IF((NPFILT(NF).GT.0).AND.LBEXIST)THEN
                        WRITE(30,'(2(2X,F7.3),$)') MAG(NF),MAG_RMS(NF)
                      END IF
                    END DO
                    NF_=0
                    DO NF=1,NFILTROS
                      CALL BUTTQEX(NF+6,LBEXIST)
                      IF(NF_.EQ.0)THEN
                        IF((NPFILT(NF).GT.0).AND.LBEXIST)THEN
                          NF_=NF
                        END IF
                      ELSE
                        IF((NPFILT(NF).GT.0).AND.LBEXIST)THEN
                          WRITE(30,'(2(2X,F7.3),$)') COLOR(NF_),
     +                     COLOR_RMS(NF_)
                          NF_=NF
                        END IF
                      END IF
                    END DO
                    WRITE(30,*)
                  END IF
                END DO
              END DO
            END DO
            IF(COUT.EQ.'y') CLOSE(30)
            WRITE(*,101) '...OK!'
            IF(NB.EQ.20)THEN
              CALL BUTTON(NB,'[c]ompute',0)
            ELSEIF(NB.EQ.21)THEN
              CALL BUTTON(NB,'[k] correc.',0)
            ELSE
              CALL BUTTON(NB,'E(B-V)',0)
            END IF
C..............................................................................
          ELSEIF(NB.EQ.23)THEN
            CALL BUTTON(NB,'Xi error',5)
            IF1=READI('First filter number','@')
            IF2=READI('Last filter number','@')
            NUNIT=READI('Unit number for ouput file fort.?? (0=none)',
     +       '0')
            IF(IF1.GT.IF2)THEN
              WRITE(*,101) 'ERROR: invalid numbers!'
              WRITE(*,100) 'Press <CR> to continue...'
              READ(*,*)
            ELSE
              DO NFILTER_=IF1,IF2
                CALL LEEFILTER(XMINV,XMAXV,NFILTER_,
     +           NPFILT_,WL_FILT_,FLUX_FILT_,FILTERNAME_)
                IF(NPFILT_.GT.0)THEN
                  WRITE(*,100) '>>> Filter width  (A): '
                  WRITE(*,*) FILTERWIDTH(NPFILT_,WL_FILT_,FLUX_FILT_)
                  XIFILTER0=XIFILTER(NPFILT_,WL_FILT_,FLUX_FILT_)
                  WRITE(*,100) '>>> Xi filter (x1E-4): '
                  WRITE(*,*) XIFILTER0*10000.0
                  IF(NUNIT.GT.0)THEN
                    WRITE(NUNIT,*) 
     +               FILTERWIDTH(NPFILT_,WL_FILT_,FLUX_FILT_),
     +               XIFILTER0*10000.0
                  END IF
                END IF
              END DO
            END IF
            CALL BUTTON(NB,'Xi error',0)
C..............................................................................
          ELSEIF(NB.EQ.24)THEN
            CALL BUTTON(NB,'[.] fit SED',5)
            CMODEL_SED='1'
            DO WHILE(CMODEL_SED.NE.'x')
              WRITE(*,*)
              WRITE(*,101) '(1) Starburst99'
              WRITE(*,101) '(2) PEGASE'
              WRITE(*,101) '(3) CWW 1980'
              WRITE(*,101) '(x) EXIT'
              CMODEL_SED(1:1)=READC('Model (x/1/2/3)','x','x123')
              IF(CMODEL_SED.NE.'x')THEN
                CALL FIT_SED(CMODEL_SED,CLABVEGA,XMINV,XMAXV,
     +           NPVEGA,WL_VEGA,FLUX_VEGA)
              END IF
            END DO
            !restaura los filtros y espectro dibujados antes de entrar aqui
            DO ITERM=NTERM,1,-1
              CALL PGSLCT(IDN(ITERM))
              IF(ITERM.EQ.1)THEN
                CALL RPGERASW(0.00,1.00,0.00,0.80,0)
                CALL RPGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
              ELSE
                CALL PGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
              END IF
              CALL PGBOX('BCTSN',0.0,0,'BC',0.0,0)
              IF(LPVEGA) CALL PLOTVEGA(NPVEGA,WL_VEGA,FLUX_VEGA,
     +         EFLUX_VEGA,LPERR,LPABST0,CLABVEGA)
              IF(LPFILTER)THEN
                CALL PLOTFILTER(NPFILT,WL_FILT,FLUX_FILT,
     +           FILTERNAME,NFILTER)
              END IF
              IF(LPSPECT)THEN
                WRITE(CDUMMY,*) NS0
                CALL RMBLANK(CDUMMY,CDUMMY,L_)
                L=TRUELEN(INFILE)
                CLABEL_INFILE=INFILE(1:L)//' (#'//CDUMMY(1:L_)//')'
                CALL PLOTSPECTRUM(NAXIS(1),WL_SPEC,IMAGEN(1,NS0),
     +           IMAGENE(1,NS0),CLABEL_INFILE,LPERR,CERR)
              END IF
              IF(LPATM) CALL PLOTATM(NPATM,WL_ATM,FLUX_ATM)
            END DO
            CALL BUTTON(NB,'[.] fit SED',0)
C..............................................................................
          END IF
        END DO
C------------------------------------------------------------------------------
        CALL PGEND
C------------------------------------------------------------------------------
        STOP
100     FORMAT(A,$)
101     FORMAT(A)
        END
