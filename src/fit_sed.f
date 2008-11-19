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
C Determine the SED that matches a given set of photometric measurements, using
C the models given in:
C (1) starburst99 at http://www.stsci.edu/science/starburst99/
C (2) PEGASE at http://www.iap.fr/users/fioc/
C (3) CWW80 (Coleman, Wu & Weedman 1980), from Hyperz v1.1
C
        SUBROUTINE FIT_SED(CMODEL_SED,CLABVEGA,XMINV,XMAXV,
     +   NPVEGA,WL_VEGA,FLUX_VEGA)
        IMPLICIT NONE
        CHARACTER*1 CMODEL_SED
        CHARACTER*20 CLABVEGA
        REAL XMINV,XMAXV
        INTEGER NPVEGA
        REAL WL_VEGA(NPVEGA),FLUX_VEGA(NPVEGA)
C
        INCLUDE 'photometry.inc'
C
        INTEGER NMAX_FILTERS                !numero maximo de filtros a ajustar
        PARAMETER (NMAX_FILTERS=99)
C
        INTEGER NMAX_AGES_SB99,NMAX_PARAM2_SB99
        PARAMETER (NMAX_AGES_SB99=36,NMAX_PARAM2_SB99=5)
        INTEGER NMAX_AGES_PEGA,NMAX_PARAM2_PEGA
        PARAMETER (NMAX_AGES_PEGA=69,NMAX_PARAM2_PEGA=1)
        INTEGER NMAX_AGES_CWW,NMAX_PARAM2_CWW
        PARAMETER (NMAX_AGES_CWW=4,NMAX_PARAM2_CWW=1)
        INTEGER NMAX_AGES,NMAX_PARAM2
        PARAMETER (NMAX_AGES=69,NMAX_PARAM2=7) !tienen que ser los mayores entre
                                              !todos los valores de los modelos
C
        INTEGER TRUEBEG,TRUELEN
        INTEGER SYSTEMFUNCTION
        REAL READF
        REAL LAMBDAEFF
        REAL LAMBDAEFFVEGA
        REAL MAGNITUDE
        REAL LININTERP
        CHARACTER*255 READC
C variables globales (COMMON`s)
        INTEGER NAXIS(2)
        REAL IMAGEN_(NXMAX,NYMAX)
C
        INTEGER I,J,K,JJ
        INTEGER L,L1,L2
        INTEGER LL1,LL2
        INTEGER NAGES,NPARAM2
        INTEGER NFILTERS
        INTEGER NFILTER(NMAX_FILTERS)
        INTEGER NPFILT(NMAX_FILTERS)
        INTEGER IAGE,IPARAM2,IAGE_MINCHISQR,IPARAM2_MINCHISQR
        INTEGER ITERM,NTERM,IDN(8)
        INTEGER NCOLOR
        INTEGER NPTFLUX_MODEL
        INTEGER NAGE(NMAX_AGES)
        INTEGER NAGE_SB99(NMAX_AGES)
        INTEGER NAGE_PEGA(NMAX_AGES)
        INTEGER NAGE_CWW(NMAX_AGES)
        INTEGER MINAGE,MAXAGE
        INTEGER N_EL,NSKIP_EL
        INTEGER IFLAG,IDUM1,IDUM2
        INTEGER ISYSTEM
        REAL Z !redshift
        REAL PARAM2(NMAX_PARAM2) !second parameter: e.g. metallicity
        REAL PARAM2_SB99(NMAX_PARAM2) !metallicity in Starburst99
        REAL PARAM2_PEGA(NMAX_PARAM2)
        REAL PARAM2_CWW(NMAX_PARAM2)
        REAL MAG(NMAX_FILTERS),ERRMAG(NMAX_FILTERS)
        REAL WL_FILT(NPMAX,NMAX_FILTERS),FLUX_FILT(NPMAX,NMAX_FILTERS)
        REAL FWIDTH(NMAX_FILTERS),CTEFLUX(NMAX_FILTERS)
        REAL FLUX_AVE(NMAX_FILTERS),ERRFLUX_AVE(NMAX_FILTERS)
        REAL FLUX_LDO(NMAX_FILTERS),ERRFLUX_LDO(NMAX_FILTERS)
        REAL XMIN,XMAX,DX,YMIN,YMAX,DY
        REAL YMINFINAL,YMAXFINAL
        REAL XMINCHI,XMAXCHI,YMINCHI,YMAXCHI
        REAL YMAXFILTER
        REAL YMINMODEL,YMAXMODEL,DYMODEL
        REAL YMINMODEL_BEST,YMAXMODEL_BEST
        REAL WL_MODEL(NPMAX),FLUX_MODEL(NPMAX,NMAX_AGES)
        REAL FLUX_MODEL_BEST(NPMAX)
        REAL FLUXMEAN_MODEL(NMAX_FILTERS)
        REAL FLUXMEAN_MODEL_BEST(NMAX_FILTERS)
        REAL MAG_MODEL(NMAX_FILTERS)
        REAL SUM1,SUM2
        REAL FACTOR_FLUXMAX,FACTOR_FLUXMAX_MODEL
        REAL FLUXMIN,FLUXMAX,DFLUX
        REAL FLUXMIN_MODEL,FLUXMAX_MODEL,DFLUX_MODEL
        REAL CHISQR(NMAX_AGES,NMAX_PARAM2),CHISQRMIN
        REAL LOGCHISQR(NMAX_AGES,NMAX_PARAM2)
        REAL BCHISQR,BCHISQR_BEST
        REAL EBV_GALEXT,EBV_INTRED,RV,K_LAMBDA,WL0,WL1,WL2
        REAL TR(6),BG_CHISQR,FG_CHISQR
        REAL FDUM
        REAL FACTORFINAL
        DOUBLE PRECISION DP_FLUX_MODEL(NPMAX)
        CHARACTER*1 CIMF,CSFR
        CHARACTER*1 CURVE_GALEXT,CURVE_INTRED
        CHARACTER*1 CPLOT
        CHARACTER*4 CSYSTEM
        CHARACTER*80 FILEDATA,OUTFILE
        CHARACTER*50 CDUMMY
        CHARACTER*80 FILTERNAME(NMAX_FILTERS)
        CHARACTER*255 CLINEA
        CHARACTER*255 STARBURST99_DIR
        CHARACTER*255 PEGASE_DIR
        CHARACTER*255 INFILE
        LOGICAL LOGFILE
        LOGICAL LOK
        LOGICAL LPLOT
        LOGICAL LFIRST_CHISQR
C
        COMMON/BLKGRAPHIC/NTERM,IDN
        COMMON/BLKIMAGEN_/IMAGEN_                                  !imagen FITS
        COMMON/BLKNAXIS/NAXIS                                      !dimensiones
C------------------------------------------------------------------------------
        DATA(NAGE_SB99(IAGE),IAGE=1,NMAX_AGES_SB99) 
     +   /1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
     +   30,40,50,60,70,80,90,100,200,300,400,500,600,700,800,900/
        DATA(PARAM2_SB99(IPARAM2),IPARAM2=1,NMAX_PARAM2_SB99) 
     +   /0.0400,0.0200,0.0080,0.0040,0.0010/
C
ccc     !NOTE: NAGE_PEGA(IAGE) is read from data file
        DATA(PARAM2_PEGA(IPARAM2),IPARAM2=1,NMAX_PARAM2_PEGA) /1.0/
C
        DATA(NAGE_CWW(IAGE),IAGE=1,NMAX_AGES_CWW) /1,2,3,4/ 
        DATA(PARAM2_CWW(IPARAM2),IPARAM2=1,NMAX_PARAM2_PEGA) /1.0/
C limites en l.d.o. para corregir de extincion
        WL1=1216.0
        WL2=22000.0
C
        CHISQRMIN=0.0 !evita warning de compilacion
        BCHISQR_BEST=0.0   !evita warning de compilacion
C------------------------------------------------------------------------------
        IF(CMODEL_SED.EQ.'1')THEN !Starburst99
          NAGES=NMAX_AGES_SB99
          DO IAGE=1,NMAX_AGES_SB99
            NAGE(IAGE)=NAGE_SB99(IAGE)
          END DO
          NPARAM2=NMAX_PARAM2_SB99
          DO IPARAM2=1,NMAX_PARAM2_SB99
            PARAM2(IPARAM2)=PARAM2_SB99(IPARAM2)
          END DO
        ELSEIF(CMODEL_SED.EQ.'2')THEN !PEGASE
ccc       NAGES=NMAX_AGES_PEGA
ccc       DO IAGE=1,NMAX_AGES_PEGA
ccc         NAGE(IAGE)=NAGE_PEGA(IAGE)
ccc       END DO
          NPARAM2=NMAX_PARAM2_PEGA
          DO IPARAM2=1,NMAX_PARAM2_PEGA
            PARAM2(IPARAM2)=PARAM2_PEGA(IPARAM2)
          END DO
        ELSEIF(CMODEL_SED.EQ.'3')THEN !PEGASE
          NAGES=NMAX_AGES_CWW
          DO IAGE=1,NMAX_AGES_CWW
            NAGE(IAGE)=NAGE_CWW(IAGE)
          END DO
          NPARAM2=NMAX_PARAM2_CWW
          DO IPARAM2=1,NMAX_PARAM2_CWW
            PARAM2(IPARAM2)=PARAM2_CWW(IPARAM2)
          END DO
        ELSE
          WRITE(*,100) 'CMODEL_SED: '
          WRITE(*,*) CMODEL_SED
          WRITE(*,101) 'ERROR: invalid CMODEL_SED'
          WRITE(*,100) 'Press <CR> to continue...'
          READ(*,*)
          RETURN
        END IF
C------------------------------------------------------------------------------
        TR(1)=0.
        TR(2)=1.
        TR(3)=0.
        TR(4)=0.
        TR(5)=0.
        TR(6)=1.
C------------------------------------------------------------------------------
        LOGFILE=.FALSE.
        DO WHILE(.NOT.LOGFILE)
          INFILE=READC('Input *.mag file','*.mag','@')
          IF((INDEX(INFILE,'*').NE.0).OR.(INDEX(INFILE,'?').NE.0))THEN
            ISYSTEM=SYSTEMFUNCTION('ls '//INFILE(1:TRUELEN(INFILE)))
          ELSE
            INQUIRE(FILE=INFILE,EXIST=LOGFILE)
            IF(.NOT.LOGFILE)THEN !fichero no existe
              WRITE(*,100) 'ERROR: this file does not exist.'
              WRITE(*,101) ' Try again.'
              WRITE(*,100) 'Press <CR> to continue...'
              READ(*,*)
            END IF
          END IF
        END DO
C------------------------------------------------------------------------------
C Open *.mag file
        OPEN(11,FILE=INFILE,STATUS='OLD',FORM='FORMATTED')
C Starburst99 data directory
        READ(11,101,ERR=900) CLINEA
        L1=TRUEBEG(CLINEA)
        I=INDEX(CLINEA,'#')
        IF(I.EQ.0)THEN !no hay comentario (?)
          L2=TRUELEN(CLINEA)
        ELSE
          L2=TRUELEN(CLINEA(1:I-1))
        END IF
        STARBURST99_DIR=CLINEA(L1:L2)
        IF(CMODEL_SED.EQ.'1')THEN !chequeamos que existe
          WRITE(*,100) '> Directory: '
          WRITE(*,100) STARBURST99_DIR(L1:L2)
          INQUIRE(FILE=STARBURST99_DIR,EXIST=LOGFILE)
          IF(.NOT.LOGFILE)THEN
            CLOSE(11)
            WRITE(*,*)
            WRITE(*,101) 'ERROR: this directory does not exist.'
            WRITE(*,100) 'Press <CR> to continue...'
            READ(*,*)
            RETURN
          ELSE
            WRITE(*,101) ' OK!'
          END IF
        END IF
C PEGASE data directory
        READ(11,101,ERR=900) CLINEA
        L1=TRUEBEG(CLINEA)
        I=INDEX(CLINEA,'#')
        IF(I.EQ.0)THEN !no hay comentario (?)
          L2=TRUELEN(CLINEA)
        ELSE
          L2=TRUELEN(CLINEA(1:I-1))
        END IF
        PEGASE_DIR=CLINEA(L1:L2)
        IF(CMODEL_SED.EQ.'2')THEN !chequeamos que existe
          WRITE(*,100) '> Directory: '
          WRITE(*,100) PEGASE_DIR(L1:L2)
          INQUIRE(FILE=PEGASE_DIR,EXIST=LOGFILE)
          IF(.NOT.LOGFILE)THEN
            CLOSE(11)
            WRITE(*,*)
            WRITE(*,101) 'ERROR: this directory does not exist.'
            WRITE(*,100) 'Press <CR> to continue...'
            READ(*,*)
            RETURN
          ELSE
            WRITE(*,101) ' OK!'
          END IF
        END IF
C Total number of filters to be fitted
        READ(11,101,ERR=900) CLINEA
        READ(CLINEA,*) NFILTERS
        WRITE(*,100) '> No. of filters to be fitted: '
        WRITE(*,*) NFILTERS
        IF(NFILTERS.GT.NMAX_FILTERS)THEN
          CLOSE(11)
          WRITE(*,101) 'ERROR: NFILTERS.GT.NMAX_FILTERS'
          WRITE(*,100) 'Press <CR> to continue...'
          READ(*,*)
          RETURN
        END IF
C Photometric system: VEGA, AB or ST only!
        READ(11,101,ERR=900) CLINEA
        L1=TRUEBEG(CLINEA)
        I=INDEX(CLINEA,'#')
        IF(I.EQ.0)THEN !no hay comentario (?)
          L2=TRUELEN(CLINEA)
        ELSE
          L2=TRUELEN(CLINEA(1:I-1))
        END IF
        CSYSTEM=CLINEA(L1:L2)
        IF((CSYSTEM.EQ.'VEGA').AND.(CLABVEGA.EQ.'\ga Lyr'))THEN
          WRITE(*,101) '> Vega system OK!'
        ELSEIF((CSYSTEM.EQ.'AB').AND.(CLABVEGA.EQ.'AB\d\gn\u'))THEN
          WRITE(*,101) '> AB system OK!'
        ELSEIF((CSYSTEM.EQ.'ST').AND.(CLABVEGA.EQ.'ST\d\gl\u'))THEN
          WRITE(*,101) '> ST system OK!'
        ELSE
          CLOSE(11)
          WRITE(*,100) 'Photometric system: '
          WRITE(*,101) CSYSTEM
          WRITE(*,101) 'ERROR: invalid photometric system.'
          WRITE(*,100) 'Press <CR> to continue...'
          READ(*,*)
          RETURN
        END IF
C Redshift
        READ(11,101,ERR=900) CLINEA
        READ(CLINEA,*) Z
        WRITE(*,100) '> Redshift: '
        WRITE(*,*) Z
C Leemos filtros, magnitudes y errores
        DO I=1,NFILTERS
          READ(11,101,ERR=900) CLINEA
          READ(CLINEA,*) NFILTER(I)
          WRITE(*,100) '> Filter ID number: '
          WRITE(*,*) NFILTER(I)
          CALL LEEFILTER(XMINV,XMAXV,NFILTER(I),
     +     NPFILT(I),WL_FILT(1,I),FLUX_FILT(1,I),FILTERNAME(I))
          YMAXFILTER=FLUX_FILT(1,I) !Normalizamos la curva respuesta del filtro
          DO J=2,NPFILT(I)
            YMAXFILTER=AMAX1(YMAXFILTER,FLUX_FILT(J,I))
          END DO
          DO J=1,NPFILT(I)
            FLUX_FILT(J,I)=FLUX_FILT(J,I)/YMAXFILTER
          END DO
          READ(11,101,ERR=900) CLINEA
          READ(CLINEA,*) MAG(I),ERRMAG(I)
          WRITE(*,100) '> Magnitude and error: '
          WRITE(*,*) MAG(I),ERRMAG(I)
        END DO
C Leemos edad minima y maxima a emplear en los modelos
        READ(11,101,ERR=900) CLINEA
        READ(CLINEA,*) MINAGE
        READ(11,101,ERR=900) CLINEA
        READ(CLINEA,*) MAXAGE
        WRITE(*,100) '> Minimum and maximum age (Myr): '
        WRITE(*,*) MINAGE,MAXAGE
        IF(CMODEL_SED.EQ.'3')THEN !no queremos que afecte
          WRITE(*,101) ' WARNING: these values will not be employed'
          MINAGE=-100000000
          MAXAGE=+100000000
        END IF
C Close *.mag file
        CLOSE(11)
C------------------------------------------------------------------------------
C Pedir IMF y tipo de SF
        IF(CMODEL_SED.EQ.'1')THEN
          WRITE(*,*)
          WRITE(*,101) '(1) IMF Salpeter (1955), Mass: 1 - 100 M_sun'
          WRITE(*,101) '(2) IMF alpha=3.30, Mass: 1 - 100 M_sun'
          WRITE(*,101) '(3) IMF alpha=2.35, Mass: 1 - 30 M_sun'
          CIMF(1:1)=READC('Option (1/2/3)','1','123')
          WRITE(*,*)
          WRITE(*,101) '(1) Instantaneous burst'
          WRITE(*,101) '(2) Continuous star formation'
          CSFR(1:1)=READC('Option (1/2)','1','12')
        ELSEIF(CMODEL_SED.EQ.'2')THEN
          WRITE(*,*)
          WRITE(*,101) '(1) IMF Salpeter (1955), Mass: 0.1 - 120 M_sun'
          CIMF(1:1)=READC('Option (1)','1','1')
          WRITE(*,*)
          WRITE(*,101) '(1) Instantaneous burst'
          CSFR(1:1)=READC('Option (1)','1','1')
        ELSEIF(CMODEL_SED.EQ.'3')THEN
        END IF
C------------------------------------------------------------------------------
C Galactic extinction
        WRITE(*,*)
        WRITE(*,101) '(1) Galaxy: Savage & Mathis (1979)'
        WRITE(*,101) '(2) Galaxy: Seaton (1979)'
        WRITE(*,101) '(3) Galaxy: Cardelli, Clayton and Mathis (1989)'//
     +   ' + O'//CHAR(39)//'Donnell (1994)'
        WRITE(*,101) '(4) Galaxy: Fitzpatrick (1999)'
        WRITE(*,101) '(a) LMC: Howarth (1983)'
        WRITE(*,101) '(b) LMC (30 Doradus): Fitzpatrick (1985)'
        WRITE(*,101) '(m) SMC: Prevot et al. (1984) + '//
     +   'Bouchet et al. (1985)'
        WRITE(*,101) '(p) Starburst: Calzetti (1997)'
        WRITE(*,101) '(q) Starburst: Calzetti et al. (2000)'
        CURVE_GALEXT(1:1)=
     +   READC('Galactic extinction curve','4','1234abmpq')
        EBV_GALEXT=READF('Galactic extinction E(B-V)','0.0')
C------------------------------------------------------------------------------
C Internal reddening
        WRITE(*,*)
        WRITE(*,101) '(1) Galaxy: Savage & Mathis (1979)'
        WRITE(*,101) '(2) Galaxy: Seaton (1979)'
        WRITE(*,101) '(3) Galaxy: Cardelli, Clayton and Mathis (1989)'//
     +   ' + O'//CHAR(39)//'Donnell (1994)'
        WRITE(*,101) '(4) Galaxy: Fitzpatrick (1999)'
        WRITE(*,101) '(a) LMC: Howarth (1983)'
        WRITE(*,101) '(b) LMC (30 Doradus): Fitzpatrick (1985)'
        WRITE(*,101) '(m) SMC: Prevot et al. (1984) + '//
     +   'Bouchet et al. (1985)'
        WRITE(*,101) '(p) Starburst: Calzetti (1997)'
        WRITE(*,101) '(q) Starburst: Calzetti et al. (2000)'
        CURVE_INTRED(1:1)=
     +   READC('Internal reddening curve','4','1234abmpq')
        EBV_INTRED=READF('Internal reddening E(B-V)','0.0')
C------------------------------------------------------------------------------
C si corregimos de reddening, pedimos el intervalo en el que vamos a realizar
C la correccion
        IF((EBV_GALEXT.GT.0.0).OR.(EBV_INTRED.GT.0.0))THEN
          RV=3.1
          WRITE(*,*)
          WRITE(CDUMMY,*) WL1
          WL1=READF('Minimum wavelength to correct for extinction '//
     +     '(angstroms)',CDUMMY)
          WRITE(CDUMMY,*) WL2
          WL2=READF('Maximum wavelength to correct for extinction '//
     +     '(angstroms)',CDUMMY)
          WRITE(*,*)
        END IF
        WRITE(*,*)
        CPLOT(1:1)=READC('Show intermediate plots (y/g/n)','g','ygn')
        LPLOT=((CPLOT.EQ.'y').OR.(CPLOT.EQ.'g'))
C------------------------------------------------------------------------------
C Buscamos limites en longitud de onda (hace falta incluso con LPLOT=.FALSE.)
        XMIN=WL_FILT(1,1)
        XMAX=WL_FILT(NPFILT(1),1)
        IF(NFILTERS.GT.1)THEN
          DO I=2,NFILTERS
            IF(WL_FILT(1,I).LE.XMIN) XMIN=WL_FILT(1,I)
            IF(WL_FILT(NPFILT(I),I).GT.XMAX) XMAX=WL_FILT(NPFILT(I),I)
          END DO
        END IF
        DX=XMAX-XMIN
        XMIN=XMIN-DX/20.
        XMAX=XMAX+DX/20.
        YMIN=0.0
        YMAX=1.0
        DY=YMAX-YMIN
        YMIN=YMIN-DY/20.
        YMAX=YMAX+DY/10.
C Plot filter region
        IF(LPLOT)THEN
          DO ITERM=NTERM,1,-1
            CALL PGSLCT(IDN(ITERM))
            IF(ITERM.EQ.1)THEN
              CALL RPGERASW(0.00,1.00,0.00,0.80,0)
              CALL RPGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
            ELSE
              CALL PGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
            END IF
            CALL PGBOX('BTSN',0.0,0,'BCTSN',0.0,0)
            CALL PGMTEXT('B',3.0,0.5,0.5,'observed wavelength [\A]')
            CALL PGMTEXT('L',2.5,0.5,0.5,
     +       'normalized filter transmission and fluxes')
            CALL PGSWIN(XMIN/(1.+Z),XMAX/(1.+Z),YMIN,YMAX)
            CALL PGBOX('CTSM',0.0,0,' ',0.0,0)
            CALL PGMTEXT('T',2.5,0.5,0.5,'rest-frame wavelength [\A]')
            LL1=TRUEBEG(INFILE)
            LL2=TRUELEN(INFILE)
            CALL PGMTEXT('T',2.5,0.0,0.0,INFILE(LL1:LL2))
            CALL PGSWIN(XMIN,XMAX,YMIN,YMAX)
            NCOLOR=1
            DO I=1,NFILTERS
              NCOLOR=NCOLOR+1
              IF(NCOLOR.GT.7) NCOLOR=2
              CALL PGSCI(NCOLOR)
              CALL PGLINE(NPFILT(I),WL_FILT(1,I),FLUX_FILT(1,I))
            END DO
            CALL PGSCI(1)
          END DO
        END IF
C------------------------------------------------------------------------------
C Calculamos la longitud de onda media y el flujo promedio en cada banda 
C disponible
        DO I=1,NFILTERS
          FLUX_LDO(I)=LAMBDAEFF(NPFILT(I),WL_FILT(1,I),FLUX_FILT(1,I),
     +     SUM1,SUM2)
          FWIDTH(I)=SUM2 !Filter Width
          ERRFLUX_LDO(I)=FWIDTH(I)/2.0
          !Calculamos el factor que nos permite determinar el flujo a partir
          !de la magnitud; notar que la expresion es valida para cualquier
          !sistema de magnitudes, dado que previamente hemos chequeado que
          !el programa esta utilizando como espectro de referencia el mismo
          !que el que nos hace falta aqui.
          FDUM=LAMBDAEFFVEGA(NPFILT(I),WL_FILT(1,I),FLUX_FILT(1,I),
     +     NPVEGA,WL_VEGA,FLUX_VEGA,SUM1,SUM2)
          CTEFLUX(I)=SUM2/FWIDTH(I)
          FLUX_AVE(I)=CTEFLUX(I)*(10.0**(-0.4*MAG(I)))
          ERRFLUX_AVE(I)=FLUX_AVE(I)*ERRMAG(I)/1.086
        END DO
C Normalizamos los flujos al valor en el maximo
        FACTOR_FLUXMAX=FLUX_AVE(1)
        IF(NFILTERS.GT.1)THEN
          DO I=2,NFILTERS
            FACTOR_FLUXMAX=AMAX1(FACTOR_FLUXMAX,FLUX_AVE(I))
          END DO
        END IF
        DO I=1,NFILTERS
          FLUX_AVE(I)=FLUX_AVE(I)/FACTOR_FLUXMAX
          ERRFLUX_AVE(I)=ERRFLUX_AVE(I)/FACTOR_FLUXMAX
        END DO
C Calculamos limites adecuados para el plot
        FLUXMIN=0.0
        FLUXMAX=1.0
        DFLUX=FLUXMAX-FLUXMIN
        FLUXMIN=FLUXMIN-DFLUX/20.
        FLUXMAX=FLUXMAX+DFLUX/10.
C------------------------------------------------------------------------------
C Recorremos los diferentes modelos: para una IMF y una SF dada, recorremos
C los distintos valores del segundo parametro (e.g. metalicidades) y las 
C distintas edades.
        IF(CMODEL_SED.EQ.'1')THEN
          L1=TRUEBEG(STARBURST99_DIR)
          L2=TRUELEN(STARBURST99_DIR)
        ELSEIF(CMODEL_SED.EQ.'2')THEN
          L1=TRUEBEG(PEGASE_DIR)
          L2=TRUELEN(PEGASE_DIR)
        ELSEIF(CMODEL_SED.EQ.'3')THEN
          L1=TRUEBEG(PHOTODIR)
          L2=TRUELEN(PHOTODIR)
        END IF
        LFIRST_CHISQR=.TRUE.
C
        DO IPARAM2=1,NPARAM2
C..............................................................................
c---------
          IF(CMODEL_SED.EQ.'1')THEN
c---------
            !Nombre del fichero a leer: le an~adimos la letra "a", "b", "c",...
            !para IPARAM2=1, 2, 3,... (recordar que CHAR(97)='a')
            IF(CIMF.EQ.'1')THEN
              IF(CSFR.EQ.'1')THEN
                FILEDATA='fig1'//CHAR(IPARAM2+96)//'.dat'
              ELSE
                FILEDATA='fig2'//CHAR(IPARAM2+96)//'.dat'
              END IF
            ELSEIF(CIMF.EQ.'2')THEN
              IF(CSFR.EQ.'1')THEN
                FILEDATA='fig3'//CHAR(IPARAM2+96)//'.dat'
              ELSE
                FILEDATA='fig4'//CHAR(IPARAM2+96)//'.dat'
              END IF
            ELSEIF(CIMF.EQ.'3')THEN
              IF(CSFR.EQ.'1')THEN
                FILEDATA='fig5'//CHAR(IPARAM2+96)//'.dat'
              ELSE
                FILEDATA='fig6'//CHAR(IPARAM2+96)//'.dat'
              END IF
            END IF
c---------
          ELSEIF(CMODEL_SED.EQ.'2')THEN
c---------
            IF(CIMF.EQ.'1')THEN
              IF(CSFR.EQ.'1')THEN
                FILEDATA='spectra4.dat'
              END IF
            END IF
c---------
          ELSEIF(CMODEL_SED.EQ.'3')THEN
c---------
            FILEDATA='CWW_sed/CWW_all_ext.sed'
c---------
          END IF
c---------
C..............................................................................
C Comprobamos si el fichero existe
          WRITE(*,*)
          WRITE(*,100) '> Data file is: '
          L=TRUELEN(FILEDATA)
c---------
          IF(CMODEL_SED.EQ.'1')THEN
c---------
            WRITE(*,*) STARBURST99_DIR(L1:L2)//'/'//FILEDATA(1:L)
            INQUIRE(FILE=STARBURST99_DIR(L1:L2)//'/'//FILEDATA(1:L),
     +       EXIST=LOGFILE)
c---------
          ELSEIF(CMODEL_SED.EQ.'2')THEN
c---------
            WRITE(*,*) PEGASE_DIR(L1:L2)//'/'//FILEDATA(1:L)
            INQUIRE(FILE=PEGASE_DIR(L1:L2)//'/'//FILEDATA(1:L),
     +       EXIST=LOGFILE)
c---------
          ELSEIF(CMODEL_SED.EQ.'3')THEN
c---------
            WRITE(*,*) PHOTODIR(L1:L2)//'/'//FILEDATA(1:L)
            INQUIRE(FILE=PHOTODIR(L1:L2)//'/'//FILEDATA(1:L),
     +       EXIST=LOGFILE)
c---------
          END IF
c---------
          IF(.NOT.LOGFILE)THEN
            WRITE(*,101) 'ERROR: this file does not exist.'
            WRITE(*,100) 'Press <CR> to continue...'
            READ(*,*)
            RETURN
          END IF
C..............................................................................
C Leemos fichero con predicciones del modelo
c---------
          IF(CMODEL_SED.EQ.'1')THEN
c---------
            OPEN(15,FILE=STARBURST99_DIR(L1:L2)//'/'//FILEDATA(1:L),
     +       STATUS='OLD',FORM='FORMATTED')
            DO J=1,3 !Skip first 3 lines
              READ(15,*)
            END DO
            J=1
10          READ(15,*,END=12) WL_MODEL(J),
     +       (FLUX_MODEL(J,IAGE),IAGE=1,NAGES)
            WL_MODEL(J)=WL_MODEL(J)*(1.+Z) !desplazamiento al rojo
            DO IAGE=1,NAGES !dividimos el flujo por un factor arbitrario para
                            !tener valores pequen~os que podamos manejar con
                            !numeros en formato REAL
              FLUX_MODEL(J,IAGE)=10.0**(FLUX_MODEL(J,IAGE)-37.0)
            END DO
            J=J+1
            GOTO 10
12          CLOSE(15)
            NPTFLUX_MODEL=J-1
c---------
          ELSEIF(CMODEL_SED.EQ.'2')THEN
c---------
            OPEN(15,FILE=PEGASE_DIR(L1:L2)//'/'//FILEDATA(1:L),
     +       STATUS='OLD',FORM='FORMATTED')
            !salta bloque con la descripcion del escenario evolutivo
            CLINEA=' '
            DO WHILE(CLINEA(1:10).NE.'**********')
              READ(15,101) CLINEA
            END DO
            !leemos: timesteps, no. of wavelengths and no. of emission lines
            READ(15,*) NAGES,NPTFLUX_MODEL,N_EL
            IF(NAGES.GT.NMAX_AGES_PEGA)THEN
              WRITE(*,100) 'NAGES, NMAX_AGES_PEGA: '
              WRITE(*,*) NAGES,NMAX_AGES_PEGA
              WRITE(*,101) 'ERROR: NAGES.GT.NMAX_AGES_PEGA'
              WRITE(*,100) 'Press <CR> to continue...'
              READ(*,*)
              CLOSE(15)
              RETURN
            END IF
            !leemos las longitudes de onda
            J=0
            JJ=0 !evita warnings de compilacion
            DO K=1,NPTFLUX_MODEL/5 !leemos lineas completas
              JJ=(K-1)*5+1
              READ(15,*) (WL_MODEL(J),J=JJ,JJ+4)
            END DO
            IF(JJ+4.LT.NPTFLUX_MODEL)THEN !leemos lineas incompletas
              JJ=JJ+5
              READ(15,*) (WL_MODEL(J),J=JJ,NPTFLUX_MODEL)
            END IF
            !introducimos desplazamiento al rojo
            DO J=1,NPTFLUX_MODEL
              WL_MODEL(J)=WL_MODEL(J)*(1.+Z)
            END DO
            !saltamos las l.d.o. de las lineas de emision
            NSKIP_EL=N_EL/5
            IF(NSKIP_EL*5.LT.N_EL) NSKIP_EL=NSKIP_EL+1
            DO K=1,NSKIP_EL
              READ(15,*)
            END DO
            !leemos los diferentes espectros para cada edad
            DO IAGE=1,NAGES
              READ(15,*) NAGE_PEGA(IAGE)
              NAGE(IAGE)=NAGE_PEGA(IAGE)
              READ(15,*) !saltamos segunda linea
              J=0
              DO K=1,NPTFLUX_MODEL/5 !leemos lineas completas
                JJ=(K-1)*5+1
                READ(15,*) (DP_FLUX_MODEL(J),J=JJ,JJ+4)
              END DO
              IF(JJ+4.LT.NPTFLUX_MODEL)THEN !leemos lineas incompletas
                JJ=JJ+5
                READ(15,*) (DP_FLUX_MODEL(J),J=JJ,NPTFLUX_MODEL)
              END IF
              !dividimos el flujo por un valor arbitrario para tener numeros
              !manejares en variables de tipo REAL
              DO J=1,NPTFLUX_MODEL
                DP_FLUX_MODEL(J)=DP_FLUX_MODEL(J)/1.D30
                FLUX_MODEL(J,IAGE)=REAL(DP_FLUX_MODEL(J))
              END DO
              !saltamos las luminosidades de las lineas de emision
              DO K=1,NSKIP_EL
                READ(15,*)
              END DO
            END DO
            CLOSE(15)
c---------
          ELSEIF(CMODEL_SED.EQ.'3')THEN
c---------
            OPEN(15,FILE=PHOTODIR(L1:L2)//'/'//FILEDATA(1:L),
     +       STATUS='OLD',FORM='FORMATTED')
            NPTFLUX_MODEL=896
            DO J=1,NPTFLUX_MODEL
              READ(15,*) WL_MODEL(J),
     +         (FLUX_MODEL(J,IAGE),IAGE=1,NAGES)
              WL_MODEL(J)=WL_MODEL(J)*(1.+Z)
            END DO
            CLOSE(15)
c---------
          END IF
c---------
C..............................................................................
C Proteccion: comprobamos que el intervalo en l.d.o. de los espectros del
C modelo abarca a todos los filtros
          IF(WL_MODEL(1).GT.XMIN)THEN
            WRITE(*,101) 'ERROR: WL_MODEL(1).GT.XMIN'
            RETURN
          END IF
          IF(WL_MODEL(NPTFLUX_MODEL).LT.XMAX)THEN
            WRITE(*,101) 'ERROR: WL_MODEL(NPTFLUX_MODEL).LT.XMAX'
            RETURN
          END IF
C..............................................................................
C informacion sobre lo que se va a representar graficamente
          IF(LPLOT)THEN
            WRITE(*,101) 
     +       '*************************************************'
            WRITE(*,101) 
     +       'COLOR POINTS...: mean measured fluxes with errors'
            WRITE(*,101) 
     +       'DARK GRAY SPECTRUM...............: model spectrum'
            WRITE(*,101)
     +       'LIGHT GRAY SPECTRUM..: model spectrum * Gal. ext.'
            WRITE(*,101)
     +       'WHITE SPECTRUM: model spectrum * (Gal.&Int.) ext.'
            WRITE(*,101) 
     +       'ORANGE OPEN SQUARES.....: best fit to mean fluxes'
            WRITE(*,101) 
     +       'ORANGE SPECTRUM...............: best fit spectrum'
            WRITE(*,101) 
     +       '*************************************************'
          END IF
C..............................................................................
C Recorremos todas las edades
          DO IAGE=1,NAGES
            IF((NAGE(IAGE).GE.MINAGE).AND.(NAGE(IAGE).LE.MAXAGE))THEN
              IF(LPLOT)THEN
                !redibujamos caja del plot
                DO ITERM=NTERM,1,-1
                  CALL PGSLCT(IDN(ITERM))
                  IF(ITERM.EQ.1)THEN
                    CALL RPGERASW(0.00,1.00,0.00,0.80,0)
                    CALL RPGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
                  ELSE
                    CALL PGENV(XMIN,XMAX,YMIN,YMAX,0,-2)
                  END IF
                  CALL PGBOX('BTSN',0.0,0,'BCTSN',0.0,0)
                  CALL PGMTEXT('B',3.0,0.5,0.5,
     +             'observed wavelength [\A]')
                  CALL PGMTEXT('L',2.5,0.5,0.5,
     +             'normalized filter transmission and fluxes')
                  CALL PGSWIN(XMIN/(1.+Z),XMAX/(1.+Z),YMIN,YMAX)
                  CALL PGBOX('CTSM',0.0,0,' ',0.0,0)
                  CALL PGMTEXT('T',2.5,0.5,0.5,
     +             'rest-frame wavelength [\A]')
                  LL1=TRUEBEG(INFILE)
                  LL2=TRUELEN(INFILE)
                  CALL PGMTEXT('T',2.5,0.0,0.0,INFILE(LL1:LL2))
                  CALL PGSWIN(XMIN,XMAX,YMIN,YMAX)
                END DO
                !dibuja filtros
                DO ITERM=NTERM,1,-1
                  CALL PGSLCT(IDN(ITERM))
                  CALL PGSWIN(XMIN,XMAX,YMIN,YMAX)
                  NCOLOR=1
                  DO I=1,NFILTERS
                    NCOLOR=NCOLOR+1
                    IF(NCOLOR.GT.7) NCOLOR=2
                    CALL PGSCI(NCOLOR)
                    CALL PGLINE(NPFILT(I),WL_FILT(1,I),FLUX_FILT(1,I))
                  END DO
                  CALL PGSCI(1)
                END DO
                !Si no estamos dibujando el primer espectro, podemos 
                !representar el mejor ajuste conseguido hasta el momento
                IF(.NOT.LFIRST_CHISQR)THEN
                  DO ITERM=NTERM,1,-1
                    CALL PGSLCT(IDN(ITERM))
                    CALL PGSWIN(XMIN,XMAX,FLUXMIN_MODEL,FLUXMAX_MODEL)
                    CALL PGSCI(8)
                    DO I=1,NFILTERS !puntos calculados con el modelo
                      CALL PGPOINT(1,FLUX_LDO(I),FLUXMEAN_MODEL_BEST(I),
     +                 19)
                    END DO
                    CALL PGSWIN(XMIN,XMAX,YMINMODEL_BEST,YMAXMODEL_BEST)
                    CALL PGLINE(NPTFLUX_MODEL,WL_MODEL,FLUX_MODEL_BEST)
                    CALL PGSCI(1)
                  END DO
                END IF
                !dibujamos espectro del proximo modelo al redshift elegido
                YMINMODEL=0.0
                YMAXMODEL=-1.0
                DO J=1,NPTFLUX_MODEL
                  IF((WL_MODEL(J).GE.XMIN).AND.
     +             (WL_MODEL(J).LE.XMAX))THEN
                    YMAXMODEL=AMAX1(YMAXMODEL,FLUX_MODEL(J,IAGE))
                  END IF
                END DO
                DYMODEL=YMAXMODEL-YMINMODEL
                YMINMODEL=YMINMODEL-DYMODEL/20.
                YMAXMODEL=YMAXMODEL+DYMODEL/10.
                DO ITERM=NTERM,1,-1
                  CALL PGSLCT(IDN(ITERM))
                  CALL PGSWIN(XMIN,XMAX,YMINMODEL,YMAXMODEL)
                  IF((EBV_GALEXT.GT.0.0).OR.(EBV_INTRED.GT.0.0))
     +             CALL PGSCI(14)
                  CALL PGLINE(NPTFLUX_MODEL,WL_MODEL,FLUX_MODEL(1,IAGE))
                  CALL PGSCI(1)
                END DO
              END IF
              !corregimos el modelo de extincion galactica
              IF(EBV_GALEXT.GT.0.0)THEN
                DO J=1,NPTFLUX_MODEL
                  WL0=WL_MODEL(J) !observed wavelength (note that the models 
                                 !have been already redshifted)
                  !La correccion de extincion se realiza EXCLUSIVAMENTE entre
                  !WL1 y WL2
                  IF((WL0.GE.WL1).AND.(WL0.LE.WL2))THEN
                    CALL SCOMPEXT(CURVE_GALEXT,WL0,RV,K_LAMBDA,LOK)
                    IF(.NOT.LOK)THEN
                      WRITE(*,101) '* Galactic extinction correction'
                      WRITE(*,100) 'Wavelength: '
                      WRITE(*,*) WL0
                      WRITE(*,101) 'ERROR: LOK=.FALSE. in SCOMPEXT'
                      RETURN
                    END IF
                    FLUX_MODEL(J,IAGE)=FLUX_MODEL(J,IAGE)*
     +               (10.0**(0.4*EBV_GALEXT*K_LAMBDA))
                  END IF
                END DO
                IF(LPLOT)THEN
                  !dibujamos espectro del modelo corregido de ext. galactica
                  YMINMODEL=0.0
                  YMAXMODEL=-1.0
                  DO J=1,NPTFLUX_MODEL
                    IF((WL_MODEL(J).GE.XMIN).AND.
     +               (WL_MODEL(J).LE.XMAX))THEN
                      YMAXMODEL=AMAX1(YMAXMODEL,FLUX_MODEL(J,IAGE))
                    END IF
                  END DO
                  DYMODEL=YMAXMODEL-YMINMODEL
                  YMINMODEL=YMINMODEL-DYMODEL/20.
                  YMAXMODEL=YMAXMODEL+DYMODEL/10.
                  DO ITERM=NTERM,1,-1
                    CALL PGSLCT(IDN(ITERM))
                    CALL PGSWIN(XMIN,XMAX,YMINMODEL,YMAXMODEL)
                    CALL PGSCI(15)
                    CALL PGLINE(NPTFLUX_MODEL,WL_MODEL,
     +               FLUX_MODEL(1,IAGE))
                    CALL PGSCI(1)
                  END DO
                END IF
              END IF
              !corregimos el modelo de extincion interna
              IF(EBV_INTRED.GT.0.0)THEN
                DO J=1,NPTFLUX_MODEL
                  WL0=WL_MODEL(J)/(1.+Z) !rest-frame wavelength (note that the 
                                        !models have been already redshifted)
                  !La correccion de extincion se realiza EXCLUSIVAMENTE entre
                  !WL1 y WL2
                  IF((WL0.GE.WL1).AND.(WL0.LE.WL2))THEN
                    CALL SCOMPEXT(CURVE_INTRED,WL0,RV,K_LAMBDA,LOK)
                    IF(.NOT.LOK)THEN
                      WRITE(*,101) '* Internal reddening correction'
                      WRITE(*,100) 'Wavelength: '
                      WRITE(*,*) WL0
                      WRITE(*,101) 'ERROR: LOK=.FALSE. in SCOMPEXT'
                      RETURN
                    END IF
                    FLUX_MODEL(J,IAGE)=FLUX_MODEL(J,IAGE)*
     +               (10.0**(0.4*EBV_INTRED*K_LAMBDA))
                  END IF
                END DO
                IF(LPLOT)THEN
                  !dibujamos espectro del modelo corregido de extincion interna
                  YMINMODEL=0.0
                  YMAXMODEL=-1.0
                  DO J=1,NPTFLUX_MODEL
                    IF((WL_MODEL(J).GE.XMIN).AND.
     +               (WL_MODEL(J).LE.XMAX))THEN
                      YMAXMODEL=AMAX1(YMAXMODEL,FLUX_MODEL(J,IAGE))
                    END IF
                  END DO
                  DYMODEL=YMAXMODEL-YMINMODEL
                  YMINMODEL=YMINMODEL-DYMODEL/20.
                  YMAXMODEL=YMAXMODEL+DYMODEL/10.
                  DO ITERM=NTERM,1,-1
                    CALL PGSLCT(IDN(ITERM))
                    CALL PGSWIN(XMIN,XMAX,YMINMODEL,YMAXMODEL)
                    CALL PGLINE(NPTFLUX_MODEL,WL_MODEL,
     +               FLUX_MODEL(1,IAGE))
                  END DO
                END IF
              END IF
              IF(LPLOT)THEN
                !dibujamos flujos que queremos ajustar con barras de error
                DO ITERM=NTERM,1,-1
                  CALL PGSLCT(IDN(ITERM))
                  CALL PGSWIN(XMIN,XMAX,FLUXMIN,FLUXMAX)
                  NCOLOR=1
                  DO I=1,NFILTERS
                    NCOLOR=NCOLOR+1
                    IF(NCOLOR.GT.7) NCOLOR=2
                    CALL PGSCI(NCOLOR)
                    CALL PGPOINT(1,FLUX_LDO(I),FLUX_AVE(I),17)
                    CALL PGERRX(1,FLUX_LDO(I)-ERRFLUX_LDO(I),
     +               FLUX_LDO(I)+ERRFLUX_LDO(I),FLUX_AVE(I),1.0)
                    CALL PGERRY(1,FLUX_LDO(I),
     +               FLUX_AVE(I)-ERRFLUX_AVE(I),
     +               FLUX_AVE(I)+ERRFLUX_AVE(I),1.0)
                  END DO
                  CALL PGSCI(1)
                END DO
              END IF
              !calculamos el flujo promedio en cada filtro usando el espectro
              !del modelo
              DO I=1,NFILTERS
                MAG_MODEL(I)=
     +           MAGNITUDE(NPFILT(I),WL_FILT(1,I),FLUX_FILT(1,I),
     +           NPVEGA,WL_VEGA,FLUX_VEGA,
     +           NPTFLUX_MODEL,WL_MODEL,FLUX_MODEL(1,IAGE),SUM1,SUM2)
ccc             FLUXMEAN_MODEL(I)=CTEFLUX(I)*(10.0**(-0.4*MAG_MODEL(I)))
                !Aqui no necesitamos introducir CTEFLUX(I) porque vamos a
                !ajustar los flujos promedios en cada filtro a los flujos
                !medidos.
                FLUXMEAN_MODEL(I)=10.0**(-0.4*MAG_MODEL(I))
              END DO
              !normalizamos estos flujos al valor en el maximo
              FACTOR_FLUXMAX_MODEL=FLUXMEAN_MODEL(1)
              IF(NFILTERS.GT.1)THEN
                DO I=2,NFILTERS
                  FACTOR_FLUXMAX_MODEL=
     +             AMAX1(FACTOR_FLUXMAX_MODEL,FLUXMEAN_MODEL(I))
                END DO
              END IF
              DO I=1,NFILTERS
                FLUXMEAN_MODEL(I)=FLUXMEAN_MODEL(I)/FACTOR_FLUXMAX_MODEL
              END DO
              IF(LPLOT)THEN
                !limtes para el plot
                FLUXMIN_MODEL=0.0
                FLUXMAX_MODEL=1.0
                DFLUX_MODEL=FLUXMAX_MODEL-FLUXMIN_MODEL
                FLUXMIN_MODEL=FLUXMIN_MODEL-DFLUX_MODEL/20.
                FLUXMAX_MODEL=FLUXMAX_MODEL+DFLUX_MODEL/10.
                DO ITERM=NTERM,1,-1
                  CALL PGSLCT(IDN(ITERM))
                  CALL PGSWIN(XMIN,XMAX,FLUXMIN_MODEL,FLUXMAX_MODEL)
                  DO I=1,NFILTERS !puntos calculados con el modelo
                    CALL PGPOINT(1,FLUX_LDO(I),FLUXMEAN_MODEL(I),19)
                  END DO
                END DO
              END IF
C CHISQR:
              !calculamos el factor de normalizacion que minimiza el CHISQR
              !(por fortuna puede hacerse analiticamente)
              SUM1=0.0
              SUM2=0.0
              DO I=1,NFILTERS
                SUM1=SUM1+FLUX_AVE(I)*FLUXMEAN_MODEL(I)/
     +           (ERRFLUX_AVE(I)*ERRFLUX_AVE(I))
                SUM2=SUM2+FLUXMEAN_MODEL(I)*FLUXMEAN_MODEL(I)/
     +           (ERRFLUX_AVE(I)*ERRFLUX_AVE(I))
              END DO
              BCHISQR=SUM1/SUM2
              !calculamos el CHISQR con el nuevo factor de normalizacion
              CHISQR(IAGE,IPARAM2)=0.0
              DO I=1,NFILTERS
                CHISQR(IAGE,IPARAM2)=CHISQR(IAGE,IPARAM2)+
     +           (FLUX_AVE(I)-BCHISQR*FLUXMEAN_MODEL(I))*
     +           (FLUX_AVE(I)-BCHISQR*FLUXMEAN_MODEL(I))/
     +           (ERRFLUX_AVE(I)*ERRFLUX_AVE(I))
              END DO
              WRITE(*,100) 'Age (Myr), 2nd. param., factor, CHISQR: '
              WRITE(*,'(I5,2X,F6.4,2X,F6.3,$)') 
     +         NAGE(IAGE),PARAM2(IPARAM2),BCHISQR
              WRITE(*,*) CHISQR(IAGE,IPARAM2)
              !actualizamos el menor valor de CHISQR obtenido hasta el 
              !momento; asimismo pintamos de verde el mejor ajuste conseguido
              !y de gris el resto
              IF(LFIRST_CHISQR)THEN
                LFIRST_CHISQR=.FALSE.
                CHISQRMIN=CHISQR(IAGE,IPARAM2)
                IAGE_MINCHISQR=IAGE
                IPARAM2_MINCHISQR=IPARAM2
                DO I=1,NFILTERS
                  FLUXMEAN_MODEL_BEST(I)=FLUXMEAN_MODEL(I)
                END DO
                DO J=1,NPTFLUX_MODEL
                  FLUX_MODEL_BEST(J)=FLUX_MODEL(J,IAGE)
                END DO
                YMINMODEL_BEST=YMINMODEL
                YMAXMODEL_BEST=YMAXMODEL
                BCHISQR_BEST=BCHISQR
              ELSE
                IF(CHISQR(IAGE,IPARAM2).LT.CHISQRMIN)THEN
                  CHISQRMIN=CHISQR(IAGE,IPARAM2)
                  IAGE_MINCHISQR=IAGE
                  IPARAM2_MINCHISQR=IPARAM2
                  DO I=1,NFILTERS
                    FLUXMEAN_MODEL_BEST(I)=FLUXMEAN_MODEL(I)
                  END DO
                  DO J=1,NPTFLUX_MODEL
                    FLUX_MODEL_BEST(J)=FLUX_MODEL(J,IAGE)
                  END DO
                  YMINMODEL_BEST=YMINMODEL
                  YMAXMODEL_BEST=YMAXMODEL
                  BCHISQR_BEST=BCHISQR
                END IF
              END IF
              IF(CPLOT.EQ.'y')THEN
                WRITE(*,100) 'Press <CR> to continue...'
                READ(*,*)
              END IF
            END IF
          END DO
C..............................................................................
        END DO
C------------------------------------------------------------------------------
C Inicializamos imagen LOGCHISQR al maximo valor de CHISQR
        FDUM=0.0
        DO IPARAM2=1,NPARAM2
          DO IAGE=1,NAGES
            IF((NAGE(IAGE).GE.MINAGE).AND.(NAGE(IAGE).LE.MAXAGE))THEN
              FDUM=AMAX1(FDUM,CHISQR(IAGE,IPARAM2))
            END IF
          END DO
        END DO
        IF(FDUM.GT.0.0) FDUM=ALOG10(FDUM)
        DO IPARAM2=1,NMAX_PARAM2
          DO IAGE=1,NMAX_AGES
            LOGCHISQR(IAGE,IPARAM2)=FDUM
          END DO
        END DO
C..............................................................................
C Dibujamos CHISQR
        WRITE(*,*)
        WRITE(*,101) '* CHISQR image:'
        BG_CHISQR=1.E33
        FG_CHISQR=-1.E33
        DO IPARAM2=1,NPARAM2
          DO IAGE=1,NAGES
            IF((NAGE(IAGE).GE.MINAGE).AND.(NAGE(IAGE).LE.MAXAGE))THEN
              LOGCHISQR(IAGE,IPARAM2)=ALOG10(CHISQR(IAGE,IPARAM2))
              IF(LOGCHISQR(IAGE,IPARAM2).LT.BG_CHISQR) 
     +         BG_CHISQR=LOGCHISQR(IAGE,IPARAM2)
              IF(LOGCHISQR(IAGE,IPARAM2).GT.FG_CHISQR) 
     +         FG_CHISQR=LOGCHISQR(IAGE,IPARAM2)
            END IF
          END DO
        END DO
        WRITE(CDUMMY,*) BG_CHISQR
        BG_CHISQR=READF('BG for LOGCHISQR image',CDUMMY)
        WRITE(CDUMMY,*) FG_CHISQR
        FG_CHISQR=READF('FG for LOGCHISQR image',CDUMMY)
        DO IAGE=1,NAGES
          IF(NAGE(IAGE).LE.MAXAGE) XMAXCHI=REAL(IAGE)+0.6
        END DO
        DO IAGE=NAGES,1,-1
          IF(NAGE(IAGE).GE.MINAGE) XMINCHI=REAL(IAGE)-0.6
        END DO
        YMINCHI=0.4
        YMAXCHI=0.6+REAL(NPARAM2)
        DO ITERM=NTERM,1,-1
          CALL PGSLCT(IDN(ITERM))
          IF(ITERM.EQ.1)THEN
            CALL RPGERASW(0.00,1.00,0.00,0.80,0)
            CALL RPGENV(XMINCHI,XMAXCHI,YMINCHI,YMAXCHI,0,-2)
          ELSE
            CALL PGENV(XMINCHI,XMAXCHI,YMINCHI,YMAXCHI,0,-2)
          END IF
          CALL PGBOX('BCNTSI',0.0,0,'BCNTSI',0.0,0)
          CALL PGMTEXT('B',3.0,0.5,0.5,'age axis')
          IF(CMODEL_SED.EQ.'1')THEN
            CALL PGMTEXT('T',1.5,0.5,0.5,'Starburst99')
          ELSEIF(CMODEL_SED.EQ.'2')THEN
            CALL PGMTEXT('T',1.5,0.5,0.5,'PEGASE')
          ELSEIF(CMODEL_SED.EQ.'3')THEN
            CALL PGMTEXT('T',1.5,0.5,0.5,'Coleman, Wu & Weedman (1980)')
          END IF
          LL1=TRUEBEG(INFILE)
          LL2=TRUELEN(INFILE)
          CALL PGMTEXT('T',2.5,0.0,0.0,INFILE(LL1:LL2))
          CALL PGMTEXT('L',2.5,0.5,0.5,'2nd. parameter axis')
          CALL PGGRAY(LOGCHISQR,NMAX_AGES,NMAX_PARAM2,
     +     1,NAGES,1,NPARAM2,FG_CHISQR,BG_CHISQR,TR)
        END DO
C..............................................................................
C Si se solicita salvamos imagen CHISQR en formato FITS
        IF(READC('Save CHISQR image in FITS file (y/n)','n','yn')
     +   .EQ.'y')THEN
          NAXIS(1)=NMAX_AGES
          NAXIS(2)=NMAX_PARAM2
          DO IPARAM2=1,NMAX_PARAM2
            DO IAGE=1,NMAX_AGES
              IMAGEN_(IAGE,IPARAM2)=LOGCHISQR(IAGE,IPARAM2)
            END DO
          END DO
          CALL SESCRFITS
        END IF
C------------------------------------------------------------------------------
C Dibujamos el espectro que mejor ajusta, utilizando la escala correcta en
C flujo.
        YMINFINAL=YMIN*FACTOR_FLUXMAX
        YMAXFINAL=YMAX*FACTOR_FLUXMAX*1.3 !lo hacemos un poco mayor
        DO ITERM=NTERM,1,-1
          CALL PGSLCT(IDN(ITERM))
          !Dibujamos la caja del plot
          IF(ITERM.EQ.1)THEN
            CALL RPGERASW(0.00,1.00,0.00,0.80,0)
            CALL RPGENV(XMIN,XMAX,YMINFINAL,YMAXFINAL,0,-2)
          ELSE
            CALL PGENV(XMIN,XMAX,YMINFINAL,YMAXFINAL,0,-2)
          END IF
          CALL PGBOX('BTSN',0.0,0,'BCTSN',0.0,0)
          CALL PGMTEXT('B',3.0,0.5,0.5,'observed wavelength [\A]')
          CALL PGMTEXT('L',2.5,0.5,0.5,
     +     'flux [erg s\u-1\d cm\u-2\d \A\u-1\d]')
          CALL PGSWIN(XMIN/(1.+Z),XMAX/(1.+Z),YMINFINAL,YMAXFINAL)
          CALL PGBOX('CTSM',0.0,0,' ',0.0,0)
          CALL PGMTEXT('T',2.5,0.5,0.5,'rest-frame wavelength [\A]')
          LL1=TRUEBEG(INFILE)
          LL2=TRUELEN(INFILE)
          CALL PGMTEXT('T',2.5,0.0,0.0,INFILE(LL1:LL2))
          !Dibujamos los filtros
          CALL PGSWIN(XMIN,XMAX,YMIN,YMAX)
          NCOLOR=1
          DO I=1,NFILTERS
            NCOLOR=NCOLOR+1
            IF(NCOLOR.GT.7) NCOLOR=2
            CALL PGSCI(NCOLOR)
            CALL PGLINE(NPFILT(I),WL_FILT(1,I),FLUX_FILT(1,I))
          END DO
          CALL PGSCI(1)
          CALL PGSWIN(XMIN,XMAX,YMINFINAL,YMAXFINAL)
          CALL PGSCI(7)
        END DO
        !Escalamos los flujos medidos
        DO I=1,NFILTERS
          FLUX_AVE(I)=FLUX_AVE(I)*FACTOR_FLUXMAX
          ERRFLUX_AVE(I)=ERRFLUX_AVE(I)*FACTOR_FLUXMAX
        END DO
        !Dibujamos los flujos medidos
        DO ITERM=NTERM,1,-1
          CALL PGSLCT(IDN(ITERM))
          NCOLOR=1
          DO I=1,NFILTERS
            NCOLOR=NCOLOR+1
            IF(NCOLOR.GT.7) NCOLOR=2
            CALL PGSCI(NCOLOR)
            CALL PGPOINT(1,FLUX_LDO(I),FLUX_AVE(I),17)
            CALL PGERRX(1,FLUX_LDO(I)-ERRFLUX_LDO(I),
     +       FLUX_LDO(I)+ERRFLUX_LDO(I),FLUX_AVE(I),1.0)
            CALL PGERRY(1,FLUX_LDO(I),FLUX_AVE(I)-ERRFLUX_AVE(I),
     +       FLUX_AVE(I)+ERRFLUX_AVE(I),1.0)
          END DO
          CALL PGSCI(1)
        END DO
        !Dibujamos los flujos medios en cada filtro que mejor ajustan: 
        !notar que tenemos que aplicar el factor FACTOR_FLUXMAX para 
        !pasar de una escala normalizada de [0,1] a la escala de 
        !erg s^-1 cm^2 A^1. Asimismo, el factor BCHISQR_BEST es
        !el coeficiente (normalmente muy cercano a uno) que se introduce 
        !al minimizar el CHISQR.
        DO I=1,NFILTERS !puntos calculados con el modelo
          FLUXMEAN_MODEL_BEST(I)=FLUXMEAN_MODEL_BEST(I)*
     +     FACTOR_FLUXMAX*BCHISQR_BEST
        END DO
        DO ITERM=NTERM,1,-1
          CALL PGSLCT(IDN(ITERM))
          NCOLOR=1
          DO I=1,NFILTERS !puntos calculados con el modelo
            NCOLOR=NCOLOR+1
            IF(NCOLOR.GT.7) NCOLOR=2
            CALL PGSCI(NCOLOR)
            CALL PGPOINT(1,FLUX_LDO(I),FLUXMEAN_MODEL_BEST(I),19)
          END DO
          CALL PGSCI(1)
        END DO
        !Escalamos y dibujamos el mejor espectro (el factor basta calcularlo
        !con uno cualquiera de los filtros => usamos el primero)
        I=1
        FACTORFINAL=CTEFLUX(I)*(10.0**(-0.4*MAGNITUDE(NPFILT(I),
     +   WL_FILT(1,I),FLUX_FILT(1,I),NPVEGA,WL_VEGA,FLUX_VEGA,
     +   NPTFLUX_MODEL,WL_MODEL,FLUX_MODEL_BEST,SUM1,SUM2)))/
     +   FLUXMEAN_MODEL_BEST(I)
        DO J=1,NPTFLUX_MODEL
          FLUX_MODEL_BEST(J)=FLUX_MODEL_BEST(J)/FACTORFINAL
        END DO
        DO ITERM=NTERM,1,-1
          CALL PGSLCT(IDN(ITERM))
          CALL PGSCI(8)
          CALL PGLINE(NPTFLUX_MODEL,WL_MODEL,FLUX_MODEL_BEST)
          CALL PGSCI(1)
        END DO
        !mostramos las magnitudes de entrada y las ajustadas
        WRITE(*,*)
        WRITE(*,101) '                     mag    err_mag   mag_fit'
        WRITE(*,101) '                   ======   =======   ======='
        DO I=1,NFILTERS
          WRITE(*,'(A,I4,$)') 'Filter ID: ',NFILTER(I)
          WRITE(*,'(3(2X,F8.3))') MAG(I),ERRMAG(I),MAGNITUDE(NPFILT(I),
     +     WL_FILT(1,I),FLUX_FILT(1,I),NPVEGA,WL_VEGA,FLUX_VEGA,
     +     NPTFLUX_MODEL,WL_MODEL,FLUX_MODEL_BEST,SUM1,SUM2)
        END DO
        !permitimos salvar el espectro y/o calcular el flujo en una l.d.o.
        WRITE(*,*)
        WL0=1000.
        DO WHILE(WL0.NE.0)
          WL0=READF('Rest-frame wavelength (0=EXIT,<0=save spectrum'//
     +     ' as ASCII)','0')
          WL0=WL0*(1.+Z)
          IF(WL0.EQ.0)THEN
          ELSEIF(WL0.LT.0.0)THEN
            LOGFILE=.TRUE.
            DO WHILE(LOGFILE)
              OUTFILE(1:80)=READC('Output ASCII file name','@','@')
              INQUIRE(FILE=OUTFILE,EXIST=LOGFILE)
              IF(LOGFILE)THEN
                WRITE(*,100) 'ERROR: this file already exist.'
                WRITE(*,101) ' Try again.'
                WRITE(*,100) 'Press <CR> to continue...'
                READ(*,*)
              END IF
            END DO
            WRITE(*,100) 'Saving file...'
            OPEN(17,FILE=OUTFILE,STATUS='NEW',FORM='FORMATTED')
            DO J=1,NPTFLUX_MODEL
              WRITE(17,*) WL_MODEL(J),FLUX_MODEL_BEST(J)
            END DO
            CLOSE(17)
            WRITE(*,101) 'OK!'
          ELSE
            IF((WL0.GE.XMIN).AND.(WL0.LE.XMAX))THEN
              FDUM=LININTERP(NPTFLUX_MODEL,WL_MODEL,
     +         FLUX_MODEL_BEST,WL0,IFLAG,IDUM1,IDUM2)
              WRITE(*,100) 'Flux: '
              WRITE(*,*) FDUM
              DO ITERM=NTERM,1,-1
                CALL PGSLCT(IDN(ITERM))
                CALL PGSCI(6)
                CALL PGPOINT(1,WL0,FDUM,8)
                CALL PGSCI(1)
              END DO
            ELSE
              WRITE(*,101) 'ERROR: wavelength outside display region.'
              WRITE(*,100) 'Press <CR> to continue...'
              READ(*,*)
            END IF
          END IF
        END DO
C------------------------------------------------------------------------------
C Retornamos al programa principal
        RETURN
C------------------------------------------------------------------------------
100     FORMAT(A,$)
101     FORMAT(A)
C------------------------------------------------------------------------------
900     WRITE(*,101) 'ERROR: input error while working with file '
        WRITE(*,101) INFILE(1:TRUELEN(INFILE))
        CLOSE(10)
        WRITE(*,100) 'Press <CR> to continue...'
        READ(*,*)
        END
