C******************************************************************************
C******************************************************************************
C Conjunto de rutinas para calcular parametros interesantes de un filtro
C******************************************************************************
C******************************************************************************
C Calcula el ancho del filtro. Esto es importante cuando se calcula, por
C ejemplo, el flujo a traves de un filtro en el sistema del telescopio
C espacial.
C La integral la hacemos con trapecios del ancho de los intervalos del filtro.
C No hace falta mas resolucion porque estamos aproximando la respuesta del
C filtro como la interpolacion lineal (he comprobado que una interpolacion
C con un polinomio de segundo grado tiene discontinuidades cuando se cambia 
C de terna de puntos).
C
        REAL FUNCTION FILTERWIDTH(NPFILT,WL_FILT,FLUX_FILT)
        IMPLICIT NONE
C
        INTEGER NPFILT
        REAL WL_FILT(NPFILT),FLUX_FILT(NPFILT)
C
        INTEGER I
        REAL Y0_FILT,DX
        DOUBLE PRECISION DSUM
C------------------------------------------------------------------------------
        DSUM=0.D0
        DO I=1,NPFILT-1 !con N puntos hay N-1 intervalos
          DX=WL_FILT(I+1)-WL_FILT(I)
          Y0_FILT=(FLUX_FILT(I)+FLUX_FILT(I+1))/2.
          DSUM=DSUM+DBLE(Y0_FILT*DX)
        END DO
        FILTERWIDTH=REAL(DSUM)
C
        END
C
C******************************************************************************
C Calcula la longitud de onda efectiva de un filtro (Eq. A1 de Fukugita et al.)
C La integral la hacemos con trapecios del ancho de los intervalos del filtro.
C No hace falta mas resolucion porque estamos aproximando la respuesta del
C filtro como la interpolacion lineal (he comprobado que una interpolacion
C con un polinomio de segundo grado tiene discontinuidades cuando se cambia 
C de terna de puntos).
C
        REAL FUNCTION LAMBDAEFF(NPFILT,WL_FILT,FLUX_FILT,SUM1,SUM2)
        IMPLICIT NONE
C
        INTEGER NPFILT
        REAL WL_FILT(NPFILT),FLUX_FILT(NPFILT)
        REAL SUM1,SUM2
C
        INTEGER I
        REAL X0,Y0_FILT,DX
        DOUBLE PRECISION DSUM1,DSUM2
C------------------------------------------------------------------------------
        DSUM1=0.D0
        DSUM2=0.D0
        DO I=1,NPFILT-1 !con N puntos hay N-1 intervalos
          DX=WL_FILT(I+1)-WL_FILT(I)
          X0=(WL_FILT(I)+WL_FILT(I+1))/2.
          Y0_FILT=(FLUX_FILT(I)+FLUX_FILT(I+1))/2.
          DSUM1=DSUM1+DBLE(X0*Y0_FILT*DX)
          DSUM2=DSUM2+DBLE(Y0_FILT*DX)
        END DO
C
        SUM1=REAL(DSUM1)
        SUM2=REAL(DSUM2)
        LAMBDAEFF=SUM1/SUM2
C
        END
C
C******************************************************************************
C Calcula el flujo efectivo de un filtro en el caso de Vega (Eq. A2 de 
C Fukugita et al.). Como la discretizacion del filtro sera en general diferente
C a la de la estrella, la anchura de cada trapecio se determina de forma que
C es siempre el menor intervalo posible (es decir, usando como siguiente punto
C el mas cercano, ya sea el del filtro o el de la estrella). Al igual que
C antes en LAMBDAEFF, no necesitamos una resolucion mayor que esa.
C
        REAL FUNCTION FLUXEFFVEGA(NPFILT,WL_FILT,FLUX_FILT,
     +   NPVEGA,WL_VEGA,FLUX_VEGA,SUM1,SUM2)
        IMPLICIT NONE
C
        INTEGER NPFILT
        REAL WL_FILT(NPFILT),FLUX_FILT(NPFILT)
        INTEGER NPVEGA
        REAL WL_VEGA(NPVEGA),FLUX_VEGA(NPVEGA)
        REAL SUM1,SUM2
C
        REAL LININTERP
C
        INTEGER IFILT,IVEGA
        INTEGER IFLAG,N1,N2
        REAL X1,X2
        REAL X0,Y0_FILT,Y0_VEGA,DX
        DOUBLE PRECISION DSUM1,DSUM2
C------------------------------------------------------------------------------
        DSUM1=0.D0
        DSUM2=0.D0
        IFILT=0 !ultimo indice usado en el filtro
        X2=WL_FILT(1) !extremo izquierdo del filtro
        DO WHILE(X2.LT.WL_FILT(NPFILT)) !mientras estamos dentro del filtro
          X1=X2
          IF(IFILT.EQ.0)THEN !estamos empezando
            IFILT=1 !primer punto del filtro
            IVEGA=NPVEGA/2 !como arranque en la busqueda tomamos el centro
            CALL BINSEARCH(WL_VEGA,NPVEGA,X1,IVEGA) !buscamos X1 en WL_VEGA
          END IF
          IF(WL_FILT(IFILT+1).LT.WL_VEGA(IVEGA+1))THEN
            IFILT=IFILT+1
            X2=WL_FILT(IFILT)
          ELSE
            IVEGA=IVEGA+1
            X2=WL_VEGA(IVEGA)
          END IF
          DX=X2-X1
          X0=(X1+X2)/2.
          Y0_FILT=LININTERP(NPFILT,WL_FILT,FLUX_FILT,X0,IFLAG,N1,N2)
          Y0_VEGA=LININTERP(NPVEGA,WL_VEGA,FLUX_VEGA,X0,IFLAG,N1,N2)
          DSUM1=DSUM1+DBLE(Y0_VEGA*Y0_FILT*DX)
          DSUM2=DSUM2+DBLE(Y0_FILT*DX)
        END DO
C
        SUM1=REAL(DSUM1)
        SUM2=REAL(DSUM2)
        FLUXEFFVEGA=SUM1/SUM2
C
        END
C
C******************************************************************************
C Calcula la longitud de onda efectiva de un filtro en el caso de Vega 
C (Eq. A3 de Fukugita et al.). El metodo de integracion es el mismo que en
C el caso de FLUXEFFVEGA.
C
        REAL FUNCTION LAMBDAEFFVEGA(NPFILT,WL_FILT,FLUX_FILT,
     +   NPVEGA,WL_VEGA,FLUX_VEGA,SUM1,SUM2)
        IMPLICIT NONE
C
        INTEGER NPFILT
        REAL WL_FILT(NPFILT),FLUX_FILT(NPFILT)
        INTEGER NPVEGA
        REAL WL_VEGA(NPVEGA),FLUX_VEGA(NPVEGA)
        REAL SUM1,SUM2
C
        REAL LININTERP
C
        INTEGER IFILT,IVEGA
        INTEGER IFLAG,N1,N2
        REAL X1,X2
        REAL X0,Y0_FILT,Y0_VEGA,DX
        DOUBLE PRECISION DSUM1,DSUM2
C------------------------------------------------------------------------------
        DSUM1=0.D0
        DSUM2=0.D0
        IFILT=0 !ultimo indice usado en el filtro
        X2=WL_FILT(1) !extremo izquierdo del filtro
        DO WHILE(X2.LT.WL_FILT(NPFILT)) !mientras estamos dentro del filtro
          X1=X2
          IF(IFILT.EQ.0)THEN !estamos empezando
            IFILT=1 !primer punto del filtro
            IVEGA=NPVEGA/2 !como arranque en la busqueda tomamos el centro
            CALL BINSEARCH(WL_VEGA,NPVEGA,X1,IVEGA) !buscamos X1 en WL_VEGA
          END IF
          IF(WL_FILT(IFILT+1).LT.WL_VEGA(IVEGA+1))THEN
            IFILT=IFILT+1
            X2=WL_FILT(IFILT)
          ELSE
            IVEGA=IVEGA+1
            X2=WL_VEGA(IVEGA)
          END IF
          DX=X2-X1
          X0=(X1+X2)/2.
          Y0_FILT=LININTERP(NPFILT,WL_FILT,FLUX_FILT,X0,IFLAG,N1,N2)
          Y0_VEGA=LININTERP(NPVEGA,WL_VEGA,FLUX_VEGA,X0,IFLAG,N1,N2)
          DSUM1=DSUM1+DBLE(X0*Y0_VEGA*Y0_FILT*DX)
          DSUM2=DSUM2+DBLE(Y0_VEGA*Y0_FILT*DX)
        END DO
C
        SUM1=REAL(DSUM1)
        SUM2=REAL(DSUM2)
        LAMBDAEFFVEGA=SUM1/SUM2
C
        END
C
C******************************************************************************
C Calcula la magnitud de un objeto (Eq. 1 de Fukugita et al.). El metodo de 
C integracion es el mismo que en el caso de FLUXEFFVEGA y LAMBDAEFFVEGA.
C
        REAL FUNCTION MAGNITUDE(NPFILT,WL_FILT,FLUX_FILT,
     +                          NPVEGA,WL_VEGA,FLUX_VEGA,
     +                          NPSPEC,WL_SPEC,FLUX_SPEC,SUM1,SUM2)
        IMPLICIT NONE
C
        INTEGER NPFILT
        REAL WL_FILT(NPFILT),FLUX_FILT(NPFILT)
        INTEGER NPVEGA
        REAL WL_VEGA(NPVEGA),FLUX_VEGA(NPVEGA)
        INTEGER NPSPEC
        REAL WL_SPEC(NPSPEC),FLUX_SPEC(NPSPEC)
        REAL SUM1,SUM2
C
        REAL LININTERP
C
        INTEGER IFILT,IVEGA,ISPEC
        INTEGER IFLAG,N1,N2
        REAL X1,X2
        REAL X0,Y0_FILT,Y0_VEGA,Y0_SPEC,DX
        DOUBLE PRECISION DSUM1,DSUM2
C------------------------------------------------------------------------------
        DSUM1=0.D0
        DSUM2=0.D0
        IFILT=0 !ultimo indice usado en el filtro
        X2=WL_FILT(1) !extremo izquierdo del filtro
        DO WHILE(X2.LT.WL_FILT(NPFILT)) !mientras estamos dentro del filtro
          X1=X2
          IF(IFILT.EQ.0)THEN !estamos empezando
            IFILT=1 !primer punto del filtro
            IVEGA=NPVEGA/2 !como arranque en la busqueda tomamos el centro
            CALL BINSEARCH(WL_VEGA,NPVEGA,X1,IVEGA) !buscamos X1 en WL_VEGA
            ISPEC=NPSPEC/2 !como arranque en la busqueda tomamos el centro
            CALL BINSEARCH(WL_SPEC,NPSPEC,X1,ISPEC) !buscamos X1 en WL_VEGA
          END IF
          IF((WL_FILT(IFILT+1).LT.WL_VEGA(IVEGA+1)).AND.
     +     (WL_FILT(IFILT+1).LT.WL_SPEC(ISPEC+1)))THEN
            IFILT=IFILT+1
            X2=WL_FILT(IFILT)
          ELSEIF((WL_VEGA(IVEGA+1).LT.WL_FILT(IFILT+1)).AND.
     +     (WL_VEGA(IVEGA+1).LT.WL_SPEC(ISPEC+1)))THEN
            IVEGA=IVEGA+1
            X2=WL_VEGA(IVEGA)
          ELSE
            ISPEC=ISPEC+1
            X2=WL_SPEC(ISPEC)
          END IF
          DX=X2-X1
          X0=(X1+X2)/2.
          Y0_FILT=LININTERP(NPFILT,WL_FILT,FLUX_FILT,X0,IFLAG,N1,N2)
          Y0_VEGA=LININTERP(NPVEGA,WL_VEGA,FLUX_VEGA,X0,IFLAG,N1,N2)
          Y0_SPEC=LININTERP(NPSPEC,WL_SPEC,FLUX_SPEC,X0,IFLAG,N1,N2)
          DSUM1=DSUM1+DBLE(Y0_SPEC*Y0_FILT*DX)
          DSUM2=DSUM2+DBLE(Y0_VEGA*Y0_FILT*DX)
        END DO
C
        SUM1=REAL(DSUM1)
        SUM2=REAL(DSUM2)
        MAGNITUDE=-2.5*ALOG10(SUM1/SUM2)
C
        END
C
C******************************************************************************
C Calcula el coeficiente xi asociado al cálculo de errores en colores.
C
        REAL FUNCTION XIFILTER(NPFILT,WL_FILT,FLUX_FILT)
        IMPLICIT NONE
C
        INTEGER NPFILT
        REAL WL_FILT(NPFILT),FLUX_FILT(NPFILT)
C
        INTEGER I
        REAL Y0_FILT,DX
        DOUBLE PRECISION DSUM1,DSUM2
c..............................................................................
!       integer ldo1,ldo2,ldo
!       integer iflag,n1,n2
!       real lininterp
!       real fldo,yldo
!       double precision sum1,sum2
C------------------------------------------------------------------------------
        DSUM1=0.D0
        DSUM2=0.D0
        DO I=1,NPFILT-1 !con N puntos hay N-1 intervalos
          DX=WL_FILT(I+1)-WL_FILT(I)
          Y0_FILT=(FLUX_FILT(I)+FLUX_FILT(I+1))/2.
          DSUM1=DSUM1+DBLE(Y0_FILT*Y0_FILT*DX)
          DSUM2=DSUM2+DBLE(Y0_FILT*DX)
        END DO
C
        XIFILTER=REAL(DSUM1/(DSUM2*DSUM2))
C..............................................................................
C hacemos el sumatorio en angstroms para comprobar que es equivalente
!       sum1=0.D0
!       sum2=0.D0
!       ldo1=nint(wl_filt(1))-1
!       ldo2=nint(wl_filt(NPFILT))+1
!       print*,ldo1,ldo2
!       do ldo=ldo1,ldo2
!         fldo=real(ldo)
!         if((fldo.ge.WL_FILT(1)).and.(fldo.le.WL_FILT(NPFILT)))then
!           yldo=lininterp(NPFILT,WL_FILT,FLUX_FILT,fldo,iflag,n1,n2)
!           sum1=sum1+dble(yldo*yldo)
!           sum2=sum2+dble(yldo)
!         end if
!       end do
!       print*,'xi=',sum1/(sum2*sum2)
C
        END
