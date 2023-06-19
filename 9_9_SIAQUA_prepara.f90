!---------------------------------------------------------------------------------
! SIAQUA_prepara.f90
!---------------------------------------------------------------------------------
! Discussion:
!
! This subroutine calculates gumbel statitics and the flow duration curve in order to prepare an input file for SIAQUA
!
! Usage:
!
! CALL SIAQUA_prepara
!
! uses modules, functions, and subroutines
!
! * VARS_MAIN.f90
!
! opens
!
!	 * Opens Qmes90.TXT 	   output  ascii  file with calculated Q90 in all catchments
!	 * Opens Qmes95.TXT 	   output  ascii  file with calculated Q95 in all catchments
!	 * Opens RESUMO_SIAQUA.TXT output  ascii  file for SIAQUA	
!
! reads
!
! * No files are read
!
! creates
!
! * No files are created
!
!---------------------------------------------------------------------------------
!  Licensing:
!
!   This program is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   This program is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with this program.  If not, see <http://www.gnu.org/licenses/>
!
!  Version/Modified: 
!
! 2014.11.12 - 22 November 2014
! By: Fernando Mainardi Fan
!
! Authors:
!
! Original fortran version by Walter Collischonn
! Present fortran version by:
! * Walter Collischonn
! * Rodrigo Cauduro Dias de Paiva
! * Diogo da Costa Buarque
! * Paulo Pontes Rógenes
! * Mino Viana Sorribas
! * Fernando Mainardi Fan
! * Juan Martin Bravo
!
! Main References:
! COLLISCHONN, W. ; ALLASIA, D. G. ; SILVA, B. C. ; TUCCI, C. E. M. The MGB-IPH model for large-scale rainfall-runoff modelling. Hydrological Sciences Journal, v. 52, p. 878-895, 2007.
! COLLISCHONN, W., TUCCI, C. E. M. Simulação hidrológica de grandes bacias. Revista Brasileira de Recursos Hídricos, v. 6, n. 2, 2001.
! COLLISCHONN, W. Modelagem de Grandes Bacias - ph.d. Thesis. 2001
! FAN, F. M. ; MELLER, A. ; COLLISCHONN, W. . Incorporação de filtro numérico de separação de escoamento na assimilação de dados para previsão de vazões utilizando modelagem hidrológica. Revista Brasileira de Recursos Hídricos, 2015. 
! PAZ AR, COLLISCHONN W, TUCCI C, CLARKE R, ALLASIA D. Data Assimilation in a Large-scale Distributed Hydrological Model for Medium Range Flow Forecasts. IAHS Press, Wallingford, UK, IAHS Publication, n. 313, p. 471–478, 2007.
!
!---------------------------------------------------------------------------------
! Variables and Parameters:
! *Variables declarations and routines calls are all commented below.
!---------------------------------------------------------------------------------
! End of header
!---------------------------------------------------------------------------------
	SUBROUTINE SIAQUA_prepara

	USE VARS_MAIN
    USE IFPORT
	IMPLICIT NONE
    
    REAL,ALLOCATABLE:: QSOR(:) 		!observed discharges (without "missing data") for sorting and exceedance probaility curve
    INTEGER MES,KSOR,IPOS90,IPOS95 				  !indexes and counter
	
    !REAL QMES90(NC,12),QMES95(NC,12)              !Q90, Q95 and Long-term Mean Discharge for each month in each catchment
	REAL,ALLOCATABLE:: QMES90(:,:), QMES95(:,:)
    
    INTEGER IPOS,IPERMA
	
    !REAL QPERMA(NC,100) 			! Discharge values in Exceedance Probability Curve for each catchment
	!REAL HPERMA(NC,100) 			! Water Depth values in Exceedance Probability Curve for each catchment
	!REAL VPERMA(NC,100) 			! Velocity values in Exceedance Probability Curve for each catchment
    REAL,ALLOCATABLE:: QPERMA(:,:),HPERMA(:,:),VPERMA(:,:)
    
    !REAL RUGMAN						! Manning bed roughness coeficient !RUGMAN IS DECLARED IN VARS_MAIN AND ALLOCA_VARS SUBROUTINES	!FMF 09/09/2015 
    INTEGER ITini
    REAL,ALLOCATABLE:: Qmean(:),Qmin(:),Qmax(:),QmaxTR(:,:),Qmax_aux(:) 
    INTEGER,ALLOCATABLE:: ANOaux(:)  ! RP abril/2013
    INTEGER:: i,j
    REAL:: gumbel_v,gumbel_b,gumbel_med,gumbel_var,gumbel_T
    
    ALLOCATE (QMES90(NC,12),QMES95(NC,12))
    ALLOCATE (QPERMA(NC,100),HPERMA(NC,100),VPERMA(NC,100))
    
    OPEN(1960,FILE=OUTPUT_DIRECTORY // 'Qmes90.TXT',STATUS='UNKNOWN',ACTION='WRITE')
	OPEN(1961,FILE=OUTPUT_DIRECTORY // 'Qmes95.TXT',STATUS='UNKNOWN',ACTION='WRITE')
	OPEN(1971,FILE=OUTPUT_DIRECTORY // 'RESUMO_SIAQUA.TXT',STATUS='UNKNOWN',ACTION='WRITE')
        
    write(1971,'(A200)')'      MINI       Q5       Q10       Q30       Q50       Q70       Q90       Q95       V10       V30       V50       V70       V90   LARGURA     Qmean     Qmax     Qmin    Qmax05ano    Qmax10ano     Qmax25ano' ! RP abril/2013

    !-------------------------------------------------------------
    ! Record discharge values for cumulative exceedances, from 1 to 99% (1% interval) for each catchment
	DO IC=1,NC

			ITini=1 	 !Should be 365 days due to intial conditions		! Initial time to consider
			KSOR=NT-ITini+1 		! Size of vector
			ALLOCATE (QSOR(KSOR))   ! Allocate
			QSOR=0.0 				! Intitialize - zeros.

			j=0
			DO IT=ITini,NT
				j=j+1
				QSOR(j)=QTUDO(IC,IT)
			ENDDO

			! Sort values on vector
			Call SORTQQ (LOC(QSOR), KSOR, SRT$REAL4)            
			
			! Store values in QPERMA
			DO IPERMA=1,99
			    IPOS=INT((100-IPERMA)*KSOR/100) 
			    QPERMA(IC,IPERMA)=QSOR(IPOS)
			ENDDO    
			DEALLOCATE (QSOR)
	ENDDO

    !-------------------------------------------------------------
    ! Calculates mean, min and max discharge
    allocate(Qmean(nC),Qmin(nC),Qmax(nC))
    ITini= 1 !367 !Should be 365 days due to intial conditions	
	KSOR=NT-ITini+1 ! RP abril/2013
    Qmean=sum(QTUDO(:,ITini:NT),DIM=2)/KSOR
    Qmin=minval(QTUDO(:,ITini:NT),DIM=2)
    Qmax=maxval(QTUDO(:,ITini:NT),DIM=2)
    
	!-------------------------------------------------------------
    ! Calculates annual maxima for diferente return periods
	! Year vector:
	allocate(ANOaux(NT))
	do iT=1,NT
	    JDIA=IDINI+INT((IT+HORAINI-1)/(86400./DTP))
	    CALL CALDAT(JDIA,IMES,IDIA,IANO)
	    ANOaux(iT)=IANO
	enddo
	KSOR=ANOaux(NT)-ANOaux(ITini)+1
	
	allocate(Qmax_aux(KSOR),QmaxTR(nC,3))	!dim=3 because, three return periods, defined below
    QmaxTR=0.0
    
	! Loop for catchments
    do iC=1,nC
    	Qmax_aux=0.0
        ! Store max discharge for each year
        j=0
        do i=ANOaux(ITini),ANOaux(NT)
            j=j+1
            Qmax_aux(j)=maxval(QTUDO(iC,:),MASK=(ANOaux==i))
        enddo
        
        ! Mean
        gumbel_med=sum(Qmax_aux)/KSOR
        ! Variance
        Qmax_aux=Qmax_aux-gumbel_med
        Qmax_aux=Qmax_aux**2
        gumbel_var=sum(Qmax_aux)/(KSOR-1)
        ! Gumbel Distribution parameters
        gumbel_v=((6.*gumbel_var)**0.5)/3.14159265359
        gumbel_b=gumbel_med-0.5772*gumbel_v
        
		! 5 year
        gumbel_T=5.
        QmaxTR(iC,1)=gumbel_b-gumbel_v*log(-log(1-1/gumbel_T))
        
        ! 10 year
        gumbel_T=10.
        QmaxTR(iC,2)=gumbel_b-gumbel_v*log(-log(1-1/gumbel_T))
        
        !25 year
        gumbel_T=25.
        QmaxTR(iC,3)=gumbel_b-gumbel_v*log(-log(1-1/gumbel_T))
        
    enddo
    
	!----------------------------------------------------------------------------------------------------------
    ! Calculates water depth and velocity for each discharge in flow exceedance curve, for allcatchment
    DO IC=1,NC
        DO IPERMA=1,99
            HPERMA(IC,IPERMA)=((QPERMA(IC,IPERMA)*RUGMAN(IC))/(BRIO(IC)*(DECL(IC))**0.5))**0.6 !Estimated depth based n discharge and manning !FMF 09/09/2015 
            VPERMA(IC,IPERMA)=QPERMA(IC,IPERMA)/(BRIO(IC)*HPERMA(IC,IPERMA))
        ENDDO
        

        ! Record header in RESUMO_SIAQUA.TXT
        write(1971,'(I10,19F10.3)') IC,QPERMA(IC,5),(QPERMA(IC,IPERMA),IPERMA=10,90,20),QPERMA(IC,95),(VPERMA(IC,IPERMA),IPERMA=10,90,20),BRIO(IC),Qmean(IC),Qmax(IC),Qmin(IC),(QmaxTR(IC,i),i=1,3)
    ENDDO
    
    
  	!----------------------------------------------------------------------------------------------------------
	! Calculates Q90 and Q95 for for each month for all catchments
	DO IC=1,NC
		DO MES=1,12
			KSOR=0
			DO IT=1,NT
				JDIA=IDINI+IT-1
				CALL CALDAT(JDIA,IMES,IDIA,IANO)
				IF(IMES==MES)THEN
					KSOR=KSOR+1
				ENDIF
			ENDDO
			ALLOCATE (QSOR(KSOR))
			KSOR=0
			QSOR=0.0
			DO IT=1,NT
				JDIA=IDINI+IT-1
				CALL CALDAT(JDIA,IMES,IDIA,IANO)
				IF(IMES==MES)THEN
					KSOR=KSOR+1
					QSOR(KSOR)=QTUDO(IC,IT)
				ENDIF
			ENDDO
			!Sort values
			Call SORTQQ (LOC(QSOR), KSOR, SRT$REAL4)
			!Get Q90
			IPOS90=INT(KSOR/10)
			QMES90(IC,MES)=QSOR(IPOS90)

			!Get Q95
			IPOS95=INT(KSOR/20)
			QMES95(IC,MES)=QSOR(IPOS95)

			DEALLOCATE (QSOR)
		ENDDO
	ENDDO



	! Record file with monthly Q90 for all catchments
	DO IC=1,NC
		WRITE(1960,'(I6,12F10.3)')IC,(QMES90(IC,MES),MES=1,12)
	ENDDO
	close (1960)
	

	! Record file with monthly Q95 for all catchments
	DO IC=1,NC
		WRITE(1961,'(I6,12F10.3)')IC,(QMES95(IC,MES),MES=1,12)
	ENDDO
	close (1961)

    
    DEALLOCATE (QMES90,QMES95)
    DEALLOCATE (QPERMA,HPERMA,VPERMA)
    
    END
    