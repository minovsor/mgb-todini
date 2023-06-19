    !*********************************************************************************
    !
    !  SUBROUTINE MODELO controls the main time loop in MGB-IPH and call routines
	!					that realize catchment and river routing
    !
    !---------------------------------------------------------------------------------
    !  Discussion:
    ! 
    !    This routine has the main loop of the MGB-IPH, the time loop, from iT=1,nT
    !     where iT is the time interval and nT is the number of time steps.
	!
	!	 For each time interval date info is set, then rainfall and climate data are
	!	   loaded through subroutines LECHUVA and LECLIMA. Afterwards catchment flow
	!	   generation is done using subroutine CELULA. Finally, river routing is 
	!	   achieved by calling (i) REDE, for Muskingum-Cunge Method or
	!	   (ii) flood_inertial, for a 1D Hydrodynamic Model (w/ inertia and pressure)
	!	
	!	At the end discharge time series are stored in :
	!		QRB: calculated discharge time series in all subbasins
	!		QRG: calculated discharge time series in catchments pre-defined by user
	!		QR:  calculated discharge time series in subbasin outlet w observed data
	!	Those are recorded in files at the end when returns to SIMULA
	!
	!
	!	 * iT value should not be changed inside subroutines!
	!
	!    * AUX_MOD module from full hydrodynamic is deactivated (commented)!
	!    * Hidrodinamico2 subroutine from full hydrodynamic is deactivated (commented)!
    !
    !  Usage:
    !
    !    CALL MODELO
    !
    !    where
    !
    !    * no arguments are passed in this subroutine
    !
    !    uses modules and functions
    !
	!	 * function CALDAT	      		in	  caldat.f90
    !    * module     VARS_MAIN   		in    VARS_MAIN.f90
    !    * module     VARS_INERC  		in    VARSINERC.f90  !Quais? 
 	!	 * subroutine LECHUVA	  		in	  LECHUVA.f90
	!	 * subroutine LECLIMA	  		in	  LECLIMA.f90
	!	 * subroutine CELULA	  		in	  CELULA.f90
	!	 * subroutine REDE	      		in	  REDE.f90	
	!	 * subroutine flood_inercial	in	  flood_inercial.f90
	!
	!    Deactivated:	
	!	 * module	  AUX_MOD        	in	  HD_AUX_MOD.f90
	!	 * subroutine Hidrodinamico2	in	  Hidrodinamico2.f90
    !
    !	 opens
    !
    !    * Does not open files
    !
    !    reads
    !
    !    * Does not read files
    !
    !    creates
    !
    !    * Does not create files
    !    
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
    !    2014.25.11 - 25 November 2014 (By: Mino V. Sorribas)    
    !
    !  Authors:
    !
    !    Original fortran version by Walter Collischonn
    !    Present fortran version by:
    !    * Walter Collischonn
    !    * Rodrigo Cauduro Dias de Paiva
    !    * Diogo da Costa Buarque
    !    * Paulo Pontes Rógenes
    !    * Mino  Viana Sorribas
    !    * Fernando Mainardi Fan
    !    * Juan Martin Bravo 
    !
    !  Main Reference:
    !
    !    Walter Collischonn,
    !    Modelo de Grandes Bacias - Thesis
    !    Porto Alegre, 2001
    !    ISBN: XXXXXXXXXXX,
    !
    !---------------------------------------------------------------------------------
    !  Variables and Parameters:
    !
    !   * Variables declarations and routines calls are all commented below.
    !
    !---------------------------------------------------------------------------------
	SUBROUTINE MODELO

	USE VARS_MAIN
	USE VARS_INERC
    USE MUSKTODINI_VARS !MS: TODINI
    
	IMPLICIT NONE
	INTEGER K,KHID 					!indexes and counters
	INTEGER KC,KB,JCB,MWP 	!... same


	! Initialize
	IT=0
	
    DO K=1,NUMSUBST
        nentradas=minimont(ISUBST(K),1)
        DO KC=1,nentradas
            CELJUS(minimont(ISUBST(K),1+KC))=-1
        end do        
	ENDDO
    
    
    ! Time Loop    
    DO WHILE (IT<NT)
		
		IT=IT+1
        if(mod(it,100)==0) write(*,*)100*iT/NT,'%'
!		! Save individual records of Discharge, water level and depth for all catchments
		! only at first time-step ?!


		JDIA=IDINI+INT((IT+HORAINI-1)/(86400./DTP)) 	! Gets calendar day (big number)
		CALL CALDAT(JDIA,IMES,IDIA,IANO)

		DIAH(IT)=IDIA !stores day corresponding to iT time interval
		MESH(IT)=IMES !stores month corresponding to iT time interval
		ANOH(IT)=IANO !stores year corresponding to iT time interval
		HORAH(IT)=MOD(IT,24)+HORAINI 	!hour of day corresponding to iT time interval

		
		! Read rainfall data for all catchments in iT time-step
		ITCHUVA=IT
		CALL LECHUVA
	
		DO KC=1,NC
			PM(KC)=PM(KC)+P(KC) !Cumulative rainfall (used later for average)
		ENDDO		
	
		! Reads climate data
		TONTEM=TA
		CALL LECLIMA
	
		! Calls catchment routing/discharge for lateral inflow
		CALL CELULA		
	
		! Call main river routing routine using Muskingum-Cunge Method        
		if(hdFLAG0==0) then            
            IF (flag_todini>0) THEN !MS: TODINI
                CALL MUSKTODINI_REDE
            ELSE
                CALL REDE
            END IF
            
		endif
		
		! Calculates lateral inflow ! necessary to run inertial model and to save files! 
        do ic=1,nc
		    QCEL2(IC)=QBAS(IC)+QINT(IC)+QSUP(IC) !sums surface, subsurface and 
            QITUDO(IC,IT)=QCEL2(IC) !save QCEL information at binary file !FMF and PETER 10/08/2016
		    QBASTUDO(IC,IT)=QBAS(IC) !save minibasin QBAS only information at binary file !FMF and PETER 10/08/2016
		enddo

        ! Calls river routing routine using Inertial Method
        if(hdFLAG0>0)then
            CALL flood_inercial
        endif

		! Stores calculated discharges in sub-basins with observed data - for calibration and assessment.
		DO K=1,NOBS
			KHID=IQOBS(K) 		! outlet catchment id
			QR(K,IT)=QJ2(KHID)  ! saves on QR(K,IT), that will be compared to QOBS in FOBJ routine.
        ENDDO
		
  
		! Stores discharges for file ouput when in simulation model
		IF(ICALIB.EQ.0)THEN 	! only if it is not calibration
			
			DO KB=1,NB				! store discharge in sub-basin
				JCB=IEXUT(KB) 		! outlet catchment of KB-th subbasin
				QRB(KB,IT)=QJ2(JCB)
			ENDDO
	
			DO K=1,NUMHIDG 				! store discharge in catchments defined in PARHIG.HIG
				KHID=IHIDG(K) 			! indexed catchment
				QRG(K,IT)=QJ2(KHID) 	! stored in QRG
				
			if(hdFLAG0>0)then !Stores the water level for inertial model 	           
                HRG(K,IT)=Hfl(KHID) ! stored in HRG                  
            endif     
				
			ENDDO
	
			! Store discharge by water origin (i.e. surface, subsurface, groundwater) in a specified catchment
			MWP=nC 						!catchment (this is manual)
			QB(IT)=QJ2(MWP)*PJB2(MWP)
			QBI(IT)=QB(IT)+QJ2(MWP)*PJI2(MWP)
			QBIS(IT)=QBI(IT)+QJ2(MWP)*PJS2(MWP)
        ENDIF
        
 
        
	
	ENDDO !End time loop
	

	

75	FORMAT(I6,8F10.4)

66	FORMAT(<nC>F10.4)

     
	RETURN
	END
