    !*********************************************************************************
    !
    !  SUBROUTINE LECLIMA select meteorological data
    !
    !---------------------------------------------------------------------------------
    !  Discussion:
    ! 
    !    This routine select meteorological data

	!
	!	 LECLIMA is called inside subroutine MGB_Inercial.
	!
	!	 Saves global variable: 
	!		TA = monthly climatology of near surface temperature
	!       UR = monthly climatology of near surface relative humidity (%)  
	!       SOL = monthly climatology of sunshine hours (hours/day)  
	!       VV = monthly climatology of near surface wind speed)  
	!       UR = monthly climatology of near surface relative humidity (%)  
	!       PA= monthly climatology of near surface atmospheric pressure (kPa) 
	!
    !
    !  	Usage:
    !
    !    * CALL caldat(julian,mm,id,iyyy) determines the day/month/year of the Julian Calendar
    !
    !    where
    !
    !    * no arguments are passed in this subroutine
    !
    !    uses modules and functions
    !
    !    * module     VARS_MAIN   in      VARS_MAIN.f90
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
    !    2014.26.11 - 25 November 2014 (By: Rodrigo Paiva)    
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
    !   *Variables declarations and routines calls are all commented below.
	!	* All variables are global!?
    !
    !---------------------------------------------------------------------------------		
    SUBROUTINE LECLIMA
	!THIS SUBROUTINE PREPARES CLIMATE DATA
	USE VARS_MAIN
	IMPLICIT NONE
	INTEGER KLI


	DO IC=1,NC
		KLI=ICBOM(IC)

		!AIR TEMPERATURE
		IF(TD(KLI,IT).LT.-200.0.OR.flagaclimed==1)THEN !(TESTS FAILURE=-1.0)
			TA(IC)=TAMM(KLI,IMES)	!USES CLIMATOLOGY DATA
		ELSE
			TA(IC)=TD(KLI,IT) !USES DAILY DATA
		ENDIF
		
		!RELATIVE HUMIDITY
		IF(UD(KLI,IT).LT.0.0.OR.flagaclimed==1)THEN !TESTA FALHA (FALHA=-1.0)
			UR(IC)=URMM(KLI,IMES) !USES CLIMATOLOGY DATA
		ELSE
			UR(IC)=UD(KLI,IT) !USES DAILY DATA
		ENDIF

		!WIND SPEED
		IF(VD(KLI,IT).LT.0.0.OR.flagaclimed==1)THEN !
			VV(IC)=VVMM(KLI,IMES) ! IN M/S
		ELSE
			VV(IC)=VD(KLI,IT) !USES DAILY DATA
		ENDIF	        

		!HOURS OF SUNLIGHT
		IF(SOLD(KLI,IT).LT.0.0.OR.flagaclimed==1)THEN
			SOL(IC)=SOLMM(KLI,IMES) !IN HOURS/DAY
		ELSE
			SOL(IC)=SOLD(KLI,IT) !USES DAILY DATA
		ENDIF

		!ATMOSPHERIC PRESSURE 
		IF(PAD(KLI,IT).LT.0.0.OR.flagaclimed==1)THEN
			PA(IC)=PAMM(KLI,IMES) 
		ELSE
			PA(IC)=PAD(KLI,IT) !USES DAILY DATA
		ENDIF
	ENDDO

734	FORMAT(5F10.2)
	RETURN
	END