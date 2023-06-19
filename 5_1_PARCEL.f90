    !*********************************************************************************
    !
    !  SUBROUTINE PARCEL is the routine that calculates additional catchment and river
	!			parameters necessary for routing and other operations
    !
    !---------------------------------------------------------------------------------
    !  Discussion:
    ! 
    !    This routine sets catchment additional parameters, mainly width and depth given by
	!		geomorphological relations. Also reference discharge for Muskingum-Cunge method
	!		and concentration time for catchment routing are calculated.
	!
	!	 PARCEL is called in MGB-IPH Main routine.
	!	
    !	 It calls subroutine INTECLIM to assing nearest climate station for each catchment
	!	 	also it calls subroutine REGION to get hydraulic parameters.
	!
    !
    !  	Usage:
    !
    !    CALL PARCEL
    !
    !    where
    !
    !    * no arguments are passed in this subroutine
    !
    !    uses modules and functions
    !
    !    * module     VARS_MAIN   in      VARS_MAIN.f90
    !    * module     VARS_INERC  in      VARS_INERC.f90  
	!    * subroutine INTECLIM    in      INTECLIM.f90 
	!    * subroutine REGION      in      REGION.f90 
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
    !   *Variables declarations and routines calls are all commented below.
    !
    !---------------------------------------------------------------------------------	
	SUBROUTINE PARCEL

	USE VARS_MAIN
	USE VARS_INERC
	IMPLICIT NONE
	INTEGER IW

	! Calculates Time of Concentration (i.e. time of travel) for Catchment
	DO IW=1,NC

        XLADO=LCEL(IW) 			! main river length in catchment (in km) !RP
		DH=HCEL(IW)*LCEL(IW) 	! altitude variation in main river (in meters) !RP

		TIND(IW)=3600.*((0.868*XLADO**3.0)/DH)**0.385	!Concentration Time by Kirpich eq. (ni seconds)
	ENDDO
	

	RETURN
	END
