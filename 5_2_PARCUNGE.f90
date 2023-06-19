    !---------------------------------------------------------------------------------
    !  Discussion:
    ! 
    !    This Sub-Routine calculates the dtcal and ntrech opitimal values.
    !
    !    !dtcal: Computational time of Muskingum-Cunge method
    !    !ntrech: Sub-river number
    !
    ! Usage:
    !
    !    * no files are opened in this routine
    !
    ! uses modules and functions
    !
    !    * module VARS_MAIN in VARSMAIN.f90
    !
    ! opens
    !
    ! * no files are opened in this routine
    !
    ! reads
    !
    ! * no files are read in this routine
    !
    ! creates
    !
    ! * no files are created in this routine
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
    !    2014.09.001 - 09 September 2014
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
    ! Variables and Parameters:
    ! *Variables declarations and routines calls are all commented below.
    !---------------------------------------------------------------------------------
    ! End of header
    !---------------------------------------------------------------------------------
    
	SUBROUTINE PARCUNGE

    ! Variables and Parameters
	USE VARS_MAIN
	IMPLICIT NONE
	INTEGER IWCT,NTRECH
	REAL COEF,AT,DTCAL 	
	REAL DX
	REAL DECLIV,ALONG,CLX 	

	ICODMUSK=0 
	DO IC=1,NC
		IF(OD(IC)>1)THEN ! Unit-Catchment order
			COEF=1.67*DECL(IC)**0.3/RUGMAN(IC)**0.6 !FMF 09/09/2015 
			CEL(IC)=COEF*(QREF(IC)/BRIO(IC))**0.4
			AT=DTP				
			DTCAL=AT/24.
			IWCT=24

			QRX=QREF(IC)
			DECLIV=DECL(IC)
			BRX=BRIO(IC)
			CLX=CEL(IC)
            ! Ideal DX using Fread Equation - Fread(1992) Flow Routing, in Handbook of Hydrology, Maidment
			DX=0.5*CLX*DTCAL*(1+(1+1.5*(QRX/BRX)/(CLX**2*DECLIV*DTCAL))**0.5)
			ALONG=SRIO(IC)*1000. !To meters 
			NTRECH=NINT(ALONG/DX) !Calculates Ntrech number
			NTRECH=MAX(NTRECH,1)
			IF(NTRECH.GT.NUMUSK)THEN 
                write(*,*),IC
				WRITE(*,*) 'ntrech number greater then', NUMUSK,' in parcunge subroutine!!!'
				PAUSE 'STOP - Anything is wrong '
			ENDIF
			DT(IC)=DTCAL
			NSUBT(IC)=NTRECH

		ELSE
			DT(IC)=0.0
			NSUBT(IC)=0
		ENDIF

	ENDDO
	
	RETURN
	END
