    !---------------------------------------------------------------------------------
    !  Discussion:
    ! 
    !    This Sub-Routine find the best climatic station number to each unit-catchment
    !
    ! Usage:
    !
    !   * no files are opened in this routine
    !
    ! uses modules and functions
    !
    !    * module VARS_MAIN in VARSMAIN.f90
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
    
	SUBROUTINE INTECLIM

    ! Variables and Parameters
	USE VARS_MAIN
	IMPLICIT NONE

	REAL DMIN,DIST
	INTEGER IP,IPMIN

	DO IC=1,NC !unit-catchment loop
		DMIN=1000000.0
		DO IP=1,NCLI !Climatic station loop
			DIST=((X(IC)-XYC(IP,1))**2.+(Y(IC)-XYC(IP,2))**2.)**0.5
			IF(DIST.LT.DMIN)THEN !Save the best station number
				DMIN=DIST
				IPMIN=IP
			ENDIF
		ENDDO
		ICBOM(IC)=IPMIN
	ENDDO
	RETURN
	END
