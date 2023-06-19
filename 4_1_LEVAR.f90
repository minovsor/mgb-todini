    !*********************************************************************************
    !
    !  SUBROUTINE LEVAR reads file with fixed parameters (ALBEDOS, IAF, RS, Z; monthly) (File: ALBIAF.FIX)
    !
    !---------------------------------------------------------------------------------
    !  Discussion:
    ! 
    !    This routine reads file with fixed parameters (ALBEDOS, IAF, RS, Z; monthly) (File: ALBIAF.FIX)

	!
	!	 LEVAR is called inside 1main.
	!
	!	 Saves discharge time series: 
	!     	ALB(IU,K) = albedo
	!     	RIAF(IU,K) = leaf area index.
	!     	Z(IU,K) = vegetation height.
	!     	RS(IU,K) = surface resistance.
	!
	!
	!
    !
    !  	Usage:
    !
    !    * no subroutine is called in this subroutine
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
    !    * file with calibration parameters (File: PARUSO.CAL). 
    !
    !    reads
    !
    !    * file with calibration parameters (File: PARUSO.CAL)
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
    !    2015.27.05 - 27 MAY 2015 (By: Rodrigo Paiva)    
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
	SUBROUTINE LEVAR
	! File with fixed parameters for each HRU (ALBEDOS, IAF, RS, Z; monthly) (File: ALBIAF.FIX)
	USE VARS_MAIN
	IMPLICIT NONE
	INTEGER K
	character (10) CABEV
	CHARACTER (5) AMES(12)
	OPEN(FILVAR,FILE=INPUT_DIRECTORY // 'ALBIAF.fix',STATUS='OLD')
	! Read albedo:
	READ(FILVAR,71)cabev
	READ(FILVAR,73)cabev,(AMES(K),K=1,12)
	DO IU=1,NU
		READ(FILVAR,72)AUSO(IU),(ALB(IU,K),K=1,12)
		!WRITE(*,72)AUSO(IU),(ALB(IU,K),K=1,12) !ENABLE TO SHOW IN THE SCREEN
	ENDDO
	! Read leaf area index:
	READ(FILVAR,71)cabev
	READ(FILVAR,73)cabev,(AMES(K),K=1,12)
	DO IU=1,NU
		READ(FILVAR,72)AUSO(IU),(RIAF(IU,K),K=1,12)
		!WRITE(*,72)AUSO(IU),(RIAF(IU,K),K=1,12) !ENABLE TO SHOW IN THE SCREEN
	ENDDO
	! Read vegetation height:
	READ(FILVAR,71)cabev
	READ(FILVAR,73)cabev,(AMES(K),K=1,12)
	DO IU=1,NU
		READ(FILVAR,72)AUSO(IU),(Z(IU,K),K=1,12)
		!WRITE(*,72)AUSO(IU),(Z(IU,K),K=1,12) !ENABLE TO SHOW IN THE SCREEN
	ENDDO
	! Read surface resistance:
	READ(FILVAR,71)cabev
	READ(FILVAR,73)cabev,(AMES(K),K=1,12)
	DO IU=1,NU
		READ(FILVAR,72)AUSO(IU),(RS(IU,K),K=1,12)
		!WRITE(*,72)AUSO(IU),(RS(IU,K),K=1,12) !ENABLE TO SHOW IN THE SCREEN
	ENDDO
	CLOSE(FILVAR)
71	FORMAT(A10)
72	FORMAT(A10,12F5.2)
73	FORMAT(A10,12A5)
	RETURN
	END
