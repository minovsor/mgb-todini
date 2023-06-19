    !*********************************************************************************
    !
    !  SUBROUTINE LEFIX reads simulation main parameters
    !
    !---------------------------------------------------------------------------------
    !  Discussion:
    ! 
    !    This routine reads simulation setup patameters

	!
	!	 LECLIMED is called inside subroutine MGB_Inercial.
	!
	!	 Saves global variable: 
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
    !    * Opens infoMGB.sim file  containing simulation setup parameters.
    !
    !    reads
    !
    !    * Reads infoMGB.sim file  containing simulation setup parameters.
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
    !    2015.21.06 - 21 June 2015 (By: Fernando Mainardi Fan)    
    !
    !  Authors:
    !
    !    Original fortran version by Walter Collischonn
    !    Present fortran version by:
    !    * Walter Collischonn
    !    * Rodrigo Cauduro Dias de Paiva
    !    * Diogo da Costa Buarque
    !    * Paulo Pontes RÃ³genes
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


	SUBROUTINE LEFIX
	USE VARS_MAIN
    use VARS_INERC
	IMPLICIT NONE
	INTEGER K
	
	! Opens input file contaning simulation setup parameters
	OPEN(FILFIX,FILE=INPUT_DIRECTORY // 'infoMGB.sim',STATUS='OLD',ACTION='READ')
    
	READ(FILFIX,75)TITULO ! Reads text comments
	READ(FILFIX,75)TITULO ! Reads blanc line
	READ(FILFIX,75)TITULO ! Reads text comments
	READ(FILFIX,*)TITULO ! Reads blanc line

    READ(FILFIX,72)(CABE(K),K=1,4)
	READ(FILFIX,71)IDIA,IMES,IANO,HORAINI ! Reads initial time of simulation: day, month, year, and hour
	
    READ(FILFIX,*) ! Blanc line
	READ(FILFIX,72)(CABE(K),K=1,2)
	READ(FILFIX,76)NT,DTP ! Reads number of time steps and size of time step (sec)
	
    READ(FILFIX,*) ! Blanc line
	READ(FILFIX,72)(CABE(K),K=1,4)
    READ(FILFIX,70)NC,NU,NB,NCLI,CRU_INDEX,IND_RESERV,IND_FLOODROUTING, ALPHA100 ! Reads number of catchments, HRUs, sub-basins and climate stations
	write(*,*)NC,NU,NB,NCLI,CRU_INDEX,IND_RESERV, IND_FLOODROUTING,ALPHA100
    ALPHA = ALPHA100/100.0
    
    READ(FILFIX,*) ! Blanc line.
	READ(FILFIX,72)(CABE(K),K=1,1)
	
    READ(FILFIX,71)ICALIB	
	READ(FILFIX,*) ! Blanc line
	READ(FILFIX,75)TITULO !Text comments

    if(NCLI<=0)then ! Control type of climate data.
	    NCLI=-NCLI !Uses only climatology data
	    flagaclimed=1	
    else
	    flagaclimed=0 !Uses daily climate data
	    DO K=1,NCLI
	        READ(FILFIX,77)ARQCLI(K) !read name of daily climate data
        ENDDO
    endif

    READ(FILFIX,*) ! Blanc line
	READ(FILFIX,75)TITULO ! Text comments
	READ(FILFIX,*)ACLIMED

	READ(FILFIX,*) ! Blanc line
	READ(FILFIX,75)TITULO ! Text comments
	READ(FILFIX,*)NOBS, ARQOBS !Reads number of stream flow gauging stations and name of file with discharge data. 

	READ(FILFIX,*) ! Blanc line
	READ(FILFIX,75)TITULO ! Text comments
	READ(FILFIX,*)(IQOBS(K),K=1,NOBS) ! ID codes of catchments for each gauge

	READ(FILFIX,*) ! Blanc line
	READ(FILFIX,75)TITULO ! Text comments. 
	READ(FILFIX,*)NUMHIDG ! Number of catchments to write discharge time series. 

	READ(FILFIX,*) ! Blanc line
	READ(FILFIX,75)TITULO ! Text comments

	DO K=1,NUMHIDG
	    READ(FILFIX,*)IHIDG(K)!,ARQHID(K) ! ID codes of catchments where discharge time series will be saved. 
	ENDDO

	READ(FILFIX,*)
	READ(FILFIX,75)TITULO !LE TEXTO
	READ(FILFIX,*)NUMSUBST,ARQSUBST ! Number of catchments for discharge substitution. (Discharge is read from input file) and name of file with discharge data. 

	READ(FILFIX,*)
	READ(FILFIX,75)TITULO

	DO K=1,NUMSUBST
	    READ(FILFIX,*)ISUBST(K) ! ID codes of catchments where discharge substitution is performed
    ENDDO
 
	CLOSE (FILFIX)
    70	FORMAT(8I10)
    71  FORMAT(6I10)
	72	FORMAT(5A10)
	73	FORMAT(F10.2)
	74	FORMAT(A10)
	75	FORMAT(A20)
	76	FORMAT(I10,F10.1)
    77  FORMAT(A20)     
	RETURN
	END
