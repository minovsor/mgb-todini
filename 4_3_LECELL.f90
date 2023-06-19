 !*********************************************************************************
    !
    !  SUBROUTINE LECELL reads catchment parameters from file mini.gtp
    !
    !---------------------------------------------------------------------------------
    !  Discussion:
    ! 
    !    This routine reads parameters of each catchment as surface area, upstream area, river length, etc...

	!
	!	 LECELL is called inside subroutine MGB_Inercial.
	!
	!	 Saves global variables of main catchment parameters:
	!     	X = longitude of catchment centroid,Y = latitude of catchment centroid
	!       IBAC = subbasin ID code
	!       ACEL = surface area (km2)
	!       ACUR = upstream dreainage area (km2)
	!       SRIO = main river length (km)
	!       DECL = valley slope of main river
	!       LCEL = length of main river tributary
	!       HCEL - valley slope of main tributary
	! 	    CELJUS = ID code of downstream catchment
	!       OD = catchment order
	!       hdFLAG = flag for hydrodynamic flow routing
	!       PUSO = fractions of hydrological response units HRUs 
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
	!    * module     VARS_INERC   in     VARS_INERC.f90
    !
    !	 opens
    !
    !    * mini.gtp input file containing catchment parameters
    !
    !    reads
    !
    !    * mini.gtp input file containing catchment parameters
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
    ! 2015.06.21 - 21 June 2015 (By: Fernando Mainardi Fan)    
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

	SUBROUTINE LECELL
	USE VARS_MAIN
	USE VARS_INERC
	IMPLICIT NONE
	! Local variables
	INTEGER ICELL(NC)
	INTEGER IBANT,I,K,KW
	integer aux !RP
    real :: PUSO2
	
	! Opens mini.gtp:
	OPEN(FILHIG,FILE=INPUT_DIRECTORY // 'mini.gtp',STATUS='OLD')
	READ(FILHIG,77) !RP
	IBANT=1
	IEXUT=-999999
	DO I=1,NC
		READ(FILHIG,*)aux,ICELL(I),X(I),Y(I),IBAC(I),ACEL(I),ACUR(I),SRIO(I),DECL(I),LCEL(I),HCEL(I),CELJUS(I),OD(I),hdFLAG(I),BRIO(I),HRIO(I),RUGMAN(I),(PUSO(I,K),K=1,NU) ! RP
       
        QREF(I)=0.333*ACUR(I)**0.7 !Calculates reference initial discharge
                          
		nMan(I)=RUGMAN(I) !Reads inertial model subroutines manning 

        ! Check errors in HRU fractions:
		if (sum(PUSO(I,:))>100.01.or.sum(PUSO(I,:))<99.99.or.minval(PUSO(I,:))<0.0.or.maxval(PUSO(I,:))>100.0) then
			write(*,*) 'Error in the % of HRUs in the catchments',i
			write(*,*) 'Sum=',sum(PUSO(I,:)),'Min=',minval(PUSO(I,:)),'no HRU ',minloc(PUSO(I,:)),'Max=',maxval(PUSO(I,:)),' at HRU',maxloc(PUSO(I,:))
			read(*,*)
        endif

        ! Define outlet of sub-basins:
		! Outlet equals catchment of larger ID code.
		KW=IBAC(I)
		IF(IEXUT(KW)<ICELL(I)) IEXUT(KW)=ICELL(I)

    ENDDO

	write(*,*)''

	! Change units of river slope to m/m
	!write(*,*)'Reach slopes from geometry transformed to m/m'
	DECL=DECL/1000.0 !RP
    
	! Correct length of longest tributary:
	!write(*,*)'Minimum tributarie lenght set to 0.001 km'
	do iC=1,nC
		LCEL(iC)=max(LCEL(iC),0.001)
    enddo
    
    ! Excluding urh water from the minibasin if its inertial model
    if (IND_FLOODROUTING==1) then
        ! LEUSO.f90 had to be called before this
        do iC=1,nC
            IB=IBAC(IC) ! Identifying Subbasin
            PUSO2=0.0
            do IU=1,NU ! Loop URHs
                if (WM(IB,IU)<0.001) then ! if urh is water
                    if (PUSO(IC,IU)>99.999) THEN ! if minibasin is only urh water
                        do i=1,NU
                            PUSO(IC,IU)=100./DBLE(NU) ! the URHS will have equal areas at this minibasin
                        enddo
                    endif
                    PUSO2=PUSO(IC,IU)
                    PUSO(IC,IU)=0.0 ! nulling urh water
                endif
            enddo
            do iU=1,NU
                PUSO(IC,IU)=PUSO(IC,IU)+PUSO(IC,IU)*PUSO2/(100.-PUSO2) ! redistributing URH percentage
            enddo
        enddo
    endif
    
	write(*,*)''
	
	CLOSE (FILHIG)
77	FORMAT(I5,2F15.3,I5,2F10.1,2I5,F10.2,F10.6,I5,6F5.1)
	RETURN
	END
