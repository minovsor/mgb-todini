	!---------------------------------------------------------------------------------
    !  Discussion:
    ! 
    !    This subroutine calculates the flow for each catchment from Inertial equation   (Inertial version).
    !
    !
    ! Usage:
    !
    ! *
    !
    ! uses modules, functions, and subroutines
    !
    ! * USE VARS_MAIN
    ! * USE VARS_INERC (only to Inertial version)
    !
    ! opens
    !
    ! * no files are created in this routine
    !
    ! reads
    !
    ! * no files are created in this routine
    !
    ! creates
    !
    ! * no files are created in this routine
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
    ! 2015.07.06 - 07 July 2015 (by Paulo Pontes)
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

	subroutine flood_discharge

	!--------------------------------------------------------------------------------------
	! Variables and parameters:
	use VARS_INERC
	use VARS_MAIN
	implicit none
    
    INTEGER :: THREAD_NUM = 0
	!-------------------------------------------------------------------------------------
    
    

    do iC=1,nC
        
        IB=IBAC(IC) 
		!IF((IB<SUBini).OR.(IB>SUBfim))CYCLE ! Manual controls for especialized calibration

            ! Bottom level and water level of IC catchment:
            z1=ZTAB(1,iC)
            y1=Hfl(iC)+z1

            ! Bottom level and water level of downstream IC catchment:
            iCJus = CELJUS(iC)
            
            if(iCJus == -1)then
                z2=z1-DECL(IC)*SRIO(iC)*1000 ! same riverbed slope as the previous catchment
                z2=min(z1,z2)
                y2=Dble(Hfl(iC))+z2                
            else
                z2=ZTAB(1,iCJus)
                y2=Hfl(iCJus)+z2
            endif
            
            ! Calculates the hflow variable:
            hflow=max(y2,y1)-max(z2,z1)
            hflow=max(hflow,0.0)
            
           
            if(iCJus /= -1)then
                dxflow=DBLE(SRIO(IC)*1000.) + DBLE(SRIO(iCJus)*1000.)       
                dxflow=dxflow/2.
            else
                dxflow=DBLE(SRIO(IC)*1000.)
            endif

            !River width
            bflow=DBLE(BRIO(iC))
                      
            !River manning coefficient
            xMan=nMan(iC)
            
            ! Flow in the last time-step
            q0=Q2fl(iC)/bflow ! in m2/s
                    
            ! Water table slope:
            Sflow=-(y1-y2)/dxflow
            
            ! Calculates flow from Inertial equation (m2/s) for each IC:
            if (hflow>0.0) then
                q=(q0-(g*dtflood*hflow*Sflow))/(1.+g*dtflood*hflow*xMan*xMan*abs(q0)/(hflow**(10.0/3.0)))
                q=q*bflow !(m3/s)
            else
                q=0.0
            endif
            
            ! Updates variable flow inertial:
            Q2fl(iC)=q ! in m3/s
            QJ2(iC)=Q2fl(iC) ! in m3/s
            
    enddo
 
    
        !Loop que calcula a vazão nas interconexões entre minibacias.
    if (1==0) then !liga conexões
        CALL flood_pseudo2D
    endif

	endsubroutine
