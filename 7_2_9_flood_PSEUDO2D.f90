    !*********************************************************************************
    !
    !  SUBROUTINE flood_pseudo2D is an optional routine for inertial simulation
	!                    
    !
    !---------------------------------------------------------------------------------
    !  Discussion:
    ! 
    !    This subroutine calculates lateral connections from the Discharge routine   (Inertial version).
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

    subroutine flood_pseudo2D

	!--------------------------------------------------------------------------------------
	! Variables and parameters:
	use VARS_INERC
	use VARS_MAIN
	implicit none
    INTEGER K,KHID 					!indexes and counters
    
    Q2viz=0.0
    
!    !$OMP PARALLEL DO NUM_THREADS(4) DEFAULT (SHARED)
!    !PRIVATE ()
    do iFACE=1,nFACE
    
            KCAT=nFACECAT1(iFACE)
            KCAT2=nFACECAT2(iFACE)
            
            ! Nível de Fundo e Nível da Água da minibacia iC:
            z1=ZTAB(1,KCAT)
            y1=Hfl(KCAT)+z1
            z2=ZTAB(1,KCAT2)
            y2=Hfl(KCAT2)+z2

            ! Cálculo da profundidade de escoamento:
            hflow=max(y2,y1)-max(z2,z1)

            !Correção de valores negativos
            hflow=max(hflow,0.0)
            
            !A rotina DBLE transforma a variável de entrada em um real*8
            !Média dos dx de IC e ICJUS
            dxflow=DBLE(nFACEDX(iFACE))       !Verificar se precisa de um limitador do dx
            bflow=100.0
            !WIDTH FOR ESPECIFIC CONNECTIONS (E.G. RIVER DEFLUENCES)
            xMan=nMan(iFACE)
           
            ! Vazão no tempo anterior:
            q0=Q2face(iFACE)/bflow ! em m2/s
                    
            ! Declividade da linha de água:
            Sflow=-(y1-y2)/dxflow
            
                
            ! Cálculo da vazão Inercial (por unidade de largura do rio) na face de jusante da minibacia iC:
            if (hflow>0.0) then
                q=(q0-(g*dtflood*hflow*Sflow))/(1.+g*dtflood*hflow*xMan*xMan*abs(q0)/(hflow**(10.0/3.0)))
                q=q*bflow
            else
                q=0.0;
            endif
            
            ! Calcula a nova vazão no próximo intervalo de tempo:
            Q2face(iFACE)=q ! em m3/s
            Q2viz(KCAT)=Q2viz(KCAT)- Q2face(iFACE)
            Q2viz(KCAT2)=Q2viz(KCAT2)+ Q2face(iFACE)
                        
            DO K=1,NUMHIDG 				! store discharge in catchments defined in PARHIG.HIG
				KHID=IHIDG(K) 			! indexed catchment
				IF(KHID==KCAT) THEN
                     QRG_viz(K,IT)=Q2viz(KCAT)  
                ENDIF
            ENDDO          
           
    enddo 
!    !$OMP END PARALLEL DO
    end
 