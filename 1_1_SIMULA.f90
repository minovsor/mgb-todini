    !*********************************************************************************
    !
    !  SUBROUTINE SIMULA is the main routine for hydrological simulation
	!                    (wo calibration)
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
    !    CALL SIMULA
    !
    !    where
    !
    !    * no arguments are passed in this subroutine
    !
    !    uses modules and functions
    !
	!	 * module     IFPORT  			from (visual studio 2008)
    !    * module     VARS_MAIN   		in    VARS_MAIN.f90
	!    * module     VARS_CALIB   		in    VARS_CALIB.f90
    !    * module     VARS_INERC  		in    VARSINERC.f90  !Quais? 
 	!	 * subroutine CONDINIC	  		in	  CONDINIC.f90
	!	 * subroutine MODELO	  		in	  MODELO.f90
	!	 * subroutine FOBJ	  			in	  FOBJ.f90	
	!
    !
    !	 opens
    !
    !    * Opens QPROP.PRP  	   output  ascii  file with calculated discharge by origin (i.e. surface, subsurface, gw)
	!	 * Opens VAZAO.QCL 		   output  ascii  file with calculated discharge in subbasins defined in setup file
	!	 * Opens QTUDO.QBI 		   output  binary file with calculated discharge in all catchments
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
    !   2015.06.21 - 21 June 2015 (By: Fernando Mainardi Fan)   
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

	SUBROUTINE SIMULA	
	USE VARS_MAIN
	USE VARS_CALIB
	USE VARS_INERC
    USE MUSKTODINI_VARS !MS:TODINI
	USE IFPORT

	IMPLICIT NONE
	INTEGER KB,K					!counters
	CHARACTER (10):: strIHIDG
	CHARACTER (50):: strARQUIVO	


	! Main Routines
	WRITE(FILLOG,*)'Calling initial conditions...'
	CALL CONDINIC  ! Initial Conditions
	WRITE(FILLOG,*)'Running main model routine...'
	CALL MODELO    ! Time Loop
	WRITE(FILLOG,*)'Calculating objective functions...'
	CALL FOBJ 	   ! Error/objective Functions Evaluation
   

	! Outputs
	WRITE(FILLOG,*)'Writting outputs...'
	OPEN(FILPRP,FILE=OUTPUT_DIRECTORY // 'QPROP.PRP',STATUS='UNKNOWN')
	OPEN(FILHID,FILE=OUTPUT_DIRECTORY // 'VAZAO.QCL',STATUS='UNKNOWN')
    If(hdFLAG0>1)  OPEN(FILHID2,FILE=OUTPUT_DIRECTORY // 'WATERDEPTH.QCL',STATUS='UNKNOWN')
	OPEN(FILTUD,FILE=OUTPUT_DIRECTORY // 'QTUDO.QBI',STATUS='UNKNOWN',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT')
    OPEN(19799,FILE=OUTPUT_DIRECTORY // 'YTUDO.MGB',STATUS='UNKNOWN',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT')
	OPEN(1972,FILE=OUTPUT_DIRECTORY // 'QITUDO_bin.MGB',STATUS='UNKNOWN',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT') 
	OPEN(1973,FILE=OUTPUT_DIRECTORY // 'QBASTUDO_bin.MGB',STATUS='UNKNOWN',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT') 
	OPEN(1974,FILE=OUTPUT_DIRECTORY // 'EVAPTUDO_bin.MGB',STATUS='UNKNOWN',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT')
	OPEN(1975,FILE=OUTPUT_DIRECTORY // 'WTUDO_bin.MGB',STATUS='UNKNOWN',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT')
    OPEN(1976,FILE=OUTPUT_DIRECTORY // 'HANDTUDO.MGB',STATUS='UNKNOWN',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT')
    OPEN(1977,FILE=OUTPUT_DIRECTORY // 'TWS2_bin.MGB',STATUS='UNKNOWN',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT')
    OPEN(1978,FILE=OUTPUT_DIRECTORY // 'STUDO_bin.MGB',STATUS='UNKNOWN',RECL=NC,FORM='UNFORMATTED',ACCESS='DIRECT')
    

	
	!-------------------------------------------------------------
	! Record outputs in files
    
	DO IT=1,NT
		WRITE(FILHID,71)IT,(QRG(KB,IT),KB=1,NUMHIDG) ! Record Discharge Time Series in NB predefined catchments outlets 
        If(hdFLAG0>1)    WRITE(FILHID2,71)IT,(HRG(KB,IT),KB=1,NUMHIDG) ! Record Water Depth Time Series in NB predefined catchments outlets 
		! Record Special characteristics for a defined (below) sub-basin

        WRITE(FILPRP,71)IT,QB(IT),QBI(IT),QBIS(IT)		! Discharge by Origin (i.e. surface, subs, gw) Time Series ONLY FOR Muskingum Cunge (baseflow, base+subsup, base+subsup+sup)
        
        DO IC=1,NC
            HANDTUDO(IT,IC) = YTUDO(IT,IC) - REAL(ZTAB(1,IC)) - REAL(HRIO(IC))
        END DO
        
		! Record binary file with Discharge Time Series for all catchments
		WRITE(FILTUD,REC=IT)(QTUDO(IC,IT),IC=1,NC)
        WRITE(19799,REC=IT)(YTUDO(IT,IC),IC=1,NC)
		WRITE(1972,REC=IT)(QITUDO(IC,IT),IC=1,NC)
		WRITE(1973,REC=IT)(QBASTUDO(IC,IT),IC=1,NC)
		WRITE(1974,REC=IT)(EVAPTUDO(IC,IT),IC=1,NC)
		WRITE(1975,REC=IT)(WTUDO(IC,IT),IC=1,NC)
        WRITE(1976,REC=IT)(HANDTUDO(IT,IC),IC=1,NC)
        WRITE(1977,REC=IT)(TWS2(IC),IC=1,NC) !TWS GRACE
        write(1978,REC=IT)(STUDO(IC,IT),IC=1,NC)

	ENDDO
	
	!Close file units
	CLOSE (FILHID)
    If(hdFLAG0>1)  CLOSE(FILHID2)     
    CLOSE (FILPRP)
	CLOSE (FILTUD)
    CLOSE (19799)
	CLOSE (1972)
	CLOSE (1973)
	CLOSE (1974)
	CLOSE (1975)
    CLOSE(1976)
    close(1977)
	close(1978)
    
    ! Calling SIAQUA_prepara subroutine to generate SIAQUA input files
    CALL SIAQUA_prepara
    
    
	!-------------------------------------------------------------
	! Calculates mean annual precipitation by basin (long-term)
	PMB=0.0
	KPM=0
	PM=(PM/NT)*(365.25*24.*3600./DTP)

	DO IC=1,NC
		IB=IBAC(IC)
		PMB(IB)=PMB(IB)+PM(IC)*ACEL(IC) !Pondered by the minibasin area
		KPM(IB)=KPM(IB)+ACEL(IC)
	ENDDO
	DO IB=1,NB
		PMB(IB)=PMB(IB)/KPM(IB) !Pondered by the minibasin area
		WRITE(*,*)'Sub-basin ',IB,'   Annual Rainfall',PMB(IB)
		WRITE(FILLOG,*)'Sub-basin ',IB,'   Annual Rainfall',PMB(IB)
    ENDDO
	
	!-------------------------------------------------------------
	! Record Objective-Function for each station
    WRITE(FILAJU,'(A9,3A10)')'Sub-basin','Nash','Nashlog','Evol' !FILE
	WRITE(*,'(A9,3A8)')'Sub-basin','Nash','Nashlog','Evol' !SCREEN
	DO K=1,NOBS
		WRITE(FILAJU,'(I9,3F10.3)')IQOBS(K),R2(K),R2L(K),ERRV(K) !FILE
        WRITE(*,'(I5,3F10.3)')K,R2(K),R2L(K),ERRV(K) !SCREEN
	ENDDO
	CLOSE (FILAJU)

    
	! Record Objective-Function for each station
    DO K=1,NOBS
		WRITE(FILAJU2,'(3F10.3)')R2(K),R2L(K),ERRV(K) !FILE
	ENDDO
	CLOSE (FILAJU2)

    
    ! Open files and record individual catchment discharge time series
    DO K=1,NUMHIDG
        WRITE(strIHIDG,'(I10)')IHIDG(K)
        !strARQUIVO = 'SIM_'//TRIM(adjustl((strIHIDG)))//'.TXT'
        if(hdFLAG0>1)then 
            strARQUIVO = 'SIM_INERC_'//TRIM(adjustl((strIHIDG)))//'.TXT'
        ! MS :TODINI
        elseif(hdFLAG0==0 .and. FLAG_TODINI==1)then
            strARQUIVO = 'SIM_MCT_FPI_'//TRIM(adjustl((strIHIDG)))//'.TXT'
            
        elseif(hdFLAG0==0 .and. FLAG_TODINI==2)then
            strARQUIVO = 'SIM_MCT_FPA_'//TRIM(adjustl((strIHIDG)))//'.TXT'
        !MCUNGE
        elseif(hdFLAG0==0)then
            strARQUIVO = 'SIM_MC_'//TRIM(adjustl((strIHIDG)))//'.TXT'
        endif
        !OPEN(801,FILE=OUTPUT_DIRECTORY // ''//'SIM_'//ARQHID(K),STATUS='UNKNOWN',ACTION='WRITE')
        OPEN(801,FILE=OUTPUT_DIRECTORY // ''//strARQUIVO,STATUS='UNKNOWN',ACTION='WRITE')
            !WRITE(801,*)'   DIA   MES   ANO  HORA        Q_(m3/s)'
            DO IT=1,NT
                WRITE(801,'(3I6,F16.6)')DIAH(IT),MESH(IT),ANOH(IT),QRG(K,IT)
            ENDDO
        CLOSE(801)
    ENDDO
    
    
    if(hdFLAG0>1)then 
        ! Open files and record individual catchment water level time series
        DO K=1,NUMHIDG
            WRITE(strIHIDG,'(I10)')IHIDG(K)
            strARQUIVO = 'SIM_INERC_Hfl_'//TRIM(adjustl((strIHIDG)))//'.TXT'
            OPEN(801,FILE=OUTPUT_DIRECTORY // ''//strARQUIVO,STATUS='UNKNOWN',ACTION='WRITE')
                DO IT=1,NT
                    WRITE(801,'(3I6,F16.6)')DIAH(IT),MESH(IT),ANOH(IT),HRG(K,IT)
                ENDDO
            CLOSE(801)     
        ENDDO
        
    ! MS :TODINI
    elseif(hdFLAG0==0 .and. FLAG_TODINI==1)then
        DO K=1,NUMHIDG
            WRITE(strIHIDG,'(I10)')IHIDG(K)        
            strARQUIVO = 'SIM_MCT_FPI_HFL'//TRIM(adjustl((strIHIDG)))//'.TXT'
            OPEN(801,FILE=OUTPUT_DIRECTORY // ''//strARQUIVO,STATUS='UNKNOWN',ACTION='WRITE')
                DO IT=1,NT
                    WRITE(801,'(3I6,2F16.6)')DIAH(IT),MESH(IT),ANOH(IT),HFL_TOD(IHIDG(K),IT),ZTAB_TOD(1,IHIDG(K))
                ENDDO
            CLOSE(801)
        END DO
            
    elseif(hdFLAG0==0 .and. FLAG_TODINI==2)then        
        DO K=1,NUMHIDG
            WRITE(strIHIDG,'(I10)')IHIDG(K)        
            strARQUIVO = 'SIM_MCT_FPA_HFL'//TRIM(adjustl((strIHIDG)))//'.TXT'
            OPEN(801,FILE=OUTPUT_DIRECTORY // ''//strARQUIVO,STATUS='UNKNOWN',ACTION='WRITE')
                DO IT=1,NT
                    WRITE(801,'(3I6,2F16.6)')DIAH(IT),MESH(IT),ANOH(IT),HFL_TOD(IHIDG(K),IT),ZTAB_TOD(1,IHIDG(K))
                ENDDO
            CLOSE(801)
        END DO  
    
    endif
  
    If(hdFLAG0>1)then 
    OPEN(19751,FILE=OUTPUT_DIRECTORY // 'TOTAL_FLOODED_AREAS.FLOOD',STATUS='UNKNOWN')
    DO IT=1,NT
		! Record Discharge Time Series in NB predefined catchments outlets 
		WRITE(19751,'(3I6,F16.6)')DIAH(IT),MESH(IT),ANOH(IT),AFLTUDO(1,IT) !FLOODED AREA IN KM²
    ENDDO
    CLOSE(19751)
  ENDIF
  
  
    !if(1==0)then 
    !! Open files and record individual connection discharges
    !DO K=1,NUMHIDG
    !    WRITE(strIHIDG,'(I10)')IHIDG(K)
    !    strARQUIVO = 'SIM_INERC_Connection_'//TRIM(adjustl((strIHIDG)))//'.TXT'
    !    OPEN(801,FILE=OUTPUT_DIRECTORY // ''//strARQUIVO,STATUS='UNKNOWN',ACTION='WRITE')
    !        DO IT=1,NT
    !            WRITE(801,'(3I6,F16.6)')DIAH(IT),MESH(IT),ANOH(IT),QRG_viz(K,IT)
    !        ENDDO
    !    CLOSE(801)
    !ENDDO
    !endif

	RETURN

71	FORMAT(I10,<NUMHIDG>F15.3)
72	FORMAT(<NUMHIDG>F10.3)

	END
