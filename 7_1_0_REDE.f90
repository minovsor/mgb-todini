	!---------------------------------------------------------------------------------
    !  Discussion:
    ! 
    !    This Sub-Routine calculates the flow routing in drainage network .
    !
    !
    ! Usage:
    !
    ! *CALL MUSK(QJX1,QJX2,NTRECH,NTC,C1,C2,C3)
    !
    ! uses modules, functions, and subroutines
    !
    ! * USE VARS_MAIN
    ! * USE VARS_INERC (only to Inertial version)
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
    ! 2015.06.21 - 21 June 2015 (by Fernando Mainardi Fan)
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
    
	SUBROUTINE REDE
	!This Sub-Routine calculates the flow routing in drainage network .
	
	! Variables and Parameters:
	USE VARS_MAIN
	USE VARS_INERC
	
	IMPLICIT NONE
	REAL DX		                                                       !Space-step
	REAL XXX,AK		                                                   !X E K: Muskingum-Cunge parameters
	REAL QJX1,QJX2,DTCAL,DEN,VT 	                       !Auxiliary variables
	INTEGER JUS                                                        !Auxiliary variables
	INTEGER NTRECH,NTC                                                 !Sub-Rivers number / Number of intervals in a day
	REAL C1,C2,C3 	                                                   !Muskingum-Cunge coefficients
	
	
	!PROPORÇÕES DE ORIGEM
	REAL PMB1(NC+1),PMI1(NC+1),PMS1(NC+1)
	REAL PJB1(NC),PJI1(NC),PJS1(NC)
	
	!Counters
	INTEGER NTMUSK,LT
	INTEGER I,IS 

	IF(DT(NC)<0.00001)STOP 'Something wrong in time-step on rede.f90'
	NTMUSK=DTP/DT(NC)

    !Initial conditions: condinic.f90

	!In each time-step variables i+1 are changed to i
	DO IC=1,NC
		QM1(IC)=QM2(IC)
		QJ1(IC)=QJ2(IC)
		QM2(IC)=0.0
		QJ2(IC)=0.0
		PMB1(IC)=PMB2(IC)
		PMI1(IC)=PMI2(IC)
		PMS1(IC)=PMS2(IC)
		PJB1(IC)=PJB2(IC)
		PJI1(IC)=PJI2(IC)
		PJS1(IC)=PJS2(IC)
		QCEL1(IC)=QCEL2(IC)
	ENDDO
	QCONTORM=0.0
	QCONTORJ=0.0

	DO IC=1,NC		
		
		
		if (sbtFLAG(iC)==1) cycle ! RP:  Does not compute upstream catchments of replacement catchments

		IB=IBAC(IC)
		
		!code used to calibraton
		!IF((IB<SUBini).OR.(IB>SUBfim))CYCLE ! Manual controls for especialized calibration	

		QCEL2(IC)=QBAS(IC)+QINT(IC)+QSUP(IC) !Total of flow in each catchment

        !Catchment with river
		IF(NSUBT(IC).GT.0)THEN 
				
			DTCAL=DT(IC)
			DX=SRIO(IC)*1000./NSUBT(IC)
			AK=DX/CEL(IC)
			XXX=0.5*(1.0-(QREF(IC)/(BRIO(IC)*DECL(IC)*CEL(IC)*DX)))

			! Verification of X and K values:
			! Testa condição 0.2<=X<=0.5:
			if (XXX<0.2) then
				XXX=0.2
				AK=DTCAL/(3.125*XXX**1.25) !Eq 4.111 DO LIVRO DO TUCCI MODELOS
			elseif(XXX<0.4) then
!				AK=DTCAL/(3.125*XXX**1.25) !Eq 4.111 DO LIVRO DO TUCCI MODELOS
			elseif (XXX<0.5) then

			else
				XXX=0.5
				AK=DTCAL
			endif	
		
		
			DEN=2.*AK*(1.-XXX)+DTCAL			
			C1=(2.*AK*XXX+DTCAL)/DEN
			C2=(DTCAL-2.*AK*XXX)/DEN
			C3=(2.*AK*(1.-XXX)-DTCAL)/DEN

            !Lateral inflow are used upstream of same catchment

			!Proportions to Flows
			PMB2(IC)=PMB2(IC)*QM2(IC) !underground flow
			PMI2(IC)=PMI2(IC)*QM2(IC) !subsurface flow
			PMS2(IC)=PMS2(IC)*QM2(IC) !runoff
			
			QM2(IC)=QM2(IC)+QSUP(IC)+QINT(IC)+QBAS(IC)


			!MUSKINGUM-CUNGE Boundary conditions
			DO LT=1,NTMUSK+1
				QCONTORM(IC,LT)=QCONTORM(IC,LT)+QCEL1(IC)+(LT-1)*(QCEL2(IC)-QCEL1(IC))/NTMUSK
			ENDDO

			!Update proportions
			PMB2(IC)=(PMB2(IC)+QBAS(IC))/QM2(IC)
			PMI2(IC)=(PMI2(IC)+QINT(IC))/QM2(IC)
			PMS2(IC)=(PMS2(IC)+QSUP(IC))/QM2(IC)

			!Update volume proportions
			VRB(IC)=VRB(IC)+PMB2(IC)*QM2(IC)*DTP
			VRI(IC)=VRI(IC)+PMI2(IC)*QM2(IC)*DTP
			VRS(IC)=VRS(IC)+PMS2(IC)*QM2(IC)*DTP

            
			QJX1=QJ1(IC)
			NTRECH=NSUBT(IC)
			NTC=DTP/DT(IC)			            

		
			CALL MUSK(QJX1,QJX2,NTRECH,NTC,C1,C2,C3)
			QJ2(IC)=QJX2

	
			!Update volume proportions
			VT=VRB(IC)+VRI(IC)+VRS(IC)
			PJB2(IC)=VRB(IC)/VT
			PJI2(IC)=VRI(IC)/VT
			PJS2(IC)=VRS(IC)/VT
			VRB(IC)=MAX(0.0,VRB(IC)-PJB2(IC)*QJ2(IC)*DTP)
			VRI(IC)=MAX(0.0,VRI(IC)-PJI2(IC)*QJ2(IC)*DTP)
			VRS(IC)=MAX(0.0,VRS(IC)-PJS2(IC)*QJ2(IC)*DTP)

			!Flows routing in each catcment are used as upstream boundary in downstream catchment
			JUS=CELJUS(IC)
			
			! IF catchment is a outlet:
			if (JUS>0) then

				PMB2(JUS)=PMB2(JUS)*QM2(JUS) !underground flow
				PMI2(JUS)=PMI2(JUS)*QM2(JUS) !subsurface flow
				PMS2(JUS)=PMS2(JUS)*QM2(JUS) !runoff

				QM2(JUS)=QM2(JUS)+QJ2(IC)


				!Muskingum-Cunge boundary condition
				!Tests if calculated flow must be replaced by flow by file
				IS=0
				DO I=1,NUMSUBST 
					IF(IT>1.AND.IC==ISUBST(I))THEN 			
						IS=I
						EXIT
					ELSE
						IS=0
					ENDIF
				ENDDO

				IF(IS>0)THEN
					IF(QLIDO(IS,IT)<0.0)then !Problem in read data
						QM2(JUS)=QM2(JUS) !Uses calculated flow
						DO LT=1,NTMUSK+1                                      
							QCONTORM(JUS,LT)=QCONTORM(JUS,LT)+QCONTORJ(IC,LT) !update boundary 
							!WRITE(*,*)IC,LT,QCONTORJ(IC,LT),QCONTORM(JUS,LT)
						ENDDO
					ELSE !use flow from file
						QM2(JUS)=QLIDO(IS,IT)
						DO LT=1,NTMUSK+1 !Replaces calculated flow by read flow (from file)
							QCONTORM(JUS,LT)=QLIDO(IS,IT-1)+(LT-1)*(QLIDO(IS,IT)-QLIDO(IS,IT-1))/NTMUSK
						ENDDO
					ENDIF
				ELSE
					DO LT=1,NTMUSK+1
						QCONTORM(JUS,LT)=QCONTORM(JUS,LT)+QCONTORJ(IC,LT) !Update boundary
					ENDDO
				ENDIF

				!Update proportions
				PMB2(JUS)=(PMB2(JUS)+PJB2(IC)*QJ2(IC))/QM2(JUS)  
				PMI2(JUS)=(PMI2(JUS)+PJI2(IC)*QJ2(IC))/QM2(JUS)
				PMS2(JUS)=(PMS2(JUS)+PJS2(IC)*QJ2(IC))/QM2(JUS)
			endif	

        !headboard catchment
		ELSE 
			DX=0.0
			AK=0.0
			XXX=0.0

			QJ2(IC)=QBAS(IC)+QINT(IC)+QSUP(IC)
			PJB2(IC)=QBAS(IC)/QJ2(IC)
			PJI2(IC)=QINT(IC)/QJ2(IC)
			PJS2(IC)=QSUP(IC)/QJ2(IC)

            !Flows routing in each catcment are used as upstream boundary in downstream catchment
			JUS=CELJUS(IC)
			
			!IF catchment is a outlet
			if (JUS>0) then

				PMB2(JUS)=PMB2(JUS)*QM2(JUS) !underground flow
				PMI2(JUS)=PMI2(JUS)*QM2(JUS) !subsrface flow
				PMS2(JUS)=PMS2(JUS)*QM2(JUS) !runoff

				QM2(JUS)=QM2(JUS)+QSUP(IC)+QINT(IC)+QBAS(IC)
				
				!MUSKINGUM-CUNGE boundary condition
				DO LT=1,NTMUSK+1
					QCONTORM(JUS,LT)=QCONTORM(JUS,LT)+(QJ1(IC)+(LT-1)*(QJ2(IC)-QJ1(IC))/NTMUSK)
				ENDDO
				
				!update proportions
				PMB2(JUS)=(PMB2(JUS)+QBAS(IC))/QM2(JUS)  
				PMI2(JUS)=(PMI2(JUS)+QINT(IC))/QM2(JUS)
				PMS2(JUS)=(PMS2(JUS)+QSUP(IC))/QM2(JUS)
			endif
		ENDIF
		
		!STORE OUTPUT INFORMATION !FMF and PETER 10/08/2016
		QTUDO(IC,IT)=QJ2(IC)

	        ! MS: UPDATE TWS
	        !IF (IND_FLOODROUTING==0.AND.OD(IC)>1) THEN          
		    !DO ITR=1,NTRECH
			!TWS2(IT,IC) = TWS2(IT,IC) + (AK*XXX*QRIOINI(IC,ITR) + AK*(1.-XXX)*QRIOINI(IC,ITR+1))*86400./(ACEL(IC)*1000)
		    !END DO
	        !END IF  

	ENDDO
	RETURN
	END
