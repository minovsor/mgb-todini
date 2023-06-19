
    
	SUBROUTINE MUSKTODINI_SOLVER(QJX1,QJX2,NTRECH,NTC)
    !
    ! TODINI'S MUSKINGUM-CUNGE ROUTING
    !
    ! MINO SORRIBAS: 2023/03/28
    ! 
    ! BASED ON
    ! TODINI R.(2007) A mass conservative and water storage consistent variable
    ! parameter Muskingum-Cunge approach
    ! Hydrol. Earth Syst. Sci., 11, 1645–1659, 2007
    !
    !
    ! - "VER VARS_TODINI.F90 PARA LISTA DE AJUSTES NECESSARIOS"
    !
    ! -> testar bacia simples: zerar tabela da planicie!
    

    ! Variables and Parameters
	USE VARS_MAIN
    USE VARS_INERC
    USE MUSKTODINI_VARS
	IMPLICIT NONE
    
    ! Old Muskingum-Cunge variables
	INTEGER ITC,ITR
	INTEGER NTRECH,NTC          !Sub-Rivers number / Number of intervals in a day
	REAL,INTENT(IN)    :: QJX1
    REAL,INTENT(INOUT) :: QJX2  !MS: DECLARADO INTENT PARA CLAREZA
    
    ! -- MS: NOVAS VARIAVEIS PARA MUSKINGUM TODINI    
    INTEGER :: NPTS    !NUMERO DE PONTOS NAS TABELAS
    INTEGER :: ITK     !ITERADOR
    INTEGER :: NKMAX   !NUMERO DE ITERACOES
    INTEGER :: JTABAUX
    REAL(8) :: FINT
    REAL(8) :: DEN,C1T,C2T,C3T
    REAL(8) :: DQ
    REAL(8) :: DXC,RDTDX, DIFAUX, RCOUR
    REAL(8) :: QI, QD_1, QC_2, QC_1
    REAL(8) :: QREF_1, QREF_2, YREF_1, YREF_2
    REAL(8) :: QHAT, QHAT_OLD, ERRQHAT    
    REAL(8) :: JTAB1, JTAB2, QDQ_1A, QDQ_1B, QDQ_2A,QDQ_2B
    REAL(8) :: WETAREA_1A, WETAREA_1B, WETAREA_2A, WETAREA_2B
    REAL(8) :: WETAREA_1, WETAREA_2
    REAL(8) :: CELTOD_1, CELTOD_2
    REAL(8) :: COURSTAR_1, COURSTAR_2, BETASTAR_1, BETASTAR_2, DIFUSTAR_1, DIFUSTAR_2
    REAL(8) :: ERR_QHAT
    
    INTEGER :: K

    ! --
	
    
    ! Begin Muskingum-Cunge
	REAL,ALLOCATABLE:: QC(:,:)
	ALLOCATE (QC(NTRECH+1,NTC+1))
	QC = 0.0 	

	!Upstream boundary conditions	
	DO ITC=1,NTC+1	  
	    !Modified to remove evapotranspiration of water surfaces
		QC(1,ITC) = MAX(QCONTORM(IC,ITC)-EVQ(IC),0.0) 
	ENDDO

	!Initial conditions
	DO ITR=1,NTRECH+1
		QC(ITR,1) = QRIOINI(IC,ITR)
    ENDDO
    
    !Downstream Boundary Condition
    !: ms: nao sei bem pq tem isso.
    QCONTORJ(IC,1) = QJX1
    
    
    !-------------------------------------
    !-- BEGIN TODINI'S METHOD    
    !-------------------------------------
    ! GET NUMBER OF TABLE POINTS FROM FLOODTABLE
    NPTS = NPFL(IC)+2
        
    ! PARAMETERS
    DQ = 0.1        ! % PERTURBATION FOR CELERITY ESTIMATION
    NKMAX = 2       ! MAX NUMBER OF ITERATIONS
    
    ! AUXILIARY
    DXC = DBLE(1000.*SRIO(IC))/NSUBT(IC)
    RDTDX = DT(IC)/DXC
    DIFAUX = DXC*BRIO(IC)*DECL(IC)
    
	DO ITC=1,NTC
		DO ITR=1,NTRECH

            ! GRID-STENCIL DISCHARGES (j:spatial, i:time)
            QD_1 = QC(ITR+1,ITC) !Q(j+1,i) # Q(t)
            QC_2 = QC(ITR,ITC+1) !Q(j,i+1) # I(t+dt)
            QC_1 = QC(ITR,ITC)   !Q(j,i)   # I(t)           
            QI = QC_2
            
            ! First guess for UNKNOWN ->> Q(j+1,i+1) #
            QHAT = QD_1 + (QC_2 - QC_1)
            
            ERRQHAT = 9999999.
            ITK = 0
            DO WHILE (ABS(ERRQHAT)>1E6)
                ITK = ITK+1
            !DO ITK = 1,2
                
                ! Last guess
                QHAT_OLD = QHAT
                
                ! Calculate reference streamflow
                QREF_1 = 0.5*(QC_1 + QD_1)
                QREF_2 = 0.5*(QHAT + QI)   !<- This is the updated unkown                
               
                ! Calculate water level (YREF_1, YREF_2)  
                CALL MUSKTODINI_HUNT(QTAB1_TOD(:,IC), QREF_1, JTAB_TOD(IC), ZTAB_TOD(:,IC), YREF_1, NPTS) 
                CALL MUSKTODINI_HUNT(QTAB1_TOD(:,IC), QREF_2, JTAB_TOD(IC), ZTAB_TOD(:,IC), YREF_2, NPTS) 
                
                
                !TODO: PODERIA TESTAR SE Y< CALHA CHEIA E APLICAR MANNING.
                
                
                !Calculate wetted area (from tables)
                ! USING HUNT -> RETURNS WETAREA_1, WETAREA_2                
                CALL MUSKTODINI_HUNT(ZTAB_TOD(:,IC), YREF_1, JTAB_TOD(IC), ATAB1_TOD(:,IC), WETAREA_1, NPTS)
                CALL MUSKTODINI_HUNT(ZTAB_TOD(:,IC), YREF_2, JTAB_TOD(IC), ATAB1_TOD(:,IC), WETAREA_2, NPTS)
                                
                
                !! Estimate c=dQ/dA numerically
                !! make a perturbation in flow and get new area
                !JTAB1 = JTAB_TOD(IC) ! NAO QUEREMOS O PONTEIRO                
                !QDQ_1A = QREF_1*(1.+DQ)
                !QDQ_1B = QREF_1*(1.-DQ)
                !CALL MUSKTODINI_HUNT(QTAB1_TOD(:,IC), QDQ_1A, JTAB1, ATAB1_TOD(:,IC), WETAREA_1A, NPTS)  
                !CALL MUSKTODINI_HUNT(QTAB1_TOD(:,IC), QDQ_1B, JTAB1, ATAB1_TOD(:,IC), WETAREA_1B, NPTS)                  
                !JTAB2 = JTAB_TOD(IC)
                !QDQ_2A = QREF_2*(1.+DQ)
                !QDQ_2B = QREF_2*(1.-DQ) 
                !CALL MUSKTODINI_HUNT(QTAB1_TOD(:,IC), QDQ_2A, JTAB2, ATAB1_TOD(:,IC), WETAREA_2A, NPTS)  
                !CALL MUSKTODINI_HUNT(QTAB1_TOD(:,IC), QDQ_2B, JTAB2, ATAB1_TOD(:,IC), WETAREA_2B, NPTS)                   
                !
                !! Celerity based on derivatives
                !CELTOD_1 = (QDQ_1A - QDQ_1B)/(WETAREA_1A-WETAREA_1B)
                !CELTOD_2 = (QDQ_2A - QDQ_2B)/(WETAREA_2A-WETAREA_2B)  
                !!--- estimativa com vazao

                ! Estimate c=dQ/dA from table     
                CALL MUSKTODINI_HUNT(ZTAB_TOD(:,IC), YREF_1, JTAB_TOD(IC), DQDA_TOD(:,IC), CELTOD_1, NPTS)  
                CALL MUSKTODINI_HUNT(ZTAB_TOD(:,IC), YREF_2, JTAB_TOD(IC), DQDA_TOD(:,IC), CELTOD_2, NPTS)                  

                 
                ! Calculate correction factor
                BETASTAR_1 = CELTOD_1*WETAREA_1/QREF_1
                BETASTAR_2 = CELTOD_2*WETAREA_2/QREF_2
                
                ! Calculate courant and difusion
                COURSTAR_1 = CELTOD_1*RDTDX/BETASTAR_1
                COURSTAR_2 = CELTOD_2*RDTDX/BETASTAR_2
                DIFUSTAR_1 = QREF_1/(BETASTAR_1*CELTOD_1*DIFAUX)
                DIFUSTAR_2 = QREF_2/(BETASTAR_2*CELTOD_2*DIFAUX)
                
                ! Calculate coefficients
                DEN = 1.+COURSTAR_2 + DIFUSTAR_2
                RCOUR = COURSTAR_2/COURSTAR_1
                C1T = (-1.+COURSTAR_2 + DIFUSTAR_2)/DEN
                C2T = ((1.+COURSTAR_1 - DIFUSTAR_1)/DEN) * RCOUR
                C3T = ((1.-COURSTAR_1 + DIFUSTAR_1)/DEN) * RCOUR
                !TODO: CORRIGIR ->DAR MAIS UMA OLHADA
                
                ! New estimate
                !QHAT = C1T*QC_1 + C2T*QC_2+ C3T*QD_1
                QHAT = C1T*QC_2 + C2T*QC_1+ C3T*QD_1
                
                ! Suaviza com estimativa anterior
                QHAT = 0.5*(QHAT+QHAT_OLD)
			
                !Avoid negative flows
                QHAT = MAX(QHAT,0.0)
                
                !Diference from last estimate
                !ERRQHAT = QHAT - QHAT_OLD
                ERRQHAT = QHAT/QHAT_OLD-1.
                IF (ITK>2) THEN
                    WRITE(*,*)IT,IC,ITK
                END IF
                
                !write(*,*)c1t,c2t,c3t                
                !write(*,*)c1t+c2t+c3t
                
                
            END DO
            
            ! Save streamflow
            QC(ITR+1,ITC+1) = QHAT
            ! Save storage            
            !SC(ITR+1,ITC+1) = 0.5*(1.-DIFUSTAR_2)*DT/COURSTAR_2)*QC_2 + 0.5*(1.+DIFUSTAR_2)*DT/COURSTAR_2)*QHAT
            ! Save mean wetted area
            !WAC(ITR+1,ITC+1) = SC(ITR+1,ITC+1)/DX
            
            !WRITE(*,*)IT,IC,QHAT
            !WRITE(*,*)JTAB_TOD(IC),QREF_1,QREF_2            
            
		ENDDO
		!Save output flows (all stretches and times)
		QCONTORJ(IC,ITC+1) = QC(NTRECH+1,ITC+1)
	ENDDO

    
    
    !-- FINISH TODINI METHOD !MS :2023/03/28
	
    !Save Initial Condition values to next Muskingum-Cunge simulation
	QRIOINI(IC,1) = QCONTORM(IC,NTC+1)
	DO ITR=2,NTRECH+1
		QRIOINI(IC,ITR)=QC(ITR,NTC+1)
	ENDDO

	!Save last downstream value. This value is returned to REDE SUBROUTINE.
	QJX2 = QC(NTRECH+1,NTC+1) 
    
    ! Calculate H = Z-ZF -> NAO PRESTA POIS Q=0 PODE TER VARIAS SOLUCOES...
    CALL MUSKTODINI_HUNT(QTAB1_TOD(:,IC), DBLE(QJX2), JTAB_TOD(IC), ZTAB_TOD(:,IC), YREF_1, NPTS) 
    HFL_TOD(IC,IT) = YREF_1 - ZTAB_TOD(1,IC)
    !WRITE(*,*)IC,IT,HFL_TOD(IC,IT),ZTAB_TOD(1,IC)
    

	DEALLOCATE (QC)
	RETURN
	END SUBROUTINE
