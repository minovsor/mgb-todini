
    
	SUBROUTINE MUSKVPMM_SOLVER(QJX1,QJX2,NTRECH,NTC)
    !
    ! PERUMAL'S MUSKINGUM-CUNGE ROUTING (VPMM)
    !
    ! MINO SORRIBAS: 2023/04/11
    ! 
    ! BASED ON
    ! PERUMAL M. & PRICE R. (2013)A fully mass conservative variable parameter McCarthy–Muskingum
    ! method: Theory and verification
    ! Journal of Hydrology, 502, 89-102, 2013
    !
	!	TODO: VERIFICAR - NAO LEMBRO SE TERMINEI DE IMPLEMENTAR
    

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
    
    REAL(8) :: PERIM_2,DRDY_2,DPDY_2,DADY_2,VEL_2,CEL_2,FR_2,K_2,BTOP_2,THETA2
    
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
    !-- BEGIN PERUMAL'S METHOD    
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
   
                ! FROM PREVIOUS STEP
                QHAT_OLD = QHAT
                K_1 = K_2
                
                ! 1. TRAVEL TIME (K) AND WEIGHTING (THETA)
                QREF_2 = QHAT_OLD
                CALL MUSKTODINI_HUNT(QTAB1_TOD(:,IC), QREF_2, JTAB_TOD(IC), ZTAB_TOD(:,IC), YREF_2, NPTS)   ! get level
                CALL MUSKTODINI_HUNT(ZTAB_TOD(:,IC), YREF_2, JTAB_TOD(IC), ATAB1_TOD(:,IC), WETAREA_2, NPTS) ! get wetarea                
                CALL MUSKTODINI_HUNT(ZTAB_TOD(:,IC), YREF_2, JTAB_TOD(IC), BTAB_VPMM(:,IC), BTOP_2, NPTS)   ! get top width
                CALL MUSKTODINI_HUNT(ZTAB_TOD(:,IC), YREF_2, JTAB_TOD(IC), PTAB_VPMM(:,IC), PERIM_2, NPTS) ! get perimeter
                CALL MUSKTODINI_HUNT(ZTAB_TOD(:,IC), YREF_2, JTAB_TOD(IC), DRDYTAB_VPMM(:,IC), DRDY_2, NPTS)   ! get dRdY
                CALL MUSKTODINI_HUNT(ZTAB_TOD(:,IC), YREF_2, JTAB_TOD(IC), DPDYTAB_VPMM(:,IC), DPDY_2, NPTS)   ! get dPdY
                CALL MUSKTODINI_HUNT(ZTAB_TOD(:,IC), YREF_2, JTAB_TOD(IC), DADYTAB_VPMM(:,IC), DADY_2, NPTS)   ! get dAdY
                
                !TODO: MAKE A SINGLE 'N' VARIABLE INTERPOLATION HUNT_N
                !BIGTAB(:,1) = ATAB1_1TOD(:,IC)
                !BIGTAB(:,2) = BTAB_VPMM(:,IC)
                !BIGTAB(:,3) = PTAB_VPMM(:,IC)
                !BIGTAB(:,4) = DRDYTAB_VPMM(:,IC)
                !BIGTAB(:,5) = DPDYTAB_VPMM(:,IC)
                !BIGTAB(:,6) = DPDYTAB_VPMM(:,IC)                
                !CALL MUSKTODINI_HUNT(ZTAB_TOD(:,IC), YREF_2, JTAB_TOD(IC), BIGTAB(:,:), YOUT(:)) 
                
                SO_2 = DECL(IC)
                VEL_2 = QREF_2/WETAREA_2
                                
                ! Eq. 5 and 7
                CEL_2 = 1.+(2./3.)* (PERIM_2/BTOP_2)*DRDY_2 )*VEL_2
                FR_2 = SQRT((VEL_2**2)*BTOP_2/(9.81*WETAREA_2))
                
                ! Eq. 38
                K_2 = DXX/VEL_2   
                
                ! Eq. 39
                A0_2 = QREF_2/(2.*S0_2*BTOP_2) * (1. - (4./9.)*FR_2**2 * (PERIM_2/BTOP_2*DRDY_2)**2)  
                THETA_2 = 0.5 - A0_2/(CEL_2*DXC)     
                 
                ! 2. consider values of K and theta from previous time-step (_1) for unrefined estimation
                DEN = DT + 2.*K_2*(1.-THETA_2)
                C1T = DT - 2.*K_2*THETA_2/DEN         !Q(I,T+1)
                C2T = DT + 2.*K_1*THETA_1/DEN        !Q(I,T)
                C3T = -DT + 2.*K_1*(1.-THETA_1)/DEN  !Q(I+1,T)
                 
                ! 3. the value of Q(I+1,T+1) is estimated using eq. 31
                QHAT = C1T*QC_2 + C2T*QC_1+ C3T*QD_1
                
                
                !---------------------------------------
                ! 4. The discharge at section 3 (Q3)
                ! corresponds to the normal discharge Q{0,M} at the centre of the reach
                QREF_3 = THETA_2*QC_2 + (1.-THETA_2)*QHAT       !Eq. 40
                
                ! 5.  Get Normal flow depth (y3)
                CALL MUSKTODINI_HUNT(QTAB1_TOD(:,IC), QREF_3, JTAB_TOD(IC), ZTAB_TOD(:,IC), YREF_3, NPTS)  !get y
                CALL MUSKTODINI_HUNT(ZTAB_TOD(:,IC), YREF_3, JTAB_TOD(IC), ATAB1_TOD(:,IC), WETAREA_3, NPTS) !get wet area
                VEL_3 = QREF_3/WETAREA_3
                FR_3 = SQRT((VEL_3**2)*BRIO(IC)/(9.81*WETAREA_3))
                K_3 = DXX/VEL_3 
                
                !---------------------------------------
                ! 6. Discharge at Mid-section
                QREF_M = 0.5*(QC_2 + QREF_3)                
                CALL MUSKTODINI_HUNT(QTAB1_TOD(:,IC), QREF_M, JTAB_TOD(IC), ZTAB_TOD(:,IC), YREF_M, NPTS) !get y
                CALL MUSKTODINI_HUNT(ZTAB_TOD(:,IC), YREF_M, JTAB_TOD(IC), ATAB1_TOD(:,IC), WETAREA_M, NPTS) !get wet area
                CALL MUSKTODINI_HUNT(ZTAB_TOD(:,IC), YREF_M, JTAB_TOD(IC), BTAB_VPMM(:,IC), BTOP_M, NPTS) !get top width
                CALL MUSKTODINI_HUNT(ZTAB_TOD(:,IC), YREF_M, JTAB_TOD(IC), PTAB_VPMM(:,IC), PERIM_M, NPTS) !get perimeter
                CALL MUSKTODINI_HUNT(ZTAB_TOD(:,IC), YREF_M, JTAB_TOD(IC), DRDYTAB_VPMM(:,IC), DRDY_M, NPTS)   ! get dRdY
                
                
                ! 7.Calculate Froude and 8. Celerity
                ! Eq. 5 and 7
                VEL_M = QREF_M/WETAREA_M
                K_M = DXX/VEL_M
                FR_M = SQRT((VEL_M**2)*BTOP_M/(9.81*WETAREA_M))
                CEL_M = 1.+(2./3.)* (PERIM_M/BTOP_M)*DRDY_M )*VEL_2
                
                ! 9. Weighting factor
                SO_M = DECL(IC)
                A0_M = QREF_M/(2.*S0_M*BTOP_M) * (1. - (4./9.)*FR_M**2 * (PERIM_M/BTOP_M*DRDY_M)**2)  
                THETA_M = 0.5 - A0_M/(CEL_2*DXC)    
                
                ! 10. Refined coefficients
                DEN = DT + 2.*K_2*(1.-THETA_M)
                C1T = DT - 2.*K_2*THETA_M/DEN         !Q(I,T+1)
                C2T = DT + 2.*K_1*THETA_1/DEN        !Q(I,T)
                C3T = -DT + 2.*K_1*(1.-THETA_1)/DEN  !Q(I+1,T)
                 
                ! 11. Refined discharge Q(I+1,T+1)
                QHAT = C1T*QC_2 + C2T*QC_1+ C3T*QD_1
                
                
                !--------------------------------------
                ! 12. Recalculate p3 and midpoint for refined discharge and normal flow depth
                QREF_3 = THETA_M*QC_2 + (1.-THETA_M)*QHAT       !note interpolation between upstream (qc2) and downstream (qhat)
                CALL MUSKTODINI_HUNT(QTAB1_TOD(:,IC), QREF_3, JTAB_TOD(IC), ZTAB_TOD(:,IC), YREF_3, NPTS)  !get y
                CALL MUSKTODINI_HUNT(ZTAB_TOD(:,IC), YREF_3, JTAB_TOD(IC), ATAB1_TOD(:,IC), WETAREA_3, NPTS) !get wet area                
                VEL_3 = QREF_3/WETAREA_3
                FR_3 = SQRT((VEL_3**2)*BRIO(IC)/(9.81*WETAREA_3))
                K_3 = DXX/VEL_3 
                
                ! 13. New midpoint
                QREF_M = 0.5*(QC_2 + QREF_3)
                CALL MUSKTODINI_HUNT(QTAB1_TOD(:,IC), QREF_M, JTAB_TOD(IC), ZTAB_TOD(:,IC), YREF_M, NPTS) !get y
                CALL MUSKTODINI_HUNT(ZTAB_TOD(:,IC), YREF_M, JTAB_TOD(IC), DADYTAB_VPMM(:,IC), DADY_M, NPTS)   ! get dAdY
                
                ! 14. Now estimate water level (YM_2), using continuity and celerity
                ! dQ/dx = c.(dA/dy)(dy/dx) = c*B*dy/dx
                !YREF_3 = YM_2 + (QREF_3 - QREF_M)/(BTOP_M*CEL_M)   !-> RESOLVER YM_2 COM NEWTON RAPHSON
                !YREF_3 = YM_2 + (QREF_3 - QREF_M)/(DADY_M*CEL_M)   !-> RESOLVER YM_2 COM NEWTON RAPHSON                
			    
                ! Note:
                ! (YREF_3,QREF_3) is the pair for the normal rating curve
                ! (YHAT,QHAT) is the pair for the downstream end containd the looped rating curve
                
                !Avoid negative flows
                QHAT = MAX(QREF_3,0.0)
                
                !Diference from last estimate
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
