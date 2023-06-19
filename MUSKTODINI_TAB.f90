    SUBROUTINE MUSKTODINI_TAB
    
    ! INITIALIZE TABLES FOR TODINI'S MUSKIUNGUM-CUNGE ROUTING
    ! MINO SORRIBAS: 2023/03/28
    ! 
    ! ASSUME QUE TABELAS DO "INERCIAL MODEL" FORAM CARREGADAS.
    !
    ! NA PRATICA, DAS TABELAS GERADAS AQUI, SO UTILIZA EM MUSKTODINI: "QTAB1_TOD" E "ATAB1_TOD"
    ! MAS AS DEMAIS PODEM SER UTEIS PARA OUTROS OUTPUTS-VERIFICACOES
    !
    !
    ! A ROTINA ESTA FEITA COMO SE TODOS OS CALCULOS TIVESSE A AREA TOTAL
    
    

    !Variables and parameters
    USE VARS_MAIN
    USE VARS_INERC
    USE MUSKTODINI_VARS
    IMPLICIT NONE
    
        
    !MS: LOCAL VARIABLES    
    REAL :: VOLINCR
    REAL(8) :: DXX, DZ, DYY 
    REAL(8) :: KCHN, KFLP, WETAREA, PERIM, RAIOH, BFLP
    REAL(8) :: WETAREA_RIO, PERIM_RIO, PERIM_FLP
    INTEGER :: K
    
    ! ALOCA VARIAVEIS
    CALL MUSKTODINI_ALLOCA(0)
    
    ! INICIAIZA PONTO DE BUSCAO PARA INTERPOLACOES "HUNT"
    JTAB_TOD(:) = 1
    
    ! INICIALIZA TABELAS ZERADAS
    ZTAB_TOD(:,:) = 0.
    ATAB_TOD(:,:) = 0.
    VTAB_TOD(:,:) = 0.
    ATAB1_TOD(:,:) = 0.
    ATAB2_TOD(:,:) = 0.
    ATAB3_TOD(:,:) = 0.
    QTAB1_TOD(:,:) = 0.
    QTAB2_TOD(:,:) = 0.
    QTAB3_TOD(:,:) = 0.
    DQDA_TOD(:,:) = 0.
    
    ! TABELAS PARA METODO VPMM
    PTAB_VPMM(:,:) = 0.    
    RTAB_VPMM(:,:) = 0.
    DRDYTAB_VPMM(:,:) = 0.
    DADYTAB_VPMM(:,:) = 0.
    DPDYTAB_VPMM(:,:) = 0.
    
    
    !H_TOD !TUDO
    HFL_TOD(:,:) = 0.
    VOLINCR=0D0
    
    DO IC=1,NC
        
        ! LENGTH IN METERS
        DXX = DBLE(1000.*SRIO(IC))    
        
        ! A) 1ST THREE POINTS
        !------------------------------------------
        ! AT BOTTOM
        ATAB_TOD(1,IC) = DBLE(BRIO(IC))*DBLE(SRIO(IC))/1000.0  !SURFACE AREA -> m*(1km/1000m)*km=km2
        ZTAB_TOD(1,IC) = ZFUNDOFL(IC)-HRIO(IC)         
        VTAB_TOD(1,IC) = 0.0
        !XS WET AREA
        ATAB1_TOD(1,IC) = 0.0  !TOTAL
        ATAB2_TOD(1,IC) = 0.0  !CHANNEL:ACTIVE
        ATAB3_TOD(1,IC) = 0.0  !FLOODPL:INACTIVE
        
      
        
        !------------------------------------------
        ! LEVEL AT BOTTOM OF FLOOPLAIN -> OVERBANK LEVEL
        ATAB_TOD(2,IC) = ATAB_TOD(1,IC)
        ZTAB_TOD(2,IC) = ZFUNDOFL(IC) 
        VTAB_TOD(2,IC) = DBLE(BRIO(IC))*DBLE(HRIO(IC))*DXX   ![m3] assuming retangular channel
        !XS WET AREA
        ATAB1_TOD(2,IC) = VTAB_TOD(2,IC)/DXX    !TOTAL
        ATAB2_TOD(2,IC) = VTAB_TOD(2,IC)/DXX    !CHANNEL:ACTIVE
        ATAB3_TOD(2,IC) = 0.0                   !FLOODPL:INACTIVE
     
        
        !------------------------------------------
        ! LEVEL AT 1ST VERTICAL ABOVE OVERBANK
        ATAB_TOD(3,IC) = MAX(ATAB_TOD(2,IC),AFL(1,IC))     !MS:compara projecao de calha retangular [km2] com area minima da planicie?!
        ZTAB_TOD(3,IC) = ZFL(1,IC) 
        VTAB_TOD(3,IC) = VTAB_TOD(2,IC)+0.5*(ATAB_TOD(3,IC)+ATAB_TOD(2,IC))*(ZTAB_TOD(3,IC)-ZTAB_TOD(2,IC))*1000000.0      

        ! XS WET AREA
        DZ = ZTAB_TOD(3,IC) - ZFUNDOFL(IC)
        ATAB1_TOD(3,IC) =  VTAB_TOD(3,IC)/DXX                    !TOTAL
        ATAB2_TOD(3,IC) =  ATAB2_TOD(2,IC) + DZ*BRIO(IC)         !CHANNEL:XS WET AREA BELOW + DZ*B  (assuming retangular)
        ATAB3_TOD(3,IC) =  ATAB1_TOD(3,IC) - ATAB2_TOD(3,IC)     !FLOODPL:[TOTAL - CHANNEL ]
        
        
        !------------------------------------------
        ! B) OTHER LEVELS
        DO K =1,NPFL(IC)-1
            !!ATAB_TOD(K+3,IC) = MAX(AFL(K+1,IC),ATAB_TOD(3,IC)) 
            ATAB_TOD(K+3,IC) = MAX(AFL(K+1,IC),ATAB_TOD(K+2,IC)) !MS:
            ZTAB_TOD(K+3,IC) = ZFL(K+1,IC)                      
            VOLINCR = (ZFL(K+1,IC)-ZFL(K,IC))*1000000.0*((ATAB_TOD(K+2,IC)+ATAB_TOD(K+3,IC))*0.5)
            VTAB_TOD(K+3,IC) = VTAB_TOD(K+2,IC)+VOLINCR
            
            ! XS WET AREA
            DZ = ZTAB_TOD(K+3,IC) - ZFUNDOFL(IC)   !MS: DESCONFIO DISSO AQUI, POIS WETAREA É INCREMENTAL
            DZ = ZTAB_TOD(K+3,IC) - ZTAB_TOD(K+2,IC) !MS: CORRECAO->PROVAVEL
            ATAB1_TOD(K+3,IC) =  VTAB_TOD(K+3,IC)/DXX                 !TOTAL MEAN WET AREA
            ATAB2_TOD(K+3,IC) =  ATAB2_TOD(K+2,IC)+DZ*BRIO(IC)*1.     !CHANNEL:XS WET AREA BELOW + DZ*B (assuming retangular)
            ATAB3_TOD(K+3,IC) =  ATAB1_TOD(K+3,IC)-ATAB2_TOD(K+3,IC)  !FLOODPL: [TOTAL - CHANNEL]
            
            VOLINCR = 0D0

        !IF (IC==398) THEN
        !WRITE(*,*)ZFUNDOFL(IC)
        !WRITE(*,*)BRIO(IC)*1000.
        !WRITE(*,*)DBLE(BRIO(IC))*1000.
        !WRITE(*,*)ATAB_TOD(1:4,IC)
        !WRITE(*,*)ZTAB_TOD(1:4,IC)
        !WRITE(*,*)VTAB_TOD(1:4,IC)
        !PAUSE
        !END IF

        ENDDO
        
    ENDDO 

    
    
    !---------------------------------
    !  DISCHARGE TABLES
    !---------------------------------
    RUGMAN_TOD(:) = 0.08
    DO IC=1,NC

        KCHN = SQRT(DECL(IC))/RUGMAN(IC)
        KFLP = SQRT(DECL(IC))/RUGMAN_TOD(IC)
        
        PERIM_RIO = DBLE(BRIO(IC)) + 2.*DBLE(HRIO(IC))
        WETAREA_RIO = DBLE(BRIO(IC))*DBLE(HRIO(IC))
        
        ! A) 1ST 3 POINTS
        !------------------------------------------
        ! BOTTOM
        QTAB2_TOD(1,IC) = 0.   !CHANNEL
        QTAB3_TOD(1,IC) = 0.   !FLOODPLAIN
        QTAB1_TOD(1,IC) = 0.   !TOTAL
        
        ! wet perimeter and top width for VPMM method
        PTAB_VPMM(1,IC) = 0.   
        BTAB_VPMM(1,IC) = DBLE(BRIO(IC))
        
        !------------------------------------------
        ! LEVEL AT FLOODPLAIN BOTTOM -> AT OVERBANK
        PERIM   = PERIM_RIO
        WETAREA = WETAREA_RIO
        RAIOH = WETAREA/PERIM
        QTAB2_TOD(2,IC) = WETAREA*RAIOH**(2./3.)*KCHN  !CHANNEL
        QTAB3_TOD(2,IC) = 0.                           !FLOODP
        QTAB1_TOD(2,IC) = QTAB2_TOD(2,IC)              !TOTAL
        
        ! wet perimeter and top width for VPMM method
        PTAB_VPMM(2,IC) = PERIM_RIO
        BTAB_VPMM(2,IC) = DBLE(BRIO(IC))
        
        
        !------------------------------------------
        ! LEVEL AT 1ST VERTICAL ABOVE OVERBANK
        DZ = ZTAB_TOD(3,IC)-ZFUNDOFL(IC)  ! water depth above overbank level
        
        !-- CHANNEL ( + virtual box over main channel, above overbank level)
        PERIM   = PERIM_RIO + 2.*DZ 
        WETAREA = WETAREA_RIO + DBLE(BRIO(IC))*DZ
        RAIOH = WETAREA/PERIM 
        QTAB2_TOD(3,IC) = WETAREA*RAIOH**(2./3.)*KCHN
        
        !-- FLOODPLAIN
        !BFLP = ATAB3_TOD(3,IC)/DZ/2                        ! two-sided floodplain width = (A/dz)/2
        !BFLP = MAX( 0.0, (ATAB3_TOD(3,IC) - WETAREA )/DZ)   ! AJUSTE: DESCONTANDO AREA DA CALHA -> VERSAO C PAULO/WALTER
        BFLP = MAX( 0.0, (ATAB1_TOD(3,IC) - WETAREA )/DZ)   ! MS: 11/ABR/2023 -> LARGURA PLANICIE = [TOTAL - RIO]
        PERIM  = BFLP + 2.*DZ
        WETAREA = BFLP*DZ
        RAIOH = WETAREA/PERIM 
        QTAB3_TOD(3,IC) = WETAREA*RAIOH**(2./3.)*KFLP
        
        IF (FLAG_TODINI==1) THEN !NO FLOW
            QTAB3_TOD(3,IC) = 0.
        END IF
        
        !--TOTAL
        QTAB1_TOD(3,IC) = QTAB2_TOD(3,IC) + QTAB3_TOD(3,IC)
        
        ! wet perimeter and top width for VPMM method
        PTAB_VPMM(3,IC) = PERIM_RIO + PERIM                
        BTAB_VPMM(3,IC) = DBLE(BRIO(IC)) + BFLP  !!SOMA BFLP
        
        
        !------------------------------------------
        ! B) OTHER LEVELS
        DO K =1,NPFL(IC)-1
            DZ = ZTAB_TOD(K+3,IC)-ZFUNDOFL(IC) !water depth above overbank

            !-- CHANNEL ( + virtual above overbank box)
            PERIM   = PERIM_RIO + 2.*DZ
            WETAREA = WETAREA_RIO + DBLE(BRIO(IC))*DZ
            RAIOH = WETAREA/PERIM 
            QTAB2_TOD(K+3,IC) = WETAREA*RAIOH**(2./3.)*KCHN
            
            !-- FLOODPL
            !BFLP = MAX(0.0, (ATAB3_TOD(K+3,IC) - WETAREA )/DZ )
            BFLP = MAX(0.0, (ATAB1_TOD(K+3,IC) - WETAREA )/DZ ) ! MS: 11/ABR/2023 -> LARGURA PLANICIE = [TOTAL - RIO]
            PERIM  = BFLP + 2.*DZ
            WETAREA = BFLP*DZ
            RAIOH = WETAREA/PERIM 
            QTAB3_TOD(K+3,IC) = WETAREA*RAIOH**(2./3.)*KFLP
            
            IF (FLAG_TODINI==1) THEN !NO FLOW
                QTAB3_TOD(K+3,IC) = 0.
            END IF            
            
            !--TOTAL
            QTAB1_TOD(K+3,IC) = QTAB2_TOD(K+3,IC) + QTAB3_TOD(K+3,IC)
            
            ! wet perimeter and top width for VPMM method
            PTAB_VPMM(3,IC) = PERIM_RIO + PERIM
            BTAB_VPMM(3,IC) = DBLE(BRIO(IC)) + BFLP  !!SOMA BFLP

        ENDDO     
        
    
    END DO
    
   
    
    
    ! Calculates DQDA
    DO IC=1,NC
        ! Forward-derivative
        DO K=1,102-1
            DQDA_TOD(K,IC) = (QTAB1_TOD(K+1,IC)-QTAB1_TOD(K,IC))/(ATAB1_TOD(K+1,IC)-ATAB1_TOD(K,IC))
        END DO
        DQDA_TOD(102,IC) = DQDA_TOD(101,IC)            
        ! Smooth by Average
        DQDA_TOD(:,IC) = 0.5*( DQDA_TOD(1:101,IC)+DQDA_TOD(2:102,IC) )
        
        
    END DO
    
    DO IC=1,NC
        DO K=1,102
            IF (FLAG_TODINI==1) THEN
            WRITE(331,'(2I16,12F16.3)') IC,K,ZTAB_TOD(K,IC),ATAB_TOD(K,IC),VTAB_TOD(K,IC), &
                ATAB1_TOD(K,IC),ATAB2_TOD(K,IC),ATAB3_TOD(K,IC), &
                QTAB1_TOD(K,IC),QTAB2_TOD(K,IC),QTAB3_TOD(K,IC), &
                DQDA_TOD(K,IC)
            END IF
            
            IF (FLAG_TODINI==2) THEN
            WRITE(332,'(2I16,12F16.3)') IC,K,ZTAB_TOD(K,IC),ATAB_TOD(K,IC),VTAB_TOD(K,IC), &
                ATAB1_TOD(K,IC),ATAB2_TOD(K,IC),ATAB3_TOD(K,IC), &
                QTAB1_TOD(K,IC),QTAB2_TOD(K,IC),QTAB3_TOD(K,IC), &
                DQDA_TOD(K,IC)
            END IF
        END DO
    END DO
    
    
    ! Additional tables for Perumal's method
    DO IC=1,NC
        RTAB_PER(:,IC) = ATAB1_TOD(:,IC)/PTAB_PER(:,IC) !Hydraulic Radius
        ! Forward-Derivatives
        DO K=1,102-1
            DYY = ZTAB_TOD(K+1,IC) - ZTAB_TOD(K,IC)
            DRDYTAB_VPMM(K,IC) = (RTAB_VPMM(K+1,IC)-RTAB_VPMM(K,IC))/DYY            
            DPDYTAB_VPMM(K,IC) = (PTAB_VPMM(K+1,IC)-PTAB_VPMM(K,IC))/DYY
            DADYTAB_VPMM(K,IC) = (ATAB1_TOD(K+1,IC)-ATAB1_TOD(K,IC))/DYY
        END DO
        ! Last point
        DRDYTAB_VPMM(102,IC) = DRDYTAB_VPMM(101,IC)
        DPDYTAB_VPMM(102,IC) = DPDYTAB_VPMM(101,IC)
        DADYTAB_VPMM(102,IC) = DADYTAB_VPMM(101,IC)
        ! Smooth by Average
        DRDYTAB_VPMM(:,IC) = 0.5*(DRDYTAB_VPMM(1:101,IC)+DRDYTAB_VPMM(2:102,IC)
        DPDYTAB_VPMM(:,IC) = 0.5*(DPDYTAB_VPMM(1:101,IC)+DPDYTAB_VPMM(2:102,IC)
        DADYTAB_VPMM(:,IC) = 0.5*(DADYTAB_VPMM(1:101,IC)+DADYTAB_VPMM(2:102,IC)
    END DO    
    !todo: BTAP_VPMM TOP WIDTH

       
    RETURN
    END SUBROUTINE
