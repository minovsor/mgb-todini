
    MODULE MUSKTODINI_VARS
	! MODULE FOR TODINI'S ROUTING MODEL GLOBAL VARIABLES
    ! MINO SORRIBAS: 2023/03/28
    !
    ! TODO:
    !      
    ! - LEFIX.F90: INCLUIR 'FLAG_TODINI', MAS INCLUIR EM 'LEFIX.HIG' TBM
    ! - MAIN.F90: INCLUIR USE VARS_TODINI
    ! - MAIN.F90: INCLUIR IF (FLAG_TODINI>0) CALL FLOOD_TAB
    ! - REDE.F90: INCLUIR IF (FLAG_TODINI>0) CALL MUSK_TODINI, SEM EXECUTAR 'CALL MUSK'
    !
    ! - VERIFICAR ALGUMA INCOMPATIBILIDADE DE FLAG_TODINI COM HDFLAG
    ! - VERIFICAR SE HA VINCULO DE INICIALIZACAO DE TABELAS COM HDFLAG
    ! - VERIFICAR SE RUGMAN É SUFICIENTE PARA RUGOSIDADE
    ! - INCLUIR DEALOCACAO DE VARIAVEIS EM ALGUM LUGAR
    !
    ! - TESTAR MODELO
    ! - DUVIDA: DISCRETIZACAO ORIGINAL DO MCUNGE É SUFICIENTE?
    !
    
	IMPLICIT NONE
	SAVE

    INTEGER :: FLAG_TODINI
    !FLAG_TODINI = 1     -> PLANICIE INATIVA (QFP=0)
    !FLAG_TODINI = OUTRO -> MODO SECRETO, PLANICIE ATIVA
    
    INTEGER,ALLOCATABLE :: JTAB_TOD(:)        !PONTEIRO DA TABELA DE INTERPOLACAO
    
    REAL(8),ALLOCATABLE :: HFL_TOD(:,:)       !PROFUNDIDADE [NC,NT]
    
    REAL(8),ALLOCATABLE :: RUGMAN_TOD(:)      !RUGOSIDADE PARA PLANICIE [NC]
    
    REAL(8),ALLOCATABLE :: ZTAB_TOD(:,:)      !WATER LEVEL TABLE
    REAL(8),ALLOCATABLE :: VTAB_TOD(:,:)      !SURFACE RIVER-FLOODPLAIN STORAGE
    REAL(8),ALLOCATABLE :: ATAB_TOD(:,:)      !SURFACE AREA
    
    REAL(8),ALLOCATABLE :: ATAB1_TOD(:,:)     !MEAN TOTAL CROSS-SECTION WETTED AREA
    REAL(8),ALLOCATABLE :: ATAB2_TOD(:,:)     !MEAN CHANNEL CROSS-SECTION WETTED AREA
    REAL(8),ALLOCATABLE :: ATAB3_TOD(:,:)     !MEAN FLOODPLAIN CROSS-SECTION WETTED AREA
    
    REAL(8),ALLOCATABLE :: QTAB1_TOD(:,:)   !TOTAL FLOW
    REAL(8),ALLOCATABLE :: QTAB2_TOD(:,:)   !CHANNEL FLOW
    REAL(8),ALLOCATABLE :: QTAB3_TOD(:,:)   !FLOODPLAIN FLOW
    
    REAL(8),ALLOCATABLE :: DQDA_TOD(:,:)   !FLOODPLAIN FLOW
    

    REAL(8),ALLOCATABLE :: BTAB_VPMM(:,:)   !TOP WIDTH
    REAL(8),ALLOCATABLE :: PTAB_VPMM(:,:)   !TOTAL PERIMETER   
    REAL(8),ALLOCATABLE :: RTAB_VPMM(:,:)   !HYDRAULIC RADIUS
    REAL(8),ALLOCATABLE :: DRDYTAB_VPMM(:,:)   !dRdY   (RATE OF VARIATION OF HYDRAULIC RADIUS TO WATER DEPTH)
    REAL(8),ALLOCATABLE :: DPDYTAB_VPMM(:,:)   !dRdY   (RATE OF VARIATION OF PERIMETER TO WATER DEPTH)
    REAL(8),ALLOCATABLE :: DADYTAB_VPMM(:,:)   !dRdY   (RATE OF VARIATION OF WETAREA TO WATER DEPTH)


    
    END MODULE    