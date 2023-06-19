	MODULE VARS_INERC
	!DECLARA��O DE VARI�VEIS RELACIONADAS AO MODELO DE PROPAGA��O INERCIAL
	IMPLICIT NONE
	SAVE

    REAL*8,PARAMETER:: g=9.81                 
    REAL*8,PARAMETER:: dxart=5.               !dx artificial caso o comprimento do rio de uma minibacia seja muito pequeno Ver flood_timestep
    
    REAL*8:: ALPHA
    REAL*8,ALLOCATABLE:: nMan(:)            !Rugosidade considerada no modelo Inercial !FMF 09/09/2015
    REAL*8,ALLOCATABLE:: DINFILT(:)
    INTEGER,ALLOCATABLE:: HDFLAG(:)             !C�digo do modelo hidrodin�mico ou inercial
    INTEGER hdFLAG0                             !Flag do modelo hidrin�mico ou inercial
    INTEGER,ALLOCATABLE:: MINIMONT(:,:)         !MATRIZ COM RELA��ES TOPOL�GICAS DE CADA MINI-BACIA
    INTEGER,ALLOCATABLE:: NPFL(:)               !NUMERO DE PONTOS DA TABELA COTA-AREA EM CADA MINI-BACIA
    INTEGER,ALLOCATABLE:: ZFUNDOFL(:)           !COTA DO FUNDO DA PLANICIE DA TABELA COTA-�REA
    INTEGER,ALLOCATABLE:: ZFL(:,:)              !COTA DA PLANICIE PARA TABELA COTA-AREA
    REAL*8,ALLOCATABLE:: AFL(:,:)                 !�REA DA PLANICIE PARA TABELA COTA-AREA
    REAL*8,ALLOCATABLE:: HRIO(:)                  !PROFUNDIDADE DE CALHA CHEIA DO RIO
    REAL*8 HRX                                    !PROFUNDIDADE DE CALHA CHEIA DO RIO (VARI�VEL AUXILIAR) 
    REAL*8,ALLOCATABLE:: ZTAB(:,:)                !COTA PARA TABELA COTA-VOLUME DE CADA MINI-BACIA
    REAL*8,ALLOCATABLE:: VTAB(:,:)                !VOLUME PARA TABELA COTA-VOLUME DE CADA MINI-BACIA
    REAL*8,ALLOCATABLE:: ATAB(:,:)
    REAL*8 dtfloodmax,dtflood,dtflood0,tflood     !Vari�veis relacionadas ao intervalo de tempo do modelo inercial
    REAL*8,ALLOCATABLE:: dtfloodIC(:)
    REAL*8 hmaxfl                                 !Vari�vel que recebe a profundidade m�xima do vetor Hfl
    REAL*8,ALLOCATABLE:: Q2fl(:)            !Vaz�o e velocidade calculada pelo modelo inercial em cada minibacia
    REAL*8,ALLOCATABLE:: Qmont(:),Vol2(:),Vol1(:) !Vaz�o a montante e Volumes no tempo t e t+1 em uma determinada minibacia
    REAL*8:: SumQup
    REAL*8 :: DINFILTX
    REAL*8,ALLOCATABLE:: Area2(:)
    REAL*8,ALLOCATABLE:: Hfl(:),Yfl(:)            !Profundidade e N�vel de �gua em cada minibacia
    REAL*8 nfroude                                !Numero de Froude para testes de regime supercritico
    REAL,ALLOCATABLE:: YRG(:,:)	 !ARMAZENA HIDROGRAMAS ONDE SE DESEJA GRAVAR
    REAL,ALLOCATABLE:: HRG(:,:)	 !ARMAZENA HIDROGRAMAS ONDE SE DESEJA GRAVAR
    REAL,ALLOCATABLE:: AFLRG(:,:)!ARMAZENA HIDROGRAMAS ONDE SE DESEJA GRAVAR
    REAL,ALLOCATABLE:: AFLTUDO(:,:) !TOTAL FLOODED AREA FOR THE BASIN
    REAL,ALLOCATABLE:: YTUDO(:,:) !TOTAL FLOODED AREA FOR THE BASIN
    REAL,ALLOCATABLE:: HANDTUDO(:,:) !HAND VALUES FOR THE BASIN    
    
    ! Discharge subroutine variables
    real*8 z1,y1,z2,y2,Sflow,hflow
	real*8 dxflow,bflow,q0,q, xMan
	integer iCJus
    
    ! Continuity subroutine variables
    integer Nentradas
    real*8 y2_fl 
    
    INTEGER :: nFACE,iFACE,KCAT,KCAT2                             !Number of points on the FACE table (pesudo2D)
    REAL*8,ALLOCATABLE:: nFACECAT1(:),nFACECAT2(:),Q2face(:),nFACEY1(:),nFACEY2(:),nFACEDX(:),Q2viz(:)                     !Vaz�o nas faces
    integer,allocatable:: jtab(:)
    REAL,ALLOCATABLE:: QRG_viz(:,:)	!Stores connection flows where you want to record them
    
	END MODULE
