	SUBROUTINE ALLOCA_VARSINERC(IOP)
	!SUBROTINA DE ALLOCA��O DE MEM�RIA DAS VARI�VEIS PRINCIPAIS
    !---------------------------------------------------------------------------------
    ! ALLOCA_VARSINERC.f90
    !---------------------------------------------------------------------------------
    ! Discussion:
    !
    ! This routine allocate or deallocate the MGB-IPH memory for the main variables
    !
    ! Usage:
    !
    ! CALL ALLOCA_VARSINERC(IOP)
    !
    ! uses modules, functions, and subroutines
    !
    ! *Module VARS_MAIN !module with the model main variables
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
    ! 2015.06.21 - 21 June 2016
    ! By: Fernando Mainardi Fan
    !
    ! Authors:
    !
    ! Original fortran version by Walter Collischonn
    ! Present fortran version by:
    ! * Walter Collischonn
    ! * Rodrigo Cauduro Dias de Paiva
    ! * Diogo da Costa Buarque
    ! * Paulo Pontes R�genes
    ! * Mino Viana Sorribas
    ! * Fernando Mainardi Fan
    ! * Juan Martin Bravo
    !
    ! Main References:
    ! COLLISCHONN, W. ; ALLASIA, D. G. ; SILVA, B. C. ; TUCCI, C. E. M. The MGB-IPH model for large-scale rainfall-runoff modelling. Hydrological Sciences Journal, v. 52, p. 878-895, 2007.
    ! COLLISCHONN, W., TUCCI, C. E. M. Simula��o hidrol�gica de grandes bacias. Revista Brasileira de Recursos H�dricos, v. 6, n. 2, 2001.
    ! COLLISCHONN, W. Modelagem de Grandes Bacias - ph.d. Thesis. 2001
    !
    !---------------------------------------------------------------------------------
    ! Variables and Parameters:
    ! *Variables declarations and routines calls are all commented below.
    !---------------------------------------------------------------------------------
    ! End of header
    !---------------------------------------------------------------------------------
    
	USE VARS_MAIN
	!use AUX_MOD !RP
	use vars_calib !RP
    USE VARS_INERC
    
	IMPLICIT NONE	
	INTEGER IOP
    SAVE

	ALLOCV_CASE: SELECT CASE (IOP) !VERIFICA SE ALLOCA OU DEALLOCA
	CASE (0) ! ALLOCA
		ALLOCATE (HDFLAG(NC))               !C�digo para rodar hidrodin�mico ou inercial
		ALLOCATE (MINIMONT(NC,10))           !MATRIZ DE LIGA��ES TOPOLOGICAS PARA PROPAGA��O MODELO INERCIAL
		ALLOCATE (NPFL(NC),ZFUNDOFL(NC))    !N�mero de pontos do arquivo Cota-Area e N�vel de Fundo da plan�cie
		ALLOCATE (ZFL(100,NC))               !COTAS PARA TABELA COTA-AREA DA PLANICIE
		ALLOCATE (AFL(100,NC))               !�REAS PARA TABELA COTA-AREA DA PLANICIE
		ALLOCATE (HRIO(NC))                 !PROFUNDAIDE DE CALHA CHEIA DO RIO
		ALLOCATE (ZTAB(102,NC),VTAB(102,NC))  !COTAS E VOLUMES PARA TABELA COTA-VOLUME
		ALLOCATE (ATAB(102,NC))
		ALLOCATE (dtfloodIC(NC))            !Intervalo de tempo em cada minibacia
		ALLOCATE (Q2fl(NC))      !Vaz�o e velocidade calculada pelo modelo inercial em cada minibacia
		ALLOCATE (Qmont(3))                 !Vaz�o a montante de uma determinada minibacia
		ALLOCATE (Vol2(NC),Vol1(NC))        !Volumes no tempo t e t+1 em uma determinada minibacia
		ALLOCATE (Area2(nc))
		ALLOCATE (Hfl(NC),Yfl(NC))          !Profundidade e N�vel de �gua em cada minibacia
		ALLOCATE (nFACECAT1(NC),nFACECAT2(NC),nFACEY1(NC),nFACEY2(NC),nFACEDX(NC),Q2face(nC),Q2viz(nC))   !VARIAVEIS DA FACE
		ALLOCATE (YRG(NUMHIDG,NT)) !HIDROGRAMAS PARA GRAVA��O
		ALLOCATE (HRG(NUMHIDG,NT)) !HIDROGRAMAS PARA GRAVA��O
		ALLOCATE (AFLRG(NUMHIDG,NT)) !HIDROGRAMAS PARA GRAVA��O
		ALLOCATE (jtab(NC))
		ALLOCATE (QRG_viz(NUMHIDG,NT)) !Hydrograph for recording
		!FMF 09/09/2015 
		ALLOCATE (nMan(NC))
        ALLOCATE(AFLTUDO(1,NT)) !TOTAL BASIN FLOODED AREA AT EACH IT
        ALLOCATE(YTUDO(NT,NC)) !WATER LEVEL AT EACH IT
        ALLOCATE(HANDTUDO(NT,NC)) !HEIGHT ABOVE NEAREST DRAINAGE
        ALLOCATE(DINFILT(NC)) ! FLOODPLAIN INFILTRATION

	CASE (1) ! DEALLOCA
		DEALLOCATE (HDFLAG)
		DEALLOCATE (MINIMONT) 
		DEALLOCATE (NPFL,ZFUNDOFL)		
		DEALLOCATE (ZFL)
		DEALLOCATE (AFL)
		DEALLOCATE (HRIO)
		DEALLOCATE (ZTAB,VTAB)
		DEALLOCATE (ATAB)
		DEALLOCATE (dtfloodIC)
		DEALLOCATE (Q2fl)
		DEALLOCATE (Qmont)
		DEALLOCATE (Vol2,Vol1)
		DEALLOCATE (Area2)
		DEALLOCATE (Hfl,Yfl)
		DEALLOCATE (nFACECAT1,nFACECAT2,nFACEY1,nFACEY2,nFACEDX,Q2face,Q2viz)
		DEALLOCATE (YRG,HRG,AFLRG)
		DEALLOCATE (AFLTUDO)
		DEALLOCATE (jtab)
        DEALLOCATE (HANDTUDO)
		
		!FMF 09/09/2015 
		DEALLOCATE (nMan)
		
	CASE DEFAULT
		STOP ' ERRO: IOP DESCONHECIDO NO ROTINA ALLOCA_CALIB!!!'
	END SELECT ALLOCV_CASE





	END