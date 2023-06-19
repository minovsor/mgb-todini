	MODULE VARS_MAIN

!---------------------------------------------------------------------------------
! VARS_MAIN.f90
!---------------------------------------------------------------------------------
! Discussion:
!
! This module contains the main variables of MGB-IPH model
!
! Usage:
!
! USE VARS_MAIN
!
! uses modules, functions, and subroutines
!
! * no modules, functions, or subroutines are used in this module
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
! 2015.06.21 - 21 June 2015
! By: Fernando Mainardi Fan
!
! Authors:
!
! Original fortran version by Walter Collischonn
! Present fortran version by:
! * Walter Collischonn
! * Rodrigo Cauduro Dias de Paiva
! * Diogo da Costa Buarque
! * Paulo Pontes Rógenes
! * Mino Viana Sorribas
! * Fernando Mainardi Fan
! * Juan Martin Bravo
!
! Main References:
! COLLISCHONN, W. ; ALLASIA, D. G. ; SILVA, B. C. ; TUCCI, C. E. M. The MGB-IPH model for large-scale rainfall-runoff modelling. Hydrological Sciences Journal, v. 52, p. 878-895, 2007.
! COLLISCHONN, W., TUCCI, C. E. M. Simulação hidrológica de grandes bacias. Revista Brasileira de Recursos Hídricos, v. 6, n. 2, 2001.
! COLLISCHONN, W. Modelagem de Grandes Bacias - ph.d. Thesis. 2001
!
!---------------------------------------------------------------------------------
! Variables and Parameters:
! *Variables declarations and routines calls are all commented below.
!---------------------------------------------------------------------------------
! End of header
!---------------------------------------------------------------------------------
	
	!Variables declaration

	IMPLICIT NONE
	SAVE
	
	CHARACTER(8) INPUT_DIRECTORY !('.\INPUT\')
	CHARACTER(9) OUTPUT_DIRECTORY !('.\OUTPUT\')
	
    !CHARACTER(13) INPUT_DIRECTORY !('C:\MGB\INPUT\')
    !CHARACTER(14) OUTPUT_DIRECTORY !('C:\MGB\OUTPUT\')
	
	!Minibasins and muskingum-cunge routing
	INTEGER,PARAMETER:: NUP=12 !MAXIMUM NUMBER OF HRUs
	INTEGER,PARAMETER:: NBP=505 !@ (DCB 2013_01_25 NBP=300) !MAXIMUM NUMBER OF SUB-BASINS
	INTEGER,PARAMETER:: NCLIP=501 !@ (DCB 2013_01_25 NCLIP=300) !Maximum number of climatological stations
	INTEGER,PARAMETER:: NUMUSK=150 !Maximum number of stretchs for MUSKINGUM CUNGE routing
	INTEGER,PARAMETER:: NFP=3 !Number of objective functions for each gauge
	INTEGER,PARAMETER:: NTMUP=20 !Number of points in the table of non-linear muskingun Cunge
	
	!Numbers
	INTEGER NC,NU !minibasins, uses
	INTEGER NT !time intervals
	INTEGER NCLI !number of climatological stations
	INTEGER NB !number of sub-basins
	INTEGER NOBS,IQOBS(NBP) !Number of observed streamflow series, number of minibasins with observed data
	INTEGER NUMHIDG,IHIDG(NBP) !Number of hydrographs you want to save in a file, number of minibasins in which the hydrograph is desired
	INTEGER NUMSUBST,ISUBST(NBP) !Number of minibasins where streamflow calculated will be replaced by the information read from a file
	INTEGER,PARAMETER:: NMAXHID=500 !Maximum number of points taht one wants hydrographs output to be saved
    INTEGER IND_FLOODROUTING !Index of type of flood routing method.  IND_FLOODROUTING=1 is INERTIAL method; IND_FLOODROUTING=0 is Muskingum-Cunge.
    INTEGER IND_RESERV !defines if man-made reservoirs are used
    integer:: ALPHA100
    
	!Counters
	INTEGER IT !Time Interval

	!Numbers of files daily climatological data
	INTEGER,ALLOCATABLE:: NARQ(:)

	!Variables with numbers of files
	!Input Files
	INTEGER,PARAMETER:: FILFIX=1 !File of fixed parameters
	INTEGER,PARAMETER:: FILVAR=2 !File of calibrated parameters
	INTEGER,PARAMETER:: FILUSO=3 !File of parameters associated with the HRU's
	INTEGER,PARAMETER:: FILHIG=4 !File of minibasins informations
	INTEGER,PARAMETER:: FILPLU=5 !File of rainfall data
	INTEGER,PARAMETER:: FILMED=6 !File of monthly mean climatological data
	INTEGER,PARAMETER:: FILOBS=7 !File of observed streamflows
	INTEGER,PARAMETER:: FILPREV=999 !File of rain forecast data
	INTEGER,PARAMETER:: FILSUBS=10 !Hydrographs of file read to substitute the calculated ones
	INTEGER,PARAMETER:: FILCALIB=12 !File with the description of parameters for calibration

	!Output Files
	INTEGER,PARAMETER:: FILHID=21 !Hydrograph file of the sub-basins
    INTEGER,PARAMETER:: FILHID2=210 !Water Depth file of the sub-basins
	INTEGER,PARAMETER:: FILPRP=22 !Hydrograph file with proportions
	INTEGER,PARAMETER:: FILAJU=23 !Model Perfomance file
	INTEGER,PARAMETER:: FILSOL=25 !Soil file (one HRU)
	INTEGER,PARAMETER:: FILEVO=26 !EVOLUTION parameters file evolution
	INTEGER,PARAMETER:: FILBOM=27 !File containing the best parameters yet
	INTEGER,PARAMETER:: FILTUD=28 !File containing all mini-basins calculated hydrographs
    INTEGER,PARAMETER:: FILLOG=55 !File run log
    INTEGER,PARAMETER:: FILAJU2=56

!	!Calibration
	INTEGER ICALIB
	REAL,ALLOCATABLE:: R2(:),ERRV(:),R2L(:) !coef. nash, error in volumes, coef nash logarithms
	REAL VFO(NFP) !Vector with values of the objective functions

	!Variables with dimensions used
	REAL,ALLOCATABLE:: QTUDO(:,:) !Minibasins streamflow at all locations in all time steps
	REAL,ALLOCATABLE:: QR(:,:),QOBS(:,:) !Streamflow in the outlets of all sub-basins
	REAL,ALLOCATABLE:: QLIDO(:,:) !Streamflow at some point to replace the calculated values
	INTEGER,ALLOCATABLE:: KCB(:)!Nnumber of cells in each sub-basin
		
	!Streamflow in accordance with the origin for each one of the time steps
	REAL,ALLOCATABLE:: QB(:),QBI(:),QBIS(:)
	
	INTEGER IDIA,IMES,IANO,IDINI,IHORA !Date and day of the Julian 
	REAL,ALLOCATABLE:: QESP(:)!Especific baseflow (M3/S/KM2)
	
	!Albedo, Leaf Area Index, average height, and surface resistance
	REAL,ALLOCATABLE:: ALB(:,:),RIAF(:,:),Z(:,:),RS(:,:)
	
	!Number of stretchs and time intervals used in the calculation  of muskingum-cunge
	INTEGER,ALLOCATABLE:: NSUBT(:)
	REAL,ALLOCATABLE:: DT(:)
	REAL,ALLOCATABLE:: CEL(:),TIND(:)!River celerity and concentration time of the minibasin
	
	!File names of daily climatological data
	CHARACTER (20) ARQCLI(NCLIP)
	
	!File names of monthly climatological data
	CHARACTER (20) ACLIMED
	
	!File name of the streamflows observed data
	CHARACTER (20) ARQOBS
	
	!Name of the file that have the streamflows to override the flows calculated in some minibasins
	CHARACTER (20) ARQSUBST
	
	!File name to record the output hydrographs
	CHARACTER (20) ARQHID(NMAXHID)
	
	!Parameters related to the soil
	REAL,ALLOCATABLE:: WM(:,:),B(:,:),KINS(:,:),KBAS(:,:)
	REAL,ALLOCATABLE:: WMOLD(:,:),BOLD(:,:),KBOLD(:,:),KIOLD(:,:)
	REAL,ALLOCATABLE:: PLAM(:,:),CAP(:,:),WC(:,:)
	REAL,ALLOCATABLE:: CI(:),CB(:),CS(:)!Propagation parameters of the minibasin
	REAL,ALLOCATABLE:: CIOLD(:),CSOLD(:)!Copy of the parameters of propagation in the minibasin
	CHARACTER (10) AUSO(NUP)!Names of the HRUs
	
	!Parameters related to minibasins
	INTEGER,ALLOCATABLE:: IEXUT(:)!Indicates the outlet minibasins
	REAL,ALLOCATABLE:: BRIO(:)!River width
	REAL BRX !River width (auxiliary variable)
	REAL,ALLOCATABLE:: PM(:) !Average rainfall
	INTEGER,ALLOCATABLE:: IBAC(:),CELJUS(:)!Watershed, downstream minibasin
	REAL,ALLOCATABLE::  HCEL(:),LCEL(:) ! Declividade e comprimento do rio mais longo
	REAL,ALLOCATABLE:: X(:),Y(:)!Coordinates of the minibasin center
	!Area of the minibasin, drainage area in number of minibasins, river length and slope of the river
	REAL,ALLOCATABLE:: ACEL(:),ACUR(:),SRIO(:),DECL(:)
	!Proportion of HRUs in the minibasins
	REAL,ALLOCATABLE:: PUSO(:,:)
	INTEGER,ALLOCATABLE:: ICBOM(:)!Number of climatological stations in the cell
	REAL,ALLOCATABLE:: QREF(:) !Reference streamflow
	REAL QRX !Reference streamflow (auxiliary variable)
	! temp., humidity, wind, insolation, pressure, daily at each station
	REAL,ALLOCATABLE:: TD(:,:),UD(:,:),VD(:,:),SOLD(:,:),PAD(:,:) 
	REAL,ALLOCATABLE:: PMB(:)!By basin average rainfall
	INTEGER,ALLOCATABLE:: KPM(:) !Auxiliar variable
	! temp., humidity, wind, insolation, pressure, monthly values
	REAL,ALLOCATABLE:: TAMM(:,:),URMM(:,:),VVMM(:,:),PAMM(:,:)
	REAL,ALLOCATABLE:: SOLMM(:,:)
	REAL,ALLOCATABLE:: XYC(:,:)!x and y coordinates of climatological stations
    real*8,allocatable::  TWS(:) !For comparison to GRACE data
    REAL,ALLOCATABLE:: TWS2(:)
    REAL,ALLOCATABLE:: STUDO(:,:) 
    
	!Counters
	INTEGER IC
	
	!CHeaders without relevance
	CHARACTER (10) CABE(NBP)

	!More variables
	REAL,ALLOCATABLE:: SI(:,:) !Amount of water intercepted in the surface
	REAL XLADO,DH,ARX !Length of the minibasin side, difference in altitude, area (auxiliary variable)
	REAL,ALLOCATABLE:: QRG(:,:)	!Stores hydrographs where you want to record them
	REAL,ALLOCATABLE:: QRB(:,:)	!Stores hydrographs at the outlets
	REAL,ALLOCATABLE:: QM1(:),QJ1(:),QM2(:),QJ2(:) !Streamflows upstream and downstream in the cell i at times 1 and 2
	REAL,ALLOCATABLE:: QCEL1(:),QCEL2(:) !Streamflows originated in the cell at time t and t + 1 in cell i
	REAL,ALLOCATABLE:: PMB2(:),PMI2(:),PMS2(:),PJB2(:),PJI2(:),PJS2(:) !Proportions of origin of the flows in the river
	REAL,ALLOCATABLE:: VRB(:),VRI(:),VRS(:) !Volumes of proportions in the stretch
	REAL,ALLOCATABLE:: ET(:,:) 	!Total evapotraspiration
	REAL,ALLOCATABLE:: CAF(:,:)		!Capillary flow upward
	REAL,ALLOCATABLE:: W(:,:) 	!mount of of water over the soil
	REAL,ALLOCATABLE:: QBAS(:),QINT(:),QSUP(:)		!Flows in the minibasins
	REAL,ALLOCATABLE:: VBAS(:),VINT(:),VSUP(:)		!Volumes in the minibasins
	REAL,ALLOCATABLE:: TONTEM(:)		!Temperature the day before
	REAL,ALLOCATABLE:: P(:) !Rainfall at the interval in the cell
	REAL,ALLOCATABLE:: TA(:),UR(:),VV(:),SOL(:),PA(:)	!Temperature, humidity, wind, insolation, pressure
	INTEGER HORAINI !Time that the simulation starts
	INTEGER JDIA !Julian day
	CHARACTER (40) TITULO !Variable alpha  to read titles in the input files
	REAL DTP !Time interval of main calculation (in seconds given in the input file)
	REAL PEVAPO !Parameter that multiplies the evaporation
	!Variables related to radiation
	REAL GLAT !Latitude in decimal degrees (- south)
	REAL SOLX,T1,TAR2 !Insolation (hours of sun) and temperature (°c)
	REAL ALBX !ALBEDO
	REAL RLX !Resulting net radiation  (MJ/m2/dia)
	REAL RDIA !Julian day (real)
	REAL URX
	REAL,PARAMETER:: MESP=1000.0 !Water specific mass (KG/M3)
	REAL,PARAMETER:: PI=3.141592 !pi
	REAL,PARAMETER:: STEBOL=4.903E-9 !CONST. STEFAN BOLTZMANN
	!Internal variables of the subroutine
	REAL CLATE !latent heat of vaporization
	REAL SDECL,SANG !solar declination, angle of birth
	REAL HIM !maximum duration of sunshine (hours)
	REAL DR ! Relative distance Earth - Sun
	REAL GS,STO !Heat flux to the soil, Rariadion in the top of the atmosphere
	REAL SSUP,SN !Heat flux
	REAL ED,ES !Vapor pressures (real, Saturation)
	REAL SLONG !Longwave radiation

    INTEGER CRU_INDEX !Indicates whether the climatological averages come from CRU or INMET data format    
	!Variables from Subroutine CELULA
	!Auxiliaries
	REAL WX,WMX,PAX,TAX,VVX,BX,KIX,KBX,RIAFX,ZX,EX,PX,SIX,VVX2
	REAL XL,T2,RSX,CAPX,ETX,WCX,VBX,VIX,VSX,PPU,WXB,SIB,ETB,DCAP, wmmini

	!Variables from Subroutine EVAPO
	REAL D !Vapor pressure deficit
	REAL DEL !Derivative of the function ES x T
	REAL GAMA !Psychrometric constant
	REAL MSPA !Density of air
	REAL RUG !Roughness
	REAL SF !IDEM
	REAL SIL !Maxima intercepted amount of water
	REAL EIX !Amount of water evaporated from interception (POTENTIAL)
	REAL EIXP !Amount of water evaporated from the water surface  (POTENTIAL)
	REAL,ALLOCATABLE:: EVQ(:) !Direct evaporation from liquid surfaces (Water URH) from the minibasin in M3/S
	REAL REIX !Amount of water evaporated from interception  (REAL)
	REAL FDE !Fraction of available evaporative demand
	REAL,PARAMETER:: CSPA=0.001013 	!Heat especific from the humid air  MJ/kg/C
	REAL RA !Aerodynamic resistance
	REAL RAEP !Aerodynamic resistance to potential evapotranspiration
	REAL WL !Limit soil moisture above which no soil moisture restricts evapotranspiration (Shuttleworth)
	REAL WPM !Wilting point  (SHUTTLEWORTH)
	REAL F4 !Correction factor for surface resistence as a function of soil moisture deficit

	INTEGER IB,IU !Counter of Sub-basins and HRUs
	
	REAL,ALLOCATABLE:: PPREV(:,:)
	REAL,ALLOCATABLE:: QPREV(:,:)
	REAL,ALLOCATABLE:: WPREV(:,:) 	!Amount of water in the soil at the beginning of the forecasting cycle
	REAL,ALLOCATABLE:: VBASPREV(:),VINTPREV(:),VSUPPREV(:) !Minibasin volume of water at the beginning of the forecasting loop
	REAL,ALLOCATABLE:: TAPREV(:) !Temperature on the start of the loop of forecasting
	REAL,ALLOCATABLE:: QM2PREV(:),QJ2PREV(:),QJ1PREV(:) !Streamflow in the minibasins at the beginning of the forecasting loop
	REAL,ALLOCATABLE:: SIPREV(:,:) !interception storage (forecast loop)
	INTEGER ITCHUVA

	REAL,ALLOCATABLE:: QCONTORM(:,:),QCONTORJ(:,:) !Flow of boundary condition of MUSKINGUM CUNGE
	REAL,ALLOCATABLE:: QRIOINI(:,:) !Initial condition for MUSKINGUM CUNGE in the river stretch

	REAL,ALLOCATABLE:: QCONTORMPREV(:,:),QCONTORJPREV(:,:) !Flow of boundary condition for MUSKINGUM CUNGE at the beginning of the forecasting loop
	REAL,ALLOCATABLE:: QRIOINIPREV(:,:) !Initial condition for MUSKINGUM CUNGE in the river stretch at the beginning of the forecasting loop

	REAL,ALLOCATABLE:: QCEL1PREV(:),QCEL2PREV(:) !Sum of flows generated in the minibasins
	REAL,ALLOCATABLE:: PMB2PREV(:),PMI2PREV(:),PMS2PREV(:) !Proportions of origin of the flows in the river
	REAL,ALLOCATABLE:: PJB2PREV(:),PJI2PREV(:),PJS2PREV(:) !Proportions of origin of the flows in the river


	REAL,ALLOCATABLE:: BPLAN(:) !Width of the floodplain (includes own river)
	REAL,ALLOCATABLE:: HCALHA1(:),HCALHA2(:) !Depht that starts and that the floodplain is totally flooded
	REAL,ALLOCATABLE:: QMUP(:,:),AMUP(:,:),BMUP(:,:),CMUP(:,:) !Table muskingum non-linear
	INTEGER,ALLOCATABLE:: ICODMUSK(:) !Code that indicates whether linear (0) or nonlinear (1)

	integer,allocatable:: OD(:) !RP
	real,allocatable:: CalibFLAG(:,:)
	real,allocatable:: CBOLD(:),PLAMOLD(:,:),CAPOLD(:,:),WCOLD(:,:)
	integer,allocatable::	sbtFLAG(:)	! RP

	! Variables to calculate indicators of maximum evapotranspiration:
	real,allocatable:: E0agua(:),E0TOPO(:),E0SUP(:)
	INTEGER :: flagaclimed   !To indicate that we are working with CRU
	INTEGER,ALLOCATABLE:: DIAH(:),MESH(:),ANOH(:),HORAH(:) !Date and time corresponding to the time interval counter

    !Forecasting module extra variables
    !FMF 21/06/2015
    INTEGER,ALLOCATABLE:: SUBJUS(:),ATIVABACIA(:) !Sub-basin downstream from the actual one, flag to activate sub-basin
    INTEGER ISUBSTAUX(NBP) !Number of cells where Q calculated must be substituted by the ones reads from files
    INTEGER,PARAMETER:: FILTUD2=29 !forecasting module discharge results
	INTEGER,PARAMETER:: FILTUD3=30 !incremental discharges from the forecasting module
	
	!FMF 21/06/2015
	REAL,ALLOCATABLE:: QITUDO(:,:) !Incremental discharges everywhere
	REAL,ALLOCATABLE:: EVAPTUDO(:,:) !Evapotranspiration everytime and everywhere
	REAL,ALLOCATABLE:: QBASTUDO(:,:) !catchment baseflow  everytime and everywhere
	REAL,ALLOCATABLE:: WTUDO(:,:) !soil moisture volume everytime and everywhere
	
	!CONTROL VARIABLES
	INTEGER SUBini,SUBfim !Control especial cases of run
	
	!MANNING !FMF 09/09/2015 
	REAL,ALLOCATABLE:: RUGMAN(:) !Manning coefficient

	END MODULE
