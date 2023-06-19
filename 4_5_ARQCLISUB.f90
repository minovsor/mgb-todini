!---------------------------------------------------------------------------------
!  Discussion:
! 
!    This sub-routine reads daily climate files.
!
!    uses modules and functions
!
!    * module VARS_MAIN in VARSMAIN.f90
!
!	 opens
!
!    * ARQCLI(KLI),  the climatic data file
!
!    reads
!
!    * ARQCLI(KLI),  the climatic data file
!
!    creates
!
!    * no files are created in this routine
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
!    2014.09.001 - 09 September 2014
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

SUBROUTINE ARQCLISUB

!  Variables and Parameters:
USE VARS_MAIN
IMPLICIT NONE
INTEGER KLI,K


!IF flagclimed==1 the model uses only climatology data.
if (flagaclimed==1) then
    TD(:,:)=-9999.0
    UD(:,:)=-9999.0
    VD(:,:)=-9999.0
    SOLD(:,:)=-9999.0
    PAD(:,:)=-9999.0
else
    !Opens daily climatic data files.
    DO KLI=1,NCLI
        NARQ(KLI)=KLI+30
        OPEN(NARQ(KLI),FILE=INPUT_DIRECTORY // ''//ARQCLI(KLI),STATUS='OLD')
        READ(NARQ(KLI),733)(CABE(K),K=1,4)
    ENDDO
    DO KLI=1,NCLI
        DO IT=1,NT
            READ(NARQ(KLI),*)TD(KLI,IT),UD(KLI,IT),SOLD(KLI,IT),VD(KLI,IT),PAD(KLI,IT) !read climatological data for each gauge at each time interval !ASF OCT2018
        ENDDO
    ENDDO
    DO KLI=1,NCLI
        NARQ(KLI)=KLI+30
        CLOSE (NARQ(KLI))
    ENDDO
endif
733	FORMAT(4A10)
734	FORMAT(5F10.2)
!__________________________________________________________
RETURN
END
