!---------------------------------------------------------------------------------
!  Discussion:
! 
!    This function is used to interpolate variables.
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

FUNCTION FINT(X,Y,N,ABC)

	!-----------------------------------------------------------------------
	! Input Variables:
	!
	! X(.) = Matrix N x 1 with x variable values in ascending order
	! Y(.) = Matrix N x 1 with y variable values
	! N = Number of points
	! ABC = x value
	! 
	! Output variables:
	!
	! FINT = Interpolated y value
	!
	! Local variables:
	!
	! I,J = Auxiliary variables
	!
	!----------------------------------------------------------------------
	
	
    !  Variables and Parameters:
	IMPLICIT NONE
	! Input varibbles:
	integer,intent(in)::N
	real*8, intent(in):: X(N),Y(N),ABC
	! Output variables:
	real*8 :: FINT
	! Local Variables:
	integer:: I, J
	!----------------------------------------------------------------------------
	
	
	DO I=2,N
		IF (ABC<X(I)) THEN
			! Inerpolation or extrapolation when X(1)>ABC
			J=I-1
			FINT=Y(J)+(Y(I)-Y(J))*(ABC-X(J))/(X(I)-X(J))
			EXIT
		ELSEIF (ABC==X(I)) THEN
			FINT=Y(I)		
			EXIT
		ELSEIF (I==N) THEN
			! Extrapolation:
			J=I-1
			FINT=Y(J)+(Y(I)-Y(J))*(ABC-X(J))/(X(I)-X(J))
		ENDIF
	ENDDO

	END FUNCTION FINT
