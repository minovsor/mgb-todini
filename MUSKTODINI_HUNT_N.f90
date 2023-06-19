	SUBROUTINE MUSKTODINI_HUNT_N(xx,x,jlo,yy,y)
	
	IMPLICIT NONE
	INTEGER, INTENT(INOUT) :: jlo
	!!REAL*8, INTENT(OUT) :: y,z
	REAL*8, INTENT(IN) :: x
	!!REAL*8, DIMENSION(n), INTENT(IN) :: xx, yy    
	INTEGER :: n,inc,jhi,jm
	real*8 incjlo
	LOGICAL :: ascnd
    ! interpolacao para varias colunas
    REAL*8, DIMENSION(:), INTENT(IN) :: xx    !TABELA COM PONTOS EM X(i)
    REAL*8, DIMENSION(:,:), INTENT(IN) :: yy  !TABELA COM PONTOS EM Y(i,1), Y(i,2), ... cada coluna uma variavel
    REAL*8, DIMENSION(:), INTENT(INOUT) :: y   !TABELA COM VALORES INTERPOLADOS DE Y(:)	
    INTEGER :: ydim(1),yydim(2),ynrow,yncol
       
    !
    n = size(xx)
    ydim = shape(y)
    yydim = shape(yy)    
    ynrow = yydim(1)
    yncol = yydim(2)
    if (n/=ynrow) print *,'verifique hunt_n'    
    if (ydim(1)/=yncol) print *,'verifique hunt_n'
        
    
	ascnd = (xx(n) >= xx(1))
	if (jlo <= 0 .or. jlo > n) then
		jlo=0
		jhi=n+1
	else
		inc=1
		if (x >= xx(jlo) .eqv. ascnd) then
			do
				jhi=jlo+inc
				if (jhi > n) then
					jhi=n+1
					exit
				else
					if (x < xx(jhi) .eqv. ascnd) exit
					jlo=jhi
					inc=inc+inc
				end if
			end do
		else
			jhi=jlo
			do
				jlo=jhi-inc
				if (jlo < 1) then
					jlo=0
					exit
				else
					if (x >= xx(jlo) .eqv. ascnd) exit
					jhi=jlo
					inc=inc+inc
				end if
			end do
		end if
	end if
	do
		if (jhi-jlo <= 1) then
			if (x == xx(n)) jlo=n-1
			if (x == xx(1)) jlo=1
			exit
		else
			jm=(jhi+jlo)/2
			if (x >= xx(jm) .eqv. ascnd) then
				jlo=jm
			else
				jhi=jm
			end if
		end if
    end do
	
	!Interpolação do vetor yy entre os valores jlo e jlo+1 -> para yy(:,1:nk)
	if(jlo<n)then
		incjlo = jlo + (1/(xx(jlo+1)-xx(jlo)))*(x-xx(jlo))
		y(:) = yy(jlo,:) + (yy(jlo+1,:)-yy(jlo,:))*(incjlo-jlo)
	else
		y(:) = yy(jlo,:) + (yy(jlo,:)-yy(jlo-5,:))/(5) * (jlo)
    endif
        

	

	
	END SUBROUTINE MUSKTODINI_HUNT_N
