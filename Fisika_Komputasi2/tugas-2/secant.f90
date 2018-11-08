program secant

	implicit none
	real*8, parameter :: toleransi=1.d-5
	integer :: langkah
	real*8 :: a,b,c,x1,x2,kesrel


	do

 		write(*,*) 'Masukkan batas kiri, batas kanan:'
 		read (*,*) a,b

 		if (f(a)*f(b) < 0.d0) then
 			exit
		else
			write(*,*) 'Batas kiri-kanan tidak mengapit akar fungsi.'
			write(*,*) 'Coba lagi dengan nilai-nilai yang lain.'
		end if
	end do


	langkah=0

	do
		if (langkah > 3) then
			x1=(b*f(a)-a*f(b))/(f(a)-f(b))
		else
			x1=(c*f(a)-a*f(c))/(f(b)-f(c))
		end if

 		kesrel=abs((a-x1)/x1)

 		langkah=langkah+1

 		if (kesrel < toleransi) exit

		c=a
 		a=x1

	end do

	write(*,*) 'Pencarian akar konvergen pada langkah ke-', langkah
	write(*,*) 'Akar    = ',x1
	write(*,*) 'f(akar) = ',f(x1)
	write(*,*) 'Kesalahan relatif =',kesrel

	stop

	contains

	function f(x) result(y)

	implicit none
	real*8, intent(in) :: x
	real*8 :: y

	y=cos(x)-x

	return

	end function f

end program secant
