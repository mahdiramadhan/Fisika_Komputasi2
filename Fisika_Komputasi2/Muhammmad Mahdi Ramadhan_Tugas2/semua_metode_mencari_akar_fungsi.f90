! File disusun oleh Muhammad Mahdi Ramadhan
! Mahasiswa Fisika_FMIPA Universitas Indonesia
! NPM 1506725571_guna menyelesaikan tugas Fisika Komputasi 2
! dibuat pada tanggal 6 Oktober 2018
! Dosen = M. Aziz Majidi,Ph.D.
	program semua_metode_mencari_akar_fungsi
	implicit none
	real*8, parameter :: toleransi=1.d-5
	integer :: langkah_bisection,langkah_falsePosition,langkah_newraph,langkah_secant, langkah_secant2
	real*8 :: a,b,a1,b1,x1,x2,x3,x4,x5,x6,kesrel1,kesrel2,kesrel3,kesrel4,kesrel5,kesmut
	
	!input data
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
	! Buka file data bernama "nilai.dat" dan baca data:
		open(unit=20,file="mencari_akar_fungsi.dat",status="unknown")
		write(20,*)
		write(20,*) "mencari akar fungsi melalui semua metode"
		write(20,*)
		write(20,*) "nilai batas kiri yang dimasukkan:",a
		write(20,*) "nilai batas kanan yang dimasukkan:",b
		write(20,*)
		
	!metode bisection
	langkah_bisection=0
	do 

 		if (f(a)*f(x1) < 0.d0) then
  			b=x1
 		else
  			a=x1
 		end if 

 		x2=(a+b)*0.5d0

 		kesrel1=abs((x1-x2)/x2)

 		langkah_bisection=langkah_bisection+1
 		
 		if (kesrel1 < toleransi) exit

 		x1=x2
	end do
	!metode false position
	langkah_falsePosition=0
	
	do 
	
		x3=(a*f(b)-b*f(a))/(f(b)-f(a))

 		kesrel2=abs((b-x3)/x3)

 		langkah_falsePosition=langkah_falsePosition+1
 		
 		if (kesrel2 < toleransi) exit

 		b=x3

	end do
	
	!metode Newraph
	langkah_newraph=0
	do
		x4 = a -(f(a)/futur(a))

		kesrel3=abs((a-x4)/x4)
		kesmut=abs(a-x4)
		
		langkah_newraph=langkah_newraph+1
		if (kesrel3 < toleransi .and. kesmut < toleransi) exit
		a = x4;
	end do;
    
	!metode Secant
	langkah_secant=0
	
	do 
		if (langkah_secant > 3) then
		x5=(b*f(a)-a*f(b))/(f(a)-f(b))
		else
			x1=(c*f(a)-a*f(c))/(f(b)-f(c))
		end if
		kesrel4=abs((b-x5)/x5)
		langkah_secant=langkah_secant+1
   		if (kesrel4 < toleransi) exit
		c=a
		a=x5
	end do
    
	!metode Secant2
	langkah_secant2=0
	
	do 
		a1=(b*f(a)-a*f(b))/(f(a)-f(b))
		b1=(b*f(a)-a*f(b))/(f(a)-f(b))
		x6=(a1*f(b1)-b1*f(a1))/(f(b1)-f(a1))
		kesrel4=abs((b-x6)/x6)
		langkah_secant=langkah_secant+1
   		if (kesrel5 < toleransi) exit
		b=x6
	end do
    


	write(20,*) 'Hasil Data Bisection'
	write(20,*) 'Pencarian akar konvergen pada langkah ke-', langkah_bisection
	write(20,*) 'Akar    = ',x2
	write(20,*) 'f(akar) = ',f(x2)
	write(20,*) 'Kesalahan relatif =',kesrel1
	write(20,*)
	write(20,*) 'Hasil Data False Position'
	write(20,*) 'Pencarian akar konvergen pada langkah ke-', langkah_falsePosition
	write(20,*) 'Akar    = ',x3
	write(20,*) 'f(akar) = ',f(x3) 
	write(20,*) 'Kesalahan relatif =',kesrel2
	write(20,*)
	write(20,*) 'Hasil Data Newraph'
	write(20,*) 'Pencarian akar konvergen pada langkah ke-', langkah_newraph
	write(20,*) 'Akar    = ',x4
	write(20,*) 'f(akar) = ',f(x4) 
	write(20,*) 'Kesalahan relatif =',kesrel3
	write(20,*) 'Kesalahan mutlak=',kesmut
	write(20,*)
	write(20,*) 'Hasil Data Secant'
	write(20,*) 'Pencarian akar konvergen pada langkah ke-', langkah_secant
	write(20,*) 'Akar    = ',x5
	write(20,*) 'f(akar) = ',f(x5) 
	write(20,*) 'Kesalahan relatif =',kesrel4
	write(20,*)
	write(20,*) 'Hasil Data Secant2'
	write(20,*) 'Pencarian akar konvergen pada langkah ke-', langkah_secant2
	write(20,*) 'Akar    = ',x6
	write(20,*) 'f(akar) = ',f(x6) 
	write(20,*) 'Kesalahan relatif =',kesrel5
	stop

	contains

	function f(x) result(y)

	implicit none
	real*8, intent(in) :: x
	real*8 :: y

	y=cos(x)-x

	return

	end function f
	function futur(x) result(y)

	implicit none
	real*8, intent(in) :: x
	real*8 :: y

	y=-sin(x)-1

	return

	end function futur
	end program semua_metode_mencari_akar_fungsi