! File disusun oleh Muhammad Mahdi Ramadhan
! Mahasiswa Fisika_FMIPA Universitas Indonesia
! NPM 1506725571_guna menyelesaikan tugas Fisika Komputasi 2
! dibuat pada tanggal 6 Oktober 2018
! Dosen = M. Aziz Majidi,Ph.D.
	program newraph
	implicit none
	real*8, parameter :: toleransi=1.d-5
	integer :: langkah
	real*8 :: a,x1,x2,kesrel,kesmut
	
	!input data
 		write(*,*) 'Masukkan prakiraan akar fungsi:'
 		read (*,*) a



	!prosesing newraph
	langkah=0
	do
    x1 = a -(f(a)/futur(a))

    kesrel=abs((a-x1)/x1)
    kesmut=abs(a-x1)
	
	langkah=langkah+1
    if (kesrel < toleransi .and. kesmut < toleransi) exit
	a = x1;
	end do;
    

	
	!menampilkan hasil
	write(*,*) 'Pencarian akar konvergen pada langkah ke-', langkah
	write(*,*) 'Akar    = ',x1
	write(*,*) 'f(akar) = ',f(x1)
	write(*,*) 'fungsi turunan(akar) = ',futur(x1)
	write(*,*) 'Kesalahan relatif =',kesrel
	write(*,*) 'Kesalahan mutlak=',kesmut

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

	end program newraph
