! File disusun oleh Muhammad Mahdi Ramadhan
! Mahasiswa Fisika_FMIPA Universitas Indonesia
! NPM 1506725571_guna menyelesaikan tugas Fisika Komputasi 2
! dibuat pada tanggal 23 September 2018
! Dosen = M. Aziz Majidi,Ph.D.
		program sorting
		implicit none
		integer, parameter :: N=10
		integer :: i,j,c
		character(8) :: nama(N), nama_urut(N)
		real*8 :: nilai(N), nilai_urut(N)
		
		
! membuka file data bernama "nilai.dat" dan baca data:
		open(unit=20,file="nilai.dat",status="old")
		do i=1,N
			read(20,*) nama(i), nilai(i)
		end do
		close(20)
! membuka file hasil bernama "nilai_urut.dat":
		open(unit=30,file="nama_urut.dat",status="unknown")
	
! Menulis hasil bacaan ke file bernama nama_urut.dat	
	write(30,*) "Sebelum diurutkan:"
		write(30,*)
		do i=1,N
			write(30,*) nama(i), nilai(i)
		end do	
	
			
! menulis hasil pembacaan ke layar:
		write(*,*)
		write(*,*) "Sebelum diurutkan:"
		write(*,*)
		do i=1,N
			write(*,*) nama(i), nilai(i)
		end do
		
!proses sorting nama_urut
	do  c= 65 , 122
			nama_urut(1) = achar(c)
				do	i=1,N
					if (nama(i) < nama_urut(1)) then
						nilai_urut(1)=nilai(i)
						nama_urut(1)=nama(i)
					end if
			end do
			
		
		do j=2,N
			nama_urut(j) = achar(c)
			do i=1,N
				if ((nama(i) < nama_urut(j)).and.(nama(i) > nama_urut(j-1))) then
					nilai_urut(j)=nilai(i)
					nama_urut(j)=nama(i)
				end if
			end do
		end do	
	end do
		
!menulis hasil sorting ke layar:		
		write(*,*)
		write(*,*) "Urutan nama sesuai abjad:"
		write(*,*)
		do j=1,N
			write(*,*) nama_urut(j), nilai_urut(j)
		end do
!menulis hasil sorting ke file bernama "nama_urut.dat":
		write(30,*)
		write(30,*) "mengurutkan berdasarkan abjad nama:"
		write(30,*)
		do j=1,N
			write(30,*) nama_urut(j), nilai_urut(j)
		end do


		end program sorting