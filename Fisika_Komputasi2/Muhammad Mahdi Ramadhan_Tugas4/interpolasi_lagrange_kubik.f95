!Nama			:Muhammad Mahdi Ramadhan
!NPM			:1506725571
!Mata Kuliah	:Fisika Komputasi 2
!Dosen			:Aziz Majidi  Ph.d
!Tugas			:Fisika Komputasi4
!Tanggal			:11/11/2018	
	program interpolasi_lagrange_kubik
	implicit none
	integer, parameter :: N=10,M=100
	integer :: i,j,k,c
	real*8 :: xd(N),f(N)
	real*8 :: x(M),p(M),dx,sum,prod,II(M,N)
	
!   Baca data
	do i=1,N
		read(*,*) xd(i),f(i)
	end do	
	open(unit=20,file="interpolasi_lagrange_kubik.dat",status="unknown")
!   titik-titik pengisi interpolasi
	x(1)=xd(1)
	x(M)=xd(N)
	dx=(x(M)-x(1))/real(M-1,8)
	do i=2,M-1
		x(i)=x(i-1)+dx
	end do

		
!	Perhitungan p(x)
	do k=1,M			
		if (x(k)>=xd(2) .and. x(k)<=xd(N-1) ) then !nilai data yang ada di tengah
			do c=1,N
				if (x(k)<=xd(c)) exit
			end do	
			sum=0.d0
			do i=c-2,c+1
				prod=1.d0
				do j=c-2,c+1
					if (j==i) then
						cycle !semua pekerjaan yang ada di antara perintah CYCLE dan pernyataan END DO tidak dilakukan
					else
						prod=prod*(x(k)-xd(j))/(xd(i)-xd(j))
					end if
				end do	!j
				II(k,i)=prod
				sum=sum+II(k,i)*f(i) 
			end do !i
			p(k)=sum
			
		else if (x(k)<xd(2) .or. x(k)>xd(N-1) ) then !nilai data yang ada di ujung kiri dan ujung kanan
!		Interpolasi Linier
			sum=0.d0
			do i=1,N
				prod=1.d0
				do j=1,N
					if (j==i) then
						cycle !semua pekerjaan yang ada di antara perintah CYCLE dan pernyataan END DO tidak dilakukan
					else
						prod=prod*(x(k)-xd(j))/(xd(i)-xd(j))
					end if
				end do	!j
				II(k,i)=prod
				sum=sum+II(k,i)*f(i)
			end do !i
			p(k)=sum
		end if
		write(*,*) x(k),p(k)
		write(20,*) x(k), p(k)
	end do
	
	stop
	end program