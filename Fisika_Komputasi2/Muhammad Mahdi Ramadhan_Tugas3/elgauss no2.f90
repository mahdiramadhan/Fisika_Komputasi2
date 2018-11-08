		!Nama			:Muhammad Mahdi Ramadhan
		!NPM			:1506725571]
		!Departemen		:Fisika
		!Mata Kuliah	:Fisika Komputasi 2
		!Dosen			:Dr Aziz Mazidi
		!Tanggal		:21/10/2018
		
		program Elgaus
		implicit none
		integer :: i,j,N,M,k,r
		real*8, allocatable :: A(:,:),B(:,:),X(:,:)
		real*8, allocatable :: As(:),Bs(:), dummy0
		character*50 :: nama_file_data
		
		nama_file_data="data_matrix_no3.dat"
		
		! Buka file data
		open(unit=10, file=nama_file_data, status="old")
		
		! Baca data matrix
		read(10,*) N, M
		
		allocate(A(N,N))
		allocate(B(N,M))
		allocate(X(N,M))
		allocate(As(N))
		allocate(Bs(N))
	
				
		read(10,*)
		
		do i=1,N
			read(10,*) (A(i,j), j=1,N)
		end do
		read(10,*) 
		do i=1,N
			read(10,*) (B(i,j), j=1,M)
		end do
		
		close(10)
		
		! Tampilkan data
		write(*,*) N, M
		write(*,*)
		do i=1,N
			write(*,*) (A(i,j), j=1,N)
		end do
		write(*,*)
		do i=1,N
			write(*,*) (B(i,j), j=1,M)
		end do
		
		
		! Triangulasi matriks A menjadi matriks triangular
		do k=1,N-1
			do i=k+1,N
				dummy0=A(i,k)/A(k,k)
				do j=k,N
					A(i,j)=A(i,j)-dummy0*A(k,j)
				end do
				do r=1,M
					B(i,r)=B(i,r)-dummy0*B(k,r)
				end do
			end do
			! Jika ada elemen diagonal A yang berharga nol, maka tukar baris A tsb dengan baris dibawahnya.
			if (A(k+1,k+1)==0.0) then
				As(:)=A(k+1,:)
				A(k+1,:)=A(k+2,:)
				A(k+2,:)=As(:)
				Bs(:)=B(k+1,:)
				B(k+1,:)=B(k+2,:)
				B(k+2,:)=Bs(:)								
			end if	
		end do
		
		! Tampilkan matrix A
		write(*,*)
		write(*,*) "Metode Eliminasi gauss hanya mentriangularisasi matriks A"
		write(*,*)
		write(*,*) "Matrix A setelah dilakukan triangularisasi ="
		do i=1,N
			write(*,*) (A(i,j), j=1,N)
		end do
		
		
		! Mencari matrix X dengan substitusi mundur
		X(N,:)=B(N,:)/A(N,N)
		do j=1,N-1
			do r=1,M
				dummy0=0.d0
				do k=N-j+1,N
					dummy0=dummy0+A(N-j,k)*X(k,r)
				end do
				X(N-j,r)=(B(N-j,r)-dummy0)/A(N-j,N-j)
			end do
		end do
		
		! Tampilkan matrix X
		write(*,*)
		write(*,*) "Matrix X:"
		do i=1,N
			write(*,*) (X(i,j), j=1,M)
		end do
		write(*,*)		
		
				
		deallocate(A)
		deallocate(B)
		deallocate(X)
		deallocate(As)
		deallocate(Bs)
		end program Elgaus