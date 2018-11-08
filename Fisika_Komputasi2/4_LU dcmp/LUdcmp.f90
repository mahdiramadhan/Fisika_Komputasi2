		program LUdcmp
		implicit none
		integer :: i,j,N,p,k
		real*8, allocatable :: A(:,:),B(:),X(:)
		real*8, allocatable :: L(:,:),U(:,:),Y(:),Ls(:),As(:)
		real*8 :: Bs, dummy
		
		character*50 :: nama_file_data
		
		nama_file_data="data_matrix.dat"
		
		! Buka file data
		open(unit=10, file=nama_file_data, status="old")
		
		! Baca data matrix
		read(10,*) N
		
		allocate(A(N,N))
		allocate(B(N))
		allocate(X(N))
		allocate(L(N,N))
		allocate(U(N,N))
		allocate(Y(N))
		allocate(Ls(N))
		allocate(As(N))
				
		read(10,*)
		
		do i=1,N
			read(10,*) (A(i,j), j=1,N)
		end do
		read(10,*) 
		do i=1,N
			read(10,*) B(i)
		end do
		
		close(10)
		
		! Tampilkan data
		write(*,*) N
		write(*,*)
		do i=1,N
			write(*,*) (A(i,j), j=1,N)
		end do
		write(*,*)
		do i=1,N
			write(*,*) B(i)
		end do
		
		! Dekomposisi A menjadi LU
		
		L(:,:)=0.d0
		U(:,:)=0.d0
		do i=1,N
			U(i,i)=1.d0
		end do
		
		do p=1,N
		
		! Kerjakan kolom-kolom L
		j=p
		
		
		do i=j,N
			if (j==1) then
				L(i,1)=A(i,1)
			else
				dummy=0.0
				do k=1,j-1
					dummy=dummy+L(i,k)*U(k,j)
				end do
				L(i,j)=A(i,j)-dummy
			end if
		end do
				
		
		! Jika ada elemen diagonal L yang berharga nol, maka tukar baris L tsb
		! dengan baris dibawahnya.
		if (L(p,p)==0.0) then

			Ls(:)=L(p,:)
			L(p,:)=L(p+1,:)
			L(p+1,:)=Ls(:)
		
			As(:)=A(p,:)
			A(p,:)=A(p+1,:)
			A(p+1,:)=As(:)
		
			Bs=B(p)
			B(p)=B(p+1)
			B(p+1)=Bs								
		
		end if	
		
		!Kerjakan baris-baris U
		i=p
		if (i==1) then
			do j=2,N
				U(i,j)=A(i,j)/L(i,i)
			end do
		else
			do j=i+1,N
				dummy=0.d0
				do k=1,i-1
					dummy=dummy-L(i,k)*U(k,j)
				end do
				U(i,j)=(A(i,j)-dummy)/L(i,i)
			end do
		end if	
			
		end do ! p
		
		! Tampilkan matrix L, U, dan B'
		
		write(*,*)
		write(*,*) "Matrix L ="
		do i=1,N
			write(*,*) (L(i,j), j=1,N)
		end do
		write(*,*)
		write(*,*) "Matrix U ="
		do i=1,N
			write(*,*) (U(i,j), j=1,N)
		end do
		write(*,*)		
		write(*,*) "Matrix B (sesudah penukaran baris) ="
		do i=1,N
			write(*,*) B(i)
		end do
		write(*,*)
		write(*,*) "Matrix A (sesudah penukaran baris) ="
		do i=1,N
			write(*,*) (A(i,j), j=1,N)
		end do
		write(*,*)	
		
		deallocate(A)
		deallocate(B)
		deallocate(X)
		deallocate(L)
		deallocate(U)
		deallocate(Y)
		deallocate(Ls)
		deallocate (As)
		
		end program