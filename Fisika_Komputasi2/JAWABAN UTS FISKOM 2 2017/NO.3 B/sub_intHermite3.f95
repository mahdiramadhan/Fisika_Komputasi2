	subroutine interpolasi_hermite3(N_,xd_,fd_,xmin_, xmax_,M_)
	implicit none
	integer, intent(in) :: N_,M_
	real*8 :: xd_(N_),fd_(N_),xmin_,xmax_
	integer :: i,j,k,N,M,c
	real*8, allocatable :: xd(:), fd(:)
	real*8 :: xmin, xmax, dx, p, x,x0,x1,x2,x3,fx0,fx1,fx2,fx3,f_1,f_2,p1,p2
	real*8, external :: f_,h1,h2
	
	N = N_
	M = M_
	allocate(xd(N))
	allocate(fd(N))
	xd = xd_
	fd = fd_
	xmin = xmin_
	xmax = xmax_



open(unit=12, file="hasil_intHermite3.txt", status="replace", action="write")
write(12,*) "# "
write(12,*)	"# =======Estimasi Titik Data======="
write(12,*) "# Batas awal :", xmin
write(12,*) "# Batas akhir :", xmax
write(12,*) "# Jumlah titik :", M
write(12,*) "# x			f(x)"

	dx=(xmax-xmin)/ real(M, 8)
	
	
	do k=0,M
		x = xmin + real(k, 8)*dx	
		do c=1,N
			if (x<=xd(c)) exit
		end do
		if (c<2 .OR. c>N) cycle
		! Interval jepit antara c-1 dan c
		
		if (c==2) then
			x1	=	xd(c-1)
			x2	=	xd(c)
			x3	=	xd(c+1)

			fx1	=	fd(c-1)
			fx2	=	fd(c)
			fx3	=	fd(c+1)
			
			f_1 = f_(x1,x1,x2,x3,fx1,fx2,fx3) 
			f_2 = f_(x2,x1,x2,x3,fx1,fx2,fx3)
		
		else if (c==N) then
			x0	=	xd(c-2)
			x1	=	xd(c-1)
			x2	=	xd(c)

			fx0	=	fd(c-2)
			fx1	=	fd(c-1)
			fx2	=	fd(c)
			
			f_1 = f_(x1,x0,x1,x2,fx0,fx1,fx2) 
			f_2 = f_(x2,x0,x1,x2,fx0,fx1,fx2)
			
		else
			x0	=	xd(c-2)
			x1	=	xd(c-1)
			x2	=	xd(c)
			x3	=	xd(c+1)
			fx0	=	fd(c-2)
			fx1	=	fd(c-1)
			fx2	=	fd(c)
			fx3	=	fd(c+1)
			
			f_1 = f_(x1,x0,x1,x2,fx0,fx1,fx2) 
			f_2 = f_(x2,x1,x2,x3,fx1,fx2,fx3)
		end if

		
 		p1 = h1(x,x1,x2)*fx1 + h2(x,x1,x2)*f_1
		p2 = h1(x,x2,x1)*fx2 + h2(x,x2,x1)*f_2
	
		p = p1+p2
		
		
		write(12,*) x,"			", p
	end do
	
	write(*,*)
	write(*,*) "~~Proses Selesai~~"
	write(*,*) "Silakan cek titik data pada : hasil_intHermite3.txt"

	close(12)
	deallocate(xd)
	deallocate(fd)
	return
	end subroutine  
	
	real*8 function f_(x_,x_0,x_1,x_2,fx_0,fx_1,fx_2) result (z)
		real*8 :: x_0,x_1,x_2,fx_0,fx_1,fx_2,x_
			
		z = ((2*x_-x_1-x_2)/((x_0-x_1)*(x_0-x_2)))*fx_0+ &
       ((2*x_-x_0-x_2)/( (x_1-x_0)*(x_1-x_2)))*fx_1+ &
       ((2*x_-x_0-x_1)/((x_2-x_0)*(x_2-x_1)))*fx_2;
		
		
		return
	end function f_
	
	real*8 function h1(x_,x_1,x_2) result (z)
		real*8 :: x_,x_1,x_2
		
		z= (1-(2*((x_-x_1)/(x_1-x_2))))*(((x_-x_2)/(x_1-x_2))**2)
		
		return
	end function h1
	
	real*8 function h2(x_,x_1,x_2) result (z)
		real*8 :: x_,x_1,x_2
		
		z= (x_-x_1)*(((x_-x_2)/(x_1-x_2))**2)
		
		return
	end function h2