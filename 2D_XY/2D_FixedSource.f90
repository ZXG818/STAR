!  XYGeo.f90 
!
!  FUNCTIONS:
!  XYGeo - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: XYGeo
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

  module DATA_STRUCTURE
  implicit none
  private i, N
  integer :: i
  integer, parameter :: SN = 4
  integer, parameter :: N = SN*(SN+2)/2
  integer, parameter :: SPECIFIED_BOUND = 1
  integer, parameter :: REFLECTIVE_BOUND = 2
  ! angular type, with quadrature set
  type :: angular_type
    real(8) :: mu
    real(8) :: eta
    real(8) :: omega
  endtype
  ! mesh type
  type :: mesh_type
    real(8) :: flux             ! angular flux
    real(8) :: x                
    real(8) :: y                
    real(8) :: sigma_t          ! total macro-xs
    real(8) :: sigma_s11        ! scatter macro-xs 
    real(8) :: sigma_s12
    real(8) :: sigma_s21
    real(8) :: sigma_s22
    real(8) :: sigma_f          ! fission macro-xs
    real(8) :: scatter_source   ! scattering source term
    real(8) :: external_source  ! external source term
    real(8) :: fission_source   ! fission source term
    real(8) :: total_source     ! total source term
  endtype
  ! group constant type
  type :: group_xs
    real(8) :: sigma_t
    real(8) :: sigma_s11
    real(8) :: sigma_s12
    real(8) :: sigma_s21
    real(8) :: sigma_s22
    real(8) :: sigma_f
    real(8) :: external_source
  endtype
  ! initialize the angular parameter
  type(angular_type), dimension(N) :: angular
  ! 1. mu<0, eta<0
  data angular(1)%mu  / -0.90444905  /
  data angular(1)%eta / -0.30163878  / 
  data angular(2)%mu  / -0.30163878  /
  data angular(2)%eta / -0.90444905  /
  data angular(3)%mu  / -0.30163878  / 
  data angular(3)%eta / -0.30163878  / 
  ! 2. mu>0, eta<0
  data angular(4)%mu  /  0.90444905  /
  data angular(4)%eta / -0.30163878  /
  data angular(5)%mu  /  0.30163878  /
  data angular(5)%eta / -0.90444905  / 
  data angular(6)%mu  /  0.30163878  /
  data angular(6)%eta / -0.30163878  /
  ! 3. mu<0, eta>0
  data angular(7)%mu  / -0.30163878  /
  data angular(7)%eta /  0.30163878  /
  data angular(8)%mu  / -0.30163878  /
  data angular(8)%eta /  0.90444905  /
  data angular(9)%mu  / -0.90444905  /
  data angular(9)%eta /  0.30163878  /
  ! 4. mu>0, eta>0
  data angular(10)%mu /  0.30163878  /
  data angular(10)%eta/  0.30163878  /
  data angular(11)%mu /  0.30163878  / 
  data angular(11)%eta/  0.90444905  /
  data angular(12)%mu /  0.90444905  /
  data angular(12)%eta/  0.30163878  /
  ! omega
  data(angular(i)%omega, i=1, N) / N*0.333333333333333d0 /     
  endmodule
  
  ! mesh module
  module MESHER
  use DATA_STRUCTURE
  implicit none
  contains
  ! generate the grid
  subroutine Generate_grid(mesh_group, len_x, len_y)
  implicit none
  type(mesh_type),dimension(:,:,:,:),intent(inout)::mesh_group
  real(8), intent(in) :: len_x
  real(8), intent(in) :: len_y
  integer :: x_cnt
  integer :: y_cnt
  integer :: angular_cnt
  integer :: energy_cnt
  integer :: i, j, k, g
  real(8) :: dx, dy
  energy_cnt = size(mesh_group, 1)
  angular_cnt = size(mesh_group, 2)
  x_cnt = size(mesh_group, 3)
  y_cnt = size(mesh_group, 4)
  dx = len_x / (x_cnt-1)
  dy = len_y / (y_cnt-1)
  do g=1, energy_cnt
    do k=1, angular_cnt
      do j=1, y_cnt
        do i=1, x_cnt
          mesh_group(g,k,i,j)%x = (i-1)*dx
          mesh_group(g,k,i,j)%y = (j-1)*dy
          mesh_group(g,k,i,j)%flux = 0.d0
          mesh_group(g,k,i,j)%sigma_t = 0.d0
          mesh_group(g,k,i,j)%sigma_s11 = 0.d0
          mesh_group(g,k,i,j)%sigma_s12 = 0.d0
          mesh_group(g,k,i,j)%sigma_s21 = 0.d0
          mesh_group(g,k,i,j)%sigma_s22 = 0.d0
          mesh_group(g,k,i,j)%sigma_f = 0.d0
          mesh_group(g,k,i,j)%scatter_source = 0.d0
          mesh_group(g,k,i,j)%external_source = 0.d0
          mesh_group(g,k,i,j)%fission_source = 0.d0
          mesh_group(g,k,i,j)%total_source = 0.d0
        enddo
      enddo
    enddo
  enddo
  return 
  endsubroutine
  endmodule
  
  ! solver module
  module SOLVER
  use DATA_STRUCTURE
  use MESHER
  implicit none
  ! set the maxxium iteration
  integer, parameter :: MAX_ITER = 1E3
  ! set the convergence standard
  real(8), parameter :: errno = 1.d-9
  contains
  ! initialize the field
  subroutine Initialize(mesh_group, value)
  implicit none
  type(mesh_type), dimension(:,:,:,:),intent(inout)::mesh_group
  real(8),intent(in) :: value
  integer :: i, j, k, g
  integer :: x_cnt, y_cnt, angular_cnt, energy_cnt
  energy_cnt = size(mesh_group, 1)
  angular_cnt = size(mesh_group, 2)
  x_cnt = size(mesh_group,3)
  y_cnt = size(mesh_group,4)
  do g=1, energy_cnt
    do k=1, angular_cnt
      do j=1, y_cnt
        do i=1, x_cnt
          mesh_group(g,k,i,j)%flux = value
        enddo
      enddo
    enddo
  enddo
  return 
  endsubroutine
  ! initialize the macro-xs
  subroutine MacroXS(mesh_group)
  implicit none
  type(mesh_type),dimension(:,:,:,:),intent(inout) :: mesh_group
  integer :: i,j,k,g
  integer :: x_cnt, y_cnt, angular_cnt, energy_cnt
  energy_cnt = size(mesh_group, 1)
  angular_cnt = size(mesh_group, 2)
  x_cnt = size(mesh_group, 3)
  y_cnt = size(mesh_group, 4)
  do g=1, energy_cnt
    do k=1, angular_cnt
      do j=1, y_cnt
        do i=1, x_cnt    
          if((mod(i,2).eq.0) .and. (mod(j,2).eq.0)) then
            if(g .eq. 1) then 
              mesh_group(g,k,i,j)%sigma_t = 1.0       ! 1st group, sigma_t
              mesh_group(g,k,i,j)%sigma_s11 = 0.3     ! 1st group, sigma_s11
              mesh_group(g,k,i,j)%sigma_s12 = 0.2     ! 1st group, sigma_s12
              mesh_group(g,k,i,j)%sigma_s21 = 0.d0    ! non-used
              mesh_group(g,k,i,j)%sigma_s22 = 0.d0    ! non-used
              mesh_group(g,k,i,j)%sigma_f = 0.d0
              if((mesh_group(g,k,i,j)%x.LT.2.d0) .and. (mesh_group(g,k,i,j)%y.LT.2.d0))then
                mesh_group(g,k,i,j)%external_source = 1.0 ! 1st group, external source
              else
                mesh_group(g,k,i,j)%external_source = 0.d0
              endif
            endif
            if(g .eq. 2) then
              mesh_group(g,k,i,j)%sigma_t = 1.0       ! sigma_t
              mesh_group(g,k,i,j)%sigma_s11 = 0.d0    ! non-used
              mesh_group(g,k,i,j)%sigma_s12 = 0.d0    ! non-used
              mesh_group(g,k,i,j)%sigma_s21 = 0.d0    ! 2nd group, sigma_s21
              mesh_group(g,k,i,j)%sigma_s22 = 0.4     ! 2nd group, sigma_s22
              if((mesh_group(g,k,i,j)%x.GT.8.d0) .and. (mesh_group(g,k,i,j)%y.GT.8.d0))then
                mesh_group(g,k,i,j)%external_source = 1.0 ! 2nd group, external_source
              else
                mesh_group(g,k,i,j)%external_source = 0.d0
              endif
            endif
          endif
        enddo
      enddo
    enddo
  enddo
  return 
  endsubroutine
  ! source term calculation, the fission source term need to think twice. 
  subroutine SourceTerm(mesh_group)
  implicit none
  type(mesh_type), dimension(:,:,:,:), intent(inout) :: mesh_group
  integer :: g, gg, k, kk, i, j
  integer :: energy_cnt
  integer :: angular_cnt
  integer :: x_cnt, y_cnt
  real(8), dimension(:), allocatable :: scalar_flux
  energy_cnt = size(mesh_group, 1)
  angular_cnt = size(mesh_group, 2)
  x_cnt = size(mesh_group, 3)
  y_cnt = size(mesh_group, 4)
  ! allocate the scalar_flux array
  allocate(scalar_flux(energy_cnt))
  ! loop over the spatial, angular and energy parameter.
  do g=1, energy_cnt
    do k=1, angular_cnt
      do j=1, y_cnt
        do i=1, x_cnt
          scalar_flux = 0.d0
          mesh_group(g,k,i,j)%scatter_source = 0.d0
          mesh_group(g,k,i,j)%fission_source = 0.d0
          mesh_group(g,k,i,j)%total_source = 0.d0
          if((mod(i,2).eq.0) .and. (mod(j,2).eq.0)) then
            do gg=1, energy_cnt
              do kk=1, angular_cnt
                scalar_flux(gg) = scalar_flux(gg)                                               &
                                 +0.25d0*mesh_group(gg,kk,i,j)%flux*angular(kk)%omega
              enddo
            enddo
            ! scattering source term
            if(g .eq. 1) then
              mesh_group(g,k,i,j)%scatter_source = mesh_group(g,k,i,j)%scatter_source           &
                                                  +mesh_group(1,k,i,j)%sigma_s11*scalar_flux(1) &
                                                  +mesh_group(2,k,i,j)%sigma_s21*scalar_flux(2)
            else
              mesh_group(g,k,i,j)%scatter_source = mesh_group(g,k,i,j)%scatter_source           &
                                                  +mesh_group(1,k,i,j)%sigma_s12*scalar_flux(1) &
                                                  +mesh_group(2,k,i,j)%sigma_s22*scalar_flux(2)
            endif
            ! fission source term
            mesh_group(g,k,i,j)%fission_source = 0.d0
            ! total source term
            mesh_group(g,k,i,j)%total_source = mesh_group(g,k,i,j)%scatter_source               &
                                              +mesh_group(g,k,i,j)%fission_source               &
                                              +mesh_group(g,k,i,j)%external_source
          endif
        enddo
      enddo
    enddo
  enddo
  ! release the allocated memory
  deallocate(scalar_flux)
  return 
  end subroutine
  ! solve the first direction
  ! mu<0, eta<0
  subroutine SolveFirstDirection(mesh_group, boundary_type, boundary_angular_flux)
  implicit none
  type(mesh_type),dimension(:,:,:,:),intent(inout) :: mesh_group
  integer, intent(in) :: boundary_type
  real(8), intent(in) :: boundary_angular_flux
  integer :: i, j, k, g
  integer :: x_cnt, y_cnt
  integer :: angular_cnt
  integer :: energy_cnt
  real(8) :: nume
  real(8) :: deno
  real(8) :: dx
  real(8) :: dy
  energy_cnt = size(mesh_group, 1)
  angular_cnt = size(mesh_group, 2)
  x_cnt = size(mesh_group, 3)
  y_cnt = size(mesh_group, 4)
  do g=1, energy_cnt
    do k=1, angular_cnt
      if((angular(k)%mu.LT.0.d0) .and. (angular(k)%eta.LT.0.d0)) then
        do j=y_cnt, 1, -1
          do i=x_cnt, 1, -1
            if(j .eq. y_cnt) then
              if(boundary_type .eq. SPECIFIED_BOUND) then
                mesh_group(g,k,i,j)%flux = boundary_angular_flux
              endif
              cycle
            endif
            if(i .eq. x_cnt) then
              if(boundary_type .eq. SPECIFIED_BOUND) then
                mesh_group(g,k,i,j)%flux = boundary_angular_flux
              endif
              cycle
            endif
            if(mod(j,2).eq.0) then
              if(mod(i,2).eq.0) then
                dx = mesh_group(g,k,i+1,j)%x - mesh_group(g,k,i-1,j)%x
                dy = mesh_group(g,k,i,j+1)%y - mesh_group(g,k,i,j-1)%y
                nume = mesh_group(g,k,i,j)%total_source*dx*dy              &
                      -2.d0*angular(k)%mu*mesh_group(g,k,i+1,j)%flux*dy    &
                      -2.d0*angular(k)%eta*mesh_group(g,k,i,j+1)%flux*dx  
                deno = mesh_group(g,k,i,j)%sigma_t*dx*dy &
                      -2.d0*angular(k)%mu*dy             &
                      -2.d0*angular(k)%eta*dx
                mesh_group(g,k,i,j)%flux = nume / deno
                ! exterpolate the flux, diamond difference
                mesh_group(g,k,i,j-1)%flux = 2.d0*mesh_group(g,k,i,j)%flux - mesh_group(g,k,i,j+1)%flux
              else
                ! exterpolate the flux, diamond difference
                mesh_group(g,k,i,j)%flux = 2.d0*mesh_group(g,k,i+1,j)%flux - mesh_group(g,k,i+2,j)%flux
              endif
            endif
          enddo
        enddo
      endif
    enddo
  enddo
  return 
  endsubroutine
  ! solve the second direction
  ! mu>0, eta<0
  subroutine SolveSecondDirection(mesh_group, boundary_type, boundary_angular_flux)
  implicit none
  type(mesh_type),dimension(:,:,:,:),intent(inout) :: mesh_group
  integer, intent(in) :: boundary_type
  real(8), intent(in) :: boundary_angular_flux
  integer :: i, j, k, g
  integer :: x_cnt, y_cnt
  integer :: angular_cnt
  integer :: energy_cnt
  real(8) :: nume
  real(8) :: deno
  real(8) :: dx
  real(8) :: dy
  energy_cnt = size(mesh_group, 1)
  angular_cnt = size(mesh_group, 2)
  x_cnt = size(mesh_group, 3)
  y_cnt = size(mesh_group, 4)
  do g=1, energy_cnt
    do k=1, angular_cnt
      if((angular(k)%mu.GT.0.d0) .and. (angular(k)%eta.LT.0.d0))then
        do j=y_cnt, 1, -1
          do i=1, x_cnt
            if(j .eq. y_cnt)then
              if(boundary_type .eq. SPECIFIED_BOUND)then
                mesh_group(g,k,i,j)%flux = boundary_angular_flux
              endif
              cycle
            endif
            if(i .eq. 1)then
              if(boundary_type .eq. SPECIFIED_BOUND)then
                mesh_group(g,k,i,j)%flux = boundary_angular_flux
              endif
              cycle
            endif
            if(mod(j,2).eq.0)then
              if(mod(i,2).eq.0)then
                dx = mesh_group(g,k,i+1,j)%x - mesh_group(g,k,i-1,j)%x
                dy = mesh_group(g,k,i,j+1)%y - mesh_group(g,k,i,j-1)%y
                nume = mesh_group(g,k,i,j)%total_source*dx*dy             &
                      +2.d0*angular(k)%mu*mesh_group(g,k,i-1,j)%flux*dy   &
                      -2.d0*angular(k)%eta*mesh_group(g,k,i,j+1)%flux*dx
                deno = mesh_group(g,k,i,j)%sigma_t*dx*dy +  &
                      +2.d0*angular(k)%mu*dy                &
                      -2.d0*angular(k)%eta*dx
                mesh_group(g,k,i,j)%flux = nume/deno
                ! exterpolate the flux, diamond difference
                mesh_group(g,k,i,j-1)%flux = 2.d0*mesh_group(g,k,i,j)%flux - mesh_group(g,k,i,j+1)%flux
              else
                ! exterpolate the flux, diamond difference
                mesh_group(g,k,i,j)%flux = 2.d0*mesh_group(g,k,i-1,j)%flux - mesh_group(g,k,i-2,j)%flux
              endif
            endif
          enddo
        enddo
      endif
    enddo
  enddo
  return
  endsubroutine
  ! solve the third direction
  ! mu<0, eta>0        
  subroutine SolveThirdDirection(mesh_group, boundary_type, boundary_angular_flux)
  implicit none
  type(mesh_type),dimension(:,:,:,:),intent(inout) :: mesh_group
  integer, intent(in) :: boundary_type
  real(8), intent(in) :: boundary_angular_flux
  integer :: i, j, k, g
  integer :: x_cnt, y_cnt
  integer :: angular_cnt
  integer :: energy_cnt
  real(8) :: nume
  real(8) :: deno
  real(8) :: dx
  real(8) :: dy
  energy_cnt = size(mesh_group, 1)
  angular_cnt = size(mesh_group, 2)
  x_cnt = size(mesh_group, 3)
  y_cnt = size(mesh_group, 4)
  do g=1, energy_cnt
    do k=1, angular_cnt
      if((angular(k)%mu.LT.0.d0) .and. (angular(k)%eta.GT.0.d0))then
        do j=1, y_cnt
          do i=x_cnt, 1, -1
            if(j .eq. 1) then
              if(boundary_type .eq. SPECIFIED_BOUND)then
                mesh_group(g,k,i,j)%flux = boundary_angular_flux
              endif
              cycle
            endif
            if(i .eq. x_cnt) then
              if(boundary_type .eq. SPECIFIED_BOUND)then
                mesh_group(g,k,i,j)%flux = boundary_angular_flux
              endif
              cycle
            endif
            if(mod(j,2).eq.0)then
              if(mod(i,2).eq.0)then
                dx = mesh_group(g,k,i+1,j)%x - mesh_group(g,k,i-1,j)%x
                dy = mesh_group(g,k,i,j+1)%y - mesh_group(g,k,i,j-1)%y
                nume = mesh_group(g,k,i,j)%total_source*dx*dy               &
                      -2.d0*angular(k)%mu*mesh_group(g,k,i+1,j)%flux*dy     & 
                      +2.d0*angular(k)%eta*mesh_group(g,k,i,j-1)%flux*dx
                deno = mesh_group(g,k,i,j)%sigma_t*dx*dy       &
                      -2.d0*angular(k)%mu*dy                   & 
                      +2.d0*angular(k)%eta*dx
                mesh_group(g,k,i,j)%flux = nume / deno
                ! exterpolate the flux, diamond difference 
                mesh_group(g,k,i,j+1)%flux = 2.d0*mesh_group(g,k,i,j)%flux - mesh_group(g,k,i,j-1)%flux
              else
                mesh_group(g,k,i,j)%flux = 2.d0*mesh_group(g,k,i+1,j)%flux - mesh_group(g,k,i+2,j)%flux
              endif
            endif          
          enddo
        enddo
      endif
    enddo
  enddo
  return 
  endsubroutine
  ! solve the fourth direction
  ! mu>0, eta>0        
  subroutine SolveFourthDirection(mesh_group, boundary_type, boundary_angular_flux)
  implicit none
  type(mesh_type),dimension(:,:,:,:),intent(inout) :: mesh_group
  integer, intent(in) :: boundary_type
  real(8), intent(in) :: boundary_angular_flux
  integer :: i, j, k, g
  integer :: x_cnt, y_cnt
  integer :: angular_cnt
  integer :: energy_cnt
  real(8) :: nume
  real(8) :: deno
  real(8) :: dx
  real(8) :: dy
  energy_cnt = size(mesh_group, 1)
  angular_cnt = size(mesh_group, 2)
  x_cnt = size(mesh_group, 3)
  y_cnt = size(mesh_group, 4)
  do g=1, energy_cnt
    do k=1, angular_cnt
      if((angular(k)%mu.GT.0.d0) .and. (angular(k)%eta.GT.0.d0))then
        do j=1, y_cnt
          do i=1, x_cnt
            if(j .eq. 1) then
              if(boundary_type .eq. SPECIFIED_BOUND) then
                mesh_group(g,k,i,j)%flux = boundary_angular_flux
              endif
              cycle
            endif
            if(i .eq. 1) then
              if(boundary_type .eq. SPECIFIED_BOUND) then
                mesh_group(g,k,i,j)%flux = boundary_angular_flux
              endif
              cycle
            endif
            if(mod(j,2).eq.0)then
              if(mod(i,2).eq.0)then
                dx = mesh_group(g,k,i+1,j)%x - mesh_group(g,k,i-1,j)%x
                dy = mesh_group(g,k,i,j+1)%y - mesh_group(g,k,i,j-1)%y
                nume = mesh_group(g,k,i,j)%total_source*dx*dy            &
                      +2.d0*angular(k)%mu*mesh_group(g,k,i-1,j)%flux*dy  &
                      +2.d0*angular(k)%eta*mesh_group(g,k,i,j-1)%flux*dx 
                deno = mesh_group(g,k,i,j)%sigma_t*dx*dy     & 
                      +2.d0*angular(k)%mu*dy                 & 
                      +2.d0*angular(k)%eta*dx
                mesh_group(g,k,i,j)%flux = nume/deno
                ! exterpolate the flux, diamond difference 
                mesh_group(g,k,i,j+1)%flux = 2.d0*mesh_group(g,k,i,j)%flux - mesh_group(g,k,i,j-1)%flux
              else
                mesh_group(g,k,i,j)%flux = 2.d0*mesh_group(g,k,i-1,j)%flux - mesh_group(g,k,i-2,j)%flux
              endif
            endif
          enddo
        enddo
      endif
    enddo
  enddo
  return 
  endsubroutine
  ! Power Iteration Method
  subroutine PowerIteration(mesh_group, old_mesh_group, len_x, len_y)
  implicit none
  type(mesh_type), dimension(:,:,:,:), intent(inout) :: mesh_group
  type(mesh_type), dimension(:,:,:,:), intent(inout) :: old_mesh_group
  real(8), intent(in) :: len_x
  real(8), intent(in) :: len_y
  integer :: g,k,i,j,gg,kk
  integer :: iter
  integer :: energy_cnt
  integer :: angular_cnt
  integer :: x_cnt
  integer :: y_cnt
  real(8), intrinsic :: sqrt
  real(8), allocatable, dimension(:) :: sum_source
  energy_cnt = size(mesh_group, 1)
  angular_cnt = size(mesh_group, 2)
  x_cnt = size(mesh_group, 3)
  y_cnt = size(mesh_group, 4)
  allocate(sum_source(energy_cnt))
  ! mesher
  call Generate_grid(mesh_group, len_x, len_y)
  ! initialize the macroscopic cross section
  call MacroXS(mesh_group)
  ! initialize the field
  call Initialize(mesh_group, 0.d0)
  old_mesh_group = mesh_group
  do iter=1, MAX_ITER
    sum_source = 0.d0
    ! calculate the source term
    call SourceTerm(mesh_group)
    ! calculate the neutron angular flux field, vaccum boundary
    call SolveFirstDirection(mesh_group, SPECIFIED_BOUND, 0.d0)
    call SolveSecondDirection(mesh_group, SPECIFIED_BOUND, 0.d0)
    call SolveThirdDirection(mesh_group, SPECIFIED_BOUND, 0.d0)
    call SolveFourthDirection(mesh_group, SPECIFIED_BOUND, 0.d0)
    ! check out the source convergence
    do g=1, energy_cnt
      do k=1, angular_cnt
        do j=1, y_cnt
          do i=1, x_cnt
            if((mod(i,2).eq.0).and.(mod(j,2).eq.0))then
              sum_source(g) = sum_source(g) + (mesh_group(g,k,i,j)%total_source &
                             -old_mesh_group(g,k,i,j)%total_source)**2
            endif
          enddo
        enddo
      enddo
    enddo
    if((sqrt(sum_source(1)).LT.errno) .and. (sqrt(sum_source(2)).LT.errno))then
      write(*,*) "The Result has converged!"
      exit
    endif
    write(*,*)"==================================================="
    write(*, "(A10,I5,A15,G15.7)") "Iteration=", iter, "Residual 1  =", sqrt(sum_source(1))
    write(*, "(A15,A15,G15.7)") "          ", "Residual 2  =", sqrt(sum_source(2))
    old_mesh_group = mesh_group
  enddo
  if(iter .GT. MAX_ITER)then
    write(*,*) " Diverge...." 
  endif
  deallocate(sum_source)
  return 
  endsubroutine
  endmodule
  
  module POSTPROCESS
  use DATA_STRUCTURE
  use MESHER
  use SOLVER
  implicit none
  contains
  ! Check out the negative angular flux
  subroutine CheckNegativeFlux(mesh_group)
  implicit none
  type(mesh_type), dimension(:,:,:,:), intent(in) :: mesh_group
  integer :: g,k,i,j
  integer :: energy_cnt
  integer :: angular_cnt
  integer :: x_cnt
  integer :: y_cnt
  integer :: flag
  energy_cnt = size(mesh_group, 1)
  angular_cnt = size(mesh_group, 2)
  x_cnt = size(mesh_group, 3)
  y_cnt = size(mesh_group, 4)
  flag = 1
  do g=1, energy_cnt
    do k=1, angular_cnt
      do j=1, y_cnt
        do i=1, x_cnt
          if(mesh_group(g,k,i,j)%flux .LT. 0.d0)then
            if(abs(mesh_group(g,k,i,j)%flux .GT. 1.d-8))then
              write(*,*)mesh_group(g,k,i,j)%flux
            endif
          endif
        enddo
      enddo
    enddo
  enddo
  if(flag .eq. 0)then
    write(*,*)"Negative angular flux..."
  else
    write(*,*)"Check negative angular flux has passed successfully..."
  endif
  return
  endsubroutine
  ! Export the scalar flux data
  subroutine ExportScalarFlux(mesh_group)
  implicit none
  type(mesh_type), dimension(:,:,:,:), intent(in) :: mesh_group
  integer :: g,k,i,j, kk
  integer :: energy_cnt, angular_cnt
  integer :: x_cnt, y_cnt
  real(8) :: scalar_flux = 0.d0
  integer :: group1_fileid = 20
  integer :: group2_fileid = 21
  character(len=20) :: group1_filename = "FastGroup.dat"
  character(len=20) :: group2_filename = "ThermalGroup.dat"
  energy_cnt = size(mesh_group, 1)
  angular_cnt = size(mesh_group, 2)
  x_cnt = size(mesh_group, 3)
  y_cnt = size(mesh_group, 4)
  ! open the file
  open(unit=group1_fileid, file=group1_filename)
  open(unit=group2_fileid, file=group2_filename)
  ! export the fast group scalar flux data
  do j=y_cnt, 1, -1
    do i=1, x_cnt
      scalar_flux = 0.d0
      if((mod(i,2).eq.0) .and. (mod(j,2).eq.0))then
        do k=1, angular_cnt
          scalar_flux = scalar_flux + 0.25d0*mesh_group(1,k,i,j)%flux*angular(k)%omega
        enddo
        write(group1_fileid,"(F12.6\)") scalar_flux
      endif
    enddo
    write(group1_fileid,*)
  enddo
  ! export the thermal group scalar flux data
  do j=y_cnt, 1, -1
    do i=1, x_cnt
      scalar_flux = 0.d0
      if((mod(i,2).eq.0) .and. (mod(j,2).eq.0))then
        do k=1, angular_cnt
          scalar_flux = scalar_flux + 0.25d0*mesh_group(2,k,i,j)%flux*angular(k)%omega
        enddo
        write(group2_fileid,"(F12.6\)") scalar_flux
      endif
    enddo
    write(group2_fileid,*)
  enddo
  close(group1_fileid)
  close(group2_fileid)
  return
  endsubroutine
  endmodule 
  
  program main
  use DATA_STRUCTURE
  use MESHER
  use SOLVER
  use POSTPROCESS
  implicit none
  type(mesh_type), dimension(:,:,:,:), allocatable :: mesh_group
  type(mesh_type), dimension(:,:,:,:), allocatable :: old_mesh_group
  real(8) :: len_x = 10
  real(8) :: len_y = 10
  integer :: x_cnt = 400
  integer :: y_cnt = 400
  
  allocate(mesh_group(2, SN*(SN+2)/2, 2*x_cnt+1, 2*y_cnt+1))
  allocate(old_mesh_group(2, SN*(SN+2)/2, 2*x_cnt+1, 2*y_cnt+1))
  
  call PowerIteration(mesh_group, old_mesh_group, len_x, len_y)
  call CheckNegativeFlux(mesh_group)
  call ExportScalarFlux(mesh_group)
  
  deallocate(mesh_group)
  deallocate(old_mesh_group)
  endprogram