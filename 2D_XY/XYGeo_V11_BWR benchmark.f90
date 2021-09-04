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
  integer, parameter :: SN = 8
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
    real(8) :: Xf               ! fission spectrum
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
  data angular(1)%mu  / -0.2182179   /
  data angular(2)%mu  / -0.5773503   /
  data angular(3)%mu  / -0.7867958   /
  data angular(4)%mu  / -0.9511897   /
  data angular(5)%mu  / -0.2182179   /
  data angular(6)%mu  / -0.5773503   /
  data angular(7)%mu  / -0.7867958   /
  data angular(8)%mu  / -0.2182179   /
  data angular(9)%mu  / -0.5773503   /
  data angular(10)%mu / -0.2182179   /
  data angular(1)%eta / -0.9511897   /
  data angular(2)%eta / -0.7867958   /
  data angular(3)%eta / -0.5773503   /
  data angular(4)%eta / -0.2182179   /
  data angular(5)%eta / -0.7867958   /
  data angular(6)%eta / -0.5773503   /
  data angular(7)%eta / -0.2182179   /
  data angular(8)%eta / -0.5773503   /
  data angular(9)%eta / -0.2182179   /
  data angular(10)%eta/ -0.2182179   /
  ! 2. mu>0, eta<0
  data angular(11)%mu  /  0.2182179   /
  data angular(12)%mu  /  0.5773503   /
  data angular(13)%mu  /  0.7867958   /
  data angular(14)%mu  /  0.9511897   /
  data angular(15)%mu  /  0.2182179   /
  data angular(16)%mu  /  0.5773503   /
  data angular(17)%mu  /  0.7867958   /
  data angular(18)%mu  /  0.2182179   /
  data angular(19)%mu  /  0.5773503   /
  data angular(20)%mu  /  0.2182179   /
  data angular(11)%eta / -0.9511897   /
  data angular(12)%eta / -0.7867958   /
  data angular(13)%eta / -0.5773503   /
  data angular(14)%eta / -0.2182179   /
  data angular(15)%eta / -0.7867958   /
  data angular(16)%eta / -0.5773503   /
  data angular(17)%eta / -0.2182179   /
  data angular(18)%eta / -0.5773503   /
  data angular(19)%eta / -0.2182179   /
  data angular(20)%eta / -0.2182179   /
  ! 3. mu<0, eta>0
  data angular(21)%mu  / -0.2182179   /
  data angular(22)%mu  / -0.5773503   /
  data angular(23)%mu  / -0.7867958   /
  data angular(24)%mu  / -0.9511897   /
  data angular(25)%mu  / -0.2182179   /
  data angular(26)%mu  / -0.5773503   /
  data angular(27)%mu  / -0.7867958   /
  data angular(28)%mu  / -0.2182179   /
  data angular(29)%mu  / -0.5773503   /
  data angular(30)%mu  / -0.2182179   /
  data angular(21)%eta /  0.9511897   /
  data angular(22)%eta /  0.7867958   /
  data angular(23)%eta /  0.5773503   /
  data angular(24)%eta /  0.2182179   /
  data angular(25)%eta /  0.7867958   /
  data angular(26)%eta /  0.5773503   /
  data angular(27)%eta /  0.2182179   /
  data angular(28)%eta /  0.5773503   /
  data angular(29)%eta /  0.2182179   /
  data angular(30)%eta /  0.2182179   /
  ! 4. mu>0, eta>0
  data angular(31)%mu  /  0.2182179   /
  data angular(32)%mu  /  0.5773503   /
  data angular(33)%mu  /  0.7867958   /
  data angular(34)%mu  /  0.9511897   /
  data angular(35)%mu  /  0.2182179   /
  data angular(36)%mu  /  0.5773503   /
  data angular(37)%mu  /  0.7867958   /
  data angular(38)%mu  /  0.2182179   /
  data angular(39)%mu  /  0.5773503   /
  data angular(40)%mu  /  0.2182179   /
  data angular(31)%eta /  0.9511897   /
  data angular(32)%eta /  0.7867958   /
  data angular(33)%eta /  0.5773503   /
  data angular(34)%eta /  0.2182179   /
  data angular(35)%eta /  0.7867958   /
  data angular(36)%eta /  0.5773503   /
  data angular(37)%eta /  0.2182179   /
  data angular(38)%eta /  0.5773503   /
  data angular(39)%eta /  0.2182179   /
  data angular(40)%eta /  0.2182179   /
  ! omega
  data angular(1)%omega  / 0.1209877 /
  data angular(2)%omega  / 0.0907407 /
  data angular(3)%omega  / 0.0907407 / 
  data angular(4)%omega  / 0.1209877 /
  data angular(5)%omega  / 0.0907407 /
  data angular(6)%omega  / 0.0925926 /
  data angular(7)%omega  / 0.0907407 /
  data angular(8)%omega  / 0.0907407 /
  data angular(9)%omega  / 0.0907407 / 
  data angular(10)%omega / 0.1209877 / 
  
  data angular(11)%omega  / 0.1209877 /
  data angular(12)%omega  / 0.0907407 /
  data angular(13)%omega  / 0.0907407 / 
  data angular(14)%omega  / 0.1209877 /
  data angular(15)%omega  / 0.0907407 /
  data angular(16)%omega  / 0.0925926 /
  data angular(17)%omega  / 0.0907407 /
  data angular(18)%omega  / 0.0907407 /
  data angular(19)%omega  / 0.0907407 / 
  data angular(20)%omega  / 0.1209877 / 
  
  data angular(21)%omega  / 0.1209877 /
  data angular(22)%omega  / 0.0907407 /
  data angular(23)%omega  / 0.0907407 / 
  data angular(24)%omega  / 0.1209877 /
  data angular(25)%omega  / 0.0907407 /
  data angular(26)%omega  / 0.0925926 /
  data angular(27)%omega  / 0.0907407 /
  data angular(28)%omega  / 0.0907407 /
  data angular(29)%omega  / 0.0907407 / 
  data angular(30)%omega  / 0.1209877 / 
  
  data angular(31)%omega  / 0.1209877 /
  data angular(32)%omega  / 0.0907407 /
  data angular(33)%omega  / 0.0907407 / 
  data angular(34)%omega  / 0.1209877 /
  data angular(35)%omega  / 0.0907407 /
  data angular(36)%omega  / 0.0925926 /
  data angular(37)%omega  / 0.0907407 /
  data angular(38)%omega  / 0.0907407 /
  data angular(39)%omega  / 0.0907407 / 
  data angular(40)%omega  / 0.1209877 / 
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
          mesh_group(g,k,i,j)%Xf = 0.d0
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
  integer, parameter :: MAX_ITER = 1E4
  ! set the convergence standard
  real(8), parameter :: errno = 1.d-6
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
            ! Fast group
            if(g .eq. 1) then
              ! region 1
              if((mesh_group(g,k,i,j)%x.GT.1.5d0 .and. mesh_group(g,k,i,j)%x.LT.7.9d0) &
                  .and.(mesh_group(g,k,i,j)%y.GT.1.0d0 .and. mesh_group(g,k,i,j)%y.LT.7.4d0))then
                mesh_group(g,k,i,j)%sigma_t = 1.96647d-1
                mesh_group(g,k,i,j)%sigma_s11 = 1.780d-1
                mesh_group(g,k,i,j)%sigma_s12 = 1.002d-2
                mesh_group(g,k,i,j)%sigma_f = 6.203d-3
                mesh_group(g,k,i,j)%Xf = 1.0d0
                mesh_group(g,k,i,j)%external_source = 0.d0
              ! region 2
              else
                mesh_group(g,k,i,j)%sigma_t = 2.22064d-1
                mesh_group(g,k,i,j)%sigma_s11 = 1.995d-1
                mesh_group(g,k,i,j)%sigma_s12 = 2.188d-2
                mesh_group(g,k,i,j)%sigma_f = 0.d0
                mesh_group(g,k,i,j)%Xf = 1.0d0
                mesh_group(g,k,i,j)%external_source = 0.d0
              endif
            endif
            ! Thermal group
            if(g .eq. 2) then
              ! region 1
              if((mesh_group(g,k,i,j)%x.GT.1.5d0 .and. mesh_group(g,k,i,j)%x.LT.7.9d0) &
                  .and.(mesh_group(g,k,i,j)%y.GT.1.0d0 .and. mesh_group(g,k,i,j)%y.LT.7.4d0))then
                mesh_group(g,k,i,j)%sigma_t = 5.96159d-1
                mesh_group(g,k,i,j)%sigma_s21 = 1.089d-3
                mesh_group(g,k,i,j)%sigma_s22 = 5.255d-1
                mesh_group(g,k,i,j)%sigma_f = 1.101d-1
                mesh_group(g,k,i,j)%Xf = 0.d0
                mesh_group(g,k,i,j)%external_source = 0.d0
              ! region 2
              else
                mesh_group(g,k,i,j)%sigma_t = 8.87874d-1
                mesh_group(g,k,i,j)%sigma_s21 = 1.558d-3
                mesh_group(g,k,i,j)%sigma_s22 = 8.783d-1
                mesh_group(g,k,i,j)%sigma_f = 0.d0
                mesh_group(g,k,i,j)%Xf = 0.d0
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
  subroutine SourceTerm(mesh_group, keff)
  implicit none
  type(mesh_type), dimension(:,:,:,:), intent(inout) :: mesh_group
  real(8), intent(in) :: keff
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
          if((mod(i,2).eq.0) .and. (mod(j,2).eq.0)) then
            do gg=1, energy_cnt
              do kk=1, angular_cnt
                scalar_flux(gg) = scalar_flux(gg)                                               &
                                 +0.25d0*mesh_group(gg,kk,i,j)%flux*angular(kk)%omega
              enddo
            enddo
            ! scattering source term
            if(g .eq. 1) then
              mesh_group(g,k,i,j)%scatter_source = mesh_group(1,k,i,j)%sigma_s11*scalar_flux(1) &
                                                  +mesh_group(2,k,i,j)%sigma_s21*scalar_flux(2)
            else
              mesh_group(g,k,i,j)%scatter_source = mesh_group(1,k,i,j)%sigma_s12*scalar_flux(1) &
                                                  +mesh_group(2,k,i,j)%sigma_s22*scalar_flux(2)
            endif
            ! fission source term
            if(g .eq. 1) then
              mesh_group(g,k,i,j)%fission_source = mesh_group(1,k,i,j)%sigma_f*scalar_flux(1)   &
                                                  *mesh_group(1,k,i,j)%Xf                       &
                                                  +mesh_group(2,k,i,j)%sigma_f*scalar_flux(2)   &
                                                  *mesh_group(1,k,i,j)%Xf
            ! in this case, the thermal group does not have the fission source term.
            else
              mesh_group(g,k,i,j)%fission_source = 0.d0
            endif
            ! total source term
            mesh_group(g,k,i,j)%total_source = mesh_group(g,k,i,j)%scatter_source               &
                                              +mesh_group(g,k,i,j)%external_source              &
                                              +mesh_group(g,k,i,j)%fission_source/keff
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
  integer :: i, j, k, g, kk
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
              ! specified boundary
              if(boundary_type .eq. SPECIFIED_BOUND) then
                mesh_group(g,k,i,j)%flux = boundary_angular_flux
              endif
              ! reflective boundary
              if(boundary_type .eq. REFLECTIVE_BOUND) then
                do kk=1, angular_cnt
                  if(angular(kk)%mu .eq. -angular(k)%mu) then
                    if(angular(kk)%eta .eq. -angular(k)%eta) then
                      mesh_group(g,k,i,j)%flux = mesh_group(g,kk,i,j)%flux
                    endif
                  endif
                enddo
              endif
              cycle
            endif
            if(i .eq. x_cnt) then
              ! specified boundary
              if(boundary_type .eq. SPECIFIED_BOUND) then
                mesh_group(g,k,i,j)%flux = boundary_angular_flux
              endif
              ! reflective boundary
              if(boundary_type .eq. REFLECTIVE_BOUND) then
                do kk=1, angular_cnt
                  if(angular(kk)%mu .eq. -angular(k)%mu)then
                    if(angular(kk)%eta .eq. -angular(k)%eta) then
                      mesh_group(g,k,i,j)%flux = mesh_group(g,kk,i,j)%flux
                    endif
                  endif
                enddo
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
                if(mesh_group(g,k,i,j-1)%flux .LT. 0.d0) then
                  mesh_group(g,k,i,j-1)%flux = 0.d0
                endif
              else
                ! exterpolate the flux, diamond difference
                mesh_group(g,k,i,j)%flux = 2.d0*mesh_group(g,k,i+1,j)%flux - mesh_group(g,k,i+2,j)%flux
                if(mesh_group(g,k,i,j)%flux .LT. 0.d0) then
                  mesh_group(g,k,i,j)%flux = 0.d0
                endif
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
  integer :: i, j, k, g, kk
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
              ! specified boundary
              if(boundary_type .eq. SPECIFIED_BOUND)then
                mesh_group(g,k,i,j)%flux = boundary_angular_flux
              endif
              ! reflective boundary
              if(boundary_type .eq. REFLECTIVE_BOUND) then
                do kk=1, angular_cnt
                  if(angular(kk)%mu .eq. -angular(k)%mu)then
                    if(angular(kk)%eta .eq. -angular(k)%eta) then
                      mesh_group(g,k,i,j)%flux = mesh_group(g,kk,i,j)%flux
                    endif
                  endif
                enddo
              endif
              cycle
            endif
            if(i .eq. 1)then
              ! specified boundary
              if(boundary_type .eq. SPECIFIED_BOUND)then
                mesh_group(g,k,i,j)%flux = boundary_angular_flux
              endif
              ! reflective boundary
              if(boundary_type .eq. REFLECTIVE_BOUND) then
                do kk=1, angular_cnt
                  if(angular(kk)%mu .eq. -angular(k)%mu)then
                    if(angular(kk)%eta .eq. -angular(k)%eta) then
                      mesh_group(g,k,i,j)%flux = mesh_group(g,kk,i,j)%flux
                    endif
                  endif
                enddo
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
                if(mesh_group(g,k,i,j-1)%flux .LT. 0.d0)then
                  mesh_group(g,k,i,j-1)%flux = 0.d0
                endif
              else
                ! exterpolate the flux, diamond difference
                mesh_group(g,k,i,j)%flux = 2.d0*mesh_group(g,k,i-1,j)%flux - mesh_group(g,k,i-2,j)%flux
                if(mesh_group(g,k,i,j)%flux .LT. 0.d0)then
                  mesh_group(g,k,i,j)%flux= 0.d0
                endif
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
  integer :: i, j, k, g, kk
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
              ! specified boundary
              if(boundary_type .eq. SPECIFIED_BOUND)then
                mesh_group(g,k,i,j)%flux = boundary_angular_flux
              endif
              ! reflective boundary
              if(boundary_type .eq. REFLECTIVE_BOUND) then
                do kk=1, angular_cnt
                  if(angular(kk)%mu .eq. -angular(k)%mu)then
                    if(angular(kk)%eta .eq. -angular(k)%eta) then
                      mesh_group(g,k,i,j)%flux = mesh_group(g,kk,i,j)%flux
                    endif
                  endif
                enddo
              endif
              cycle
            endif
            if(i .eq. x_cnt) then
              ! specified boundary
              if(boundary_type .eq. SPECIFIED_BOUND)then
                mesh_group(g,k,i,j)%flux = boundary_angular_flux
              endif
              ! reflective boundary
              if(boundary_type .eq. REFLECTIVE_BOUND) then
                do kk=1, angular_cnt
                  if(angular(kk)%mu .eq. -angular(k)%mu)then
                    if(angular(kk)%eta .eq. -angular(k)%eta) then
                      mesh_group(g,k,i,j)%flux = mesh_group(g,kk,i,j)%flux
                    endif
                  endif
                enddo
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
                if(mesh_group(g,k,i,j+1)%flux .LT. 0.d0)then
                  mesh_group(g,k,i,j+1)%flux = 0.d0
                endif
              else
                mesh_group(g,k,i,j)%flux = 2.d0*mesh_group(g,k,i+1,j)%flux - mesh_group(g,k,i+2,j)%flux
                if(mesh_group(g,k,i,j)%flux .LT. 0.d0)then
                  mesh_group(g,k,i,j)%flux = 0.d0
                endif
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
  integer :: i, j, k, g,kk
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
              ! specified boundary
              if(boundary_type .eq. SPECIFIED_BOUND) then
                mesh_group(g,k,i,j)%flux = boundary_angular_flux
              endif
              ! reflective boundary
              if(boundary_type .eq. REFLECTIVE_BOUND) then
                do kk=1, angular_cnt
                  if(angular(kk)%mu .eq. -angular(k)%mu)then
                    if(angular(kk)%eta .eq. -angular(k)%eta) then
                      mesh_group(g,k,i,j)%flux = mesh_group(g,kk,i,j)%flux
                    endif
                  endif
                enddo
              endif
              cycle
            endif
            if(i .eq. 1) then
              ! specified boundary
              if(boundary_type .eq. SPECIFIED_BOUND) then
                mesh_group(g,k,i,j)%flux = boundary_angular_flux
              endif
              ! reflective boundary
              if(boundary_type .eq. REFLECTIVE_BOUND) then
                do kk=1, angular_cnt
                  if(angular(kk)%mu .eq. -angular(k)%mu)then
                    if(angular(kk)%eta .eq. -angular(k)%eta) then
                      mesh_group(g,k,i,j)%flux = mesh_group(g,kk,i,j)%flux
                    endif
                  endif
                enddo
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
                if(mesh_group(g,k,i,j+1)%flux .LT. 0.d0)then
                  mesh_group(g,k,i,j+1)%flux = 0.d0
                endif
              else
                mesh_group(g,k,i,j)%flux = 2.d0*mesh_group(g,k,i-1,j)%flux - mesh_group(g,k,i-2,j)%flux
                if(mesh_group(g,k,i,j)%flux .LT. 0.d0)then
                  mesh_group(g,k,i,j)%flux = 0.d0
                endif
              endif
            endif
          enddo
        enddo
      endif
    enddo
  enddo
  return 
  endsubroutine
  ! Solve the k-eigenvalue
  subroutine EigenValue(mesh_group, old_mesh_group, keff)
  implicit none
  type(mesh_type), dimension(:,:,:,:), intent(inout) :: mesh_group
  type(mesh_type), dimension(:,:,:,:), intent(inout) :: old_mesh_group
  real(8), intent(inout) :: keff
  integer :: g, k, i, j
  integer :: energy_cnt
  integer :: angular_cnt
  integer :: x_cnt, y_cnt
  real(8) :: sum_fission_source
  real(8) :: old_sum_fission_source
  energy_cnt = size(mesh_group, 1)
  angular_cnt = size(mesh_group, 2)
  x_cnt = size(mesh_group, 3)
  y_cnt = size(mesh_group, 4)
  sum_fission_source = 0.d0
  old_sum_fission_source = 0.d0
  ! loop over the spatial, angular and energy parameter.
  do g=1, energy_cnt
    do k=1, angular_cnt
      do j=1, y_cnt
        do i=1, x_cnt
          if((mod(i,2).eq.0) .and. (mod(j,2).eq.0)) then
            sum_fission_source = sum_fission_source + mesh_group(g,k,i,j)%fission_source 
            old_sum_fission_source = old_sum_fission_source + old_mesh_group(g,k,i,j)%fission_source 
          endif
        enddo
      enddo
    enddo
  enddo
  keff = keff*sum_fission_source/old_sum_fission_source
  return 
  end subroutine
  ! Power Iteration Method
  subroutine PowerIteration(mesh_group, old_mesh_group, len_x, len_y, keff_guess)
  implicit none
  type(mesh_type), dimension(:,:,:,:), intent(inout) :: mesh_group
  type(mesh_type), dimension(:,:,:,:), intent(inout) :: old_mesh_group
  real(8), intent(in) :: len_x
  real(8), intent(in) :: len_y
  real(8), intent(in) :: keff_guess
  integer :: g,k,i,j,gg,kk
  integer :: iter
  integer :: energy_cnt
  integer :: angular_cnt
  integer :: x_cnt
  integer :: y_cnt
  real(8) :: keff, old_keff
  real(8) :: residual = 0.d0
  real(8), intrinsic :: sqrt
  energy_cnt = size(mesh_group, 1)
  angular_cnt = size(mesh_group, 2)
  x_cnt = size(mesh_group, 3)
  y_cnt = size(mesh_group, 4)
  ! mesher
  call Generate_grid(mesh_group, len_x, len_y)
  ! initialize the macroscopic cross section
  call MacroXS(mesh_group)
  ! initialize the field
  keff = keff_guess
  call Initialize(mesh_group, 1.d0)
  call SourceTerm(mesh_group, keff)
  old_mesh_group = mesh_group
  old_keff = keff + 1.d0
  do iter=1, MAX_ITER
    ! calculate the source term
    call SourceTerm(mesh_group,keff)
    ! calculate the neutron angular flux field, vaccum boundary
    call SolveFirstDirection(mesh_group, REFLECTIVE_BOUND, -1.d0)
    call SolveSecondDirection(mesh_group, REFLECTIVE_BOUND, -1.d0)
    call SolveThirdDirection(mesh_group, REFLECTIVE_BOUND, -1.d0)
    call SolveFourthDirection(mesh_group, REFLECTIVE_BOUND, -1.d0)
    ! check out the keff convergence
    call EigenValue(mesh_group, old_mesh_group, keff)
    residual = abs(old_keff - keff)
    if(residual .LT. errno) then
      write(*,*) "The keff has converged!"
      write(*,*) "keff = ", keff
      exit
    endif
    write(*, "(A15,I3,A15,G15.7,A20,G15.7)") "Iteration=", iter,      &
                                             "Residual=", residual,   &
                                             "K-eigenvalue=", keff
    old_mesh_group = mesh_group
    old_keff = keff
  enddo
  if(iter .GT. MAX_ITER)then
    write(*,*) " Diverge...." 
  endif
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
            if(abs(mesh_group(g,k,i,j)%flux) .GT. 1.d-8)then
              write(*,*)mesh_group(g,k,i,j)%flux
              flag = 0
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
  real(8) :: len_x = 8.9
  real(8) :: len_y = 8.9
  integer :: x_cnt = 400
  integer :: y_cnt = 400
  
  allocate(mesh_group(2, SN*(SN+2)/2, 2*x_cnt+1, 2*y_cnt+1))
  allocate(old_mesh_group(2, SN*(SN+2)/2, 2*x_cnt+1, 2*y_cnt+1))
  
  call PowerIteration(mesh_group, old_mesh_group, len_x, len_y, 1.d0)
  call CheckNegativeFlux(mesh_group)
  call ExportScalarFlux(mesh_group)
  
  deallocate(mesh_group)
  deallocate(old_mesh_group)
  endprogram