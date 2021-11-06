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
  integer, parameter :: GROUP = 7 ! number of energy group
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
    integer :: color            ! used to draw the picture.
    real(8) :: flux             ! angular flux
    real(8) :: x                
    real(8) :: y                
    real(8) :: scatter_source   ! scattering source term
    real(8) :: external_source  ! external source term
    real(8) :: fission_source   ! fission source term
    real(8) :: total_source     ! total source term
  endtype
  ! group constant type
  type :: group_constant_type
    real(8) :: sigma_t          ! total macro-xs
    real(8) :: sigma_s(GROUP)   ! scatter macro-xs 
    real(8) :: sigma_f          ! fission macro-xs
    real(8) :: Nu               ! number of emission neutron
    real(8) :: Xf               ! fission spectrum
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
  USE omp_lib
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
  ! initialize the group constant
  subroutine GenerateMacroXS(group_constant)
  implicit none
  type(group_constant_type), dimension(:, :), intent(inout) :: group_constant
  integer :: region_cnt
  integer :: group_cnt
  integer :: i, j
  region_cnt = size(group_constant,1)
  group_cnt = size(group_constant,2)
  do i=1, region_cnt
    do j=1, group_cnt
      group_constant(i,j)%sigma_t = 0.d0
      group_constant(i,j)%sigma_s = 0.d0
      group_constant(i,j)%sigma_f = 0.d0
      group_constant(i,j)%Nu = 0.d0
      group_constant(i,j)%Xf = 0.d0
    enddo
  enddo  
  ! guide tube, color = 1
  ! group 1
  group_constant(1,1)%sigma_t = 1.26032E-01
  group_constant(1,1)%sigma_s(1) = 6.61659d-2
  group_constant(1,1)%sigma_s(2) = 5.90700d-2
  group_constant(1,1)%sigma_s(3) = 2.83340d-4
  group_constant(1,1)%sigma_s(4) = 1.46220d-6
  group_constant(1,1)%sigma_s(5) = 2.06420d-8
  group_constant(1,1)%sigma_s(6) = 0.d0
  group_constant(1,1)%sigma_s(7) = 0.d0
  ! group 2
  group_constant(1,2)%sigma_t = 2.93160E-01
  group_constant(1,2)%sigma_s(1) = 0.d0
  group_constant(1,2)%sigma_s(2) = 2.40377d-1
  group_constant(1,2)%sigma_s(3) = 5.24350d-2
  group_constant(1,2)%sigma_s(4) = 2.49900d-4
  group_constant(1,2)%sigma_s(5) = 1.92390d-5
  group_constant(1,2)%sigma_s(6) = 2.98750d-6
  group_constant(1,2)%sigma_s(7) = 4.21400d-7
  ! group 3
  group_constant(1,3)%sigma_t = 2.84240E-01
  group_constant(1,3)%sigma_s(1) = 0.d0
  group_constant(1,3)%sigma_s(2) = 0.d0
  group_constant(1,3)%sigma_s(3) = 1.83297d-1
  group_constant(1,3)%sigma_s(4) = 9.23970d-2
  group_constant(1,3)%sigma_s(5) = 6.94460d-3
  group_constant(1,3)%sigma_s(6) = 1.08030d-3
  group_constant(1,3)%sigma_s(7) = 2.05670d-4
  ! group 4
  group_constant(1,4)%sigma_t = 2.80960E-01
  group_constant(1,4)%sigma_s(1) = 0.d0
  group_constant(1,4)%sigma_s(2) = 0.d0
  group_constant(1,4)%sigma_s(3) = 0.d0
  group_constant(1,4)%sigma_s(4) = 7.88511d-2
  group_constant(1,4)%sigma_s(5) = 1.70140d-1
  group_constant(1,4)%sigma_s(6) = 2.58810d-2
  group_constant(1,4)%sigma_s(7) = 4.92970d-3
  ! group 5
  group_constant(1,5)%sigma_t = 3.34440E-01
  group_constant(1,5)%sigma_s(1) = 0.d0
  group_constant(1,5)%sigma_s(2) = 0.d0
  group_constant(1,5)%sigma_s(3) = 0.d0
  group_constant(1,5)%sigma_s(4) = 3.73330d-5
  group_constant(1,5)%sigma_s(5) = 9.97372d-2
  group_constant(1,5)%sigma_s(6) = 2.06790d-1
  group_constant(1,5)%sigma_s(7) = 2.44780d-2
  ! group 6
  group_constant(1,6)%sigma_t = 5.65640E-01
  group_constant(1,6)%sigma_s(1) = 0.d0
  group_constant(1,6)%sigma_s(2) = 0.d0
  group_constant(1,6)%sigma_s(3) = 0.d0
  group_constant(1,6)%sigma_s(4) = 0.d0
  group_constant(1,6)%sigma_s(5) = 9.17260d-4
  group_constant(1,6)%sigma_s(6) = 3.16765d-1
  group_constant(1,6)%sigma_s(7) = 2.38770d-1
  ! group 7
  group_constant(1,7)%sigma_t = 1.17215E+00
  group_constant(1,7)%sigma_s(1) = 0.d0
  group_constant(1,7)%sigma_s(2) = 0.d0
  group_constant(1,7)%sigma_s(3) = 0.d0
  group_constant(1,7)%sigma_s(4) = 0.d0
  group_constant(1,7)%sigma_s(5) = 0.d0
  group_constant(1,7)%sigma_s(6) = 4.97920d-2
  group_constant(1,7)%sigma_s(7) = 1.09912d0
  
  ! fission chamber, color = 2
  ! group 1
  group_constant(2,1)%sigma_t = 1.26032E-01
  group_constant(2,1)%sigma_f = 4.79002d-9
  group_constant(2,1)%Nu = 2.76283d0
  group_constant(2,1)%Xf = 5.87910d-1
  group_constant(2,1)%sigma_s(1) = 6.61659d-2
  group_constant(2,1)%sigma_s(2) = 5.90700d-2
  group_constant(2,1)%sigma_s(3) = 2.83340d-4
  group_constant(2,1)%sigma_s(4) = 1.46220d-6
  group_constant(2,1)%sigma_s(5) = 2.06420d-8
  group_constant(2,1)%sigma_s(6) = 0.d0
  group_constant(2,1)%sigma_s(7) = 0.d0
  ! group 2
  group_constant(2,2)%sigma_t = 2.93160E-01
  group_constant(2,2)%sigma_f = 5.82564d-9
  group_constant(2,2)%Nu = 2.46239d+00
  group_constant(2,2)%Xf = 4.11760d-01
  group_constant(2,2)%sigma_s(1) = 0.d0
  group_constant(2,2)%sigma_s(2) = 2.40377d-01
  group_constant(2,2)%sigma_s(3) = 5.24350d-02
  group_constant(2,2)%sigma_s(4) = 2.49900d-04
  group_constant(2,2)%sigma_s(5) = 1.92390d-05
  group_constant(2,2)%sigma_s(6) = 2.98750d-06
  group_constant(2,2)%sigma_s(7) = 4.21400d-07
  ! group 3
  group_constant(2,3)%sigma_t = 2.84250E-01
  group_constant(2,3)%sigma_f = 4.63719d-07
  group_constant(2,3)%Nu = 2.43380d0
  group_constant(2,3)%Xf = 3.39060d-04
  group_constant(2,3)%sigma_s(1) = 0.d0
  group_constant(2,3)%sigma_s(2) = 0.d0
  group_constant(2,3)%sigma_s(3) = 1.83425d-01
  group_constant(2,3)%sigma_s(4) = 9.22880d-02
  group_constant(2,3)%sigma_s(5) = 6.93650d-03
  group_constant(2,3)%sigma_s(6) = 1.07900d-03
  group_constant(2,3)%sigma_s(7) = 2.05430d-04
  ! group 4
  group_constant(2,4)%sigma_t = 2.81020E-01
  group_constant(2,4)%sigma_f = 5.24406E-06
  group_constant(2,4)%Nu = 2.43380E+00
  group_constant(2,4)%Xf = 1.17610E-07
  group_constant(2,4)%sigma_s(1) = 0.d0
  group_constant(2,4)%sigma_s(2) = 0.d0
  group_constant(2,4)%sigma_s(3) = 0.d0
  group_constant(2,4)%sigma_s(4) = 7.90769E-02
  group_constant(2,4)%sigma_s(5) = 1.69990E-01
  group_constant(2,4)%sigma_s(6) = 2.58600E-02
  group_constant(2,4)%sigma_s(7) = 4.92560E-03
  ! group 5
  group_constant(2,5)%sigma_t = 3.34460E-01
  group_constant(2,5)%sigma_f = 1.45390E-07
  group_constant(2,5)%Nu = 2.43380E+00 
  group_constant(2,5)%Xf = 0.d0
  group_constant(2,5)%sigma_s(1) = 0.d0
  group_constant(2,5)%sigma_s(2) = 0.d0
  group_constant(2,5)%sigma_s(3) = 0.d0
  group_constant(2,5)%sigma_s(4) = 3.73400E-05
  group_constant(2,5)%sigma_s(5) = 9.97570E-02
  group_constant(2,5)%sigma_s(6) = 2.06790E-01
  group_constant(2,5)%sigma_s(7) = 2.44780E-02
  ! group 6
  group_constant(2,6)%sigma_t = 5.65640E-01
  group_constant(2,6)%sigma_f = 7.14972E-07
  group_constant(2,6)%Nu = 2.43380E+00
  group_constant(2,6)%Xf = 0.d0
  group_constant(2,6)%sigma_s(1) = 0.d0
  group_constant(2,6)%sigma_s(2) = 0.d0
  group_constant(2,6)%sigma_s(3) = 0.d0
  group_constant(2,6)%sigma_s(4) = 0.d0
  group_constant(2,6)%sigma_s(5) = 9.17420E-04
  group_constant(2,6)%sigma_s(6) = 3.16774E-01
  group_constant(2,6)%sigma_s(7) = 2.38760E-01
  ! group 7
  group_constant(2,7)%sigma_t = 1.17214E+00
  group_constant(2,7)%sigma_f = 2.08041E-06
  group_constant(2,7)%Nu = 2.43380E+00
  group_constant(2,7)%Xf = 0.00000E+00
  group_constant(2,7)%sigma_s(1) = 0.d0
  group_constant(2,7)%sigma_s(2) = 0.d0
  group_constant(2,7)%sigma_s(3) = 0.d0
  group_constant(2,7)%sigma_s(4) = 0.d0
  group_constant(2,7)%sigma_s(5) = 0.d0
  group_constant(2,7)%sigma_s(6) = 4.97930E-02
  group_constant(2,7)%sigma_s(7) = 1.09910E+00
  
  ! UO2 fuel, color = 3
  ! group 1
  group_constant(3,1)%sigma_t = 1.77949E-01
  group_constant(3,1)%sigma_f = 7.21206E-03
  group_constant(3,1)%Nu = 2.78145E+00
  group_constant(3,1)%Xf = 5.87910E-01
  group_constant(3,1)%sigma_s(1) = 1.27537E-01
  group_constant(3,1)%sigma_s(2) = 4.23780E-02
  group_constant(3,1)%sigma_s(3) = 9.43740E-06
  group_constant(3,1)%sigma_s(4) = 5.51630E-09
  group_constant(3,1)%sigma_s(5) = 0.d0
  group_constant(3,1)%sigma_s(6) = 0.d0
  group_constant(3,1)%sigma_s(7) = 0.d0
  ! group 2
  group_constant(3,2)%sigma_t = 3.29805E-01
  group_constant(3,2)%sigma_f = 8.19301E-04
  group_constant(3,2)%Nu = 2.47443E+00
  group_constant(3,2)%Xf = 4.11760E-01
  group_constant(3,2)%sigma_s(1) = 0.d0
  group_constant(3,2)%sigma_s(2) = 3.24456E-01
  group_constant(3,2)%sigma_s(3) = 1.63140E-03
  group_constant(3,2)%sigma_s(4) = 3.14270E-09
  group_constant(3,2)%sigma_s(5) = 0.d0 
  group_constant(3,2)%sigma_s(6) = 0.d0
  group_constant(3,2)%sigma_s(7) = 0.d0
  ! group 3
  group_constant(3,3)%sigma_t = 4.80388E-01
  group_constant(3,3)%sigma_f = 6.45320E-03
  group_constant(3,3)%Nu = 2.43383E+00
  group_constant(3,3)%Xf = 3.39060E-04
  group_constant(3,3)%sigma_s(1) = 0.d0
  group_constant(3,3)%sigma_s(2) = 0.d0
  group_constant(3,3)%sigma_s(3) = 4.50940E-01
  group_constant(3,3)%sigma_s(4) = 2.67920E-03
  group_constant(3,3)%sigma_s(5) = 0.d0
  group_constant(3,3)%sigma_s(6) = 0.d0
  group_constant(3,3)%sigma_s(7) = 0.d0
  ! group 4
  group_constant(3,4)%sigma_t = 5.54367E-01
  group_constant(3,4)%sigma_f = 1.85648E-02
  group_constant(3,4)%Nu = 2.43380E+00
  group_constant(3,4)%Xf = 1.17610E-07
  group_constant(3,4)%sigma_s(1) = 0.d0
  group_constant(3,4)%sigma_s(2) = 0.d0
  group_constant(3,4)%sigma_s(3) = 0.d0
  group_constant(3,4)%sigma_s(4) = 4.52565E-01
  group_constant(3,4)%sigma_s(5) = 5.56640E-03
  group_constant(3,4)%sigma_s(6) = 0.d0
  group_constant(3,4)%sigma_s(7) = 0.d0
  ! group 5
  group_constant(3,5)%sigma_t = 3.11801E-01
  group_constant(3,5)%sigma_f = 1.78084E-02
  group_constant(3,5)%Nu = 2.43380E+00
  group_constant(3,5)%Xf = 0.d0
  group_constant(3,5)%sigma_s(1) = 0.d0
  group_constant(3,5)%sigma_s(2) = 0.d0
  group_constant(3,5)%sigma_s(3) = 0.d0
  group_constant(3,5)%sigma_s(4) = 1.25250E-04
  group_constant(3,5)%sigma_s(5) = 2.71401E-01
  group_constant(3,5)%sigma_s(6) = 1.02550E-02
  group_constant(3,5)%sigma_s(7) = 1.00210E-08
  ! group 6
  group_constant(3,6)%sigma_t = 3.95168E-01
  group_constant(3,6)%sigma_f = 8.30348E-02
  group_constant(3,6)%Nu = 2.43380E+00
  group_constant(3,6)%Xf = 0.d0
  group_constant(3,6)%sigma_s(1) = 0.d0
  group_constant(3,6)%sigma_s(2) = 0.d0
  group_constant(3,6)%sigma_s(3) = 0.d0
  group_constant(3,6)%sigma_s(4) = 0.d0
  group_constant(3,6)%sigma_s(5) = 1.29680E-03
  group_constant(3,6)%sigma_s(6) = 2.65802E-01
  group_constant(3,6)%sigma_s(7) = 1.68090E-02
  ! group 7
  group_constant(3,7)%sigma_t = 5.64406E-01
  group_constant(3,7)%sigma_f = 2.16004E-01
  group_constant(3,7)%Nu = 2.43380E+00
  group_constant(3,7)%Xf = 0.d0
  group_constant(3,7)%sigma_s(1) = 0.d0
  group_constant(3,7)%sigma_s(2) = 0.d0
  group_constant(3,7)%sigma_s(3) = 0.d0
  group_constant(3,7)%sigma_s(4) = 0.d0
  group_constant(3,7)%sigma_s(5) = 0.d0
  group_constant(3,7)%sigma_s(6) = 8.54580E-03
  group_constant(3,7)%sigma_s(7) = 2.73080E-01
  
  ! moderator, color = 4
  ! group 1
  group_constant(4,1)%sigma_t = 1.59206E-01
  group_constant(4,1)%sigma_s(1) = 4.44777E-02
  group_constant(4,1)%sigma_s(2) = 1.13400E-01
  group_constant(4,1)%sigma_s(3) = 7.23470E-04
  group_constant(4,1)%sigma_s(4) = 3.74990E-06
  group_constant(4,1)%sigma_s(5) = 5.31840E-08
  group_constant(4,1)%sigma_s(6) = 0.d0
  group_constant(4,1)%sigma_s(7) = 0.d0
  ! group 2
  group_constant(4,2)%sigma_t = 4.12970E-01
  group_constant(4,2)%sigma_s(1) = 0.d0
  group_constant(4,2)%sigma_s(2) = 2.82334E-01
  group_constant(4,2)%sigma_s(3) = 1.29940E-01
  group_constant(4,2)%sigma_s(4) = 6.23400E-04
  group_constant(4,2)%sigma_s(5) = 4.80020E-05
  group_constant(4,2)%sigma_s(6) = 7.44860E-06
  group_constant(4,2)%sigma_s(7) = 1.04550E-06
  ! group 3
  group_constant(4,3)%sigma_t    = 5.90310E-01
  group_constant(4,3)%sigma_s(1) = 0.d0
  group_constant(4,3)%sigma_s(2) = 0.d0
  group_constant(4,3)%sigma_s(3) = 3.45256E-01
  group_constant(4,3)%sigma_s(4) = 2.24570E-01
  group_constant(4,3)%sigma_s(5) = 1.69990E-02
  group_constant(4,3)%sigma_s(6) = 2.64430E-03
  group_constant(4,3)%sigma_s(7) = 5.03440E-04
  ! group 4
  group_constant(4,4)%sigma_t    = 5.84350E-01
  group_constant(4,4)%sigma_s(1) = 0.d0
  group_constant(4,4)%sigma_s(2) = 0.d0
  group_constant(4,4)%sigma_s(3) = 0.d0
  group_constant(4,4)%sigma_s(4) = 9.10284E-02
  group_constant(4,4)%sigma_s(5) = 4.15510E-01
  group_constant(4,4)%sigma_s(6) = 6.37320E-02
  group_constant(4,4)%sigma_s(7) = 1.21390E-02
  ! group 5
  group_constant(4,5)%sigma_t    = 7.18000E-01
  group_constant(4,5)%sigma_s(1) = 0.d0
  group_constant(4,5)%sigma_s(2) = 0.d0
  group_constant(4,5)%sigma_s(3) = 0.d0
  group_constant(4,5)%sigma_s(4) = 7.14370E-05
  group_constant(4,5)%sigma_s(5) = 1.39138E-01
  group_constant(4,5)%sigma_s(6) = 5.11820E-01
  group_constant(4,5)%sigma_s(7) = 6.12290E-02
  ! group 6
  group_constant(4,6)%sigma_t    = 1.25445E+00
  group_constant(4,6)%sigma_s(1) = 0.d0
  group_constant(4,6)%sigma_s(2) = 0.d0
  group_constant(4,6)%sigma_s(3) = 0.d0
  group_constant(4,6)%sigma_s(4) = 0.d0
  group_constant(4,6)%sigma_s(5) = 2.21570E-03
  group_constant(4,6)%sigma_s(6) = 6.99913E-01
  group_constant(4,6)%sigma_s(7) = 5.37320E-01
  ! group 7
  group_constant(4,7)%sigma_t    = 2.65038E+00
  group_constant(4,7)%sigma_s(1) = 0.d0
  group_constant(4,7)%sigma_s(2) = 0.d0
  group_constant(4,7)%sigma_s(3) = 0.d0
  group_constant(4,7)%sigma_s(4) = 0.d0
  group_constant(4,7)%sigma_s(5) = 0.d0
  group_constant(4,7)%sigma_s(6) = 1.32440E-01
  group_constant(4,7)%sigma_s(7) = 2.48070E+00
  return
  endsubroutine
  ! get the distance
  subroutine GetDistance(x1, y1, x2, y2, distance)
  implicit none
  real(8), intent(in) :: x1
  real(8), intent(in) :: y1
  real(8), intent(in) :: x2
  real(8), intent(in) :: y2
  real(8), intent(inout) :: distance
  real(8), intrinsic :: sqrt
  distance = sqrt((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2))
  return 
  endsubroutine
  ! initialize the macro-xs
  subroutine MacroXS(mesh_group)
  implicit none
  type(mesh_type),dimension(:,:,:,:),intent(inout) :: mesh_group
  integer :: i,j,k,g,ii,jj
  integer :: x_cnt, y_cnt, angular_cnt, energy_cnt
  real(8) :: x, y
  real(8) :: circle_x, circle_y   ! tube circle central location.
  real(8) :: distance
  energy_cnt = size(mesh_group, 1)
  angular_cnt = size(mesh_group, 2)
  x_cnt = size(mesh_group, 3)
  y_cnt = size(mesh_group, 4)
  do g=1, energy_cnt
    do k=1, angular_cnt
      do j=1, y_cnt
        do i=1, x_cnt
          if((mod(i,2).eq.0) .and. (mod(j,2).eq.0)) then
            x = mesh_group(g,k,i,j)%x
            y = mesh_group(g,k,i,j)%y
            ! the left-top UO2 region
            do ii=1, 17
              do jj=1, 17
                if((x.GT.(0.d0+(ii-1)*1.26).and.x.LT.(0.d0+ii*1.26)) .and. &
                    (y.GT.(21.42-jj*1.26).and.y.LT.(21.42-(jj-1)*1.26))) then
                  circle_x = 0.5d0*((0.d0+(ii-1)*1.26)+(0.d0+ii*1.26))
                  circle_y = 0.5d0*((21.42-jj*1.26)+(21.42-(jj-1)*1.26))
                  ! calculate the distance, judge the point in tube or not.
                  call GetDistance(x,y,circle_x,circle_y,distance)
                  if(distance.LT.0.54)then ! in the tube
                    ! judge the fission chamber, fuel or the guide tube
                    if ((ii.eq.6 .and.jj.eq.3).OR. &
                        (ii.eq.9 .and.jj.eq.3).OR. &
                        (ii.eq.12.and.jj.eq.3).OR. &
                        (ii.eq.4 .and.jj.eq.4).OR. &
                        (ii.eq.14.and.jj.eq.4).OR. &
                        (ii.eq.3 .and.jj.eq.6).OR. &
                        (ii.eq.6 .and.jj.eq.6).OR. &
                        (ii.eq.9 .and.jj.eq.6).OR. &
                        (ii.eq.12.and.jj.eq.6).OR. &
                        (ii.eq.15.and.jj.eq.6).OR. &
                        (ii.eq.3 .and.jj.eq.9).OR. &
                        (ii.eq.6 .and.jj.eq.9).OR. & 
                        (ii.eq.12.and.jj.eq.9).OR. &
                        (ii.eq.15.and.jj.eq.9).OR. &
                        (ii.eq.3 .and.jj.eq.12).OR. &
                        (ii.eq.6 .and.jj.eq.12).OR. &
                        (ii.eq.9 .and.jj.eq.12).OR. &
                        (ii.eq.12.and.jj.eq.12).OR. &
                        (ii.eq.15.and.jj.eq.12).OR. &
                        (ii.eq.4 .and.jj.eq.14).OR. &
                        (ii.eq.14.and.jj.eq.14).OR. &
                        (ii.eq.6 .and.jj.eq.15).OR. &
                        (ii.eq.9 .and.jj.eq.15).OR. &
                        (ii.eq.12.and.jj.eq.15))then
                      ! guide tube
                      mesh_group(g,k,i,j)%color = 1
                    elseif(ii.eq.9.and.jj.eq.9)then
                      ! fission chamber
                      mesh_group(g,k,i,j)%color = 2
                    else
                      ! UO2 fuel
                      mesh_group(g,k,i,j)%color = 3
                    endif  ! inner tube region, differnet material endif
                  else   ! outer tube region
                    ! moderator
                    mesh_group(g,k,i,j)%color = 4
                  endif
                endif
              enddo
            enddo
          endif
        enddo
      enddo
    enddo
  enddo
  return 
  endsubroutine
! source term calculation, the fission source term need to think twice. 
  subroutine SourceTerm(mesh_group, group_constant, keff)
  implicit none
  type(mesh_type), dimension(:,:,:,:), intent(inout) :: mesh_group
  type(group_constant_type),dimension(:,:),intent(in) :: group_constant
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
!$omp parallel do num_threads(7) private(scalar_flux)
  do g=1, energy_cnt
    do k=1, angular_cnt
      do j=1, y_cnt
        do i=1, x_cnt
          scalar_flux = 0.d0
          mesh_group(g,k,i,j)%scatter_source = 0.d0
          mesh_group(g,k,i,j)%fission_source = 0.d0
          if((mod(i,2).eq.0) .and. (mod(j,2).eq.0)) then
            do gg=1, energy_cnt
              do kk=1, angular_cnt
                scalar_flux(gg) = scalar_flux(gg)                                               &
                                 +0.25d0*mesh_group(gg,kk,i,j)%flux*angular(kk)%omega
              enddo
            enddo
            ! scattering source term
            do gg=1,energy_cnt
              mesh_group(g,k,i,j)%scatter_source = mesh_group(g,k,i,j)%scatter_source           &
                              +group_constant(mesh_group(gg,k,i,j)%color,gg)%sigma_s(g)         &
                              *scalar_flux(gg)
            enddo
            ! fission source term
            do gg=1, energy_cnt
              mesh_group(g,k,i,j)%fission_source = mesh_group(g,k,i,j)%fission_source           &
                              +group_constant(mesh_group(gg,k,i,j)%color,gg)%sigma_f            &
                              *group_constant(mesh_group(gg,k,i,j)%color,gg)%Nu                 &
                              *group_constant(mesh_group(g,k,i,j)%color,g)%Xf*scalar_flux(gg)
            enddo
            ! total source term
            mesh_group(g,k,i,j)%total_source = mesh_group(g,k,i,j)%scatter_source               &
                                              +mesh_group(g,k,i,j)%external_source              &
                                              +mesh_group(g,k,i,j)%fission_source/keff            
          endif
        enddo
      enddo
    enddo
  enddo
!$omp end parallel do
  ! release the allocated memory
  deallocate(scalar_flux)
  return 
  end subroutine
  ! solve the first direction
  ! mu<0, eta<0
  subroutine SolveFirstDirection(mesh_group, old_mesh_group,                  &
                                 group_constant,                              &
                                 boundary_type_X, boundary_angular_flux_X,    &
                                 boundary_type_Y, boundary_angular_flux_Y)
  implicit none
  type(mesh_type),dimension(:,:,:,:),intent(inout) :: mesh_group
  type(mesh_type),dimension(:,:,:,:),intent(inout) :: old_mesh_group
  type(group_constant_type),dimension(:,:),intent(in) :: group_constant
  integer, intent(in) :: boundary_type_X
  real(8), intent(in) :: boundary_angular_flux_X
  integer, intent(in) :: boundary_type_Y
  real(8), intent(in) :: boundary_angular_flux_Y
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
!$omp parallel do num_threads(7) private(dx,dy,nume,deno)
  do g=1, energy_cnt
    do k=1, angular_cnt
      if((angular(k)%mu.LT.0.d0) .and. (angular(k)%eta.LT.0.d0)) then
        do j=y_cnt, 1, -1
          do i=x_cnt, 1, -1
            if(j .eq. y_cnt) then
              ! specified boundary
              if(boundary_type_Y .eq. SPECIFIED_BOUND) then
                mesh_group(g,k,i,j)%flux = boundary_angular_flux_Y
              endif
              ! reflective boundary
              if(boundary_type_Y .eq. REFLECTIVE_BOUND) then
                do kk=1, angular_cnt
                  if(angular(kk)%mu .eq. angular(k)%mu) then
                    if(angular(kk)%eta .eq. -angular(k)%eta) then
                      mesh_group(g,k,i,j)%flux = old_mesh_group(g,kk,i,j)%flux
                    endif
                  endif
                enddo
              endif
              cycle
            endif
            if(i .eq. x_cnt) then
              ! specified boundary
              if(boundary_type_X .eq. SPECIFIED_BOUND) then
                mesh_group(g,k,i,j)%flux = boundary_angular_flux_X
              endif
              ! reflective boundary
              if(boundary_type_X .eq. REFLECTIVE_BOUND) then
                do kk=1, angular_cnt
                  if(angular(kk)%mu .eq. -angular(k)%mu)then
                    if(angular(kk)%eta .eq. angular(k)%eta) then
                      mesh_group(g,k,i,j)%flux = old_mesh_group(g,kk,i,j)%flux
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
                deno = group_constant(mesh_group(g,k,i,j)%color,g)%sigma_t*dx*dy &
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
!$omp end parallel do
  return 
  endsubroutine
  ! solve the second direction
  ! mu>0, eta<0
  subroutine SolveSecondDirection(mesh_group, old_mesh_group,               &
                                  group_constant,                           &
                                  boundary_type_X, boundary_angular_flux_X, &
                                  boundary_type_Y,boundary_angular_flux_Y)
  implicit none
  type(mesh_type),dimension(:,:,:,:),intent(inout) :: mesh_group
  type(mesh_type),dimension(:,:,:,:),intent(inout) :: old_mesh_group
  type(group_constant_type),dimension(:,:),intent(in) :: group_constant
  integer, intent(in) :: boundary_type_X
  real(8), intent(in) :: boundary_angular_flux_X
  integer, intent(in) :: boundary_type_Y
  real(8), intent(in) :: boundary_angular_flux_Y
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
!$omp parallel do num_threads(7) private(dx,dy,nume,deno)
  do g=1, energy_cnt
    do k=1, angular_cnt
      if((angular(k)%mu.GT.0.d0) .and. (angular(k)%eta.LT.0.d0))then
        do j=y_cnt, 1, -1
          do i=1, x_cnt
            if(j .eq. y_cnt)then
              ! specified boundary
              if(boundary_type_Y .eq. SPECIFIED_BOUND)then
                mesh_group(g,k,i,j)%flux = boundary_angular_flux_Y
              endif
              ! reflective boundary
              if(boundary_type_Y .eq. REFLECTIVE_BOUND) then
                do kk=1, angular_cnt
                  if(angular(kk)%mu .eq. angular(k)%mu)then
                    if(angular(kk)%eta .eq. -angular(k)%eta) then
                      mesh_group(g,k,i,j)%flux = old_mesh_group(g,kk,i,j)%flux
                    endif
                  endif
                enddo
              endif
              cycle
            endif
            if(i .eq. 1)then
              ! specified boundary
              if(boundary_type_X .eq. SPECIFIED_BOUND)then
                mesh_group(g,k,i,j)%flux = boundary_angular_flux_X
              endif
              ! reflective boundary
              if(boundary_type_X .eq. REFLECTIVE_BOUND) then
                do kk=1, angular_cnt
                  if(angular(kk)%mu .eq. -angular(k)%mu)then
                    if(angular(kk)%eta .eq. angular(k)%eta) then
                      mesh_group(g,k,i,j)%flux = old_mesh_group(g,kk,i,j)%flux
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
                deno = group_constant(mesh_group(g,k,i,j)%color,g)%sigma_t*dx*dy &
                      +2.d0*angular(k)%mu*dy                                     &
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
!$omp end parallel do
  return
  endsubroutine
  ! solve the third direction
  ! mu<0, eta>0        
  subroutine SolveThirdDirection(mesh_group, old_mesh_group,               &
                                 group_constant,                           &
                                 boundary_type_X, boundary_angular_flux_X, &
                                 boundary_type_Y,boundary_angular_flux_Y)
  implicit none
  type(mesh_type),dimension(:,:,:,:),intent(inout) :: mesh_group
  type(mesh_type),dimension(:,:,:,:),intent(inout) :: old_mesh_group
  type(group_constant_type),dimension(:,:),intent(in) :: group_constant
  integer, intent(in) :: boundary_type_X
  real(8), intent(in) :: boundary_angular_flux_X
  integer, intent(in) :: boundary_type_Y
  real(8), intent(in) :: boundary_angular_flux_Y
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
!$omp parallel do num_threads(7) private(dx,dy,nume,deno)
  do g=1, energy_cnt
    do k=1, angular_cnt
      if((angular(k)%mu.LT.0.d0) .and. (angular(k)%eta.GT.0.d0))then
        do j=1, y_cnt
          do i=x_cnt, 1, -1
            if(j .eq. 1) then
              ! specified boundary
              if(boundary_type_Y .eq. SPECIFIED_BOUND)then
                mesh_group(g,k,i,j)%flux = boundary_angular_flux_Y
              endif
              ! reflective boundary
              if(boundary_type_Y .eq. REFLECTIVE_BOUND) then
                do kk=1, angular_cnt
                  if(angular(kk)%mu .eq. angular(k)%mu)then
                    if(angular(kk)%eta .eq. -angular(k)%eta) then
                      mesh_group(g,k,i,j)%flux = old_mesh_group(g,kk,i,j)%flux
                    endif
                  endif
                enddo
              endif
              cycle
            endif
            if(i .eq. x_cnt) then
              ! specified boundary
              if(boundary_type_X .eq. SPECIFIED_BOUND)then
                mesh_group(g,k,i,j)%flux = boundary_angular_flux_X
              endif
              ! reflective boundary
              if(boundary_type_X .eq. REFLECTIVE_BOUND) then
                do kk=1, angular_cnt
                  if(angular(kk)%mu .eq. -angular(k)%mu)then
                    if(angular(kk)%eta .eq. angular(k)%eta) then
                      mesh_group(g,k,i,j)%flux = old_mesh_group(g,kk,i,j)%flux
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
                deno = group_constant(mesh_group(g,k,i,j)%color,g)%sigma_t*dx*dy &
                      -2.d0*angular(k)%mu*dy                                     & 
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
!$omp end parallel do
  return 
  endsubroutine
  ! solve the fourth direction
  ! mu>0, eta>0        
  subroutine SolveFourthDirection(mesh_group, old_mesh_group,               &
                                  group_constant,                           &
                                  boundary_type_X, boundary_angular_flux_X, &
                                  boundary_type_Y,boundary_angular_flux_Y)
  implicit none
  type(mesh_type),dimension(:,:,:,:),intent(inout) :: mesh_group
  type(mesh_type),dimension(:,:,:,:),intent(inout) :: old_mesh_group
  type(group_constant_type), dimension(:,:),intent(in)::group_constant
  integer, intent(in) :: boundary_type_X
  real(8), intent(in) :: boundary_angular_flux_X
  integer, intent(in) :: boundary_type_Y
  real(8), intent(in) :: boundary_angular_flux_Y
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
!$omp parallel do num_threads(7) private(dx,dy,nume,deno)
  do g=1, energy_cnt
    do k=1, angular_cnt
      if((angular(k)%mu.GT.0.d0) .and. (angular(k)%eta.GT.0.d0))then
        do j=1, y_cnt
          do i=1, x_cnt
            if(j .eq. 1) then
              ! specified boundary
              if(boundary_type_Y .eq. SPECIFIED_BOUND) then
                mesh_group(g,k,i,j)%flux = boundary_angular_flux_Y
              endif
              ! reflective boundary
              if(boundary_type_Y .eq. REFLECTIVE_BOUND) then
                do kk=1, angular_cnt
                  if(angular(kk)%mu .eq. angular(k)%mu)then
                    if(angular(kk)%eta .eq. -angular(k)%eta) then
                      mesh_group(g,k,i,j)%flux = old_mesh_group(g,kk,i,j)%flux
                    endif
                  endif
                enddo
              endif
              cycle
            endif
            if(i .eq. 1) then
              ! specified boundary
              if(boundary_type_X .eq. SPECIFIED_BOUND) then
                mesh_group(g,k,i,j)%flux = boundary_angular_flux_X
              endif
              ! reflective boundary
              if(boundary_type_X .eq. REFLECTIVE_BOUND) then
                do kk=1, angular_cnt
                  if(angular(kk)%mu .eq. -angular(k)%mu)then
                    if(angular(kk)%eta .eq. angular(k)%eta) then
                      mesh_group(g,k,i,j)%flux = old_mesh_group(g,kk,i,j)%flux
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
                deno = group_constant(mesh_group(g,k,i,j)%color,g)%sigma_t*dx*dy &
                      +2.d0*angular(k)%mu*dy                                     & 
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
!$omp end parallel do
  return 
  endsubroutine
  ! Solve the k-eigenvalue
  subroutine EigenValue(mesh_group, old_mesh_group, group_constant, keff)
  implicit none
  type(mesh_type), dimension(:,:,:,:), intent(inout) :: mesh_group
  type(mesh_type), dimension(:,:,:,:), intent(inout) :: old_mesh_group
  type(group_constant_type),dimension(:,:),intent(in):: group_constant
  real(8), intent(inout) :: keff
  integer :: g, gg, k, kk, i, j
  integer :: energy_cnt
  integer :: angular_cnt
  integer :: x_cnt, y_cnt
  real(8) :: sum_fission_source
  real(8) :: old_sum_fission_source
  real(8), dimension(:), allocatable :: scalar_flux
  real(8), dimension(:), allocatable :: old_scalar_flux
  energy_cnt = size(mesh_group, 1)
  angular_cnt = size(mesh_group, 2)
  x_cnt = size(mesh_group, 3)
  y_cnt = size(mesh_group, 4)
  sum_fission_source = 0.d0
  old_sum_fission_source = 0.d0
  ! allocate the scalar_flux array
  allocate(scalar_flux(energy_cnt))
  allocate(old_scalar_flux(energy_cnt))
  ! loop over the spatial, angular and energy parameter.
!$omp parallel num_threads(7) private(scalar_flux,old_scalar_flux)
!$omp do reduction(+:sum_fission_source) reduction(+:old_sum_fission_source)
  do g=1, energy_cnt
    do k=1, angular_cnt
      do j=1, y_cnt
        do i=1, x_cnt
          scalar_flux = 0.d0
          old_scalar_flux = 0.d0
          if((mod(i,2).eq.0) .and. (mod(j,2).eq.0)) then
            do gg=1, energy_cnt
              do kk=1, angular_cnt
                scalar_flux(gg) = scalar_flux(gg)                                     &
                                 +0.25d0*mesh_group(gg,kk,i,j)%flux*angular(kk)%omega
                old_scalar_flux(gg) = old_scalar_flux(gg)                             &
                                     +0.25d0*old_mesh_group(gg,kk,i,j)%flux*angular(kk)%omega
              enddo
            enddo
            do gg=1, energy_cnt
              sum_fission_source = sum_fission_source                        &
                  +group_constant(mesh_group(gg,k,i,j)%color,gg)%sigma_f     &                    
                  *group_constant(mesh_group(gg,k,i,j)%color,gg)%Nu          &
                  *scalar_flux(gg)                        
              old_sum_fission_source = old_sum_fission_source                &
                  +group_constant(old_mesh_group(gg,k,i,j)%color,gg)%sigma_f &
                  *group_constant(old_mesh_group(gg,k,i,j)%color,gg)%Nu      &
                  *old_scalar_flux(gg)
            enddo
          endif
        enddo
      enddo
    enddo
  enddo
!$omp end do
!$omp end parallel
  keff = keff*sum_fission_source/old_sum_fission_source
  sum_fission_source = 0.d0
  old_sum_fission_source = 0.d0
  deallocate(scalar_flux)
  deallocate(old_scalar_flux)
  return 
  end subroutine
  ! Power Iteration Method
  subroutine PowerIteration(mesh_group, old_mesh_group, group_constant, len_x, len_y, keff_guess)
  implicit none
  type(mesh_type), dimension(:,:,:,:), intent(inout) :: mesh_group
  type(mesh_type), dimension(:,:,:,:), intent(inout) :: old_mesh_group
  type(group_constant_type), dimension(:,:),intent(inout) :: group_constant
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
  real(8) :: parallel_start = 0.d0
  real(8) :: parallel_end = 0.d0
  energy_cnt = size(mesh_group, 1)
  angular_cnt = size(mesh_group, 2)
  x_cnt = size(mesh_group, 3)
  y_cnt = size(mesh_group, 4)
  ! mesher
  call Generate_grid(mesh_group, len_x, len_y)
  ! initialize the Group Constant
  call GenerateMacroXS(group_constant)
  ! initialize the macroscopic cross section
  call MacroXS(mesh_group)
  ! initialize the field
  keff = keff_guess
  call Initialize(mesh_group, 1.d0)
  !call SourceTerm(mesh_group, group_constant, keff)
  old_mesh_group = mesh_group
  old_keff = keff + 1.d0
  
!$ call omp_set_nested(2)
!$omp parallel num_threads(4)
  do iter=1, MAX_ITER
!$omp barrier
!$omp single
    ! calculate the source term
    call SourceTerm(mesh_group,group_constant,keff)
!$omp end single
    ! calculate the neutron angular flux field, vaccum boundary
!$omp sections
!$omp section
!$  write(*, *) "1st direction, thread id=", omp_get_thread_num()
    call SolveFirstDirection(mesh_group, old_mesh_group, group_constant,    &
                             REFLECTIVE_BOUND, 0.d0, REFLECTIVE_BOUND, -1.d0)
!$omp section
!$  write(*, *) "2nd direction, thread id=", omp_get_thread_num()
    call SolveSecondDirection(mesh_group, old_mesh_group, group_constant,   &
                              REFLECTIVE_BOUND, -1.d0, REFLECTIVE_BOUND, -1.d0)
!$omp section
!$  write(*, *) "3rd direction, thread id=", omp_get_thread_num()
    call SolveThirdDirection(mesh_group, old_mesh_group, group_constant,    &
                             REFLECTIVE_BOUND, 0.d0, REFLECTIVE_BOUND, 0.d0)
!$omp section
!$  write(*, *) "4th direction, thread id=", omp_get_thread_num()
    call SolveFourthDirection(mesh_group, old_mesh_group, group_constant,   &
                              REFLECTIVE_BOUND, -1.d0, REFLECTIVE_BOUND, 0.d0)
!$omp end sections
    ! check out the keff convergence
!$omp single
!$  write(*, *) "Eigenvalue calculation, thread id=", omp_get_thread_num()
!$  parallel_start = omp_get_wtime()
    call EigenValue(mesh_group, old_mesh_group, group_constant, keff)
!$  parallel_end = omp_get_wtime()
!$  write(*, *) "Eigenvalue calculation time consumption: ", parallel_end - parallel_start
    residual = abs(old_keff - keff)
    write(*, "(A15,I3,A15,G15.7,A20,G15.7)") "Iteration=", iter,      &
                                             "Residual=", residual,   &
                                             "K-eigenvalue=", keff
    old_mesh_group = mesh_group
    old_keff = keff
!$omp end single
    if(residual .LT. errno) then
      write(*,*) "The keff has converged!"
      write(*,*) "keff = ", keff
      exit
    endif
  enddo
!$omp end parallel
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
  integer :: group1_fileid = 21
  integer :: group2_fileid = 22
  integer :: group3_fileid = 23
  integer :: group4_fileid = 24
  integer :: group5_fileid = 25
  integer :: group6_fileid = 26
  integer :: group7_fileid = 27
  character(len=20) :: group1_filename = "G1.dat"
  character(len=20) :: group2_filename = "G2.dat"
  character(len=20) :: group3_filename = "G3.dat"
  character(len=20) :: group4_filename = "G4.dat"
  character(len=20) :: group5_filename = "G5.dat"
  character(len=20) :: group6_filename = "G6.dat"
  character(len=20) :: group7_filename = "G7.dat"
  energy_cnt = size(mesh_group, 1)
  angular_cnt = size(mesh_group, 2)
  x_cnt = size(mesh_group, 3)
  y_cnt = size(mesh_group, 4)
  ! open the file
  open(unit=group1_fileid, file=group1_filename)
  open(unit=group2_fileid, file=group2_filename)
  open(unit=group3_fileid, file=group3_filename)
  open(unit=group4_fileid, file=group4_filename)
  open(unit=group5_fileid, file=group5_filename)
  open(unit=group6_fileid, file=group6_filename)
  open(unit=group7_fileid, file=group7_filename)
  ! export the Group 1 scalar flux data
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
  ! export the Group 2 scalar flux data
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
  ! export the Group 3 scalar flux data
  do j=y_cnt, 1, -1
    do i=1, x_cnt
      scalar_flux = 0.d0
      if((mod(i,2).eq.0) .and. (mod(j,2).eq.0))then
        do k=1, angular_cnt
          scalar_flux = scalar_flux + 0.25d0*mesh_group(3,k,i,j)%flux*angular(k)%omega
        enddo
        write(group3_fileid,"(F12.6\)") scalar_flux
      endif
    enddo
    write(group3_fileid,*)
  enddo
  ! export the Group 4 scalar flux data
  do j=y_cnt, 1, -1
    do i=1, x_cnt
      scalar_flux = 0.d0
      if((mod(i,2).eq.0) .and. (mod(j,2).eq.0))then
        do k=1, angular_cnt
          scalar_flux = scalar_flux + 0.25d0*mesh_group(4,k,i,j)%flux*angular(k)%omega
        enddo
        write(group4_fileid,"(F12.6\)") scalar_flux
      endif
    enddo
    write(group4_fileid,*)
  enddo
  ! export the Group 5 scalar flux data
  do j=y_cnt, 1, -1
    do i=1, x_cnt
      scalar_flux = 0.d0
      if((mod(i,2).eq.0) .and. (mod(j,2).eq.0))then
        do k=1, angular_cnt
          scalar_flux = scalar_flux + 0.25d0*mesh_group(5,k,i,j)%flux*angular(k)%omega
        enddo
        write(group5_fileid,"(F12.6\)") scalar_flux
      endif
    enddo
    write(group5_fileid,*)
  enddo
  ! export the Group 6 scalar flux data
  do j=y_cnt, 1, -1
    do i=1, x_cnt
      scalar_flux = 0.d0
      if((mod(i,2).eq.0) .and. (mod(j,2).eq.0))then
        do k=1, angular_cnt
          scalar_flux = scalar_flux + 0.25d0*mesh_group(6,k,i,j)%flux*angular(k)%omega
        enddo
        write(group6_fileid,"(F12.6\)") scalar_flux
      endif
    enddo
    write(group6_fileid,*)
  enddo
  ! export the Group 7 scalar flux data
  do j=y_cnt, 1, -1
    do i=1, x_cnt
      scalar_flux = 0.d0
      if((mod(i,2).eq.0) .and. (mod(j,2).eq.0))then
        do k=1, angular_cnt
          scalar_flux = scalar_flux + 0.25d0*mesh_group(7,k,i,j)%flux*angular(k)%omega
        enddo
        write(group7_fileid,"(F12.6\)") scalar_flux
      endif
    enddo
    write(group7_fileid,*)
  enddo
  close(group1_fileid)
  close(group2_fileid)
  close(group3_fileid)
  close(group4_fileid)
  close(group5_fileid)
  close(group6_fileid)
  close(group7_fileid)
  return
  endsubroutine
  ! draw the material picture
  subroutine DrawPicture(mesh_group)
  implicit none
  type(mesh_type),dimension(:,:,:,:),intent(in) :: mesh_group
  integer :: i, j
  integer :: x_cnt, y_cnt
  character(len=20) :: fig = "material.dat"
  integer :: fileid = 100
  x_cnt = size(mesh_group,3)
  y_cnt = size(mesh_group,4)
  ! open the file
  open(unit=fileid, file=fig)
  ! export the material message
  do j=y_cnt, 1, -1
    do i=1,x_cnt
      if((mod(i,2).eq.0).and.(mod(j,2).eq.0))then
        write(fileid,"(I2\)") mesh_group(1,1,i,j)%color
      endif
    enddo
    write(fileid,*)
  enddo
  close(fileid)
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
  type(group_constant_type), dimension(:,:),allocatable::group_constant
  real(8) :: len_x = 21.42d0
  real(8) :: len_y = 21.42d0
  integer :: x_cnt = 300
  integer :: y_cnt = 300
  integer :: region_cnt = 4
  
  allocate(mesh_group(GROUP, SN*(SN+2)/2, 2*x_cnt+1, 2*y_cnt+1))
  allocate(old_mesh_group(GROUP, SN*(SN+2)/2, 2*x_cnt+1, 2*y_cnt+1))
  allocate(group_constant(region_cnt, GROUP))
  
  
  call PowerIteration(mesh_group, old_mesh_group, group_constant, len_x, len_y, 1.d0)
  call CheckNegativeFlux(mesh_group)
  
  !call Generate_grid(mesh_group, len_x, len_y)
  !call MacroXS(mesh_group)
  !call DrawPicture(mesh_group)
  
  call ExportScalarFlux(mesh_group)
  
  deallocate(mesh_group)
  deallocate(old_mesh_group)
  deallocate(group_constant)
  write(*,*)"done..."
  endprogram