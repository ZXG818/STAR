!  slab.f90 
!
!  FUNCTIONS:
!  slab - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: slab
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    ! One-Dimension slab geometry NTE solver
    ! structure module
    module data_structure
    implicit none
    private i
    integer :: i
    real(8), parameter :: PI = 3.141592653589793
    integer, parameter :: SN = 8
    integer, parameter :: SPECIFIED_BOUND = 1
    integer, parameter :: REFLECTIVE_BOUND = 2
    real(8), dimension(SN) :: angular
    real(8), dimension(SN) :: omega
    ! self-definition structure
    type :: meshtype
        real(8) :: flux
        real(8) :: x
        real(8) :: sigma_t         ! total macro-xs
        real(8) :: sigma_s         ! scatter marco-xs
        real(8) :: scatter_source  ! Scattering source term
        real(8) :: external_source ! External constant source 
        real(8) :: total_source    ! total source 
    endtype
    ! multi-region definition structure
    type :: regiontype
        real(8) :: x_lower
        real(8) :: x_upper
        real(8) :: sigma_t
        real(8) :: sigma_s
        real(8) :: external_source
    endtype
    ! initialize the angular and omega
    data(angular(i), i=1, SN)      &
    /-0.9602898565, -0.7966664774, &
    -0.5255324099, -0.1834346425,  &
    0.1834346425, 0.5255324099,    &
    0.7966664774, 0.9602898565/
    data(omega(i), i=1, SN)        &
    /0.0506142681, 0.1111905172,   &
     0.1568533229, 0.1818418917,   &
     0.1818418917, 0.1568533229,   &
     0.1111905172, 0.0506142681/
    end module
    
    ! mesh module 
    module mesher
    use data_structure
    implicit none
    contains
    ! generate the grid
    subroutine Generate_grid(mesh_group, length)
    implicit none
    type(meshtype),dimension(:,:),intent(inout) :: mesh_group
    real(8),intent(in) :: length
    real(8) :: dx
    integer :: angular_cnt, x_cnt
    integer :: i, j
    angular_cnt = size(mesh_group,1)
    x_cnt = size(mesh_group,2)
    dx = length / (x_cnt-1)
    do i=1,angular_cnt      ! angular parameter
        do j=1,x_cnt        ! locate parameter
            mesh_group(i,j)%x = (j-1)*dx
            mesh_group(i,j)%flux = 0.d0
            mesh_group(i,j)%sigma_t = 0.d0
            mesh_group(i,j)%sigma_s = 0.d0
            mesh_group(i,j)%scatter_source = 0.d0
            mesh_group(i,j)%external_source = 0.d0
            mesh_group(i,j)%total_source = 0.d0
        enddo
    enddo
    return 
    endsubroutine
    endmodule
    
    ! solver module, contains the field initialization and computation.
    module solver
    use data_structure
    use mesher
    implicit none
    ! set the maximum iteration is 1000.
    integer, parameter :: MAX_ITER = 1000
    ! set the convergence standard
    real(8), parameter :: errno = 1.d-9
    contains
    ! initialize the field
    subroutine Initialize(mesh_group, value)
    implicit none
    type(meshtype), dimension(:, :), intent(inout) :: mesh_group
    real(8), intent(in) :: value
    integer :: i,j
    do i=1,size(mesh_group,1)
        do j=1, size(mesh_group,2)
            mesh_group(i,j)%flux = value
        enddo
    enddo
    endsubroutine
    ! initialize the macro-xs
    subroutine MacroXS(mesh_group, x_lower, x_upper, SIGMA_T, SIGMA_S, external_source)
    implicit none
    type(meshtype),dimension(:,:),intent(inout) :: mesh_group
    real(8),intent(in) :: x_lower
    real(8),intent(in) :: x_upper
    real(8),intent(in) :: SIGMA_T
    real(8),intent(in) :: SIGMA_S
    real(8),intent(in) :: external_source
    integer :: i,j
    integer :: angular_cnt, x_cnt
    angular_cnt = size(mesh_group, 1)
    x_cnt = size(mesh_group, 2)
    do i=1, angular_cnt
        do j=1, x_cnt
            if((mod(i,2).eq.0) .and. (mod(j,2).eq.0)) then
                if(mesh_group(i,j)%x .LT. x_upper) then
                    if(mesh_group(i,j)%x .GT. x_lower) then
                        mesh_group(i,j)%sigma_t = SIGMA_T
                        mesh_group(i,j)%sigma_s = SIGMA_S
                        mesh_group(i,j)%external_source = external_source
                    endif
                endif
            endif
        enddo
    enddo
    return
    endsubroutine
    ! source term calculation
    subroutine SourceTerm(mesh_group)
    implicit none
    type(meshtype), dimension(:,:), intent(inout) :: mesh_group
    integer :: i, j, k
    integer :: angular_cnt, x_cnt
    real(8) :: sum = 0.d0
    angular_cnt = size(mesh_group, 1)
    x_cnt = size(mesh_group, 2)
    do i=1, angular_cnt
        do j=1, x_cnt
            sum = 0.d0
            if((mod(i,2).eq.0) .and. (mod(j,2).eq.0)) then
                do k=1, angular_cnt
                    if(mod(k,2).eq.0) then
                        sum = sum + mesh_group(k, j)%flux * omega(k/2)
                    endif
                enddo
                ! scattering source term
                mesh_group(i,j)%scatter_source = mesh_group(i,j)%sigma_s * sum
                ! total source term = scattering source + external source
                mesh_group(i,j)%total_source = mesh_group(i,j)%scatter_source &
                                             + mesh_group(i,j)%external_source
            endif
        enddo
    enddo
    return 
    endsubroutine
    ! solve right side, the incoming field
    ! boundary_type = 1 => specified boundary angular flux, just like the vaccum
    !               = 2 => the reflective boundary
    subroutine SolveRightSide(mesh_group, boundary_type, boundary_angular_flux)
    implicit none
    type(meshtype),dimension(:,:),intent(inout) :: mesh_group
    integer, intent(in) :: boundary_type
    real(8), intent(in) :: boundary_angular_flux
    real(8) :: l_km  ! optical thickness
    real(8) :: temp
    integer :: i, j
    integer :: angular_cnt, x_cnt
    angular_cnt = size(mesh_group,1)
    x_cnt = size(mesh_group, 2)
    do i=2, angular_cnt/2+1, 2
        do j=x_cnt, 1, -1
            if(j .eq. x_cnt) then
                if(boundary_type .eq. SPECIFIED_BOUND) then
                    mesh_group(i,j)%flux = boundary_angular_flux
                else if(boundary_type .eq. REFLECTIVE_BOUND) then
                    mesh_group(i,j)%flux = mesh_group(angular_cnt-i+1,j)%flux
                else
                    write(*, *) "The unknown boundary type, please recheck the input..."
                    stop
                endif
                cycle
            endif
            if(mod(j,2).eq.0)then
                ! optical thickness
                l_km = (mesh_group(i,j+1)%x - mesh_group(i,j-1)%x) &
                       * mesh_group(i,j)%sigma_t / angular(i/2)
                temp = 2.d0*mesh_group(i,j+1)%flux & 
                        /(2.d0-l_km)
                mesh_group(i,j)%flux = temp-l_km*mesh_group(i,j)%total_source / &
                    (2.d0-l_km) / mesh_group(i,j)%sigma_t
            else
                ! optical thickness
                l_km = (mesh_group(i,j+2)%x - mesh_group(i,j)%x) & 
                        * mesh_group(i,j+1)%sigma_t / angular(i/2)
                temp = (2.d0 + l_km) * mesh_group(i,j+2)%flux / (2.d0 - l_km)
                temp = temp + (-2.d0)*l_km * mesh_group(i,j+1)%total_source / &
                        (2.d0 - l_km) / mesh_group(i,j+1)%sigma_t
                mesh_group(i,j)%flux = temp
                !mesh_group(i,j)%flux = 2.d0*mesh_group(i,j+1)%flux - mesh_group(i,j+2)%flux
            endif
        enddo
    enddo
    return 
    endsubroutine
    ! solve left side, the outgoing field
    subroutine SolveLeftSide(mesh_group, boundary_type, boundary_angular_flux)
    implicit none
    type(meshtype),dimension(:,:),intent(inout) :: mesh_group
    integer, intent(in) :: boundary_type
    real(8), intent(in) :: boundary_angular_flux
    real(8) :: l_km ! optical thickness
    real(8) :: temp
    integer :: i,j 
    integer :: angular_cnt, x_cnt
    angular_cnt = size(mesh_group, 1)
    x_cnt = size(mesh_group, 2)
    do i=angular_cnt/2+2, angular_cnt, 2
        do j=1, x_cnt
            if(j .eq. 1) then ! reflect boundary
                if(boundary_type .eq. SPECIFIED_BOUND) then
                    mesh_group(i,j)%flux = boundary_angular_flux
                else if(boundary_type .eq. REFLECTIVE_BOUND) then
                    mesh_group(i,j)%flux = mesh_group(angular_cnt-i+1,j)%flux
                else
                    write(*, *) "The unknown boundary type, please recheck the input..."
                    stop
                endif
                cycle
            endif
            if(mod(j,2).eq.0)then
                ! optical thickness
                l_km = (mesh_group(i,j+1)%x - mesh_group(i,j-1)%x) &
                       * mesh_group(i,j)%sigma_t / angular(i/2)
                temp = 2.d0*mesh_group(i,j-1)%flux / (2.d0+l_km)
                mesh_group(i,j)%flux = temp + l_km*mesh_group(i,j)%total_source &
                        / (2.d0 + l_km) / mesh_group(i,j)%sigma_t
            else
                ! optical thickness
                l_km = (mesh_group(i,j)%x - mesh_group(i,j-2)%x) & 
                        * mesh_group(i,j-1)%sigma_t / angular(i/2)
                temp = (2.d0 - l_km) * mesh_group(i,j-2)%flux / (2.d0+l_km)
                temp = temp + (2.d0*l_km)*mesh_group(i,j-1)%total_source & 
                        / (2.d0+l_km) / mesh_group(i,j-1)%sigma_t
                mesh_group(i,j)%flux = temp
                !mesh_group(i,j)%flux = 2.d0*mesh_group(i,j-1)%flux - mesh_group(i,j-2)%flux
            endif
        enddo
    enddo
    return 
    endsubroutine
    ! Power Iteration Method
    subroutine PowerIteration(mesh_group, old_mesh_group, length, REGION)
    implicit none
    type(meshtype), dimension(:, :), intent(inout) :: mesh_group
    type(meshtype), dimension(:, :), intent(inout) :: old_mesh_group
    real(8), intent(in) :: length
    type(regiontype), dimension(:), intent(in) :: REGION
    real(8), intrinsic  :: sqrt
    integer :: i, j, k
    integer :: angular_cnt, x_cnt, region_cnt
    real(8) :: sum = 0.d0
    angular_cnt = size(mesh_group, 1)
    x_cnt = size(mesh_group, 2)
    region_cnt = size(REGION, 1)
    ! mesher
    call Generate_grid(mesh_group, length)
    ! First is the MOX fuel region, second is the UO2 fuel region
    ! initialize the macro-xs
    do i=1, region_cnt
        call MacroXS(mesh_group, region(i)%x_lower, region(i)%x_upper, &
                    region(i)%sigma_t, region(i)%sigma_s, region(i)%external_source)
    enddo
    ! initialize the field
    call Initialize(mesh_group, 0.d0)
    old_mesh_group = mesh_group
    do i=1, MAX_ITER
        sum = 0.d0
        ! calculate the source term
        call SourceTerm(mesh_group)
        ! calculate the incoming field
        call SolveRightSide(mesh_group, REFLECTIVE_BOUND, -1.d0)
        ! calculate the outgoing field
        call SolveLeftSide(mesh_group, REFLECTIVE_BOUND, -1.d0)
        do j=1, angular_cnt
            do k=1, x_cnt
                if((mod(j,2).eq.0) .and. (mod(k,2).eq.0))then
                    sum = sum + (mesh_group(j,k)%total_source &
                            -old_mesh_group(j,k)%total_source)**2
                endif
            enddo
        enddo
        if(sqrt(sum) .LT. errno)then
            write(*,*) "The result has converged!"
            exit
        endif      
        write(*, "(A10,I3,A10,G15.7)") "Iteration=", i, "Residual=", sqrt(sum)
        old_mesh_group = mesh_group
    enddo
    if(i .eq. MAX_ITER)then
        write(*,*) "Diverge...."      
    endif
    return 
    endsubroutine    
    endmodule
    
    ! Post
    module GetResult
    use data_structure
    implicit none
    contains
    ! get the neutron flux
    subroutine ScalarFlux(mesh_group)
    implicit none
    type(meshtype), dimension(:, :), intent(in) :: mesh_group
    real(8), allocatable, dimension(:) :: scalar_flux
    integer :: i, j
    integer :: angular_cnt, x_cnt
    angular_cnt = size(mesh_group, 1)
    x_cnt = size(mesh_group, 2)
    allocate(scalar_flux(x_cnt/2))
    scalar_flux = 0.d0
    do i=1, angular_cnt
        do j=1, x_cnt
            if((mod(i,2).eq.0) .and. (mod(j,2).eq.0)) then
                scalar_flux(j/2) = scalar_flux(j/2) + omega(i/2) & 
                                    * mesh_group(i,j)%flux
            endif
            if(mesh_group(i,j)%flux .LT. 0.d0)then
                write(*,*)"WARNING : NEGATIVATE FLUX"
            endif
        enddo
    enddo
    write(*, *)"scalar_flux : "
    do i=1, x_cnt/2
        write(*, *) mesh_group(1,2*i)%x, scalar_flux(i)
    enddo
    deallocate(scalar_flux)
    return 
    endsubroutine
    ! write the angular flux matrix
    subroutine AngularFluxTable(mesh_group)
    implicit none
    type(meshtype),dimension(:,:),intent(in) :: mesh_group
    integer :: angular_cnt
    integer :: x_cnt
    integer :: i, j
    integer :: fileid = 11
    character(len=20) :: filename = "angularflux.txt"
    
    angular_cnt = size(mesh_group, 1)
    x_cnt = size(mesh_group, 2)
    open(unit=fileid, file=filename)
    write(fileid, "(17I10)") (i, i=1, 17)
    do j=x_cnt, 1, -1
        write(fileid, "(I2,F8.4,16F10.4)")j,mesh_group(:,j)%flux
    enddo
    close(fileid)
    return 
    endsubroutine
    endmodule
    
    ! main program
    program main
    use data_structure
    use solver
    use GetResult
    implicit none
    type(meshtype), dimension(:, :), allocatable :: mesh_group
    type(meshtype), dimension(:, :), allocatable :: old_mesh_group
    type(regiontype), dimension(:), allocatable  :: region
    real(8) :: SIGMA_T = 1.0d0
    real(8) :: SIGMA_S = 0.5d0
    real(8) :: external_source = 1.0d0
    real(8) :: length = 21.42
    integer :: X_CNT = 100
    integer :: region_cnt = 2
    
    allocate(mesh_group(2*SN+1, 2*X_CNT+1))
    allocate(old_mesh_group(2*SN+1, 2*X_CNT+1))
    allocate(region(region_cnt))
    
    ! initialize the region parameter
    region(1)%x_lower = 0.d0
    region(1)%x_upper = 10.71d0
    region(1)%sigma_t = 0.83333d0
    region(1)%sigma_s = 0.60272d0
    region(1)%external_source = 1.0d0
    
    region(2)%x_lower = 10.71d0
    region(2)%x_upper = 21.42d0
    region(2)%sigma_t = 0.83333d0
    region(2)%sigma_s = 0.74091d0
    region(2)%external_source = 0.65d0
    
    ! PI method
    call PowerIteration(mesh_group, old_mesh_group, length, region)

    call ScalarFlux(mesh_group)
    call AngularFluxTable(mesh_group)
    
    deallocate(mesh_group)
    deallocate(old_mesh_group)
    deallocate(region)
    write(*,*) "done"
    endprogram