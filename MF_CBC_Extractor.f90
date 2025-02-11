program MF_CBC_Extractor
  !-----------------------------------------------------------------------------------------------!
  ! MF_CBC_Extractor
  ! ****************
  ! Author: Leland Scantlebury, lscantle@ucdavis.edu
  ! Date: February 2025
  !
  ! Command Line Arguments:
  ! - CBB Filename 
  ! - Starting SP of accumulation
  ! - Ending SP of accumulation (stops when it sees this number: not inclusive)
  ! - Target label (e.g., "STORAGE")
  !-----------------------------------------------------------------------------------------------!
  implicit none
  
  integer              :: ca_count, nitems, ierr, start_sp, end_sp, isp, i, row, col, lay
  integer              :: ncol, nrow, nlay, kstp, kper, nsp, method, nlist, naux, n2
  integer              :: node
  integer, allocatable :: ilayer(:,:)
  real                 :: delt, pertim, totim, value
  real, allocatable    :: read_in(:,:,:)       ! ncol, nrow, nlay
  real, allocatable    :: aux(:)
  real*8, allocatable  :: accumulator(:,:,:,:) ! nsp, ncol, nrow, nlay
  character(len=16)    :: temp, text_label, target_label
  character(len=16),allocatable    :: auxtxt(:)
  character(len=256)   :: cbbfile, outfile
  real*8, parameter    :: zero = 0.0d0
  
  ! Get input parameters
  ca_count = COMMAND_ARGUMENT_COUNT()
  if (ca_count < 4) then
    write(*,*) 'Command arguments missing - must pass CBB filename, start sp, end sp, and target label (e.g., "STORAGE")'
    stop !! Early exit
  end if
  
  call get_command_argument(1, cbbfile)
  call get_command_argument(2, temp)
  read(temp,'(i)') start_sp
  call get_command_argument(3, temp)
  read(temp,'(i)') end_sp
  call get_command_argument(4, target_label)
  target_label = trim(adjustl(target_label))
  
  ! Setup
  open(6, carriagecontrol='fortran')
  nsp                  = end_sp - start_sp
  write(outfile,'(3a)') 'MF_CBC_', trim(target_label), '.out'
  
  ! Reading CBB file
  write(*,'(2x,a)') 'Reading MODFLOW Cell-by-Cell flow file'

  open(10, file=trim(cbbfile), form='binary', status='old', iostat=ierr)
  do
    read(10, iostat=ierr) kstp, kper, text_label, ncol, nrow, nlay, method, delt, pertim, totim
    !write(*,*) kstp, kper, text_label, ncol, nrow, nlay, method, delt, pertim, totim

    ! Check if the file is still going
    if(ierr /= 0) then
      exit
    end if
    
    ! if not allocated, allocate
    if (.not.allocated(accumulator)) then
      allocate(accumulator(nsp, ncol, nrow, abs(nlay)), &
               read_in    (     ncol, nrow, abs(nlay))  )
      ! init
      accumulator(:,:,:,:) = zero
    end if
    read_in(:,:,:)       = zero
    
    select case (method)
      case(1)  ! UBDSV1
        read(10) read_in
      case(2)  ! UBDSV2
        ! Read in size
        read(10) nlist
        do i=1, nlist
          read(10) node, value
          call node_to_rcl(node, nrow, ncol, nlay, row, col, lay)
          read_in(col, row, nlay) = value
        end do
      case(3)  ! UBDSV3
        ! Array of layer relating to value, then values
        if(.not.allocated(ilayer)) allocate(ilayer(ncol, nrow))
          read(10) ilayer
          ! No way around a loop here
          do col=1, ncol
            do row=1, nrow
              read(10) read_in(col, row, ilayer(col, row))
            end do
          end do
      case(4)
        ! All values assumed to be in Lay 1 (e.g., ET)
        read(10) read_in(:,:,1)
      case(5)  ! UBUSV4
        read(10) naux
        naux = naux -1
        if (allocated(auxtxt)) deallocate(auxtxt)
        if (allocated(aux)) deallocate(aux)
        allocate(auxtxt(naux))
        allocate(aux(naux))
        if (naux>0) read(10) auxtxt(1:)
        read(10) nlist
        do i=1, nlist
          !  UBDSVB
          read(10) node, value, aux
          call node_to_rcl(node, nrow, ncol, nlay, row, col, lay)
          read_in(col, row, lay) = value
        end do
    end select

    ! If this isn't our target SP range, move forward or out
    if(kper < start_sp) then
      write(6,'("+",2x,2(a,i))') 'Looking at SP:', kper, ' TS:', kstp
      cycle
    else
      write(6,'("+",2x,2(a,i))') 'Accumulating  SP:', kper, ' TS:', kstp
    end if
    if(kper >= end_sp) exit

    ! Date range is good, is this the right budget item?
    if (index(text_label, trim(target_label)) > 0) then
      isp = kper - start_sp + 1
      accumulator(isp,:,:,:) = accumulator(isp,:,:,:) + read_in(:,:,:)
    end if
    
  end do
  close(10)
  
  ! Write file
  open(11, file=outfile, status='replace')
  
  ! Write header
  write(11, '(3a16, *(a16))') 'ROW', 'COLUMN', 'LAYER', (to_char(start_sp + i - 1), i = 1, nsp), 'TOTAL'

  ! Loop through all cells and write data
  do col = 1, ncol
    do row = 1, nrow
      do lay = 1, abs(nlay)
          write(11, '(3i16, *(es16.6))') row, col, lay, (accumulator(i, col, row, lay), i = 1, nsp), sum(accumulator(:, col, row, lay))
      end do
    end do
  end do
  
  close(11)
  
!-----------------------------------------------------------------------------------------------!  
  ! End of main program
  
  contains

!-----------------------------------------------------------------------------------------------!
! Supporting Functions
!-----------------------------------------------------------------------------------------------!

subroutine node_to_rcl(node, nrow, ncol, nlay, row, col, lay)
  implicit none
  integer, intent(in)   :: node, ncol, nrow, nlay
  integer, intent(out)  :: row, col, lay
  
  lay = ((node - 1) / (nrow * ncol)) + 1
  row = (mod(node - 1, nrow * ncol) / ncol) + 1
  col = mod(mod(node - 1, nrow * ncol), ncol) + 1
  
end subroutine node_to_rcl

!-----------------------------------------------------------------------------------------------!

function to_char(num) result(str)
  implicit none
  integer, intent(in) :: num
  character(len=16)   :: str
  character(len=10)   :: num_str  ! Temporary storage for number

  ! Convert integer to string
  write(num_str, '(i10)') num
  num_str = adjustl(num_str)  ! Remove leading spaces

  ! Concatenate "SP_" with the formatted number
  str = 'SP_' // trim(num_str)

  ! Ensure it's exactly 16 characters by right-padding with spaces
  str = adjustr(str)  ! Left-align in case of extra spaces
end function to_char

!-----------------------------------------------------------------------------------------------!
end program MF_CBC_Extractor