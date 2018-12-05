! Read grid from ARL packed data file
! Ben Fasoli

subroutine fetch_grid(path, var, nx, ny, yy, mm, dd, hh, lvl, rdata)

    ! Argument descriptions
    character(160),    intent(in)  :: path  ! path to data file
    character(4),      intent(in)  :: var  ! 4 letter code describing variable
    integer,           intent(in)  :: nx  ! number of grid cells in x dimension
    integer,           intent(in)  :: ny  ! number of grid cells in y dimension
    integer,           intent(in)  :: yy  ! desired record year
    integer,           intent(in)  :: mm  ! desired record month
    integer,           intent(in)  :: dd  ! desired record day of month
    integer,           intent(in)  :: hh  ! desired record hour of day
    integer,           intent(in)  :: lvl  ! desired vertical level (1 to nz)
    double precision,  intent(out) :: rdata(nx,ny)  ! container for output data

    character(1)                   :: cpack(nx*ny)  ! container for packed data
    character(4)                   :: kvar  ! 4 letter variable code
    character(50)                  :: label  ! label for each packed record
    integer                        :: i, nxy  ! loop counter, nx*ny
    real                           :: prec  ! real precision of packed data array

    !--------------------------------------------------------------------------
    interface
    subroutine unpack(cpack, rdata, nx, ny, nexp, var1)
    character(1),      intent(in)  :: cpack(:)
    double precision,  intent(out) :: rdata(:,:)
    integer,           intent(in)  :: nx, ny
    integer,           intent(in)  :: nexp  ! integer scaling exponent of packed data array
    real,              intent(in)  :: var1  ! real value of array at position (1,1)
    end subroutine
    end interface
    !--------------------------------------------------------------------------

    nxy = nx * ny

    ! open file skipping header record and extract CPACK from label
    open(10, file=path, recl=nxy+50, access='DIRECT', form='UNFORMATTED')

    ! loop through records until user specified record is found
    do i = 1, 99999
        read(10, rec=i, iostat=ios) label, (cpack(K), K=1, nxy)
        read(label, '(6I2,2X,A4,I4,2E14.7)') iy,im,id,ih,ic,il,kvar,nexp,prec,var1
        if ((kvar .eq. var) &
            .and. (iy .eq. yy) &
            .and. (im .eq. mm) &
            .and. (id .eq. dd) &
            .and. (ih .eq. hh) &
            .and. (il .eq. lvl)) exit
        if (ios .ne. 0) exit
    end do

    close(10)

    ! unpack binary cpack data to form numeric rdata matrix
    ! if eof, exit returning default rdata
    if (ios .eq. 0) call unpack(cpack, rdata, nx, ny, nexp, var1)

end subroutine fetch_grid


! original source: HYSPLIT v4.9
SUBROUTINE UNPACK(CPACK,RDATA,NX,NY,NEXP,VAR1)

CHARACTER(1),INTENT(IN)       :: CPACK(:)
DOUBLE PRECISION, INTENT(OUT) :: RDATA(:,:)
INTEGER,     INTENT(IN)       :: NX,NY,NEXP
REAL,        INTENT(IN)       :: VAR1
DOUBLE PRECISION              :: SCALE,VOLD

! only required when dealing with F95 compilers
! replace ICHAR below with internally defined JCHAR function
! CHARACTER MYCHR*1
! JCHAR(MYCHR)=IAND(ICHAR(MYCHR),255)
SCALE=2.0**(7-NEXP)
VOLD=VAR1
INDX=0
DO J=1,NY
    DO I=1,NX
        INDX=INDX+1
        RDATA(I,J)=(ICHAR(CPACK(INDX))-127.)/SCALE+VOLD
        VOLD=RDATA(I,J)
    END DO
    VOLD=RDATA(1,J)
END DO
END SUBROUTINE UNPACK
