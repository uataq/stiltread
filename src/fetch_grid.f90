! Read grid from ARL packed data file
! Ben Fasoli

subroutine fetch_grid(path, var, nx, ny, rdata)

    character(80),     intent(in)  :: path
    character(4),      intent(in)  :: var
    integer,           intent(in)  :: nx, ny
    double precision,  intent(out) :: rdata(nx,ny)

    character(1)                   :: cpack(nx*ny)
    character(4)                   :: kvar
    character(50)                  :: label
    integer                        :: i, nxy
    real                           :: prec

    !--------------------------------------------------------------------------
    interface
    subroutine unpack(cpack, rdata, nx, ny, nexp, var1)
    character(1),      intent(in)  :: cpack(:)
    double precision,  intent(out) :: rdata(:,:)
    integer,           intent(in)  :: nx, ny, nexp
    real,              intent(in)  :: var1
    end subroutine
    end interface
    !--------------------------------------------------------------------------

    nxy = nx * ny

    ! open file skipping header record and extract CPACK from label
    open(10, file=path, recl=nxy+50, access='DIRECT', form='UNFORMATTED')

    ! loop through records until kvar string matching var is found
    do i = 1, 1000
        read(10, rec=i) label, (cpack(K), K=1, nxy)
        read(label,'(14X,A4,I4,2E14.7)') kvar,nexp,prec,var1
        if (kvar .eq. var) exit
    end do

    close(10)

    ! unpack binary cpack data to form numeric rdata matrix
    call unpack(cpack, rdata, nx, ny, nexp, var1)

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
