c
c-----------------------------------------------------------------------
c
c ****** Diffuse a 2D scalar field on the surface of a sphere.
c
c-----------------------------------------------------------------------
c
c **********************************************************************
c
c Copyright 2021 Predictive Science Inc.
c
c Licensed under the Apache License, Version 2.0 (the "License");
c you may not use this file except in compliance with the License.
c You may obtain a copy of the License at
c
c    http://www.apache.org/licenses/LICENSE-2.0
c
c Unless required by applicable law or agreed to in writing, software
c distributed under the License is distributed on an "AS IS" BASIS,
c WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
c implied.
c See the License for the specific language governing permissions and
c limitations under the License.
c
c **********************************************************************
c
c#######################################################################
c
c ****** This tool uses modules from ZM's tools library.
c
c#######################################################################
      module ident
c
      character(*), parameter :: cname='DIFFUSE'
      character(*), parameter :: cvers='2.12.3'
      character(*), parameter :: cdate='06/02/2021'
c
      end module
c#######################################################################
      module params
c
c-----------------------------------------------------------------------
c ****** Parameters.
c-----------------------------------------------------------------------
c
      use number_types
c
      implicit none
c
      logical :: verbose
      character(512) :: infile=' '
      character(512) :: outfile=' '
      character(512) :: sourcefile=' '
      real(r_typ) :: visc=0.
      character(512) :: viscfile=' '
      logical :: viscgrid=.false.
      real(r_typ) :: viscfac=1._r_typ
      real(r_typ) :: time=1._r_typ
      logical :: pt_order=.false.
      character(16) :: filtdims=' '
      real(r_typ) :: diffuse_t=0.
      real(r_typ) :: diffuse_p=0.
      logical :: use_sts=.true.
      real(r_typ) :: stsfac=0.
c
      end module
c#######################################################################
      module mesh
c
      use number_types
c
      implicit none
c
      integer :: nt,ntm1,ntm2
      integer :: np,npm1,npm2
c
      real(r_typ) :: d2t_j1,d2t_jntm1
c
      real(r_typ), dimension(:), pointer :: t,p
      real(r_typ), dimension(:), allocatable :: dp,ph,dph,dt,th,dth
      real(r_typ), dimension(:), allocatable :: dt_i,dth_i,dp_i,dph_i
      real(r_typ), dimension(:), allocatable :: st,sth,st_i
c
      end module
c#######################################################################
      module fields
c
      use number_types
c
      implicit none
c
      real(r_typ), dimension(:,:), allocatable :: f
      real(r_typ), dimension(:,:), allocatable :: fe
      real(r_typ), dimension(:,:), allocatable :: visc_total
      real(r_typ), dimension(:,:), allocatable :: source
c
      end module
c#######################################################################
      module sts
c
      use number_types
c
      implicit none
c
      real(r_typ) :: dtime_sts
      integer*8 :: sts_s
c
      real(r_typ), dimension(:), allocatable :: sts_uj
      real(r_typ), dimension(:), allocatable :: sts_vj
      real(r_typ), dimension(:), allocatable :: sts_ubj
      real(r_typ), dimension(:), allocatable :: sts_gj
      real(r_typ), dimension(:), allocatable :: sts_b
      real(r_typ), dimension(:,:), allocatable :: y0
      real(r_typ), dimension(:,:), allocatable :: tMy0
      real(r_typ), dimension(:,:), allocatable :: Mym1
      real(r_typ), dimension(:,:), allocatable :: yjm1
      real(r_typ), dimension(:,:), allocatable :: yjm2
c
      end module
c#######################################################################
      module matrix_storage
c
      use number_types
c
      implicit none
c
      real(r_typ), dimension(:,:,:), allocatable :: coef
c
      end module
c#######################################################################
      program DIFFUSE
c
c-----------------------------------------------------------------------
c
      use ident
      use number_types
      use sds_def
      use mesh
      use fields
      use params
      use sts
      use matrix_storage
      use iso_fortran_env
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      real(r_typ), parameter :: one=1._r_typ
      real(r_typ), parameter :: two=2._r_typ
      real(r_typ), parameter :: twopi_i=0.159154943091895_r_typ
      real(r_typ) :: t1,t2,wtime
c
c-----------------------------------------------------------------------
c
      type(sds) :: s
      integer :: ierr
      integer*8 :: ntime,nout,ntime_sts,i
      real(r_typ) :: dtime,dtmax,dtime_sts_default
c
c-----------------------------------------------------------------------
c
c ****** Set the parameters.
c
      t1=wtime()
      call set_parameters
c
c ****** Set the dimensions along which to filter.
c
      if (filtdims.eq.'t') then
        diffuse_t=one
        diffuse_p=0.
      else if (filtdims.eq.'p') then
        diffuse_t=0.
        diffuse_p=one
      else if (filtdims.eq.'tp'.or.filtdims.eq.'pt') then
        diffuse_t=one
        diffuse_p=one
      else
        write (*,*)
        write (*,*) '### ERROR in ',cname,':'
        write (*,*) '### Invalid dimensions along which to diffuse:'
        write (*,*) 'Dimensions specified:'
        write (*,*) trim(filtdims)
        write (*,*) 'Dimensions allowed:'
        write (*,*) 't'
        write (*,*) 'p'
        write (*,*) 'tp'
        write (*,*) 'pt'
        call exit (1)
      end if
c
c ****** Read the input field.
c
      call read_and_check_sds (infile,s)
c
c ****** Set the coordinates.
c
      if (pt_order) then
        ntm1=s%dims(2)
        npm1=s%dims(1)
        t=>s%scales(2)%f
        p=>s%scales(1)%f
      else
        ntm1=s%dims(1)
        npm1=s%dims(2)
        t=>s%scales(1)%f
        p=>s%scales(2)%f
      end if
c
      nt=ntm1+1
      np=npm1+1
      ntm2=ntm1-1
      npm2=npm1-1
c
      if (verbose) then
        write (*,*) 'DIFFUSE'
        write (*,*) '-------'
        write (*,*)
        write (*,*) 'End time,                 tau = ',time
        write (*,*)
        write (*,*) 'Number of t mesh points, NTM1 = ',ntm1
        write (*,*) 'Number of p mesh points, NPM1 = ',npm1
        write (*,*)
        if (diffuse_t.gt.0.) then
          write (*,*) 'Field diffusion in t: YES'
        else
          write (*,*) 'Field diffusion in t: NO'
        end if
        if (diffuse_p.gt.0.) then
          write (*,*) 'Field diffusion in p: YES'
        else
          write (*,*) 'Field diffusion in p: NO'
        end if
      end if
c
c ****** Get the mesh quantities.
c
      call set_mesh
c
c ****** Define the viscosity.
c
      call load_viscosity
c
c ****** Read the source term.
c
      call load_source
c
c ****** Load the field.
c
      call load_field (s)
c
c ****** Set unchanging polar boundary values.
c
      d2t_j1=diffuse_t*two*(visc_total(1,1)+
     &            visc_total(2,1))*twopi_i*dt_i(1)*dt_i(1)

      d2t_jntm1=diffuse_t*two*(visc_total(ntm1,1)+
     &               visc_total(ntm2,1))*twopi_i*dt_i(ntm1)*dt_i(ntm1)
c
c ****** Load matrix for diffusion ax.
c
      call load_matrix
c
c ****** Get the maximum timestep for Euler explicit stable diffusion.
c
      call get_dtmax (dtmax)
c
c ****** Find the number of time steps needed and adjust dtime so
c        that the end time is correct.
c
      dtime=dtmax
c
      ntime=CEILING(time/dtime,8)
      dtime=time/ntime
c
c ****** Do not use STS if there are too few exp steps required.
c
      if (ntime.le.500) then
        use_sts=.false.
        write (*,*)
        write (*,*) 'NOTICE: STS not used because N<=500. Using exp.'
      end if
c
      if (use_sts) then
c
c ****** Set dtime_sts so that there are 60 super steps.
c ****** This ensures that the high modes are damped.
c
        dtime_sts=time/60._r_typ
c
c ****** Overide STS time step if user chooses to,
c ****** but not so there is under 60 super steps.
c
        if (stsfac.ne.0) then
          dtime_sts_default=dtime_sts
          dtime_sts=min(stsfac*dtime,dtime_sts_default)
          if (dtime_sts.eq.dtime_sts_default) then
            write (*,*)
            write (*,*) 'NOTICE: Your choice of STSFAC was too large.'
            write (*,*) 'Lowering it for accuracy.'
          end if
        end if
c
c ****** Set number of supersteps, and recompute step size to be exact.
c
        ntime_sts=CEILING(time/dtime_sts,8)
        dtime_sts=time/ntime_sts
c
      end if
c
c ****** Diffuse the field using Euler explicit advance.
c
      if (.not.use_sts) then
        if (verbose) then
          nout=CEILING(ntime/100._r_typ,8)
          write (*,*)
          write (*,*) '*** Using 1st-order Euler Time-stepping ***'
          write (*,*)
          write (*,*) 'Time-step (for stability) (dt_exp) = ',dtime
          write (*,*) 'Number of time steps needed (Nexp) = ',ntime
          write (*,*)
          flush(OUTPUT_UNIT)
        end if
!$acc enter data copyin(source,visc_total,f,st_i,sth,dp_i,
!$acc&                  dph_i,dt_i,dth_i,dp)
!$acc enter data create(fe)
        do i=1,ntime
          call diffuse_step (dtime)
          if (verbose.and.MOD(i,nout).eq.0) then
            write(*,*) 'Computed step #',i,' of ',ntime
            flush(OUTPUT_UNIT)
          endif
        enddo
!$acc exit data copyout(f)
!$acc exit data delete(source,visc_total,fe,st_i,sth,dp_i,
!$acc&                 dph_i,dt_i,dth_i,dp)
c
      else
c
c ****** Diffuse the field using super-time-stepping (RKL2)
c ****** from Meyer, et. al. J. Comp. Phys. 257 (2014) 594-626.
c
        if (verbose) then
          write (*,*)
        write (*,*) '*** Using 2nd-order Super-time-stepping (RKL2) ***'
          write (*,*)
        end if
c
        call load_sts (dtime)
c
        if (verbose) then
         nout=CEILING(ntime_sts/100._r_typ,8)
        if (stsfac.ne.0) then
          write (*,*) 'WARNING: User override of dt_sts enabled!'
          write (*,*) 'Default super-time-step   (dt_sts) = ',
     &                                                 dtime_sts_default
        end if
         write (*,*) 'Super-time-step used      (dt_sts) = ',dtime_sts
         write (*,*) 'Euler time-step           (dt_exp) = ',dtime
         write (*,*) 'STS factor         (dt_sts/dt_exp) = ',
     &                                                dtime_sts/dtime
         write (*,*) 'Number of iterations/superstep (s) = ',sts_s
         write (*,*) 'Number of super-steps       (Nsts) = ',ntime_sts
         write (*,*) 'Total STS iterations       (Niter) = ',
     &                                                 ntime_sts*sts_s
         write (*,*) 'Euler iterations needed  (Nexp)    = ',ntime
         write (*,*) 'Potential max speedup due to STS   = ',
     &                             ntime/(one*ntime_sts*sts_s),'X'
         write (*,*)
c
        flush(OUTPUT_UNIT)
        endif
c
c ****** Start outer super-time-step loop.
c
!$acc enter data copyin(source,coef,f,dp,sts_uj,sts_vj,sts_ubj,sts_gj)
!$acc enter data create(y0,tMy0,yjm1,yjm2,Mym1)
        do i=1,ntime_sts
          call diffuse_step_sts
          if (verbose) then
            if (verbose.and.MOD(i,nout).eq.0) then
              write(*,*) 'Computed step #',i,' of ',ntime_sts
              flush(OUTPUT_UNIT)
            endif
          endif
        enddo
!$acc exit data copyout(f)
!$acc exit data delete(source,coef,dp,sts_uj,sts_vj,sts_ubj,sts_gj)
!$acc exit data delete(y0,tMy0,yjm1,yjm2,Mym1)
      end if
c
c ****** Load the field into the SDS.
c
      if (pt_order) then
        s%f(:,:,1)=transpose(f)
      else
        s%f(:,:,1)=f
      end if
c
c ****** Write the SDS.
c
      call wrhdf (outfile,s,ierr)
c
      if (ierr.ne.0) then
        write (*,*)
        write (*,*) '### ERROR in ',cname,':'
        write (*,*) '### Could not write the output data set.'
        write (*,*) 'IERR (from WRHDF) = ',ierr
        write (*,*) 'File name: ',trim(outfile)
        call exit (1)
      else
        write(*,*) ' '
        write(*,*) 'Smoothed map written out to ',outfile,'.'
        write(*,*)
      endif
c
      t2=wtime()
      write(*,*) "Wall clock time (seconds): ",t2-t1
      call exit (0)

c
      end
c#######################################################################
      subroutine load_matrix
c
c-----------------------------------------------------------------------
c
c ****** Load matrix coefs.
c
c-----------------------------------------------------------------------
c
      use number_types
      use mesh
      use fields
      use params
      use matrix_storage
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      integer :: j,k
c
c-----------------------------------------------------------------------
c
c ****** Allocate coef array.
c
      allocate (coef(2:ntm2,1:npm2,5))
      coef(:,:,:)=0.
c
c ****** Set coef for internal points and phi boundary point at k=1.
c
      do k=1,npm2
        do j=2,ntm2
          coef(j,k,1)=diffuse_p*visc_total(j,k)*dph_i(k  )
     &                *dp_i(k)*st_i(j)*st_i(j)
          coef(j,k,2)=diffuse_t*visc_total(j,k)*dth_i(j  )
     &                *dt_i(j)*st_i(j)*sth(j )
          coef(j,k,4)=diffuse_t*visc_total(j+1,k)*dth_i(j+1)
     &                *dt_i(j)*st_i(j)*sth(j+1)
          coef(j,k,5)=diffuse_p*visc_total(j,k+1)*dph_i(k+1)
     &                *dp_i(k)*st_i(j)*st_i(j )
c
          coef(j,k,3)=-(coef(j,k,1)+coef(j,k,2)+coef(j,k,4)+coef(j,k,5))
        enddo
      enddo
c
      end subroutine
c#######################################################################
      subroutine get_dtmax (dtime_exp)
c
c-----------------------------------------------------------------------
c
c ****** Get the explicit Euler time step limit for thermal conduction.
c
c-----------------------------------------------------------------------
c
      use matrix_storage
      use mesh
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      real(r_typ) :: dtime_exp
c
c-----------------------------------------------------------------------
c
      real(r_typ), parameter :: two=2._r_typ
c
c-----------------------------------------------------------------------
c
      real(r_typ), parameter :: safety=0.95_r_typ
c
c-----------------------------------------------------------------------
c
      integer :: i,j,k,d
      real(r_typ) :: max_eig,gersh_rad
c
c-----------------------------------------------------------------------
c
c *** Estimate maximum eigenvalue using Gershgorin disks:
c
      max_eig=0.
c
      do k=1,npm2
        do j=2,ntm2
            gersh_rad=0.
            do d=1,5
              gersh_rad=gersh_rad+abs(coef(j,k,d))
            enddo
            max_eig=max(gersh_rad,max_eig)
        enddo
      enddo
c
c *** Compute the Euler time-step bound.
c
      dtime_exp=two/max_eig
c
c *** Apply safety factor.
c
      dtime_exp=safety*dtime_exp
c
      end subroutine
c#######################################################################
      subroutine diffuse_step_sts
c
c-----------------------------------------------------------------------
c
c ****** Diffuse the field by one time step using STS.
c
c-----------------------------------------------------------------------
c
      use number_types
      use mesh
      use fields
      use params
      use sts
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      real(r_typ), parameter :: one=1.0_r_typ
c
c-----------------------------------------------------------------------
c
      integer :: j,k
      integer*8 :: i
c
c-----------------------------------------------------------------------
c
      call ax(f,tMy0)
c
      do concurrent (k=1:npm1, j=1:ntm1)
        y0(j,k)=f(j,k)
        yjm2(j,k)=f(j,k)
        tMy0(j,k)=dtime_sts*tMy0(j,k)
        yjm1(j,k)=f(j,k)+sts_ubj(1)*tMy0(j,k)
      enddo
c
c ****** Inner s-step loop
c
      do i=2,sts_s
c
        call ax(yjm1,Mym1)
c
        do concurrent (k=1:npm1, j=1:ntm1)
          f(j,k)=sts_uj(i)*yjm1(j,k)+sts_vj(i)*yjm2(j,k)+
     &     (one-sts_uj(i)-sts_vj(i))*y0(j,k)+
     &     sts_ubj(i)*dtime_sts*Mym1(j,k)+sts_gj(i)*tMy0(j,k)
          yjm2(j,k)=yjm1(j,k)
          yjm1(j,k)=f(j,k)
        enddo
      enddo
      end subroutine
c#######################################################################
      subroutine load_sts (dtime)
c
c-----------------------------------------------------------------------
c
c ****** Set up parameters and coefficient arrays for STS advance.
c ****** This uses the RKL2 2nd-order STS as given in
c ****** Meyer, et. al. J. Comp. Phys. 257 (2014) 594-626
c
c ****** Note that dtime_sts must be set before calling this routine.
c
c-----------------------------------------------------------------------
c
      use number_types
      use params
      use mesh
      use sts
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      real(r_typ), parameter :: one=1._r_typ
      real(r_typ), parameter :: two=2._r_typ
      real(r_typ), parameter :: three=3._r_typ
      real(r_typ), parameter :: four=4._r_typ
      real(r_typ), parameter :: six=6._r_typ
      real(r_typ), parameter :: nine=9._r_typ
      real(r_typ), parameter :: sixteen=16._r_typ
      real(r_typ), parameter :: half=0.5_r_typ
      integer*8,   parameter :: two_int=2
c
c-----------------------------------------------------------------------
c
      real(r_typ) :: dtime
c
c-----------------------------------------------------------------------
c
      real(r_typ) :: sts_s_real,bj_bjm2,bj_bjm1
      integer*8 :: j
c
c-----------------------------------------------------------------------
c
c ****** Compute number of iterations per super-step.
c
      sts_s_real=half*(sqrt(nine+sixteen*(dtime_sts/dtime))-one)
      sts_s=ceiling(sts_s_real)
c
c ****** Make sure s is odd for better stability.
c
      if(MOD(sts_s,2).eq.0) then
        sts_s=sts_s+1
      endif
c
c ****** Allocate super-time-step coefficent arrays.
c
      allocate (sts_uj(sts_s))
      allocate (sts_vj(sts_s))
      allocate (sts_ubj(sts_s))
      allocate (sts_gj(sts_s))
      allocate (sts_b(sts_s))
c
c ****** Compute super-time-step coefficents.
c
      sts_b(1)=one/three
      sts_b(2)=one/three

      sts_uj(1)=-9999._r_typ
      sts_vj(1)=-9999._r_typ
      sts_ubj(1)=four/(one*three*(sts_s*sts_s+sts_s-two_int))
      sts_gj(1)=-9999._r_typ
c
      sts_uj(2)=three/two
      sts_vj(2)=-one/two
      sts_ubj(2)=six/(sts_s*sts_s+sts_s-two_int)
      sts_gj(2)=-four/(sts_s*sts_s+sts_s-two_int)

      do j=3,sts_s
        sts_b(j)=(j*j+j-two)/(two*j*(j+1))
        bj_bjm1=sts_b(j)/sts_b(j-1)
        bj_bjm2=sts_b(j)/sts_b(j-2)
c
        sts_uj(j)=bj_bjm1*(two-one/j)
        sts_vj(j)=-bj_bjm2*(one-one/j)
        sts_ubj(j)=sts_uj(j)*four/(sts_s*sts_s+sts_s-two_int)
        sts_gj(j)=-(one-sts_b(j-1))*sts_ubj(j)
      enddo
c
c ****** Leave the following commented code here for now.
c
c      do j=3,sts_s
c        t_int=(j+2)*(2*j-1)*(j-1)*(j-1)
c        b_int=j*(j+1)*(j+1)*(j-2)
c        t_re=one*t_int
c        b_re=one*b_int
c        sts_uj(j)=t_re/b_re
c
c        if(j.eq.3) then
c          sts_vj(3)=-five/six
c        else
c          t_int=(j-2)*(j+2)*(j-1)*(j-1)*(j-1)
c          b_int=j*j*j*(j+1)*(j-3)
c          t_re=one*t_int
c          b_re=one*b_int
c          sts_vj(j)=-t_re/b_re
c        endif
c
c        t_int=(4*(j+2))*(2*j-1)*(j-1)*(j-1)
c        b_int=j*(j+1)*(j+1)*(sts_s+2)*(sts_s-1)*(j-2)
c        t_re=one*t_int
c        b_re=one*b_int
c        sts_ubj(j)=t_re/b_re
c
c        t_int=2*(j-1)*(2*j-1)*(j+2)*(j*j-j+2)
c        b_int=(j+1)*(j+1)*(j-2)*(sts_s+2)*(sts_s-1)*j*j
c        t_re=one*t_int
c        b_re=one*b_int
c        sts_gj(j)=-t_re/b_re
c      enddo
c
c ****** Allocate scratch arrays for STS advance.
c
      allocate (y0(ntm1,npm1))
      allocate (tMy0(ntm1,npm1))
      allocate (Mym1(ntm1,npm1))
      allocate (yjm1(ntm1,npm1))
      allocate (yjm2(ntm1,npm1))
c
      end subroutine
c#######################################################################
      subroutine read_and_check_sds (fname,s)
c
c-----------------------------------------------------------------------
c
c ****** Read a 2D SDS from HDF file FNAME into structure S.
c
c-----------------------------------------------------------------------
c
      use ident
      use sds_def
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      character(*) :: fname
      type(sds) :: s
c
c-----------------------------------------------------------------------
c
      integer :: ierr
c
c-----------------------------------------------------------------------
c
c ****** Read the SDS.
c
      call rdhdf (fname,s,ierr)
c
      if (ierr.ne.0) then
        write (*,*)
        write (*,*) '### ERROR in ',cname,':'
        write (*,*) '### Could not read the requested data set.'
        write (*,*) 'IERR (from RDHDF) = ',ierr
        write (*,*) 'File name: ',trim(fname)
        call exit (1)
      end if
c
c ****** Check that it is a 2D SDS.
c
      if (s%ndim.ne.2) then
        write (*,*)
        write (*,*) '### ERROR in ',cname,':'
        write (*,*) '### The file does not contain a 2D field.'
        write (*,*) '### Number of dimensions = ',s%ndim
        write (*,*) 'File name: ',trim(fname)
        call exit (1)
      end if
c
c ****** Check that the SDS has scales.
c
      if (.not.s%scale) then
        write (*,*)
        write (*,*) '### ERROR in ',cname,':'
        write (*,*) '### The data set does not contain scales.'
        write (*,*) 'File name: ',trim(fname)
        call exit (1)
      end if
c
      return
      end
c#######################################################################
      subroutine set_mesh
c
c-----------------------------------------------------------------------
c
c ****** Compute the mesh quantities.
c
c-----------------------------------------------------------------------
c
      use ident
      use number_types
      use mesh
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      real(r_typ), parameter :: half=.5_r_typ
      real(r_typ), parameter :: one=1.0_r_typ
      real(r_typ), parameter :: pi=3.14159265358979323846_r_typ
      real(r_typ), parameter :: twopi=6.28318530717958647692_r_typ
c
c-----------------------------------------------------------------------
c
c ****** Tolerance for precision of coordinates.
c
      real(r_typ), parameter :: eps=1.e-6_r_typ
c
c-----------------------------------------------------------------------
c
      integer :: i
c
c-----------------------------------------------------------------------
c
c ****** Check that the mesh covers a complete spherical surface.
c
      if (abs(t(1)).gt.eps.or.
     &    abs(t(ntm1)-pi).gt.eps.or.
     &    abs(p(1)).gt.eps.or.
     &    abs(p(npm1)-twopi).gt.eps) then
        write (*,*)
        write (*,*) '### ERROR in ',cname,':'
        write (*,*) '### Anomaly in data file coordinates:'
        write (*,*)
        write (*,*) 'Expected t range:'
        write (*,*) 'Min: ',0.
        write (*,*) 'Max: ',pi
        write (*,*) 'Actual t range:'
        write (*,*) 'Min: ',t(1)
        write (*,*) 'Max: ',t(ntm1)
        write (*,*)
        write (*,*) 'Expected p range:'
        write (*,*) 'Min: ',0.
        write (*,*) 'Max: ',twopi
        write (*,*) 'Actual p range:'
        write (*,*) 'Min: ',p(1)
        write (*,*) 'Max: ',p(npm1)
        call exit (1)
      end if
c
      allocate (dt(ntm1))
      allocate (dt_i(ntm1))
      allocate (th(nt))
      allocate (dth(nt))
      allocate (dth_i(nt))
      allocate (st(ntm1))
      allocate (st_i(ntm1))
      allocate (sth(nt))
c
      allocate (dp(npm1))
      allocate (dp_i(npm1))
      allocate (ph(np))
      allocate (dph(np))
      allocate (dph_i(np))
c
      do i=2,ntm1
        th(i)=half*(t(i)+t(i-1))
        dth(i)=t(i)-t(i-1)
      enddo
      th(1)=th(2)-dth(2)
      th(nt)=th(ntm1)+dth(ntm1)
      dth(1)=dth(2)
      dth(nt)=dth(ntm1)
      dth_i=one/dth
c
      do i=1,ntm1
        dt(i)=th(i+1)-th(i)
        dt_i(i)=one/dt(i)
      enddo
c
      st=sin(t)
      st_i(2:ntm2)=one/st(2:ntm2)
      sth=sin(th)
c
      do i=2,npm1
        ph(i)=half*(p(i)+p(i-1))
        dph(i)=p(i)-p(i-1)
      enddo
      ph(1)=ph(npm1)-twopi
      ph(np)=ph(2)+twopi
      dph(1)=dph(npm1)
      dph(np)=dph(2)
      dph_i=one/dph
c
      do i=1,npm1
        dp(i)=ph(i+1)-ph(i)
        dp_i(i)=one/dp(i)
      enddo
c
      return
      end
c#######################################################################
      subroutine load_field (s)
c
c-----------------------------------------------------------------------
c
c ****** Allocate local arrays to hold the scalar field and
c ****** load the field from the SDS structure S.
c
c-----------------------------------------------------------------------
c
      use number_types
      use sds_def
      use mesh
      use fields
      use params
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      type(sds) :: s
c
c-----------------------------------------------------------------------
c
      real(r_typ), parameter :: half=.5_r_typ
c
c-----------------------------------------------------------------------
c
      real(r_typ) :: fn1,fn2,fs1,fs2
c
c-----------------------------------------------------------------------
c
c ****** Allocate memory.
c
      allocate (f(ntm1,npm1))
      allocate (fe(ntm1,0:npm1))
c
c ****** Load the field in (t,p) order into the main field array.
c
      if (pt_order) then
        f=transpose(s%f(:,:,1))
      else
        f=s%f(:,:,1)
      end if
c
c ****** Enforce periodicity.
c
      f(:,1)=half*(f(:,1)+f(:,npm1))
      f(:,npm1)=f(:,1)
c
c ****** Get the m=0 components near the poles.
c
      call get_m0 (f,fn1,fn2,fs1,fs2)
c
c ****** Set the pole values to have only an m=0 component.
c
      f(1,:)=fn1
      f(ntm1,:)=fs1
c
      return
      end
c#######################################################################
      subroutine load_viscosity
c
c-----------------------------------------------------------------------
c
c ****** Define the total viscosity, the sum of a uniform part
c ****** given by VISC, and a part defined by the 2D SDS in
c ****** file VISCFILE.
c
c-----------------------------------------------------------------------
c
      use ident
      use number_types
      use sds_def
      use mesh
      use fields
      use params
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      type(sds) :: v
c
c-----------------------------------------------------------------------
c
      real(r_typ), parameter :: zero=0._r_typ
      real(r_typ), parameter :: one=1._r_typ
      real(r_typ), parameter :: two=2._r_typ
      real(r_typ), parameter :: twopi=6.28318530717958647692_r_typ
c
c-----------------------------------------------------------------------
c
      integer :: j,k,ierr
      real(r_typ) :: fn1,fs1
c
      integer :: nft,nfp
      real(r_typ), dimension(:), pointer :: tf,pf
      real(r_typ), dimension(:,:), pointer :: vf
      real(r_typ), dimension(:,:), allocatable :: visc_fint
c
c-----------------------------------------------------------------------
c
c ****** Allocate memory for the total viscosity.
c
      allocate (visc_total(nt,np))
c
c ****** Set default uniform viscosity if not set,
c
      if (visc.le.0.) then
        if (viscfile.eq.' '.and..not.viscgrid) then
          visc=one
        else
          visc=zero
        end if
      end if
c
c ****** Set the viscosity to the uniform value.
c
      visc_total(:,:)=visc
c
      if (verbose) then
        write (*,*)
        write (*,*) 'Uniform viscosity = ',visc
      end if
c
c ****** Read the viscosity file, if it was specified.
c
      if (viscfile.ne.' ') then
c
        call read_and_check_sds (viscfile,v)
c
c ****** Load the viscosity into an array in (t,p) order.
c
        if (pt_order) then
          nft=v%dims(2)
          nfp=v%dims(1)
          tf=>v%scales(2)%f
          pf=>v%scales(1)%f
        else
          nft=v%dims(1)
          nfp=v%dims(2)
          tf=>v%scales(1)%f
          pf=>v%scales(2)%f
        end if
c
        allocate (vf(nft,nfp))
c
        if (pt_order) then
          vf=transpose(v%f(:,:,1))
        else
          vf=v%f(:,:,1)
        end if
c
c ****** Interpolate the viscosity onto the half mesh (th,ph).
c
        allocate (visc_fint(nt,np))
c
        call intrp2d (nft,nfp,tf,pf,vf,nt,np,th,ph,visc_fint,ierr)
        if (ierr.ne.0) go to 900
c
c ****** Enforce periodicity.
c
        visc_fint(:, 1)=visc_fint(:,npm1)
        visc_fint(:,np)=visc_fint(:,   2)
c
c ****** Set the pole value to only have an m=0 component.
c
        fn1=0.
        fs1=0.
        do k=2,npm1
          fn1=fn1+visc_fint(2,k)*dph(k)
          fs1=fs1+visc_fint(ntm1,k)*dph(k)
        enddo
        fn1=fn1/twopi
        fs1=fs1/twopi
c
        visc_fint( 1,:)=two*fn1-visc_fint(   2,:)
        visc_fint(nt,:)=two*fs1-visc_fint(ntm1,:)
c
        if (verbose) then
          write (*,*)
          write (*,*) 'Viscosity from file: ',trim(viscfile)
          write (*,*) 'Minimum value = ',minval(visc_fint)
          write (*,*) 'Maximum value = ',maxval(visc_fint)
        end if
c
c ****** Add the file viscosity to the uniform value.
c
        visc_total=visc_total+visc_fint
c
        deallocate (visc_fint)
        deallocate (vf)
        deallocate (v%scales(1)%f)
        deallocate (v%scales(2)%f)
        deallocate (v%f)
c
      end if

      if (viscgrid) then
        do k=1,np
          do j=1,nt
            visc_total(j,k)=visc_total(j,k)
     &                      +(dth(j)**2+(dph(k)*sth(j))**2)
          enddo
        enddo

        if (verbose) then
          write (*,*)
          write (*,*) 'Grid-based viscosity is activated.'
        end if

      end if
c
      if (verbose) then
        write (*,*)
        write (*,*) 'Viscosity scale factor: ',viscfac
      end if
c
      visc_total(:,:)=viscfac*visc_total(:,:)
c
      if (verbose) then
        write (*,*)
        write (*,*) 'Viscosity used (total): '
        write (*,*) 'Minimum value = ',MINVAL(visc_total)
        write (*,*) 'Maximum value = ',MAXVAL(visc_total)
      end if
c
      return
c
c ****** Error exit: interpolation error.
c
  900 continue
c
      write (*,*)
      write (*,*) '### ERROR in LOAD_VISCOSITY:'
      write (*,*) '### The scales in the viscosity file are'//
     &            ' not monotonically increasing.'
      write (*,*) 'File name: ',trim(viscfile)
      call exit (1)
c
      return
      end
c#######################################################################
      subroutine load_source
c
c-----------------------------------------------------------------------
c
c ****** Define the source term for the diffusion equation.
c
c ****** This is read in from the file SOURCEFILE if it is not blank.
c
c-----------------------------------------------------------------------
c
      use ident
      use number_types
      use sds_def
      use mesh
      use fields
      use params
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      type(sds) :: s
c
c-----------------------------------------------------------------------
c
      real(r_typ), parameter :: twopi=6.28318530717958647692_r_typ
      real(r_typ), parameter :: half=.5_r_typ
c
c-----------------------------------------------------------------------
c
      integer :: k,ierr
      real(r_typ) :: fn1,fs1
c
      integer :: nft,nfp
      real(r_typ), dimension(:), pointer :: tf,pf
      real(r_typ), dimension(:,:), pointer :: sf
c
c-----------------------------------------------------------------------
c
c ****** Allocate memory for the source term.
c
      allocate (source(ntm1,npm1))
c
      source=0.
c
c ****** Read the source file if it was specified.
c
      if (sourcefile.ne.' ') then
c
        call read_and_check_sds (sourcefile,s)
c
c ****** Load the source term into an array in (t,p) order.
c
        if (pt_order) then
          nft=s%dims(2)
          nfp=s%dims(1)
          tf=>s%scales(2)%f
          pf=>s%scales(1)%f
        else
          nft=s%dims(1)
          nfp=s%dims(2)
          tf=>s%scales(1)%f
          pf=>s%scales(2)%f
        end if
c
        allocate (sf(nft,nfp))
c
        if (pt_order) then
          sf=transpose(s%f(:,:,1))
        else
          sf=s%f(:,:,1)
        end if
c
c ****** Interpolate the source term onto the main mesh (t,p).
c
        call intrp2d (nft,nfp,tf,pf,sf,ntm1,npm1,t,p,source,ierr)
        if (ierr.ne.0) go to 900
c
c ****** Enforce periodicity.
c
        source(:,   1)=half*(source(:,1)+source(:,npm1))
        source(:,npm1)=source(:,1)
c
c ****** Set the pole value to only have an m=0 component.
c
        fn1=0.
        fs1=0.
        do k=1,npm2
          fn1=fn1+source(   1,k)*dp(k)
          fs1=fs1+source(ntm1,k)*dp(k)
        enddo
        fn1=fn1/twopi
        fs1=fs1/twopi
c
        source(   1,:)=fn1
        source(ntm1,:)=fs1
c
        if (verbose) then
          write (*,*)
          write (*,*) 'A source term was read in from file: ',
     &                trim(sourcefile)
          write (*,*) 'Minimum value = ',minval(source)
          write (*,*) 'Maximum value = ',maxval(source)
        end if
c
        deallocate (sf)
        deallocate (s%scales(1)%f)
        deallocate (s%scales(2)%f)
        deallocate (s%f)
c
      end if
c
      return
c
c ****** Error exit: interpolation error.
c
  900 continue
c
      write (*,*)
      write (*,*) '### ERROR in LOAD_SOURCE:'
      write (*,*) '### The scales in the source term file are'//
     &            ' not monotonically increasing.'
      write (*,*) 'File name: ',trim(sourcefile)
      call exit (1)
c
      return
      end
c#######################################################################
      subroutine get_dtmax_old (dtmax)
c
c-----------------------------------------------------------------------
c
c ****** Get the maximum time step that can be used for stable
c ****** (explicit) diffusion.
c
c-----------------------------------------------------------------------
c
      use number_types
      use mesh
      use fields
      use params
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      real(r_typ) :: dtmax
c
c-----------------------------------------------------------------------
c
      real(r_typ), parameter :: quarter=.25_r_typ
      real(r_typ), parameter :: safety=0.95_r_typ
c
c-----------------------------------------------------------------------
c
      integer :: j,k
      real(r_typ) :: nuksq
c
c-----------------------------------------------------------------------
c
      dtmax=huge(dtmax)
      do k=2,npm1
        do j=2,ntm1
          nuksq=visc_total(j,k)*( diffuse_t/dth(j)**2
     &                           +diffuse_p/(sth(j)*dph(k))**2)
          dtmax=min(quarter/nuksq,dtmax)
        enddo
      enddo
      dtmax=safety*dtmax
c
      return
      end
c#######################################################################
      subroutine diffuse_step (dtime)
c
c-----------------------------------------------------------------------
c
c ****** Diffuse the field by one time step.
c
c-----------------------------------------------------------------------
c
      use number_types
      use mesh
      use fields
      use params
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      real(r_typ) :: dtime
c
c-----------------------------------------------------------------------
c
      integer :: j,k
      real(r_typ) :: fn2_fn1,fs2_fs1
      real(r_typ) :: d2t,d2p
c
c-----------------------------------------------------------------------
c
      fn2_fn1=0.
      fs2_fs1=0.
c
c ****** Save current x to fe (old).
c
      do concurrent (k=1:npm1, j=1:ntm1)
        fe(j,k)=f(j,k)
      enddo
c
c ****** Get the m=0 components near the poles.
c
!$omp parallel do default(shared) private(k) reduction(+:fn2_fn1,
!$omp&                                                  fs2_fs1) 
!$acc parallel loop default(present) reduction(+:fn2_fn1,fs2_fs1)
      do k=1,npm2
        fn2_fn1=fn2_fn1+(f(2   ,k)-f(1   ,k))*dp(k)
        fs2_fs1=fs2_fs1+(f(ntm2,k)-f(ntm1,k))*dp(k)
      enddo
!$omp end parallel do
c
c ****** Compute y=Ax.
c
c ****** Compute boundary points.
c
      do concurrent (j=2:ntm2)
        d2t=( visc_total(j+1,1)*sth(j+1)*(fe(j+1,1)-fe(j,1))*dth_i(j+1)
     &       -visc_total(j  ,1)*sth(j  )*(fe(j,1)-fe(j-1,1))*dth_i(j  )
     &      )*st_i(j)*dt_i(j)
        d2p=( visc_total(j,2)*(fe(j,2)-fe(j,1   ))*dph_i(2)
     &       -visc_total(j,1)*(fe(j,1)-fe(j,npm2))*dph_i(1)
     &      )*dp_i(1)*st_i(j)*st_i(j)
c
       f(j,1)=fe(j,1)+dtime*(source(j,1)+(diffuse_t*d2t+diffuse_p*d2p))
       f(j,npm1)=f(j,1)
      enddo
c
      do concurrent (k=1:npm1)
        f(   1,k)=fe(1   ,k)+dtime*(source(   1,k)+d2t_j1*fn2_fn1)
        f(ntm1,k)=fe(ntm1,k)+dtime*(source(ntm1,k)+d2t_jntm1*fs2_fs1)
      enddo
c
c ****** Compute inner points.
c
      do concurrent (k=2:npm2, j=2:ntm2)
        d2p=( visc_total(j,k+1)*(fe(j,k+1)-fe(j,k  ))*dph_i(k+1)
     &       -visc_total(j,k  )*(fe(j,k  )-fe(j,k-1))*dph_i(k  )
     &       )*dp_i(k)*st_i(j)*st_i(j)
        d2t=( visc_total(j+1,k)*sth(j+1)
     &                         *(fe(j+1,k)-fe(j  ,k))*dth_i(j+1)
     &        -visc_total(j,k)*sth(j  )
     &                         *(fe(j  ,k)-fe(j-1,k))*dth_i(j  )
     &        )*st_i(j)*dt_i(j)
c
        f(j,k)=fe(j,k)+dtime*(source(j,k)+
     &                        (diffuse_t*d2t+diffuse_p*d2p))
      enddo
c
      return
      end
c#######################################################################
      subroutine ax (x,y)
c
c-----------------------------------------------------------------------
c
c ****** Apply the diffuse operator A as y=Ax.
c
c-----------------------------------------------------------------------
c
      use number_types
      use mesh
      use fields
      use params
      use matrix_storage
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      integer :: j,k
      real(r_typ) :: fn2_fn1,fs2_fs1
      real(r_typ), dimension(ntm1,npm1) :: x,y
c
c-----------------------------------------------------------------------
c
c ****** Compute y=Ax.
c
      fn2_fn1=0.
      fs2_fs1=0.
c
c ****** Compute inner points.
c
      do concurrent (k=2:npm2, j=2:ntm2)
        y(j,k)=source(j,k)
     &        +coef(j,k,1)*x(j,  k-1)
     &        +coef(j,k,2)*x(j-1,k  )
     &        +coef(j,k,3)*x(j  ,k  )
     &        +coef(j,k,4)*x(j+1,k  )
     &        +coef(j,k,5)*x(j,  k+1)
      enddo
c
c ****** Compute boundary points.
c
c ****** Get the m=0 components near the poles.
c
!$omp parallel do default(shared) private(k) reduction(+:fn2_fn1, 
!$omp&                                                  fs2_fs1) 
!$acc parallel loop default(present) reduction(+:fn2_fn1,fs2_fs1)
      do k=1,npm2
        fn2_fn1=fn2_fn1+(x(2   ,k)-x(1   ,k))*dp(k)
        fs2_fs1=fs2_fs1+(x(ntm2,k)-x(ntm1,k))*dp(k)
      enddo
!$omp end parallel do
c
       do concurrent (j=2:ntm2)
         y(j,1)=source(j,1)
     &         +coef(j,1,1)*x(j  ,npm2)
     &         +coef(j,1,2)*x(j-1,1)
     &         +coef(j,1,3)*x(j  ,1)
     &         +coef(j,1,4)*x(j+1,1)
     &         +coef(j,1,5)*x(j  ,2)
c
         y(j,npm1)=y(j,1)
       enddo
c
       do concurrent (k=1:npm1)
         y(   1,k)=source(   1,k)+d2t_j1*fn2_fn1
         y(ntm1,k)=source(ntm1,k)+d2t_jntm1*fs2_fs1
       enddo
c
      return
      end
c#######################################################################
      subroutine get_m0 (f,fn1,fn2,fs1,fs2)
c
c-----------------------------------------------------------------------
c
c ****** Get the m=0 component of the field F near the North and
c ****** South poles.
c
c-----------------------------------------------------------------------
c
      use ident
      use number_types
      use mesh
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      real(r_typ), dimension(ntm1,npm1) :: f
c
c-----------------------------------------------------------------------
c
      real(r_typ), parameter :: twopi=6.28318530717958647692_r_typ
c
c-----------------------------------------------------------------------
c
      real(r_typ) :: fn1,fn2,fs1,fs2
c
c-----------------------------------------------------------------------
c
      integer :: k
c
c-----------------------------------------------------------------------
c
      fn1=0.
      fn2=0.
      fs1=0.
      fs2=0.
c
      do k=1,npm2
        fn1=fn1+f(1,k)*dp(k)
        fn2=fn2+f(2,k)*dp(k)
        fs1=fs1+f(ntm1,k)*dp(k)
        fs2=fs2+f(ntm2,k)*dp(k)
      enddo
c
      fn1=fn1/twopi
      fn2=fn2/twopi
      fs1=fs1/twopi
      fs2=fs2/twopi
c
      return
      end
c#######################################################################
      subroutine intrp2d (nxi,nyi,xi,yi,fi,nx,ny,x,y,f,ierr)
c
c-----------------------------------------------------------------------
c
c ****** Interpolate a 2D field from array FI(NXI,NYI), defined
c ****** on the mesh XI(NXI) x YI(NYI), into the array F(NX,NY),
c ****** defined on the mesh X(NX) x Y(NY).
c
c ****** Zero values are returned at data points outside the
c ****** bounds of the XI x YI mesh.
c
c-----------------------------------------------------------------------
c
      use number_types
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      integer :: nxi,nyi
      real(r_typ), dimension(nxi) :: xi
      real(r_typ), dimension(nyi) :: yi
      real(r_typ), dimension(nxi,nyi) :: fi
      integer :: nx,ny
      real(r_typ), dimension(nx) :: x
      real(r_typ), dimension(ny) :: y
      real(r_typ), dimension(nx,ny) :: f
      integer :: ierr
c
c-----------------------------------------------------------------------
c
      real(r_typ), parameter :: zero=0._r_typ
      real(r_typ), parameter :: one=1._r_typ
c
c-----------------------------------------------------------------------
c
      integer :: i,j,ii,jj,iip1,jjp1
      real(r_typ) :: dummy,ax,ay,xv,yv
c
c-----------------------------------------------------------------------
c
      real(r_typ), external :: flint
c
c-----------------------------------------------------------------------
c
      ierr=0
c
c ****** Check that the scales XI and YI are monotonic.
c
      dummy=flint(.true.,zero,nxi,xi,xi,ierr)
      if (ierr.ne.0) go to 900
c
      dummy=flint(.true.,zero,nyi,yi,yi,ierr)
      if (ierr.ne.0) go to 900
c
c ****** Interpolate the data.
c
      do j=1,ny
        yv=y(j)
        if (yv.lt.yi(1).or.yv.gt.yi(nyi)) then
          f(:,j)=0.
          cycle
        else
          call interp (nyi,yi,yv,jj,jjp1,ay,ierr)
          if (ierr.ne.0) then
            f(:,j)=0.
            cycle
          end if
        end if
        do i=1,nx
          xv=x(i)
          if (xv.lt.xi(1).or.xv.gt.xi(nxi)) then
            f(i,j)=0.
            cycle
          else
            call interp (nxi,xi,xv,ii,iip1,ax,ierr)
            if (ierr.ne.0) then
              f(i,j)=0.
              cycle
            end if
          end if
          f(i,j)= (one-ax)*((one-ay)*fi(ii  ,jj  )+ay*fi(ii  ,jjp1))
     &           +     ax *((one-ay)*fi(iip1,jj  )+ay*fi(iip1,jjp1))
        enddo
      enddo
c
      return
c
c ****** Error exit: invalid scales.
c
  900 continue
c
      write (*,*)
      write (*,*) '### ERROR in INTRP2D:'
      write (*,*) '### Scales are not monotonically increasing.'
      ierr=1
c
      return
      end
c#######################################################################
      function wtime ( )
c
c-----------------------------------------------------------------------
c
c ****** WTIME returns a reading of the wall clock time.
c
c-----------------------------------------------------------------------
c
c ****** Discussion:
c
c ****** To get the elapsed wall clock time, call WTIME before and  
c ****** after a given operation, and subtract the first reading 
c ****** from the second.
c
c ****** This function is meant to suggest the similar routines:
c
c      "omp_get_wtime ( )" in OpenMP,
c      "MPI_Wtime ( )" in MPI,
c      and "tic" and "toc" in MATLAB.
c
c ****** This function is only going to work if SYSTEM_CLOCK is
c ****** available. SYSTEM_CLOCK is not a FORTRAN77 standard library 
c ****** function. It does come with FORTRAN90, however. So this ,
c ****** function may work, but if so it is only by the grace of 
c ****** compiler writers!
c
c ****** Licensing:
c ****** This code is distributed under the GNU LGPL license. 
c
c ****** Modified:
c ****** 27 April 2009
c
c ****** Author:
c ****** John Burkardt
c
c ****** Parameters:
c ****** Output, double precision WTIME, the wall clock reading, 
c ****** in seconds.
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
       integer ( kind = 4 ) clock_max
       integer ( kind = 4 ) clock_rate
       integer ( kind = 4 ) clock_reading
       real ( kind = 8 ) wtime
c
c-----------------------------------------------------------------------
c
      call system_clock ( clock_reading, clock_rate, clock_max )
c
      wtime = real (clock_reading,kind=8) / real (clock_rate,kind=8)
c
      return
      end
c#######################################################################
      function flint (check,x,n,xn,fn,ierr)
c
c-----------------------------------------------------------------------
c
c ****** Interpolate a function linearly.
c
c-----------------------------------------------------------------------
c
c ****** The funcion is defined at N nodes, with values given by
c ****** FN(N) at positions XN(N).  The function value returned is
c ****** the linear interpolant at X.
c
c ****** Note that if X.lt.XN(1), the function value returned
c ****** is FN(1), and if X.gt.XN(N), the function value returned
c ****** is FN(N).
c
c ****** Call once with CHECK=.true. to check that the values
c ****** in XN(N) are monotonically increasing.  In this mode
c ****** the array XN(N) is checked, and X and FN(N) are not
c ****** accessed.  If the check is passed, IERR=0 is returned.
c ****** Otherwise, IERR=1 is returned.
c
c-----------------------------------------------------------------------
c
      use number_types
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      logical :: check
      real(r_typ) :: x
      integer :: n
      real(r_typ), dimension(n) :: xn,fn
      integer :: ierr
      real(r_typ) :: flint
c
c-----------------------------------------------------------------------
c
      real(r_typ), parameter :: one=1._r_typ
c
c-----------------------------------------------------------------------
c
      integer :: i
      real(r_typ) :: x1,x2,alpha
c
c-----------------------------------------------------------------------
c
      ierr=0
      flint=0.
c
c ****** If CHECK=.true., check the abscissa table.
c
      if (check) then
        if (n.le.0) then
          write (*,*)
          write (*,*) '### ERROR in FLINT:'
          write (*,*) '### Invalid abscissa table dimension.'
          write (*,*) 'N = ',n
          ierr=1
          return
        end if
        do i=1,n-1
          if (xn(i+1).le.xn(i)) then
            write (*,*)
            write (*,*) '### ERROR in FLINT:'
            write (*,*) '### Abscissa table values are not'//
     &                  ' monotonically increasing.'
            write (*,*) 'N = ',n
            write (*,*) 'XN = ',xn
            ierr=1
            return
          end if
        enddo
        return
      end if
c
c ****** Get the interpolated value.
c
      if (x.le.xn(1)) then
        flint=fn(1)
      else if (x.gt.xn(n)) then
        flint=fn(n)
      else
        do i=1,n-1
          if (x.ge.xn(i).and.x.lt.xn(i+1)) exit
        enddo
        x1=xn(i)
        x2=xn(i+1)
        alpha=(x-x1)/(x2-x1)
        flint=fn(i)*(one-alpha)+fn(i+1)*alpha
      end if
c
      return
      end
c#######################################################################
      subroutine interp (n,x,xv,i,ip1,a,ierr)
c
c-----------------------------------------------------------------------
c
c ****** Get the interpolation factor at XV from the table X(N).
c
c-----------------------------------------------------------------------
c
c ****** This routine does not do the actual interpolation.  Use the
c ****** returned values of I, IP1, and A to get the interpolated
c ****** value.
c
c-----------------------------------------------------------------------
c
      use number_types
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      integer :: n
      real(r_typ), dimension(n) :: x
      real(r_typ) :: xv
      integer :: i,ip1
      real(r_typ) :: a
      integer :: ierr
c
c-----------------------------------------------------------------------
c
      ierr=0
c
c ****** Check if the x-scale has only one point.
c
      if (n.eq.1.and.xv.eq.x(1)) then
        ip1=1
        a=0.
        return
      end if
c
c ****** Find the interval and compute the interpolation factor.
c
      do i=1,n-1
        if (xv.ge.x(i).and.xv.le.x(i+1)) then
          ip1=i+1
          if (x(i).eq.x(i+1)) then
            a=0.
          else
            a=(xv-x(i))/(x(i+1)-x(i))
          end if
          return
        end if
      enddo
c
c ****** ERROR: the value was not found.
c
      ierr=1
c
      return
      end
c#######################################################################
      subroutine set_parameters
c
c-----------------------------------------------------------------------
c
c ****** Set parameters from the command line arguments.
c
c-----------------------------------------------------------------------
c
      use ident
      use number_types
      use syntax
      use paragraph_def
      use get_usage_line_interface
      use print_par_interface
      use delete_par_interface
      use params
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
c ****** Storage the for usage line.
c
      type(paragraph), pointer :: usage
c
c ****** Storage for the error message.
c
      character(72) :: errmsg
c
c-----------------------------------------------------------------------
c
      integer :: ierr
      character(512) :: arg
      logical :: set
c
c-----------------------------------------------------------------------
c
      real(r_typ), external :: fpval
c
c-----------------------------------------------------------------------
c
c ****** Define the syntax.
c
      call defarg (GROUP_K ,'-v',' ',' ')
      call defarg (GROUP_A ,'infile',' ',' ')
      call defarg (GROUP_K ,'-pt',' ',' ')
      call defarg (GROUP_KA,'-dims','tp','t|p')
      call defarg (GROUP_KA,'-visc','0.','<val>')
      call defarg (GROUP_KA,'-viscfile','<none>','<file>')
      call defarg (GROUP_K,'-viscgrid',' ',' ')
      call defarg (GROUP_KA,'-viscfac','1.','<val>')
      call defarg (GROUP_KA,'-s','<none>','<file>')
      call defarg (GROUP_KA,'-time','1.','<val>')
      call defarg (GROUP_K,'-sts',' ',' ')
      call defarg (GROUP_KA,'-stsfac','0.','<val>')
      call defarg (GROUP_K,'-exp',' ',' ')
      call defarg (GROUP_A ,'outfile',' ',' ')
c
c ****** Parse the command line.
c
      call parse (errmsg,ierr)
c
      if (ierr.ne.0) then
c
        write (*,*)
        write (*,*) '### ',cname,' Version ',cvers,' of ',cdate,'.'
        write (*,*) '### Diffuse a 2D scalar field on the surface'//
     &              ' of a sphere.'
c
c ****** Print the usage line.
c
        call get_usage_line (usage)
c
        write (*,*)
        write (*,*) 'Usage:'
        write (*,*)
c
        call print_par (usage)
c
        call delete_par (usage)
c
        write (*,*)
        write (*,*) '-time <#> Set the end time. (Default 1.0)'
        write (*,*)
        write (*,*) 'Use -pt to indicate (p,t) coordinate order'//
     &              ' in the input file [default=(t,p)].'
        write (*,*)
        write (*,*) 'Use -dims to apply the diffusion only along'//
     &              ' the specified dimensions.'
        write (*,*) 'The default is to diffuse along both'//
     &              ' dimensions.'
        write (*,*)
        write (*,*) 'Viscosity is set as: viscfac*(visc + visc_file '//
     &              '+ visc_grid)'
        write (*,*) ' '
        write (*,*) ' -viscfac <#> (Overall multiplier, Default: 1.0)'
        write (*,*) ' -visc <#> (Uniform viscosity)'
        write (*,*) '           Default: If neither viscfile or '//
     &                   'viscgrid set, 1, otherwise, 0.'
        write (*,*) ' -viscfile <fname> (Read from file, Default: none)'
        write (*,*) '                    The file must contain a 2D'//
     &                                 ' field with scales.'
        write (*,*) '                    Any region outside the '//
     &                                   'domain is set to 0.'
        write (*,*) ' -viscgrid (Grid viscosity, Default: disabled)'
        write (*,*) '           Defined by: visc_grid = dt^2 + '//
     &                           '(sin(t)*dt)^2'
        write (*,*)
        write (*,*) 'Source term can be specified using: -s <file>.'
        write (*,*) '            (Default: none)'
        write (*,*) '            The file must contain a 2D'//
     &                           ' field with scales.'
        write (*,*) '            Any region outside the '//
     &                           'domain is set to 0.'
        write (*,*) ' '
c
        write (*,*) '-exp (Use old explicit algorithm).'
        write (*,*) ' '
        write (*,*) '-sts_fac (Set time-step as: dt=sts_fac*dt_exp '
        write (*,*) '          where dt_exp is the stable explicit'//
     &                          ' time-step)'
        write (*,*) ' '
        write (*,*) '-sts (depricated parameter).'
        write (*,*) ' '
        call exit (1)
c
      end if
c
c ****** Set the parameters.
c
c ****** Verbose flag.
c
      call fetcharg ('-v',set,arg)
      verbose=set
c
c ****** Input file.
c
      call fetcharg ('infile',set,arg)
      infile=trim(arg)
c
c ****** Output file.
c
      call fetcharg ('outfile',set,arg)
      outfile=trim(arg)
c
c ****** Viscosity file.
c
      call fetcharg ('-viscfile',set,arg)
      if (set) then
        viscfile=trim(arg)
      else
        viscfile=' '
      end if
c
c ****** Source term file.
c
      call fetcharg ('-s',set,arg)
      if (set) then
        sourcefile=trim(arg)
      else
        sourcefile=' '
      end if
c
c ****** Uniform viscosity.
c
      call fetcharg ('-visc',set,arg)
      visc=fpval(arg,'-visc')
c
c ****** Viscosity multiplier.
c
      call fetcharg ('-viscfac',set,arg)
      viscfac=fpval(arg,'-viscfac')
c
c ****** Activate auto-viscosity.
c
      call fetcharg ('-viscgrid',set,arg)
      viscgrid=set
c
c ****** Time to diffuse for.
c
      call fetcharg ('-time',set,arg)
      time=fpval(arg,'-time')
c
c ****** Order of coordinates in the file.
c
      call fetcharg ('-pt',set,arg)
      pt_order=set
c
c ****** Dimensions along which to diffuse.
c
      call fetcharg ('-dims',set,arg)
      filtdims=trim(arg)
c
c ****** Use original explicit algorithm.
c
      call fetcharg ('-exp',set,arg)
      if (set) use_sts=.false.
c
c ****** Depricated super-time-stepping flag.
c
      call fetcharg ('-sts',set,arg)
c
c ****** Modify super-time-step.
c
      call fetcharg ('-stsfac',set,arg)
      stsfac=fpval(arg,'-stsfac')
c
      return
      end
c
c-----------------------------------------------------------------------
c
c ****** Update log:
c
c        10/25/2005, ZM, Version 1.00:
c
c         - Original version of the program.
c
c        07/23/2010, ZM, Version 1.01:
c
c         - Added the ability to have a source for the diffusion
c           equation.  This is read in from a file.
c
c        03/26/2011, ZM, Version 1.02:
c
c         - Corrected some bugs having to do with the rather hasty
c           introduction of the source term in the previous version.
c         - Cleaned the code up a bit.
c
c        09/05/2014, RC,CD, Version 1.03:
c
c         - Implemented OpenMP and OpenACC directives in the code
c           to speed it up.  New code can be compiled with either
c           OpenMP or OpenACC flags (or no flags for serial code).
c
c        08/07/2015, RC, Version 2.00:
c
c         - Implemented super-time-stepping algorithm (RKL2)
c           from Meyer, et. al. J. Comp. Phys. 257 (2014) 594-626
c           which can speedup runs significantly over previous method.
c           Add "-sts" to command line to use (eventually
c           it may become the default.)
c           One can also add "-stsdtfac #" to modify the sts time step,
c           gaining more speed with less accuracy.
c         - Changed verbose output of steps to only output a maximum
c           of 100 lines.
c
c        10/11/2015, RC, Version 2.01:
c
c         - Modified code to eliminate floating-point divisions
c           and a few OpenACC optimizations.
c           This new code runs much faster than the original
c           even when using the original algorithm (see svn log
c           for timings and validation).
c
c        10/20/2015, RC, Version 2.02:
c
c         - Further optimized OpenMP and OpenACC directives.
c         - Changed STS behavior as follows:
c           Setting -sts will activate STS with a time-step
c           100X larger than the explicit step size.
c           Set the renamed -stsfac to override this to whatever number
c           you choose.  Example:  -sts -stsfac 10000.
c
c        11/05/2015, RC, Version 2.03:
c
c         - Further optimized algorithm.
c .       - Default stsfac updated to 500.
c
c        01/28/2016, RC, Version 2.04:
c
c         - Optimized algorithms.
c           The code runs up to 50% faster for small grids.
c         - Changed STS to compute STS factors AFTER modifying STS dt.
c         - Added check to avoid STS steps that are larger than
c           the end time.  Now, there are always at least 6 STS steps.
c
c        02/08/2016, RC, Version 2.05:
c
c         - Changed the way the default time-step for STS is computed.
c           With no "-stsfac" option selected, dt_sts~1/(2*pi*nu*k).
c           The behavior of the -stsfac option is the same and
c           overrides the above default.
c         - Lowered minimum STS steps to ~3.
c
c        02/10/2016, RC+CD, Version 2.06:
c
c         - Changed (agian) the way the default time-step for STS
c           is computed.  It is now set to the minimum of the previous
c           default time-step and dt_sts=sqrt(end_time*dt_exp).
c         - Added a check in STS so that if dt_sts is smaller
c           than 2*dt_exp, the code switches back to the
c           original explicit algorithm.
c
c        09/12/2016, RC, Version 2.07:
c
c         - Changed (yet again) the way the default time-step for STS
c           is computed as follows:
c           - If there will be less than 500 exp steps, use exp method.
c           - Use default of sqrt(dtexp*time) unless it will result
c             in less than 50 STS steps, in which case set to 50 steps.
c           - When using -stsfac option, only allow factors up to
c             that which yields 50 STS steps.  After that, set to 50.
c         - These changes should make the tool much more robust,
c           and accurate, even when using large sts factors.
c         - Changes explicit limit to have 1/4 instead of 1/2
c           for high-mode consistency.
c
c        11/20/2017, RC, Version 2.08:
c
c         - Cleaned up and modernized OpenACC pragmas.
c         - Changed default time-step for STS to be such that
c           the algorithm takes 60 super steps.  This results in the
c           fastest run-time while ensuring high-mode structures are
c           damped correctly.
c           One can achieve more accuracy by setting the -stsfac #
c           command line argument to a lower value than the default
c           step size produces.
c
c        12/13/2017, RC, Version 2.09:
c
c         - Modifed ax() in STS algorithm to use explicit matrix coefs.
c         - Matrix coefs computed in both STS and EXP mode to get better
c           estimate of maximum stable dt. New stable dt estimate up
c           to 10x bigger than before, leading to 10x speedup using
c           EXP algorithm, and >2x speedup using STS.
c
c        04/09/2019, RC, Version 2.10:
c
c         - Made the default mode to be sts.  New parameter -exp is used
c           to run the old explicit algorithm. The -sts parameter
c           is depricated but still there for script compatibility.
c         - Added automatic grid-based viscosity. Activate w/ -viscgrid.
c         - Added -viscfac which multiplies total viscosity (default 1).
c           Now visc is defined as: viscfac*(visc+viscfile+viscgrid).
c           By default, visc=1 so default is the same as before.
c           However if viscgrid or viscfile is used, visc defaults to 0
c           (but can still be specified with -visc).
c         - Time now defaults to 1.0 instead of 0.
c
c        11/27/2019, RC, Version 2.11:
c
c         - Modified a couple of OpenACC directives for optimization.
c         - Changed matrix coefs for sts to vector-friendly layout.
c
c        02/12/2020, RC, Version 2.12:
c
c         - Small bug fix for OpenACC.
c
c        05/20/2021, RC, Version 2.12.1:
c
c         - Small modifications, updated version number style.
c         - Note that now by default there are 60 STS super steps
c           instead of 61, leading to a small numerical solution diff.
c
c        05/27/2021, RC, Version 2.12.2:
c
c         - Bug fix for runs with many steps (int8 issue).
c         - Added flushes to output.
c
c        06/02/2021, RC, Version 2.12.3:
c
c         - Removed OpenMP and OpenACC asynchronous pragmas.
c           They looked suspicious and were perhaps wrong.
c
c-----------------------------------------------------------------------
c
