c#######################################################################
      module number_types
c
c-----------------------------------------------------------------------
c ****** Basic number types.
c ****** This module is used to set the default precision for REALs.
c-----------------------------------------------------------------------
c
c **********************************************************************
c
c Copyright 2018 Predictive Science Inc.
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
      use iso_fortran_env
c
c-----------------------------------------------------------------------
c
      implicit none
c
      integer, parameter :: KIND_REAL_4=REAL32
      integer, parameter :: KIND_REAL_8=REAL64
      integer, parameter :: KIND_REAL_16=max(REAL128,REAL64)
c
      integer, parameter :: r_typ=KIND_REAL_8
c
      end module
c#######################################################################

