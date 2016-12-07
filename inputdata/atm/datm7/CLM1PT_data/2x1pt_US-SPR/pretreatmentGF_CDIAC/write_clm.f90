      subroutine write_clm(sitename, lat, lon, ht, firstyear, N_out, &
          Tair_out, RH_out, wsp_out, precip_out, press_out, &
          Rg_in_out, Rlong_in_out, thisnpd, lst)

      use netcdf
      implicit none

      integer  i, n, v, N_out, RCODE, NCID(2), thisnpd, lst
      integer thisy, thism, thisf, N_file, dimid(4), dpm(12)
      integer tempdim(2)
      integer startf(3), countf(3)
      integer*8 starti, starti_gmt
      integer firstyear, nmonths, varids(2,20), thisnyears
      real Tair_out(1000000), RH_out(1000000)
      real wsp_out(1000000), precip_out(1000000)
      real press_out(1000000), Rg_in_out(1000000)
      real Rlong_in_out(1000000)
      real scale_factors(8), add_offsets(8)
      double precision dsince(1000000)
      real lat, lon, ht 
      double precision temp3d(8,1,1,1000000), temp_out(1,1,10000)
      integer*2 temp_out_short(10000,1)
      double precision lat_in(1,1), lon_in(1,1)
      double precision ht_in(1,1,1000000)
      character(len=3) mst
      character(len=4) yst
      character(len=11) latst, lonst, htst
      character(len=20) varnames(8), varunits(8)
      character(len=6) sitename
      character(len=100) fname, cmd, var_longnames(8)

      print*, N_out

      varnames(1) = 'TBOT'
      varnames(2) = 'QBOT'
      varnames(3) = 'WIND'
      varnames(4) = 'FSDS'
      varnames(5) = 'FLDS'
      varnames(6) = 'PSRF'
      varnames(7) = 'PRECTmms'
      varnames(8) = 'ZBOT'

      varunits(1) = 'K'
      varunits(2) = 'kg/kg'
      varunits(3) = 'm/s'
      varunits(4) = 'W/m2'
      varunits(5) = 'W/m2'
      varunits(6) = 'Pa'
      varunits(7) = 'kg/m2/s'
      varunits(8) = 'm'

      var_longnames(1) =  'temperature at the lowest atm level (TBOT)'
      var_longnames(2) =  &
         'specific humidity at the lowest atm level (QBOT)'
      var_longnames(3) =  'wind at the lowest atm level (WIND)'
      var_longnames(4) =  'incident solar (FSDS)'
      var_longnames(5) =  'incident longwave (FLDS)'
      var_longnames(6) =  'pressure at the lowest atm level (PSRF)'
      var_longnames(7) =  'precipitation (PRECTmms)'
      var_longnames(8) =  'observational height'
      
      temp3d(1,1,1,:) = Tair_out
      temp3d(2,1,1,:) = RH_out
      temp3d(3,1,1,:) = wsp_out
      temp3d(4,1,1,:) = Rg_in_out
      temp3d(5,1,1,:) = Rlong_in_out
      temp3d(6,1,1,:) = press_out
      temp3d(7,1,1,:) = precip_out / (3600*24d0/thisnpd)
      temp3d(8,1,1,:) = ht

      thisnyears = N_out/(8760*thisnpd/24)
      nmonths = thisnyears*12

      do v=1,8
         add_offsets(v) = sum(temp3d(v,1,1,1:N_out))/N_out
         scale_factors(v) = max((maxval(temp3d(v,1,1,1:N_out)) - &
              minval(temp3d(v,1,1,1:N_out)))*1.1, 1.0e-6)/2.0**15
      end do


      if (lon .lt. 0) lon=lon+360

      starti     = 1
      starti_gmt = 1+lst*thisnpd/24   !output files in GMT

      RCODE=NF90_CREATE('1x1pt_' // trim(sitename) // '/all_hourly.nc', &
           NF90_CLOBBER, NCID(2))

      do thisf = 1,nmonths

      thisy = firstyear+(thisf-1)/12 
      thism = mod(thisf-1,12)+1
     
      DATA dpm/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
         
      write(latst, '(f9.5)')  lat
      write(lonst, '(f11.5)') lon
      write(htst,  '(f5.0)')  ht
      write(yst,   '(I4)')    thisy
      write(mst,   '(I3)')    100+thism

      N_file = dpm(thism)*thisnpd

      cmd = 'mkdir -p ' // '1x1pt_' // trim(sitename)
      call system(cmd)
      fname= '1x1pt_' // trim(sitename) // '/' // yst // '-' // &
        mst(2:3) // '.nc'   
      print*, fname

      RCODE=NF90_CREATE(trim(fname), NF90_CLOBBER, NCID(1))
      
      do n=1,2
         if (n .eq. 1 .or. (n .eq. 2 .and. starti .eq. 1)) then 
            RCODE = NF90_DEF_DIM(NCID(n), 'scalar', 1, dimid(1))
            if (n .eq. 1) then 
               RCODE = NF90_DEF_DIM(NCID(n), 'lon', 1, dimid(2))
               RCODE = NF90_DEF_DIM(NCID(n), 'lat', 1, dimid(3))   
            else
               RCODE = NF90_DEF_DIM(NCID(n), 'gridcell', 1, dimid(3))
            end if
            if (n .eq. 1) then 
               RCODE = NF90_DEF_DIM(NCID(n), 'time', NF90_UNLIMITED, dimid(4))
            else
              RCODE = NF90_DEF_DIM(NCID(n), 'DTIME', N_out, dimid(4))
           end if

            !global attributes
            RCODE = NF90_PUT_ATT(NCID(n), NF90_GLOBAL, 'institution', &
                 'Oak Ridge National Laboratory')
            RCODE = NF90_PUT_ATT(NCID(n), NF90_GLOBAL, 'history', &
                 'File Origin - This file was created at '&
                 // 'Oak Ridge National Laboratory for the ' &
                 // 'site-level MDC in 2010')
            RCODE = NF90_PUT_ATT(NCID(n), NF90_GLOBAL, 'site_location', & 
                 'Latitude:' // latst // ' Longitude: ' &
                 // lonst // ' Elevation (masl): ' // htst)        

            !time
            RCODE = NF90_DEF_VAR(NCID(n), 'DTIME', NF90_DOUBLE, dimid(4), &
                 varids(n,1))
            RCODE = NF90_PUT_ATT(NCID(n), varids(n,1), 'long_name', &
                 'observation time')
            RCODE = NF90_PUT_ATT(NCID(n), varids(n,1), 'units', &
                 'days since ' // yst // '-' // mst(2:3) // '-01 00:00:00')
            RCODE = NF90_PUT_ATT(NCID(n), varids(n,1), 'calendar', 'noleap')
            !longitude
            RCODE = NF90_DEF_VAR(NCID(n), 'LONGXY', NF90_DOUBLE, dimid(n+1), &
                 varids(n,2))
            RCODE = NF90_PUT_ATT(NCID(n), varids(n,2), 'long_name', 'longitude')
            RCODE = NF90_PUT_ATT(NCID(n), varids(n,2), 'units', 'degrees E')
            !latitude
            RCODE = NF90_DEF_VAR(NCID(n), 'LATIXY', NF90_DOUBLE, dimid(3), &
                 varids(n,3))
            RCODE = NF90_PUT_ATT(NCID(n), varids(n,3), 'long_name', 'latitude')
            RCODE = NF90_PUT_ATT(NCID(n), varids(n,3), 'units', 'degrees N')
            RCODE = NF90_PUT_ATT(NCID(n), varids(n,4), 'units', 'm')
            RCODE = NF90_PUT_ATT(NCID(n), varids(n,4), 'Mode', 'time-dependent')
            !EDGES
            RCODE=NF90_DEF_VAR(NCID(n), 'EDGEW', NF90_DOUBLE, dimid(1), &
                 varids(n,5))
            RCODE=NF90_PUT_ATT(NCID(n), varids(n,5), 'long_name', &
                 'western edge in atmospheric data')
            RCODE=NF90_PUT_ATT(NCID(n), varids(n,5), 'units', 'degrees E')      
            RCODE=NF90_DEF_VAR(NCID(n), 'EDGEE', NF90_DOUBLE, dimid(1), &
                 varids(n,6))
            RCODE=NF90_PUT_ATT(NCID(n), varids(n,6), 'long_name', &
                 'eastern edge in atmospheric data')
            RCODE=NF90_PUT_ATT(NCID(n), varids(n,6), 'units', 'degrees E')
            RCODE=NF90_DEF_VAR(NCID(n), 'EDGES', NF90_DOUBLE, dimid(1), &
                 varids(n,7))
            RCODE=NF90_PUT_ATT(NCID(n), varids(n,7), 'long_name', &
                 'southern edge in atmospheric data')
            RCODE=NF90_PUT_ATT(NCID(n), varids(n,7), 'units', 'degrees N')
            RCODE=NF90_DEF_VAR(NCID(n), 'EDGEN', NF90_DOUBLE, dimid(1), &
                 varids(n,8))
            RCODE=NF90_PUT_ATT(NCID(n), varids(n,8), 'long_name', & 
                 'northern edge in atmospheric data')
            RCODE=NF90_PUT_ATT(NCID(n), varids(n,8), 'units', 'degrees N')
            if (n .eq. 2) then 
               RCODE = NF90_DEF_VAR(NCID(n), 'start_year', &
                        NF90_INT, dimid(1), varids(n,19))
               RCODE = NF90_DEF_VAR(NCID(n), 'end_year', &
                        NF90_INT, dimid(1), varids(n,20))
            endif

            !Meteorology
            do v=1,8
               if (n .eq. 1) then 
                  RCODE = NF90_DEF_VAR(NCID(n), trim(varnames(v)), &
                       NF90_DOUBLE, dimid(2:4), varids(n,v+8))
               else
                  tempdim(1) = dimid(4)
                  tempdim(2) = dimid(3)
                  RCODE = NF90_DEF_VAR(NCID(n), trim(varnames(v)), &
                       NF90_SHORT, tempdim(1:2), varids(n,v+8))
               end if
               RCODE = NF90_PUT_ATT(NCID(n), varids(n,v+8), 'long_name', &
                    trim(var_longnames(v)))
               RCODE = NF90_PUT_ATT(NCID(n), varids(n,v+8), 'units', &
                    trim(varunits(v)))
               RCODE = NF90_PUT_ATT(NCID(n), varids(n,v+8), 'mode', &
                    'time-dependent')
               if (n .eq. 2) then 
                  RCODE = NF90_PUT_ATT(NCID(n), varids(n,v+8), 'scale_factor', &
                       scale_factors(v))
                  RCODE = NF90_PUT_ATT(NCID(n), varids(n,v+8), 'add_offset', &
                       add_offsets(v))
               end if
            end do
            RCODE=NF90_ENDDEF(NCID(n))
         end if

         !create dsince
         do i=1,N_file
            dsince(i) = ((starti-1)*(n-1)+i)*(1.0/thisnpd)
         end do
      
         !put varibles
         startf(1) = (starti-1)*(n-1)+1
         countf(1) = N_file
         if (n .eq. 1 .or. (n .eq. 2 .and. starti .eq. 1)) then 
            RCODE = NF90_PUT_VAR(NCID(n), varids(n,1), dsince(1:N_file))
            lon_in(1,1)=lon
            lat_in(1,1)=lat
            ht_in(1,1,:)=ht
            RCODE = NF90_PUT_VAR(NCID(n), varids(n,2), lon_in)
            RCODE = NF90_PUT_VAR(NCID(n), varids(n,3), lat_in)
            RCODE = NF90_PUT_VAR(NCID(n), varids(n,4), ht_in(1,1,1:N_file), &
                 startf(1:1), countf(1:1))
            RCODE = NF90_PUT_VAR(NCID(n), varids(n,5), lon-0.1d0)
            RCODE = NF90_PUT_VAR(NCID(n), varids(n,6), lon+0.1d0)      
            RCODE = NF90_PUT_VAR(NCID(n), varids(n,7), lat-0.1d0)
            RCODE = NF90_PUT_VAR(NCID(n), varids(n,8), lat+0.1d0)
            if (n .eq. 2) then 
              RCODE = NF90_PUT_VAR(NCID(n), varids(n,19), firstyear)
              RCODE = NF90_PUT_VAR(NCID(n), varids(n,20), firstyear+thisnyears-1)
            end if 
         else
            RCODE = NF90_PUT_VAR(NCID(n), varids(n,1), dsince(1:N_file), &
                 startf(1:1), countf(1:1))
         end if
         
         do v=1,8
            startf(:) = 1
            countf(:) = 1
            temp_out(1,1,1:N_file) = temp3d(v,1,1,starti_gmt:starti_gmt+ &
                 N_file-1)
            if (starti_gmt .lt. 1) then 
               temp_out(1,1,1:(-1*lst*(thisnpd/24)))= temp3d(v,1,1,1)
               !print*, temp_out(1,1,1:N_file)
            end if
           
            temp_out_short(:,1) = &
                 nint((temp_out(1,1,:)-add_offsets(v))/scale_factors(v))
            if (n .eq. 1) then
               countf(3) = N_file
               RCODE = NF90_PUT_VAR(NCID(n), varids(n,v+8), &
                    temp_out(:,:,1:N_file), startf, countf)
            else
               countf(1) = N_file
               startf(1) = (starti-1)*(n-1)+1
               RCODE = NF90_PUT_VAR(NCID(n), varids(n,v+8), &
                    temp_out_short(1:N_file,:), startf(1:2), &
                    countf(1:2))
            end if
         end do
      
  
         RCODE=NF90_CLOSE(NCID(1))
         
      end do
      starti=starti+N_file
      starti_gmt = starti_gmt+N_file
      print*, starti, starti_gmt
      
   end do
   RCODE = NF90_CLOSE(NCID(2))
   
 end subroutine write_clm
