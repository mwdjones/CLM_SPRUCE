#!/usr/bin/python

import os, csv, time, math
import Scientific.IO
from Scientific.IO import NetCDF
import numpy

site_code  = 'US-SPR'  #AmeriFlux/FLUXNET identifier


#Function for cacluating saturation specific humidity
def qsat(t,pres):
    a = [6.107799961, 4.436518521e-01, 1.428945805e-02, 2.650648471e-04, \
             3.031240396e-06, 2.034080948e-08, 6.136820929e-11]
    b = [6.109177956, 5.034698970e-01, 1.886013408e-02, 4.176223716e-04, \
             5.824720280e-06, 4.838803174e-08, 1.838826904e-10]

    if (t > 150):
        t = t-273.15  #convert K to C
    if (t > 0):
        esat = 100.*(a[0]+t*(a[1]+t*(a[2]+t*(a[3]+t*(a[4]+t*(a[5]+t*a[6]))))))
    else:
        esat = 100.*(b[0]+t*(b[1]+t*(b[2]+t*(b[3]+t*(b[4]+t*(b[5]+t*b[6]))))))
    myqsat = 0.622 * esat / (pres - 0.378*esat)
    return myqsat

site_lat = 47.563 
site_lon = 266.512
lst = -6
startyear = 2011
endyear  = 2015

print 'Site code is '+site_code
print 'Site latitude is  '+str(site_lat)
print 'Site longitude is '+str(site_lon)
print 'Local time offset is UTC'+str(lst)


#---------------load the data -----------------------------------------

vars_ind = [7,9,11,17,19,15,13,13]
vars_out = ['TBOT','QBOT','WIND','FSDS','FLDS','PSRF','PRECTmms','ZBOT']
vars_ncep = ['air', 'shum', 'uwnd', 'dswrf', 'dlwrf', 'pres', 'prate', 'vwnd']
levs_ncep = ['2m', '2m', '10m', 'sfc', 'sfc', 'sfc', 'sfc', '10m']
vars_out_units = ['K','kg/kg','m/s','W/m2','W/m2','Pa','kg/m2/s','m']
long_names = []
long_names.append('temperature at the lowest atm level (TBOT)')
long_names.append('humidity at the lowest atm level (RH)')
long_names.append('wind at the lowest atm level (WIND)')
long_names.append('incident solar (FSDS)')
long_names.append('incident longwave (FLDS)')
long_names.append('pressure at the lowest atm level (PSRF)')
long_names.append('precipitation (PRECTmms)')
long_names.append('observational height')
alldata = numpy.zeros((8,1000000), dtype=numpy.float)-9999

fname =  'SPRUCE_all_GF_met.csv'
vnum = 0
npd = 48
for v in vars_in:
    myinput = open(fname, 'r')
    lnum = 0
    thisind=0
    for s in myinput:
        myval =  myvar_vals.append(s.split(',')[vars_ind]) 
        if (lnum == 3):
            for l in range(0,12):
                alldata[vnum,thisind] = float(myval)
                thisind = thisind+1
        if (lnum > 3):
            alldata[vnum,thisind] = float(myval)
            thisind = thisind+1
        lnum = lnum+1
    vnum = vnum+1
    myinput.close()
npoints = thisind-12       

# ------------create CLM-style output variables -----------------

toffset = lst*(npd/24)-1  # conversion from LST to UTC, CLM units
ndaysm = [31,28,31,30,31,30,31,31,30,31,30,31]  #number of days per month
#output meteorogical data to netcdf file(s)

#unit conversions
vnum=0
for v in vars_out:
    if (vnum == 0):     #convert temperature from C to K
        alldata[vars_out_i[vnum],0:npoints] = \
        alldata[vars_out_i[vnum],0:npoints] + 273.15
    if (vnum == 1):
        #RH to QBOT
        for i in range(0,npoints):
            alldata[vars_out_i[vnum],i] = qsat(alldata[vars_out_i[0],i], \
                alldata[vars_out_i[5],i]*1000.) * alldata[vars_out_i[vnum],i] \
                /100.0
            #alldata_ncep[1,i] = (alldata_ncep[1,i]/ \
            #    qsat(alldata_ncep[0,i], alldata_ncep[5,i]))*100.
            #alldata_ncep[1,i] = max( min(alldata_ncep[1,i], 100.), 0.)
    elif (vnum == 3 and vars_out_i[3] == 2):  #convert PAR to W/m2
        alldata[vars_out_i[vnum],0:npoints] = \
        alldata[vars_out_i[vnum],0:npoints] / 0.48 / 4.6 
    elif (vnum == 5):   #convert pressure from kpa to pa
        alldata[vars_out_i[vnum],0:npoints] = \
        alldata[vars_out_i[vnum],0:npoints] * 1000.0
    elif (vnum == 6):   #convert precip from mm to kg/m2/s
        for i in range(0,npoints):
            if (alldata[vars_out_i[vnum],i] > -9000):
                    alldata[vars_out_i[vnum],i] = \
                    alldata[vars_out_i[vnum],i] / (3600.0 * 24.0/npd)
    vnum = vnum+1

for ftype in range(0,2):
    if (ftype == 0):     #standard monthly files
        nfiles = (endyear-startyear+1)*12
    else:                #CPL_BYPASS style files 
        nfiles = 1
    starti = 0
    outdirname = inputdata+'/1x1pt_'+site_code
    os.system('mkdir -p '+outdirname)
    for n in range(0,nfiles):
        if (ftype == 0):
            yst = str(startyear+n/12)
            mst = str((n % 12)+101)
            out_nc = NetCDF.NetCDFFile(outdirname+'/'+yst+'-'+mst[1:]+'.nc','w')
            out_nc.createDimension('lon', 1)
            out_nc.createDimension('lat', 1)
            npoints_out =  ndaysm[n % 12]*npd
            timedim = 'time'
        else:
            yst = str(startyear)
            mst = str(101)
            out_nc = NetCDF.NetCDFFile(outdirname+'/all_hourly.nc','w')
            out_nc.createDimension('gridcell',1)
            npoints_out = npoints
            timedim = 'DTIME'
        out_nc.createDimension('scalar', 1)
        out_nc.createDimension(timedim, npoints_out)

        #create variables and attributes
        time = out_nc.createVariable(timedim,'d',(timedim,))
        time.units = 'days since '+yst+'-'+mst[1:]+'-01 00:00:00'
        time.long_name = 'Time axis'
        time.calendar = 'noleap'
        time[:] = numpy.arange(npoints_out)*(1.0/npd)
        if (ftype == 0):
            longxy = out_nc.createVariable('LONGXY','d',('lon',))
            latixy = out_nc.createVariable('LATIXY','d',('lat',))
        else:
            longxy = out_nc.createVariable('LONGXY','d',('gridcell',))
            latixy = out_nc.createVariable('LATIXY','d',('gridcell',))
        longxy.long_name = 'longitude'
        longxy.units = 'degrees E'
        longxy[:] = site_lon
        latixy.long_name = 'latitude'
        latixy.units = 'degrees N'
        latixy[:] = site_lat
        edgew = out_nc.createVariable('EDGEW','d',('scalar',))
        edgew.long_name = 'western edge in atmospheric data'
        edgew.units = 'degrees E'
        edgew[:] = site_lon-0.05
        edgee = out_nc.createVariable('EDGEE','d',('scalar',))
        edgee.long_name = 'eastern edge in atmospheric data'
        edgee.units = 'degrees E'
        edgee[:] = site_lon+0.05
        edgen = out_nc.createVariable('EDGEN','d',('scalar',))
        edgen.long_name = 'northern edge in atmospheric data'
        edgen.units = 'degrees N'
        edgen [:]= site_lat+0.05
        edges = out_nc.createVariable('EDGES','d',('scalar',))
        edges.long_name = 'southern edge in atmospheric data'
        edges.units = 'degrees N'
        edges[:] = site_lat-0.05
        start_year = out_nc.createVariable('start_year','i',('scalar',))
        start_year[:] = startyear
        end_year = out_nc.createVariable('end_year','i',('scalar',))
        end_year[:] = endyear

        vnum = 0
        for v in vars_out:
            if (ftype == 1):
                thisvar = out_nc.createVariable(v,'s',('gridcell',timedim,))
                #figure out scale factors and add_offsets for compressed data
                if (v == 'PRECTmms'):
                    if (numpy.max(alldata[vars_out_i[vnum], 0:npoints]) > -9000):
                        scale_factor = (numpy.max(alldata[vars_out_i[vnum], \
                                      0:npoints]))*1.1 / 2.0**15
                    else:
                        scale_factor = (numpy.max(alldata_ncep[vnum, \
                                      0:npoints/(npd/4)]))*1.1 / 2.0**15
                    add_offset = scale_factor*2.0**14
                else:
                    if (numpy.max(alldata[vars_out_i[vnum], 0:npoints]) > -9000):
                        add_offset = numpy.mean(alldata[vars_out_i[vnum], \
                                                        0:npoints])
                        scale_factor = (numpy.max(alldata[vars_out_i[vnum], \
                          0:npoints]) - numpy.min(alldata[vars_out_i[vnum], \
                          0:npoints])) / 2.0**15
                    else:
                        add_offset = numpy.mean(alldata_ncep[vnum,0:npoints/(npd/4)])
                        scale_factor = (numpy.max(alldata_ncep[vnum,0:npoints/(npd/4)]) \
                                   - numpy.min(alldata_ncep[vnum,0:npoints/(npd/4)])) / 2.0**15
                scale_factor = max(1e-9, scale_factor)
                thisvar.add_offset = add_offset
                thisvar.scale_factor = scale_factor
            else:
                thisvar = out_nc.createVariable(v,'f',(timedim,'lat','lon',))

            thisvar.long_name = long_names[vnum]
            thisvar.units = vars_out_units[vnum]
            thisvar.mode = 'time-dependent'

            for np in range(starti,starti+npoints_out):
                if (alldata[vars_out_i[vnum], max(min(np+toffset,npoints-1),0)] < -900):
                    #replace still-missing data with NCEP, use linear interpolation
                    wt1 = 1 - float(np % (npd/4))/(npd/4)
                    wt2 = float(np % (npd/4))/(npd/4)
                    myval = alldata_ncep[vnum, int(np/(npd/4))]*wt1 + \
                         alldata_ncep[vnum, min(int(np/(npd/4))+1, npoints/(npd/4)-1)]*wt2
                else:
                    myval = alldata[vars_out_i[vnum], max(min(np+toffset,npoints-1),0)]
                if (ftype == 1):      #Convert to short int format
                    thisvar[0,np-starti] = int((myval - add_offset) / scale_factor)
                else:
                    thisvar[np-starti,0,0] = myval
            vnum = vnum+1
        out_nc.close()
        starti = starti+ndaysm[n % 12]*npd
  
