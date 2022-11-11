# -*- coding: utf-8 -*-
"""
Created on Sun Sep 20 16:12:36 2020

@author: jw2495
"""

from netCDF4 import Dataset
import matplotlib as mpl
mpl.use('TkAgg')
import matplotlib.pyplot as plt
# from mpl_toolkits.basemap import Basemap, cm
import numpy as np
import os
import csv
from scipy import stats
import time
import multiprocessing as mp
from functools import partial

def lat_lon_reproj(nc_file,lat,lon,offset0,outputfolder):
    g16nc = Dataset(nc_file, 'r')
    var_names = [ii for ii in g16nc.variables]
    var_name = var_names[0]
    qc_name = var_names[1]
    
    data_array = g16nc.variables[var_name][:]
    data_units = g16nc.variables[var_name].units
    data_time_grab = ((g16nc.time_coverage_end).replace('T',' ')).replace('Z','')
    data_long_name = g16nc.variables[var_name].long_name
    DQF = g16nc.variables[qc_name][:]
    
    if var_name in ['LST', 'TPW']:
        proj_info = g16nc.variables['goes_imager_projection']
        lon_origin = proj_info.longitude_of_projection_origin
        H = proj_info.perspective_point_height+proj_info.semi_major_axis
        r_eq = proj_info.semi_major_axis
        r_pol = proj_info.semi_minor_axis
        # grid info
        lat_rad_1d = g16nc.variables['x'][:]
        lon_rad_1d = g16nc.variables['y'][:]
        # create meshgrid filled with radian angles
        lat_rad,lon_rad = np.meshgrid(lat_rad_1d,lon_rad_1d)
        # lat/lon calc routine from satellite radian angle vectors
        lambda_0 = (lon_origin*np.pi)/180.0
        
        a_var = np.power(np.sin(lat_rad),2.0) + (np.power(np.cos(lat_rad),2.0)*(np.power(np.cos(lon_rad),2.0)+(((r_eq*r_eq)/(r_pol*r_pol))*np.power(np.sin(lon_rad),2.0))))
        b_var = -2.0*H*np.cos(lat_rad)*np.cos(lon_rad)
        c_var = (H**2.0)-(r_eq**2.0)
        
        r_s = (-1.0*b_var - np.sqrt((b_var**2)-(4.0*a_var*c_var)))/(2.0*a_var)
        s_x = r_s*np.cos(lat_rad)*np.cos(lon_rad)
        s_y = - r_s*np.sin(lat_rad)
        s_z = r_s*np.cos(lat_rad)*np.sin(lon_rad)
        
        lat = (180.0/np.pi)*(np.arctan(((r_eq*r_eq)/(r_pol*r_pol))*((s_z/np.sqrt(((H-s_x)*(H-s_x))+(s_y*s_y))))))
        lon = (lambda_0 - np.arctan(s_y/(H-s_x)))*(180.0/np.pi)
        # print(stats.describe(data_array.flatten()))
        # print(np.max(data_array.flatten()))
    elif var_name == 'DSR':
        lat_rad_1d = g16nc.variables['lat'][:]
        lon_rad_1d = g16nc.variables['lon'][:]
        lat,lon = np.meshgrid(lat_rad_1d,lon_rad_1d)
    else:
        print ("check variable name!")
    # close file when finished
    g16nc.close()
    g16nc = None
    
    latmax=lat0+offset0
    latmin=lat0-offset0
    lonmax=lon0+offset0
    lonmin=lon0-offset0
    
    lon1=lon.flatten()
    lat1=lat.flatten()
    data1=data_array.flatten()
    DQF1=DQF.flatten()
    
    lon_region=lon1[(lat1<latmax)&(lat1>latmin)&(lon1<lonmax)&(lon1>lonmin)] #&(data1<65535.0)
    lat_region=lat1[(lat1<latmax)&(lat1>latmin)&(lon1<lonmax)&(lon1>lonmin)] #&(data1<65535.0)
    LST_region=data1[(lat1<latmax)&(lat1>latmin)&(lon1<lonmax)&(lon1>lonmin)] #&(data1<65535.0)
    DQF_region=DQF1[(lat1<latmax)&(lat1>latmin)&(lon1<lonmax)&(lon1>lonmin)] #&(data1<65535.0)
    
    output=np.column_stack((lon_region,lat_region,LST_region,DQF_region))
    
    fields = ['lon', 'lat', var_name, 'DQF']
    # name of csv file  
    filename = outputfolder + var_name+'_'+data_time_grab[:10]+'_'+data_time_grab[11:13]+'-'+data_time_grab[14:16]+'-'+data_time_grab[17:19]+'.csv'
    # writing to csv file  
    with open(filename, 'w',newline='') as csvfile:  
        # creating a csv writer object  
        csvwriter = csv.writer(csvfile)      
        # writing the fields  
        csvwriter.writerow(fields)    
        # writing the data rows  
        csvwriter.writerows(output) 


var_name='LST' #TPW  LST  DSR 
# lon0=-106.7020;lat0=34.3623;outputfolder='/local/workdir/jw2495/ECOSTRESS_diurnal/Data/us-seg-region2new/GOES_LST/csvfile/';startdate=2018216;enddate=2018226
# lon0=-106.7020;lat0=34.3623;outputfolder='/local/workdir/jw2495/ECOSTRESS_diurnal/Data/us-seg-region4new/GOES_LST/csvfile/';startdate=2020270;enddate=2020279
# lon0=-106.7020;lat0=34.3623;outputfolder='/local/workdir/jw2495/ECOSTRESS_diurnal/Data/us-seg-region5new/GOES_LST/csvfile/';startdate=2020098;enddate=2020108
lon0=-97.4888;lat0=36.6058;outputfolder='/local/workdir/jw2495/ECOSTRESS_diurnal/Data/us-arm-region2new/GOES_LST/csvfile/';startdate=2021166;enddate=2021170

if var_name == 'LST':
    offset0=0.2
elif var_name == 'TPW':
    offset0=0.5
elif var_name == 'DSR':
    offset0=1.5

nc_folder = '/local/workdir/jw2495/GOES/'
os.chdir(nc_folder)
full_direc = os.listdir()

#non-parallel TPW 25min LST 40min DSR 1min
# nc_files = [ii for ii in full_direc if (ii.endswith('.nc'))]
# for file_indx in np.arange(len(nc_files)):
#     print(file_indx)
#     if (nc_files[file_indx][19:21]=='16')&(nc_files[file_indx][10:13]==var_name): 
#         nc_file = nc_files[file_indx] # select .nc file
#         print(nc_files[file_indx]) # print file name
#         lat_lon_reproj(nc_file,lat0,lon0,offset0,outputfolder)

if var_name in ['LST', 'DSR']:
    nc_files = [ii for ii in full_direc if (ii.endswith('.nc'))&(ii[19:21]=='16')&(ii[10:13]==var_name)&(int(ii[23:30])>startdate-1)&(int(ii[23:30])<enddate+1)]
elif var_name == 'TPW':
    nc_files0 = [ii for ii in full_direc if (ii.endswith('.nc'))&(ii[19:21]=='16')]
    nc_files = [ii for ii in nc_files0 if (ii[10:13]==var_name)&(int(ii[32:34])<=5)]

starttime=time.time()
pool = mp.Pool(30)
lat_lon_reproj2=partial(lat_lon_reproj,lon=lon0, lat=lat0, offset0=offset0, outputfolder=outputfolder)
pool.map(lat_lon_reproj2, nc_files)
pool.close()
endtime=time.time()
print(endtime-starttime)

#######################################################################################
# plt.scatter(lon, lat, c=data)

# bbox = [np.min(lon),np.min(lat),np.max(lon),np.max(lat)] # set bounds for plotting

# # figure routine for visualization
# fig = plt.figure(figsize=(9,4),dpi=200)

# n_add = 0
# m = Basemap(llcrnrlon=bbox[0]-n_add,llcrnrlat=bbox[1]-n_add,urcrnrlon=bbox[2]+n_add,urcrnrlat=bbox[3]+n_add,resolution='c', projection='cyl')
# m.drawcoastlines(linewidth=0.5)
# m.drawcountries(linewidth=0.25)
# m.pcolormesh(lon.data, lat.data, data, latlon=True)
# parallels = np.linspace(np.min(lat),np.max(lat),5)
# m.drawparallels(parallels,labels=[True,False,False,False])
# meridians = np.linspace(np.min(lon),np.max(lon),5)
# m.drawmeridians(meridians,labels=[False,False,False,True])
# cb = m.colorbar()

# data_units = ((data_units.replace('-','^{-')).replace('1','1}')).replace('2','2}')
# plt.rc('text', usetex=True)
# cb.set_label(r'%s $ \left[ \mathrm{%s} \right] $'% (var_name,data_units))
# # plt.title('{0}{2}{3}{4} on {1}'.format(data_long_name,data_time_grab))
# # plt.tight_layout()

# # plt.savefig('goes_16_demo.png',dpi=200,transparent=True) # uncomment to save figure
# plt.show()
