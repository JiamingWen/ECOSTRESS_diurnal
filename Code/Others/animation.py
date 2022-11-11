# -*- coding: utf-8 -*-
"""
Created on Tue Jan 29 09:05:56 2019

@author: shark
"""
# please make sure only png files in path location
import imageio
import os
import os.path
def animation(gif_name, path, duration = 0.4, loop = 1):
    frames = []
    pngFiles = os.listdir(path)
    image_list = [os.path.join(path, f) for f in pngFiles]
    for image_name in image_list:
        print (image_name)
        frames.append(imageio.imread(image_name))
    imageio.mimsave(gif_name, frames, 'GIF', duration = duration,loop=loop)
    return

varname = 'LST' #LST ET
gif_name = '/local/workdir/jw2495/ECOSTRESS/DTC/Figures/AGU/animation/' + varname + '.gif'
path = '/local/workdir/jw2495/ECOSTRESS/DTC/Figures/AGU/animation/' + varname + '/'   #指定文件路径
frames = []
pngFiles = os.listdir(path)
image_list = [os.path.join(path, f) for f in pngFiles]
duration = 0.6
loop = 10
animation(gif_name, path, duration, loop)
