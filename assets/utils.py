# Jonatan H Sundqvist
# Various asset-related utilities


import os
import json

from os.path import splitext
from PIL import Image


def toHexArray(fn, out):

  im = Image.open(fn)
  px = im.load()
  dx, dy = im.size

  print('Size is {}x{} pixels'.format(dx,dy))


  with open(out, 'w', encoding='UTF-8') as f:
    f.write('/n,'.join(
      ', '.join('0x{0:02x}{1:02x}{2:02x}{3:02x}'.format(*px[x,y]) for x in range(dx)) for y in range(dy)
    ))



def stencils(fnimage, fnrects):

  # TODO: Rename
  with open(fnrects, 'r', encoding='UTF-8') as f:
  	rects = json.load(f)

  im = Image.open(fnimage)

  name, ext = splitext(fnimage)

  for i, rect in enumerate(rects):
    centre = rect['centre']
    size   = rect['size']
    new = im.crop((int(centre['x']-size['x']/2),
                   int(centre['y']-size['y']/2),
                   int(centre['x']+size['x']/2),
                   int(centre['y']+size['y']/2))) # left, upper, right, lower

    if not os.path.exists(name):
    	os.makedirs(name)
    new.save('{name}/{n}{ext}'.format(n=i, name=name, ext=ext))	



def subdivide(fn, cols, rows):
  
  # Assumes the sprites are evenly spaced

  im = Image.open(fn)
  width, height = im.size

  dx, dy = int(width/cols), int(height/rows)

  name, ext = splitext(fn)
  print(name, ext)

  for col in range(cols):
    for row in range(rows):
      new = im.crop((col*dx, row*dy, col*dx+dx, row*dy+dy)) # left, upper, right, lower

      if not os.path.exists(name):
      	os.makedirs(name)
      new.save('{name}/{col}-{row}{ext}'.format(row=row, col=col, name=name, ext=ext))


def main():
  toHexArray('pencil.png', 'pencil.txt')
  # subdivide('C:/Users/Jonatan/Pictures/run_frames.png', 4, 4)
  stencils('C:/Users/Jonatan/Pictures/run_frames.png',
  	       'C:/Users/Jonatan/Desktop/Haskell/projects/Pixels/assets/2016-08-28_06-15-54.5479569_UTC.json')



if __name__ == '__main__':
  main()