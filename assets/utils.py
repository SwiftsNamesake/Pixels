# Jonatan H Sundqvist
# Various asset-related utilities


from PIL import Image

im = Image.open('pencil.png')
px = im.load()
dx, dy = im.size

print('Size is {}x{} pixels'.format(dx,dy))


with open('out.txt', 'w', encoding='utf-8') as f:
  f.write('\n,'.join(
    ', '.join('0x{0:02x}{1:02x}{2:02x}{3:02x}'.format(*px[x,y]) for x in range(dx)) for y in range(dy)
  ))
