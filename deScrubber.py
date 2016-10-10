import fileinput
import re
 
for line in fileinput.input():
    line = re.sub('\[\.\]', '.', line.rstrip())
    line = re.sub('\[\@\]', '@', line.rstrip())
    line = re.sub('\[dot\]', '.', line.rstrip())
    line = re.sub('\[at\]', '@', line.rstrip())
    print '%r \n ' % line
