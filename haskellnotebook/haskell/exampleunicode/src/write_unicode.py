"""
 Simple Test to Write Unicode Data
 write_unicode.py
"""

import sys
from array import *
from struct import *

TEXT_ENCODE = "Simple"

if __name__ == '__main__':
    print "Writing Unicode File, UTF-8"
    """ File.Encoding: The encoding that this file uses. 
    When Unicode strings are written to a file, 
    they will be converted to byte strings using this encoding"""
    f = open("simplefile.dat", "wb")
    print "Encoding=%s" % f.encoding
    
    print "Len String:%s" % len(TEXT_ENCODE)
    str_console = TEXT_ENCODE.encode("ascii")

    u = unicode( TEXT_ENCODE, 'utf-8' )
    print "Len Bytes:%s" % len(u)

    print "Writing data to file=%s" % str_console
    f.write(u)
    f.close()
    print "Done"
