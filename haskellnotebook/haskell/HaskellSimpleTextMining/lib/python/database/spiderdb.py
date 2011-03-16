"""
File: spiderdb.py

Copyright (c) 2007, Botnode.com (Berlin Brown)
http://www.opensource.org/licenses/bsd-license.php

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice, 
    this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice, 
    this list of conditions and the following disclaimer in the documentation 
    and/or other materials provided with the distribution.
    * Neither the name of the Newspiritcompany.com (Berlin Brown) nor 
    the names of its contributors may be used to endorse or promote 
    products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
'AS IS' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Description:

Save spider database format in big endian format (network format).

Also see:
(1) http://docs.python.org/lib/module-struct.html

"""

import sys
from array import *
from struct import *

# Ideally, Spider Bot Database Magic Format [05D4 B0DB]
# Saved as little-endian format
MAGIC_NUMBER_U2A = 0x05D4
MAGIC_NUMBER_U2B = 0xB0DB

MAJOR_NUMBER = 0x0001
MINOR_NUMBER = 0x0000

HEADER_DELIM = 0xFF07

END_FILE_DELIM = 0x6666

URL_TAG = 0x75
TITLE_TAG = 0x76
DESCR_TAG = 0x77
KEYWORDS_TAG = 0x78

"""DatabaseFile struct, ensure to pack as little-endian
(NOTE:'u' is associated with an unsigned byte
<code>
DatabaseFile {
  unsigned short [2] (u4) magic_number = MAGIC NUMBER
  unsigned short (u2) major_number
  unsigned short (u2) minor_number

  header delim

  unsigned short u2 url_pool_count;
  url_info url_pool[url_pool_count-1];

}
</code>

Data structure for url pool data:

<code>
URLInfo [] {
  unsigned char (u1) pool_tag = TAG
  URL url
  TITLE title
  DESCRIPTION descr
  keywords
}
URL {
  u1 tag = 0x75
  u2 id = <some index value>
  u2 length
  ul bytes[length]
}
TITLE {
  u1 tag = 0x76
  u2 length
  ul bytes[length]
}
DESCRIPTION {
  u1 tag = 0x77
  u2 length
  ul bytes[length]
}
KEYWORDS {
  u1 tag = 0x78
  u2 length
  ul bytes[length]
}			
</code>
"""

def create_database(dbdir, pool):
	print "Database pool size=%s" % len(pool.url_pool)
	url_dbfile = "%s/spiderdb_7.sdb" % dbdir
	fobj = open(url_dbfile, "wb")
	try:
		# Two short values, save as little endian
		magic_number = pack('>HH', MAGIC_NUMBER_U2A, MAGIC_NUMBER_U2B)
		fobj.write(magic_number)

		# Write the version
		version = pack('>HH', MAJOR_NUMBER, MINOR_NUMBER)
		fobj.write(version)

		# Write the header delimiter
		header_delim = pack('>H', HEADER_DELIM)
		fobj.write(header_delim)

		pool_count = pack('>H', len(pool.url_pool))
		fobj.write(pool_count)
		
		for index, field_info in enumerate(pool.url_pool):
			try:
				#url = unpack('>s', field_info.url)
				#title = pack('>s', field_info.title)
				#descr = pack('>s', str(field_info.descr))
				#keywords = pack('>s', field_info.keywords)
				url = field_info.url
				title = field_info.title
				descr = field_info.descr
				keywords = field_info.keywords
			
				url_l = pack('>H', field_info.url_len_u2)
				title_l = pack('>H', field_info.title_len_u2)
				descr_l = pack('>H', field_info.descr_len_u2)
				keywords_l = pack('>H', field_info.keywords_len_u2)

				# Ensure that string is UTF-8, unicode encoded
				# BYTE, SHORT(Idx),  SHORT, STRING(UNICODE,UTF-8)
				fobj.write(pack('>B', URL_TAG))
				fobj.write(pack('>H', index))
				fobj.write(url_l)
				fobj.write(url)
				fobj.write(pack('>B', TITLE_TAG))
				fobj.write(title_l)
				fobj.write(title)
				fobj.write(pack('>B', DESCR_TAG))
				fobj.write(descr_l)
				fobj.write(descr)
				fobj.write(pack('>B', KEYWORDS_TAG))
				fobj.write(keywords_l)
				fobj.write(keywords)
				
			except Exception, db_err:
				print "ERR: writing database record=%s" % db_err
				
	finally:
		print "INFO [spiderdb]: closing database file"
		fobj.write(pack('>H', END_FILE_DELIM))
		fobj.close()
