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
from spiderbot_const import KEY_HTML_TAGS, MAX_LEN_EXTRACT

def _write_info_headers(dir_name):
	""" Write headers for page stats content"""
	filename = "%s/_dump_file.info_headers" % (dir_name)
	#hdrs = [ "%s::|" % n for n in KEY_HTML_TAGS[:-1]]
	str = '::|'.join(KEY_HTML_TAGS)
	f = open(filename, "w")
	# Write the first item; URL column
	f.write("URL::|")
	f.write(str + '\n')	
	f.close()

def create_content_db(dir_name, content_list):
	""" Save the content to plain text files"""
	print "\nSaving content dump: size=%s" % len(content_list)
	for i, field in enumerate(content_list):
		try:
			filename = "%s/_dump_file_%s.html" % (dir_name, i)
			f = open(filename, "w")
			f.write(field.full_content)
			f.close()

			# Write the partial feature extract file
			extract_filename = "%s/_dump_file_%s.extract" % (dir_name, i)			
			f = open(extract_filename, "w")
			# Also write the keywords and description to the 
			# Partial extraction file.
			f.write(field.descr) ; f.write('\n')
			f.write(field.keywords) ; f.write('\n')
			# put a cap on the extract content, only write
			# so much data.
			field.extract_content = field.extract_content[:MAX_LEN_EXTRACT]
			f.write(field.extract_content)
			f.close()

			# Write the page content info stats
			_write_info_headers(dir_name)
			stats_filename = "%s/_dump_file_%s.info" % (dir_name, i)
			f = open(stats_filename, "w")
			page_info_stats = "%s" % field.info_stats
			if field.info_stats is not None:
				f.write(page_info_stats)
			f.close()			
		except Exception, e:
			print "ERR <create_content_db> %s", e
