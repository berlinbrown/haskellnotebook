"""
Copyright (c) 2007, Botnode.com (Berlin Brown)
http://www.opensource.org/licenses/bsd-license.php

Date: 1/1/2008

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
"""

__author__ = "Berlin Brown"
__version__ = "0.1"
__copyright__ = "Copyright (c) 2006-2008 Berlin Brown"
__license__ = "New BSD"

import sys
import time, datetime
import socket

import urllib2
from urlparse import urlparse
from optparse import OptionParser
import glob

from database.spiderdb import create_database
from database.content_db import create_content_db
from spiderbot_util import DEFAULT_REQUEST_TIMEOUT, FF_USER_AGENT, buildOpener
from url_info_pool import URLInfoPool, crawlForURLContentDump

ARG_SUB_COMMAND = 1
APP_SUB_COMMANDS = {
	"seed": "Crawl based on seed crawl",
	"dump": "Dump html content based on data from seed directory"
}
MAIN_USAGE_STR = "usage: %prog <subcommand> <database dir> [options]"
ERROR_MESSAGES = {
	"err_sub": "ERR: Invalid subcommand"
}

def validate_cmd_invalid(options, cmd_key):
	return False

def validate_cmd_dump(options, cmd_key):
	""" Validate dump options"""
	if cmd_key == 'dump':
		if options.seed_dir is not None \
			    and options.dump_dir is not None:
			return True
	return False

# Use of method dispatch to validate
# arguments.
ARG_METHOD_VALIDATORS = {
    "dump": validate_cmd_dump,
    "seed": validate_cmd_invalid
}

def connectLinkService(requrl):
	""" First, connect to the botlist URL service and extract
	the most recent list of links.  This will seed the
	botlist spider crawler."""	
	opener = buildOpener()	
	req = urllib2.Request(requrl)
	req.add_header('user-agent', FF_USER_AGENT)
	link_data = opener.open(req).read()
	link_data = [ line.strip() for line in link_data.split('\n') ]
	link_data = filter(lambda (line):
					   (len(line) > 0) and (len(line.split('::|')) == NO_COLS_SERVICE),
					   link_data)
	content = [ col.split('::|') for col in link_data ]
	return content
	
def runServiceURLPool():
	try:
		data = connectLinkService(URL_LINK_SERVICE)
	except urllib2.URLError, urlerr:
		print "FATAL ERR: could not connect to link seed service"
		print urlerr
		sys.exit(-1)

	link_list = [ line_set[0] for line_set in data ]
	# The URL Pool contains a collection of the url field data structures
	infoPool = URLInfoPool()
	infoPool.buildURLPool(link_list)
	create_database(sys.argv[1], infoPool)

def runSeedDir(seed_dir):
	""" Return all lines in file of type extension tdb in this directory"""
	print "*** Processing seed directory (all files of *.tdb): %s" % seed_dir
	tdb_files = glob.glob('%s/*.tdb' % seed_dir)
	content_lines = []
	for fline in tdb_files:
		fline = open(fline, 'r')
		content = fline.readlines()
		urllines = [line.strip() for line in content]
		for line in urllines:
			content_lines.append(line)
		fline.close()
	return content_lines

def main():		
	print "***"
	print "*** Spider Bot v%s" % __version__

	parser = OptionParser(usage=MAIN_USAGE_STR)
	if len(sys.argv) < 3:
		print MAIN_USAGE_STR
		sys.exit(1)		
	parser.add_option('-s', '--seed_dir', dest='seed_dir',
			  help='Simple text seed directory path (default: %default)',
			  default=None)
	parser.add_option('-d', '--dump_dir', dest='dump_dir',
			  help='Output content dump directory path (default: %default)',
			  default=None)
	options, args = parser.parse_args()
	if not args:
		parser.print_help()
		sys.exit(1)

	# Access the options with 'options.seed_dir', for example
	now = time.localtime(time.time())
	print "*** database directory=%s" % sys.argv[1]
	print "*** %s" % time.asctime(now)
	start = time.time()
	socket.setdefaulttimeout(DEFAULT_REQUEST_TIMEOUT)
	
	if sys.argv[ARG_SUB_COMMAND] is not None:
		cmd_key = sys.argv[ARG_SUB_COMMAND].lower()
		if APP_SUB_COMMANDS.has_key(cmd_key):
			bot_method = ARG_METHOD_VALIDATORS.get(cmd_key, None)
			if (bot_method(options, "seed")):
				link_list = runSeedDir(options.seed_dir)
				# The URL Pool contains a collection of the url field data structures
				infoPool = URLInfoPool()
				infoPool.buildURLPool(link_list)
				create_database(sys.argv[:-1], infoPool)
			elif (bot_method(options, "dump")):
				link_list = runSeedDir(options.seed_dir)
				dump_list = crawlForURLContentDump(link_list)
				create_content_db(options.dump_dir, dump_list)
			else:
				# APP_EXIT_POINT (invalid sub command)
				print ERROR_MESSAGES['err_sub']
				print MAIN_USAGE_STR
				print "ERR with subcommand, COMMAND-LINE ARGS:\n<<<%s>>>" % sys.argv
				sys.exit(1)
		else:
			# APP_EXIT_POINT (invalid sub command)
			print ERROR_MESSAGES['err_sub']
			print MAIN_USAGE_STR
			sys.exit(1)

	end = time.time()
	diff = end - start
	print "\n*** Done"
	print "*** spider bot processing time=%s" % diff

if __name__ == '__main__':
	main()
