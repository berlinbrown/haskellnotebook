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

"""

__author__ = "Berlin Brown"
__copyright__ = "Copyright (c) 2006-2008 Berlin Brown"
__license__ = "New BSD"

import sys
import time, datetime
import socket

from soup.BeautifulSoup import *
import urllib2
from urlparse import urlparse
from optparse import OptionParser
import glob

from database.spiderdb import create_database
from spiderbot_util import DEFAULT_REQUEST_TIMEOUT, FF_USER_AGENT, \
    LINK_SET_INDICATOR, URLField, buildOpener, validateSubLink, convertStrAscii
from content.spiderbot_content import doc_ignore_content, \
    clean_content, build_page_info

def processSubLink(link_tag):
	"""Process each link, ensure that a 'href' value is available,
	also convert relative URIs to full URLs"""
	# TODO: BUG, currently ignoring all internal links (don't have http)
	link_val = link_tag['href']
	link = None
	# If URL found, ignore; if relative than attempt to build URL
	if link_val.lower().startswith('http'):
		link = link_val
	else:
		link = link_val
	return link

def get_meta_content(meta_data_arr):
	""" Use with soup, in the following manner:
	<code>meta_data_keywords = soup.findAll('meta', {'name':'keywords'})
	meta_data_descr = soup.findAll('meta', {'name':'description'})</code>
	keywords = get_meta_content(meta_data_keywords)"""
	try:
		content_content = None
		if meta_data_arr and len(meta_data_arr) > 0:
			content_data = [el['content'] for el in meta_data_arr]
			if content_data and len(content_data) > 0:
				return content_data[0]			
	except:
		pass	
	return ""

def crawlSingleURL(link, idx, total_links):
	try:
		opener = buildOpener()
		start = time.time()
		data = opener.open(link).read()
		soup = BeautifulSoup(data)
		meta_data_keywords = soup.findAll('meta', {'name':'keywords'})
		meta_data_descr = soup.findAll('meta', {'name':'description'})
		keywords = get_meta_content(meta_data_keywords)
		descr = get_meta_content(meta_data_descr)
		# Extract the title tag
		titleTag = None
		try:
			titleTag = soup.html.head.title
			titleTag = str(titleTag.string)
		except:
			titleTag = ""			
		end = time.time()

		# Return the basic URL data structure
		field = URLField(link, titleTag, descr, keywords)
		field.populate()	
		if ((idx % LINK_SET_INDICATOR) == 0):			
			sys.stdout.write("[%s/%s] " % (idx, total_links))
		# Exit crawl single URL with url field.
		# @return URLField
		return field
	except socket.timeout:
		print "ERR: timeout [%s/%s] " % (idx, total_links)
	except urllib2.URLError:
		print "ERR: timeout [%s/%s] " % (idx, total_links)
	except Exception, e:
		pass

def crawlSingleURLForContent(link, idx, total_links):
	""" Crawl this URL but only extract the content for content
	analysis.  A more extensive model than crawlSingleURL"""
	try:
		opener = buildOpener()
		start = time.time()
		data = opener.open(link).read()
		istats = build_page_info(link, data)
		data = clean_content(data)
		soup = BeautifulSoup(data)
		meta_data_keywords = soup.findAll('meta', {'name':'keywords'})
		meta_data_descr = soup.findAll('meta', {'name':'description'})
		keywords = get_meta_content(meta_data_keywords)
		descr = get_meta_content(meta_data_descr)

		# Extract the title tag
		titleTag = None
		try:
			titleTag = soup.html.head.title
			titleTag = str(titleTag.string)
		except:
			titleTag = ""
		# Ignore content we aren't concerned with
		partial_content = doc_ignore_content(soup)
		
		end = time.time()
		# Return the basic URL data structure
		field = URLField(link, titleTag, descr, keywords)

		field.descr = field.tokenizeTags(field.descr)
		field.keywords = field.tokenizeTags(field.keywords)

		field.full_content = data
		field.extract_content = partial_content
		field.info_stats = istats
		field.populate()
		if ((idx % LINK_SET_INDICATOR) == 0):
			sys.stdout.write("[%s/%s] " % (idx, total_links))
	   		
		# Exit crawl single URL with url field.
		# @return URLField
		return field
	except urllib2.URLError:
		print "ERR: timeout [%s/%s] " % (idx, total_links)
	except Exception, e:
		# NOTE: if pass allowed, compile errors will be ignored.
		print "ERR<crawlSingleURLForContent>: %s" % e
		pass

def crawlForURLContentDump(link_list):
	dump_data = []
	""" Iterate through list and dump data"""
	for index, link in enumerate(link_list):
		data_field = crawlSingleURLForContent(link, index, len(link_list))
		dump_data.append(data_field)
	return dump_data
		
def crawlBuildLinks(link_list):
	opener = buildOpener()
	""" Iterate through the list of links and collect links found
	on each page through the use of the beautiful soup lib."""
	total_links = 0
	total_links_tag = 0
	sub_links = None
	for link in link_list:
		try:
			data = opener.open(link).read()
			soup = BeautifulSoup(data)
			sub_links_tag = soup.findAll('a')
			total_links_tag = total_links_tag + len(sub_links_tag)
			sub_links = [processSubLink(el) for el in sub_links_tag if validateSubLink(el)]			
			# Filter out duplicates with set
			sub_links = set(sub_links)		
			total_links = total_links + len(sub_links)
		except Exception, e:
			print "ERR <crawlBuildLinks>: %s" % e
			print "    <crawlBuildLinks>: url=[%s]" % link

	if total_links_tag != 0:
		valid_ratio =  float(total_links) / total_links_tag
		print "INFO: valid links ratio: %s, max=%s/%s" % \
		(valid_ratio,
		 total_links,
		 total_links_tag)

	# Return an empty list or valid content
	if sub_links is None:
		return ([], total_links)
	else:
		return (sub_links, total_links)

class URLInfoPool:	
	def __init__(self):
		self.url_pool = []
		
	def buildURLPool(self, link_list):
		links, total_links = crawlBuildLinks(link_list)
		for index, link_proc in enumerate(links):
			# DEBUG
			if index > 10:
				break			
			url_info = crawlSingleURL(link_proc, index, total_links)
			if url_info:
				self.url_pool.append(url_info)
