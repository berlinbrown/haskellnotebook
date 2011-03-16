#!/bin/sh

# Depending on if we are running from pwd or not, determine
# proper location to change directory to.
case $0 in 
	/*) 
		ABS_APP_PATH=$0
		ABS_CONF=`dirname $ABS_APP_PATH`
		ABS_CONF=`dirname $ABS_CONF`
		;; 
	bin*)
		ABS_APP_PATH=`pwd`
		ABS_CONF=$ABS_APP_PATH
		;;
	*)
		ABS_APP_PATH=`pwd`
		ABS_CONF=`dirname $ABS_APP_PATH`
		;; 
esac

BOTBERT_HOME=$ABS_CONF
TOPDIR=$BOTBERT_HOME
DIR_DB=$BOTBERT_HOME

cd $BOTBERT_HOME

APP_MAIN=${BOTBERT_HOME}/dbreader
echo "-----------------------"
echo "running in directory=${BOTBERT_HOME} operation=$1"
echo "-----------------------"
echo

SPIDER_DB=example/spiderdb_7.sdb
CONTENT_DB=$DIR_DB/../../../../var/lib/spiderdb/contentdb

ls $SPIDER_DB

$APP_MAIN $SPIDER_DB $CONTENT_DB

# Write the process id
# echo $! > $BOTBERT_HOME/bin/botgems.pid
# End of Script
