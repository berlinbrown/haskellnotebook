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

if [ $(uname -s | grep -c CYGWIN) -gt 0 ]; then
	echo "WARN: running in CYGWIN environment"
	DIR_DB=`cygpath -wp $BOTBERT_HOME`
else
	CPBOTBERT=$LIN_CPBOTBERT
fi

cd $BOTBERT_HOME

APP_MAIN=${BOTBERT_HOME}/lib/python/spiderbot.py
echo "-----------------------"
echo "running in directory=${BOTBERT_HOME} operation=$1"
echo "-----------------------"
echo

DIR_DB=$DIR_DB/var/lib/spiderdb/spider

# Use the first arg, for example 'sweep' 'findcategory'
python $APP_MAIN $DIR_DB

# Write the process id
# echo $! > $BOTBERT_HOME/bin/botgems.pid

# End of Script --
