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
DIR_DB=$DIR_DB/var/lib/spiderdb/spider

python $APP_MAIN $DIR_DB $1 $2 $3 $4 $5

# End of Script --
