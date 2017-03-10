TERMFS=8

launch_xterm () {
	local __pid_var=$2
	echo "Launching: $1"
	uxterm -fs ${TERMFS} -e "$1" &
	#uxterm -hold -fs ${TERMFS} -e "$1" &
	eval $__pid_var="$!"
	sleep 0.2
}

check_processes () {
	#echo "check processes: $@"
	local __counter=$1
	local __tmp=0
	shift
	while test $# -gt 0; do
		kill -0 $1 2> /dev/null
		__tmp=`expr ${__tmp} + $?`
		#echo "  pid: $1 count: $__tmp"
		shift
	done
	eval $__counter="$__tmp"
}

stop_processes () {
	while test $# -gt 0; do
		kill -9 $1 2> /dev/null
		shift
	done
}

make_empire () {
	make -C empire-client
	if test $? -ne 0; then
		echo "error while compiling empire-client"
		exit 1
	fi

	make -C empire-server
	if test $? -ne 0; then
		echo "error while compiling empire-server"
		exit 1
	fi
}
