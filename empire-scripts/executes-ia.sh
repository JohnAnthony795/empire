#!/usr/bin/bash

SPORT=$(( ( RANDOM % 1000 ) + 2000 ))

IA1=$1
IA2=$2

if test x$IA1 = x -o x$IA2 = x; then
	echo "usage: $0 <ia1> <ia2>"
	exit 1
fi

source ./execute-lib.sh ; cd ..
make_empire

NBWIN0=0
NBWIN1=0

NB=100
while test $NB -gt 0; do
	NB=`expr $NB - 1`
	# Demarrage des programmes.
	./empire-server/Main.native -sport ${SPORT} > out_S 2>&1 &
	SPID=$!
	sleep 1
	pypy ./empire-captain/ai${IA1}.py localhost ${SPORT} > out_P1 2>&1 &
	PPID1=$!
	pypy ./empire-captain/ai${IA2}.py localhost ${SPORT} > out_P2 2>&1 &
	PPID2=$!

	PIDS="${SPID} ${PPID1} ${PPID2}"

	# Regarde si un des programmes est stoppe ou si le temps d'execution
	# est trop important.
	START=`date +%s`
	DEADLINE=`expr $START + 10`
	STOPPED=0
	PREMATURELY=0
	while test $STOPPED -eq 0; do
		sleep 2
		NOW=`date +%s`
		if test $NOW -gt $DEADLINE; then
			STOPPED=1
			PREMATURELY=1
		else
			check_processes STOPPED ${PIDS}
		fi
	done

	# Arret de tous les programmes.
	stop_processes ${PIDS}

	if test $PREMATURELY -eq 0; then
		tail -n 1 out_S | grep 'winner 0' > /dev/null
		if test $? -eq 0; then
			NBWIN0=`expr $NBWIN0 + 1`
		else
			tail -n 1 out_S | grep 'winner 1' > /dev/null
			if test $? -eq 0; then
				NBWIN1=`expr $NBWIN1 + 1`
			else
				echo "ERR!"
				exit
			fi
		fi
	fi
	echo "END PART"
	echo "END PART $NBWIN0 $NBWIN1"
done
echo $NBWIN0 $NBWIN1
