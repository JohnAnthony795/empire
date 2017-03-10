#!/bin/bash

TERMFS=10

SPORT=$(( ( RANDOM % 1000 ) + 2000 ))
PPORT1=$(( ( RANDOM % 1000 ) + 3000 ))
PPORT2=$(( ( RANDOM % 1000 ) + 4000 ))
OPORT1=$(( ( RANDOM % 1000 ) + 5000 ))
OPORT2=$(( ( RANDOM % 1000 ) + 6000 ))

IA1=$1
IA2=$2

if test x$IA1 = x -o x$IA2 = x; then
	echo "usage: $0 <ia1> <ia2>"
	exit 1
fi

source ./execute-lib.sh ; cd ..
make_empire

INTERPRETER=pypy
which pypy > /dev/null
if test $? -eq 1; then
  INTERPRETER=""
fi

#CLIENT="./empire-client/Main.byte -obs -sport"
CLIENT="cd ./empire-gtk-client ; ./cliempire.native -obs -port"

# Demarrage des programmes.
launch_xterm "./empire-server/Main.native -sport ${SPORT} > out_S 2>&1" SPID
launch_xterm "./empire-tee/tee.py localhost ${SPORT} ${PPORT1} ${OPORT1} > out_T1 2>&1" TPID1
launch_xterm "${CLIENT} ${OPORT1} > out_C1 2>&1" OPID1
launch_xterm "$INTERPRETER ./empire-captain/ai${IA1}.py localhost ${PPORT1} > out_P1 2>&1" PPID1
launch_xterm "./empire-tee/tee.py localhost ${SPORT} ${PPORT2} ${OPORT2} > out_T2 2>&1" TPID2
launch_xterm "${CLIENT} ${OPORT2} > out_C2 2>&1" OPID2
launch_xterm "$INTERPRETER ./empire-captain/ai${IA2}.py localhost ${PPORT2} > out_P2 2>&1" PPID2

PIDS="${SPID} ${TPID1} ${OPID1} ${PPID1} ${TPID2} ${OPID2} ${PPID2}"

# Regarde si un des programmes est stoppe.
STOPPED=0
while test $STOPPED -eq 0; do
	sleep 2
	check_processes STOPPED ${PIDS}
done

# Arret de tous les programmes.
stop_processes ${PIDS}
